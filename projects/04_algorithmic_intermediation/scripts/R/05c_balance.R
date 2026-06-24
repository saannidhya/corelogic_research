#' 05c_balance.R — Pre-treatment covariate balance across Zillow-exposure terciles
#'
#' Exit-design balance table: ZIP-level pre-period (2019) characteristics and
#' 2019Q1–2021Q3 appreciation across terciles of Zshare among exposed ZIPs
#' (zshare > 0), plus the zero-exposure group, within the exit sample.
#' Normalized differences (Imbens-Rubin) between top tercile and comparison
#' groups. Writes tables_dir/T6_balance.tex (\label{tab:T6}) and
#' out_dir/balance_stats.rds.

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))
set.seed(20260610)

EXPO_START <- round(ZILLOW_EXPO_START * 4)
EXPO_END   <- round(ZILLOW_EXPO_END * 4)

panel <- read_parquet(path(data_dir, "zip_quarter_panel.parquet"))
disp  <- read_parquet(path(data_dir, "zip_quarter_dispersion.parquet"))
zcov  <- read_parquet(path(data_dir, "zip_covariates.parquet"))

df <- panel |>
  left_join(disp |> select(state, zip5, yq, disp_sd),
            by = c("state", "zip5", "yq")) |>
  left_join(zcov |> select(state, zip5, cbsa, stock_resi, sfr_share,
                           sd_lsqft_stock, share_post1990, med_yearbuilt),
            by = c("state", "zip5")) |>
  filter(!is.na(cbsa), stock_resi >= 500) |>
  mutate(qid = as.integer(round(yq * 4)),
         zid = paste0(state, "_", zip5))

expo <- df |>
  filter(qid >= EXPO_START, qid <= EXPO_END) |>
  group_by(state, zip5, zid, cbsa) |>
  summarise(sales_w = sum(n_sales), z_buys = sum(n_zillow_buy),
            od_buys = sum(n_opendoor_buy) + sum(n_offerpad_buy),
            .groups = "drop") |>
  filter(sales_w >= 50) |>
  mutate(zshare = 100 * z_buys / sales_w, odshare = 100 * od_buys / sales_w)
w99z <- quantile(expo$zshare[expo$zshare > 0], 0.99, na.rm = TRUE)
expo <- expo |> mutate(zshare = pmin(zshare, w99z))
z_cbsas <- expo |> group_by(cbsa) |> summarise(zb = sum(z_buys)) |>
  filter(zb >= 50) |> pull(cbsa)
expo <- expo |> filter(cbsa %in% z_cbsas)

# Pre-period (2019) ZIP characteristics + appreciation 2019->2021Q3
pre19 <- df |>
  filter(sale_year == 2019, zid %in% expo$zid) |>
  group_by(zid) |>
  summarise(
    vol19   = sum(n_sales),
    lmedp19 = log(median(med_price, na.rm = TRUE)),
    disp19  = mean(disp_sd, na.rm = TRUE),
    cash19  = mean(cash_share, na.rm = TRUE),
    inv19   = mean(investor_share, na.rm = TRUE),
    .groups = "drop"
  )
vol20 <- df |> filter(sale_year == 2020, zid %in% expo$zid) |>
  group_by(zid) |> summarise(vol20 = sum(n_sales), .groups = "drop")
p21 <- df |>
  filter(qid == round(2021.5 * 4), zid %in% expo$zid) |>
  group_by(zid) |>
  summarise(lmedp21q3 = log(med_price[1]), .groups = "drop")

bal <- expo |>
  left_join(pre19, by = "zid") |>
  left_join(vol20, by = "zid") |>
  left_join(p21, by = "zid") |>
  left_join(zcov |> mutate(zid = paste0(state, "_", zip5)) |>
              select(zid, sfr_share, sd_lsqft_stock, share_post1990,
                     med_yearbuilt, stock_resi),
            by = "zid") |>
  mutate(
    appr_19_21 = lmedp21q3 - lmedp19,
    volg_19_20 = log(pmax(vol20, 1)) - log(pmax(vol19, 1)),
    group = case_when(
      zshare == 0 ~ "zero",
      zshare <= quantile(zshare[zshare > 0], 1/3) ~ "t1",
      zshare <= quantile(zshare[zshare > 0], 2/3) ~ "t2",
      TRUE ~ "t3"
    )
  )

vars <- c(lmedp19 = "Log median price, 2019",
          vol19 = "Transactions, 2019",
          appr_19_21 = "Log price growth, 2019--2021Q3",
          volg_19_20 = "Log volume growth, 2019--2020",
          disp19 = "Residual SD, 2019",
          cash19 = "Cash share, 2019",
          inv19 = "Investor share, 2019",
          odshare = "Opendoor/Offerpad exposure (\\%)",
          sd_lsqft_stock = "Stock SD log sqft",
          share_post1990 = "Share built post-1990",
          sfr_share = "Single-family share",
          stock_resi = "Residential parcels")

grp_mean <- function(g, v) mean(bal[[v]][bal$group == g], na.rm = TRUE)
grp_sd   <- function(g, v) sd(bal[[v]][bal$group == g],  na.rm = TRUE)
# Imbens-Rubin normalized difference vs top tercile
ndiff <- function(g, v) {
  (grp_mean("t3", v) - grp_mean(g, v)) /
    sqrt((grp_sd("t3", v)^2 + grp_sd(g, v)^2) / 2)
}

fmtb <- function(x, d = 2) formatC(x, format = "f", digits = d)
rows <- map_chr(names(vars), function(v) {
  d <- ifelse(v %in% c("vol19", "stock_resi"), 0, 2)
  paste0(vars[[v]], " & ", fmtb(grp_mean("zero", v), d), " & ",
         fmtb(grp_mean("t1", v), d), " & ", fmtb(grp_mean("t2", v), d), " & ",
         fmtb(grp_mean("t3", v), d), " & ", fmtb(ndiff("t1", v), 2), " & ",
         fmtb(ndiff("zero", v), 2), "\\\\")
})
counts <- bal |> count(group)
nrow_line <- paste0("ZIPs & ",
  counts$n[counts$group == "zero"], " & ", counts$n[counts$group == "t1"],
  " & ", counts$n[counts$group == "t2"], " & ", counts$n[counts$group == "t3"],
  " & & \\\\")

tex <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Exit design: pre-treatment balance across Zillow-exposure groups}",
  "\\label{tab:T6}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccc}",
  "\\toprule",
  " & \\multicolumn{4}{c}{Group means} & \\multicolumn{2}{c}{Norm.\\ diff.\\ vs T3}\\\\",
  "\\cmidrule(lr){2-5}\\cmidrule(lr){6-7}",
  " & Zero & T1 & T2 & T3 (high) & T1 & Zero\\\\",
  "\\midrule",
  rows,
  "\\midrule",
  nrow_line,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.95\\linewidth}\\footnotesize\\vspace{2pt}",
  "\\emph{Notes:} ZIPs in the exit sample (CBSAs with $\\geq 50$ Zillow",
  "purchases), grouped by terciles of positive Zillow exposure (2018Q2--2021Q3",
  "purchase share), with zero-exposure ZIPs separate. Pre-treatment",
  "characteristics measured in 2019 or from the housing stock. Final columns:",
  "Imbens--Rubin normalized differences between the top tercile and the",
  "comparison group ($|\\cdot| < 0.25$ conventionally balanced).",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(tex, path(tables_dir, "T6_balance.tex"))

stats <- list(
  means = map(names(vars), \(v) c(zero = grp_mean("zero", v),
                                  t1 = grp_mean("t1", v), t2 = grp_mean("t2", v),
                                  t3 = grp_mean("t3", v))) |> setNames(names(vars)),
  ndiff_t1   = map_dbl(names(vars), \(v) ndiff("t1", v)) |> setNames(names(vars)),
  ndiff_zero = map_dbl(names(vars), \(v) ndiff("zero", v)) |> setNames(names(vars)),
  counts = counts
)
saveRDS(stats, path(out_dir, "balance_stats.rds"))
message("05c_balance.R complete.")
print(round(cbind(t1 = stats$ndiff_t1, zero = stats$ndiff_zero), 3))
