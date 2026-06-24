#' 05b_exit_donut.R — Donut variant of the exit DiD
#'
#' Exposure is built from realized Zillow purchases, so the event study
#' mechanically shows the machine's 2021 ramp-up into the reference quarter
#' (2021Q3 = peak buying). This variant drops the ramp quarters 2021Q1–Q3
#' so the pre period is 2019Q1–2020Q4 (normal machine operation) and the
#' post period starts 2021Q4. Writes out_dir/exit_donut.rds.

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))
set.seed(20260610)

W_START <- round(2019.00 * 4)
W_END   <- round(2024.25 * 4)
EXIT_QID <- round(EXIT_YQ * 4)
EXPO_START <- round(ZILLOW_EXPO_START * 4)
EXPO_END   <- round(ZILLOW_EXPO_END * 4)
RAMP_QIDS <- (round(2021.00 * 4)):(EXIT_QID - 1L)   # 2021Q1–2021Q3

panel <- read_parquet(path(data_dir, "zip_quarter_panel.parquet"))
disp  <- read_parquet(path(data_dir, "zip_quarter_dispersion.parquet"))
zcov  <- read_parquet(path(data_dir, "zip_covariates.parquet"))

df <- panel |>
  left_join(disp |> select(state, zip5, yq, n_hedonic, disp_sd, disp_iqr,
                           left_tail),
            by = c("state", "zip5", "yq")) |>
  left_join(zcov |> select(state, zip5, cbsa, stock_resi, sd_lsqft_stock),
            by = c("state", "zip5")) |>
  filter(!is.na(cbsa), stock_resi >= 500) |>
  mutate(qid = as.integer(round(yq * 4)),
         zid = paste0(state, "_", zip5))

expo <- df |>
  filter(qid >= EXPO_START, qid <= EXPO_END) |>
  group_by(state, zip5, cbsa) |>
  summarise(sales_w = sum(n_sales), z_buys = sum(n_zillow_buy),
            od_buys = sum(n_opendoor_buy) + sum(n_offerpad_buy),
            .groups = "drop") |>
  filter(sales_w >= 50) |>
  mutate(zshare = 100 * z_buys / sales_w, odshare = 100 * od_buys / sales_w)
w99z  <- quantile(expo$zshare[expo$zshare > 0],  0.99, na.rm = TRUE)
w99od <- quantile(expo$odshare[expo$odshare > 0], 0.99, na.rm = TRUE)
expo <- expo |>
  mutate(zshare = pmin(zshare, w99z), odshare = pmin(odshare, w99od))
z_cbsas <- expo |> group_by(cbsa) |> summarise(zb = sum(z_buys)) |>
  filter(zb >= 50) |> pull(cbsa)

es_df <- df |>
  inner_join(expo |> select(state, zip5, zshare, odshare, sales_w),
             by = c("state", "zip5")) |>
  filter(cbsa %in% z_cbsas, qid >= W_START, qid <= W_END,
         !(qid %in% RAMP_QIDS)) |>
  mutate(post = as.integer(qid >= EXIT_QID),
         lvol = log(n_sales), lmedp = log(med_price))

outcomes <- c(lvol = "lvol", disp = "disp_sd", iqr = "disp_iqr",
              lmedp = "lmedp", ltail = "left_tail")
donut <- list()
for (nm in names(outcomes)) {
  y <- outcomes[[nm]]
  m <- feols(as.formula(paste0(y, " ~ zshare:post + odshare:post | zid + cbsa^qid")),
             data = es_df, cluster = ~cbsa, notes = FALSE)
  donut[[nm]] <- list(tidy = broom::tidy(m, conf.int = TRUE), nobs = nobs(m))
  b <- broom::tidy(m)
  message(nm, ": donut zshare x post = ",
          round(b$estimate[grepl("zshare", b$term)][1], 5),
          " (se ", round(b$std.error[grepl("zshare", b$term)][1], 5), ")")
}
saveRDS(donut, path(out_dir, "exit_donut.rds"))
message("05b_exit_donut.R complete.")
