#' 07_figures.R — Manuscript figures
#'
#'   figures_dir/F1_ibuyer_volume.pdf — national iBuyer purchases by firm-quarter
#'   figures_dir/F2_sorting.pdf       — iBuyer share vs segment heterogeneity (P1)
#'   figures_dir/F3_entry_es.pdf      — entry event studies (volume, dispersion)
#'   figures_dir/F4_exit_es.pdf       — exit event studies (Zillow vs OD exposure)

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))

FIRM_LABELS <- c(opendoor = "Opendoor", zillow = "Zillow Offers",
                 offerpad = "Offerpad", redfin = "RedfinNow")
FIRM_COLORS <- c(`Opendoor` = "#012169", `Zillow Offers` = "#b91c1c",
                 `Offerpad` = "#f2a900", `RedfinNow` = "#15803d")

# ------------------------------------------------- F1: national volume by firm
ibuyer <- read_parquet(path(data_dir, "ibuyer_deeds.parquet"))
vol <- ibuyer |>
  filter(!is.na(buyer_firm), !is.na(yq), yq >= 2014, yq <= 2024.25) |>
  count(buyer_firm, yq) |>
  mutate(firm = factor(FIRM_LABELS[buyer_firm], levels = FIRM_LABELS))

f1 <- ggplot(vol, aes(yq, n, color = firm)) +
  geom_line(linewidth = 0.9) +
  geom_vline(xintercept = 2021.75, linetype = "dashed", color = "grey40") +
  annotate("text", x = 2021.85, y = max(vol$n) * 0.95, hjust = 0, size = 3.2,
           label = "Zillow Offers shutdown\n(Nov 2, 2021)", color = "grey25") +
  scale_color_manual(values = FIRM_COLORS, name = NULL) +
  labs(x = NULL, y = "Purchases per quarter") +
  theme(legend.position = "bottom")
ggsave(path(figures_dir, "F1_ibuyer_volume.pdf"), f1,
       width = 9, height = 4.8, bg = "transparent")

# ------------------------------------------------- F2: sorting on heterogeneity
panel <- read_parquet(path(data_dir, "zip_quarter_panel.parquet"))
zcov  <- read_parquet(path(data_dir, "zip_covariates.parquet"))

zip_ib <- panel |>
  filter(yq >= 2018.25, yq <= 2021.50) |>
  group_by(state, zip5) |>
  summarise(sales = sum(n_sales), ib = sum(n_ib_buy), .groups = "drop") |>
  filter(sales >= 50) |>
  mutate(ib_share = 100 * ib / sales) |>
  inner_join(zcov |> select(state, zip5, sd_lsqft_stock, stock_resi),
             by = c("state", "zip5")) |>
  filter(!is.na(sd_lsqft_stock), stock_resi >= 500)

bins <- zip_ib |>
  mutate(bin = ntile(sd_lsqft_stock, 20)) |>
  group_by(bin) |>
  summarise(x = mean(sd_lsqft_stock), y = mean(ib_share), .groups = "drop")

f2 <- ggplot(bins, aes(x, y)) +
  geom_point(size = 2.4, color = "#012169") +
  geom_smooth(data = zip_ib, aes(sd_lsqft_stock, ib_share),
              method = "lm", se = TRUE, color = "#b91c1c",
              linewidth = 0.7, fill = "grey85") +
  labs(x = "ZIP housing-stock heterogeneity (SD of log sqft)",
       y = "iBuyer share of transactions, 2018Q2–2021Q3 (%)")
ggsave(path(figures_dir, "F2_sorting.pdf"), f2,
       width = 8, height = 4.8, bg = "transparent")

saveRDS(
  list(slope = coef(lm(ib_share ~ sd_lsqft_stock, data = zip_ib))[2],
       n_zips = nrow(zip_ib),
       cor = cor(zip_ib$ib_share, zip_ib$sd_lsqft_stock)),
  path(out_dir, "sorting_stats.rds")
)

# ------------------------------------------------- F3: entry event studies
es <- readRDS(path(out_dir, "entry_es_models.rds"))

es_plot_df <- function(coefs, label) {
  coefs |>
    filter(grepl("^qid::", term) | grepl("cohort", term)) |>
    mutate(rel = suppressWarnings(as.numeric(gsub(".*::(-?\\d+).*", "\\1", term)))) |>
    filter(!is.na(rel), abs(rel) <= 12) |>
    transmute(rel, estimate, conf.low, conf.high, outcome = label)
}
df3 <- bind_rows(
  es_plot_df(es$lvol$coefs, "log volume"),
  es_plot_df(es$disp$coefs, "residual SD")
)
f3 <- ggplot(df3, aes(rel / 4, estimate)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = -0.125, linetype = "dashed", color = "grey60") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.3, color = "#012169") +
  facet_wrap(~outcome, scales = "free_y") +
  labs(x = "Years since iBuyer entry", y = "Event-study coefficient")
ggsave(path(figures_dir, "F3_entry_es.pdf"), f3,
       width = 9.5, height = 4.4, bg = "transparent")

# ------------------------------------------------- F4: exit event studies
ex <- readRDS(path(out_dir, "exit_es_models.rds"))
EXIT_QID <- round(2021.75 * 4)

exit_plot_df <- function(tidy_tbl, outcome_label) {
  tidy_tbl |>
    filter(grepl("^qid::\\d+:zshare$", term)) |>
    mutate(qid = as.numeric(gsub("^qid::(\\d+):zshare$", "\\1", term))) |>
    filter(!is.na(qid)) |>
    transmute(yq = qid / 4, estimate, conf.low, conf.high,
              outcome = outcome_label)
}
df4 <- bind_rows(
  exit_plot_df(ex$disp$tidy, "residual SD"),
  exit_plot_df(ex$lvol$tidy, "log volume")
)
f4 <- ggplot(df4, aes(yq, estimate)) +
  geom_hline(yintercept = 0, color = "grey60") +
  geom_vline(xintercept = 2021.75, linetype = "dashed", color = "#b91c1c") +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  size = 0.3, color = "#012169") +
  facet_wrap(~outcome, scales = "free_y") +
  labs(x = NULL, y = "Zillow exposure × quarter coefficient")
ggsave(path(figures_dir, "F4_exit_es.pdf"), f4,
       width = 9.5, height = 4.4, bg = "transparent")

message("07_figures.R complete — 4 figures written.")
