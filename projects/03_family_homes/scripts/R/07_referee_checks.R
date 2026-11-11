# ============================================================
# 07: Referee checks — inference + direct supply test + leads
# Author: Saani Rawat
# Purpose: Estimate the headline volume DDD, direct supply comparison,
#          annual event study, and deed-count reconciliation.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: prop19_ddd_volume.csv/rds, ref_estate_seller_did.csv/rds,
#          ref_event_study.csv/rds, ref_reconciliation.csv,
#          ref_state_year_panel.csv
# Log:     10/22/2026: added the pooled family-vs-market triple difference
#          for volumes, the paper's headline post-reform estimate.
# ============================================================

# 11/04/2026: removed state-reassignment inference; retained model specifications.
# 11/04/2026: restored other interfamily to the main family proxy.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20260610)

log_file <- path(logs_dir, "07_referee_checks.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 07_referee_checks at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev <- glue("read_parquet('{sql_quote_path(events_path)}')")

fam_classes <- family_classes_sql
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# ---- shared state-year panel (2017-2023, all years for event study) -------
sy_all <- dbGetQuery(con, glue("
  SELECT state, sale_year,
         SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS n_fam,
         SUM(CASE WHEN class = 'market_sale' THEN 1 ELSE 0 END)  AS n_market,
         SUM(CASE WHEN class = 'market_sale'
                   AND (estate_kw OR trust_kw) THEN 1 ELSE 0 END) AS n_mkt_estate_trust,
         SUM(CASE WHEN class = 'market_sale' AND estate_kw THEN 1 ELSE 0 END) AS n_mkt_estate
  FROM {ev}
  WHERE sale_year BETWEEN 2017 AND 2023
  GROUP BY state, sale_year
")) |>
  filter(!state %in% territories)
assert_rows(sy_all, 300, "ref_state_year_panel")

write_csv_strict(sy_all, path(tables_out_dir, "ref_state_year_panel.csv"))

sy_did <- sy_all |>
  filter(sale_year %in% c(2017:2019, 2022:2023)) |>
  mutate(post = as.integer(sale_year >= 2022))
stopifnot(all(sy_did$n_fam > 0), all(sy_did$n_market > 0))

states <- sort(unique(sy_did$state))
stopifnot("CA" %in% states, length(states) >= 45)

# ---- Headline triple difference: family vs market deeds ----
# Group-specific state and year effects make the DDD equal the difference
# between the separate family and market DiD coefficients.
sy_stacked <- sy_did |>
  select(state, sale_year, post, n_fam, n_market) |>
  pivot_longer(c(n_fam, n_market), names_to = "grp", values_to = "n") |>
  mutate(fam = as.integer(grp == "n_fam"),
         ca = as.integer(state == "CA"))

m_ddd_vol <- feols(log(n) ~ ca:post:fam + ca:post | state^grp + sale_year^grp,
                   data = sy_stacked, cluster = ~state)
print(summary(m_ddd_vol))

ddd_vol_coef <- unname(coef(m_ddd_vol)["ca:post:fam"])
volume_models <- readRDS(path(out_dir, "prop19_did_volume.rds"))
net_ca <- unname(coef(volume_models$fam)["ca:post"] -
                 coef(volume_models$market_placebo)["ca:post"])
stopifnot(abs(ddd_vol_coef - net_ca) < 1e-8)

ddd_vol_summary <- tibble(
  estimate = ddd_vol_coef,
  pct_effect = 100 * (exp(ddd_vol_coef) - 1),
  crve_se_state = unname(se(m_ddd_vol)["ca:post:fam"]),
  n_states = length(states),
  nobs = nobs(m_ddd_vol)
)
message("Volume triple difference (family vs market, CA x Post):")
print(ddd_vol_summary, width = Inf)
write_csv_strict(ddd_vol_summary, path(tables_out_dir, "prop19_ddd_volume.csv"))
saveRDS(list(model = m_ddd_vol, summary = ddd_vol_summary),
        path(out_dir, "prop19_ddd_volume.rds"))

# ---- (b) Direct supply test: estate/trust-seller market sales -------------
m_est <- feols(log(n_mkt_estate_trust) ~ tr:post | state + sale_year,
               data = sy_did |> mutate(tr = as.integer(state == "CA")),
               cluster = ~state)
m_market <- feols(log(n_market) ~ tr:post | state + sale_year,
                  data = sy_did |> mutate(tr = as.integer(state == "CA")),
                  cluster = ~state)
est_summary <- tibble(
  ca_coef = unname(coef(m_est)["tr:post"]),
  ca_se_clustered = unname(se(m_est)["tr:post"]),
  ca_coef_share = unname(coef(m_est)["tr:post"] - coef(m_market)["tr:post"]),
  n_states = length(states)
)
message("Estate/trust-seller market-sale DiD (CA x Post; levels and net-of-market):")
print(est_summary)

ca_est_counts <- sy_all |>
  filter(state == "CA") |>
  select(sale_year, n_mkt_estate_trust, n_mkt_estate, n_market)
print(ca_est_counts)
write_csv_strict(est_summary, path(tables_out_dir, "ref_estate_seller_did.csv"))
write_csv_strict(ca_est_counts, path(tables_out_dir, "ref_ca_estate_seller_counts.csv"))
saveRDS(list(model = m_est, summary = est_summary),
        path(out_dir, "ref_estate_seller.rds"))

# ---- (c) Event-study leads/lags (volume) ----------------------------------
es_dat <- sy_all |> mutate(ca = as.integer(state == "CA"))
m_es <- feols(log(n_fam) ~ i(sale_year, ca, ref = 2019) | state + sale_year,
              data = es_dat, cluster = ~state)
ct <- as.data.frame(coeftable(m_es))
es_tab <- ct |>
  rownames_to_column("term") |>
  filter(grepl("sale_year::", term)) |>
  mutate(year = as.integer(gsub("sale_year::(\\d+):ca", "\\1", term))) |>
  select(year, estimate = Estimate, std.error = `Std. Error`)
message("Event study (CA x year, ref 2019; conventional state-clustered SEs; one treated state):")
print(es_tab)
write_csv_strict(es_tab, path(tables_out_dir, "ref_event_study.csv"))
saveRDS(m_es, path(out_dir, "ref_event_study.rds"))

# ---- (e) Reconciliation stats ---------------------------------------------
recon <- dbGetQuery(con, glue("
  SELECT
    SUM(CASE WHEN class IN {fam_classes} AND sale_year <= 2023 THEN 1 ELSE 0 END) AS fam_broad_0723,
    SUM(CASE WHEN class = 'market_sale' AND sale_year <= 2023 THEN 1 ELSE 0 END)  AS market_0723,
    SUM(CASE WHEN class = 'market_sale' AND sale_year <= 2023
              AND coalesce(new_construction, 0) <> 1 THEN 1 ELSE 0 END)           AS market_existing_0723,
    COUNT(DISTINCT CASE WHEN class IN {fam_classes} AND sale_year <= 2023
                        THEN clip END)                                            AS fam_clips_0723
  FROM {ev}
  WHERE sale_year BETWEEN 2007 AND 2023
"))
recon <- recon |>
  mutate(
    ratio_all = fam_broad_0723 / market_0723,
    ratio_existing = fam_broad_0723 / market_existing_0723,
    deeds_per_fam_clip = fam_broad_0723 / fam_clips_0723
  )
message("Reconciliation:")
print(as.data.frame(recon))
write_csv_strict(recon, path(tables_out_dir, "ref_reconciliation.csv"))

message("Finished 07_referee_checks at ", Sys.time())
