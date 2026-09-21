# ============================================================
# 07: Referee checks — inference + direct supply test + leads
# Author: Saani Rawat
# Purpose: (a) Permutation (placebo-state) inference for the Prop 19 volume
#              DiD and the sold24 DDD — CA is the single treated cluster, so
#              CRVE SEs are not interpretable (Conley-Taber logic);
#          (b) Direct supply-release test: market sales by estate/trust
#              sellers, CA vs donors (referee C3);
#          (c) Event-study leads/lags for the volume DiD (referee M3);
#          (d) Reconciliation stats: existing-stock ratio, deeds-per-clip.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: _outputs/tables/ref_perm_volume.csv, ref_perm_ddd.csv,
#          ref_estate_seller_did.csv, ref_event_study.csv,
#          ref_reconciliation.csv (+ rds)
# ============================================================

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

fam_classes <- "('family_person','family_other','family_estate')"
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

sy_did <- sy_all |>
  filter(sale_year %in% c(2017:2019, 2022:2023)) |>
  mutate(post = as.integer(sale_year >= 2022))
stopifnot(all(sy_did$n_fam > 0), all(sy_did$n_market > 0))

states <- sort(unique(sy_did$state))
stopifnot("CA" %in% states, length(states) >= 45)

# ---- (a) Permutation inference: volume DiD --------------------------------
perm_coef <- function(panel, treated_state, yvar) {
  d <- panel |> mutate(tr = as.integer(state == treated_state))
  m <- feols(as.formula(paste0("log(", yvar, ") ~ tr:post | state + sale_year")),
             data = d)
  unname(coef(m)["tr:post"])
}

perm_vol <- map_dfr(states, function(s) {
  tibble(
    state = s,
    coef_fam = perm_coef(sy_did, s, "n_fam"),
    coef_mkt = perm_coef(sy_did, s, "n_market")
  )
}) |>
  mutate(coef_net = coef_fam - coef_mkt)

p_rank <- function(df, col) {
  ca_val <- abs(df[[col]][df$state == "CA"])
  mean(abs(df[[col]]) >= ca_val)
}
perm_summary <- tibble(
  outcome = c("fam", "market", "net"),
  ca_coef = c(perm_vol$coef_fam[perm_vol$state == "CA"],
              perm_vol$coef_mkt[perm_vol$state == "CA"],
              perm_vol$coef_net[perm_vol$state == "CA"]),
  perm_p = c(p_rank(perm_vol, "coef_fam"),
             p_rank(perm_vol, "coef_mkt"),
             p_rank(perm_vol, "coef_net")),
  n_states = length(states)
)
message("Volume DiD permutation:")
print(perm_summary)
write_csv_strict(perm_vol, path(tables_out_dir, "ref_perm_volume_all.csv"))
write_csv_strict(perm_summary, path(tables_out_dir, "ref_perm_volume.csv"))
saveRDS(list(all = perm_vol, summary = perm_summary),
        path(out_dir, "ref_perm_volume.rds"))

# ---- (b) Direct supply test: estate/trust-seller market sales -------------
m_est <- feols(log(n_mkt_estate_trust) ~ tr:post | state + sale_year,
               data = sy_did |> mutate(tr = as.integer(state == "CA")),
               cluster = ~state)
perm_est <- map_dfr(states, function(s) {
  tibble(state = s, coef = perm_coef(sy_did, s, "n_mkt_estate_trust"))
})
est_net <- perm_est |>
  left_join(perm_vol |> select(state, coef_mkt), by = "state") |>
  mutate(coef_share = coef - coef_mkt)
est_summary <- tibble(
  ca_coef = unname(coef(m_est)["tr:post"]),
  ca_se_clustered = unname(se(m_est)["tr:post"]),
  perm_p = mean(abs(perm_est$coef) >= abs(perm_est$coef[perm_est$state == "CA"])),
  ca_coef_share = est_net$coef_share[est_net$state == "CA"],
  perm_p_share = mean(abs(est_net$coef_share) >= abs(est_net$coef_share[est_net$state == "CA"])),
  n_states = length(states)
)
message("Estate/trust-seller market-sale DiD (CA x Post; levels and net-of-market):")
print(as.data.frame(est_summary))

ca_est_counts <- sy_all |>
  filter(state == "CA") |>
  select(sale_year, n_mkt_estate_trust, n_mkt_estate, n_market)
print(ca_est_counts)
write_csv_strict(perm_est, path(tables_out_dir, "ref_perm_estate_seller.csv"))
write_csv_strict(est_summary, path(tables_out_dir, "ref_estate_seller_did.csv"))
write_csv_strict(ca_est_counts, path(tables_out_dir, "ref_ca_estate_seller_counts.csv"))
saveRDS(list(model = m_est, perm = perm_est, summary = est_summary),
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
message("Event study (CA x year, ref 2019; SEs are CRVE — see permutation for inference):")
print(es_tab)
write_csv_strict(es_tab, path(tables_out_dir, "ref_event_study.csv"))
saveRDS(m_es, path(out_dir, "ref_event_study.rds"))

# ---- (d) Pre-fit-adjusted placebo inference for monthly counterfactual -----
cf_placebo_summary <- read_csv(path(tables_out_dir, "prop19_cf_placebo_summary.csv"),
                               show_col_types = FALSE)
assert_rows(cf_placebo_summary, 100, "prop19_cf_placebo_summary")

rank_cols <- c("antic", "feb", "post2223")
for (prefix in rank_cols) {
  rank_col <- paste0(prefix, "_rank_ratio")
  p_col <- paste0(prefix, "_p_ratio")
  if (any(abs(cf_placebo_summary[[p_col]] -
              cf_placebo_summary[[rank_col]] / cf_placebo_summary$n_states) > 1e-12)) {
    stop("Rank/p-value mismatch in ", p_col)
  }
}

prefit_ca <- cf_placebo_summary |>
  filter(treated_state == "CA") |>
  select(
    base_window, treated_state, n_states, donor_count,
    pre_rmspe, pre_mae,
    antic_gap, antic_rank_raw, antic_p_raw, antic_ratio,
    antic_rank_ratio, antic_p_ratio,
    feb_gap, feb_rank_raw, feb_p_raw, feb_ratio,
    feb_rank_ratio, feb_p_ratio,
    post2223_gap, post2223_rank_raw, post2223_p_raw, post2223_ratio,
    post2223_rank_ratio, post2223_p_ratio
  )

message("Pre-fit-adjusted placebo inference (CA rows):")
print(as.data.frame(prefit_ca))
write_csv_strict(cf_placebo_summary,
                 path(tables_out_dir, "ref_prop19_prefit_placebo_all.csv"))
write_csv_strict(prefit_ca,
                 path(tables_out_dir, "ref_prop19_prefit_placebo.csv"))

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

# ---- (f) Permutation inference: sold24 DDD --------------------------------
cells <- read_csv(path(tables_out_dir, "prop19_cohort_cells.csv"),
                  show_col_types = FALSE) |>
  filter(!state %in% territories) |>
  mutate(fam = as.integer(grp == "fam"))
assert_rows(cells, 2000, "ref_ddd_cells")

perm_ddd_coef <- function(treated_state) {
  d <- cells |> mutate(tr = as.integer(state == treated_state))
  m <- feols(sold24 ~ tr:post:fam + tr:post | state^grp + ym^grp,
             data = d, weights = ~n)
  unname(coef(m)["tr:post:fam"])
}
perm_ddd <- map_dfr(states, function(s) tibble(state = s, coef = perm_ddd_coef(s)))
ddd_summary <- tibble(
  ca_coef = perm_ddd$coef[perm_ddd$state == "CA"],
  perm_p = mean(abs(perm_ddd$coef) >= abs(perm_ddd$coef[perm_ddd$state == "CA"])),
  n_states = length(states)
)
message("DDD (sold24) permutation:")
print(as.data.frame(ddd_summary))
write_csv_strict(perm_ddd, path(tables_out_dir, "ref_perm_ddd_all.csv"))
write_csv_strict(ddd_summary, path(tables_out_dir, "ref_perm_ddd.csv"))

message("Finished 07_referee_checks at ", Sys.time())
