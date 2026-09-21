# ============================================================
# 04: RQ3 — Proposition 19 as a natural experiment
# Author: Saani Rawat
# Purpose: (a) Retiming/bunching of CA family transfers around the
#              Feb 16, 2021 parent-child exclusion deadline;
#          (b) steady-state DiD of family-transfer volume (CA vs rest);
#          (c) supply release: DiD/DDD on P(next market sale within 24m)
#              for family-transfer cohorts, with market-purchase placebo;
#          (d) composition: absentee-recipient share DiD.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: _outputs/tables/prop19_monthly_series.csv
#          _outputs/tables/prop19_bunching_summary.csv
#          _outputs/prop19_did_volume.rds (+csv)
#          _outputs/prop19_ddd_hazard.rds (+csv)
#          _outputs/prop19_absentee_did.rds (+csv)
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20260609)

log_file <- path(logs_dir, "04_prop19.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 04_prop19 at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(path(data_dir, 'duckdb_tmp'))}'"))

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev <- glue("read_parquet('{sql_quote_path(events_path)}')")

fam_classes <- "('family_person','family_other','family_estate')"
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# ---- (a) Monthly series + bunching ---------------------------------------
monthly <- dbGetQuery(con, glue("
  SELECT state, ym, sale_year, sale_month,
         SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS n_fam,
         SUM(CASE WHEN class = 'family_trust' THEN 1 ELSE 0 END) AS n_trust,
         SUM(CASE WHEN class = 'market_sale' THEN 1 ELSE 0 END)  AS n_market
  FROM {ev}
  WHERE sale_year BETWEEN 2018 AND 2023
  GROUP BY state, ym, sale_year, sale_month
  ORDER BY state, ym
"))
assert_rows(monthly, 2000, "prop19_monthly_series")
write_csv_strict(monthly, path(tables_out_dir, "prop19_monthly_series.csv"))

monthly_states <- monthly |>
  filter(!state %in% territories) |>
  mutate(n_fam = as.numeric(n_fam),
         n_market = as.numeric(n_market))
states <- sort(unique(monthly_states$state))
stopifnot("CA" %in% states, length(states) == 51)

ca <- monthly_states |> filter(state == "CA")
donors <- monthly_states |>
  filter(state != "CA") |>
  group_by(ym, sale_year, sale_month) |>
  summarise(n_fam = sum(n_fam), n_market = sum(n_market), .groups = "drop")

# Counterfactual CA(m) = CA(2019, same calendar month) * donors(m)/donors(2019, same month)
base_ca <- ca |> filter(sale_year == 2019) |> select(sale_month, ca_base = n_fam)
base_dn <- donors |> filter(sale_year == 2019) |> select(sale_month, dn_base = n_fam)

cf <- ca |>
  select(ym, sale_year, sale_month, ca_actual = n_fam) |>
  left_join(donors |> select(ym, dn_actual = n_fam), by = "ym") |>
  left_join(base_ca, by = "sale_month") |>
  left_join(base_dn, by = "sale_month") |>
  mutate(ca_cf = ca_base * dn_actual / dn_base,
         excess = ca_actual - ca_cf)
write_csv_strict(cf, path(tables_out_dir, "prop19_ca_counterfactual_monthly.csv"))

window_antic <- cf |> filter(ym >= 202011, ym <= 202102)
window_post  <- cf |> filter(ym >= 202103, ym <= 202212)
bunch <- tibble(
  excess_antic_window = sum(window_antic$excess),
  antic_actual = sum(window_antic$ca_actual),
  antic_cf = sum(window_antic$ca_cf),
  missing_post_window = -sum(window_post$excess),
  post_actual = sum(window_post$ca_actual),
  post_cf = sum(window_post$ca_cf),
  excess_pct = 100 * sum(window_antic$excess) / sum(window_antic$ca_cf),
  missing_pct = 100 * (-sum(window_post$excess)) / sum(window_post$ca_cf)
)
print(bunch, width = Inf)
write_csv_strict(bunch, path(tables_out_dir, "prop19_bunching_summary.csv"))
saveRDS(bunch, path(out_dir, "prop19_bunching_summary.rds"))

# ---- (a2) Leave-one-out counterfactuals + pre-fit placebo inference --------
cf_base_windows <- list("2019" = 2019L, "2018_2019" = 2018:2019)

build_cf_for_state <- function(treated_state, base_label, base_years) {
  treated <- monthly_states |>
    filter(state == treated_state) |>
    select(ym, sale_year, sale_month, actual_fam = n_fam)

  donor_series <- monthly_states |>
    filter(state != treated_state) |>
    group_by(ym, sale_year, sale_month) |>
    summarise(
      donor_fam = sum(n_fam),
      donor_count = n_distinct(state),
      treated_in_donor = as.integer(any(state == treated_state)),
      .groups = "drop"
    )

  base_treated <- treated |>
    filter(sale_year %in% base_years) |>
    group_by(sale_month) |>
    summarise(base_treated_fam = mean(actual_fam), .groups = "drop")

  base_donor <- donor_series |>
    filter(sale_year %in% base_years) |>
    group_by(sale_month) |>
    summarise(base_donor_fam = mean(donor_fam), .groups = "drop")

  treated |>
    left_join(donor_series, by = c("ym", "sale_year", "sale_month")) |>
    left_join(base_treated, by = "sale_month") |>
    left_join(base_donor, by = "sale_month") |>
    mutate(
      base_window = base_label,
      treated_state = treated_state,
      cf_fam = base_treated_fam * donor_fam / base_donor_fam,
      excess_fam = actual_fam - cf_fam,
      gap_fam = excess_fam / cf_fam,
      .before = 1
    ) |>
    arrange(treated_state, ym)
}

cf_placebo_monthly <- imap_dfr(
  cf_base_windows,
  ~ map_dfr(states, build_cf_for_state, base_label = .y, base_years = .x)
)

stopifnot(
  all(cf_placebo_monthly$donor_count == length(states) - 1),
  all(cf_placebo_monthly$treated_in_donor == 0),
  all(is.finite(cf_placebo_monthly$cf_fam)),
  all(cf_placebo_monthly$cf_fam > 0),
  all(cf_placebo_monthly$base_donor_fam > 0)
)

write_csv_strict(cf_placebo_monthly,
                 path(tables_out_dir, "prop19_cf_placebo_monthly.csv"))

rank_ge <- function(x) {
  vapply(x, function(z) sum(x >= z), integer(1))
}

rank_le <- function(x) {
  vapply(x, function(z) sum(x <= z), integer(1))
}

cf_placebo_summary <- cf_placebo_monthly |>
  group_by(base_window, treated_state) |>
  summarise(
    donor_count = first(donor_count),
    pre_start_ym = 201801L,
    pre_end_ym = 202010L,
    pre_rmspe = sqrt(mean(gap_fam[ym >= pre_start_ym & ym <= pre_end_ym]^2)),
    pre_mae = mean(abs(gap_fam[ym >= pre_start_ym & ym <= pre_end_ym])),
    antic_gap = sum(excess_fam[ym >= 202011 & ym <= 202102]) /
      sum(cf_fam[ym >= 202011 & ym <= 202102]),
    feb_gap = sum(excess_fam[ym == 202102]) / sum(cf_fam[ym == 202102]),
    post2021_gap = sum(excess_fam[ym >= 202103 & ym <= 202212]) /
      sum(cf_fam[ym >= 202103 & ym <= 202212]),
    post2223_gap = sum(excess_fam[ym >= 202201 & ym <= 202312]) /
      sum(cf_fam[ym >= 202201 & ym <= 202312]),
    antic_ratio = antic_gap / pre_rmspe,
    feb_ratio = feb_gap / pre_rmspe,
    post2223_ratio = -post2223_gap / pre_rmspe,
    .groups = "drop"
  ) |>
  group_by(base_window) |>
  mutate(
    n_states = n(),
    antic_rank_raw = rank_ge(antic_gap),
    feb_rank_raw = rank_ge(feb_gap),
    post2223_rank_raw = rank_le(post2223_gap),
    antic_rank_ratio = rank_ge(antic_ratio),
    feb_rank_ratio = rank_ge(feb_ratio),
    post2223_rank_ratio = rank_ge(post2223_ratio),
    antic_p_raw = antic_rank_raw / n_states,
    feb_p_raw = feb_rank_raw / n_states,
    post2223_p_raw = post2223_rank_raw / n_states,
    antic_p_ratio = antic_rank_ratio / n_states,
    feb_p_ratio = feb_rank_ratio / n_states,
    post2223_p_ratio = post2223_rank_ratio / n_states
  ) |>
  ungroup() |>
  relocate(n_states, .after = treated_state)

write_csv_strict(cf_placebo_summary,
                 path(tables_out_dir, "prop19_cf_placebo_summary.csv"))
saveRDS(list(monthly = cf_placebo_monthly, summary = cf_placebo_summary),
        path(out_dir, "prop19_cf_placebo.rds"))

sensitivity_windows <- tribble(
  ~window, ~start_ym, ~end_ym, ~direction,
  "anticipation", 202011L, 202102L, "positive",
  "february", 202102L, 202102L, "positive",
  "post_202103_202212", 202103L, 202212L, "negative",
  "steady_2022_2023", 202201L, 202312L, "negative"
)

window_stats <- pmap_dfr(sensitivity_windows, function(window, start_ym, end_ym, direction) {
  cf_placebo_monthly |>
    filter(ym >= start_ym, ym <= end_ym) |>
    group_by(base_window, treated_state) |>
    summarise(
      window = window,
      start_ym = start_ym,
      end_ym = end_ym,
      direction = direction,
      gap = sum(excess_fam) / sum(cf_fam),
      .groups = "drop"
    )
})

prop19_window_sensitivity <- window_stats |>
  left_join(
    cf_placebo_summary |> select(base_window, treated_state, n_states, pre_rmspe),
    by = c("base_window", "treated_state")
  ) |>
  mutate(
    directional_gap = if_else(direction == "positive", gap, -gap),
    ratio = directional_gap / pre_rmspe
  ) |>
  group_by(base_window, window) |>
  mutate(
    raw_rank = rank_ge(directional_gap),
    raw_p = raw_rank / n_states,
    ratio_rank = rank_ge(ratio),
    ratio_p = ratio_rank / n_states
  ) |>
  ungroup() |>
  filter(treated_state == "CA") |>
  transmute(
    base_window, window, start_ym, end_ym, direction,
    ca_gap = gap,
    ca_ratio = ratio,
    raw_rank, raw_p, ratio_rank, ratio_p, n_states
  )

write_csv_strict(prop19_window_sensitivity,
                 path(tables_out_dir, "prop19_window_sensitivity.csv"))
saveRDS(prop19_window_sensitivity,
        path(out_dir, "prop19_window_sensitivity.rds"))

# ---- (b) Steady-state volume DiD ------------------------------------------
# ln(family transfers) state x year; pre = 2017-2019, post = 2022-2023;
# transition years 2020-2021 excluded (anticipation + deadline retiming).
sy <- dbGetQuery(con, glue("
  SELECT state, sale_year,
         SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS n_fam,
         SUM(CASE WHEN class IN ('family_person','family_other','family_estate',
                                 'family_trust') THEN 1 ELSE 0 END) AS n_fam_wtrust,
         SUM(CASE WHEN class = 'market_sale' THEN 1 ELSE 0 END)  AS n_market
  FROM {ev}
  WHERE sale_year IN (2017, 2018, 2019, 2022, 2023)
  GROUP BY state, sale_year
"))
sy <- sy |>
  filter(!state %in% c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")) |>
  mutate(post = as.integer(sale_year >= 2022),
         ca = as.integer(state == "CA"))
stopifnot(nrow(sy) > 200, sum(sy$ca) == 5)
# log() of a zero cell would drop the observation silently (r-review M6)
stopifnot(all(sy$n_fam > 0), all(sy$n_market > 0), all(sy$n_fam_wtrust > 0))

m_vol  <- feols(log(n_fam) ~ ca:post | state + sale_year, data = sy, cluster = ~state)
m_volt <- feols(log(n_fam_wtrust) ~ ca:post | state + sale_year, data = sy, cluster = ~state)
m_mkt  <- feols(log(n_market) ~ ca:post | state + sale_year, data = sy, cluster = ~state)
print(etable(m_vol, m_volt, m_mkt))
saveRDS(list(fam = m_vol, fam_wtrust = m_volt, market_placebo = m_mkt),
        path(out_dir, "prop19_did_volume.rds"))
write_csv_strict(
  tibble(
    model = c("fam", "fam_wtrust", "market_placebo"),
    coef = c(coef(m_vol)["ca:post"], coef(m_volt)["ca:post"], coef(m_mkt)["ca:post"]),
    se = c(se(m_vol)["ca:post"], se(m_volt)["ca:post"], se(m_mkt)["ca:post"]),
    n = c(nobs(m_vol), nobs(m_volt), nobs(m_mkt))
  ),
  path(tables_out_dir, "prop19_did_volume.csv")
)

# ---- (c) Supply release: sold-within-24m DiD + market placebo -------------
# Cohorts: PRE = transfers 2017-01..2018-12; POST = 2021-07..2022-06.
# All outcome windows close before the 2024-06-30 censor.
dbExecute(con, glue("
  CREATE OR REPLACE TEMP VIEW ev_dated AS
  SELECT *,
    make_date(
      CAST(sale_raw / 10000 AS INTEGER),
      CAST((sale_raw / 100) % 100 AS INTEGER),
      CASE WHEN CAST(sale_raw % 100 AS INTEGER) = 0 THEN 1
           ELSE CAST(sale_raw % 100 AS INTEGER) END
    ) AS sale_date
  FROM {ev}
  WHERE CAST((sale_raw / 100) % 100 AS INTEGER) BETWEEN 1 AND 12
"))

dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE cohort19 AS
  SELECT clip, state, ym, sale_date, absentee_buyer,
         CASE WHEN class IN {fam_classes} THEN 'fam' ELSE 'market' END AS grp,
         CASE WHEN ym BETWEEN 202107 AND 202206 THEN 1 ELSE 0 END AS post
  FROM ev_dated
  WHERE (ym BETWEEN 201701 AND 201812 OR ym BETWEEN 202107 AND 202206)
    AND (class IN {fam_classes} OR class = 'market_sale')
    AND corp_buyer = 0
"))

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE market_sales AS
  SELECT clip, sale_date AS msale_date FROM ev_dated WHERE class = 'market_sale'
")

cells <- dbGetQuery(con, "
  WITH spells AS (
    -- clip MUST be in the select list: GROUP BY ALL without it collapses
    -- all same-date/state/group events into one spell (caught 2026-06-09:
    -- produced sold24 = 0.65 vs true ~0.10 and 40x undercounted cohorts)
    SELECT c.clip, c.state, c.ym, c.grp, c.post, c.absentee_buyer, c.sale_date,
           MIN(m.msale_date) AS next_market_date
    FROM cohort19 c
    LEFT JOIN market_sales m
      ON m.clip = c.clip AND m.msale_date > c.sale_date
    GROUP BY ALL
  )
  SELECT state, ym, grp, post,
    COUNT(*) AS n,
    AVG(CASE WHEN next_market_date IS NOT NULL
              AND next_market_date <= sale_date + INTERVAL 24 MONTH
             THEN 1.0 ELSE 0 END) AS sold24,
    AVG(CASE WHEN absentee_buyer IS NOT NULL THEN CAST(absentee_buyer AS DOUBLE) END) AS absentee_share,
    SUM(CASE WHEN absentee_buyer IS NOT NULL THEN 1 ELSE 0 END) AS n_absentee_obs
  FROM spells
  GROUP BY state, ym, grp, post
")
assert_rows(cells, 2000, "prop19_cohort_cells")
cells <- cells |>
  filter(!state %in% c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")) |>
  mutate(ca = as.integer(state == "CA"))
write_csv_strict(cells, path(tables_out_dir, "prop19_cohort_cells.csv"))

cells_fam <- cells |> filter(grp == "fam")
cells_mkt <- cells |> filter(grp == "market")

# Magnitude sanity: national family transfers run >1M/yr, so the pooled
# 3-year cohort must exceed 2M spells; same order for market sales.
stopifnot(sum(cells_fam$n) > 2e6, sum(cells_mkt$n) > 5e6)
message("Cohort sizes — fam: ", format(sum(cells_fam$n), big.mark = ","),
        " | market: ", format(sum(cells_mkt$n), big.mark = ","))

m_haz_fam <- feols(sold24 ~ ca:post | state + ym, data = cells_fam,
                   weights = ~n, cluster = ~state)
m_haz_mkt <- feols(sold24 ~ ca:post | state + ym, data = cells_mkt,
                   weights = ~n, cluster = ~state)
# Triple difference: pool, allow group-specific FEs (ca:fam and post:fam
# are absorbed by state^grp and ym^grp; excluded to keep etable clean)
cells_pooled <- cells |>
  mutate(fam = as.integer(grp == "fam"))
m_ddd <- feols(sold24 ~ ca:post:fam + ca:post
               | state^grp + ym^grp, data = cells_pooled,
               weights = ~n, cluster = ~state)
print(etable(m_haz_fam, m_haz_mkt, m_ddd))
saveRDS(list(fam = m_haz_fam, market_placebo = m_haz_mkt, ddd = m_ddd),
        path(out_dir, "prop19_ddd_hazard.rds"))

# Raw 2x2 for transparency
raw22 <- cells_fam |>
  group_by(ca, post) |>
  summarise(sold24 = weighted.mean(sold24, n), n = sum(n), .groups = "drop")
print(raw22)
write_csv_strict(raw22, path(tables_out_dir, "prop19_raw_2x2_fam.csv"))
raw22_mkt <- cells_mkt |>
  group_by(ca, post) |>
  summarise(sold24 = weighted.mean(sold24, n), n = sum(n), .groups = "drop")
write_csv_strict(raw22_mkt, path(tables_out_dir, "prop19_raw_2x2_market.csv"))

# ---- (d) Composition: absentee recipient share DiD ------------------------
cells_abs <- cells_fam |> filter(n_absentee_obs > 0, !is.na(absentee_share))
m_abs <- feols(absentee_share ~ ca:post | state + ym, data = cells_abs,
               weights = ~n_absentee_obs, cluster = ~state)
print(etable(m_abs))
saveRDS(m_abs, path(out_dir, "prop19_absentee_did.rds"))
write_csv_strict(
  tibble(coef = coef(m_abs)["ca:post"], se = se(m_abs)["ca:post"], n = nobs(m_abs)),
  path(tables_out_dir, "prop19_absentee_did.csv")
)

message("Finished 04_prop19 at ", Sys.time())
