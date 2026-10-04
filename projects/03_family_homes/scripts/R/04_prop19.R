# ============================================================
# 04: RQ3 — Proposition 19: retiming, dose-response, and allocation
# Author: Saani Rawat
# Purpose: Does removing an inherited tax privilege change who ends up
#          owning the home, or only the timing and legal form of transfer?
#          (1) Aggregate retiming: CA family-transfer bunching before the
#              Feb 16, 2021 deadline and the permanent post-deadline gap
#              (2019-scaled counterfactual + leave-one-out placebos), with
#              the retiming ratio and SCM inference from 09.
#          (2) Cross-state triple difference: long-held x CA x post family
#              transfers, with permutation and Conley-Taber inference.
#          (3) Appendix (legacy): state-year volume DiD, conditional sold24
#              cohort DDD, deed-time absentee DiD.
#          (4)-(5) Core design: fixed pre-policy CA parcel risk set with a
#              predetermined annual tax wedge (04a). Poisson hazard of
#              family transfer on wedge x period, within county-month and
#              tenure-HOX-month; quarterly event study; bin dose-response
#              by occupancy (2008 homeowner's exemption) and Prop 19 cap.
#          (6) Competing risks: where deterred family transfers went
#              (market sale / trust-retitle / other deed / no recorded deed).
#          (7) Placebos and robustness.
#          (8) Headline numbers and sufficient-statistic welfare.
# Inputs:  data/derived/03_family_homes/events.parquet
#          data/derived/03_family_homes/ca_parcel_wedge.parquet (04a)
#          _outputs/tables/prop19_scm_inference.csv (09; optional)
#          scripts/R/inputs/welfare_cpi.csv
# Outputs: _outputs/tables/prop19_monthly_series.csv, prop19_bunching_summary.csv,
#          prop19_cf_placebo_*.csv, prop19_window_sensitivity.csv (unchanged)
#          _outputs/tables/prop19_retiming_summary.csv
#          _outputs/tables/prop19_ddd_longheld_*.csv
#          _outputs/tables/prop19_did_volume.csv, prop19_cohort_cells.csv,
#          prop19_raw_2x2_*.csv, prop19_absentee_did.csv (appendix; unchanged)
#          _outputs/tables/prop19_wedge_cells_*.csv
#          _outputs/tables/prop19_dose_*.csv, prop19_exits_decomposition_*.csv,
#          prop19_robustness_*.csv, prop19_headlines.csv
#          manuscript/tables/prop19_numbers.tex
#          manuscript/figures/F_prop19_dose_*.pdf/.png
# Log:     10/02/2026: moved event aggregation and spells into R memory.
#          10/02/2026: rebuilt around parcel wedge dose-response, competing
#          risks, cross-state DDD, headline numbers. Aggregate sections and
#          appendix outputs kept byte-identical in logic and filenames.
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_inference_helpers.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20260609)

log_file <- path(logs_dir, "04_prop19.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 04_prop19 at ", Sys.time())

#============================================#
# Settings ----
#============================================#

# policy windows (YYYYMM): passage Nov 3 2020; parent-child deadline Feb 16 2021
antic_start_ym <- 202011L
antic_end_ym <- 202102L
post21_end_ym <- 202112L      # Mar-Dec 2021: post-deadline transition
steady_start_ym <- 202201L    # 2022 onward: steady state
es_ref_quarter <- "2020Q3"    # last clean quarter before anticipation

# fake reform for the pre-period placebo (two years earlier)
fake_antic <- c(201811L, 201902L)
fake_post_start <- 201903L
fake_sample_end <- 202010L

# parcel design
main_cohort <- "d2017"
robust_cohort <- "d2019"
censor_ym <- 202406L
reference_bin <- "w0_0.5k"
poor_sqft_counties <- c("06015", "06105", "06063", "06043", "06049", "06023",
                        "06025", "06045", "06069", "06113")  # <15% sqft coverage

# inference
B_boot <- as.integer(Sys.getenv("PROP19_BOOT", "199"))

# welfare (sufficient statistics)
pv_rate <- 0.03
pv_horizon <- 30L

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev_data <- arrow::read_parquet(events_path, as_data_frame = TRUE)
message("Loaded ", format(nrow(ev_data), big.mark = ","), " events into R memory")

fam_classes <- c("family_person", "family_other", "family_estate")
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# ============================================================
# 1. Monthly series and bunching ----
# ============================================================
monthly <- ev_data %>%
  filter(between(sale_year, 2018L, 2023L)) %>%
  group_by(state, ym, sale_year, sale_month) %>%
  summarize(
    n_fam = sum(class %in% fam_classes),
    n_trust = sum(class == "family_trust", na.rm = TRUE),
    n_market = sum(class == "market_sale", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(state, ym)
assert_rows(monthly, 2000, "prop19_monthly_series")
write_csv_strict(monthly, path(tables_out_dir, "prop19_monthly_series.csv"))

monthly_states <- monthly %>%
  filter(!state %in% territories) %>%
  mutate(n_fam = as.numeric(n_fam),
         n_market = as.numeric(n_market))
states <- sort(unique(monthly_states$state))

ca <- monthly_states %>% filter(state == "CA")
donors <- monthly_states %>%
  filter(state != "CA") %>%
  group_by(ym, sale_year, sale_month) %>%
  summarise(n_fam = sum(n_fam), n_market = sum(n_market), .groups = "drop")

# Counterfactual CA(m) = CA(2019, same calendar month) * donors(m)/donors(2019, same month)
base_ca <- ca %>% filter(sale_year == 2019) %>% select(sale_month, ca_base = n_fam)
base_dn <- donors %>% filter(sale_year == 2019) %>% select(sale_month, dn_base = n_fam)

cf <- ca %>%
  select(ym, sale_year, sale_month, ca_actual = n_fam) %>%
  left_join(donors %>% select(ym, dn_actual = n_fam), by = "ym") %>%
  left_join(base_ca, by = "sale_month") %>%
  left_join(base_dn, by = "sale_month") %>%
  mutate(ca_cf = ca_base * dn_actual / dn_base,
         excess = ca_actual - ca_cf)
write_csv_strict(cf, path(tables_out_dir, "prop19_ca_counterfactual_monthly.csv"))

window_antic <- cf %>% filter(ym >= 202011, ym <= 202102)
window_post  <- cf %>% filter(ym >= 202103, ym <= 202212)
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

# ============================================================
# 1b. Leave-one-out counterfactuals and pre-fit placebo inference ----
# ============================================================
cf_base_windows <- list("2019" = 2019L, "2018_2019" = 2018:2019)

build_cf_for_state <- function(treated_state, base_label, base_years) {
  treated <- monthly_states %>%
    filter(state == treated_state) %>%
    select(ym, sale_year, sale_month, actual_fam = n_fam)

  donor_series <- monthly_states %>%
    filter(state != treated_state) %>%
    group_by(ym, sale_year, sale_month) %>%
    summarise(
      donor_fam = sum(n_fam),
      donor_count = n_distinct(state),
      treated_in_donor = as.integer(any(state == treated_state)),
      .groups = "drop"
    )

  base_treated <- treated %>%
    filter(sale_year %in% base_years) %>%
    group_by(sale_month) %>%
    summarise(base_treated_fam = mean(actual_fam), .groups = "drop")

  base_donor <- donor_series %>%
    filter(sale_year %in% base_years) %>%
    group_by(sale_month) %>%
    summarise(base_donor_fam = mean(donor_fam), .groups = "drop")

  treated %>%
    left_join(donor_series, by = c("ym", "sale_year", "sale_month")) %>%
    left_join(base_treated, by = "sale_month") %>%
    left_join(base_donor, by = "sale_month") %>%
    mutate(
      base_window = base_label,
      treated_state = treated_state,
      cf_fam = base_treated_fam * donor_fam / base_donor_fam,
      excess_fam = actual_fam - cf_fam,
      gap_fam = excess_fam / cf_fam,
      .before = 1
    ) %>%
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

cf_placebo_summary <- cf_placebo_monthly %>%
  group_by(base_window, treated_state) %>%
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
  ) %>%
  group_by(base_window) %>%
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
  ) %>%
  ungroup() %>%
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
  cf_placebo_monthly %>%
    filter(ym >= start_ym, ym <= end_ym) %>%
    group_by(base_window, treated_state) %>%
    summarise(
      window = window,
      start_ym = start_ym,
      end_ym = end_ym,
      direction = direction,
      gap = sum(excess_fam) / sum(cf_fam),
      .groups = "drop"
    )
})

prop19_window_sensitivity <- window_stats %>%
  left_join(
    cf_placebo_summary %>% select(base_window, treated_state, n_states, pre_rmspe),
    by = c("base_window", "treated_state")
  ) %>%
  mutate(
    directional_gap = if_else(direction == "positive", gap, -gap),
    ratio = directional_gap / pre_rmspe
  ) %>%
  group_by(base_window, window) %>%
  mutate(
    raw_rank = rank_ge(directional_gap),
    raw_p = raw_rank / n_states,
    ratio_rank = rank_ge(ratio),
    ratio_p = ratio_rank / n_states
  ) %>%
  ungroup() %>%
  filter(treated_state == "CA") %>%
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

# |- retiming ratio and SCM reference ----
# The deadline rush is a pull-forward; the post-deadline shortfall it could
# have offset is the missing flow. A ratio well below one means most of the
# shortfall is a permanent drop, not retiming.
scm_path <- path(tables_out_dir, "prop19_scm_inference.csv")
scm_inference <- if (file_exists(scm_path)) {
  read_csv(scm_path, show_col_types = FALSE)
} else {
  tibble(gap_ss_pct = NA_real_, scm_mspe_p = NA_real_, ct_p = NA_real_)
}

prop19_retiming_summary <- bunch %>%
  transmute(
    excess_antic_deeds = excess_antic_window,
    excess_antic_pct = excess_pct,
    missing_post_deeds = missing_post_window,
    missing_post_pct = missing_pct,
    retiming_ratio = excess_antic_window / missing_post_window,
    scm_gap_2022_2023_pct = scm_inference$gap_ss_pct,
    scm_mspe_p = scm_inference$scm_mspe_p,
    scm_conley_taber_p = scm_inference$ct_p
  )
print(prop19_retiming_summary, width = Inf)
write_csv_strict(prop19_retiming_summary, path(tables_out_dir, "prop19_retiming_summary.csv"))

# ============================================================
# 2. Cross-state triple difference: long-held x CA x post ----
# ============================================================
# Long-held = family transfer of a parcel with no market sale since OT
# coverage begins (2007) before the transfer month. Long tenure proxies owner
# age, and so COVID-era mortality, in every state; only in California does it
# also proxy a large Prop 13 wedge. State x month FE absorb state shocks
# (mortality, housing cycle); long-held x month FE absorb the national
# tenure-mortality profile. Transition months (Nov 2020 - Dec 2021) are
# excluded from the static estimate.
con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
ev_sql <- glue("read_parquet('{sql_quote_path(events_path)}')")
fam_sql <- paste0("('", paste(fam_classes, collapse = "','"), "')")

ddd_counts_raw <- dbGetQuery(con, glue("
  WITH first_mkt AS (
    SELECT clip, MIN(ym) AS first_mkt_ym
    FROM {ev_sql}
    WHERE class = 'market_sale'
    GROUP BY clip
  )
  SELECT e.state, e.ym, e.sale_year,
         CASE WHEN f.first_mkt_ym IS NULL OR f.first_mkt_ym >= e.ym THEN 1 ELSE 0 END AS longheld,
         COUNT(*) AS n_fam
  FROM {ev_sql} e
  LEFT JOIN first_mkt f USING (clip)
  WHERE e.class IN {fam_sql} AND e.sale_year BETWEEN 2017 AND 2023
  GROUP BY 1, 2, 3, 4
"))

ddd_panel <- ddd_counts_raw %>%
  filter(!state %in% territories) %>%
  mutate(across(c(ym, sale_year, longheld), as.integer), n_fam = as.numeric(n_fam)) %>%
  complete(state, nesting(ym, sale_year), longheld = c(0L, 1L), fill = list(n_fam = 0)) %>%
  mutate(post = as.integer(ym >= steady_start_ym),
         transition = as.integer(between(ym, antic_start_ym, post21_end_ym)))
assert_rows(ddd_panel, 8000, "prop19_ddd_longheld_panel")
stopifnot(n_distinct(ddd_panel$state) == 51)
write_csv_strict(ddd_panel, path(tables_out_dir, "prop19_ddd_longheld_panel.csv"))

# CA long-held share of family transfers, for scale
message("NOTE: long-held share of CA family transfers, 2017-2019 = ",
        round(with(ddd_panel %>% filter(state == "CA", sale_year <= 2019),
                   sum(n_fam[longheld == 1]) / sum(n_fam)), 3))

# |- static DDD for any pseudo-treated state ----
ddd_static_sample <- ddd_panel %>% filter(transition == 0L)

fit_ddd_longheld <- function(treated_state) {
  d <- ddd_static_sample %>%
    mutate(trt = as.integer(state == treated_state) * longheld * post)
  m <- fepois(n_fam ~ trt | state^longheld + state^ym + longheld^ym, data = d)
  unname(coef(m)["trt"])
}

ddd_longheld_ca_model <- fepois(
  n_fam ~ trt | state^longheld + state^ym + longheld^ym,
  data = ddd_static_sample %>% mutate(trt = as.integer(state == "CA") * longheld * post),
  cluster = ~state
)
print(summary(ddd_longheld_ca_model))

# |- permutation and Conley-Taber inference over 51 states ----
ddd_longheld_placebos <- tibble(state = sort(unique(ddd_panel$state))) %>%
  mutate(estimate = map_dbl(state, fit_ddd_longheld))
write_csv_strict(ddd_longheld_placebos, path(tables_out_dir, "prop19_ddd_longheld_placebos.csv"))

ddd_longheld_ca <- conley_taber(
  treated_est = ddd_longheld_placebos$estimate[ddd_longheld_placebos$state == "CA"],
  placebo_est = ddd_longheld_placebos$estimate[ddd_longheld_placebos$state != "CA"]
) %>%
  mutate(pct_effect = 100 * (exp(estimate) - 1),
         perm_rank_p = rank_p_value(ddd_longheld_placebos, "CA"),
         n_states = nrow(ddd_longheld_placebos),
         crve_se_state = unname(se(ddd_longheld_ca_model)["trt"]),
         nobs = nobs(ddd_longheld_ca_model))
print(ddd_longheld_ca, width = Inf)
write_csv_strict(ddd_longheld_ca, path(tables_out_dir, "prop19_ddd_longheld_ca.csv"))

# |- CA event study by year (2019 reference; transition years kept) ----
ddd_longheld_es <- fepois(
  n_fam ~ i(sale_year, ca_longheld, ref = 2019) | state^longheld + state^ym + longheld^ym,
  data = ddd_panel %>% mutate(ca_longheld = as.integer(state == "CA") * longheld),
  cluster = ~state
)
ddd_longheld_es_tab <- broom::tidy(ddd_longheld_es) %>%
  mutate(sale_year = as.integer(str_extract(term, "[0-9]{4}"))) %>%
  bind_rows(tibble(term = "ref", sale_year = 2019L, estimate = 0, std.error = 0)) %>%
  arrange(sale_year)
print(ddd_longheld_es_tab)
write_csv_strict(ddd_longheld_es_tab, path(tables_out_dir, "prop19_ddd_longheld_eventstudy.csv"))

# ============================================================
# 3. Appendix (legacy) outputs ----
# ============================================================
# Kept under their original filenames because 05_tables.R, 06_figures.R,
# 07_referee_checks.R and 11_welfare.R read them. Caveats:
#   - volume DiD: one treated state, so the state-clustered SE is not
#     interpretable; use the 07 permutation p-value.
#   - sold24 DDD: conditions on a post-reform transfer cohort that the policy
#     itself selects; superseded by the fixed pre-policy risk set (sections 4-6).
#   - absentee DiD: mailing ZIP at deed time misclassifies post-reform occupant
#     heirs, who have a year to move in; superseded by the 2008 HOX split.

# ============================================================
# 3a. Appendix: steady-state volume DiD ----
# ============================================================
# ln(family transfers) state x year; pre = 2017-2019, post = 2022-2023;
# transition years 2020-2021 excluded (anticipation + deadline retiming).
sy <- ev_data %>%
  filter(sale_year %in% c(2017L, 2018L, 2019L, 2022L, 2023L)) %>%
  group_by(state, sale_year) %>%
  summarize(
    n_fam = sum(class %in% fam_classes),
    n_fam_wtrust = sum(class %in% c(fam_classes, "family_trust")),
    n_market = sum(class == "market_sale", na.rm = TRUE),
    .groups = "drop"
  )
sy <- sy %>%
  filter(!state %in% c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")) %>%
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



# ============================================================
# 3b. Appendix: sold-within-24m DiD and market placebo ----
# ============================================================
# Cohorts: PRE = transfers 2017-01..2018-12; POST = 2021-07..2022-06.
# All outcome windows close before the 2024-06-30 censor.
ev_date_audit <- ev_data %>%
  mutate(sale_date = parse_event_dates(sale_raw))

date_audit <- ev_date_audit %>%
  summarize(
    n = n(),
    n_day_imputed = sum(as.numeric(sale_raw) %% 100 == 0, na.rm = TRUE),
    n_invalid = sum(is.na(sale_date))
  )
print(date_audit)
stopifnot(date_audit$n > 0, date_audit$n_invalid / date_audit$n < 0.001)
write_csv_strict(date_audit, path(tables_out_dir, "04_date_audit.csv"))

ev_dated <- ev_date_audit %>%
  filter(!is.na(sale_date))

cohort19 <- ev_dated %>%
  filter(
    between(ym, 201701L, 201812L) | between(ym, 202107L, 202206L),
    class %in% c(fam_classes, "market_sale"),
    corp_buyer == 0
  ) %>%
  transmute(
    clip, state, ym,
    grp = if_else(class %in% fam_classes, "fam", "market"),
    post = as.integer(between(ym, 202107L, 202206L)),
    absentee_buyer, sale_date
  )

market_sales <- ev_dated %>%
  filter(class == "market_sale") %>%
  dplyr::select(clip, msale_date = sale_date)

# Keep clip in each spell: distinct properties must never collapse into one cell.
spells <- join_next_market_sale(cohort19, market_sales)

spell_outcomes <- spells %>%
  mutate(
    sold24 = coalesce(next_market_date <= add_calendar_months(sale_date, 24L), FALSE)
  )

cells <- spell_outcomes %>%
  group_by(state, ym, grp, post) %>%
  summarize(
    n = n(),
    sold24 = mean(sold24),
    # SQL AVG ignores nulls and returns null for an entirely missing group.
    absentee_share = if (all(is.na(absentee_buyer))) NA_real_ else mean(absentee_buyer, na.rm = TRUE),
    n_absentee_obs = sum(!is.na(absentee_buyer)),
    .groups = "drop"
  )
assert_rows(cells, 2000, "prop19_cohort_cells")
cells <- cells %>%
  filter(!state %in% c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")) %>%
  mutate(ca = as.integer(state == "CA"))
write_csv_strict(cells, path(tables_out_dir, "prop19_cohort_cells.csv"))

cells_fam <- cells %>% filter(grp == "fam")
cells_mkt <- cells %>% filter(grp == "market")

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
cells_pooled <- cells %>%
  mutate(fam = as.integer(grp == "fam"))
m_ddd <- feols(sold24 ~ ca:post:fam + ca:post
               | state^grp + ym^grp, data = cells_pooled,
               weights = ~n, cluster = ~state)
print(etable(m_haz_fam, m_haz_mkt, m_ddd))
saveRDS(list(fam = m_haz_fam, market_placebo = m_haz_mkt, ddd = m_ddd),
        path(out_dir, "prop19_ddd_hazard.rds"))

# Raw 2x2 for transparency
raw22 <- cells_fam %>%
  group_by(ca, post) %>%
  summarise(sold24 = weighted.mean(sold24, n), n = sum(n), .groups = "drop")
print(raw22)
write_csv_strict(raw22, path(tables_out_dir, "prop19_raw_2x2_fam.csv"))
raw22_mkt <- cells_mkt %>%
  group_by(ca, post) %>%
  summarise(sold24 = weighted.mean(sold24, n), n = sum(n), .groups = "drop")
write_csv_strict(raw22_mkt, path(tables_out_dir, "prop19_raw_2x2_market.csv"))

# ============================================================
# 3c. Appendix: absentee recipient share DiD ----
# ============================================================
cells_abs <- cells_fam %>% filter(n_absentee_obs > 0, !is.na(absentee_share))
m_abs <- feols(absentee_share ~ ca:post | state + ym, data = cells_abs,
               weights = ~n_absentee_obs, cluster = ~state)
print(etable(m_abs))
saveRDS(m_abs, path(out_dir, "prop19_absentee_did.rds"))
write_csv_strict(
  tibble(coef = coef(m_abs)["ca:post"], se = se(m_abs)["ca:post"], n = nobs(m_abs)),
  path(tables_out_dir, "prop19_absentee_did.csv")
)

rm(ev_data); gc(verbose = FALSE)

#============================================#
# 4. Parcel risk-set cells (fixed pre-policy cohort) ----
#============================================#

# Treatment intensity is the parcel's annual inherited-tax wedge measured at
# a baseline lien date from the 2008-09 roll (04a). It is fixed before the
# reform, so it cannot respond to Prop 19. Parcels leave the risk set at the
# first family transfer or market sale.
wedge_path <- path(data_dir, "ca_parcel_wedge.parquet")
stopifnot(file_exists(wedge_path))
parcel_wedge <- arrow::read_parquet(
  wedge_path,
  col_select = c(clip, cohort, fips5, type, hox, tenure, over_cap_2021, rate,
                 assessed_d0, market_d0, market_2021, assessed_2021, wedge, wedge_rs,
                 assess_ratio, wedge_bin, end_type, spell_end, trust_first_date,
                 other_first_date, exit_same_dynasty)
) %>%
  mutate(
    group = case_when(hox == 0L ~ "nonhox",
                      over_cap_2021 == 1L ~ "hox_over_cap",
                      TRUE ~ "hox_under_cap"),
    gap_share = pmax(0, 1 - assess_ratio)
  )
message("NOTE: loaded ", format(nrow(parcel_wedge), big.mark = ","), " parcel-cohort rows")
print(count(parcel_wedge, cohort, group))

to_ym <- function(d) as.integer(format(d, "%Y%m"))
ym_sequence <- function(start_ym, end_ym) {
  dates <- seq(as.Date(sprintf("%d-%02d-01", start_ym %/% 100, start_ym %% 100)),
               as.Date(sprintf("%d-%02d-01", end_ym %/% 100, end_ym %% 100)), by = "month")
  to_ym(dates)
}

exit_types <- c(fam_classes, "market_estate_trust", "market_other")
groups_all <- c("nonhox", "hox_under_cap", "hox_over_cap")

#' Collapse parcel spells to cell x month counts with at-risk denominators.
#'
#' Cells are county x intensity bin x occupancy group x tenure. The at-risk
#' count at the start of month m is the cell's baseline parcels minus
#' ownership exits in earlier months. Trust/retitle and other deeds are
#' counted at their first occurrence while the parcel is at risk.
#'
#' @param parcels parcel spells for one cohort (cohort names are dYYYY)
#' @param bin_var intensity-bin column
#' @param intensity_var continuous intensity column
#' @param scale divisor for intensity (1000 gives $ thousand per year)
build_wedge_cells <- function(parcels, bin_var = "wedge_bin", intensity_var = "wedge",
                              scale = 1000) {
  stopifnot(n_distinct(parcels$cohort) == 1L)
  start_ym <- as.integer(substr(parcels$cohort[1], 2, 5)) * 100L + 1L
  keys <- c("fips5", "bin", "group", "tenure")

  p <- parcels %>%
    mutate(bin = as.character(.data[[bin_var]]),
           intensity = .data[[intensity_var]],
           end_ym = if_else(end_type == "censored", NA_integer_, to_ym(spell_end)),
           trust_ym = to_ym(trust_first_date),
           other_ym = to_ym(other_first_date)) %>%
    filter(!is.na(bin), !is.na(intensity))

  cell_def <- p %>%
    group_by(across(all_of(keys))) %>%
    summarise(n0 = n(), wbar = mean(intensity) / scale, .groups = "drop") %>%
    mutate(cell_id = row_number())

  exit_counts <- p %>%
    filter(!is.na(end_ym)) %>%
    count(across(all_of(keys)), ym = end_ym, end_type) %>%
    pivot_wider(names_from = end_type, values_from = n, values_fill = 0)
  for (k in setdiff(exit_types, names(exit_counts))) exit_counts[[k]] <- 0L

  trust_counts <- p %>%
    filter(!is.na(trust_ym)) %>%
    count(across(all_of(keys)), ym = trust_ym, name = "trust_first")
  other_counts <- p %>%
    filter(!is.na(other_ym)) %>%
    count(across(all_of(keys)), ym = other_ym, name = "other_first")

  cell_def %>%
    cross_join(tibble(ym = ym_sequence(start_ym, censor_ym))) %>%
    left_join(exit_counts, by = c(keys, "ym")) %>%
    left_join(trust_counts, by = c(keys, "ym")) %>%
    left_join(other_counts, by = c(keys, "ym")) %>%
    mutate(across(c(all_of(exit_types), trust_first, other_first),
                  ~ as.numeric(coalesce(as.numeric(.x), 0)))) %>%
    arrange(cell_id, ym) %>%
    group_by(cell_id) %>%
    mutate(own_exits = family_person + family_other + family_estate +
             market_estate_trust + market_other,
           at_risk = n0 - lag(cumsum(own_exits), default = 0)) %>%
    ungroup() %>%
    mutate(
      fam = family_person + family_other + family_estate,
      market = market_estate_trust + market_other,
      hox = as.integer(group != "nonhox"),
      qtr = paste0(ym %/% 100, "Q", ((ym %% 100) - 1) %/% 3 + 1),
      antic = as.integer(between(ym, antic_start_ym, antic_end_ym)),
      post21 = as.integer(ym > antic_end_ym & ym <= post21_end_ym),
      steady = as.integer(ym >= steady_start_ym)
    ) %>%
    filter(at_risk > 0)
}

#' Add group-specific intensity x period regressors as explicit columns, so
#' the fitted treatment index can be rebuilt from the data and coefficients.
add_treatment_terms <- function(cells, periods = c("antic", "post21", "steady")) {
  for (g in groups_all) {
    for (per in periods) {
      cells[[paste0("w_", g, "_", per)]] <- cells$wbar * (cells$group == g) * cells[[per]]
    }
  }
  cells
}

cells_main <- parcel_wedge %>%
  filter(cohort == main_cohort) %>%
  build_wedge_cells() %>%
  add_treatment_terms()

# |- accounting checks ----
# at-risk parcels in the first month must equal the cohort's parcels, and no
# cell can lose more parcels than it started with
cells_check <- cells_main %>%
  group_by(cell_id) %>%
  summarise(n0 = first(n0), exits = sum(own_exits), .groups = "drop")
stopifnot(
  all(cells_check$exits <= cells_check$n0),
  sum(cells_main$at_risk[cells_main$ym == min(cells_main$ym)]) ==
    sum(parcel_wedge$cohort == main_cohort & !is.na(parcel_wedge$wedge_bin))
)
message("NOTE: main cells = ", n_distinct(cells_main$cell_id), "; rows = ",
        format(nrow(cells_main), big.mark = ","), "; family exits = ",
        format(sum(cells_main$fam), big.mark = ","))
write_csv_strict(cells_main %>% select(fips5, bin, group, tenure, cell_id, n0, wbar, ym,
                                       at_risk, fam, family_person, market_estate_trust,
                                       market_other, trust_first, other_first),
                 path(tables_out_dir, "prop19_wedge_cells_main.csv"))

#============================================#
# 5. Dose-response hazard ----
#============================================#

# Identification: within county-month and tenure x HOX-month, does the
# family-transfer hazard of high-wedge parcels fall relative to low-wedge
# parcels after the reform? Coefficients are semi-elasticities of the
# hazard per $1,000 of annual wedge (2017 dollars).
fe_pooled <- "cell_id + fips5^ym + tenure^hox^ym"
treatment_terms <- grep("^w_", names(cells_main), value = TRUE)

fit_dose_parametric <- function(cells, outcome = "fam", cluster = ~fips5) {
  terms_present <- treatment_terms[colSums(abs(as.matrix(cells[, treatment_terms]))) > 0]
  f <- as.formula(paste0(outcome, " ~ ", paste(terms_present, collapse = " + "), " | ", fe_pooled))
  fepois(f, data = cells, offset = ~log(at_risk), cluster = cluster)
}

tidy_terms <- function(m, label) {
  ct <- coeftable(m)
  tibble(spec = label, term = rownames(ct), estimate = ct[, 1], se = ct[, 2], p = ct[, 4]) %>%
    mutate(pct_per_1k = 100 * (exp(estimate) - 1),
           ci_lo = estimate - 1.96 * se, ci_hi = estimate + 1.96 * se,
           nobs = nobs(m))
}

#' Difference of two coefficients with its delta-method standard error.
lincom_diff <- function(m, a, b) {
  v <- vcov(m)
  est <- coef(m)[a] - coef(m)[b]
  se_d <- sqrt(v[a, a] + v[b, b] - 2 * v[a, b])
  tibble(contrast = paste(a, "-", b), estimate = unname(est), se = unname(se_d),
         p = 2 * pnorm(-abs(unname(est / se_d))))
}

# |- parametric model (headline) ----
dose_fam_param <- fit_dose_parametric(cells_main, "fam")
print(summary(dose_fam_param))
dose_fam_param_tab <- tidy_terms(dose_fam_param, "main_fam")
dose_fam_contrasts <- bind_rows(
  lincom_diff(dose_fam_param, "w_nonhox_steady", "w_hox_under_cap_steady"),
  lincom_diff(dose_fam_param, "w_hox_over_cap_steady", "w_hox_under_cap_steady")
)
print(dose_fam_param_tab, width = Inf)
print(dose_fam_contrasts)
write_csv_strict(dose_fam_param_tab, path(tables_out_dir, "prop19_dose_param_family.csv"))
write_csv_strict(dose_fam_contrasts, path(tables_out_dir, "prop19_dose_param_family_contrasts.csv"))

# |- descriptive pre-reform gradient (cross-sectional, not causal) ----
pre_gradient <- map_dfr(groups_all, function(g) {
  d <- cells_main %>% filter(group == g, ym < antic_start_ym)
  m <- fepois(fam ~ wbar | fips5^ym + tenure^ym, data = d,
              offset = ~log(at_risk), cluster = ~fips5)
  tidy_terms(m, paste0("pre_gradient_", g))
})
print(pre_gradient, width = Inf)
write_csv_strict(pre_gradient, path(tables_out_dir, "prop19_dose_pre_gradient.csv"))

# |- quarterly event study by group ----
pre_quarters <- sort(unique(cells_main$qtr[cells_main$ym < 202007L]))
fit_dose_es <- function(cells, outcome, g) {
  d <- cells %>% filter(group == g)
  fepois(as.formula(paste0(outcome, " ~ i(qtr, wbar, ref = '", es_ref_quarter,
                           "') | cell_id + fips5^ym + tenure^ym")),
         data = d, offset = ~log(at_risk), cluster = ~fips5)
}

dose_es <- map(set_names(groups_all), ~ fit_dose_es(cells_main, "fam", .x))
dose_es_tab <- imap_dfr(dose_es, function(m, g) {
  broom::tidy(m) %>%
    mutate(group = g, qtr = str_extract(term, "[0-9]{4}Q[1-4]")) %>%
    bind_rows(tibble(group = g, qtr = es_ref_quarter, estimate = 0, std.error = 0))
}) %>%
  mutate(ci_lo = estimate - 1.96 * std.error, ci_hi = estimate + 1.96 * std.error,
         year = as.integer(substr(qtr, 1, 4)), q = as.integer(substr(qtr, 6, 6)),
         date = as.Date(sprintf("%d-%02d-01", year, (q - 1L) * 3L + 1L))) %>%
  arrange(group, date)
write_csv_strict(dose_es_tab, path(tables_out_dir, "prop19_dose_es_family_by_group.csv"))

# joint pre-trend test (2017Q1-2020Q2) and relative-magnitudes breakdown;
# pre-period coefficients include the 2020Q3 reference (zero)
pretrend_tests <- imap_dfr(dose_es, function(m, g) {
  w <- fixest::wald(m, keep = paste0("^qtr::(", paste(pre_quarters, collapse = "|"), "):wbar$"),
                    print = FALSE)
  pre_coefs <- dose_es_tab %>%
    filter(group == g, date <= as.Date("2020-07-01")) %>%
    pull(estimate)
  steady_term <- paste0("w_", g, "_steady")
  bind_cols(
    tibble(group = g, pretrend_stat = w$stat, pretrend_p = w$p, pretrend_df = w$df1),
    rm_breakdown(pre_coefs, unname(coef(dose_fam_param)[steady_term]),
                 unname(se(dose_fam_param)[steady_term]))
  )
})
print(pretrend_tests, width = Inf)
write_csv_strict(pretrend_tests, path(tables_out_dir, "prop19_dose_pretrend_tests.csv"))

# |- nonparametric dose-response: bin x period effects ----
fit_dose_bins <- function(cells, outcome, g) {
  d <- cells %>% filter(group == g)
  fepois(as.formula(paste0(outcome, " ~ i(bin, antic, ref = '", reference_bin, "') + ",
                           "i(bin, post21, ref = '", reference_bin, "') + ",
                           "i(bin, steady, ref = '", reference_bin, "') | ",
                           "cell_id + fips5^ym + tenure^ym")),
         data = d, offset = ~log(at_risk), cluster = ~fips5)
}
bin_means <- cells_main %>%
  filter(ym == min(ym)) %>%
  group_by(group, bin) %>%
  summarise(bin_wedge_k = weighted.mean(wbar, n0), n_parcels = sum(n0), .groups = "drop")

dose_bins_tab <- map_dfr(c("nonhox", "hox_under_cap"), function(g) {
  m <- fit_dose_bins(cells_main, "fam", g)
  broom::tidy(m) %>%
    mutate(group = g,
           bin = str_match(term, "^bin::([^:]+):")[, 2],
           period = str_match(term, ":(antic|post21|steady)$")[, 2]) %>%
    bind_rows(tibble(group = g, bin = reference_bin, period = c("antic", "post21", "steady"),
                     estimate = 0, std.error = 0))
}) %>%
  left_join(bin_means, by = c("group", "bin")) %>%
  mutate(pct = 100 * (exp(estimate) - 1),
         ci_lo = estimate - 1.96 * std.error, ci_hi = estimate + 1.96 * std.error)
print(dose_bins_tab %>%
        filter(period == "steady") %>%
        select(group, bin, bin_wedge_k, estimate, std.error, pct),
      n = 20)
write_csv_strict(dose_bins_tab, path(tables_out_dir, "prop19_dose_bins_family.csv"))

#============================================#
# 6. Competing risks: where did deterred transfers go? ----
#============================================#

# Each exit type gets the same parametric model. The implied change is the
# fitted count minus the count with the steady-state treatment index set to
# zero, summed over the steady window. It is measured relative to a
# zero-wedge parcel in the same county-month, so it is the wedge-
# attributable response. Market sales by non-estate sellers can include
# Prop 19's portability arm (living 55+ owners moving their base).
decomposition_outcomes <- c("fam", "market_estate_trust", "market_other",
                            "trust_first", "other_first")
steady_months <- n_distinct(cells_main$ym[cells_main$steady == 1])

implied_change <- function(m, cells, window = "steady", weight = NULL) {
  used <- cells[obs(m), ]
  b <- coef(m)
  window_terms <- intersect(names(b), grep(paste0("_", window, "$"), treatment_terms, value = TRUE))
  lin <- as.vector(as.matrix(used[, window_terms]) %*% b[window_terms])
  mu <- fitted(m)
  delta <- (mu - mu * exp(-lin)) * (used[[window]] == 1)
  if (!is.null(weight)) delta <- delta * used[[weight]]
  sum(delta)
}

decompose_exits <- function(cells) {
  models <- map(set_names(decomposition_outcomes), ~ fit_dose_parametric(cells, .x))
  deltas <- map_dbl(models, implied_change, cells = cells)
  # wedge-weighted changes ($ thousand of annual wedge) for welfare and revenue
  deltas_w <- map_dbl(models, implied_change, cells = cells, weight = "wbar")
  # shares are defined only when the wedge gradient deters family transfers;
  # with a non-negative response there is nothing to decompose
  deterred <- -deltas[["fam"]]
  if (deterred <= 0) deterred <- NA_real_
  c(
    set_names(deltas, paste0("delta_", decomposition_outcomes)),
    set_names(deltas_w, paste0("delta_w_", decomposition_outcomes)),
    share_market_estate_trust = deltas[["market_estate_trust"]] / deterred,
    share_market_other = deltas[["market_other"]] / deterred,
    share_trust = deltas[["trust_first"]] / deterred,
    share_other_deed = deltas[["other_first"]] / deterred,
    share_no_recorded_deed = 1 - (deltas[["market_estate_trust"]] + deltas[["market_other"]] +
                                    deltas[["trust_first"]] + deltas[["other_first"]]) / deterred,
    coef_nonhox_steady = unname(coef(models$fam)["w_nonhox_steady"]),
    coef_hox_under_steady = unname(coef(models$fam)["w_hox_under_cap_steady"]),
    coef_hox_over_steady = unname(coef(models$fam)["w_hox_over_cap_steady"])
  )
}

exits_point <- decompose_exits(cells_main)
print(round(exits_point, 4))

# competing-risk coefficient tables (county-clustered)
competing_tab <- map_dfr(decomposition_outcomes, function(y) {
  tidy_terms(fit_dose_parametric(cells_main, y), y)
})
write_csv_strict(competing_tab, path(tables_out_dir, "prop19_dose_param_competing_risks.csv"))

# |- county pairs bootstrap for headline coefficients and shares ----
# A county drawn twice enters as two counties; cell ids are rebuilt so its
# cells stay distinct.
t0 <- Sys.time()
set.seed(20261002)
exits_boot <- cluster_pairs_boot(cells_main, cluster = "fips5", B = B_boot,
                                 stat_fun = function(d) {
                                   d$cell_id <- as.integer(factor(paste(d$fips5, d$cell_id)))
                                   decompose_exits(d)
                                 })
message("NOTE: bootstrap (B = ", B_boot, ") finished in ",
        round(difftime(Sys.time(), t0, units = "mins"), 1), " min; successful draws = ",
        nrow(exits_boot))
write_csv_strict(exits_boot, path(tables_out_dir, "prop19_exits_bootstrap_draws.csv"))

exits_decomposition <- tibble(statistic = names(exits_point), estimate = unname(exits_point)) %>%
  left_join(
    exits_boot %>%
      pivot_longer(everything(), names_to = "statistic", values_to = "draw") %>%
      group_by(statistic) %>%
      summarise(boot_se = sd(draw, na.rm = TRUE),
                boot_lo = quantile(draw, 0.025, na.rm = TRUE),
                boot_hi = quantile(draw, 0.975, na.rm = TRUE),
                .groups = "drop"),
    by = "statistic"
  ) %>%
  mutate(steady_months = steady_months,
         annualized = if_else(str_starts(statistic, "delta"), estimate * 12 / steady_months, NA_real_))
print(exits_decomposition, n = 40, width = Inf)
write_csv_strict(exits_decomposition, path(tables_out_dir, "prop19_exits_decomposition_steady.csv"))

#============================================#
# 7. Placebos and robustness ----
#============================================#

steady_terms <- paste0("w_", groups_all, "_steady")
extract_steady <- function(m, label) {
  ct <- coeftable(m)
  tibble(spec = label, term = steady_terms) %>%
    mutate(estimate = ct[match(term, rownames(ct)), 1],
           se = ct[match(term, rownames(ct)), 2],
           p = ct[match(term, rownames(ct)), 4],
           nobs = nobs(m))
}

# |- fake reform two years early, pre-period only ----
cells_fake <- cells_main %>%
  filter(ym <= fake_sample_end) %>%
  mutate(antic = as.integer(between(ym, fake_antic[1], fake_antic[2])),
         post21 = 0L,
         steady = as.integer(ym >= fake_post_start)) %>%
  add_treatment_terms()

# |- alternative cohorts, outcomes, samples, and intensities ----
cells_d2019 <- parcel_wedge %>%
  filter(cohort == robust_cohort) %>%
  build_wedge_cells() %>%
  add_treatment_terms()

cells_good_sqft <- cells_main %>%
  filter(!fips5 %in% poor_sqft_counties)

# gap share 1 - A/P, per 10 percentage points
cells_gap_share <- parcel_wedge %>%
  filter(cohort == main_cohort) %>%
  mutate(gap_bin = cut(gap_share, c(-Inf, 0.1, 0.3, 0.5, 0.65, 0.8, Inf)),
         gap_10pp = 10 * gap_share) %>%
  build_wedge_cells(bin_var = "gap_bin", intensity_var = "gap_10pp", scale = 1) %>%
  add_treatment_terms()

# repeat-sales valuation, available for 2007+ buyers only
cells_repeat_sales <- parcel_wedge %>%
  filter(cohort == main_cohort, !is.na(wedge_rs)) %>%
  mutate(wedge_rs_bin = cut(wedge_rs, c(0, 500, 1500, 3000, 5000, 10000, Inf), right = FALSE)) %>%
  build_wedge_cells(bin_var = "wedge_rs_bin", intensity_var = "wedge_rs") %>%
  add_treatment_terms()

prop19_robustness <- bind_rows(
  extract_steady(dose_fam_param, "main: d2017 cohort, all family classes"),
  extract_steady(fit_dose_parametric(cells_fake), "placebo: fake reform Nov 2018 (pre-period only)"),
  extract_steady(fit_dose_parametric(cells_d2019), "cohort: d2019 baseline"),
  extract_steady(fit_dose_parametric(cells_main, "family_person"), "outcome: same-surname family only"),
  extract_steady(fit_dose_parametric(cells_good_sqft), "sample: drop low-sqft-coverage counties"),
  extract_steady(fit_dose_parametric(cells_gap_share), "intensity: assessment gap share (per 10pp)"),
  extract_steady(fit_dose_parametric(cells_repeat_sales), "intensity: repeat-sales wedge (2007+ buyers)")
) %>%
  mutate(pct = 100 * (exp(estimate) - 1))
print(prop19_robustness, n = 40, width = Inf)
write_csv_strict(prop19_robustness, path(tables_out_dir, "prop19_robustness_steady.csv"))

# |- same-dynasty share of exits (2008 owner surname on the exit deed) ----
same_dynasty_by_period <- parcel_wedge %>%
  filter(cohort == main_cohort, end_type != "censored") %>%
  mutate(period = case_when(to_ym(spell_end) < antic_start_ym ~ "pre",
                            to_ym(spell_end) <= antic_end_ym ~ "antic",
                            to_ym(spell_end) <= post21_end_ym ~ "post21",
                            TRUE ~ "steady"),
         exit_class = if_else(end_type %in% fam_classes, "family", "market")) %>%
  group_by(exit_class, group, wedge_bin, period) %>%
  summarise(n = n(), share_same_dynasty = mean(exit_same_dynasty, na.rm = TRUE),
            n_with_seller = sum(!is.na(exit_same_dynasty)), .groups = "drop")
write_csv_strict(same_dynasty_by_period, path(tables_out_dir, "prop19_same_dynasty_exits.csv"))

#============================================#
# 8. Headline numbers and sufficient-statistic welfare ----
#============================================#

# 2017 dollars (wedge baseline) to 2023 dollars, matching 11_welfare.R
cpi_monthly <- read_csv(path(r_dir, "inputs", "welfare_cpi.csv"), show_col_types = FALSE)
cpi_annual <- cpi_monthly %>%
  group_by(year) %>%
  summarise(cpi = round(mean(cpi_u), 3), .groups = "drop")
inflator_2017_2023 <- cpi_annual$cpi[cpi_annual$year == 2023] / cpi_annual$cpi[cpi_annual$year == 2017]
annuity_factor <- sum((1 + pv_rate)^-(0:(pv_horizon - 1L)))

linkage <- read_csv(path(tables_out_dir, "wedge_linkage.csv"), show_col_types = FALSE)
fam_coverage <- linkage$share_fam_events_2017_2024_in_universe

# |- H1: the subsidy carried by pre-reform inheritances (2018-2019) ----
pre_reform_family_exits <- parcel_wedge %>%
  filter(cohort == main_cohort, end_type %in% fam_classes,
         between(to_ym(spell_end), 201801L, 201912L))
h1_subsidy <- pre_reform_family_exits %>%
  summarise(
    n_family_exits_per_year = n() / 2,
    median_wedge_2023usd = median(wedge) * inflator_2017_2023,
    mean_wedge_2023usd = mean(wedge) * inflator_2017_2023,
    p90_wedge_2023usd = quantile(wedge, 0.9) * inflator_2017_2023,
    share_nonhox = mean(group == "nonhox"),
    share_long_held = mean(tenure == "long_held"),
    annual_wedge_flow_2023usd = sum(wedge) / 2 * inflator_2017_2023
  ) %>%
  mutate(pv_wedge_flow_2023usd = annual_wedge_flow_2023usd * annuity_factor,
         pv_wedge_flow_statewide_2023usd = pv_wedge_flow_2023usd / fam_coverage)

# |- H3: response per $1,000 of wedge, with bootstrap intervals ----
boot_ci <- function(stat) exits_decomposition %>% filter(statistic == stat)
h3_response <- tibble(
  group = groups_all,
  statistic = paste0("coef_", c("nonhox", "hox_under", "hox_over"), "_steady")
) %>%
  mutate(
    estimate = map_dbl(statistic, ~ boot_ci(.x)$estimate),
    boot_lo = map_dbl(statistic, ~ boot_ci(.x)$boot_lo),
    boot_hi = map_dbl(statistic, ~ boot_ci(.x)$boot_hi),
    pct_per_1k = 100 * (exp(estimate) - 1),
    pct_per_1k_lo = 100 * (exp(boot_lo) - 1),
    pct_per_1k_hi = 100 * (exp(boot_hi) - 1),
    mean_wedge_k_pre_exits = map_dbl(group, ~ mean(pre_reform_family_exits$wedge[pre_reform_family_exits$group == .x]) / 1000),
    pct_at_mean_exit_wedge = 100 * (exp(estimate * mean_wedge_k_pre_exits) - 1)
  )

# |- H4-H6: deterred transfers, supply, welfare, revenue ----
dec <- set_names(exits_decomposition$estimate, exits_decomposition$statistic)
per_year <- 12 / steady_months
deterred_per_year <- -dec[["delta_fam"]] * per_year
market_estate_per_year <- dec[["delta_market_estate_trust"]] * per_year
market_all_per_year <- (dec[["delta_market_estate_trust"]] + dec[["delta_market_other"]]) * per_year

# wedge-weighted flows: $ thousand of 2017 wedge -> 2023 dollars per year
deterred_wedge_per_year <- -dec[["delta_w_fam"]] * per_year * 1000 * inflator_2017_2023
market_estate_wedge_per_year <- dec[["delta_w_market_estate_trust"]] * per_year * 1000 * inflator_2017_2023

# Harberger: a home moved from a lower-value heir to a buyer yields between
# 0 and w per year; 1/2 w under linear demand. Lower bound counts only
# deterred transfers that reached the market through an estate/trust sale;
# upper bound counts every deterred family deed.
dwl_lower_per_year <- 0.5 * max(market_estate_wedge_per_year, 0)
dwl_upper_per_year <- 0.5 * deterred_wedge_per_year

# The Harberger and revenue accounting presume a deterrent response. If the
# wedge gradient of family transfers did not fall, report NA, not a sign-flipped number.
if (deterred_per_year <= 0) {
  message("WARNING: no wedge-attributable deterrence (deterred = ", round(deterred_per_year),
          " per year); welfare and revenue headlines set to NA.")
  dwl_lower_per_year <- NA_real_
  dwl_upper_per_year <- NA_real_
}

# mechanical revenue: steady-window family exits that lost the exclusion
# (non-HOX fully; over-cap HOX on the value above the $1M allowance)
steady_family_exits <- parcel_wedge %>%
  filter(cohort == main_cohort, end_type %in% fam_classes, to_ym(spell_end) >= steady_start_ym) %>%
  mutate(gap_2021 = pmax(market_2021 - assessed_2021, 0),
         reassessed_share = case_when(group == "nonhox" ~ 1,
                                      group == "hox_over_cap" ~ pmax(gap_2021 - 1e6, 0) / gap_2021,
                                      TRUE ~ 0),
         revenue = wedge * reassessed_share)
revenue_observed_per_year <- sum(steady_family_exits$revenue) * per_year * inflator_2017_2023
# without behavior, deterred transfers would also have occurred and been reassessed
revenue_mechanical_per_year <- revenue_observed_per_year + deterred_wedge_per_year
revenue_actual_per_year <- revenue_observed_per_year + max(market_estate_wedge_per_year, 0)
if (deterred_per_year <= 0) {
  revenue_mechanical_per_year <- NA_real_
  revenue_actual_per_year <- NA_real_
}

prop19_headlines <- tribble(
  ~headline, ~statistic, ~value, ~lo, ~hi, ~units,
  "H1", "family exits per year (2018-19, risk set)", h1_subsidy$n_family_exits_per_year, NA, NA, "parcels",
  "H1", "median annual wedge of inherited homes", h1_subsidy$median_wedge_2023usd, NA, NA, "2023 $ per year",
  "H1", "mean annual wedge of inherited homes", h1_subsidy$mean_wedge_2023usd, NA, NA, "2023 $ per year",
  "H1", "annual wedge flow carried by new inheritances", h1_subsidy$annual_wedge_flow_2023usd, NA, NA, "2023 $ per year",
  "H1", "PV of tax savings, one year of inheritances (risk set)", h1_subsidy$pv_wedge_flow_2023usd, NA, NA, "2023 $",
  "H1", "PV of tax savings, scaled to all CA family deeds", h1_subsidy$pv_wedge_flow_statewide_2023usd, NA, NA, "2023 $",
  "H2", "deadline excess deeds (Nov 2020-Feb 2021)", prop19_retiming_summary$excess_antic_deeds, NA, NA, "deeds",
  "H2", "missing deeds (Mar 2021-Dec 2022)", prop19_retiming_summary$missing_post_deeds, NA, NA, "deeds",
  "H2", "retiming ratio (excess / missing)", prop19_retiming_summary$retiming_ratio, NA, NA, "ratio",
  "H3", "non-HOX: % change in family hazard per $1k wedge (steady)", h3_response$pct_per_1k[1], h3_response$pct_per_1k_lo[1], h3_response$pct_per_1k_hi[1], "percent",
  "H3", "HOX under cap: % change per $1k wedge (steady)", h3_response$pct_per_1k[2], h3_response$pct_per_1k_lo[2], h3_response$pct_per_1k_hi[2], "percent",
  "H3", "HOX over cap: % change per $1k wedge (steady)", h3_response$pct_per_1k[3], h3_response$pct_per_1k_lo[3], h3_response$pct_per_1k_hi[3], "percent",
  "H3", "non-HOX: % change at mean wedge of pre-reform exits", h3_response$pct_at_mean_exit_wedge[1], NA, NA, "percent",
  "H4", "deterred family transfers per year (wedge-attributable)", deterred_per_year, -boot_ci("delta_fam")$boot_hi * per_year, -boot_ci("delta_fam")$boot_lo * per_year, "deeds per year",
  "H4", "share to market via estate/trust seller", dec[["share_market_estate_trust"]], boot_ci("share_market_estate_trust")$boot_lo, boot_ci("share_market_estate_trust")$boot_hi, "share",
  "H4", "share to market via other seller", dec[["share_market_other"]], boot_ci("share_market_other")$boot_lo, boot_ci("share_market_other")$boot_hi, "share",
  "H4", "share to trust/retitle deed", dec[["share_trust"]], boot_ci("share_trust")$boot_lo, boot_ci("share_trust")$boot_hi, "share",
  "H4", "share to other non-arm's-length deed", dec[["share_other_deed"]], boot_ci("share_other_deed")$boot_lo, boot_ci("share_other_deed")$boot_hi, "share",
  "H4", "share with no recorded deed (deferral)", dec[["share_no_recorded_deed"]], boot_ci("share_no_recorded_deed")$boot_lo, boot_ci("share_no_recorded_deed")$boot_hi, "share",
  "H5", "additional market sales per year, estate/trust sellers", market_estate_per_year, boot_ci("delta_market_estate_trust")$boot_lo * per_year, boot_ci("delta_market_estate_trust")$boot_hi * per_year, "homes per year",
  "H5", "additional market sales per year, all sellers", market_all_per_year, NA, NA, "homes per year",
  "H6", "DWL avoided per year, lower bound", dwl_lower_per_year, NA, NA, "2023 $ per year",
  "H6", "DWL avoided per year, upper bound", dwl_upper_per_year, NA, NA, "2023 $ per year",
  "H6", "PV of DWL avoided per annual cohort, lower", dwl_lower_per_year * annuity_factor, NA, NA, "2023 $",
  "H6", "PV of DWL avoided per annual cohort, upper", dwl_upper_per_year * annuity_factor, NA, NA, "2023 $",
  "H6", "revenue per year, mechanical (no behavior)", revenue_mechanical_per_year, NA, NA, "2023 $ per year",
  "H6", "revenue per year, with behavioral response", revenue_actual_per_year, NA, NA, "2023 $ per year",
  "H6", "behavioral revenue offset (share of mechanical)", 1 - revenue_actual_per_year / revenue_mechanical_per_year, NA, NA, "share",
  "H6", "excess burden per $1 of subsidy, lower", dwl_lower_per_year / h1_subsidy$annual_wedge_flow_2023usd, NA, NA, "ratio",
  "H6", "excess burden per $1 of subsidy, upper", dwl_upper_per_year / h1_subsidy$annual_wedge_flow_2023usd, NA, NA, "ratio"
) %>%
  mutate(across(c(value, lo, hi), as.numeric))
print(prop19_headlines, n = 50, width = Inf)
write_csv_strict(prop19_headlines, path(tables_out_dir, "prop19_headlines.csv"))
write_csv_strict(h1_subsidy, path(tables_out_dir, "prop19_headline_subsidy.csv"))
write_csv_strict(h3_response, path(tables_out_dir, "prop19_headline_response.csv"))

# |- manuscript macros ----
fmt <- function(x, digits = 0) formatC(x, digits = digits, format = "f", big.mark = ",")
macro <- function(name, value) paste0("\\newcommand{\\", name, "}{", value, "}")
writeLines(c(
  "% Generated by 04_prop19.R; do not edit by hand.",
  macro("PropNineteenMedianWedge", fmt(h1_subsidy$median_wedge_2023usd)),
  macro("PropNineteenMeanWedge", fmt(h1_subsidy$mean_wedge_2023usd)),
  macro("PropNineteenSubsidyPVbn", fmt(h1_subsidy$pv_wedge_flow_statewide_2023usd / 1e9, 1)),
  macro("PropNineteenRetimingRatio", fmt(prop19_retiming_summary$retiming_ratio, 2)),
  macro("PropNineteenNonHoxPctPerK", fmt(h3_response$pct_per_1k[1], 1)),
  macro("PropNineteenHoxPctPerK", fmt(h3_response$pct_per_1k[2], 1)),
  macro("PropNineteenDeterredPerYear", fmt(deterred_per_year)),
  macro("PropNineteenShareMarketEstate", fmt(100 * dec[["share_market_estate_trust"]], 1)),
  macro("PropNineteenShareNoDeed", fmt(100 * dec[["share_no_recorded_deed"]], 1)),
  macro("PropNineteenDWLLowerM", fmt(dwl_lower_per_year / 1e6, 1)),
  macro("PropNineteenDWLUpperM", fmt(dwl_upper_per_year / 1e6, 1)),
  macro("PropNineteenDDDPct", fmt(ddd_longheld_ca$pct_effect, 1)),
  macro("PropNineteenDDDp", fmt(ddd_longheld_ca$ct_p, 3))
), path(tables_dir, "prop19_numbers.tex"))

#============================================#
# Figures ----
#============================================#

group_labels <- c(nonhox = "Not owner-occupied in 2008 (lost exclusion)",
                  hox_under_cap = "Owner-occupied, under $1M cap (kept exclusion)",
                  hox_over_cap = "Owner-occupied, over $1M cap (partial)")
save_both <- function(p, name, width = 9, height = 4.5) {
  ggsave(path(figures_dir, paste0(name, ".pdf")), p, width = width, height = height, bg = "transparent")
  ggsave(path(figures_dir, paste0(name, ".png")), p, width = width, height = height, dpi = 180, bg = "transparent")
}

f_es <- dose_es_tab %>%
  filter(group %in% c("nonhox", "hox_under_cap")) %>%
  ggplot(aes(date, 100 * estimate, color = group, fill = group)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  annotate("rect", xmin = as.Date("2020-10-01"), xmax = as.Date("2021-03-01"),
           ymin = -Inf, ymax = Inf, alpha = 0.12) +
  geom_ribbon(aes(ymin = 100 * ci_lo, ymax = 100 * ci_hi), alpha = 0.15, color = NA) +
  geom_line(linewidth = 0.7) +
  geom_point(size = 1.2) +
  scale_color_manual(values = palette_paper()[1:2], labels = group_labels, name = NULL) +
  scale_fill_manual(values = palette_paper()[1:2], labels = group_labels, name = NULL) +
  labs(x = NULL, y = "Change in family-transfer hazard\nper $1,000 of annual wedge (log points x 100)")
save_both(f_es, "F_prop19_dose_event_study")

f_bins <- dose_bins_tab %>%
  filter(period == "steady") %>%
  ggplot(aes(bin_wedge_k * inflator_2017_2023, 100 * (exp(estimate) - 1), color = group)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_pointrange(aes(ymin = 100 * (exp(ci_lo) - 1), ymax = 100 * (exp(ci_hi) - 1)),
                  position = position_dodge(width = 0.3)) +
  geom_line(linewidth = 0.5) +
  scale_color_manual(values = palette_paper()[1:2], labels = group_labels, name = NULL) +
  labs(x = "Mean annual tax wedge in bin ($ thousand, 2023 dollars)",
       y = "Change in family-transfer hazard, 2022-24\nrelative to lowest-wedge bin (%)")
save_both(f_bins, "F_prop19_dose_response_bins")

f_decomp <- exits_decomposition %>%
  filter(str_starts(statistic, "share_")) %>%
  mutate(statistic = factor(statistic,
    levels = c("share_market_estate_trust", "share_market_other", "share_trust",
               "share_other_deed", "share_no_recorded_deed"),
    labels = c("Market sale\n(estate/trust seller)", "Market sale\n(other seller)",
               "Trust / retitle\ndeed", "Other non-arm's-\nlength deed", "No recorded deed\n(deferral)"))) %>%
  ggplot(aes(statistic, 100 * estimate)) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  geom_col(fill = palette_paper()[1], width = 0.6) +
  geom_errorbar(aes(ymin = 100 * boot_lo, ymax = 100 * boot_hi), width = 0.15) +
  labs(x = NULL, y = "Share of deterred family transfers (%)")
save_both(f_decomp, "F_prop19_exit_decomposition")

# |- kill criterion check (pre-registered in the plan) ----
kill_flags <- pretrend_tests %>%
  filter(group %in% c("nonhox", "hox_under_cap")) %>%
  mutate(pretrend_rejects = pretrend_p < 0.05)
if (any(kill_flags$pretrend_rejects)) {
  message("WARNING: pre-trend joint test rejects at 5% for ",
          paste(kill_flags$group[kill_flags$pretrend_rejects], collapse = ", "),
          ". Per the plan, report before interpreting the dose-response.")
}

saveRDS(list(cells_main = cells_main, dose_fam_param = dose_fam_param,
             dose_es = dose_es, dose_es_tab = dose_es_tab, dose_bins = dose_bins_tab,
             pretrend = pretrend_tests, exits_decomposition = exits_decomposition,
             exits_boot = exits_boot, robustness = prop19_robustness,
             ddd_longheld = list(ca = ddd_longheld_ca, placebos = ddd_longheld_placebos,
                                 es = ddd_longheld_es_tab),
             headlines = prop19_headlines),
        path(out_dir, "prop19_dose.rds"))

capture.output(sessionInfo(), file = path(logs_dir, "04_prop19_sessionInfo.txt"))
message("Finished 04_prop19 at ", Sys.time())
