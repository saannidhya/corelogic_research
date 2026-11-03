# ============================================================
# 04: Proposition 19: retiming, dose-response, and allocation
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
#          _outputs/tables/prop19_robustness_*.csv
# Log:     10/22/2026: export figures as PNG only.
#          10/02/2026: moved event aggregation and spells into R memory.
#          10/02/2026: rebuilt around parcel wedge dose-response, competing
#          risks, and cross-state DDD. Aggregate sections and
#          appendix outputs kept byte-identical in logic and filenames.
#          10/22/2026: anticipation now starts at the June 2020 ballot
#          placement (constants from 00_setup.R): bunching, pre-fit, placebo,
#          DDD transition, dose-response periods, and fake reform aligned;
#          Nov 2020-Feb 2021 rush kept as a sensitivity window.
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

# policy windows (YYYYMM): ballot placement Jun 24 2020 (anticipation start, set
# in 00_setup.R); passage Nov 3 2020; parent-child deadline Feb 16 2021
antic_start_ym <- prop19_antic_start_ym
antic_end_ym <- prop19_antic_end_ym
pre_end_ym <- prop19_pre_end_ym
rush_start_ym <- 202011L      # passage-to-deadline sub-window, kept as a sensitivity
post21_end_ym <- 202112L      # Mar-Dec 2021: post-deadline transition
steady_start_ym <- 202201L    # 2022 onward: steady state
es_ref_quarter <- "2020Q1"    # last clean quarter before anticipation (Q2 includes June)

# fake reform for the pre-period placebo (same 9-month window, two years earlier)
fake_antic <- c(201806L, 201902L)
fake_post_start <- 201903L
fake_sample_end <- pre_end_ym

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
ev_data <- read_events_df(events_path)
message("Loaded ", format(nrow(ev_data), big.mark = ","), " events into R memory")

# Family transfers = same surname + estate/executor; family_other (interfamily flag,
# no surname match) is excluded (decided 10/31/2026).
fam_classes <- c("family_person", "family_estate")
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

window_antic <- cf %>% filter(ym >= antic_start_ym, ym <= antic_end_ym)
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
    pre_end_ym = pre_end_ym,
    pre_rmspe = sqrt(mean(gap_fam[ym >= pre_start_ym & ym <= pre_end_ym]^2)),
    pre_mae = mean(abs(gap_fam[ym >= pre_start_ym & ym <= pre_end_ym])),
    antic_gap = sum(excess_fam[ym >= antic_start_ym & ym <= antic_end_ym]) /
      sum(cf_fam[ym >= antic_start_ym & ym <= antic_end_ym]),
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
  "anticipation", antic_start_ym, antic_end_ym, "positive",
  "rush_202011_202102", rush_start_ym, antic_end_ym, "positive",
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
message("Cohort sizes â€” fam: ", format(sum(cells_fam$n), big.mark = ","),
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
    mutate(own_exits = family_person + family_estate +
             market_estate_trust + market_other,
           at_risk = n0 - lag(cumsum(own_exits), default = 0)) %>%
    ungroup() %>%
    mutate(
      fam = family_person + family_estate,
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
