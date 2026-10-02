# ============================================================
# 03: RQ2 — The family lock: post-transfer time to market sale
# Author: Saani Rawat
# Purpose: For 2008-2018 transfer cohorts, measure time from event to the
#          property's NEXT arm's-length market sale. Kaplan-Meier survival
#          by event class with right-censoring at 2024-06-30 (last full
#          month of data). Descriptive: selection is the object of interest.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: _outputs/tables/hazard_sold_within.csv
#          _outputs/tables/hazard_km_curves.csv
#          _outputs/hazard_summary.rds
# Log:     10/02/2026: moved hazard calculations into R memory.
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

log_file <- path(logs_dir, "03_hazard.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 03_hazard at ", Sys.time())

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev_data <- arrow::read_parquet(events_path, as_data_frame = TRUE)
message("Loaded ", format(nrow(ev_data), big.mark = ","), " events into R memory")

censor_date <- as.Date("2024-06-30")  # data truncate ~Aug 2024; stay conservative

# ============================================================
# Importing and preparing event dates ----
# ============================================================

# Day 00 becomes day 01; exclude other invalid calendar dates after auditing.
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
write_csv_strict(date_audit, path(tables_out_dir, "03_date_audit.csv"))

ev_dated <- ev_date_audit %>%
  filter(!is.na(sale_date))

# ============================================================
# Building transfer spells ----
# ============================================================

# Cohorts 2008-2018 guarantee at least 66 months of follow-up before censoring.
cohort_classes <- c("family_person", "family_other", "family_estate",
                    "family_trust", "family_retitle", "market_sale")
cohort <- ev_dated %>%
  filter(between(sale_year, 2008L, 2018L), class %in% cohort_classes) %>%
  dplyr::select(clip, state, sale_year, class, corp_buyer, absentee_buyer, sale_date)

market_sales <- ev_dated %>%
  filter(class == "market_sale") %>%
  dplyr::select(clip, msale_date = sale_date)

# Find the first strictly later market sale; a same-day sale does not qualify.
spells <- join_next_market_sale(cohort, market_sales)
n_spells <- nrow(spells)
message("Spells: ", format(n_spells, big.mark = ","))
stopifnot(n_spells > 1e6)

# ============================================================
# Calculating sold-within shares ----
# ============================================================

# Calendar-month cutoffs reproduce the original SQL interval comparisons.
spell_outcomes <- spells %>%
  mutate(
    sold_12m = coalesce(next_market_date <= add_calendar_months(sale_date, 12L), FALSE),
    sold_24m = coalesce(next_market_date <= add_calendar_months(sale_date, 24L), FALSE),
    sold_36m = coalesce(next_market_date <= add_calendar_months(sale_date, 36L), FALSE),
    sold_60m = coalesce(next_market_date <= add_calendar_months(sale_date, 60L), FALSE)
  )

sold_within <- spell_outcomes %>%
  group_by(class) %>%
  summarize(
    n = n(),
    across(c(sold_12m, sold_24m, sold_36m, sold_60m), mean),
    .groups = "drop"
  ) %>%
  arrange(desc(n))
assert_rows(sold_within, 4, "hazard_sold_within")
write_csv_strict(sold_within, path(tables_out_dir, "hazard_sold_within.csv"))

# By-cohort-year version for robustness (compositional stability)
sold_within_yr <- spell_outcomes %>%
  group_by(class, sale_year) %>%
  summarize(n = n(), sold_60m = mean(sold_60m), .groups = "drop") %>%
  arrange(class, sale_year)
write_csv_strict(sold_within_yr, path(tables_out_dir, "hazard_sold_within_by_year.csv"))

# ============================================================
# Calculating Kaplan-Meier survival ----
# ============================================================

# Preserve the original monthly clock, including same-month failure/censor ties.
# Sold-within shares above retain the original unrestricted next-sale windows.
spell_exits <- spells %>%
  mutate(
    fail_m = calendar_month_difference(sale_date, next_market_date),
    censor_m = calendar_month_difference(sale_date, censor_date)
  ) %>%
  filter(censor_m >= 1L) %>%
  mutate(
    observed_failure = !is.na(fail_m) & fail_m <= censor_m,
    exit_type = if_else(observed_failure, "fail", "censor"),
    exit_m = if_else(observed_failure, pmax(fail_m, 1L), pmax(censor_m, 1L))
  )

exits <- spell_exits %>%
  count(class, exit_type, exit_m, name = "n")
assert_rows(exits, 400, "hazard_exit_table")

km_curves <- exits %>%
  pivot_wider(names_from = exit_type, values_from = n, values_fill = 0) %>%
  complete(class, exit_m = 1:max(exit_m), fill = list(fail = 0, censor = 0)) %>%
  arrange(class, exit_m) %>%
  group_by(class) %>%
  mutate(
    n_total = sum(fail) + sum(censor),
    # at risk entering month t: everyone who hasn't failed or censored before t
    r_t = n_total - lag(cumsum(fail), default = 0) - lag(cumsum(censor), default = 0),
    h_t = if_else(r_t > 0, fail / r_t, 0),
    surv = cumprod(1 - h_t)
  ) %>%
  ungroup() %>%
  filter(exit_m <= 120) %>%
  rename(t = exit_m, d_t = fail) %>%
  dplyr::select(class, t, censor, d_t, n_total, r_t, h_t, surv)
write_csv_strict(km_curves, path(tables_out_dir, "hazard_km_curves.csv"))
saveRDS(km_curves, path(out_dir, "hazard_km_curves.rds"))

# Headline gap printed for the log
print(
  km_curves %>%
    filter(t %in% c(24, 60, 120)) %>%
    select(class, t, surv) %>%
    pivot_wider(names_from = t, values_from = surv, names_prefix = "surv_m")
)

saveRDS(sold_within, path(out_dir, "hazard_summary.rds"))
message("Finished 03_hazard at ", Sys.time())
