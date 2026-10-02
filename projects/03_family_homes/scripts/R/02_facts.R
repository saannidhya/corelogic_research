# ============================================================
# 02: RQ1 — National facts: the parallel (non-market) housing market
# Author: Saani Rawat
# Purpose: National/state volumes by taxonomy class; family:market ratios;
#          validation moments (zero-price, absentee); property correlates
#          (structure age, value tier, senior exemption) via prop join.
# Inputs:  data/derived/03_family_homes/events.parquet
#          data/corelogic_extracts/by_state/prop/ (correlates join only)
# Outputs: _outputs/tables/fact_national_by_year_class.csv
#          _outputs/tables/fact_state_class_2017_2023.csv
#          fact_headline_ratios.csv, fact_validation_moments.csv
#          fact_age_profile.csv, fact_value_quintiles.csv, fact_senior.csv
#          fact_prop_match_rate.csv
# Log:     10/02/2026: moved facts and property correlates to in-memory R.
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

log_file <- path(logs_dir, "02_facts.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 02_facts at ", Sys.time())

# ============================================================
# Importing event data ----
# ============================================================

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))

# Trust self-transfers are not counted as family transfers.
fam_classes <- c("family_person", "family_other", "family_estate")

# Load all events into R; subsequent calculations use in-memory data frames.
ev_data <- arrow::read_parquet(events_path, as_data_frame = TRUE)
message("Loaded ", format(nrow(ev_data), big.mark = ","), " events into R memory")

# Keep complete calendar years for national facts and validation moments.
ev_window <- ev_data %>%
  filter(between(sale_year, window_start_year, full_year_end))

# ============================================================
# National volumes and validation ----
# ============================================================

# |- 1. National volumes by year and class ----
nat <- ev_window %>%
  group_by(sale_year, class) %>%
  summarize(
    n = n(),
    zero_price_share = mean(is.na(amt) | amt == 0),
    med_pos_price = median(amt[!is.na(amt) & amt > 0]),
    .groups = "drop"
  ) %>%
  arrange(sale_year, class)

write_csv_strict(nat, path(tables_out_dir, "fact_national_by_year_class.csv"))

# |- 2. Headline ratios ----
headline <- ev_window %>%
  group_by(sale_year) %>%
  summarize(
    fam_conservative = sum(class == "family_person", na.rm = TRUE),
    fam_broad = sum(class %in% fam_classes),
    trust = sum(class == "family_trust", na.rm = TRUE),
    market = sum(class == "market_sale", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    ratio_conservative = fam_conservative / market,
    ratio_broad = fam_broad / market
  ) %>%
  arrange(sale_year)

assert_rows(headline, 15, "fact_headline_ratios")
write_csv_strict(headline, path(tables_out_dir, "fact_headline_ratios.csv"))
saveRDS(headline, path(out_dir, "fact_headline_ratios.rds"))

# |- 3. State and class volumes, pooled 2017-2023 ----
ev_pooled <- ev_data %>%
  filter(between(sale_year, 2017L, full_year_end))

st <- ev_pooled %>%
  group_by(state) %>%
  summarize(
    fam_broad = sum(class %in% fam_classes),
    trust = sum(class == "family_trust", na.rm = TRUE),
    market = sum(class == "market_sale", na.rm = TRUE),
    n_all = n(),
    .groups = "drop"
  ) %>%
  arrange(state)

assert_rows(st, 45, "fact_state_class")
write_csv_strict(st, path(tables_out_dir, "fact_state_class_2017_2023.csv"))

# |- 4. Validation moments by class ----
# Missing indicators contribute zero, retaining all events in each denominator.
val <- ev_window %>%
  group_by(class) %>%
  summarize(
    n = n(),
    zero_price = mean(is.na(amt) | amt == 0),
    med_pos_price = median(amt[!is.na(amt) & amt > 0]),
    quitclaim_share = mean(coalesce(dtype == "Q", FALSE)),
    absentee_share_raw = mean(coalesce(absentee_buyer == 1, FALSE)),
    absentee_obs = mean(!is.na(absentee_buyer)),
    corp_buyer_share = mean(coalesce(corp_buyer == 1, FALSE)),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

write_csv_strict(val, path(tables_out_dir, "fact_validation_moments.csv"))

# ============================================================
# Property correlates, 2017-2023 events ----
# ============================================================

# |- Importing and deduplicating property records ----
# Read each state through the shared loader; harmonize types before binding
# because property columns can be numeric in one partition and text in another.
prop_columns <- c("clip", "year_built", "calculated_total_value",
                  "senior_exempt_indicator", "homestead_exempt_indicator")
prop_states <- dir_ls(path(default_parquet_root(), "by_state", "prop"),
                      type = "directory") %>%
  path_file() %>%
  sub("^state=", "", .)

prop_data <- purrr::map_dfr(prop_states, function(st) {
  message("Loading property records: ", st)
  load_corelogic_prop(states = st, columns = prop_columns,
                      per_partition = TRUE) %>%
    transmute(
      # Cast numeric IDs directly; normalize the suffix only for text IDs.
      clip = if (is.character(clip)) {
        bit64::as.integer64(sub("\\.0$", "", clip))
      } else {
        bit64::as.integer64(clip)
      },
      year_built = suppressWarnings(as.integer(round(as.numeric(year_built)))),
      total_value = suppressWarnings(as.numeric(calculated_total_value)),
      senior_exempt = as.integer(coalesce(senior_exempt_indicator == "Y", FALSE)),
      homestead_exempt = as.integer(coalesce(homestead_exempt_indicator == "Y", FALSE)),
      prop_state = state
    )
})

# Keep one row per clip so repeated property records cannot multiply events.
# Prefer the highest value, then newest structure, then alphabetic state.
prop_slim <- prop_data %>%
  filter(!is.na(clip)) %>%
  arrange(clip, desc(total_value), desc(year_built), prop_state) %>%
  distinct(clip, .keep_all = TRUE)

# |- Matching recent events to property records ----
ev_recent <- ev_pooled %>%
  filter(class %in% c(fam_classes, "family_trust", "market_sale")) %>%
  dplyr::select(clip, state, sale_year, class)

n_ev_recent <- nrow(ev_recent)

# SQL joins do not match missing keys; keep that behavior in the R join.
ev_prop <- ev_recent %>%
  left_join(prop_slim %>% mutate(prop_matched = TRUE),
            by = "clip", na_matches = "never", relationship = "many-to-one")

match_rate <- ev_prop %>%
  summarize(
    n_events = n(),
    match_rate = mean(!is.na(prop_matched))
  )

stopifnot(match_rate$n_events == n_ev_recent)
message("Prop join match rate: ", round(match_rate$match_rate, 4),
        " on ", format(match_rate$n_events, big.mark = ","), " events")
write_csv_strict(match_rate, path(tables_out_dir, "fact_prop_match_rate.csv"))

# MEMORY.md lesson: assert the rate, not just non-emptiness.
if (match_rate$match_rate < 0.5) {
  stop("Prop join match rate ", match_rate$match_rate,
       " < 0.5 ? investigate before using correlates")
}

# Property correlates use matched events only, as in the original inner joins.
ev_matched <- ev_prop %>%
  filter(!is.na(prop_matched))

# |- Structure age profile ----
age_profile <- ev_matched %>%
  filter(year_built >= 1800, year_built <= sale_year) %>%
  mutate(
    structure_age = sale_year - year_built,
    age_bin = case_when(
      structure_age < 10 ~ "0-9",
      structure_age < 30 ~ "10-29",
      structure_age < 50 ~ "30-49",
      structure_age < 75 ~ "50-74",
      TRUE ~ "75+"
    )
  ) %>%
  group_by(age_bin) %>%
  summarize(
    fam = sum(class %in% fam_classes),
    market = sum(class == "market_sale", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(age_bin)

assert_rows(age_profile, 4, "fact_age_profile")
write_csv_strict(age_profile, path(tables_out_dir, "fact_age_profile.csv"))

# |- Value quintiles within property state ----
# Assign quintiles across matched events (including trusts), not unique homes.
value_matched <- ev_matched %>%
  filter(total_value > 1000) %>%
  group_by(prop_state) %>%
  mutate(vq = ntile(total_value, 5L)) %>%
  ungroup()

value_q <- value_matched %>%
  group_by(vq) %>%
  summarize(
    fam = sum(class %in% fam_classes),
    market = sum(class == "market_sale", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(vq)

assert_rows(value_q, 5, "fact_value_quintiles")
write_csv_strict(value_q, path(tables_out_dir, "fact_value_quintiles.csv"))

# |- Senior exemption profile ----
senior <- ev_matched %>%
  group_by(senior_exempt) %>%
  summarize(
    fam = sum(class %in% fam_classes),
    market = sum(class == "market_sale", na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(senior_exempt)

write_csv_strict(senior, path(tables_out_dir, "fact_senior.csv"))

message("Finished 02_facts at ", Sys.time())
