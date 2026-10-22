#======================================================================#
# Purpose : Build the predetermined parcel-level Prop 13 tax wedge for
#           California and each parcel's first ownership exit after two
#           fixed pre-policy baseline dates. This is the treatment-
#           intensity input for the Prop 19 dose-response design in
#           04_prop19.R.
#
#           wedge_i = t_i * max(P_i - A_i, 0)   ($ per year, baseline $)
#             A_i : factored Prop 13 base value at the baseline lien date,
#                   rolled forward from the 2008-09 tax roll (or from the
#                   last market sale after the roll date) with BOE factors
#             P_i : hedonic AVM market value at the baseline date
#             t_i : parcel effective tax rate on the 2008-09 roll
#           An heir who kept the parent's base saved wedge_i every year.
#
# Name    : Saani Rawat
# Created : 10/02/2026
# Log     : 10/02/2026: created (parcel universe, base roll-forward,
#           hedonic Automated Valuation Model (AVM) with holdout, county repeat-sales index, wedge,
#           tenure, value-cap flag, first-exit spells for 2017 and 2019
#           baselines, 2008-owner surname match on exits).
#           10/07/2026: 2019 main distribution and appendix figure script,
#           with reference statistics conditional on positive wedges.
# Inputs  : CA PropertyCharacteristics via load_corelogic_prop()
#           CA OwnerTransfer seller names via load_corelogic_ot()
#           data/derived/03_family_homes/events.parquet
#           scripts/R/inputs/ca_boe_inflation_factors.csv
# Outputs : data/derived/03_family_homes/ca_parcel_wedge.parquet
#           _outputs/tables/wedge_*.csv (linkage, AVM fit, repeat-sales
#           index, wedge distribution, LAO benchmark, spell audit)
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20261002)

log_file <- path(logs_dir, "04a_build_wedge.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 04a_build_wedge at ", Sys.time())

#============================================#
# Settings ----
#============================================#

# baseline (risk-set entry) dates; each is a January 1 lien date
baseline_dates <- c(d2017 = as.Date("2017-01-01"), d2019 = as.Date("2019-01-01"))
censor_date <- as.Date("2024-06-30")
ot_start_date <- as.Date("2007-01-01")  # CA OT is dense from 2007 (sparse before)

# AVM: 2016-2017 market sales, predicted at the January 2017 price level
avm_train_start <- as.Date("2016-01-01")
avm_train_end <- as.Date("2017-12-31")
avm_reference_ym <- 201701L
avm_holdout_share <- 0.20
avm_trim_resid <- 1.5       # drop training sales with |log residual| > 1.5, refit
price_bounds <- c(30000, 2e7)
sqft_bounds <- c(300, 20000)

# effective property tax rate bounds (Prop 13 1% + voter debt, winsorized)
rate_bounds <- c(0.008, 0.020)

# wedge bins ($ per year, baseline dollars); pre-specified before estimation
wedge_breaks <- c(0, 500, 1500, 3000, 5000, 10000, Inf)
wedge_labels <- c("w0_0.5k", "w0.5_1.5k", "w1.5_3k", "w3_5k", "w5_10k", "w10k_plus")

# Prop 19 occupant-heir allowance: exclusion limited to factored base + $1M
prop19_allowance <- 1e6
cap_year <- 2021L

# repeat-sales index: counties with fewer pairs fall back to the statewide index
rs_min_pairs <- 500L
rs_max_abs_dlog <- log(5)

# deed classes
fam_classes <- c("family_person", "family_other", "family_estate")
trust_classes <- c("family_trust", "family_retitle")
other_classes <- c("other_nonarms", "estate_noninterfam")

# small helpers
to_clip <- function(x) as.numeric(as.character(x))
pad_zip5 <- function(x) {
  z <- sub("\\.0$", "", as.character(x))
  z[is.na(z)] <- ""
  case_when(
    nchar(z) %in% 3:5 ~ str_pad(z, 5, pad = "0"),
    nchar(z) %in% 8:9 ~ substr(str_pad(z, 9, pad = "0"), 1, 5),
    TRUE ~ NA_character_
  )
}
pad_fips5 <- function(x) str_pad(sub("\\.0$", "", as.character(x)), 5, pad = "0")

#============================================#
# Prop 13 inflation factors ----
#============================================#

boe_factors <- read_csv(path(r_dir, "inputs", "ca_boe_inflation_factors.csv"),
                        show_col_types = FALSE)
stopifnot(all(boe_factors$factor <= 1.02),
          all(diff(boe_factors$roll_year) == 1L),
          all(c(2007:2024) %in% boe_factors$roll_year))

# cumulative index so roll-forward from base year b to lien year Y is
# cum(Y) / cum(b): factors for roll years b+1, ..., Y
factor_index <- boe_factors %>%
  arrange(roll_year) %>%
  mutate(cum_factor = cumprod(factor))
cum_at <- function(y) factor_index$cum_factor[match(y, factor_index$roll_year)]
roll_forward <- function(value, base_year, lien_year) {
  value * cum_at(lien_year) / cum_at(base_year)
}

#============================================#
# Importing the 2008-09 CA parcel universe ----
#============================================#

prop_columns <- c("clip", "fips_code", "situs_zip_code", "property_indicator_code",
                  "homestead_exempt_indicator", "owner_occupancy_code",
                  "owner_1_last_name", "assessed_total_value", "total_tax_amount",
                  "tax_year", "assessed_year", "universal_building_square_feet",
                  "total_number_of_bedrooms_all_buildings", "total_number_of_bathrooms",
                  "total_land_square_footage", "year_built")

t0 <- Sys.time()
ca_prop_raw <- load_corelogic_prop(states = "CA", columns = prop_columns, per_partition = TRUE)
message("NOTE: loaded ", format(nrow(ca_prop_raw), big.mark = ","), " CA property rows in ",
        round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# |- SFR (10) and condo (11) parcels, one row per clip ----
# Keep the latest roll record; break ties on the larger assessed value.
ca_parcels_2008 <- ca_prop_raw %>%
  mutate(
    clip = to_clip(clip),
    fips5 = pad_fips5(fips_code),
    zip5 = pad_zip5(situs_zip_code),
    type = as.character(property_indicator_code),
    tax_year = as.integer(coalesce(as.numeric(tax_year), as.numeric(assessed_year))),
    assessed_2008 = as.numeric(assessed_total_value),
    tax_2008 = as.numeric(total_tax_amount)
  ) %>%
  filter(type %in% c("10", "11"), !is.na(clip), str_starts(fips5, "06")) %>%
  arrange(clip, desc(tax_year), desc(assessed_2008)) %>%
  distinct(clip, .keep_all = TRUE)
# rm(ca_prop_raw); gc(verbose = FALSE)

n_parcels_raw <- nrow(ca_parcels_2008)
message("NOTE: ", format(n_parcels_raw, big.mark = ","), " unique CA SFR/condo parcels")

# |- cleaning characteristics and the effective tax rate ----
# A missing roll year is assigned the modal 2008 roll so the base can be factored.
ca_parcels_2008 <- ca_parcels_2008 %>%
  mutate(
    tax_year = if_else(is.na(tax_year) | !between(tax_year, 1998L, 2010L), 2008L, tax_year),
    roll_date = as.Date(paste0(tax_year, "-01-01")),
    hox = as.integer(homestead_exempt_indicator %in% "Y"),
    occ_code = owner_occupancy_code,
    owner_last_2008 = toupper(str_trim(coalesce(owner_1_last_name, ""))),
    sqft = as.numeric(universal_building_square_feet),
    sqft = if_else(between(sqft, sqft_bounds[1], sqft_bounds[2]), sqft, NA_real_),
    beds = as.numeric(total_number_of_bedrooms_all_buildings),
    baths = as.numeric(total_number_of_bathrooms),
    lot = as.numeric(total_land_square_footage),
    year_built = as.numeric(year_built),
    year_built = if_else(between(year_built, 1850, 2010), year_built, NA_real_),
    rate_raw = tax_2008 / assessed_2008
  ) %>%
  filter(!is.na(assessed_2008), assessed_2008 > 0) %>%
  group_by(fips5) %>%
  mutate(rate = if_else(is.finite(rate_raw), rate_raw, median(rate_raw[is.finite(rate_raw)], na.rm = TRUE))) %>%
  ungroup() %>%
  mutate(rate = pmin(pmax(rate, rate_bounds[1]), rate_bounds[2])) %>%
  select(clip, fips5, zip5, type, tax_year, roll_date, hox, occ_code, owner_last_2008,
         assessed_2008, rate, sqft, beds, baths, lot, year_built)

message("NOTE: ", format(nrow(ca_parcels_2008), big.mark = ","),
        " parcels with a positive 2008-09 assessed value (",
        round(100 * nrow(ca_parcels_2008) / n_parcels_raw, 1), "% of universe)")
message("NOTE: HOX share = ", round(mean(ca_parcels_2008$hox), 3),
        "; median effective rate = ", round(median(ca_parcels_2008$rate), 4))

#============================================#
# Importing CA deed events ----
#============================================#

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))

ca_events <- arrow::open_dataset(events_path) %>%
  filter(state == "CA") %>%
  select(clip, sale_raw, class, amt, estate_kw, trust_kw, reo, reo_sale) %>%
  collect() %>%
  mutate(clip = to_clip(clip),
         sale_raw = as.numeric(sale_raw),
         sale_date = parse_event_dates(sale_raw)) %>%
  filter(!is.na(sale_date))
message("NOTE: ", format(nrow(ca_events), big.mark = ","), " dated CA deed events")

# market sales drive base resets, tenure, the AVM, and the repeat-sales index
ca_market_sales <- ca_events %>%
  filter(class == "market_sale") %>%
  select(clip, sale_raw, sale_date, amt, reo, reo_sale) %>%
  distinct(clip, sale_date, .keep_all = TRUE)

# |- linkage audit ----
universe_clips <- ca_parcels_2008$clip
wedge_linkage <- tibble(
  n_parcels_universe = nrow(ca_parcels_2008),
  share_parcels_with_any_event = mean(universe_clips %in% ca_events$clip),
  share_events_2017_2024_in_universe = mean(
    ca_events$clip[ca_events$sale_date >= baseline_dates[["d2017"]]] %in% universe_clips),
  share_fam_events_2017_2024_in_universe = mean(
    ca_events$clip[ca_events$sale_date >= baseline_dates[["d2017"]] &
                     ca_events$class %in% fam_classes] %in% universe_clips)
)
print(wedge_linkage)
write_csv_strict(wedge_linkage, path(tables_out_dir, "wedge_linkage.csv"))

#============================================#
# County repeat-sales index (BMN, annual) ----
#============================================#

# Consecutive arm's-length sales of the same parcel; distressed sales and
# implausible jumps are dropped. Index normalized to 2007 = 1.
rs_pairs <- ca_market_sales %>%
  filter(between(amt, price_bounds[1], price_bounds[2]), coalesce(reo_sale, 0L) == 0L) %>%
  arrange(clip, sale_date) %>%
  group_by(clip) %>%
  mutate(prev_date = lag(sale_date), prev_amt = lag(amt)) %>%
  ungroup() %>%
  filter(!is.na(prev_date)) %>%
  mutate(y1 = year(prev_date), y2 = year(sale_date), dlogp = log(amt) - log(prev_amt)) %>%
  filter(y2 > y1, abs(dlogp) <= rs_max_abs_dlog, y1 >= 2007L) %>%
  inner_join(ca_parcels_2008 %>% select(clip, fips5), by = "clip")

rs_years <- 2007:2024
fit_bmn_index <- function(pairs) {
  # design: +1 in the second-sale year, -1 in the first-sale year, 2007 omitted
  x <- matrix(0, nrow(pairs), length(rs_years), dimnames = list(NULL, rs_years))
  x[cbind(seq_len(nrow(pairs)), match(pairs$y2, rs_years))] <- 1
  x[cbind(seq_len(nrow(pairs)), match(pairs$y1, rs_years))] <- -1
  x <- x[, -1, drop = FALSE]
  keep_cols <- colSums(abs(x)) > 0
  beta <- rep(NA_real_, ncol(x))
  beta[keep_cols] <- lm.fit(x[, keep_cols, drop = FALSE], pairs$dlogp)$coefficients
  tibble(year = rs_years, log_index = c(0, beta))
}

rs_index_state <- fit_bmn_index(rs_pairs) %>% mutate(fips5 = "06000")
rs_county_counts <- count(rs_pairs, fips5, name = "n_pairs")
rs_index_county <- rs_pairs %>%
  semi_join(rs_county_counts %>% filter(n_pairs >= rs_min_pairs), by = "fips5") %>%
  group_split(fips5) %>%
  map_dfr(~ fit_bmn_index(.x) %>% mutate(fips5 = .x$fips5[1]))

# counties with thin pairs, or unidentified years, use the statewide index
rs_index <- expand_grid(fips5 = unique(ca_parcels_2008$fips5), year = rs_years) %>%
  left_join(rs_index_county, by = c("fips5", "year")) %>%
  left_join(rs_index_state %>% select(year, log_index_state = log_index), by = "year") %>%
  mutate(index_source = if_else(is.na(log_index), "state", "county"),
         log_index = coalesce(log_index, log_index_state)) %>%
  select(fips5, year, log_index, index_source)
stopifnot(all(is.finite(rs_index$log_index)))
write_csv_strict(rs_index %>% left_join(rs_county_counts, by = "fips5"),
                 path(tables_out_dir, "wedge_rs_index_county_year.csv"))
message("NOTE: repeat-sales index from ", format(nrow(rs_pairs), big.mark = ","), " pairs; ",
        sum(rs_county_counts$n_pairs >= rs_min_pairs), " counties with own index")

rs_growth <- function(fips, from_year, to_year) {
  key_from <- match(paste(fips, from_year), paste(rs_index$fips5, rs_index$year))
  key_to <- match(paste(fips, to_year), paste(rs_index$fips5, rs_index$year))
  exp(rs_index$log_index[key_to] - rs_index$log_index[key_from])
}

#============================================#
# Hedonic AVM (January 2017 price level) ----
#============================================#

t0 <- Sys.time()

# |- feature construction (2008-09 roll characteristics) ----
# Room counts outside 0-20 (the roll has some negatives) are treated as
# missing; fixed factor levels keep prediction levels inside training levels.
add_avm_features <- function(df) {
  df %>%
    mutate(
      log_sqft = log(sqft),
      lot_missing = as.integer(is.na(lot) | lot <= 0),
      log_lot = if_else(lot_missing == 1L, 0, log(pmax(lot, 1))),
      beds = if_else(between(beds, 0, 20), beds, NA_real_),
      baths = if_else(between(baths, 0, 20), baths, NA_real_),
      beds_f = factor(if_else(is.na(beds), "na", as.character(pmin(round(beds), 6))),
                      levels = c(as.character(0:6), "na")),
      baths_f = factor(if_else(is.na(baths), "na", as.character(pmin(round(baths), 5))),
                       levels = c(as.character(0:5), "na")),
      age_bin = cut(2017 - year_built, c(-Inf, 10, 20, 35, 50, 70, Inf)),
      age_bin = fct_na_value_to_level(age_bin, level = "na")
    )
}

# |- training sales ----
avm_sales <- ca_market_sales %>%
  filter(between(sale_date, avm_train_start, avm_train_end),
         between(amt, price_bounds[1], price_bounds[2]),
         coalesce(reo_sale, 0L) == 0L, coalesce(reo, 0L) == 0L) %>%
  inner_join(ca_parcels_2008, by = "clip") %>%
  add_avm_features() %>%
  mutate(log_price = log(amt),
         ym = as.integer(format(sale_date, "%Y%m")),
         holdout = runif(n()) < avm_holdout_share)
avm_train_sales <- avm_sales %>% filter(!holdout)
avm_holdout_sales <- avm_sales %>% filter(holdout)
message("NOTE: AVM sales: ", format(nrow(avm_train_sales), big.mark = ","), " train / ",
        format(nrow(avm_holdout_sales), big.mark = ","), " holdout")

avm_formulas <- list(
  full = log_price ~ log_sqft + log_lot + lot_missing + beds_f + baths_f + age_bin | zip5^type + ym,
  nosqft = log_price ~ log_lot + lot_missing + beds_f + baths_f + age_bin | zip5^type + ym,
  county = log_price ~ 1 | fips5^type + ym
)

# |- fit, trim gross outliers, refit ----
# Duan smearing corrects the retransformation from log price to dollars.
fit_avm <- function(f, train) {
  fit_data <- if ("log_sqft" %in% all.vars(f)) filter(train, !is.na(log_sqft)) else train
  m0 <- feols(f, data = fit_data)
  kept <- fit_data[obs(m0), ] %>% mutate(r = resid(m0)) %>% filter(abs(r) <= avm_trim_resid)
  m1 <- feols(f, data = kept)
  list(model = m1, smear = mean(exp(resid(m1))), n_train = nobs(m1))
}
avm_models <- map(avm_formulas, fit_avm, train = avm_train_sales)

predict_avm <- function(df, models, ym_value = NULL) {
  pred_df <- df
  if (!is.null(ym_value)) pred_df$ym <- ym_value
  preds <- map(models, function(m) {
    p <- predict(m$model, newdata = pred_df)
    exp(p) * m$smear
  })
  # use the richest model that yields a finite prediction
  tibble(
    p_full = preds$full, p_nosqft = preds$nosqft, p_county = preds$county
  ) %>%
    mutate(
      avm_model = case_when(
        !is.na(df$log_sqft) & is.finite(p_full) ~ "full",
        is.finite(p_nosqft) ~ "nosqft",
        is.finite(p_county) ~ "county",
        TRUE ~ NA_character_
      ),
      avm_value = case_when(
        avm_model == "full" ~ p_full,
        avm_model == "nosqft" ~ p_nosqft,
        avm_model == "county" ~ p_county
      )
    ) %>%
    select(avm_model, avm_value)
}

# |- holdout validation (out-of-sample, actual sale month) ----
avm_holdout_fit <- avm_holdout_sales %>%
  bind_cols(predict_avm(avm_holdout_sales, avm_models)) %>%
  filter(!is.na(avm_value)) %>%
  mutate(log_err = log(avm_value) - log_price,
         ape = abs(avm_value / amt - 1))

summarise_fit <- function(df) {
  df %>%
    summarise(n = n(),
              r2_log = 1 - sum(log_err^2) / sum((log_price - mean(log_price))^2),
              median_ape = median(ape),
              share_within_10pct = mean(ape <= 0.10),
              share_within_20pct = mean(ape <= 0.20),
              mean_log_err = mean(log_err),
              .groups = "drop")
}
avm_fit_summary <- bind_rows(
  avm_holdout_fit %>% summarise_fit() %>% mutate(group = "all", level = "all"),
  avm_holdout_fit %>% group_by(level = avm_model) %>% summarise_fit() %>% mutate(group = "avm_model"),
  avm_holdout_fit %>% group_by(level = type) %>% summarise_fit() %>% mutate(group = "type"),
  avm_holdout_fit %>% group_by(level = fips5) %>% summarise_fit() %>% mutate(group = "county")
) %>%
  relocate(group, level) %>%
  left_join(tibble(level = names(avm_models), n_train = map_int(avm_models, "n_train")), by = "level")
print(avm_fit_summary %>% filter(group != "county"), width = Inf)
write_csv_strict(avm_fit_summary, path(tables_out_dir, "wedge_avm_holdout_fit.csv"))
stopifnot(avm_fit_summary$median_ape[avm_fit_summary$group == "all"] < 0.25)

# |- predicting January 2017 value for every parcel ----
parcel_avm <- ca_parcels_2008 %>%
  add_avm_features() %>%
  bind_cols(predict_avm(., avm_models, ym_value = avm_reference_ym)) %>%
  select(clip, avm_model, avm_2017 = avm_value)
message("NOTE: AVM coverage = ", round(100 * mean(!is.na(parcel_avm$avm_2017)), 1), "%; models used:")
print(count(parcel_avm, avm_model))
message("NOTE: AVM built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

#============================================#
# Base values, wedge, and tenure at each baseline ----
#============================================#

# Last market sale on or after the roll's lien date resets the Prop 13 base.
# Family transfers do not reset the base: prior law excluded parent-child
# transfers, so an inherited base is the parent's factored base.
build_baseline_wedge <- function(cohort_name) {
  d0 <- baseline_dates[[cohort_name]]
  lien_year <- year(d0)

  last_reset_sale <- ca_market_sales %>%
    filter(sale_date < d0, between(amt, price_bounds[1], price_bounds[2])) %>%
    inner_join(ca_parcels_2008 %>% select(clip, roll_date), by = "clip") %>%
    filter(sale_date >= roll_date) %>%
    group_by(clip) %>%
    slice_max(sale_date, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    select(clip, reset_date = sale_date, reset_price = amt)

  # tenure: last observed market sale since OT coverage begins (2007)
  last_market_since_2007 <- ca_market_sales %>%
    filter(sale_date < d0, sale_date >= ot_start_date) %>%
    group_by(clip) %>%
    summarise(last_market_date = max(sale_date), .groups = "drop")

  ca_parcels_2008 %>%
    select(clip, fips5, zip5, type, tax_year, hox, occ_code, owner_last_2008,
           assessed_2008, rate) %>%
    left_join(parcel_avm, by = "clip") %>%
    left_join(last_reset_sale, by = "clip") %>%
    left_join(last_market_since_2007, by = "clip") %>%
    mutate(
      cohort = cohort_name,
      d0 = d0,
      # factored base value at the baseline lien date
      base_source = if_else(is.na(reset_date), "roll_2008", "market_sale"),
      assessed_d0 = if_else(
        is.na(reset_date),
        roll_forward(assessed_2008, tax_year, lien_year),
        roll_forward(reset_price, year(reset_date), lien_year)
      ),
      # market value at baseline: AVM (Jan 2017 level) moved by the county index
      market_d0 = avm_2017 * rs_growth(fips5, 2017L, lien_year),
      # robustness valuation: last 2007+ arm's-length price moved by the index
      market_rs_d0 = if_else(
        !is.na(reset_date) & year(reset_date) >= 2007L,
        reset_price * rs_growth(fips5, year(reset_date), lien_year),
        NA_real_
      ),
      wedge = rate * pmax(market_d0 - assessed_d0, 0),
      wedge_rs = rate * pmax(market_rs_d0 - assessed_d0, 0),
      assess_ratio = assessed_d0 / market_d0,
      wedge_bin = cut(wedge, wedge_breaks, labels = wedge_labels, right = FALSE),
      tenure = case_when(
        is.na(last_market_date) ~ "long_held",
        year(last_market_date) <= 2012L ~ "bought_2007_2012",
        TRUE ~ "bought_2013_plus"
      ),
      # Prop 19 occupant-heir cap evaluated at 2021 values
      market_2021 = market_d0 * rs_growth(fips5, lien_year, cap_year),
      assessed_2021 = roll_forward(assessed_d0, lien_year, cap_year),
      over_cap_2021 = as.integer(market_2021 - assessed_2021 > prop19_allowance)
    ) %>%
    filter(!is.na(wedge))
}

#============================================#
# First ownership exit after each baseline ----
#============================================#

# Spells end at the first family transfer or market sale (a market sale wins
# a same-date collision). Trust/retitle and other non-arm's-length deeds do
# not end the spell: a living-trust retitle leaves family control unchanged.
# Their first occurrence inside the spell is recorded for competing risks.
build_first_exits <- function(cohort_name) {
  d0 <- baseline_dates[[cohort_name]]
  window_events <- ca_events %>%
    filter(sale_date >= d0, sale_date <= censor_date)

  first_ownership_exit <- window_events %>%
    filter(class %in% c(fam_classes, "market_sale")) %>%
    mutate(priority = if_else(class == "market_sale", 0L, 1L)) %>%
    arrange(clip, sale_date, priority) %>%
    distinct(clip, .keep_all = TRUE) %>%
    transmute(
      clip, end_date = sale_date, end_sale_raw = sale_raw,
      end_type = case_when(
        class == "market_sale" & (estate_kw | trust_kw) ~ "market_estate_trust",
        class == "market_sale" ~ "market_other",
        TRUE ~ class
      )
    )

  first_of_classes <- function(classes, name) {
    window_events %>%
      filter(class %in% classes) %>%
      group_by(clip) %>%
      summarise(!!name := min(sale_date), .groups = "drop")
  }

  first_ownership_exit %>%
    full_join(first_of_classes(trust_classes, "trust_first_date"), by = "clip") %>%
    full_join(first_of_classes(other_classes, "other_first_date"), by = "clip")
}

#============================================#
# Seller surname on exit deeds (2008-owner match) ----
#============================================#

# Does the deed's seller carry the 2008 owner's surname? A match indicates the
# 2008 owner line still held title at exit.
ot_sellers <- load_corelogic_ot(states = "CA", years = 2017:2024,
                                columns = c("clip", "sale_derived_date", "seller_1_last_name"),
                                per_partition = TRUE) %>%
  transmute(clip = to_clip(clip),
            end_sale_raw = as.numeric(sale_derived_date),
            seller_last = toupper(str_trim(coalesce(seller_1_last_name, "")))) %>%
  filter(seller_last != "") %>%
  distinct(clip, end_sale_raw, .keep_all = TRUE)
message("NOTE: ", format(nrow(ot_sellers), big.mark = ","), " CA 2017-2024 deeds with a seller surname")

#============================================#
# Assembling parcel-cohort spells ----
#============================================#

t0 <- Sys.time()
parcel_wedge <- map_dfr(names(baseline_dates), function(cohort_name) {
  baseline_wedge <- build_baseline_wedge(cohort_name)
  first_exits <- build_first_exits(cohort_name)
  baseline_wedge %>%
    left_join(first_exits, by = "clip") %>%
    left_join(ot_sellers, by = c("clip", "end_sale_raw")) %>%
    mutate(
      end_type = coalesce(end_type, "censored"),
      spell_end = coalesce(end_date, censor_date),
      # trust and other deeds count only while the parcel is still at risk
      trust_first_date = if_else(trust_first_date <= spell_end, trust_first_date, as.Date(NA)),
      other_first_date = if_else(other_first_date <= spell_end, other_first_date, as.Date(NA)),
      exit_same_dynasty = if_else(end_type == "censored" | is.na(seller_last) | owner_last_2008 == "",
                                  NA_integer_, as.integer(seller_last == owner_last_2008))
    ) %>%
    select(-end_sale_raw, -end_date)
})
message("NOTE: spells built in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")

# |- degeneracy guards ----
spell_audit <- parcel_wedge %>%
  group_by(cohort) %>%
  summarise(
    n_parcels = n(),
    share_fam_exit = mean(end_type %in% fam_classes),
    share_market_exit = mean(str_starts(end_type, "market")),
    share_censored = mean(end_type == "censored"),
    share_trust_first = mean(!is.na(trust_first_date)),
    share_other_first = mean(!is.na(other_first_date)),
    share_same_dynasty_fam = mean(exit_same_dynasty[end_type %in% fam_classes], na.rm = TRUE),
    share_same_dynasty_market = mean(exit_same_dynasty[str_starts(end_type, "market")], na.rm = TRUE),
    .groups = "drop"
  )
print(spell_audit, width = Inf)
write_csv_strict(spell_audit, path(tables_out_dir, "wedge_spell_audit.csv"))

bin_counts <- count(parcel_wedge, cohort, wedge_bin, hox)
print(bin_counts, n = 30)
write_csv_strict(bin_counts, path(tables_out_dir, "wedge_bin_counts.csv"))

stopifnot(
  all(spell_audit$n_parcels > 5e6),
  all(spell_audit$share_fam_exit > 0.005),
  all(!is.na(parcel_wedge$wedge_bin)),
  all(bin_counts %>% group_by(cohort, wedge_bin) %>% summarise(n = sum(n), .groups = "drop") %>% pull(n) > 50000)
)

#============================================#
# Wedge distribution and LAO benchmark ----
#============================================#

wedge_distribution <- parcel_wedge %>%
  group_by(cohort, tenure, hox) %>%
  summarise(
    n = n(),
    mean_wedge = mean(wedge), median_wedge = median(wedge),
    p10_wedge = quantile(wedge, 0.10), p90_wedge = quantile(wedge, 0.90),
    share_zero_wedge = mean(wedge == 0),
    median_assessed = median(assessed_d0), median_market = median(market_d0),
    median_assess_ratio = median(assess_ratio),
    share_over_cap_2021 = mean(over_cap_2021),
    .groups = "drop"
  )
print(wedge_distribution, width = Inf)
write_csv_strict(wedge_distribution, path(tables_out_dir, "wedge_distribution_tenure_hox.csv"))

# Main and appendix figures use the 2019 baseline; reference statistics exclude zeros.
source(path(r_dir, '04b_wedge_figures.R'))
build_wedge_figures(parcel_wedge)

# LAO (2017) put the annual saving for a long-held Los Angeles home at $3k-$4k
wedge_lao_benchmark <- parcel_wedge %>%
  filter(cohort == "d2017", fips5 == "06037", tenure == "long_held") %>%
  summarise(n = n(), median_wedge = median(wedge), mean_wedge = mean(wedge),
            p25_wedge = quantile(wedge, 0.25), p75_wedge = quantile(wedge, 0.75)) %>%
  mutate(lao_low = 3000, lao_high = 4000)
print(wedge_lao_benchmark)
write_csv_strict(wedge_lao_benchmark, path(tables_out_dir, "wedge_lao_benchmark_la_longheld.csv"))

# AVM versus repeat-sales valuation where both exist
wedge_avm_vs_rs <- parcel_wedge %>%
  filter(!is.na(market_rs_d0)) %>%
  group_by(cohort) %>%
  summarise(n = n(),
            cor_log_value = cor(log(market_d0), log(market_rs_d0)),
            median_log_ratio = median(log(market_d0 / market_rs_d0)),
            cor_wedge = cor(wedge, wedge_rs),
            .groups = "drop")
print(wedge_avm_vs_rs)
write_csv_strict(wedge_avm_vs_rs, path(tables_out_dir, "wedge_avm_vs_repeat_sales.csv"))

#============================================#
# Exporting the parcel wedge panel ----
#============================================#

out_parquet <- path(data_dir, "ca_parcel_wedge.parquet")
arrow::write_parquet(
  parcel_wedge %>% mutate(wedge_bin = as.character(wedge_bin)),
  out_parquet, compression = "zstd"
)
message("NOTE: wrote ", format(nrow(parcel_wedge), big.mark = ","), " parcel-cohort rows to ", out_parquet)
capture.output(sessionInfo(), file = path(logs_dir, "04a_build_wedge_sessionInfo.txt"))
message("Finished 04a_build_wedge at ", Sys.time())
