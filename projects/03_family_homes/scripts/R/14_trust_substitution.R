#======================================================================#
# Purpose : Test whether California trust self-transfers rose after
#           Proposition 19 and whether they substitute for direct family
#           deeds. Trust-flagged family deeds are split by direction using
#           the deed names: into a trust (owner retitles to their own trust,
#           estate planning) versus out of a trust (trustee distributes to a
#           recipient, an intergenerational transfer in substance). Each
#           class gets the headline design: an annual CA x Post DiD, a
#           triple difference against market sales, and a monthly
#           transparent counterfactual over the anticipation and 2022-2023
#           windows.
# Name    : Saani Rawat
# Created : 10/22/2026
# Log     : 10/22/2026: created (trust direction split, DiD/DDD with state
#           placebos, monthly counterfactual, family + distribution composite).
# Inputs  : data/derived/03_family_homes/events.parquet
#           CoreLogic OT buyer/seller names via register_corelogic_parquet()
# Outputs : _outputs/tables/trust_sub_state_year.csv
#           _outputs/tables/trust_sub_did.csv
#           _outputs/tables/trust_sub_monthly_cf.csv
#           _outputs/tables/trust_sub_ca_monthly.csv
#           _outputs/trust_substitution.rds
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

log_file <- path(logs_dir, "14_trust_substitution.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 14_trust_substitution at ", Sys.time())

# Settings ----
fam_classes <- c("family_person", "family_other", "family_estate")
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")
pre_years <- 2017:2019
post_years <- 2022:2023

#============================================#
# Importing deed names for trust-flagged family deeds ----
#============================================#

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
register_corelogic_parquet(con, "ot", view_name = "ot_source")

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev_sql <- glue("read_parquet('{sql_quote_path(events_path)}')")

# Trust direction per parcel-date: any record of the event naming a trust on
# the buyer side, the seller side, or both
trust_direction <- dbGetQuery(con, glue("
  WITH trust_events AS (
    SELECT clip, sale_raw, state, ym, sale_year, same_surname
    FROM {ev_sql}
    WHERE class = 'family_trust' AND sale_year BETWEEN 2017 AND 2023
  ),
  trust_names AS (
    SELECT TRY_CAST(clip AS BIGINT) AS clip,
           TRY_CAST(sale_derived_date AS BIGINT) AS sale_raw,
           bool_or(upper(coalesce(buyer_1_full_name, '')) LIKE '%TRUST%')  AS buyer_trust,
           bool_or(upper(coalesce(seller_1_full_name, '')) LIKE '%TRUST%') AS seller_trust
    FROM ot_source
    WHERE TRY_CAST(sale_derived_date AS BIGINT) BETWEEN 20170101 AND 20231231
      AND (upper(coalesce(buyer_1_full_name, '')) LIKE '%TRUST%'
           OR upper(coalesce(seller_1_full_name, '')) LIKE '%TRUST%')
    GROUP BY 1, 2
  )
  SELECT e.state, e.ym, e.sale_year,
         CASE WHEN n.buyer_trust AND NOT n.seller_trust THEN 'trust_in'
              WHEN n.seller_trust AND NOT n.buyer_trust THEN 'trust_out'
              WHEN n.buyer_trust AND n.seller_trust THEN 'trust_both'
              ELSE 'trust_unmatched' END AS direction,
         e.same_surname,
         COUNT(*) AS n
  FROM trust_events e
  LEFT JOIN trust_names n USING (clip, sale_raw)
  GROUP BY 1, 2, 3, 4, 5
"))
assert_rows(trust_direction, 5000, "trust_direction")

direction_share <- trust_direction %>%
  group_by(direction) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  mutate(share = n / sum(n))
print(direction_share)
stopifnot(direction_share$share[direction_share$direction == "trust_unmatched"] < 0.05)

# Family and market counts on the same state-month grid
base_counts <- dbGetQuery(con, glue("
  SELECT state, ym, sale_year,
         SUM(CASE WHEN class IN ('family_person','family_other','family_estate')
                  THEN 1 ELSE 0 END) AS n_fam,
         SUM(CASE WHEN class = 'family_trust' THEN 1 ELSE 0 END) AS n_trust,
         SUM(CASE WHEN class = 'market_sale' THEN 1 ELSE 0 END)  AS n_market
  FROM {ev_sql}
  WHERE sale_year BETWEEN 2017 AND 2023
  GROUP BY 1, 2, 3
"))

#============================================#
# Building the state-month panel ----
#============================================#

# Trust distributions are the closest analogue of a direct family deed
# recorded through a trust (the surname match is uninformative for trusts)
direction_wide <- trust_direction %>%
  mutate(n = as.numeric(n)) %>%
  group_by(state, ym, sale_year) %>%
  summarise(
    n_trust_in = sum(n[direction == "trust_in"]),
    n_trust_out = sum(n[direction == "trust_out"]),
    n_trust_both = sum(n[direction == "trust_both"]),
    .groups = "drop"
  )

panel_month <- base_counts %>%
  mutate(across(c(n_fam, n_trust, n_market), as.numeric)) %>%
  left_join(direction_wide, by = c("state", "ym", "sale_year")) %>%
  mutate(across(starts_with("n_"), ~ replace_na(.x, 0)),
         n_fam_plus_out = n_fam + n_trust_out,
         n_fam_plus_trust = n_fam + n_trust) %>%
  filter(!state %in% territories)
stopifnot(n_distinct(panel_month$state) == 51)

outcomes <- c(n_fam = "Family deeds (headline)",
              n_trust = "Trust self-transfers, all",
              n_trust_in = "Into trust (retitle to own trust)",
              n_trust_out = "Out of trust (distribution)",
              n_trust_both = "Trust to trust",
              n_fam_plus_out = "Family deeds + trust distributions",
              n_fam_plus_trust = "Family deeds + all trust deeds")

# |- Annual state-year panel for the DiD and triple difference ----
panel_year <- panel_month %>%
  filter(sale_year %in% c(pre_years, post_years)) %>%
  group_by(state, sale_year) %>%
  summarise(across(starts_with("n_"), sum), .groups = "drop") %>%
  mutate(post = as.integer(sale_year >= min(post_years)))
write_csv_strict(panel_year, path(tables_out_dir, "trust_sub_state_year.csv"))

ca_levels <- panel_year %>%
  filter(state == "CA") %>%
  group_by(post) %>%
  summarise(across(all_of(names(outcomes)), mean), .groups = "drop")
message("California annual means, pre (2017-19) vs post (2022-23):")
print(ca_levels, width = Inf)

#============================================#
# Estimating DiD and triple difference by outcome ----
#============================================#

# DiD: log(outcome) ~ CA x Post | state + year. DDD: the same coefficient net
# of the market-sale DiD, which equals the stacked group-interacted regression.
fit_did <- function(panel, treated_state, outcome) {
  d <- panel %>%
    mutate(tr = as.integer(state == treated_state)) %>%
    filter(.data[[outcome]] > 0)
  # a state with zero counts in every pre or post year leaves no estimate
  m <- tryCatch(
    feols(as.formula(paste0("log(", outcome, ") ~ tr:post | state + sale_year")),
          data = d, notes = FALSE, warn = FALSE),
    error = function(e) NULL)
  if (is.null(m) || !"tr:post" %in% names(coef(m))) return(NA_real_)
  unname(coef(m)["tr:post"])
}

states <- sort(unique(panel_year$state))
did_market <- map_dbl(set_names(states), ~ fit_did(panel_year, .x, "n_market"))

did_results <- map_dfr(names(outcomes), function(y) {
  did_all <- map_dbl(set_names(states), ~ fit_did(panel_year, .x, y))
  ddd_all <- did_all - did_market
  ca_did <- did_all[["CA"]]
  ca_ddd <- ddd_all[["CA"]]
  tibble(
    outcome = y,
    label = outcomes[[y]],
    did = ca_did,
    did_pct = 100 * (exp(ca_did) - 1),
    did_perm_p = mean(abs(did_all) >= abs(ca_did), na.rm = TRUE),
    ddd = ca_ddd,
    ddd_pct = 100 * (exp(ca_ddd) - 1),
    ddd_rank = sum(abs(ddd_all) >= abs(ca_ddd), na.rm = TRUE),
    ddd_perm_p = mean(abs(ddd_all) >= abs(ca_ddd), na.rm = TRUE),
    n_states = sum(!is.na(ddd_all))
  )
})
message("CA x Post DiD and net-of-market triple difference, 2017-19 vs 2022-23:")
print(did_results, width = Inf)
write_csv_strict(did_results, path(tables_out_dir, "trust_sub_did.csv"))

# |- Checking the headline reproduces ----
stopifnot(abs(did_results$ddd[did_results$outcome == "n_fam"] - (-0.2627)) < 1e-3)

#============================================#
# Calculating monthly transparent counterfactuals ----
#============================================#

# CA(m) = CA(2019, same calendar month) x donors(m) / donors(2019, same month)
build_cf <- function(outcome) {
  ca <- panel_month %>% filter(state == "CA") %>% select(ym, sale_year, y = all_of(outcome))
  donors <- panel_month %>%
    filter(state != "CA") %>%
    group_by(ym, sale_year) %>%
    summarise(dn = sum(.data[[outcome]]), .groups = "drop")
  ca %>%
    left_join(donors, by = c("ym", "sale_year")) %>%
    mutate(month = ym %% 100) %>%
    left_join(ca %>% filter(sale_year == 2019) %>% transmute(month = ym %% 100, ca_base = y),
              by = "month") %>%
    left_join(donors %>% filter(sale_year == 2019) %>% transmute(month = ym %% 100, dn_base = dn),
              by = "month") %>%
    mutate(cf = ca_base * dn / dn_base, outcome = outcome)
}

cf_monthly <- map_dfr(names(outcomes), build_cf)
write_csv_strict(cf_monthly, path(tables_out_dir, "trust_sub_ca_monthly.csv"))

cf_windows <- cf_monthly %>%
  group_by(outcome) %>%
  summarise(
    antic_excess = sum((y - cf)[ym >= prop19_antic_start_ym & ym <= prop19_antic_end_ym]),
    antic_pct = 100 * antic_excess /
      sum(cf[ym >= prop19_antic_start_ym & ym <= prop19_antic_end_ym]),
    steady_gap = sum((y - cf)[ym >= 202201]),
    steady_pct = 100 * steady_gap / sum(cf[ym >= 202201]),
    pre_rmspe_pct = 100 * sqrt(mean(((y - cf) / cf)[ym <= prop19_pre_end_ym]^2)),
    .groups = "drop"
  ) %>%
  mutate(label = outcomes[outcome])
message("Monthly counterfactual windows (Jun 2020-Feb 2021; 2022-2023):")
print(cf_windows, width = Inf)
write_csv_strict(cf_windows, path(tables_out_dir, "trust_sub_monthly_cf.csv"))

saveRDS(list(direction_share = direction_share, panel_year = panel_year,
             did = did_results, cf_windows = cf_windows, ca_levels = ca_levels),
        path(out_dir, "trust_substitution.rds"))
message("Finished 14_trust_substitution at ", Sys.time())
