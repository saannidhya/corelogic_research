# ============================================================
# 03: RQ2 — The family lock: post-transfer time to market sale
# Author: Saani Rawat
# Purpose: For 2008-2018 transfer cohorts, measure time from event to the
#          property's NEXT arm's-length market sale. Kaplan-Meier survival
#          by event class with right-censoring at 2024-06-30 (last full
#          month of data). Descriptive: selection is the object of interest.
# Inputs:  data/derived/03_family_homes/events.parquet (via events_panel_file in 00_setup.R)
# Outputs: _outputs/tables/hazard_sold_within.csv       2008-2018 cohorts (full follow-up)
#          _outputs/tables/hazard_km_curves.csv         2007-2023 cohorts, monthly life table
#          _outputs/tables/hazard_ratio_by_year.csv     family against market, by year since event
#          _outputs/hazard_summary.rds
# Log:     11/18/2026: Kaplan-Meier curves on every cohort 2007-2023 (censoring handles the
#                      short follow-up of late cohorts); curves by route of family transfer;
#                      hazard ratio of family transfers to market purchases by year since event.
# ============================================================

# 11/04/2026: restored other interfamily to the main family proxy.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

log_file <- path(logs_dir, "03_hazard.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 03_hazard at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(duckdb_temp_dir)}'"))

events_path <- events_panel_path
stopifnot(file_exists(events_path))
ev <- events_main_from(events_path)   # family measure of 2026-11-12 (00_setup.R)

censor_date <- as.Date("2024-06-30")  # data truncate ~Aug 2024; stay conservative

# ---- 1. Event dates as proper DATEs -------------------------------------
# sale_raw is YYYYMMDD int; some records carry day = 00 -> coerce to 01.
date_audit <- create_dated_events(con, ev)
write_csv_strict(date_audit, path(tables_out_dir, "03_date_audit.csv"))

# ---- 2. Cohort events + next market sale --------------------------------
# Every cohort 2007-2023 enters the Kaplan-Meier curves, which handle censoring. The
# sold-within shares of step 3 use the 2008-2018 cohorts only, which all have at least
# 66 months of follow-up before the censoring date.
full_follow_years <- c(2008L, 2018L)
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE cohort AS
  SELECT clip, state, sale_date, sale_year, class, fam_route, corp_buyer, absentee_buyer
  FROM ev_dated
  WHERE sale_year BETWEEN 2007 AND 2023
    AND class IN ('family_person','family_estate','family_other','family_via_trust',
                  'family_trust','family_retitle','market_sale')
")

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE market_sales AS
  SELECT clip, sale_date AS msale_date
  FROM ev_dated
  WHERE class = 'market_sale'
")

dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE spells AS
  SELECT
    c.clip, c.state, c.sale_year, c.class, c.fam_route, c.corp_buyer, c.absentee_buyer,
    c.sale_date,
    MIN(m.msale_date) AS next_market_date
  FROM cohort c
  LEFT JOIN market_sales m
    ON m.clip = c.clip AND m.msale_date > c.sale_date
  GROUP BY ALL
")

n_spells <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM spells")$n
message("Spells: ", format(n_spells, big.mark = ","))
stopifnot(n_spells > 1e6)

# ---- 3. Sold-within shares (12/24/36/60/120 months) ----------------------
sold_within <- dbGetQuery(con, glue("
  SELECT class,
    COUNT(*) AS n,
    AVG(CASE WHEN next_market_date IS NOT NULL
              AND next_market_date <= sale_date + INTERVAL 12 MONTH THEN 1.0 ELSE 0 END) AS sold_12m,
    AVG(CASE WHEN next_market_date IS NOT NULL
              AND next_market_date <= sale_date + INTERVAL 24 MONTH THEN 1.0 ELSE 0 END) AS sold_24m,
    AVG(CASE WHEN next_market_date IS NOT NULL
              AND next_market_date <= sale_date + INTERVAL 36 MONTH THEN 1.0 ELSE 0 END) AS sold_36m,
    AVG(CASE WHEN next_market_date IS NOT NULL
              AND next_market_date <= sale_date + INTERVAL 60 MONTH THEN 1.0 ELSE 0 END) AS sold_60m
  FROM spells
  WHERE sale_year BETWEEN {full_follow_years[1]} AND {full_follow_years[2]}
  GROUP BY class ORDER BY n DESC
"))
assert_rows(sold_within, 4, "hazard_sold_within")
family_sold <- sold_within %>%
  filter(class %in% family_measure_classes) %>%
  summarise(across(starts_with("sold_"), ~ weighted.mean(.x, n)), n = sum(n)) %>%
  mutate(class = "family_all")
sold_within <- bind_rows(sold_within, family_sold)
write_csv_strict(sold_within, path(tables_out_dir, "hazard_sold_within.csv"))

# By-cohort-year version for robustness (compositional stability)
sold_within_yr <- dbGetQuery(con, glue("
  SELECT class, sale_year,
    COUNT(*) AS n,
    AVG(CASE WHEN next_market_date IS NOT NULL
              AND next_market_date <= sale_date + INTERVAL 60 MONTH THEN 1.0 ELSE 0 END) AS sold_60m
  FROM spells
  WHERE sale_year BETWEEN {full_follow_years[1]} AND {full_follow_years[2]}
  GROUP BY class, sale_year ORDER BY class, sale_year
"))
family_sold_yr <- sold_within_yr %>%
  filter(class %in% family_measure_classes) %>%
  group_by(sale_year) %>%
  summarise(sold_60m = weighted.mean(sold_60m, n), n = sum(n), .groups = "drop") %>%
  mutate(class = "family_all")
sold_within_yr <- bind_rows(sold_within_yr, family_sold_yr)
write_csv_strict(sold_within_yr, path(tables_out_dir, "hazard_sold_within_by_year.csv"))

# ---- 4. KM survival curves, all cohorts 2007-2023 ------------------------
# Failure: next market sale at month t; censoring at 2024-06-30. Exit-table
# formulation: count exits per month per type in SQL, build risk sets
# cumulatively in R (avoids a grid x spells cross join).
exits_raw <- dbGetQuery(con, glue("
  WITH s AS (
    SELECT class, fam_route,
      CASE WHEN next_market_date IS NOT NULL THEN
        datediff('month', sale_date, next_market_date)
      ELSE NULL END AS fail_m,
      datediff('month', sale_date, DATE '{censor_date}') AS censor_m
    FROM spells
  ),
  typed AS (
    SELECT class, fam_route,
      CASE WHEN fail_m IS NOT NULL AND fail_m <= censor_m THEN 'fail' ELSE 'censor' END AS exit_type,
      CASE WHEN fail_m IS NOT NULL AND fail_m <= censor_m
           THEN GREATEST(fail_m, 1) ELSE GREATEST(censor_m, 1) END AS exit_m
    FROM s
    WHERE censor_m >= 1
  )
  SELECT class, fam_route, exit_type, exit_m, COUNT(*) AS n
  FROM typed GROUP BY ALL
"))
assert_rows(exits_raw, 400, "hazard_exit_table")

# groups for the curves: every class, the family measure as a whole, and its routes
sum_exits <- function(d, name) {
  d %>% group_by(exit_type, exit_m) %>% summarise(n = sum(n), .groups = "drop") %>% mutate(class = name)
}
fam_exits <- exits_raw %>% filter(class %in% family_measure_classes)
exits <- bind_rows(
  exits_raw %>% group_by(class, exit_type, exit_m) %>% summarise(n = sum(n), .groups = "drop"),
  sum_exits(fam_exits, "family_all"),
  sum_exits(filter(fam_exits, fam_route %in% c("lifetime", "unknown")), "family_lifetime"),
  sum_exits(filter(fam_exits, fam_route %in% c("death_affidavit", "probate")), "family_at_death"),
  sum_exits(filter(fam_exits, fam_route == "trust"), "family_trust_route")
)
# the routes partition the family measure
stopifnot(sum(exits$n[exits$class == "family_all"]) ==
            sum(exits$n[exits$class %in% c("family_lifetime", "family_at_death", "family_trust_route")]))

km_curves <- exits |>
  pivot_wider(names_from = exit_type, values_from = n, values_fill = 0) |>
  complete(class, exit_m = 1:max(exit_m), fill = list(fail = 0, censor = 0)) |>
  arrange(class, exit_m) |>
  group_by(class) |>
  mutate(
    n_total = sum(fail) + sum(censor),
    # at risk entering month t: everyone who hasn't failed or censored before t
    r_t = n_total - lag(cumsum(fail), default = 0) - lag(cumsum(censor), default = 0),
    h_t = if_else(r_t > 0, fail / r_t, 0),
    surv = cumprod(1 - h_t)
  ) |>
  ungroup() |>
  filter(exit_m <= 198) |>          # 16.5 years: January 2007 to the June 2024 censoring date
  rename(t = exit_m, d_t = fail)
write_csv_strict(km_curves, path(tables_out_dir, "hazard_km_curves.csv"))
saveRDS(km_curves, path(out_dir, "hazard_km_curves.rds"))

# Headline gap printed for the log
print(
  km_curves |>
    filter(class %in% c("family_all", "family_lifetime", "family_at_death", "market_sale"),
           t %in% c(12, 24, 60, 120, 180)) |>
    select(class, t, surv) |>
    pivot_wider(names_from = t, values_from = surv, names_prefix = "surv_m")
)

# ---- 5. Hazard of a market sale, family against market, by year since the event ----
# The test of "sooner at first, slower afterwards": within each year since the event, the
# hazard is sales over home-months at risk, and the ratio compares family transfers with
# market purchases. A ratio above one means family-transferred homes are selling faster.
# The standard error of the log ratio is sqrt(1/D_family + 1/D_market).
hazard_year <- km_curves %>%
  filter(class %in% c("family_all", "family_lifetime", "family_at_death", "family_trust_route",
                      "market_sale"), t <= 192) %>%
  mutate(year_since = (t - 1) %/% 12 + 1) %>%
  group_by(class, year_since) %>%
  summarise(sales = sum(d_t), home_months = sum(r_t), .groups = "drop") %>%
  mutate(hazard = sales / home_months)
hazard_ratio <- hazard_year %>%
  filter(class != "market_sale") %>%
  left_join(hazard_year %>% filter(class == "market_sale") %>%
              select(year_since, sales_mkt = sales, hazard_mkt = hazard), by = "year_since") %>%
  mutate(ratio = hazard / hazard_mkt,
         se_log = sqrt(1 / sales + 1 / sales_mkt),
         ratio_lo = exp(log(ratio) - 1.96 * se_log),
         ratio_hi = exp(log(ratio) + 1.96 * se_log))
assert_rows(hazard_ratio, 40, "hazard_ratio_by_year")
write_csv_strict(hazard_ratio, path(tables_out_dir, "hazard_ratio_by_year.csv"))
print(as.data.frame(hazard_ratio %>% filter(class == "family_all") %>%
                      transmute(year_since, sales, hazard_pct_month = round(100 * hazard, 3),
                                market_pct_month = round(100 * hazard_mkt, 3), ratio = round(ratio, 3),
                                ratio_lo = round(ratio_lo, 3), ratio_hi = round(ratio_hi, 3))))

# month at which the family curve crosses above the market curve
cross <- km_curves %>%
  filter(class %in% c("family_all", "market_sale")) %>%
  select(class, t, surv) %>%
  pivot_wider(names_from = class, values_from = surv) %>%
  filter(family_all > market_sale) %>%
  slice_min(t, n = 1)
message("NOTE: the family curve crosses above the market curve at month ", cross$t,
        " (", round(cross$t / 12, 1), " years).")

saveRDS(sold_within, path(out_dir, "hazard_summary.rds"))
message("Finished 03_hazard at ", Sys.time())
