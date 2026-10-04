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
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

log_file <- path(logs_dir, "03_hazard.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 03_hazard at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(path(data_dir, 'duckdb_tmp'))}'"))

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev <- glue("read_parquet('{sql_quote_path(events_path)}')")

censor_date <- as.Date("2024-06-30")  # data truncate ~Aug 2024; stay conservative

# ---- 1. Event dates as proper DATEs -------------------------------------
# sale_raw is YYYYMMDD int; some records carry day = 00 -> coerce to 01.
date_audit <- create_dated_events(con, ev)
write_csv_strict(date_audit, path(tables_out_dir, "03_date_audit.csv"))

# ---- 2. Cohort events + next market sale --------------------------------
# Cohorts 2008-2018 guarantee >= 66 months of follow-up before censoring.
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE cohort AS
  SELECT clip, state, sale_date, sale_year, class, corp_buyer, absentee_buyer
  FROM ev_dated
  WHERE sale_year BETWEEN 2008 AND 2018
    AND class IN ('family_person','family_other','family_estate',
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
    c.clip, c.state, c.sale_year, c.class, c.corp_buyer, c.absentee_buyer,
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
  GROUP BY class ORDER BY n DESC
"))
assert_rows(sold_within, 4, "hazard_sold_within")
write_csv_strict(sold_within, path(tables_out_dir, "hazard_sold_within.csv"))

# By-cohort-year version for robustness (compositional stability)
sold_within_yr <- dbGetQuery(con, glue("
  SELECT class, sale_year,
    COUNT(*) AS n,
    AVG(CASE WHEN next_market_date IS NOT NULL
              AND next_market_date <= sale_date + INTERVAL 60 MONTH THEN 1.0 ELSE 0 END) AS sold_60m
  FROM spells
  GROUP BY class, sale_year ORDER BY class, sale_year
"))
write_csv_strict(sold_within_yr, path(tables_out_dir, "hazard_sold_within_by_year.csv"))

# ---- 4. KM survival curves (monthly grid to 120m) ------------------------
# Failure: next market sale at month t; censoring at 2024-06-30. Exit-table
# formulation: count exits per month per type in SQL, build risk sets
# cumulatively in R (avoids a grid x spells cross join).
exits <- dbGetQuery(con, glue("
  WITH s AS (
    SELECT class,
      CASE WHEN next_market_date IS NOT NULL THEN
        datediff('month', sale_date, next_market_date)
      ELSE NULL END AS fail_m,
      datediff('month', sale_date, DATE '{censor_date}') AS censor_m
    FROM spells
  ),
  typed AS (
    SELECT class,
      CASE WHEN fail_m IS NOT NULL AND fail_m <= censor_m THEN 'fail' ELSE 'censor' END AS exit_type,
      CASE WHEN fail_m IS NOT NULL AND fail_m <= censor_m
           THEN GREATEST(fail_m, 1) ELSE GREATEST(censor_m, 1) END AS exit_m
    FROM s
    WHERE censor_m >= 1
  )
  SELECT class, exit_type, exit_m, COUNT(*) AS n
  FROM typed GROUP BY class, exit_type, exit_m
"))
assert_rows(exits, 400, "hazard_exit_table")

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
  filter(exit_m <= 120) |>
  rename(t = exit_m, d_t = fail)
write_csv_strict(km_curves, path(tables_out_dir, "hazard_km_curves.csv"))
saveRDS(km_curves, path(out_dir, "hazard_km_curves.rds"))

# Headline gap printed for the log
print(
  km_curves |>
    filter(t %in% c(24, 60, 120)) |>
    select(class, t, surv) |>
    pivot_wider(names_from = t, values_from = surv, names_prefix = "surv_m")
)

saveRDS(sold_within, path(out_dir, "hazard_summary.rds"))
message("Finished 03_hazard at ", Sys.time())
