# Calendar-date construction shared by event-cohort analyses.
# Explicit parsing checks the calendar without relying on numeric coercion.
# On the current extract this reproduces the old date construction exactly.
# Preserve the original convention that day 00 means day 01; reject other
# impossible calendar dates rather than silently rolling into another month.
event_date_sql <- function(field = "sale_raw") {
  glue("CAST(TRY_STRPTIME(CAST(CAST(({field} - {field} % 100 + ",
       "CASE WHEN {field} % 100 = 0 THEN 1 ELSE {field} % 100 END) ",
       "AS BIGINT) AS VARCHAR), '%Y%m%d') AS DATE)")
}

create_dated_events <- function(con, ev) {
  dbExecute(con, glue("CREATE OR REPLACE TEMP VIEW ev_date_audit AS
    SELECT *, {event_date_sql()} AS sale_date FROM {ev}"))
  audit <- dbGetQuery(con, "SELECT COUNT(*) AS n,
    SUM(CASE WHEN sale_raw % 100 = 0 THEN 1 ELSE 0 END) AS n_day_imputed,
    SUM(CASE WHEN sale_date IS NULL THEN 1 ELSE 0 END) AS n_invalid
    FROM ev_date_audit")
  print(audit)
  stopifnot(audit$n > 0, audit$n_invalid / audit$n < 0.001)
  dbExecute(con, "CREATE OR REPLACE TEMP VIEW ev_dated AS
    SELECT * FROM ev_date_audit WHERE sale_date IS NOT NULL")
  invisible(audit)
}

# ============================================================
# In-memory calendar and next-sale helpers ----
# Added 10/02/2026 for the hazard and Prop 19 scripts.
# ============================================================

parse_event_dates <- function(sale_raw) {
  # YYYYMMDD fits exactly in a double, including when Arrow returns integer64.
  raw_date <- as.numeric(sale_raw)
  normalized <- raw_date - raw_date %% 100 + ifelse(raw_date %% 100 == 0, 1, raw_date %% 100)
  date_text <- sprintf("%08.0f", normalized)
  sale_date <- as.Date(date_text, format = "%Y%m%d")

  # Reject impossible dates even if the platform parser rolls them forward.
  valid <- !is.na(raw_date) & !is.na(sale_date) & format(sale_date, "%Y%m%d") == date_text
  sale_date[!valid] <- as.Date(NA)
  sale_date
}

add_calendar_months <- function(date, months) {
  # SQL INTERVAL MONTH clamps month-end dates (e.g., Feb 29 + 12 months).
  lubridate::add_with_rollback(date, lubridate::period(month = months))
}

calendar_month_difference <- function(start_date, end_date) {
  # DuckDB datediff('month') counts calendar boundaries, ignoring day of month.
  12L * (lubridate::year(end_date) - lubridate::year(start_date)) +
    lubridate::month(end_date) - lubridate::month(start_date)
}

join_next_market_sale <- function(cohort, market_sales) {
  # GROUP BY ALL in the original query collapsed identical selected cohort rows.
  # Deduplicate market dates so a tied source sale cannot multiply a spell.
  cohort %>%
    distinct() %>%
    left_join(
      market_sales %>% distinct(clip, msale_date),
      by = join_by(clip, closest(sale_date < msale_date)),
      na_matches = "never", relationship = "many-to-one"
    ) %>%
    rename(next_market_date = msale_date)
}
