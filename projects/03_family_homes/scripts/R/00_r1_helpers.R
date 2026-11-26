# ============================================================
# 00_r1_helpers: shared helpers for the R1 review analyses (22, 23, 24)
# Author: Saani Rawat
# Purpose: One definition each of (1) the DuckDB connection used by R1 (memory cap and a
#          spill folder on a local disk outside OneDrive), (2) the business-day calendar
#          (weekdays less US federal holidays), (3) the monthly counterfactual of
#          17_bygroup_designs.R generalized to any base year, and (4) the RONE macro writer
#          (one macro file and one sources CSV shared by the three scripts).
# Inputs:  none (sourced after 00_setup.R and 00_groups.R)
# Outputs: none here; r1_save_numbers() writes _outputs/r1_numbers.tex and r1_number_sources.csv
# Log:     11/13/2026: created (R1 of plans/2026-11-12_review-analyses.md).
# ============================================================

r1_territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")
r1_numbers_tex <- path(out_dir, "r1_numbers.tex")
r1_numbers_csv <- path(out_dir, "r1_number_sources.csv")

# ---- 1. DuckDB connection ----
#' DuckDB for R1: about 25% of the machine's 63 GB, spill outside the OneDrive tree
#' @param memory_limit DuckDB memory cap.
#' @param spill_dir Local spill folder (env R1_SPILL overrides).
#' @return DBI connection.
r1_open_duckdb <- function(memory_limit = Sys.getenv("R1_DUCKDB_MEM", "15GB"),
                           spill_dir = Sys.getenv("R1_SPILL", "C:/duckdb_spill_R1")) {
  con <- open_corelogic_duckdb(memory_limit = memory_limit,
                               threads = max(1L, min(8L, parallel::detectCores() - 2L)))
  dir_create(spill_dir)
  invisible(dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(spill_dir)}'")))
  con
}

# ---- 2. business-day calendar ----
#' US federal holidays (observed dates) for the given years
#'
#' New Year, Martin Luther King, Presidents, Memorial, Juneteenth (from 2021), Independence,
#' Labor, Columbus, Veterans, Thanksgiving, Christmas. A Saturday holiday is observed on the
#' Friday before, a Sunday holiday on the Monday after.
#' @param years Integer years.
#' @return Date vector.
r1_federal_holidays <- function(years) {
  nth_wday <- function(y, m, wday, n) {
    d <- seq(as.Date(sprintf("%d-%02d-01", y, m)), by = "day", length.out = 31L)
    d <- d[as.integer(format(d, "%m")) == m & as.integer(format(d, "%u")) == wday]
    if (n > 0) d[n] else d[length(d)]
  }
  observed <- function(d) {
    wd <- as.integer(format(d, "%u"))
    d + ifelse(wd == 6L, -1L, ifelse(wd == 7L, 1L, 0L))
  }
  fixed <- function(y, m, dd) observed(as.Date(sprintf("%d-%02d-%02d", y, m, dd)))
  out <- lapply(years, function(y) {
    h <- c(fixed(y, 1, 1), nth_wday(y, 1, 1, 3), nth_wday(y, 2, 1, 3), nth_wday(y, 5, 1, -1),
           fixed(y, 7, 4), nth_wday(y, 9, 1, 1), nth_wday(y, 10, 1, 2), fixed(y, 11, 11),
           nth_wday(y, 11, 4, 4), fixed(y, 12, 25))
    if (y >= 2021L) h <- c(h, fixed(y, 6, 19))
    h
  })
  sort(unique(do.call(c, out)))
}

#' Daily calendar with a business-day flag
#' @param from,to Dates.
#' @return Tibble: date, day_raw (yyyymmdd), year, ym, is_bday.
r1_calendar <- function(from, to) {
  d <- seq(as.Date(from), as.Date(to), by = "day")
  hol <- r1_federal_holidays(seq(as.integer(format(min(d), "%Y")) - 1L,
                                 as.integer(format(max(d), "%Y")) + 1L))
  tibble(date = d, day_raw = as.integer(format(d, "%Y%m%d")),
         year = as.integer(format(d, "%Y")),
         ym = as.integer(format(d, "%Y%m")),
         is_bday = !format(d, "%u") %in% c("6", "7") & !d %in% hol)
}

#' Deeds per business day in a calendar window
#'
#' Every deed signed in the window counts (weekend and holiday signings included); the
#' denominator is the number of business days in the window.
#' @param daily Tibble with date and n for ONE series (zero days present or absent).
#' @param cal r1_calendar() output covering the window.
#' @param from,to Window ends (Dates, inclusive).
#' @return Numeric: deeds per business day.
r1_per_bday <- function(daily, cal, from, to) {
  n <- sum(daily$n[daily$date >= from & daily$date <= to])
  b <- sum(cal$is_bday[cal$date >= from & cal$date <= to])
  stopifnot(b > 0L)
  n / b
}

# ---- 3. counterfactual (17_bygroup_designs.R section A, any base year) ----
#' California counterfactual for one group: CA(base, month) * donors(m) / donors(base, month)
#'
#' Same arithmetic as bg_cf() in 17_bygroup_designs.R; the base year is an argument so the
#' design can be rerun as if a deadline fell in another year.
#' @param counts Long state-month counts: state, ym, sale_year, group, n.
#' @param g Group name.
#' @param base_years Base calendar year(s) (mean over years when several).
#' @param from_ym,to_ym Months returned (the base months must lie inside them).
#' @return Tibble by ym: ca_actual, dn_actual, ca_base, dn_base, cf, excess, gap.
r1_cf <- function(counts, g, base_years, from_ym, to_ym) {
  d <- counts %>% filter(group == g, ym >= from_ym, ym <= to_ym)
  ca <- d %>% filter(state == "CA") %>% group_by(ym, sale_year) %>%
    summarise(ca_actual = sum(n), .groups = "drop")
  dn <- d %>% filter(state != "CA") %>% group_by(ym) %>%
    summarise(dn_actual = sum(n), .groups = "drop")
  full <- ca %>% left_join(dn, by = "ym") %>%
    mutate(dn_actual = coalesce(dn_actual, 0), sale_month = ym %% 100L)
  stopifnot(all(base_years %in% full$sale_year))
  base <- full %>% filter(sale_year %in% base_years) %>% group_by(sale_month) %>%
    summarise(ca_base = mean(ca_actual), dn_base = mean(dn_actual), .groups = "drop")
  stopifnot(nrow(base) == 12L)
  full %>% left_join(base, by = "sale_month") %>%
    mutate(cf = if_else(dn_base > 0, ca_base * dn_actual / dn_base, NA_real_),
           excess = ca_actual - cf,
           gap = if_else(!is.na(cf) & cf > 0, ca_actual / cf - 1, NA_real_)) %>%
    arrange(ym)
}

#' Sum of a column over months from..to (YYYYMM, inclusive); stops if a month is missing
r1_wsum <- function(df, col, from, to) {
  sel <- df$ym >= from & df$ym <= to
  stopifnot(any(sel), all(is.finite(df[[col]][sel])))
  sum(df[[col]][sel])
}

#' Months from..to inclusive as YYYYMM integers
r1_months <- function(from, to) {
  d <- seq(as.Date(sprintf("%d-%02d-01", from %/% 100L, from %% 100L)),
           as.Date(sprintf("%d-%02d-01", to %/% 100L, to %% 100L)), by = "month")
  as.integer(format(d, "%Y%m"))
}

#' Add k months to a YYYYMM integer
r1_add_months <- function(ym, k) {
  m0 <- (ym %/% 100L) * 12L + (ym %% 100L - 1L) + as.integer(k)
  as.integer((m0 %/% 12L) * 100L + m0 %% 12L + 1L)
}

# ---- 4. RONE macros ----
#' Collect one macro row (letters only, finite value)
r1_number <- function(name, value, source, digits = 2L) {
  stopifnot(length(value) == 1L, is.finite(value), grepl("^RONE[A-Za-z]+$", name))
  tibble(macro = name, value = as.numeric(value), source = source,
         formatted = formatC(value, format = "f", digits = digits, big.mark = ","))
}

#' Write this script's macros into the shared file, replacing its earlier rows
#' @param rows Tibble of r1_number() rows.
#' @param script Script tag (e.g. "22").
r1_save_numbers <- function(rows, script) {
  stopifnot(nrow(rows) > 0L, !anyDuplicated(rows$macro))
  rows <- rows %>% mutate(script = script)
  old <- if (file_exists(r1_numbers_csv)) {
    read_csv(r1_numbers_csv, show_col_types = FALSE, col_types = cols(.default = "c")) %>%
      mutate(value = as.numeric(value)) %>% filter(script != !!script)
  } else NULL
  all <- bind_rows(old, rows) %>% arrange(script, macro)
  if (anyDuplicated(all$macro)) stop("Duplicate RONE macro across scripts: ",
                                     paste(all$macro[duplicated(all$macro)], collapse = ", "))
  write_csv_strict(all, r1_numbers_csv)
  writeLines(c("% R1 review analyses (scripts 22, 23, 24). Generated; do not edit.",
               paste0("\\newcommand{\\", all$macro, "}{", all$formatted, "}")), r1_numbers_tex)
  message("RONE macros: ", nrow(rows), " from script ", script, "; ", nrow(all), " in ",
          r1_numbers_tex)
  invisible(all)
}
