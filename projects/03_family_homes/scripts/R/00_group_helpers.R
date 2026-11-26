#' Helpers for the by-group re-runs (Phase 3). Sourced by 16-19 after 00_setup.R.
#'
#' Needs bygroup_defs (00_groups.R, sourced here if absent), fixest, tidyverse.
#' Nothing here reads data/ or writes outside the paths it is given.

if (!exists("bygroup_defs", inherits = FALSE)) {
  source(here::here("projects/03_family_homes/scripts/R/00_groups.R"))
}

# ---- filter builder -------------------------------------------------------

#' SQL filter for one group
#' @param name Group name in bygroup_defs.
#' @param defs Group table.
#' @return Single SQL boolean expression string, parenthesized.
bygroup_filter <- function(name, defs = bygroup_defs) {
  stopifnot(length(name) == 1L, name %in% defs$name)
  paste0("(", defs$filter_sql[defs$name == name], ")")
}

#' SELECT-list fragment counting rows per group in one scan
#'
#' Each group becomes `SUM(CASE WHEN <filter> THEN 1 ELSE 0 END) AS g_<name>`.
#' NULL filter results (e.g. NULL fam_vehicle) count as FALSE.
#' @param names Group names (default all).
#' @param defs Group table.
#' @param weight SQL expression summed instead of 1 (e.g. "n" on the cube).
#' @param prefix Column prefix.
#' @return One string, comma separated.
bygroup_select_sql <- function(names = bygroup_defs$name, defs = bygroup_defs,
                               weight = "1", prefix = "g_") {
  stopifnot(all(names %in% defs$name))
  paste(vapply(names, function(g) {
    glue::glue("SUM(CASE WHEN coalesce({bygroup_filter(g, defs)}, FALSE) ",
               "THEN {weight} ELSE 0 END) AS {prefix}{g}")
  }, character(1)), collapse = ",\n       ")
}

#' Group-by-state-month counts from any table with the panel columns
#' @param con duckdb connection.
#' @param source SQL relation, e.g. "read_parquet('...')".
#' @param where SQL WHERE body (no keyword), e.g. "sale_year BETWEEN 2016 AND 2024".
#' @param names Group names.
#' @param by Grouping columns.
#' @param weight "1" for the panel, "n" for the cube.
#' @return Wide tibble: `by` columns and one g_<name> column per group.
bygroup_count_query <- function(con, source, where = "TRUE", names = bygroup_defs$name,
                                by = c("state", "ym", "sale_year"), weight = "1") {
  by_sql <- paste(by, collapse = ", ")
  sel <- bygroup_select_sql(names, weight = weight)
  tibble::as_tibble(DBI::dbGetQuery(con, glue::glue(
    "SELECT {by_sql},\n       {sel}\n FROM {source}\n WHERE {where}\n GROUP BY {by_sql}")))
}

# ---- zero-cell rule -------------------------------------------------------

#' Drop states with any empty cell for a group (decision 2026-11-11: no PPML)
#'
#' A state is dropped when any of its state-year cells in `years` has a
#' non-positive count, or the state is missing a year, because log(0) would
#' silently remove cells. The count of dropped states is returned and should be
#' saved with bygroup_save_dropped().
#' @param panel Tibble with state, sale_year and the count column.
#' @param n_col Name of the count column.
#' @param years Years used in the regression.
#' @param group Group name (recorded in the result).
#' @param keep_states States never dropped (default "CA"; an empty CA cell stops).
#' @return List: data (filtered panel), dropped (character), summary (one row).
bygroup_drop_zero_states <- function(panel, n_col = "n", years, group,
                                     keep_states = "CA") {
  d <- panel %>% filter(sale_year %in% years)
  cells <- d %>%
    group_by(state) %>%
    summarise(n_years = n_distinct(sale_year), min_n = min(.data[[n_col]]),
              .groups = "drop")
  bad <- cells$state[cells$min_n <= 0 | cells$n_years < length(years)]
  if (any(bad %in% keep_states)) {
    stop("Group ", group, ": protected state has an empty cell (",
         paste(intersect(bad, keep_states), collapse = ","), ")")
  }
  out <- d %>% filter(!state %in% bad)
  stopifnot(nrow(out) > 0L)
  list(
    data = out, dropped = sort(bad),
    summary = tibble(group = group, years = paste(range(years), collapse = "-"),
                     n_states_in = n_distinct(d$state), n_dropped = length(bad),
                     dropped_states = paste(sort(bad), collapse = " "))
  )
}

#' Save the dropped-state counts, replacing earlier rows for the same group
#' @param tbl Rows from bygroup_drop_zero_states()$summary.
#' @param file CSV path (under _outputs/tables, name ending _bygroup).
bygroup_save_dropped <- function(tbl, file) {
  old <- if (file.exists(file)) readr::read_csv(file, show_col_types = FALSE,
                                                col_types = readr::cols(.default = "c")) else NULL
  new <- tbl %>% mutate(across(everything(), as.character))
  all <- bind_rows(if (!is.null(old)) old %>% filter(!group %in% new$group), new)
  write_csv_strict(all, file)
  invisible(all)
}

# ---- sample sizes printed for every regression ----------------------------

#' Fit a fixest model and print its sample size line
#'
#' Prints group, label, nobs, number of states and California deeds in the
#' pre and post periods. Inference is state-clustered, as in the old scripts.
#' @param formula fixest formula.
#' @param data Data with state, sale_year and `n_col`.
#' @param group Group name.
#' @param label Short model label (e.g. "DDD").
#' @param n_col Deed count column, used for the California deed totals.
#' @param pre_years,post_years Years used.
#' @param cluster Cluster formula.
#' @param ... Passed to fixest::feols.
#' @return List: model, info (one-row tibble with the printed numbers).
bygroup_feols <- function(formula, data, group, label, n_col = "n",
                          pre_years = 2017:2019, post_years = 2022:2023,
                          cluster = ~state, ...) {
  m <- fixest::feols(formula, data = data, cluster = cluster, ...)
  ca <- data %>% filter(state == "CA")
  ca_n <- function(yrs) {
    d <- ca %>% filter(sale_year %in% yrs)
    if ("grp" %in% names(d)) d <- d %>% filter(grp != "n_market")
    sum(d[[n_col]])
  }
  info <- tibble(group = group, model = label, nobs = stats::nobs(m),
                 n_states = n_distinct(data$state),
                 ca_deeds_pre = ca_n(pre_years), ca_deeds_post = ca_n(post_years))
  message(sprintf("[%s] %s: nobs=%d, states=%d, CA deeds pre=%s post=%s",
                  group, label, info$nobs, info$n_states,
                  format(info$ca_deeds_pre, big.mark = ","),
                  format(info$ca_deeds_post, big.mark = ",")))
  list(model = m, info = info)
}

#' Groups below the California-deed threshold (descriptive series only)
#' @param counts Tibble with state, sale_year, group, n.
#' @param pre_years,post_years Periods.
#' @param min_deeds CA deeds a year required in each period (mean per year).
#' @return Tibble: group, ca_per_year_pre, ca_per_year_post, below.
bygroup_below_threshold <- function(counts, pre_years = 2017:2019, post_years = 2022:2023,
                                    min_deeds = bygroup_min_ca_deeds) {
  counts %>%
    filter(state == "CA") %>%
    group_by(group) %>%
    summarise(ca_per_year_pre = sum(n[sale_year %in% pre_years]) / length(pre_years),
              ca_per_year_post = sum(n[sale_year %in% post_years]) / length(post_years),
              .groups = "drop") %>%
    mutate(below = pmin(ca_per_year_pre, ca_per_year_post) < min_deeds)
}

# ---- reproduction check against the EXISTING outputs ----------------------

# Tolerances by kind of number. v2 differs from the October v1 panel in 232 keys
# whose `state` differs, because the October build broke cross-state duplicate ties
# arbitrarily; two of them touch main_proxy California counts (202008, 202203), so
# deed counts move by a few deeds and percentages in the 4th decimal.
bygroup_repro_tol <- list(
  coef  = 5e-5,   # DiD, DDD, event-study phases, retiming_ratio
  count = 5,      # deeds: antic_actual, antic_cf, excess_deeds, missing_post
  pct   = 2e-3,   # percentage points: excess_pct, missing_pct
  exact = 0       # nobs_ddd, n_states_ddd
)
bygroup_repro_kind <- function(item) {
  dplyr::case_when(
    item %in% c("antic_actual", "antic_cf", "excess_deeds", "missing_post") ~ "count",
    item %in% c("excess_pct", "missing_pct") ~ "pct",
    item %in% c("nobs_ddd", "n_states_ddd") ~ "exact",
    TRUE ~ "coef")
}

#' Compare the main_proxy results with the saved legacy files; stop on any gap
#'
#' Compares to the files written by 04/07/09b/07 (never to hard-coded numbers),
#' absolute tolerance `tol`. Supply as many elements of `res` as the caller has;
#' every supplied element is checked and any NA, missing legacy file or
#' difference stops with an error listing all failures.
#' @param res Named list, any of: did_fam, ddd, nobs_ddd, n_states_ddd,
#'   excess_deeds, antic_actual, antic_cf, excess_pct, missing_post,
#'   missing_pct, retiming_ratio, es_phases (tibble: phase, mean_log_points),
#'   deeds_per_clip.
#' @param tol Named list of absolute tolerances by kind (default bygroup_repro_tol),
#'   or one number applied to every item.
#' @param tables_dir Directory of the legacy CSVs.
#' @return Invisibly, the comparison tibble.
bygroup_repro_check <- function(res, tol = bygroup_repro_tol, tables_dir = tables_out_dir) {
  rd <- function(f) {
    p <- file.path(tables_dir, f)
    if (!file.exists(p)) stop("Legacy output missing: ", p)
    readr::read_csv(p, show_col_types = FALSE)
  }
  legacy <- list()
  if (any(c("did_fam") %in% names(res))) {
    d <- rd("prop19_did_volume.csv"); legacy$did_fam <- d$coef[d$model == "fam"]
  }
  if (any(c("ddd", "nobs_ddd", "n_states_ddd") %in% names(res))) {
    d <- rd("prop19_ddd_volume.csv")
    legacy$ddd <- d$estimate; legacy$nobs_ddd <- d$nobs; legacy$n_states_ddd <- d$n_states
  }
  if (any(c("excess_deeds", "antic_actual", "antic_cf", "excess_pct", "missing_post",
            "missing_pct") %in% names(res))) {
    d <- rd("prop19_bunching_summary.csv")
    legacy$excess_deeds <- d$excess_antic_window; legacy$antic_actual <- d$antic_actual
    legacy$antic_cf <- d$antic_cf; legacy$excess_pct <- d$excess_pct
    legacy$missing_post <- d$missing_post_window; legacy$missing_pct <- d$missing_pct
  }
  if ("retiming_ratio" %in% names(res)) legacy$retiming_ratio <- rd("prop19_retiming_summary.csv")$retiming_ratio
  if ("deeds_per_clip" %in% names(res)) legacy$deeds_per_clip <- rd("ref_reconciliation.csv")$deeds_per_fam_clip
  out <- tibble(item = character(), new = double(), legacy = double())
  for (k in intersect(names(res), names(legacy))) {
    out <- bind_rows(out, tibble(item = k, new = as.numeric(res[[k]]),
                                 legacy = as.numeric(legacy[[k]])))
  }
  if ("es_phases" %in% names(res)) {
    lp <- rd("prop19_event_study_phases.csv")
    j <- inner_join(res$es_phases %>% select(phase, new = mean_log_points),
                    lp %>% select(phase, legacy = mean_log_points), by = "phase")
    stopifnot(nrow(j) == nrow(lp))
    out <- bind_rows(out, j %>% transmute(item = paste0("es_", phase), new, legacy))
  }
  unknown <- setdiff(names(res), c(names(legacy), "es_phases"))
  if (length(unknown)) stop("bygroup_repro_check: no legacy file for: ", paste(unknown, collapse = ", "))
  out <- out %>% mutate(kind = bygroup_repro_kind(item),
                        tol_used = if (is.list(tol)) unlist(tol)[kind] else tol,
                        diff = abs(new - legacy),
                        ok = is.finite(diff) & diff <= tol_used)
  print(as.data.frame(out), digits = 12)
  if (!nrow(out) || !all(out$ok)) {
    stop("main_proxy does not reproduce the existing outputs beyond its tolerances: ",
         paste(out$item[!out$ok], collapse = ", "))
  }
  message("main_proxy reproduces the existing outputs (", nrow(out), " items within their tolerances)")
  invisible(out)
}
