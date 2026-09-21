#!/usr/bin/env Rscript

project_dir <- file.path("projects", "03_family_homes")
tables_dir <- file.path(project_dir, "scripts", "R", "_outputs", "tables")

required_files <- c(
  "prop19_cf_placebo_monthly.csv",
  "prop19_cf_placebo_summary.csv",
  "prop19_window_sensitivity.csv"
)

missing_files <- required_files[!file.exists(file.path(tables_dir, required_files))]
if (length(missing_files) > 0) {
  stop("Missing Prop 19 counterfactual artifact(s): ",
       paste(missing_files, collapse = ", "), call. = FALSE)
}

monthly <- read.csv(file.path(tables_dir, "prop19_cf_placebo_monthly.csv"),
                    stringsAsFactors = FALSE)
summary <- read.csv(file.path(tables_dir, "prop19_cf_placebo_summary.csv"),
                    stringsAsFactors = FALSE)
sensitivity <- read.csv(file.path(tables_dir, "prop19_window_sensitivity.csv"),
                        stringsAsFactors = FALSE)

need_monthly <- c(
  "base_window", "treated_state", "ym", "sale_year", "sale_month",
  "actual_fam", "donor_fam", "base_treated_fam", "base_donor_fam",
  "cf_fam", "excess_fam", "gap_fam", "donor_count",
  "treated_in_donor"
)
need_summary <- c(
  "base_window", "treated_state", "n_states", "donor_count",
  "pre_start_ym", "pre_end_ym", "pre_rmspe", "pre_mae",
  "antic_gap", "feb_gap", "post2223_gap",
  "antic_ratio", "feb_ratio", "post2223_ratio",
  "antic_rank_ratio", "feb_rank_ratio", "post2223_rank_ratio",
  "antic_p_ratio", "feb_p_ratio", "post2223_p_ratio"
)
need_sensitivity <- c(
  "base_window", "window", "start_ym", "end_ym", "direction",
  "ca_gap", "ca_ratio", "raw_rank", "raw_p", "ratio_rank",
  "ratio_p", "n_states"
)

missing_monthly_cols <- setdiff(need_monthly, names(monthly))
missing_summary_cols <- setdiff(need_summary, names(summary))
missing_sensitivity_cols <- setdiff(need_sensitivity, names(sensitivity))

if (length(missing_monthly_cols) > 0) {
  stop("Monthly artifact missing columns: ",
       paste(missing_monthly_cols, collapse = ", "), call. = FALSE)
}
if (length(missing_summary_cols) > 0) {
  stop("Summary artifact missing columns: ",
       paste(missing_summary_cols, collapse = ", "), call. = FALSE)
}
if (length(missing_sensitivity_cols) > 0) {
  stop("Sensitivity artifact missing columns: ",
       paste(missing_sensitivity_cols, collapse = ", "), call. = FALSE)
}

expected_windows <- c("2019", "2018_2019")
if (!all(expected_windows %in% unique(summary$base_window))) {
  stop("Summary artifact must include base windows: ",
       paste(expected_windows, collapse = ", "), call. = FALSE)
}
if (!all(expected_windows %in% unique(monthly$base_window))) {
  stop("Monthly artifact must include base windows: ",
       paste(expected_windows, collapse = ", "), call. = FALSE)
}

if (any(monthly$treated_in_donor != 0)) {
  stop("A treated state appears in its own donor pool.", call. = FALSE)
}
if (any(monthly$donor_count != 50)) {
  stop("Every leave-one-out counterfactual should use 50 donor states.", call. = FALSE)
}
if (any(monthly$base_donor_fam <= 0) || any(monthly$cf_fam <= 0)) {
  stop("Counterfactual denominators and predictions must be positive.", call. = FALSE)
}

if (any(summary$pre_start_ym < 201801) || any(summary$pre_end_ym > 202010)) {
  stop("Pre-fit diagnostics must be bounded by 2018m1-2020m10.", call. = FALSE)
}
if (any(!is.finite(summary$pre_rmspe)) || any(summary$pre_rmspe <= 0)) {
  stop("Pre-fit RMSPE must be finite and positive.", call. = FALSE)
}
if (any(summary$donor_count != summary$n_states - 1)) {
  stop("Summary donor count must equal n_states - 1.", call. = FALSE)
}

pairs <- list(
  c("antic_rank_ratio", "antic_p_ratio"),
  c("feb_rank_ratio", "feb_p_ratio"),
  c("post2223_rank_ratio", "post2223_p_ratio")
)
for (pair in pairs) {
  expected_p <- summary[[pair[1]]] / summary$n_states
  if (any(abs(summary[[pair[2]]] - expected_p) > 1e-12)) {
    stop("P-value/rank mismatch for ", pair[2], ".", call. = FALSE)
  }
}

ca_2019 <- summary[summary$base_window == "2019" & summary$treated_state == "CA", ]
if (nrow(ca_2019) != 1) {
  stop("Expected exactly one CA row for the 2019 baseline.", call. = FALSE)
}
if (ca_2019$antic_rank_ratio != 1 ||
    ca_2019$feb_rank_ratio != 1 ||
    ca_2019$post2223_rank_ratio != 1) {
  stop("CA should be top-ranked by RMSPE-adjusted anticipation, February, and 2022-2023 gaps.",
       call. = FALSE)
}

expected_sensitivity_windows <- c("anticipation", "february", "post_202103_202212",
                                  "steady_2022_2023")
if (!all(expected_sensitivity_windows %in% unique(sensitivity$window))) {
  stop("Sensitivity artifact missing expected windows: ",
       paste(setdiff(expected_sensitivity_windows, unique(sensitivity$window)),
             collapse = ", "), call. = FALSE)
}
if (any(abs(sensitivity$raw_p - sensitivity$raw_rank / sensitivity$n_states) > 1e-12) ||
    any(abs(sensitivity$ratio_p - sensitivity$ratio_rank / sensitivity$n_states) > 1e-12)) {
  stop("Sensitivity p-values must equal rank divided by n_states.", call. = FALSE)
}

cat("Prop 19 counterfactual artifact invariants passed.\n")
