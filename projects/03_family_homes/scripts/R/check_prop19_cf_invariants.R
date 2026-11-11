# 11/04/2026: removed state-placebo inference and synchronized current outputs.
#!/usr/bin/env Rscript

project_dir <- file.path("projects", "03_family_homes")
tables_dir <- file.path(project_dir, "scripts", "R", "_outputs", "tables")

required_files <- c(
  "prop19_cf_diagnostic_monthly.csv",
  "prop19_cf_diagnostic_summary.csv",
  "prop19_window_sensitivity.csv"
)

missing_files <- required_files[!file.exists(file.path(tables_dir, required_files))]
if (length(missing_files) > 0) {
  stop("Missing Prop 19 counterfactual artifact(s): ",
       paste(missing_files, collapse = ", "), call. = FALSE)
}

monthly <- read.csv(file.path(tables_dir, "prop19_cf_diagnostic_monthly.csv"),
                    stringsAsFactors = FALSE)
summary <- read.csv(file.path(tables_dir, "prop19_cf_diagnostic_summary.csv"),
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
  "antic_ratio", "feb_ratio", "post2223_ratio"
)
need_sensitivity <- c(
  "base_window", "window", "start_ym", "end_ym", "direction",
  "ca_gap", "ca_ratio", "n_states"
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

if (any(summary$pre_start_ym < 201801) || any(summary$pre_end_ym > 202005)) {
  stop("Pre-fit diagnostics must be bounded by 2018m1-2020m5.", call. = FALSE)
}
if (any(!is.finite(summary$pre_rmspe)) || any(summary$pre_rmspe <= 0)) {
  stop("Pre-fit RMSPE must be finite and positive.", call. = FALSE)
}
if (any(summary$donor_count != summary$n_states - 1)) {
  stop("Summary donor count must equal n_states - 1.", call. = FALSE)
}

if (nrow(summary) != 2L || any(summary$treated_state != "CA") ||
    nrow(monthly) != 144L || any(monthly$treated_state != "CA")) {
  stop("Diagnostics should contain California for two baseline windows only.", call. = FALSE)
}
if (any(abs(monthly$cf_fam - monthly$base_treated_fam *
            monthly$donor_fam / monthly$base_donor_fam) > 1e-8) ||
    any(abs(monthly$excess_fam - (monthly$actual_fam - monthly$cf_fam)) > 1e-8)) {
  stop("Counterfactual scaling or excess-deed arithmetic failed.", call. = FALSE)
}

expected_sensitivity_windows <- c("anticipation", "rush_202011_202102", "february",
                                  "post_202103_202212", "steady_2022_2023")
if (!all(expected_sensitivity_windows %in% unique(sensitivity$window))) {
  stop("Sensitivity artifact missing expected windows: ",
       paste(setdiff(expected_sensitivity_windows, unique(sensitivity$window)),
             collapse = ", "), call. = FALSE)
}
if (nrow(sensitivity) != 10L ||
    any(!is.finite(sensitivity$ca_gap)) ||
    any(grepl("rank|_p$", names(sensitivity)))) {
  stop("Expected ten finite CA window diagnostics without rank/p columns.", call. = FALSE)
}

cat("Prop 19 counterfactual artifact invariants passed.\n")
