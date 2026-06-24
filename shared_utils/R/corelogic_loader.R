#' CoreLogic Loader (R)
#'
#' Three public functions:
#'   load_corelogic_ot()       — Owner Transfer (transactions)
#'   load_corelogic_prop()     — Property Characteristics
#'   load_corelogic_baseline_oh() — Prior Ohio cleaned + geocoded files
#'
#' Reads from the partitioned parquet store at data/corelogic_extracts/by_state/.
#' Falls back to raw CSV streaming with predicate pushdown if parquet doesn't
#' exist for the requested slice.
#'
#' See .claude/rules/corelogic-data-protocol.md for the full contract.

suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(here)
  library(fs)
})

#' Default parquet root (overridable for tests)
default_parquet_root <- function() {
  here("data", "corelogic_extracts")
}

#' Default sample paths
default_sample_path <- function(dataset) {
  switch(dataset,
    "ot"   = here("data", "corelogic_extracts", "ot_sample_10k.parquet"),
    "prop" = here("data", "corelogic_extracts", "prop_sample_10k.parquet"),
    stop("Unknown dataset: ", dataset)
  )
}

#' Default raw CSV directory
default_raw_root <- function() {
  "C:/CoreLogic/housing"
}

#' Load CoreLogic Owner Transfer (transactions) data
#'
#' @param states Character vector of state codes (e.g., c("OH", "CA")). NULL = all.
#' @param years Integer vector of years. NULL = all.
#' @param columns Character vector of column names. NULL = all.
#' @param sample If TRUE, read the 10K-row sample instead of full data.
#' @param source One of "parquet" (default) or "raw".
#' @param parquet_root (Internal/testing) override parquet root.
#' @return Tibble.
load_corelogic_ot <- function(states = NULL, years = NULL, columns = NULL,
                              sample = FALSE,
                              source = c("parquet", "raw"),
                              parquet_root = default_parquet_root()) {
  source <- match.arg(source)

  if (sample) {
    return(read_sample("ot", columns))
  }

  ot_path <- path(parquet_root, "by_state", "ot")

  if (source == "parquet" && dir_exists(ot_path)) {
    ds <- open_dataset(ot_path, partitioning = c("state", "year"))
    if (!is.null(states))  ds <- ds |> filter(state %in% states)
    if (!is.null(years))   ds <- ds |> filter(year  %in% years)
    if (!is.null(columns)) ds <- ds |> select(any_of(unique(c(columns, "state", "year"))))
    return(as_tibble(collect(ds)))
  }

  # Fallback: raw streaming (predicate pushdown via arrow CSV reader)
  load_corelogic_ot_raw(states, years, columns)
}

#' Load CoreLogic Property Characteristics data
#'
#' @inheritParams load_corelogic_ot
#' @param per_partition If TRUE, read each requested state's partition
#'   directory directly with its own (native) schema instead of the unified
#'   dataset schema. Use when cross-partition type drift breaks the unified
#'   scan (e.g., a column stored as string in one state and double in
#'   another, with values the unified type cannot parse). Column types in
#'   the result then reflect each partition's native types; callers must
#'   harmonize. Requires `states` to be non-NULL.
load_corelogic_prop <- function(states = NULL, columns = NULL,
                                sample = FALSE,
                                source = c("parquet", "raw"),
                                parquet_root = default_parquet_root(),
                                per_partition = FALSE) {
  source <- match.arg(source)

  if (sample) {
    return(read_sample("prop", columns))
  }

  prop_path <- path(parquet_root, "by_state", "prop")

  if (source == "parquet" && dir_exists(prop_path)) {
    if (per_partition) {
      stopifnot(!is.null(states))
      out <- lapply(states, function(st) {
        st_path <- path(prop_path, paste0("state=", st))
        if (!dir_exists(st_path)) return(NULL)
        ds <- open_dataset(st_path)
        if (!is.null(columns)) ds <- ds |> select(any_of(unique(columns)))
        d <- as_tibble(collect(ds))
        d$state <- st
        d
      })
      return(dplyr::bind_rows(out))
    }
    ds <- open_dataset(prop_path, partitioning = "state")
    if (!is.null(states))  ds <- ds |> filter(state %in% states)
    if (!is.null(columns)) ds <- ds |> select(any_of(unique(c(columns, "state"))))
    return(as_tibble(collect(ds)))
  }

  load_corelogic_prop_raw(states, columns)
}

#' Load prior Ohio cleaned + geocoded baseline files
#'
#' @param dataset One of "ot" or "prop"
#' @param variant Variant tag matching a file at
#'   data/corelogic_baseline/<dataset>_oh_<variant>.parquet
#'   (e.g., "2021_2024_cleaned", "geocoded_2007_2024",
#'   "geocoded", "geocoded_with_cousub_place")
#' @return Tibble.
load_corelogic_baseline_oh <- function(dataset = c("ot", "prop"), variant = NULL) {
  dataset <- match.arg(dataset)
  baseline_dir <- here("data", "corelogic_baseline")

  if (is.null(variant)) {
    # List available variants for this dataset
    available <- dir_ls(baseline_dir, glob = paste0("*", dataset, "_oh_*.parquet")) |>
      path_file() |> path_ext_remove()
    stop("variant argument required. Available variants for '", dataset, "':\n  ",
         paste(available, collapse = "\n  "))
  }

  fpath <- path(baseline_dir, paste0(dataset, "_oh_", variant, ".parquet"))
  if (!file_exists(fpath)) {
    stop("Baseline file not found: ", fpath)
  }
  as_tibble(read_parquet(fpath))
}

# ---- internal helpers ----

read_sample <- function(dataset, columns) {
  fpath <- default_sample_path(dataset)
  if (!file_exists(fpath)) {
    stop("Sample file not found: ", fpath,
         "\nGenerate via Phase 6 of the workflow plan.")
  }
  df <- as_tibble(read_parquet(fpath))
  if (!is.null(columns)) df <- df |> select(any_of(columns))
  df
}

load_corelogic_ot_raw <- function(states, years, columns) {
  stop("Raw streaming fallback not yet implemented for OT. ",
       "Run convert_raw_to_parquet.R first (Phase 6).")
}

load_corelogic_prop_raw <- function(states, columns) {
  stop("Raw streaming fallback not yet implemented for Prop. ",
       "Run convert_raw_to_parquet.R first (Phase 6).")
}
