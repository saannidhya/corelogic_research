#!/usr/bin/env Rscript
#' Build the sale_document_type_code lookup with counts and proportions
#'
#' Joins the curated meanings in shared_utils/R/reference/
#' sale_document_type_meanings.csv to per-code counts and profile
#' proportions computed over the full Owner Transfer parquet store.
#'
#' Output (gitignored, because it holds aggregates of licensed data):
#'   data/corelogic_extracts/_reference/sale_document_type_codes.csv
#'
#' Re-run after each extract refresh. Fails if the store contains a code
#' with no curated meaning, so new codes cannot slip through unlabeled.
#'
#' Usage: Rscript shared_utils/R/build_sale_document_type_codes.R

suppressPackageStartupMessages({
  library(here)
  library(DBI)
  library(duckdb)
  library(dplyr)
  library(readr)
  library(glue)
  library(fs)
})

source(here("shared_utils", "R", "corelogic_loader.R"))
source(here("shared_utils", "R", "data_dictionary.R"))

window_years <- 2007L:2024L
missing_code <- "(missing)"

# DuckDB rather than load_corelogic_ot(): the loader collects into memory,
# and this needs a grouped scan over all ~160M rows.
ot_glob <- gsub("\\\\", "/", path(
  normalizePath(path(default_parquet_root(), "by_state", "ot"), winslash = "/"),
  "state=*", "year=*", "*.parquet"
))

con <- dbConnect(duckdb(), dbdir = ":memory:")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
invisible(dbExecute(con, "PRAGMA memory_limit='12GB'"))

by_code_state <- dbGetQuery(con, glue("
  SELECT coalesce(sale_document_type_code, '{missing_code}') AS code,
         state,
         count(*) AS n,
         sum(CASE WHEN year BETWEEN {min(window_years)} AND {max(window_years)}
                  THEN 1 ELSE 0 END) AS n_window,
         sum(CASE WHEN coalesce(sale_amount, 0) = 0 THEN 1 ELSE 0 END) AS n_zero_price,
         sum(CASE WHEN interfamily_related_indicator = 1 THEN 1 ELSE 0 END) AS n_interfamily,
         sum(CASE WHEN foreclosure_reo_indicator = 1 THEN 1 ELSE 0 END) AS n_reo,
         sum(CASE WHEN primary_category_code = 'A' THEN 1 ELSE 0 END) AS n_arms_length,
         min(year) AS first_year,
         max(year) AS last_year
  FROM read_parquet('{ot_glob}', hive_partitioning = true)
  GROUP BY ALL
"))
stopifnot(sum(by_code_state$n) > 1e8)  # guard against a partial/empty scan

top_states <- by_code_state |>
  group_by(code) |>
  mutate(state_share = n / sum(n)) |>
  slice_max(n, n = 3, with_ties = FALSE) |>
  summarise(top_states = paste0(state, " ", round(100 * state_share), "%",
                                collapse = "; "),
            .groups = "drop")

counts <- by_code_state |>
  group_by(code) |>
  summarise(across(c(n, n_window, n_zero_price, n_interfamily, n_reo, n_arms_length), sum),
            first_year = min(first_year), last_year = max(last_year),
            .groups = "drop") |>
  mutate(
    share_all       = n / sum(n),
    share_2007_2024 = n_window / sum(n_window),
    p_zero_price    = n_zero_price / n,
    p_interfamily   = n_interfamily / n,
    p_reo           = n_reo / n,
    p_arms_length   = n_arms_length / n
  ) |>
  arrange(desc(n)) |>
  mutate(cum_share_all = cumsum(share_all)) |>
  left_join(top_states, by = "code")

meanings <- read_sale_document_type_meanings()
unlabeled <- setdiff(counts$code, meanings$code)
if (length(unlabeled) > 0) {
  stop("Codes in the store with no curated meaning: ", paste(unlabeled, collapse = ", "),
       "\nAdd them to shared_utils/R/reference/sale_document_type_meanings.csv")
}

out <- counts |>
  left_join(meanings, by = "code") |>
  transmute(
    code, meaning, group, confidence,
    n_deeds = n, share_all, cum_share_all,
    n_deeds_2007_2024 = n_window, share_2007_2024,
    p_zero_price, p_interfamily, p_reo, p_arms_length,
    first_year, last_year, top_states, evidence
  )

out_path <- sale_document_type_codes_path()
dir_create(path_dir(out_path))
write_csv(out, out_path, na = "")
message("Wrote ", nrow(out), " codes (", format(sum(out$n_deeds), big.mark = ","),
        " deed records) to ", out_path)
