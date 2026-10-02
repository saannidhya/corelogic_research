#' Project setup — sourced at the top of every analysis script.
#'
#' This is the ONLY file in this project that touches paths.
#' All other scripts begin with:
#'   source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(arrow)
  library(duckdb)
  library(DBI)
  library(fs)
  library(glue)
  library(scales)
})

# ---- shared utilities ----
source(here("shared_utils", "R", "corelogic_loader.R"))
source(here("shared_utils", "R", "data_dictionary.R"))
source(here("shared_utils", "R", "filters.R"))
source(here("shared_utils", "R", "theme_paper.R"))
theme_set(theme_paper())

# ---- project-scoped paths ----
project_dir    <- here("projects", "03_family_homes")
scripts_dir    <- path(project_dir, "scripts")
r_dir          <- path(scripts_dir, "R")
source(path(r_dir, "00_date_helpers.R"))
out_dir        <- path(r_dir, "_outputs")
tables_out_dir <- path(out_dir, "tables")
figures_out_dir <- path(out_dir, "figures")
logs_dir       <- path(out_dir, "logs")
manuscript_dir <- path(project_dir, "manuscript")
tables_dir     <- path(manuscript_dir, "tables")
figures_dir    <- path(manuscript_dir, "figures")

# Project-specific derived data lives here (gitignored)
data_dir <- here("data", "derived", "03_family_homes")

dir_create(data_dir)
dir_create(out_dir)
dir_create(tables_out_dir)
dir_create(figures_out_dir)
dir_create(logs_dir)
dir_create(tables_dir)
dir_create(figures_dir)

# ---- duckdb-over-parquet helpers (pattern established in project 02) ----
ot_parquet_glob <- function() {
  root <- normalizePath(here("data", "corelogic_extracts", "by_state", "ot"),
                        winslash = "/", mustWork = TRUE)
  gsub("\\\\", "/", file.path(root, "state=*", "year=*", "*.parquet"))
}

prop_parquet_glob <- function() {
  root <- normalizePath(here("data", "corelogic_extracts", "by_state", "prop"),
                        winslash = "/", mustWork = TRUE)
  gsub("\\\\", "/", file.path(root, "state=*", "*.parquet"))
}

sql_quote_path <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

open_corelogic_duckdb <- function(memory_limit = "12GB",
                                  threads = max(1L, parallel::detectCores() - 1L)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dbExecute(con, glue("PRAGMA memory_limit='{memory_limit}'"))
  dbExecute(con, glue("PRAGMA threads={threads}"))
  con
}

#' Assert a query result is non-degenerate before it is used downstream.
#' (MEMORY.md [LEARN:data]: a script that exits 0 is not proof the result is
#' real — guard against silent zero-row joins / empty subsamples.)
assert_rows <- function(df, min_rows, label) {
  if (!is.data.frame(df) || nrow(df) < min_rows) {
    stop("Degenerate result for ", label, ": ",
         ifelse(is.data.frame(df), nrow(df), NA), " rows (need >= ", min_rows, ")")
  }
  invisible(df)
}

write_csv_strict <- function(x, file) {
  dir_create(path_dir(file))
  readr::write_csv(x, file, na = "")
  message("Wrote: ", file)
}

# ---- analysis window constants ----
window_start_year <- 2007L   # OT density begins 2007 (probe 2026-06-09)
window_end_year   <- 2024L   # data truncate ~Aug 2024; treat 2024 as partial
full_year_end     <- 2023L   # last complete calendar year
prop19_pass_date      <- 20201103L  # Prop 19 passed (election day)
prop19_pc_effective   <- 20210216L  # parent-child exclusion changes effective

