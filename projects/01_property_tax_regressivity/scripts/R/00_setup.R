#' Project setup — sourced at the top of every analysis script.
#'
#' This is the ONLY file in this project that touches paths.
#' All other scripts begin with:
#'   source(here::here("projects/01_property_tax_regressivity/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(arrow)
  library(fs)
})

# ---- shared utilities ----
source(here("shared_utils", "R", "corelogic_loader.R"))
source(here("shared_utils", "R", "data_dictionary.R"))
source(here("shared_utils", "R", "filters.R"))
source(here("shared_utils", "R", "theme_paper.R"))
theme_set(theme_paper())

# ---- project-scoped paths ----
project_dir  <- here("projects/01_property_tax_regressivity")
scripts_dir  <- path(project_dir, "scripts")
out_dir      <- path(scripts_dir, "R", "_outputs")
manuscript_dir <- path(project_dir, "manuscript")
tables_dir   <- path(manuscript_dir, "tables")
figures_dir  <- path(manuscript_dir, "figures")

# Project-specific derived data lives here (gitignored)
data_dir     <- here("data", "derived", "01_property_tax_regressivity")
external_data_dir <- here("data", "external", "01_property_tax_regressivity")
dir_create(data_dir)
dir_create(external_data_dir)
dir_create(out_dir)

# ---- project-specific packages ----
suppressPackageStartupMessages({
  library(fixest)        # within-jurisdiction fixed effects regression (Berry replication)
  library(modelsummary)  # publication-ready tables
  library(duckdb)        # query parquet store with SQL pushdown
  library(glue)          # string interpolation in logs
})

# ---- project-specific data sub-paths ----
panel_path <- path(data_dir, "national_panel_2007_2010.parquet")  # phase 1 cleaned panel
report_dir <- path(project_dir, "quality_reports", "specs")
dir_create(report_dir)

# ---- helpers ----
log_msg <- function(...) cat(format(Sys.time(), "[%H:%M:%S] "), ..., "\n", sep = "")
