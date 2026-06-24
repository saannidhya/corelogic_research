#' Project setup — sourced at the top of every analysis script.
#'
#' This is the ONLY file in this project that touches paths.
#' All other scripts begin with:
#'   source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))

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
project_dir  <- here("projects", "04_algorithmic_intermediation")
scripts_dir  <- path(project_dir, "scripts")
out_dir      <- path(scripts_dir, "R", "_outputs")
manuscript_dir <- path(project_dir, "manuscript")
tables_dir   <- path(manuscript_dir, "tables")
figures_dir  <- path(manuscript_dir, "figures")

# Project-specific derived data lives here (gitignored)
data_dir     <- here("data", "derived", "04_algorithmic_intermediation")
dir_create(data_dir)
dir_create(out_dir)

# ---- project-specific packages ----
suppressPackageStartupMessages({
  library(fixest)
  library(modelsummary)
  library(data.table)
})

# ---- project-wide constants ----

#' iBuyer deed-name patterns (curated; conservative to avoid false positives).
#' Entity names validated against AZ 2019–2021 deeds (see plan
#' quality_reports/plans/2026-06-10_project04-algorithmic-intermediation.md)
#' and the institutional fact pack
#' (quality_reports/institutional_facts.md).
IBUYER_PATTERNS <- c(
  opendoor = "OPENDOOR|\\bOD (ARIZONA|NEVADA|TEXAS)\\b",
  zillow   = "ZILLOW",
  offerpad = "OFFERPAD|\\bOP SPE\\b",
  redfin   = "REDFINNOW|REDFIN NOW|RDFN VENTURES"
)

#' Institutional-buyer name proxy (buyer_1_corporate_indicator has bool-typed
#' all-NA partitions in the parquet store and cannot be read reliably;
#' see explorations/schema_audit_20260610.R).
CORP_NAME_PATTERN <- paste0(
  "\\bLLC\\b|\\bL L C\\b|\\bINC\\b|\\bCORP|\\bTRUST\\b|\\bCOMPANY\\b|",
  "\\bPARTNERS|\\bHOLDINGS\\b|\\bPROPERTIES\\b|\\bLP\\b|\\bLLP\\b|\\bLTD\\b"
)

#' Classify a vector of party names into an iBuyer firm (or NA).
#' @param nm Character vector of deed party names (already uppercase in data).
#' @return Character vector: "opendoor", "zillow", "offerpad", "redfin", or NA.
classify_ibuyer <- function(nm) {
  out <- rep(NA_character_, length(nm))
  for (firm in names(IBUYER_PATTERNS)) {
    hit <- !is.na(nm) & grepl(IBUYER_PATTERNS[[firm]], nm, ignore.case = TRUE)
    out[is.na(out) & hit] <- firm
  }
  out
}

#' Truncate a CoreLogic numeric ZIP (5- or 9-digit) to 5-digit ZIP.
#' @param z Numeric vector (e.g., 441043916 or 44104).
#' @return Integer ZIP5 (NA if invalid).
zip5_of <- function(z) {
  z <- ifelse(!is.na(z) & z > 99999L, floor(z / 10000), z)
  z <- as.integer(z)
  ifelse(!is.na(z) & z >= 501L & z <= 99950L, z, NA_integer_)
}

#' Parse CoreLogic YYYYMMDD numeric date into year and quarter columns.
#' @param d Numeric vector, e.g. 20211102.
#' @return Tibble with sale_year, sale_qtr (integer), yq (e.g. 2021.75 index).
parse_yyyymmdd_quarter <- function(d) {
  yr <- as.integer(d %/% 10000)
  mo <- as.integer((d %/% 100) %% 100)
  ok <- !is.na(d) & yr >= 1900 & yr <= 2030 & mo >= 1 & mo <= 12
  q  <- ifelse(ok, (mo - 1L) %/% 3L + 1L, NA_integer_)
  tibble(
    sale_year = ifelse(ok, yr, NA_integer_),
    sale_qtr  = q,
    yq        = ifelse(ok, yr + (q - 1L) / 4, NA_real_)
  )
}

# Market-sale filter constants (project-02/03 conventions)
PRICE_MIN <- 1e4
PRICE_MAX <- 1e7

# Sample window: OT reliable 2007–2024H1 (probe finding, project 03);
# panel runs 2010Q1–2024Q2
PANEL_START_YEAR <- 2010L
PANEL_END_YQ     <- 2024.25   # 2024Q2

# Zillow Offers operating window (verified: launch Apr 2018, shutdown
# announced 2021-11-02) — exposure measured 2018Q2–2021Q3
ZILLOW_EXPO_START <- 2018.25  # 2018Q2
ZILLOW_EXPO_END   <- 2021.50  # 2021Q3 (last full pre-announcement quarter)
EXIT_YQ           <- 2021.75  # 2021Q4: shutdown announced 2021-11-02
