#' Geocoding utilities (placeholder)
#'
#' Prior CoreLogic geocoding was done in Stata before this repo existed.
#' The outputs (geocoded Ohio property characteristics) are wrapped as
#' baseline inputs at data/corelogic_baseline/.
#'
#' This file is a placeholder. When a project needs NEW geocoding, fill in:
#'   - reverse-geocode an APN to lat/lon (via property characteristics)
#'   - forward-geocode an address (via Census, Nominatim, or Google API)
#'   - spatial join to county subdivision / place / tract
#'
#' See:
#'   - data/corelogic_baseline/PROVENANCE.md — what was done before
#'   - shared_utils/R/corelogic_loader.R for the read-only CoreLogic loader
#'   - shared_utils/python/ — consider Python (geopandas) for spatial joins

# Placeholder: list known baseline geocoded variants
list_baseline_geocoded_variants <- function() {
  c(
    "ot_oh_geocoded_2007_2024",
    "ot_oh_geocoded_2016_2020",
    "ot_oh_geocoded_2021_2024",
    "prop_full_geocoded",
    "prop_oh_geocoded",
    "prop_oh_geocoded_with_cousub",
    "prop_oh_geocoded_with_cousub_place"
  )
}
