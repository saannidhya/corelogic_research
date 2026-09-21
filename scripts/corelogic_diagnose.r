#================================================================================================================#
# Purpose : Check the years represented in the Ohio CoreLogic Property Characteristics extract
# Name    : Saani Rawat
# Date    : 2026-07-21
# Input   : data/corelogic_extracts/by_state/prop/state=OH/part.parquet
# Output  : Console summary of tax-year and assessed-year availability
# Note    : Property Characteristics is a current snapshot rather than a dataset partitioned by year.
#================================================================================================================#


#================================================================================================================#
# 1. Load packages and shared CoreLogic loader
#================================================================================================================#

suppressPackageStartupMessages({
  library(dplyr)
  library(here)
})

source(here("shared_utils", "R", "corelogic_loader.R"))


#================================================================================================================#
# 2. Open the Ohio Property Characteristics extract
#================================================================================================================#

parquet_root <- here("data", "corelogic_extracts")
oh_prop_path <- here(
  "data", "corelogic_extracts", "by_state", "prop", "state=OH"
)

if (!fs::dir_exists(oh_prop_path)) {
  stop("Ohio Property Characteristics directory not found: ", oh_prop_path)
}

# Read only the two fields that describe the assessment/tax vintage. This keeps
# the diagnostic lightweight while still using the project's required loader.
oh_prop_years <- load_corelogic_prop(
  states = "OH",
#   columns = c("tax_year", "assessed_year"),
  parquet_root = parquet_root,
  per_partition = TRUE
)


#================================================================================================================#
# 3. Summarize available years
#================================================================================================================#

property_years_long <- bind_rows(
  transmute(oh_prop_years, year_field = "tax_year", year = tax_year),
  transmute(oh_prop_years, year_field = "assessed_year", year = assessed_year)
) |>
  mutate(
    is_valid_year = !is.na(year) &
      year == floor(year) &
      between(year, 1900, as.integer(format(Sys.Date(), "%Y")) + 1L)
  )

year_summary <- property_years_long |>
  group_by(year_field) |>
  summarise(
    total_properties = n(),
    properties_with_year = sum(!is.na(year)),
    properties_missing_year = sum(is.na(year)),
    properties_with_invalid_year = sum(!is.na(year) & !is_valid_year),
    distinct_valid_years = n_distinct(year[is_valid_year]),
    earliest_valid_year = if (any(is_valid_year)) min(year[is_valid_year]) else NA_real_,
    latest_valid_year = if (any(is_valid_year)) max(year[is_valid_year]) else NA_real_,
    .groups = "drop"
  )

year_availability <- property_years_long |>
  filter(is_valid_year) |>
  count(year_field, year, name = "number_of_properties") |>
  arrange(year_field, year)

invalid_year_values <- property_years_long |>
  filter(!is.na(year), !is_valid_year) |>
  count(year_field, year, name = "number_of_properties") |>
  arrange(year_field, year)

cat("Ohio Property Characteristics path:\n", oh_prop_path, "\n\n", sep = "")
cat("Rows loaded:", format(nrow(oh_prop_years), big.mark = ","), "\n\n")
cat("Year-field summary:\n")
print(year_summary, n = Inf, width = Inf)
cat("\nProperties by available year:\n")
print(year_availability, n = Inf, width = Inf)

if (nrow(invalid_year_values) > 0L) {
  cat("\nInvalid year values (excluded from the availability range):\n")
  print(invalid_year_values, n = Inf, width = Inf)
}

# Optional interactive inspection:
# View(year_availability)
