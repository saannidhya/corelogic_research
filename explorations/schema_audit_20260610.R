# Schema audit: per-partition parquet types for columns needed by project 04.
# Read-only metadata scan; identifies columns with inconsistent types across
# state/year partitions (CSV-inference artifact from conversion).
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(purrr)
})

need <- c(
  "clip", "fips_code", "sale_amount", "sale_derived_date",
  "deed_situs_zip_code_static", "primary_category_code",
  "residential_indicator", "property_indicator_code_static",
  "interfamily_related_indicator", "foreclosure_reo_indicator",
  "cash_purchase_indicator", "investor_purchase_indicator",
  "new_construction_indicator", "resale_indicator",
  "buyer_1_full_name", "buyer_2_full_name",
  "seller_1_full_name", "seller_2_full_name",
  "buyer_1_corporate_indicator"
)

files <- list.files("data/corelogic_extracts/by_state/ot",
                    pattern = "\\.parquet$", recursive = TRUE,
                    full.names = TRUE)
cat("files:", length(files), "\n")

types <- map(files, function(f) {
  s <- ParquetFileReader$create(f)$GetSchema()
  setNames(
    vapply(need, function(cn) {
      fld <- s$GetFieldByName(cn)
      if (is.null(fld)) "MISSING" else fld$type$ToString()
    }, character(1)),
    need
  )
})

tt <- do.call(rbind, types) |> as.data.frame()
tt$file <- files

for (cn in need) {
  tb <- table(tt[[cn]])
  cat(sprintf("%-35s %s\n", cn,
              paste(names(tb), tb, sep = ":", collapse = "  ")))
}

# List offending partitions for name columns
bad <- tt |> filter(buyer_1_full_name != "string" | seller_1_full_name != "string")
cat("\npartitions where buyer_1/seller_1 not string:", nrow(bad), "\n")
if (nrow(bad) > 0) print(head(bad$file, 20))

# ---- PROP store audit ----
need_p <- c(
  "clip", "situs_core_based_statistical_area_cbsa",
  "universal_building_square_feet",
  "total_number_of_bedrooms_all_buildings",
  "total_number_of_bathrooms_all_buildings",
  "year_built", "property_indicator_code", "situs_zip_code"
)
files_p <- list.files("data/corelogic_extracts/by_state/prop",
                      pattern = "\\.parquet$", recursive = TRUE,
                      full.names = TRUE)
cat("\nprop files:", length(files_p), "\n")
types_p <- map(files_p, function(f) {
  s <- ParquetFileReader$create(f)$GetSchema()
  setNames(
    vapply(need_p, function(cn) {
      fld <- s$GetFieldByName(cn)
      if (is.null(fld)) "MISSING" else fld$type$ToString()
    }, character(1)),
    need_p
  )
})
ttp <- do.call(rbind, types_p) |> as.data.frame()
for (cn in need_p) {
  tb <- table(ttp[[cn]])
  cat(sprintf("%-40s %s\n", cn,
              paste(names(tb), tb, sep = ":", collapse = "  ")))
}
