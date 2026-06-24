# Diagnose AZ/IN/MD prop failures (read-only)
suppressPackageStartupMessages({library(arrow); library(dplyr)})

need <- c("clip", "situs_core_based_statistical_area_cbsa",
          "universal_building_square_feet",
          "total_number_of_bedrooms_all_buildings",
          "total_number_of_bathrooms_all_buildings",
          "year_built", "property_indicator_code", "situs_zip_code")

for (st in c("AZ", "IN", "MD")) {
  f <- sprintf("data/corelogic_extracts/by_state/prop/state=%s/part-0.parquet", st)
  if (!file.exists(f)) {
    fs <- list.files(dirname(f), pattern = "\\.parquet$", full.names = TRUE)
    f <- fs[1]
  }
  cat("\n=====", st, "=====", basename(f), "\n")
  s <- ParquetFileReader$create(f)$GetSchema()
  for (cn in need) {
    fld <- s$GetFieldByName(cn)
    cat(sprintf("  %-42s %s\n", cn,
                if (is.null(fld)) "MISSING" else fld$type$ToString()))
  }
  # read the file directly (own schema, no unification) and count non-NA
  t <- read_parquet(f, col_select = any_of(need))
  cat("  rows:", nrow(t), "\n")
  for (cn in intersect(need, names(t))) {
    v <- t[[cn]]
    cat(sprintf("  non-NA %-35s %d\n", cn, sum(!is.na(v))))
  }
}
