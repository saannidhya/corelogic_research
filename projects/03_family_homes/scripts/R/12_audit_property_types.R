#======================================================================#
# Purpose : Full-store property-type coverage and consistency audit
# Name    : Saani Rawat
# Created : 10/02/2026
# Log     : 10/02/2026: audit candidate fields without rebuilding events.
# Inputs  : CoreLogic via shared loader; existing events.parquet
# Outputs : _outputs/tables/property_type_audit_2026-10-02/*.csv
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
audit_dir <- path(tables_out_dir, "property_type_audit_2026-10-02")
dir_create(audit_dir)
con <- open_corelogic_duckdb(memory_limit = "8GB", threads = 4L)
dbExecute(con, glue("SET temp_directory='{sql_quote_path(path(data_dir, 'duckdb_tmp'))}'"))
events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
dbExecute(con, glue("CREATE TEMP VIEW panel AS SELECT clip,state,sale_raw,sale_year,class
  FROM read_parquet('{sql_quote_path(events_path)}')"))
states <- sort(sub("state=", "", path_file(dir_ls(
  path(default_parquet_root(), "by_state", "ot"), type = "directory"))))
prop_states <- sub("state=", "", path_file(dir_ls(
  path(default_parquet_root(), "by_state", "prop"), type = "directory")))

# Normalize representation only; do not assign unverified code meanings.
code_sql <- function(field) glue("NULLIF(regexp_replace(trim(CAST({field} AS VARCHAR)), '\\.0$', ''), '')")
write_audit <- function(query, name, state) {
  result <- dbGetQuery(con, query)
  result$partition_state <- state
  write_csv_strict(result, path(audit_dir, paste0(name, "_", state, ".csv")))
  invisible(result)
}

#======================================================================#
# Reading one state at a time; only aggregates return to R ----
#======================================================================#
for (st in states) {
  # Explicit recovery option: reuse completed state aggregates from this run.
  required <- c("ot_distribution", "parcel_stability", "event_distribution")
  if (st %in% prop_states) required <- c(required,"prop_distribution","prop_keys","agreement")
  if (Sys.getenv("FAM_AUDIT_RESUME", "0") == "1" &&
      all(file_exists(path(audit_dir,paste0(required,"_",st,".csv"))))) {
    message("Reusing completed state ",st)
    next
  }
  message("AUDIT STATE ", st, " at ", Sys.time())
  register_corelogic_parquet(con, "ot", states = st, view_name = "ot_source")
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE ot_codes AS SELECT
    TRY_CAST(clip AS BIGINT) AS clip, TRY_CAST(sale_derived_date AS BIGINT) AS sale_raw,
    TRY_CAST(year AS INTEGER) AS partition_year,
    CAST(floor(TRY_CAST(sale_derived_date AS BIGINT)/10000) AS INTEGER) AS sale_year,
    residential_indicator AS residential,
    TRY_CAST(interfamily_related_indicator AS INTEGER) AS interfamily,
    {code_sql('land_use_code_static')} AS land_use,
    {code_sql('property_indicator_code_static')} AS property_indicator
    FROM ot_source"))
  raw <- write_audit("SELECT partition_year,sale_year,residential,interfamily,land_use,
    property_indicator,COUNT(*) AS n FROM ot_codes GROUP BY ALL", "ot_distribution", st)
  stopifnot(sum(raw$n) > 0)
  dbExecute(con, "CREATE OR REPLACE TEMP TABLE eligible AS SELECT * FROM ot_codes
    WHERE residential='Y' AND clip IS NOT NULL AND sale_raw BETWEEN 20070101 AND 20241231
      AND floor(sale_raw/100)%100 BETWEEN 1 AND 12")
  write_audit("WITH parcels AS (SELECT clip,COUNT(*) AS n,
    COUNT(DISTINCT land_use) AS n_land_use,COUNT(DISTINCT property_indicator) AS n_indicator
    FROM eligible GROUP BY clip) SELECT COUNT(*) AS n_parcels,
    SUM(n) AS n_source_rows,SUM(CASE WHEN n_land_use>1 THEN 1 ELSE 0 END) AS land_use_changes,
    SUM(CASE WHEN n_indicator>1 THEN 1 ELSE 0 END) AS indicator_changes
    FROM parcels", "parcel_stability", st)
  dbExecute(con, "CREATE OR REPLACE TEMP TABLE source_keys AS SELECT clip,sale_raw,
    COUNT(*) AS n_source,MIN(land_use) AS land_use,MIN(property_indicator) AS property_indicator,
    COUNT(DISTINCT land_use) AS n_land_use,COUNT(DISTINCT property_indicator) AS n_indicator,
    SUM(CASE WHEN land_use IS NULL THEN 1 ELSE 0 END) AS missing_land_use,
    SUM(CASE WHEN property_indicator IS NULL THEN 1 ELSE 0 END) AS missing_indicator
    FROM eligible GROUP BY clip,sale_raw")
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE event_codes AS SELECT p.*,
    k.n_source,k.land_use,k.property_indicator,k.n_land_use,k.n_indicator,
    k.missing_land_use,k.missing_indicator FROM panel p LEFT JOIN source_keys k
      ON p.clip=k.clip AND p.sale_raw=k.sale_raw WHERE p.state='{st}'"))
  matched <- write_audit("SELECT sale_year,class,land_use,property_indicator,
    n_land_use,n_indicator,COUNT(*) AS n,
    SUM(CASE WHEN n_source IS NULL THEN 1 ELSE 0 END) AS unmatched,
    SUM(CASE WHEN n_source>1 THEN 1 ELSE 0 END) AS duplicate_source_events,
    SUM(CASE WHEN missing_land_use>0 THEN 1 ELSE 0 END) AS any_missing_land_use,
    SUM(CASE WHEN missing_indicator>0 THEN 1 ELSE 0 END) AS any_missing_indicator
    FROM event_codes GROUP BY ALL", "event_distribution", st)
  expected <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM panel WHERE state='{st}'"))$n
  stopifnot(sum(matched$n) == expected, sum(matched$unmatched) == 0)

  if (st %in% prop_states) {
    register_corelogic_parquet(con, "prop", states = st, view_name = "prop_source")
    dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE prop_codes AS SELECT
      TRY_CAST(clip AS BIGINT) AS clip,{code_sql('land_use_code')} AS land_use,
      {code_sql('property_indicator_code')} AS property_indicator,
      TRY_CAST(total_number_of_units_all_buildings AS DOUBLE) AS units FROM prop_source"))
    write_audit("SELECT land_use,property_indicator,
      CASE WHEN units IS NULL THEN 'missing' WHEN units<=0 THEN 'nonpositive'
        WHEN units=1 THEN '1' WHEN units BETWEEN 2 AND 4 THEN '2-4'
        WHEN units>=5 THEN '5+' ELSE 'other' END AS unit_bin,COUNT(*) AS n
      FROM prop_codes GROUP BY ALL", "prop_distribution", st)
    dbExecute(con, "CREATE OR REPLACE TEMP TABLE prop_keys AS SELECT clip,
      COUNT(*) AS n_source,MIN(land_use) AS land_use,MIN(property_indicator) AS property_indicator,
      COUNT(DISTINCT land_use) AS n_land_use,COUNT(DISTINCT property_indicator) AS n_indicator
      FROM prop_codes WHERE clip IS NOT NULL GROUP BY clip")
    write_audit("SELECT COUNT(*) AS n_parcels,SUM(n_source) AS n_valid_rows,
      SUM(CASE WHEN n_source>1 THEN 1 ELSE 0 END) AS duplicate_parcels,
      SUM(CASE WHEN n_land_use>1 THEN 1 ELSE 0 END) AS conflicting_land_use,
      SUM(CASE WHEN n_indicator>1 THEN 1 ELSE 0 END) AS conflicting_indicator
      FROM prop_keys", "prop_keys", st)
    write_audit("SELECT e.sale_year,e.class,e.land_use AS ot_land_use,
      p.land_use AS prop_land_use,e.property_indicator AS ot_indicator,
      p.property_indicator AS prop_indicator,e.n_land_use AS ot_n_land_use,
      p.n_land_use AS prop_n_land_use,e.n_indicator AS ot_n_indicator,
      p.n_indicator AS prop_n_indicator,COUNT(*) AS n,
      SUM(CASE WHEN p.clip IS NULL THEN 1 ELSE 0 END) AS unmatched
      FROM event_codes e LEFT JOIN prop_keys p USING(clip) GROUP BY ALL", "agreement", st)
  }
  for (tab in c("ot_codes", "eligible", "source_keys", "event_codes", "prop_codes", "prop_keys")) {
    dbExecute(con, paste0("DROP TABLE IF EXISTS ", tab))
  }
  gc()
}

#======================================================================#
# Combining state aggregates and checking complete panel coverage ----
#======================================================================#
for (name in c("ot_distribution", "parcel_stability", "event_distribution",
               "prop_distribution", "prop_keys", "agreement")) {
  files <- dir_ls(audit_dir, glob = paste0("*/", name, "_*.csv"))
  combined <- purrr::map_dfr(files, function(file) {
    fields <- names(read_csv(file, n_max = 0, show_col_types = FALSE))
    codes <- intersect(fields, c("land_use", "property_indicator", "ot_land_use",
      "prop_land_use", "ot_indicator", "prop_indicator"))
    types <- cols(.default = col_guess())
    types$cols <- setNames(rep(list(col_character()), length(codes)), codes)
    read_csv(file, show_col_types = FALSE, col_types = types)
  })
  write_csv_strict(combined, path(audit_dir, paste0(name, ".csv")))
}
event_counts <- read_csv(path(audit_dir,"event_distribution.csv"),show_col_types=FALSE)
panel_n <- dbGetQuery(con,"SELECT COUNT(*) AS n FROM panel")$n
stopifnot(sum(event_counts$n)==panel_n)
capture.output(sessionInfo(),file=path(audit_dir,"sessionInfo.txt"))
writeLines(paste("PASS: all",panel_n,"panel events matched source keys across",length(states),
  "OT states;",length(prop_states),"Property Characteristics states scanned."),path(audit_dir,"verification.txt"))
dbDisconnect(con,shutdown=TRUE)
message("COMPLETE property-type audit")
