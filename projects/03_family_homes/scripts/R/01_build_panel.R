# ============================================================
# 01: Build national residential deed-event panel + taxonomy
# Author: Saani Rawat
# Purpose: One pass over CoreLogic OT parquet -> slim, deduped event table
#          with the frozen family/market/trust/estate taxonomy from
#          research_spec.md. All downstream scripts read ONLY the output.
# Inputs:  data/corelogic_extracts/by_state/ot/ (via duckdb glob)
# Outputs: data/derived/03_family_homes/events.parquet
#          _outputs/logs/01_build_panel.log
#          _outputs/tables/audit_class_by_year.csv
#          _outputs/tables/audit_name_population_by_year.csv
#          _outputs/tables/audit_absentee_coverage_by_year.csv
# Dev:     FAM_DEV=1 restricts to VT+RI and writes events_dev.parquet
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

dev_mode <- Sys.getenv("FAM_DEV", "0") == "1"

log_file <- path(logs_dir, ifelse(dev_mode, "01_build_panel_dev.log", "01_build_panel.log"))
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 01_build_panel at ", Sys.time(), " | dev_mode=", dev_mode)

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(path(data_dir, 'duckdb_tmp'))}'"))

if (dev_mode) {
  ot_root <- normalizePath(here("data", "corelogic_extracts", "by_state", "ot"),
                           winslash = "/", mustWork = TRUE)
  globs <- gsub("\\\\", "/",
                file.path(ot_root, c("state=VT", "state=RI"), "year=*", "*.parquet"))
  glob_sql <- paste0("['", paste(sql_quote_path(globs), collapse = "', '"), "']")
  out_parquet <- path(data_dir, "events_dev.parquet")
} else {
  glob_sql <- paste0("'", sql_quote_path(ot_parquet_glob()), "'")
  out_parquet <- path(data_dir, "events.parquet")
}

# ---- 1. Raw view: slim columns, typed ----------------------------------
# Taxonomy (frozen in research_spec.md):
#   family_trust   interfam & trust keyword (estate-planning self-transfer)
#   family_person  interfam & same surname (true inter-person family transfer)
#   family_estate  interfam & estate/executor keywords
#   family_other   remaining interfam (incl. cross-surname relatives)
#   market_sale    pcat 'A', price >= $10k, not interfam
#   estate_noninterfam  estate keywords without interfam flag
#   other_nonarms  everything else (nominal-price A, B/C admin, etc.)
dbExecute(con, glue("
  CREATE OR REPLACE TEMP VIEW ot_slim AS
  SELECT
    TRY_CAST(clip AS BIGINT)                                   AS clip,
    state,
    -- fips_code is DOUBLE in some partitions, VARCHAR in others; the DOUBLE
    -- path renders as '44001.0' and drops leading zeros (MEMORY.md join bug)
    lpad(regexp_replace(CAST(fips_code AS VARCHAR), '\\.0$', ''), 5, '0') AS fips5,
    TRY_CAST(sale_derived_date AS BIGINT)                      AS sale_raw,
    CAST(floor(TRY_CAST(sale_derived_date AS BIGINT) / 10000) AS INTEGER)      AS sale_year,
    CAST(floor(TRY_CAST(sale_derived_date AS BIGINT) / 100) % 100 AS INTEGER)  AS sale_month,
    primary_category_code                                      AS pcat,
    deed_category_type_code                                    AS dtype,
    regexp_replace(CAST(land_use_code_static AS VARCHAR), '\\.0$', '') AS land_use_code_static,
    regexp_replace(CAST(property_indicator_code_static AS VARCHAR), '\\.0$', '') AS property_indicator_code_static,
    TRY_CAST(interfamily_related_indicator AS INTEGER)         AS interfam,
    TRY_CAST(sale_amount AS DOUBLE)                            AS amt,
    upper(coalesce(buyer_1_last_name, ''))                     AS b1_last,
    upper(coalesce(seller_1_last_name, ''))                    AS s1_last,
    upper(coalesce(buyer_1_full_name, ''))                     AS b1_full,
    upper(coalesce(seller_1_full_name, ''))                    AS s1_full,
    upper(coalesce(buyer_2_full_name, ''))                     AS b2_full,
    CASE WHEN buyer_1_corporate_indicator = 'Y' THEN 1 ELSE 0 END AS corp_buyer,
    TRY_CAST(new_construction_indicator AS INTEGER)            AS new_construction,
    TRY_CAST(foreclosure_reo_indicator AS INTEGER)             AS reo,
    TRY_CAST(foreclosure_reo_sale_indicator AS INTEGER)        AS reo_sale,
    -- ZIPs: strip numeric-cast '.0', then restore leading zeros for 5- and
    -- 9-digit forms before taking the 5-digit prefix (r-review M3)
    CASE
      WHEN length(regexp_replace(coalesce(CAST(buyer_mailing_zip_code AS VARCHAR), ''), '\\.0$', '')) BETWEEN 3 AND 5
        THEN lpad(regexp_replace(CAST(buyer_mailing_zip_code AS VARCHAR), '\\.0$', ''), 5, '0')
      WHEN length(regexp_replace(coalesce(CAST(buyer_mailing_zip_code AS VARCHAR), ''), '\\.0$', '')) BETWEEN 8 AND 9
        THEN substr(lpad(regexp_replace(CAST(buyer_mailing_zip_code AS VARCHAR), '\\.0$', ''), 9, '0'), 1, 5)
      ELSE ''
    END AS mail_zip5,
    CASE
      WHEN length(regexp_replace(coalesce(CAST(deed_situs_zip_code_static AS VARCHAR), ''), '\\.0$', '')) BETWEEN 3 AND 5
        THEN lpad(regexp_replace(CAST(deed_situs_zip_code_static AS VARCHAR), '\\.0$', ''), 5, '0')
      WHEN length(regexp_replace(coalesce(CAST(deed_situs_zip_code_static AS VARCHAR), ''), '\\.0$', '')) BETWEEN 8 AND 9
        THEN substr(lpad(regexp_replace(CAST(deed_situs_zip_code_static AS VARCHAR), '\\.0$', ''), 9, '0'), 1, 5)
      ELSE ''
    END AS situs_zip5,
    coalesce(CAST(buyer_mailing_state AS VARCHAR), '')         AS mail_state
  FROM read_parquet({glob_sql}, union_by_name = true)
  WHERE residential_indicator = 'Y'
    AND TRY_CAST(clip AS BIGINT) IS NOT NULL
    AND TRY_CAST(sale_derived_date AS BIGINT) BETWEEN 20070101 AND 20241231
    AND CAST(floor(TRY_CAST(sale_derived_date AS BIGINT) / 100) % 100 AS INTEGER) BETWEEN 1 AND 12
"))

# ---- 2. Classify + dedupe ----------------------------------------------
# Dedupe: one event per (clip, sale_raw). Multi-parcel sales and re-recorded
# corrections produce duplicate rows; keep the most informative record
# (arm's-length category first, then highest stated price).
dbExecute(con, "
  CREATE OR REPLACE TEMP VIEW ot_classed AS
  WITH flagged AS (
    SELECT *,
      (b1_full LIKE '%TRUST%' OR s1_full LIKE '%TRUST%')        AS trust_kw,
      -- ADMINISTRATOR/-TRIX (not ADMINISTRAT%) to avoid matching agency
      -- REO sellers like VETERANS ADMINISTRATION (r-review minor 6)
      (b1_full LIKE '%ESTATE OF%' OR s1_full LIKE '%ESTATE OF%'
        OR b1_full LIKE '%EXECUT%'  OR s1_full LIKE '%EXECUT%'
        OR b1_full LIKE '%PROBATE%' OR s1_full LIKE '%PROBATE%'
        OR b1_full LIKE '%ADMINISTRATOR%' OR s1_full LIKE '%ADMINISTRATOR%'
        OR b1_full LIKE '%ADMINISTRATRIX%' OR s1_full LIKE '%ADMINISTRATRIX%') AS estate_kw,
      (b1_last <> '' AND b1_last = s1_last)                     AS same_surname,
      -- co-owner add/remove and refi retitling: the named principal is the
      -- same person on both sides of the deed (referee C2)
      ((b1_full <> '' AND b1_full = s1_full)
        OR (b2_full <> '' AND b2_full = s1_full))               AS same_person
    FROM ot_slim
  )
  SELECT *,
    CASE
      WHEN interfam = 1 AND trust_kw                 THEN 'family_trust'
      WHEN interfam = 1 AND same_person              THEN 'family_retitle'
      WHEN interfam = 1 AND same_surname             THEN 'family_person'
      WHEN interfam = 1 AND estate_kw                THEN 'family_estate'
      WHEN interfam = 1                              THEN 'family_other'
      WHEN pcat = 'A' AND amt >= 10000               THEN 'market_sale'
      WHEN estate_kw                                 THEN 'estate_noninterfam'
      ELSE 'other_nonarms'
    END AS class,
    CASE WHEN mail_zip5 <> '' AND situs_zip5 <> ''
         THEN CASE WHEN mail_zip5 <> situs_zip5 THEN 1 ELSE 0 END
         ELSE NULL END AS absentee_buyer
  FROM flagged
")

dbExecute(con, glue("
  COPY (
    SELECT clip, state, fips5, sale_raw, sale_year, sale_month,
           sale_year * 100 + sale_month AS ym,
           land_use_code_static, property_indicator_code_static,
           pcat, dtype, interfam, amt, class,
           same_surname, same_person, trust_kw, estate_kw, corp_buyer,
           new_construction, reo, reo_sale,
           absentee_buyer, mail_state, mail_zip5, situs_zip5
    FROM (
      -- Tiebreakers beyond (pcat, amt) make the ordering total so tied
      -- same-day records resolve identically across runs/thread counts
      -- (r-review C2: bit-reproducibility of the dedupe)
      SELECT *, ROW_NUMBER() OVER (
        PARTITION BY clip, sale_raw
        ORDER BY CASE WHEN pcat = 'A' THEN 0 ELSE 1 END,
                 amt DESC NULLS LAST,
                 class ASC, dtype ASC NULLS LAST,
                 b1_full ASC, s1_full ASC
      ) AS rn
      FROM ot_classed
    )
    WHERE rn = 1
  ) TO '{sql_quote_path(out_parquet)}' (FORMAT PARQUET, COMPRESSION ZSTD)
"))
message("Wrote ", out_parquet)

# ---- 3. Audits (read back from the written parquet) ---------------------
ev <- glue("read_parquet('{sql_quote_path(out_parquet)}')")

n_total <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM {ev}"))$n
message("Events written: ", format(n_total, big.mark = ","))
stopifnot(n_total > ifelse(dev_mode, 1e5, 1e8))  # degeneracy guard

# Schema-drift guards (r-review M2): TRY_CAST silently NULLing the interfam
# flag in any partition would re-route family events to market/other classes.
guards <- dbGetQuery(con, glue("
  SELECT
    SUM(CASE WHEN interfam IS NULL THEN 1 ELSE 0 END) AS n_interfam_null,
    SUM(CASE WHEN sale_month NOT BETWEEN 1 AND 12 THEN 1 ELSE 0 END) AS n_bad_month,
    COUNT(DISTINCT state) AS n_states,
    AVG(CASE WHEN interfam = 1 THEN 1.0 ELSE 0 END) AS interfam_share
  FROM {ev}
"))
print(guards)
stopifnot(
  guards$n_interfam_null == 0,
  guards$n_bad_month == 0,
  guards$n_states >= ifelse(dev_mode, 2, 51),
  guards$interfam_share > 0.10, guards$interfam_share < 0.60
)

audit_class <- dbGetQuery(con, glue("
  SELECT sale_year, class, COUNT(*) AS n
  FROM {ev} GROUP BY sale_year, class ORDER BY sale_year, class
"))
assert_rows(audit_class, 50, "audit_class_by_year")
write_csv_strict(audit_class,
                 path(tables_out_dir, ifelse(dev_mode,
                                             "audit_class_by_year_dev.csv",
                                             "audit_class_by_year.csv")))

audit_names <- dbGetQuery(con, glue("
  SELECT sale_year,
         AVG(CASE WHEN interfam = 1 THEN 1.0 ELSE 0 END)        AS interfam_share,
         AVG(CASE WHEN same_surname THEN 1.0 ELSE 0 END)        AS same_surname_share,
         AVG(CASE WHEN trust_kw THEN 1.0 ELSE 0 END)            AS trust_share,
         AVG(CASE WHEN estate_kw THEN 1.0 ELSE 0 END)           AS estate_share
  FROM {ev} GROUP BY sale_year ORDER BY sale_year
"))
write_csv_strict(audit_names,
                 path(tables_out_dir, ifelse(dev_mode,
                                             "audit_name_population_by_year_dev.csv",
                                             "audit_name_population_by_year.csv")))

audit_absentee <- dbGetQuery(con, glue("
  SELECT sale_year,
         AVG(CASE WHEN absentee_buyer IS NOT NULL THEN 1.0 ELSE 0 END) AS absentee_observed,
         AVG(CASE WHEN absentee_buyer = 1 THEN 1.0 ELSE 0 END)         AS absentee_share_all
  FROM {ev} GROUP BY sale_year ORDER BY sale_year
"))
write_csv_strict(audit_absentee,
                 path(tables_out_dir, ifelse(dev_mode,
                                             "audit_absentee_coverage_by_year_dev.csv",
                                             "audit_absentee_coverage_by_year.csv")))

message("Class distribution (full window):")
print(
  audit_class |>
    group_by(class) |>
    summarise(n = sum(n), .groups = "drop") |>
    arrange(desc(n)),
  n = 20
)

message("Finished 01_build_panel at ", Sys.time())
