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
#          _outputs/tables/audit_trust_rel_by_year.csv
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
#   market_sale    pcat 'A', not interfam (no price floor: non-disclosure states
#                  report missing/0 prices on genuine sales; discussant, ODU 2026)
#   estate_noninterfam  estate keywords without interfam flag
#   other_nonarms  everything else (B/C admin, etc.)
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

# ---- 1b. Trust-mediated family transfer rule (research_spec.md, 2026-10-31) ----
# Applies only to family_trust events (interfam = 1 & trust keyword). Output
# trust_rel in {self, clear, ambiguous, none}:
#   self      person <-> own trust (surname link + given-name match) -> not family
#   clear     surname link, trust names a different individual       -> lower bound
#   ambiguous trust -> person, surname link, trust has no given names -> upper bound
#   none      no surname link (unrelated trustee sale etc.)
# Person names are "LAST FIRST MIDDLE". Initials and 3+ letter prefixes match.
nm_macros <- c(
  "CREATE OR REPLACE TEMP MACRO nm_gen() AS ['TRUST','TRUSTS','TRUSTEE','TRUSTEES','TR','TRS',
     'TRST','TTEE','TTEES','TSTEE','LIVING','LIV','LVG','LIVG','REVOCABLE','REVOC','REV',
     'IRREVOCABLE','IRREV','FAMILY','FAM','SETTLEMENT','PRESERVATION','SEPARATE','PROPERTY',
     'THE','OF','AND','ET','AL','DATED','DTD','UTD','UDT','DECLARATION','AGREEMENT','SURVIVORS',
     'SURVIVOR','MARITAL','BYPASS','EXEMPTION','CREDIT','SHELTER','QTIP','TESTAMENTARY','INTER',
     'VIVOS','JOINT','LIFE','ESTATE','REAL','INVESTMENT','JR','SR','II','III','IV','FBO','UTA',
     'UAD','TRUSTOR','TRUSTORS','GRANTOR','CHARITABLE','REMAINDER','UNITRUST','ANNUITY']",
  "CREATE OR REPLACE TEMP MACRO nm_tok(x) AS list_filter(string_split(trim(regexp_replace(
     upper(coalesce(x, '')), '[^A-Z]+', ' ', 'g')), ' '), z -> length(z) > 0)",
  "CREATE OR REPLACE TEMP MACRO nm_ttok(x) AS list_filter(nm_tok(x),
     z -> NOT list_contains(nm_gen(), z))",
  "CREATE OR REPLACE TEMP MACRO nm_fw(x) AS split_part(trim(coalesce(x, '')), ' ', 1)",
  "CREATE OR REPLACE TEMP MACRO nm_sur(x) AS list_filter(nm_tok(nm_fw(x)), z -> length(z) >= 2)",
  "CREATE OR REPLACE TEMP MACRO nm_giv(x) AS
     nm_tok(substr(trim(coalesce(x, '')), length(nm_fw(x)) + 1))",
  # first given name (gg[1]) matches a name in nn by exact/initial/prefix rule,
  # or any given name of 3+ letters equals a name in nn
  "CREATE OR REPLACE TEMP MACRO nm_gmatch(gg, nn) AS coalesce(len(list_filter(nn, z ->
     (len(gg) > 0 AND (gg[1] = z
        OR (length(gg[1]) = 1 AND z[1] = gg[1])
        OR (length(z) = 1 AND gg[1][1] = z)
        OR (length(gg[1]) >= 3 AND length(z) >= 3
            AND (starts_with(z, gg[1]) OR starts_with(gg[1], z)))))
     OR (length(z) >= 3 AND list_contains(gg, z)))) > 0, FALSE)",
  "CREATE OR REPLACE TEMP MACRO nm_link(pp, tt) AS
     len(list_filter(nm_sur(pp), z -> list_contains(nm_ttok(tt), z))) > 0",
  "CREATE OR REPLACE TEMP MACRO nm_ind(pp, tt) AS
     list_filter(nm_ttok(tt), z -> NOT list_contains(nm_sur(pp), z))",
  "CREATE OR REPLACE TEMP MACRO nm_t2p(tr, pe) AS CASE
     WHEN NOT nm_link(pe, tr) THEN 'none'
     WHEN nm_gmatch(nm_giv(pe), nm_ind(pe, tr)) THEN 'self'
     WHEN len(nm_ind(pe, tr)) >= 1 THEN 'clear'
     ELSE 'ambiguous' END",
  "CREATE OR REPLACE TEMP MACRO nm_p2t(pe, tr) AS CASE
     WHEN NOT nm_link(pe, tr) THEN 'none'
     WHEN len(nm_ind(pe, tr)) >= 1 AND NOT nm_gmatch(nm_giv(pe), nm_ind(pe, tr)) THEN 'clear'
     ELSE 'self' END",
  "CREATE OR REPLACE TEMP MACRO nm_common(ts, tb) AS list_filter(nm_ttok(ts),
     z -> length(z) >= 3 AND list_contains(nm_ttok(tb), z))",
  "CREATE OR REPLACE TEMP MACRO nm_uniq(ts, tb) AS list_filter(nm_ttok(ts),
     z -> NOT list_contains(nm_common(ts, tb), z))",
  "CREATE OR REPLACE TEMP MACRO nm_t2t(ts, tb) AS CASE
     WHEN len(nm_common(ts, tb)) = 0 THEN 'none'
     WHEN len(nm_uniq(ts, tb)) = 0 AND len(nm_uniq(tb, ts)) = 0 THEN 'self'
     WHEN len(nm_uniq(ts, tb)) > 0 AND len(nm_uniq(tb, ts)) > 0
          AND NOT nm_gmatch(nm_uniq(ts, tb), nm_uniq(tb, ts)) THEN 'clear'
     WHEN len(nm_uniq(ts, tb)) > 0 AND len(nm_uniq(tb, ts)) > 0 THEN 'self'
     ELSE 'ambiguous' END",
  # prior-deed check: buyer vs the person who earlier conveyed the property into the trust
  "CREATE OR REPLACE TEMP MACRO nm_pp(bu, pr) AS CASE
     WHEN len(list_filter(nm_sur(bu), z -> list_contains(nm_sur(pr), z))) = 0 THEN NULL
     WHEN nm_gmatch(nm_giv(bu), nm_giv(pr)) THEN 'self'
     ELSE 'clear' END",
  "CREATE OR REPLACE TEMP MACRO nm_best(a, b) AS CASE
     WHEN a = 'self' OR b = 'self' THEN 'self'
     WHEN a = 'clear' OR b = 'clear' THEN 'clear'
     WHEN a = 'ambiguous' OR b = 'ambiguous' THEN 'ambiguous'
     ELSE 'none' END"
)
for (m in nm_macros) dbExecute(con, m)

# Earlier deed that moved the parcel INTO the trust, to identify the settlor.
# Trust deeds only (small subset); latest prior conveyance per trust->person deed.
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE trust_deeds AS
  SELECT clip, sale_raw, b1_full, s1_full
  FROM ot_slim
  WHERE b1_full LIKE '%TRUST%' OR s1_full LIKE '%TRUST%'
")
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE prior_settlor AS
  SELECT clip, sale_raw, trust_name, p_name FROM (
    SELECT o.clip, o.sale_raw, o.s1_full AS trust_name, p.s1_full AS p_name,
           ROW_NUMBER() OVER (PARTITION BY o.clip, o.sale_raw, o.s1_full
                              ORDER BY p.sale_raw DESC, p.s1_full) AS rk
    FROM (SELECT DISTINCT clip, sale_raw, s1_full FROM trust_deeds
          WHERE s1_full LIKE '%TRUST%' AND b1_full NOT LIKE '%TRUST%') o
    JOIN trust_deeds p
      ON p.clip = o.clip AND p.sale_raw < o.sale_raw AND p.b1_full = o.s1_full
     AND p.s1_full <> '' AND p.s1_full NOT LIKE '%TRUST%'
  ) WHERE rk = 1
")
message("Prior-settlor rows: ",
        format(dbGetQuery(con, "SELECT COUNT(*) n FROM prior_settlor")$n, big.mark = ","))

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
      WHEN pcat = 'A'                                THEN 'market_sale'
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
    SELECT d.clip, d.state, d.fips5, d.sale_raw, d.sale_year, d.sale_month,
           d.sale_year * 100 + d.sale_month AS ym,
           d.land_use_code_static, d.property_indicator_code_static,
           d.pcat, d.dtype, d.interfam, d.amt, d.class,
           d.same_surname, d.same_person, d.trust_kw, d.estate_kw, d.corp_buyer,
           d.new_construction, d.reo, d.reo_sale,
           d.absentee_buyer, d.mail_state, d.mail_zip5, d.situs_zip5,
           -- trust rule applies only to family_trust events; NULL elsewhere
           CASE WHEN d.class <> 'family_trust' THEN NULL
                WHEN d.s1_full LIKE '%TRUST%' AND d.b1_full LIKE '%TRUST%' THEN 't2t'
                WHEN d.s1_full LIKE '%TRUST%' THEN 't2p'
                ELSE 'p2t' END AS trust_dir,
           CASE WHEN d.class <> 'family_trust' THEN NULL
                WHEN d.s1_full LIKE '%TRUST%' AND d.b1_full LIKE '%TRUST%'
                  THEN nm_t2t(d.s1_full, d.b1_full)
                WHEN d.s1_full LIKE '%TRUST%' THEN
                  CASE WHEN ps.p_name IS NOT NULL
                        AND (nm_pp(d.b1_full, ps.p_name) IS NOT NULL
                             OR nm_pp(d.b2_full, ps.p_name) IS NOT NULL)
                       THEN nm_best(nm_pp(d.b1_full, ps.p_name), nm_pp(d.b2_full, ps.p_name))
                       ELSE nm_best(nm_t2p(d.s1_full, d.b1_full),
                                    CASE WHEN d.b2_full <> '' AND d.b2_full NOT LIKE '%TRUST%'
                                         THEN nm_t2p(d.s1_full, d.b2_full) ELSE 'none' END)
                  END
                ELSE nm_p2t(d.s1_full, d.b1_full) END AS trust_rel,
           CASE WHEN d.class = 'family_trust' AND ps.p_name IS NOT NULL
                THEN 1 ELSE 0 END AS trust_prior_found
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
    ) d
    LEFT JOIN prior_settlor ps
      ON ps.clip = d.clip AND ps.sale_raw = d.sale_raw AND ps.trust_name = d.s1_full
    WHERE d.rn = 1
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

audit_trust <- dbGetQuery(con, glue("
  SELECT sale_year, trust_dir, trust_rel, COUNT(*) AS n,
         SUM(trust_prior_found) AS n_prior_found
  FROM {ev} WHERE class = 'family_trust'
  GROUP BY sale_year, trust_dir, trust_rel ORDER BY sale_year, trust_dir, trust_rel
"))
assert_rows(audit_trust, 5, "audit_trust_rel_by_year")
stopifnot(!anyNA(audit_trust$trust_rel))
write_csv_strict(audit_trust,
                 path(tables_out_dir, ifelse(dev_mode,
                                             "audit_trust_rel_by_year_dev.csv",
                                             "audit_trust_rel_by_year.csv")))

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
