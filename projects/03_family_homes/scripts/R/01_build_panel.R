#======================================================================#
# Purpose : Build the national residential deed-event panel. One pass over
#           the CoreLogic Owner Transfer parquet store gives one event per
#           parcel and signing date, with the deed class, the name-based
#           relation of buyer and seller, and the trust-route columns.
#           Every downstream script reads only this output.
# Name    : Saani Rawat
# Created : 06/09/2026
# Log     : 10/31/2026: class taxonomy and the trust rule (trust_rel).
#           11/11/2026: raw-field columns, name flags, trust bounds, the
#                       relation / trigger / vehicle dimensions, the
#                       chain-verified trust-to-heir flag, and the
#                       same-person and entity rules for cross-surname deeds.
#           11/16/2026: narrowed the same-person rule (person-level name
#                       match, no initial-only match), added new_buyer,
#                       dropped the _v2 file suffix, tidied the script.
#                       Added deed_cat / deed_subcat / fam_route, the locked
#                       taxonomy: deed_cat = 'family_transfer' is the family
#                       measure (direct deeds, three trust tiers, and every
#                       affidavit of death); added s_trust / b_trust on every row.
# Inputs  : data/corelogic_extracts/by_state/ot/  (through the duckdb glob)
#           shared_utils/R/reference/sale_document_type_meanings.csv
# Outputs : data/derived/03_family_homes/events[_dev].parquet
#           _outputs/logs/01_build_panel[_dev].log
#           _outputs/tables/audit_*_by_year[_state][_dev].csv
# Settings: FAM_DEV=1          build FAM_DEV_STATES only (default VT,RI,CA)
#           FAM_PANEL_OUT      output file name (default events[_dev].parquet)
#           FAM_ALLOW_REBUILD  1 = overwrite an existing output file
#           FAM_MEM_GB         duckdb memory cap in GB (default 24)
#           FAM_TMP_DIR        duckdb spill directory (default: local AppData
#                              temp, outside the OneDrive-synced repo)
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

#============================================#
# Settings ----
#============================================#

t_start    <- Sys.time()
dev_mode   <- Sys.getenv("FAM_DEV", "0") == "1"
dev_states <- strsplit(Sys.getenv("FAM_DEV_STATES", "VT,RI,CA"), ",")[[1]]
mem_gb     <- as.integer(Sys.getenv("FAM_MEM_GB", "24"))
sfx        <- ifelse(dev_mode, "_dev", "")      # suffix for the panel, log and audit tables

# output file: an existing panel is overwritten only on request
panel_out_name <- Sys.getenv("FAM_PANEL_OUT", paste0("events", sfx, ".parquet"))
out_parquet    <- path(data_dir, panel_out_name)
stopifnot(!file_exists(out_parquet) || Sys.getenv("FAM_ALLOW_REBUILD", "0") == "1")

# spill directory on a local disk (the repo lives in OneDrive)
tmp_dir <- Sys.getenv("FAM_TMP_DIR",
                      file.path(Sys.getenv("LOCALAPPDATA"), "Temp", "fam_duckdb_tmp"))
dir_create(tmp_dir)

# cat() goes to stdout so the sink log carries timestamps (message() does not)
log_file <- path(logs_dir, paste0("01_build_panel", sfx, ".log"))
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
cat("Starting 01_build_panel at ", format(Sys.time()), " | dev_mode=", dev_mode,
    " | mem_gb=", mem_gb, " | tmp=", tmp_dir, " | out=", panel_out_name, "\n", sep = "")

con <- open_corelogic_duckdb(memory_limit = paste0(mem_gb, "GB"))
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(tmp_dir)}'"))

# Owner Transfer files to read: every state, or the dev states only
if (dev_mode) {
  ot_root <- normalizePath(here("data", "corelogic_extracts", "by_state", "ot"),
                           winslash = "/", mustWork = TRUE)
  globs <- gsub("\\\\", "/",
                file.path(ot_root, paste0("state=", dev_states), "year=*", "*.parquet"))
  glob_sql <- paste0("['", paste(sql_quote_path(globs), collapse = "', '"), "']")
} else {
  glob_sql <- paste0("'", sql_quote_path(ot_parquet_glob()), "'")
}

# One event per (clip, sale_raw): the order that picks the surviving record. Arm's-length
# first, then the highest price; the remaining keys cover every output column, so the order
# is total and a rebuild from the same raw store returns the same rows. `class` is an early
# key, so it never depends on a tie. Used by the main build and by the trust-chain step.
dedupe_order_sql <- "
  CASE WHEN pcat = 'A' THEN 0 ELSE 1 END, amt DESC NULLS LAST,
  class ASC, dtype ASC NULLS LAST, b1_full ASC, s1_full ASC,
  doc_type ASC NULLS LAST, b2_full ASC,
  state ASC, fips5 ASC NULLS LAST, mail_zip5 ASC, situs_zip5 ASC,
  mail_state ASC, rel_code ASC NULLS LAST, own_code ASC NULLS LAST,
  occ_code ASC NULLS LAST, title_co ASC,
  interfam ASC NULLS LAST, land_use_code_static ASC NULLS LAST,
  property_indicator_code_static ASC NULLS LAST, corp_buyer ASC,
  new_construction ASC NULLS LAST, reo ASC NULLS LAST, reo_sale ASC NULLS LAST,
  mortgage_purch ASC NULLS LAST, partial_int ASC, mail_eq_situs ASC NULLS LAST,
  b1_last ASC, s1_last ASC, rec_raw ASC NULLS LAST, pcat ASC NULLS LAST"

#============================================#
# 1. Raw view: slim, typed columns ----
#============================================#
# Deed classes (research_spec.md):
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
    coalesce(CAST(buyer_mailing_state AS VARCHAR), '')         AS mail_state,
    -- raw-field columns: document type, vesting codes, title company, addresses
    NULLIF(upper(trim(CAST(sale_document_type_code AS VARCHAR))), '')      AS doc_type,
    NULLIF(upper(trim(CAST(buyer_relationship_type_code AS VARCHAR))), '') AS rel_code,
    NULLIF(upper(trim(CAST(buyer_ownership_rights_code AS VARCHAR))), '')  AS own_code,
    NULLIF(upper(trim(CAST(buyer_occupancy_code AS VARCHAR))), '')         AS occ_code,
    CASE WHEN trim(coalesce(CAST(title_company_name AS VARCHAR), '')) <> ''
         THEN 1 ELSE 0 END                                                 AS title_co,
    TRY_CAST(mortgage_purchase_indicator AS INTEGER)                       AS mortgage_purch,
    CASE WHEN trim(coalesce(CAST(partial_interest_indicator AS VARCHAR), '')) <> ''
         THEN 1 ELSE 0 END                                                 AS partial_int,
    -- buyer mailing street equals property street (NULL if either is empty)
    CASE WHEN length(regexp_replace(upper(coalesce(CAST(buyer_mailing_street_address AS VARCHAR), '')), '[^A-Z0-9]', '', 'g')) > 0
          AND length(regexp_replace(upper(coalesce(CAST(deed_situs_street_address_static AS VARCHAR), '')), '[^A-Z0-9]', '', 'g')) > 0
         THEN CASE WHEN regexp_replace(upper(CAST(buyer_mailing_street_address AS VARCHAR)), '[^A-Z0-9]', '', 'g')
                     = regexp_replace(upper(CAST(deed_situs_street_address_static AS VARCHAR)), '[^A-Z0-9]', '', 'g')
                   THEN 1 ELSE 0 END
         ELSE NULL END                                                     AS mail_eq_situs,
    -- recording date (yyyymmdd); sale_derived_date is the signing date. Not a dedupe key.
    TRY_CAST(sale_derived_recording_date AS BIGINT)                        AS rec_raw
  FROM read_parquet({glob_sql}, union_by_name = true)
  WHERE residential_indicator = 'Y'
    AND TRY_CAST(clip AS BIGINT) IS NOT NULL
    AND TRY_CAST(sale_derived_date AS BIGINT) BETWEEN 20070101 AND 20241231
    AND CAST(floor(TRY_CAST(sale_derived_date AS BIGINT) / 100) % 100 AS INTEGER) BETWEEN 1 AND 12
"))

#============================================#
# 2. Name rules ----
#============================================#
# Person names are recorded as "LAST FIRST MIDDLE", joint names as "LAST FIRST M & FIRST M".

# |- trust-mediated family transfers (research_spec.md, 10/31/2026) ----
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
     ELSE 'none' END",
  # ---- trust_chain (11/11/2026): settlor A (seller of the earlier into-trust deed) vs buyer B ----
  # generational suffix tokens (JR, SR, II, III, IV) on a name
  "CREATE OR REPLACE TEMP MACRO nm_sfx(x) AS
     list_filter(nm_tok(x), z -> list_contains(['JR','SR','II','III','IV'], z))",
  # one buyer b against settlor a: NULL if b is empty or a trust; other = surnames differ;
  # unclear = same surname but a given name is missing; self = same given name and same
  # suffixes; heir = same surname and a different given name (or a suffix separates them)
  "CREATE OR REPLACE TEMP MACRO nm_chain1(a, b) AS CASE
     WHEN b IS NULL OR b = '' OR b LIKE '%TRUST%' THEN NULL
     WHEN len(list_filter(nm_sur(b), z -> list_contains(nm_sur(a), z))) = 0 THEN 'other'
     WHEN len(nm_giv(a)) = 0 OR len(nm_giv(b)) = 0 THEN 'unclear'
     WHEN nm_gmatch(nm_giv(b), nm_giv(a)) AND nm_sfx(a) = nm_sfx(b) THEN 'self'
     ELSE 'heir' END",
  "CREATE OR REPLACE TEMP MACRO nm_chain(a, b1, b2) AS CASE
     WHEN nm_chain1(a, b1) = 'self' OR nm_chain1(a, b2) = 'self' THEN 'self'
     WHEN nm_chain1(a, b1) = 'heir' OR nm_chain1(a, b2) = 'heir' THEN 'heir'
     WHEN nm_chain1(a, b1) = 'unclear' OR nm_chain1(a, b2) = 'unclear' THEN 'unclear'
     ELSE 'other' END",
  # ---- same owner on both sides of a same-surname deed (11/16/2026) ----
  # Compares people, not tokens: the given part of a name is split on '&' into one token
  # list per person, e.g. 'SMITH JOHN A & MARY' -> [[JOHN, A], [MARY]].
  "CREATE OR REPLACE TEMP MACRO nm_people(x) AS list_filter(list_transform(
     string_split(substr(trim(upper(coalesce(x, ''))), length(nm_fw(x)) + 1), '&'),
     p -> nm_tok(p)), p -> len(p) > 0)",
  # middle names conflict when both are present and start with different letters
  "CREATE OR REPLACE TEMP MACRO nm_mid_ok(p, q) AS
     NOT (len(p) >= 2 AND len(q) >= 2 AND p[2][1] <> q[2][1])",
  # same first name written out: equal, or one a 3+ letter prefix of the other
  "CREATE OR REPLACE TEMP MACRO nm_first_eq(p, q) AS (length(p[1]) >= 2 AND length(q[1]) >= 2
     AND (p[1] = q[1] OR (length(p[1]) >= 3 AND length(q[1]) >= 3
          AND (starts_with(p[1], q[1]) OR starts_with(q[1], p[1])))))",
  # first name given as an initial on one side
  "CREATE OR REPLACE TEMP MACRO nm_init_eq(p, q) AS ((length(p[1]) = 1 OR length(q[1]) = 1)
     AND p[1][1] = q[1][1])",
  # one person: the first name agrees, or a first initial backed by a second name on both
  # sides. A middle initial is never matched to a first name (ORTIZ JOSEPH A is not
  # ORTIZ ALEXANDER D), and a shared first name with different middle initials is two people.
  "CREATE OR REPLACE TEMP MACRO nm_person_eq(p, q) AS (nm_mid_ok(p, q) AND (nm_first_eq(p, q)
     OR (nm_init_eq(p, q) AND len(p) >= 2 AND len(q) >= 2)))",
  # looser: a first initial alone is enough
  "CREATE OR REPLACE TEMP MACRO nm_person_loose(p, q) AS (nm_mid_ok(p, q)
     AND (nm_first_eq(p, q) OR nm_init_eq(p, q)))",
  # buyers bb against sellers ss: the owner stays on title if one buyer is a seller, or if
  # two buyers agree with sellers on initials (a couple written 'W A & T G')
  "CREATE OR REPLACE TEMP MACRO nm_owner_stays(bb, ss) AS (
     len(list_filter(bb, p -> len(list_filter(ss, q -> nm_person_eq(p, q))) > 0)) > 0
     OR len(list_filter(bb, p -> len(list_filter(ss, q -> nm_person_loose(p, q))) > 0)) >= 2)",
  # a buyer who is nobody on the seller side: a new person joins title
  "CREATE OR REPLACE TEMP MACRO nm_new_buyer(bb, ss) AS
     len(list_filter(bb, p -> len(list_filter(ss, q -> nm_person_loose(p, q))) = 0)) > 0",
  # ---- same-person rule for cross-surname deeds (11/11/2026) ----
  # entity party: LLC, INC, TRUST, HOLDINGS, OWNER RECORD and similar words in a name
  "CREATE OR REPLACE TEMP MACRO nm_ent(x) AS regexp_matches(upper(coalesce(x, '')),
     '\\b(LLC|INC|CORP|CO|LP|LTD|TRUST|HOLDINGS?|HLDGS|PROPERT[A-Z]*|HOMES?|BANK|OWNER RECORD|ASSOC[A-Z]*|ENTERPRISES?|BUILDERS?|INVESTMENTS?|FT|ESTATE|SHERIFF|COUNTY)\\b')",
  "CREATE OR REPLACE TEMP MACRO nm_sq(x) AS regexp_replace(upper(coalesce(x, '')), '[^A-Z]', '', 'g')",
  "CREATE OR REPLACE TEMP MACRO nm_skey(x) AS nm_sq(nm_fw(x))",
  "CREATE OR REPLACE TEMP MACRO nm_g1(x) AS CASE WHEN len(nm_giv(x)) > 0 THEN nm_giv(x)[1] ELSE '' END",
  "CREATE OR REPLACE TEMP MACRO nm_g2(x) AS CASE WHEN len(nm_giv(x)) > 1 THEN nm_giv(x)[2] ELSE '' END",
  # similar surname: Jaro-Winkler on the first word at least 0.85, or one contains the other
  "CREATE OR REPLACE TEMP MACRO nm_sim_sur(a, b) AS (jaro_winkler_similarity(nm_skey(a), nm_skey(b)) >= 0.85
     OR (length(nm_skey(a)) >= 4 AND length(nm_skey(b)) >= 4
         AND (contains(nm_skey(a), nm_skey(b)) OR contains(nm_skey(b), nm_skey(a)))))",
  "CREATE OR REPLACE TEMP MACRO nm_tokset(x) AS list_sort(list_distinct(list_filter(nm_tok(x), z -> length(z) >= 2)))",
  # a shares a surname token with b (the same_surname flag missed it)
  "CREATE OR REPLACE TEMP MACRO nm_shared(a, b) AS (b <> '' AND len(list_filter(nm_sur(a),
     z -> list_contains(nm_sur(b), z))) > 0)",
  # strict same person: (1) same token set in any order, (2) same first given name and a similar
  # surname, (3) same first given name and the same middle initial under any surname.
  # A generational suffix (JR, SR, II, III, IV) on either side blocks all three.
  "CREATE OR REPLACE TEMP MACRO nm_same_person(a, b) AS (b <> ''
     AND len(nm_sfx(a)) = 0 AND len(nm_sfx(b)) = 0
     AND ((len(nm_tokset(a)) >= 2 AND nm_tokset(a) = nm_tokset(b))
       OR (length(nm_g1(a)) >= 3 AND nm_g1(a) = nm_g1(b) AND nm_sim_sur(a, b)
           AND (nm_g2(a) = '' OR nm_g2(b) = '' OR nm_g2(a)[1] = nm_g2(b)[1]))
       OR (length(nm_g1(a)) >= 3 AND nm_g1(a) = nm_g1(b) AND nm_g2(a) <> '' AND nm_g2(b) <> ''
           AND nm_g2(a)[1] = nm_g2(b)[1])))"
)
for (m in nm_macros) dbExecute(con, m)

# |- settlor of a trust: the earlier deed that moved the parcel into it ----
# Trust deeds only (small subset); latest prior conveyance per trust->person deed.
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE trust_deeds AS
  SELECT clip, sale_raw, b1_full, s1_full, pcat, interfam
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

# |- chain-verified trust-to-heir (11/11/2026) ----
# A person deeds the home into a trust and the trust later deeds it to a different person
# with the same surname. For each trust-to-person conveyance, find the earlier non-market
# deed that moved the parcel from a person A into a trust. Prefer a deed whose buyer name
# equals the conveying trust's name (named = 1); otherwise take the most recent into-trust
# deed (named = 0). The chain is broken, and trust_chain = 'no_chain', if a market sale of
# the parcel falls strictly between the two deeds.
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE chain_pick AS
  SELECT clip, sale_raw, trust_name, a_name, prior_raw, named FROM (
    SELECT o.clip, o.sale_raw, o.s1_full AS trust_name, p.s1_full AS a_name,
           p.sale_raw AS prior_raw, (p.b1_full = o.s1_full)::INT AS named,
           ROW_NUMBER() OVER (PARTITION BY o.clip, o.sale_raw, o.s1_full
                              ORDER BY (p.b1_full = o.s1_full) DESC, p.sale_raw DESC,
                                       p.s1_full, p.b1_full) AS rk
    FROM (SELECT DISTINCT clip, sale_raw, s1_full FROM trust_deeds
          WHERE s1_full LIKE '%TRUST%' AND b1_full NOT LIKE '%TRUST%') o
    JOIN trust_deeds p
      ON p.clip = o.clip AND p.sale_raw < o.sale_raw
     AND p.b1_full LIKE '%TRUST%' AND p.s1_full <> '' AND p.s1_full NOT LIKE '%TRUST%'
     AND NOT (p.pcat = 'A' AND coalesce(p.interfam, 0) <> 1)   -- prior deed is not a market sale
  ) WHERE rk = 1
")
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE chain_mkt AS
  SELECT DISTINCT c.clip, c.sale_raw, c.trust_name
  FROM chain_pick c
  JOIN (SELECT clip, sale_raw FROM ot_slim WHERE pcat = 'A' AND coalesce(interfam, 0) <> 1) m
    ON m.clip = c.clip AND m.sale_raw > c.prior_raw AND m.sale_raw < c.sale_raw
")
message("Chain candidates: ",
        format(dbGetQuery(con, "SELECT COUNT(*) n FROM chain_pick")$n, big.mark = ","),
        " | broken by a market sale: ",
        format(dbGetQuery(con, "SELECT COUNT(*) n FROM chain_mkt")$n, big.mark = ","))

#============================================#
# 3. Deed class ----
#============================================#
# First match wins. Every family_* class needs the vendor's interfamily flag.
dbExecute(con, "
  CREATE OR REPLACE TEMP VIEW ot_classed AS
  WITH flagged AS (
    SELECT *,
      (b1_full LIKE '%TRUST%' OR s1_full LIKE '%TRUST%')        AS trust_kw,
      (s1_full LIKE '%TRUST%')                                  AS s_trust,
      (b1_full LIKE '%TRUST%')                                  AS b_trust,
      -- ADMINISTRATOR/-TRIX (not ADMINISTRAT%) to avoid matching agency
      -- REO sellers like VETERANS ADMINISTRATION (r-review minor 6)
      (b1_full LIKE '%ESTATE OF%' OR s1_full LIKE '%ESTATE OF%'
        OR b1_full LIKE '%EXECUT%'  OR s1_full LIKE '%EXECUT%'
        OR b1_full LIKE '%PROBATE%' OR s1_full LIKE '%PROBATE%'
        OR b1_full LIKE '%ADMINISTRATOR%' OR s1_full LIKE '%ADMINISTRATOR%'
        OR b1_full LIKE '%ADMINISTRATRIX%' OR s1_full LIKE '%ADMINISTRATRIX%') AS estate_kw,
      (b1_last <> '' AND b1_last = s1_last)                     AS same_surname,
      -- exact retitle: the whole buyer 1 or buyer 2 name equals the whole seller 1 name.
      -- Name variants of the same person are caught later, by the relation rules.
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

#============================================#
# 4. Relation, trigger and vehicle ----
#============================================#
# Document-type groups come from the shared reference CSV (first row per code,
# upper-cased so the lower-case typo codes collapse onto WD).
doc_map <- read.csv(here("shared_utils", "R", "reference", "sale_document_type_meanings.csv"),
                    stringsAsFactors = FALSE)
doc_map$code <- toupper(doc_map$code)
doc_map <- doc_map[!duplicated(doc_map$code) & doc_map$code != "(MISSING)", c("code", "group")]
names(doc_map) <- c("code", "grp")
dbWriteTable(con, "doc_map", doc_map, overwrite = TRUE)

# |- relation of buyer and seller ----
# fam_relation / fam_relation_nm: ordered CASE, first match wins. Defined only for
# family_* classes.
#   fam_relation     uses the document-type spouse line (IT, IC in California; DS in
#                    Arizona). Spouse is identified ONLY there, so use it for California
#                    descriptive series.
#   fam_relation_nm  is the same CASE without the document-type spouse line (names and
#                    trust rules only). ANY group used in a cross-state comparison
#                    (triple difference, event study) must be built from fam_relation_nm.
# The family measure (00_setup.R) drops 'owner_retained' and 'unrelated_entity' rows of
# fam_relation_nm, so these rules decide which deeds count as family transfers. `class`
# does not depend on them.
rel_case <- function(with_spouse_doc) {
  spouse_line <- if (with_spouse_doc) {
    "WHEN doc_type IN ('IT','IC','DS')                                  THEN 'spouse'"
  } else ""
  paste0("CASE
    WHEN class NOT LIKE 'family_%'                                     THEN NULL
    ", spouse_line, "
    WHEN same_person                                                   THEN 'owner_retained'
    WHEN class = 'family_trust' AND trust_dir = 'p2t' AND trust_fam_lo = 1 THEN 'parent_child_candidate'
    WHEN class = 'family_trust' AND trust_rel = 'self'                 THEN 'owner_retained'
    WHEN class = 'family_trust' AND trust_rel IN ('clear','ambiguous') THEN 'parent_child_candidate'
    WHEN class = 'family_trust'                                        THEN 'unrelated_entity'
    WHEN corp_buyer = 1                                                THEN 'unrelated_entity'
    WHEN same_surname AND given_differ = 0 AND gen_suffix = 0          THEN 'owner_retained'
    WHEN same_surname AND given_differ IS NOT NULL                     THEN 'parent_child_candidate'
    WHEN same_surname                                                  THEN 'unknown'
    -- class family_other, surnames differ by the flag fields: entity parties, the same person
    -- under another name, and same-surname deeds the flag missed are separated first; the
    -- rest are cross_surname
    WHEN class = 'family_other' AND (nm_ent(s1_full) OR nm_ent(b1_full))  THEN 'unrelated_entity'
    WHEN class = 'family_other' AND (nm_same_person(s1_full, b1_full)
                                  OR nm_same_person(s1_full, b2_full))     THEN 'owner_retained'
    WHEN class = 'family_other' AND (nm_shared(s1_full, b1_full)
                                  OR nm_shared(s1_full, b2_full)) THEN
         CASE WHEN given_differ = 0 AND gen_suffix = 0 THEN 'owner_retained'
              WHEN given_differ IS NOT NULL THEN 'parent_child_candidate'
              ELSE 'unknown' END
    WHEN class = 'family_other'                                        THEN 'cross_surname'
    ELSE 'unknown' END")
}

# |- trust keyword flags ----
# Keyword flags on the trust-side name (seller if it names a trust, else buyer 1);
# family_trust rows only. Patterns are plain strings (no braces) so glue leaves them alone.
trust_side <- "CASE WHEN d.s1_full LIKE '%TRUST%' THEN d.s1_full ELSE d.b1_full END"
kw_flag <- function(pattern) {
  paste0("CASE WHEN d.class = 'family_trust' THEN regexp_matches(", trust_side,
         ", '", pattern, "')::INT END")
}
kw_irrev <- kw_flag("\\bIRREV")
kw_resid <- kw_flag("\\b(QPRT|RESIDENCE)\\b|PERSONAL RES")
kw_child <- kw_flag("\\b(GRAND)?CHILD(REN|RENS)?\\b")
kw_post  <- kw_flag("\\b(MARITAL|BYPASS|QTIP|SURVIVORS?|DECEDENTS?|EXEMPTION|CREDIT SHELTER)\\b")
rel_sql    <- rel_case(TRUE)
rel_nm_sql <- rel_case(FALSE)

#============================================#
# 5. Dedupe and write the panel ----
#============================================#
# Multi-parcel sales and re-recorded corrections give several raw records for one parcel
# and date; dedupe_order_sql keeps one. The main query writes a stage file on the local
# disk, and the trust-chain columns are joined on afterwards (step 5b).
stage_parquet <- gsub("\\\\", "/", file.path(tmp_dir, paste0("stage_", panel_out_name)))
copy_sql <- glue("
  COPY (
    WITH d AS (
      SELECT * FROM (
        SELECT *, ROW_NUMBER() OVER (
          PARTITION BY clip, sale_raw ORDER BY {dedupe_order_sql}
        ) AS rn
        FROM ot_classed
      ) WHERE rn = 1
    ),
    n AS (
      SELECT d.*, g.grp AS doc_grp,
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
             THEN 1 ELSE 0 END AS trust_prior_found,
        -- person-name comparison: NULL for trusts, corporate buyers, missing names.
        -- given_differ = 0: a seller is still on title among the buyers (buyer 1 and 2)
        CASE WHEN d.trust_kw OR d.corp_buyer = 1 OR d.b1_full = '' OR d.s1_full = ''
                  OR len(nm_giv(d.b1_full)) = 0 OR len(nm_giv(d.s1_full)) = 0 THEN NULL
             WHEN nm_owner_stays(list_concat(nm_people(d.b1_full), nm_people(d.b2_full)),
                                 nm_people(d.s1_full))
               THEN 0
             ELSE 1 END AS given_differ,
        -- new_buyer = 1: a buyer who is nobody on the seller side (a person joins title)
        CASE WHEN d.trust_kw OR d.corp_buyer = 1 OR d.b1_full = '' OR d.s1_full = ''
                  OR len(nm_giv(d.b1_full)) = 0 OR len(nm_giv(d.s1_full)) = 0 THEN NULL
             WHEN nm_new_buyer(list_concat(nm_people(d.b1_full), nm_people(d.b2_full)),
                               nm_people(d.s1_full))
               THEN 1
             ELSE 0 END AS new_buyer,
        CASE WHEN list_has_any(nm_tok(d.b1_full), ['JR','SR','II','III','IV'])
               OR list_has_any(nm_tok(d.s1_full), ['JR','SR','II','III','IV'])
             THEN 1 ELSE 0 END AS gen_suffix,
        {kw_irrev} AS tr_irrev,
        {kw_resid} AS tr_resid,
        {kw_child} AS tr_child,
        {kw_post}  AS tr_post
      FROM d
      LEFT JOIN doc_map g ON g.code = d.doc_type
      LEFT JOIN prior_settlor ps
        ON ps.clip = d.clip AND ps.sale_raw = d.sale_raw AND ps.trust_name = d.s1_full
    ),
    b AS (
      -- Trust bounds. Trust-to-trust is NEVER counted (own vehicle cell); trust_to_self
      -- (trust -> person with given-name match) is outside both bounds.
      --   lo: trust_rel 'clear' (not t2t), plus person-into-own-trust deeds whose trust
      --       name carries an irrevocable / residence / children keyword and links to the buyer
      --   hi: lo plus 'ambiguous' trust-to-person deeds
      SELECT n.*,
        CASE WHEN class <> 'family_trust' OR trust_dir = 't2t' THEN NULL
             WHEN trust_rel = 'clear' THEN 1
             WHEN trust_dir = 'p2t' AND trust_rel = 'self'
                  AND (tr_irrev = 1 OR tr_resid = 1 OR tr_child = 1)
                  AND nm_link(b1_full, s1_full) THEN 1
             ELSE 0 END AS trust_fam_lo,
        CASE WHEN class <> 'family_trust' OR trust_dir = 't2t' THEN NULL
             WHEN trust_rel IN ('clear','ambiguous') THEN 1
             WHEN trust_dir = 'p2t' AND trust_rel = 'self'
                  AND (tr_irrev = 1 OR tr_resid = 1 OR tr_child = 1)
                  AND nm_link(b1_full, s1_full) THEN 1
             ELSE 0 END AS trust_fam_hi
      FROM n
    )
    SELECT clip, state, fips5, sale_raw, sale_year, sale_month,
           sale_year * 100 + sale_month AS ym,
           land_use_code_static, property_indicator_code_static,
           pcat, dtype, interfam, amt, class,
           same_surname, same_person, trust_kw, estate_kw, corp_buyer,
           new_construction, reo, reo_sale,
           absentee_buyer, mail_state, mail_zip5, situs_zip5,
           trust_dir, trust_rel, trust_prior_found,
           doc_type, doc_grp, rel_code, own_code, occ_code, title_co, mortgage_purch,
           partial_int, mail_eq_situs, rec_raw, given_differ, gen_suffix,
           tr_irrev, tr_resid, tr_child, tr_post, trust_fam_lo, trust_fam_hi,
           {rel_sql} AS fam_relation,
           {rel_nm_sql} AS fam_relation_nm,
           new_buyer,
           s_trust, b_trust,
           -- fam_trigger: death = death document type or estate keyword; death_likely =
           -- trust -> surname-linked person (clear/ambiguous); a trust -> own-name person
           -- deed is lifetime (owner takes title out of the trust)
           CASE
             WHEN class NOT LIKE 'family_%' THEN NULL
             WHEN doc_grp = 'estate_probate_death' AND doc_type <> 'GU' THEN 'death'
             WHEN doc_type IN ('PJ','EJ') OR estate_kw THEN 'death'
             WHEN class = 'family_trust' AND trust_dir = 't2p'
                  AND trust_rel IN ('clear','ambiguous') THEN 'death_likely'
             WHEN class = 'family_trust' AND trust_dir = 't2p' AND trust_rel = 'self' THEN 'lifetime'
             WHEN doc_grp IN ('market_conveyance','quitclaim','joint_tenancy_variant',
                              'family_interspousal','trust') THEN 'lifetime'
             ELSE 'unknown' END AS fam_trigger,
           -- fam_vehicle: how title moved. death_affidavit_trust isolates the 2018
           -- California vendor break (affidavits whose buyer name became a trust).
           CASE
             WHEN class NOT LIKE 'family_%' THEN NULL
             WHEN class <> 'family_trust' THEN 'direct'
             WHEN doc_type IN ('AF','AJ') THEN 'death_affidavit_trust'
             WHEN trust_dir = 't2t' THEN 'trust_to_trust'
             WHEN trust_rel = 'none' THEN 'trust_unlinked'
             WHEN trust_dir = 'p2t' AND trust_fam_lo = 1 THEN 'into_trust_for_others'
             WHEN trust_dir = 'p2t' THEN 'into_own_trust'
             WHEN trust_dir = 't2p' AND trust_rel = 'self' THEN 'trust_to_self'
             ELSE 'trust_to_other_person' END AS fam_vehicle
    FROM b
  ) TO '{sql_quote_path(stage_parquet)}' (FORMAT PARQUET, COMPRESSION ZSTD)
")
t_copy <- Sys.time()
dbExecute(con, copy_sql)
cat("Wrote stage ", stage_parquet, " in ",
    round(as.numeric(difftime(Sys.time(), t_copy, units = "mins")), 1),
    " min (", format(Sys.time()), ")\n", sep = "")

# |- 5b. trust_chain, on the small set of trust-to-person conveyances ----
# The dedupe winner for a (clip, sale_raw) depends only on that partition, so the same
# dedupe_order_sql on the clips that have a chain candidate gives the same winning row as
# the main query (and the same b1_full / b2_full) without joining the 148M-row result.
# Chain columns therefore never enter the dedupe decision.
t_chain <- Sys.time()
dbExecute(con, "CREATE OR REPLACE TEMP TABLE chain_clips AS SELECT DISTINCT clip FROM chain_pick")
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE chain_result AS
  SELECT w.clip, w.sale_raw,
         CASE WHEN tc.clip IS NULL OR mk.clip IS NOT NULL THEN 'no_chain'
              ELSE nm_chain(tc.a_name, w.b1_full, w.b2_full) END AS trust_chain,
         CASE WHEN tc.clip IS NOT NULL AND mk.clip IS NULL THEN tc.named END AS trust_chain_named,
         CASE WHEN tc.clip IS NOT NULL AND mk.clip IS NULL
              THEN date_diff('day', TRY_STRPTIME(CAST(tc.prior_raw AS VARCHAR), '%Y%m%d'),
                                    TRY_STRPTIME(CAST(w.sale_raw AS VARCHAR), '%Y%m%d')) END AS trust_chain_gap_days
  FROM (
    SELECT clip, sale_raw, class, s1_full, b1_full, b2_full FROM (
      SELECT *, ROW_NUMBER() OVER (
        PARTITION BY clip, sale_raw ORDER BY {dedupe_order_sql}) AS rn
      FROM ot_classed WHERE clip IN (SELECT clip FROM chain_clips)
    ) WHERE rn = 1
  ) w
  LEFT JOIN chain_pick tc ON tc.clip = w.clip AND tc.sale_raw = w.sale_raw AND tc.trust_name = w.s1_full
  LEFT JOIN chain_mkt  mk ON mk.clip = w.clip AND mk.sale_raw = w.sale_raw AND mk.trust_name = w.s1_full
  WHERE w.class = 'family_trust' AND w.s1_full LIKE '%TRUST%' AND w.b1_full NOT LIKE '%TRUST%'
"))
cat("Chain result rows: ", format(dbGetQuery(con, "SELECT COUNT(*) n FROM chain_result")$n, big.mark = ","),
    " in ", round(as.numeric(difftime(Sys.time(), t_chain, units = "mins")), 1), " min\n", sep = "")

#============================================#
# 6. Deed categories: the locked taxonomy ----
#============================================#
# deed_cat puts every deed event in exactly one of six categories, deed_subcat splits each,
# and fam_route says how a family transfer moved. Locked 11/16/2026 (research_spec.md).
#
# family_transfer  THE FAMILY MEASURE. A recorded deed that moves a home, with no sale, from
#                  one person (or that person's trust) to a different person, during life or
#                  at death. The vendor's interfamily flag is required throughout.
#   direct deeds   no trust and no entity on either side, and the names show two distinct
#                  people. deed_subcat = same_surname | cross_surname | unknown.
#                  fam_route = lifetime | death_affidavit | probate | unknown
#                    death_affidavit  affidavit of death (AF, AJ): a survivor takes title.
#                                     Affidavits with a trust party count too
#                                     (deed_subcat = affidavit_trust_party): from 2018 the
#                                     vendor records a trust on many affidavits, and only
#                                     the two kinds together are comparable over time
#                    probate          executor's, administrator's, personal representative's
#                                     deed, deed of distribution, or an estate keyword
#   trust deeds    fam_route = trust. Title leaves a trust for someone other than its
#                  settlor. Lifetime gifts by a living trustee and distributions after a
#                  death are not separated: both carry out the settlor's plan.
#                    trust_heir_chain      A deeded the home into a trust; the trust deeds it
#                                          to B, same surname, different person (10-year
#                                          look-back, complete from 2017)
#                                          SMITH JOHN -> SMITH FAMILY TRUST -> SMITH MARY
#                    trust_named_other     the trust names one person and deeds to a
#                                          different person with that surname
#                                          JOHN A SMITH TRUST -> SMITH MARY
#                    trust_to_trust_other  two trusts that share a name (any word of 3+
#                                          letters, usually the surname) and each name
#                                          someone the other does not
#                                          JOHN A SMITH TRUST -> MARY SMITH TRUST
# same_owner       flagged, no trust, the seller is still on title: the same people
#                  (same_owners) or the same people plus someone new (new_person_joins)
# entity           flagged, no trust, a company, lender or agency on either side
# trust            flagged trust deeds that are NOT family transfers
#                    into_own_trust             SMITH JOHN -> JOHN SMITH TRUST
#                    trust_to_settlor           JOHN SMITH TRUST -> SMITH JOHN
#                    trust_to_trust_same        JOHN SMITH TRUST -> JOHN SMITH 2019 TRUST
#                    trust_surname_only         SMITH FAMILY TRUST -> SMITH MARY with no chain:
#                                               heir or settlor, the names cannot say (upper bound)
#                    trust_to_unlinked_unpriced a trust to a person with another surname, no
#                                               price, not arm's-length (upper bound)
#                    trust_to_unlinked_priced   the same with a price or arm's-length
#                    into_trust_for_others, into_trust_unlinked, trust_to_trust_unclear
# market_sale      arm's-length, not flagged
# other            not flagged and not arm's-length (other_nonarms | estate_noninterfam)
family_cls_sql <- "('family_person','family_estate','family_other')"

# |- trust deeds that are family transfers (the three core tiers) ----
trust_live_sql  <- "(class = 'family_trust' AND fam_vehicle <> 'death_affidavit_trust')"
trust_chain_sql <- paste0("coalesce(trust_dir = 't2p' AND trust_chain = 'heir' ",
                          "AND trust_chain_gap_days <= 3652, FALSE)")
trust_named_sql <- paste0("coalesce(trust_dir = 't2p' AND trust_rel = 'clear' ",
                          "AND coalesce(trust_chain, '') <> 'self', FALSE)")
trust_t2t_sql   <- "coalesce(trust_dir = 't2t' AND trust_rel = 'clear', FALSE)"
trust_core_sql  <- glue("({trust_live_sql} AND ({trust_chain_sql} OR {trust_named_sql} OR {trust_t2t_sql}))")
# affidavits of death with a trust party: family transfers at death, not part of the trust route
affidavit_trust_sql <- "(class = 'family_trust' AND fam_vehicle = 'death_affidavit_trust')"
family_direct_sql <- glue("(class IN {family_cls_sql} ",
                          "AND fam_relation_nm NOT IN ('owner_retained','unrelated_entity'))")

deed_cat_sql <- glue("CASE
    WHEN class = 'market_sale'                                              THEN 'market_sale'
    WHEN {trust_core_sql} OR {affidavit_trust_sql}                          THEN 'family_transfer'
    WHEN class = 'family_trust'                                             THEN 'trust'
    WHEN class = 'family_retitle'                                           THEN 'same_owner'
    WHEN class IN {family_cls_sql} AND fam_relation_nm = 'owner_retained'   THEN 'same_owner'
    WHEN class IN {family_cls_sql} AND fam_relation_nm = 'unrelated_entity' THEN 'entity'
    WHEN class IN {family_cls_sql}                                          THEN 'family_transfer'
    ELSE 'other' END")

# the chain comes first: a chain-verified heir stays an heir whatever the name rule says
deed_subcat_sql <- glue("CASE
    WHEN class = 'market_sale' THEN 'market_sale'
    WHEN {trust_core_sql} THEN CASE
      WHEN {trust_chain_sql} THEN 'trust_heir_chain'
      WHEN {trust_named_sql} THEN 'trust_named_other'
      ELSE 'trust_to_trust_other' END
    WHEN {affidavit_trust_sql} THEN 'affidavit_trust_party'
    WHEN class = 'family_trust' THEN CASE
      WHEN trust_dir = 't2p' AND (trust_rel = 'self' OR coalesce(trust_chain, '') = 'self')
                                                           THEN 'trust_to_settlor'
      WHEN trust_dir = 't2p' AND trust_rel = 'ambiguous'   THEN 'trust_surname_only'
      WHEN trust_dir = 't2p' AND coalesce(amt, 0) = 0 AND coalesce(pcat, '') <> 'A'
                                                           THEN 'trust_to_unlinked_unpriced'
      WHEN trust_dir = 't2p'                               THEN 'trust_to_unlinked_priced'
      WHEN trust_dir = 'p2t' AND fam_vehicle = 'into_trust_for_others' THEN 'into_trust_for_others'
      WHEN trust_dir = 'p2t' AND trust_rel = 'none'        THEN 'into_trust_unlinked'
      WHEN trust_dir = 'p2t'                               THEN 'into_own_trust'
      WHEN trust_rel = 'self'                              THEN 'trust_to_trust_same'
      ELSE 'trust_to_trust_unclear' END
    WHEN class = 'family_retitle'
      OR (class IN {family_cls_sql} AND fam_relation_nm = 'owner_retained') THEN
      CASE WHEN new_buyer = 1 THEN 'new_person_joins' ELSE 'same_owners' END
    WHEN class IN {family_cls_sql} AND fam_relation_nm = 'unrelated_entity' THEN 'entity'
    WHEN class IN {family_cls_sql} THEN CASE fam_relation_nm
      WHEN 'parent_child_candidate' THEN 'same_surname'
      WHEN 'cross_surname'          THEN 'cross_surname'
      ELSE 'unknown' END
    ELSE class END")

fam_route_sql <- glue("CASE
    WHEN {affidavit_trust_sql}        THEN 'death_affidavit'
    WHEN {trust_core_sql}             THEN 'trust'
    WHEN NOT {family_direct_sql}      THEN NULL
    WHEN doc_type IN ('AF','AJ')      THEN 'death_affidavit'
    WHEN fam_trigger = 'death'        THEN 'probate'
    WHEN fam_trigger = 'lifetime'     THEN 'lifetime'
    ELSE 'unknown' END")

# |- final file = stage + chain columns + categories (rows and order of the stage are kept) ----
# chain_result holds class = 'family_trust' trust-to-person rows only, so trust_chain is NULL
# elsewhere. The join is on the two keys only: a residual predicate in ON made it crawl.
dbExecute(con, glue("
  COPY (
    SELECT f.*, {deed_cat_sql} AS deed_cat, {deed_subcat_sql} AS deed_subcat,
           {fam_route_sql} AS fam_route
    FROM (
      SELECT s.*,
             -- trust-to-person rows with no candidate chain row are 'no_chain'; other rows NULL
             CASE WHEN s.class = 'family_trust' AND s.trust_dir = 't2p'
                  THEN coalesce(r.trust_chain, 'no_chain') END AS trust_chain,
             r.trust_chain_named, r.trust_chain_gap_days
      FROM read_parquet('{sql_quote_path(stage_parquet)}') s
      LEFT JOIN chain_result r ON r.clip = s.clip AND r.sale_raw = s.sale_raw
    ) f
  ) TO '{sql_quote_path(out_parquet)}' (FORMAT PARQUET, COMPRESSION ZSTD)
"))
file_delete(stage_parquet)
cat("Wrote ", out_parquet, " at ", format(Sys.time()), "\n", sep = "")

#============================================#
# 7. Audits (read back from the written parquet) ----
#============================================#
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
  guards$n_states >= ifelse(dev_mode, length(dev_states), 51),
  guards$interfam_share > 0.10, guards$interfam_share < 0.60
)

# Deed-category guards: every event has a category, a route exactly when it is a family
# transfer, and the direct component equals the class-and-relation rule
audit_cat <- dbGetQuery(con, glue("
  SELECT sale_year, deed_cat, deed_subcat, fam_route, COUNT(*) AS n,
         SUM(((deed_cat = 'family_transfer' AND fam_route <> 'trust'
               AND deed_subcat <> 'affidavit_trust_party') IS DISTINCT FROM
              coalesce({family_direct_sql}, FALSE))::INT) AS n_mismatch
  FROM {ev} GROUP BY ALL ORDER BY sale_year, deed_cat, deed_subcat, fam_route
"))
stopifnot(!anyNA(audit_cat$deed_cat), !anyNA(audit_cat$deed_subcat),
          all(is.na(audit_cat$fam_route) == (audit_cat$deed_cat != "family_transfer")),
          sum(audit_cat$n_mismatch) == 0, sum(audit_cat$n) == n_total)
write_csv_strict(select(audit_cat, -n_mismatch),
                 path(tables_out_dir, paste0("audit_deed_cat_by_year", sfx, ".csv")))

audit_class <- dbGetQuery(con, glue("
  SELECT sale_year, class, COUNT(*) AS n
  FROM {ev} GROUP BY sale_year, class ORDER BY sale_year, class
"))
assert_rows(audit_class, 50, "audit_class_by_year")
write_csv_strict(audit_class,
                 path(tables_out_dir, paste0("audit_class_by_year", sfx, ".csv")))

audit_trust <- dbGetQuery(con, glue("
  SELECT sale_year, trust_dir, trust_rel, COUNT(*) AS n,
         SUM(trust_prior_found) AS n_prior_found
  FROM {ev} WHERE class = 'family_trust'
  GROUP BY sale_year, trust_dir, trust_rel ORDER BY sale_year, trust_dir, trust_rel
"))
assert_rows(audit_trust, 5, "audit_trust_rel_by_year")
stopifnot(!anyNA(audit_trust$trust_rel))
write_csv_strict(audit_trust,
                 path(tables_out_dir, paste0("audit_trust_rel_by_year", sfx, ".csv")))

audit_names <- dbGetQuery(con, glue("
  SELECT sale_year,
         AVG(CASE WHEN interfam = 1 THEN 1.0 ELSE 0 END)        AS interfam_share,
         AVG(CASE WHEN same_surname THEN 1.0 ELSE 0 END)        AS same_surname_share,
         AVG(CASE WHEN trust_kw THEN 1.0 ELSE 0 END)            AS trust_share,
         AVG(CASE WHEN estate_kw THEN 1.0 ELSE 0 END)           AS estate_share
  FROM {ev} GROUP BY sale_year ORDER BY sale_year
"))
write_csv_strict(audit_names,
                 path(tables_out_dir, paste0("audit_name_population_by_year", sfx, ".csv")))

audit_absentee <- dbGetQuery(con, glue("
  SELECT sale_year,
         AVG(CASE WHEN absentee_buyer IS NOT NULL THEN 1.0 ELSE 0 END) AS absentee_observed,
         AVG(CASE WHEN absentee_buyer = 1 THEN 1.0 ELSE 0 END)         AS absentee_share_all
  FROM {ev} GROUP BY sale_year ORDER BY sale_year
"))
write_csv_strict(audit_absentee,
                 path(tables_out_dir, paste0("audit_absentee_coverage_by_year", sfx, ".csv")))

# Taxonomy cells by state and year (validation 2.5): every fam_relation x fam_trigger x
# fam_vehicle cell, plus the nm relation, with the spouse split described for CA only
audit_taxonomy <- dbGetQuery(con, glue("
  SELECT state, sale_year, fam_relation, fam_relation_nm, fam_trigger, fam_vehicle,
         COUNT(*) AS n
  FROM {ev} WHERE class LIKE 'family_%'
  GROUP BY ALL ORDER BY state, sale_year, fam_relation, fam_trigger, fam_vehicle
"))
assert_rows(audit_taxonomy, 100, "audit_taxonomy_by_year_state")
write_csv_strict(audit_taxonomy,
                 path(tables_out_dir, paste0("audit_taxonomy_by_year_state", sfx, ".csv")))

# New-column guards: no all-NULL column; unmapped document types reported
newcols <- c("doc_type", "doc_grp", "rel_code", "own_code", "occ_code", "title_co",
             "mortgage_purch", "partial_int", "mail_eq_situs", "rec_raw", "given_differ",
             "gen_suffix", "tr_irrev", "tr_resid", "tr_child", "tr_post",
             "trust_fam_lo", "trust_fam_hi", "fam_relation", "fam_relation_nm", "new_buyer",
             "fam_trigger", "fam_vehicle", "trust_chain", "trust_chain_named",
             "trust_chain_gap_days", "s_trust", "b_trust", "deed_cat", "deed_subcat",
             "fam_route")
fill_sql <- paste(sprintf("AVG(CASE WHEN %s IS NOT NULL THEN 1.0 ELSE 0 END) AS %s",
                          newcols, newcols), collapse = ", ")
fill_new <- dbGetQuery(con, glue("SELECT {fill_sql} FROM {ev}"))
print(t(fill_new))
stopifnot(all(unlist(fill_new) > 0))
unmapped <- dbGetQuery(con, glue("
  SELECT doc_type, COUNT(*) AS n FROM {ev}
  WHERE doc_type IS NOT NULL AND doc_grp IS NULL GROUP BY 1 ORDER BY 2 DESC LIMIT 10"))
cat("Unmapped document types (top 10):\n"); print(unmapped)

message("Class distribution (full window):")
print(
  audit_class |>
    group_by(class) |>
    summarise(n = sum(n), .groups = "drop") |>
    arrange(desc(n)),
  n = 20
)

cat("Finished 01_build_panel at ", format(Sys.time()), " | total ",
    round(as.numeric(difftime(Sys.time(), t_start, units = "mins")), 1), " min\n", sep = "")
