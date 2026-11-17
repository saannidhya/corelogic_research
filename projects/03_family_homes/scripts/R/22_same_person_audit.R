# ============================================================
# Script  : 22_same_person_audit.R
# Purpose : How did same-person and entity deeds get into the class-based family
#           proxy? Splits `owner_retained` and `unrelated_entity` deeds by mechanism.
# Name    : Saani Rawat
# Created : 11/12/2026
# Inputs  : data/derived/03_family_homes/events.parquet (panel only);
#           CoreLogic Owner Transfer names for CA, TX, OH via the shared loader
#           (register_corelogic_parquet), read only.
# Outputs : _outputs/tables/same_person_panel_split.csv   (all states, panel columns)
#           _outputs/tables/same_person_owner_retained_split.csv
#           _outputs/tables/same_person_entity_split.csv
#           data/derived/03_family_homes/name_samples/*.csv  (git-ignored; real names)
# Notes   : Read-only audit. Name rules are the nm_* macros of 01_build_panel.R,
#           extracted from that file so there is one definition.
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

audit_states <- c("CA", "TX", "OH")
audit_years  <- 2017L:2019L
panel_sql    <- sql_quote_path(events_panel_path)

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

# ---- 1. Panel-only split (all states) ----
panel_split <- dbGetQuery(con, glue("
  SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other states' END AS region,
         sale_year, class, coalesce(fam_relation_nm, '(null)') AS fam_relation_nm, COUNT(*) AS n
  FROM read_parquet('{panel_sql}')
  WHERE class IN {family_classes_sql} AND sale_year BETWEEN 2007 AND 2023
  GROUP BY ALL ORDER BY ALL"))
write_csv_strict(panel_split, path(tables_out_dir, "same_person_panel_split.csv"))

# ---- 2. nm_* macros from the build script (single definition) ----
bp <- readLines(path(r_dir, "01_build_panel.R"), warn = FALSE)
i0 <- grep("^nm_macros <- c\\(", bp)
i1 <- grep("^for \\(m in nm_macros\\)", bp)
stopifnot(length(i0) == 1, length(i1) == 1, i1 > i0)
eval(parse(text = bp[i0:(i1 - 1)]))
for (m in nm_macros) dbExecute(con, m)

# ---- 3. Panel rows for the audit states, joined to raw names ----
register_corelogic_parquet(con, "ot", states = audit_states, view_name = "raw_ot")
st_sql <- paste0("'", audit_states, "'", collapse = ",")
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE pan AS
  SELECT clip, sale_raw, state, sale_year, class, fam_relation_nm, given_differ, gen_suffix,
         same_surname, corp_buyer
  FROM read_parquet('{panel_sql}')
  WHERE class IN {family_classes_sql} AND state IN ({st_sql})
    AND sale_year BETWEEN {min(audit_years)} AND {max(audit_years)}
    AND fam_relation_nm IN ('owner_retained','unrelated_entity')"))
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE raw_nm AS
  SELECT TRY_CAST(clip AS BIGINT) AS clip, TRY_CAST(sale_derived_date AS BIGINT) AS sale_raw,
         upper(coalesce(buyer_1_full_name, '')) AS b1, upper(coalesce(buyer_2_full_name, '')) AS b2,
         upper(coalesce(seller_1_full_name, '')) AS s1, upper(coalesce(seller_2_full_name, '')) AS s2
  FROM raw_ot
  WHERE residential_indicator = 'Y' AND TRY_CAST(interfamily_related_indicator AS INTEGER) = 1
    AND TRY_CAST(clip AS BIGINT) IN (SELECT clip FROM pan)")
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE aud AS
  SELECT p.*, r.b1, r.b2, r.s1, r.s2, r.n_raw FROM pan p
  LEFT JOIN (SELECT clip, sale_raw, b1, b2, s1, s2, n_raw FROM (
      SELECT *, ROW_NUMBER() OVER (PARTITION BY clip, sale_raw ORDER BY b1, b2, s1, s2) AS rk,
             COUNT(*) OVER (PARTITION BY clip, sale_raw) AS n_raw
      FROM raw_nm) WHERE rk = 1) r
  ON r.clip = p.clip AND r.sale_raw = p.sale_raw")
cov <- dbGetQuery(con, "SELECT COUNT(*) AS n, SUM((b1 IS NOT NULL)::INT) AS n_matched, SUM((n_raw > 1)::INT) AS n_multi FROM aud")
message("Audit rows: ", cov$n, " matched to raw names: ", cov$n_matched, " with >1 raw row: ", cov$n_multi)

# ---- 4. owner_retained: mechanism ----
# Hierarchy (first match): (a) exact primary buyer = seller 1 [retitle rule hole];
# (a2) exact buyer 2 = seller 1; (b) exact overlap with seller 2; (d) class family_other
# (cross-surname rule: different surname strings); (c1) same name tokens in any order;
# (c2) same first given name and same surname word; (c3) other given-name matches.
dbExecute(con, "
  CREATE OR REPLACE TEMP TABLE orsplit AS
  SELECT *,
    CASE WHEN b1 IS NULL THEN 'z_no_raw_match'
         WHEN b1 <> '' AND b1 = s1 AND n_raw = 1                 THEN 'a_exact_buyer1_eq_seller1'
         WHEN b2 <> '' AND b2 = s1 AND n_raw = 1                 THEN 'a2_exact_buyer2_eq_seller1'
         WHEN (b1 <> '' AND b1 = s1) OR (b2 <> '' AND b2 = s1)   THEN 'a3_exact_match_but_duplicate_raw_rows'
         WHEN s2 <> '' AND (b1 = s2 OR b2 = s2)                  THEN 'b_exact_overlap_with_seller2'
         WHEN class = 'family_other' AND (nm_same_person(s1, b1) OR nm_same_person(s1, b2))
                                                                 THEN 'd1_cross_surname_same_person_rule'
         WHEN class = 'family_other'                             THEN 'd2_cross_surname_shared_token'
         WHEN (len(nm_tokset(s1)) >= 2 AND (nm_tokset(s1) = nm_tokset(b1) OR nm_tokset(s1) = nm_tokset(b2)))
                                                                 THEN 'c1_same_tokens_variant'
         WHEN (length(nm_g1(s1)) >= 1 AND ((nm_g1(s1) = nm_g1(b1) AND nm_skey(s1) = nm_skey(b1))
                                        OR (nm_g1(s1) = nm_g1(b2) AND nm_skey(s1) = nm_skey(b2))))
                                                                 THEN 'c2_same_first_given_and_surname'
         -- c3 split: (c3a) buyer's first given name is a later given name of the seller
         -- (joint seller, e.g. 'JOE A & SUSAN', buyer 'SUSAN': a co-owner leaves title);
         -- (c3b) match only because a single-letter token equals the first letter of the
         -- buyer's first name (middle initial 'A' vs 'ALEXANDER': likely a different person);
         -- (c3c) prefix or other match (ROB / ROBERT)
         WHEN length(nm_g1(b1)) >= 3 AND list_contains(nm_giv(s1)[2:], nm_g1(b1))
                                                                 THEN 'c3a_joint_seller_buyer_is_co_owner'
         WHEN len(list_filter(nm_giv(s1), z -> length(z) = 1 AND z = nm_g1(b1)[1])) > 0
           OR len(list_filter(nm_giv(b1), z -> length(z) = 1 AND z = nm_g1(s1)[1])) > 0
           OR length(nm_g1(b1)) = 1 OR length(nm_g1(s1)) = 1
                                                                 THEN 'c3b_initial_only_match'
         ELSE 'c3c_prefix_or_other_given_match' END AS mech,
    (b2 IS NOT NULL AND b2 <> '') AS two_buyers,
    -- seller kept on title AND a second, different person is added (parent adds child,
    -- spouse added at refinance): buyer 1 matches the seller, buyer 2 does not
    (b2 <> '' AND nm_gmatch(nm_giv(b1), nm_giv(s1)) AND NOT nm_gmatch(nm_giv(b2), nm_giv(s1))) AS co_owner_added
  FROM aud WHERE fam_relation_nm = 'owner_retained'")
or_tab <- dbGetQuery(con, "
  SELECT state, class, mech, COUNT(*) n, SUM(two_buyers::INT) n_two_buyers,
         SUM((gen_suffix = 1)::INT) n_suffix, SUM((n_raw > 1)::INT) n_dup_raw,
         SUM(co_owner_added::INT) n_co_owner_added
  FROM orsplit GROUP BY ALL ORDER BY state, class, mech")
write_csv_strict(or_tab, path(tables_out_dir, "same_person_owner_retained_split.csv"))

# ---- 5. unrelated_entity: side and kind ----
ent_re <- list(
  lender_gov = r"(\b(BANK|SHERIFF|COUNTY|OWNER RECORD)\b)",
  estate     = r"(\bESTATE\b)",
  trust_ft   = r"(\b(TRUST|FT)\b)",
  company    = r"(\b(LLC|INC|CORP|CO|LP|LTD|ENTERPRISES?|ASSOC[A-Z]*)\b)",
  property_investor = r"(\b(HOLDINGS?|HLDGS|PROPERT[A-Z]*|HOMES?|BUILDERS?|INVESTMENTS?)\b)")
kind_case <- function(col) {
  paste0("CASE ", paste(sprintf("WHEN regexp_matches(%s, '%s') THEN '%s'", col, unlist(ent_re), names(ent_re)),
                        collapse = " "), " ELSE 'no_keyword' END")
}
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE entsplit AS
  SELECT *,
    CASE WHEN b1 IS NULL THEN 'z_no_raw_match'
         WHEN class = 'family_other' AND nm_ent(s1) AND (nm_ent(b1) OR nm_ent(b2)) THEN 'both_sides'
         WHEN class = 'family_other' AND nm_ent(s1)                               THEN 'seller_entity'
         WHEN class = 'family_other' AND (nm_ent(b1) OR nm_ent(b2))              THEN 'buyer_entity_keyword'
         WHEN corp_buyer = 1                                                     THEN 'buyer_vendor_corporate_flag'
         ELSE 'other' END AS side,
    CASE WHEN nm_ent(s1) THEN {kind_case('s1')}
         WHEN nm_ent(b1) THEN {kind_case('b1')}
         WHEN nm_ent(b2) THEN {kind_case('b2')}
         ELSE 'no_keyword_in_name' END AS kind
  FROM aud WHERE fam_relation_nm = 'unrelated_entity'"))
ent_tab <- dbGetQuery(con, "SELECT state, class, side, kind, COUNT(*) n FROM entsplit GROUP BY ALL ORDER BY state, side, kind")
write_csv_strict(ent_tab, path(tables_out_dir, "same_person_entity_split.csv"))

# ---- 6. Name samples (real names; git-ignored data folder) ----
samp_dir <- dir_create(path(data_dir, "name_samples"))
set.seed(20261112)
for (tb in c("orsplit", "entsplit")) {
  key <- if (tb == "orsplit") "mech" else "side"
  smp <- dbGetQuery(con, glue("
    SELECT * FROM (SELECT state, sale_year, class, {key} AS cat, b1, b2, s1, s2,
           ROW_NUMBER() OVER (PARTITION BY {key} ORDER BY hash(clip, sale_raw, 20261112)) AS rk
           FROM {tb}) WHERE rk <= 30 ORDER BY cat, rk"))
  write_csv_strict(smp, path(samp_dir, paste0(tb, "_samples.csv")))
}
message("Done.")
