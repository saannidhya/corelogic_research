#======================================================================#
# Purpose : Audit the meaning and composition of the interfamily flag
# Name    : Saani Rawat
# Created : 10/31/2026
# Log     : 10/31/2026: add read-only source audit and candidate definitions.
# Inputs  : Owner Transfer via shared loader; existing events.parquet
# Outputs : Aggregate CSVs under _outputs/tables/interfamily_audit_2026-10-31
#           No names, parcel IDs, or replacement analytical panel exported.
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

# Preparing the audit ----
audit_dir <- path(tables_out_dir, "interfamily_audit_2026-10-31")
dir_create(audit_dir)
con <- open_corelogic_duckdb(memory_limit = "3GB", threads = 2)
dbExecute(con, "SET preserve_insertion_order=false")
dbExecute(con, glue("SET temp_directory='{sql_quote_path(path(data_dir, 'duckdb_tmp'))}'"))
existing_path <- sql_quote_path(path(data_dir, "events.parquet"))
state_names <- sort(dbGetQuery(con,glue("SELECT DISTINCT state FROM read_parquet('{existing_path}')
  WHERE sale_year BETWEEN 2007 AND 2023"))$state)
requested_states <- Sys.getenv("FAM_AUDIT_STATES", "")
if (nzchar(requested_states)) state_names <- strsplit(requested_states, ",")[[1]]
stopifnot(all(grepl("^[A-Z]{2}$",state_names)))

# Keep the paper's exact current name rules and CASE priority. Added signals
# are diagnostics only. All-party overlap is not proof of unchanged ownership.
name_cols <- c(paste0("buyer_", 1:4, "_full_name"),
               paste0("seller_", 1:2, "_full_name"))
name_alias <- c(paste0("b", 1:4), "s1", "s2")
name_select <- paste0("upper(coalesce(CAST(", name_cols, " AS VARCHAR),'')) AS ", name_alias,
                     collapse = ", ")
norm_select <- paste0("trim(regexp_replace(", name_alias,
                     ", '[^A-Z0-9]+', ' ', 'g')) AS ", name_alias, "n",
                     collapse = ", ")
overlap <- paste(unlist(lapply(paste0("b", 1:4, "n"), function(b) {
  paste0("(", b, " <> '' AND ", b, " = ", c("s1n", "s2n"), ")")
})), collapse = " OR ")
all_names <- paste0("concat_ws(' | ',", paste(name_alias, collapse = ","), ")")
entity_pattern <- "(^|[^A-Z])(LLC|L L C|INC|CORP|CORPORATION|LP|LLP|LTD|COMPANY|BANK|ASSOCIATION|AUTHORITY|SECRETARY|DEPARTMENT)([^A-Z]|$)"
trust_pattern <- "(^|[^A-Z])(TRUST|TRUSTEE|TRUSTEES|TR|TTEE|TTEES|TRST|TRUSTE)([^A-Z]|$)"
estate_pattern <- "(^|[^A-Z])(EXECUTOR|EXECUTRIX|PROBATE|ADMINISTRATOR|ADMINISTRATRIX)([^A-Z]|$)|ESTATE OF"
corp_any <- paste0("coalesce(CAST(buyer_", 1:4, "_corporate_indicator AS VARCHAR),'')='Y'", collapse=" OR ")

profile_sql <- "count(*) AS n,
 sum(CASE WHEN amt IS NULL THEN 1 ELSE 0 END) AS price_missing,
 sum(CASE WHEN amt=0 THEN 1 ELSE 0 END) AS price_zero,
 sum(CASE WHEN amt>0 AND amt<10000 THEN 1 ELSE 0 END) AS price_positive_lt10k,
 sum(CASE WHEN amt>=10000 THEN 1 ELSE 0 END) AS price_ge10k,
 sum(CASE WHEN pcat='A' THEN 1 ELSE 0 END) AS category_a,
 sum(CASE WHEN corp_buyer THEN 1 ELSE 0 END) AS corporate_buyer,
 sum(CASE WHEN entity_any THEN 1 ELSE 0 END) AS entity_any,
 sum(CASE WHEN trust_kw THEN 1 ELSE 0 END) AS original_trust,
 sum(CASE WHEN trust_expanded THEN 1 ELSE 0 END) AS expanded_trust,
 sum(CASE WHEN same_surname THEN 1 ELSE 0 END) AS same_surname,
 sum(CASE WHEN same_person THEN 1 ELSE 0 END) AS original_same_person,
 sum(CASE WHEN any_name_overlap THEN 1 ELSE 0 END) AS any_name_overlap,
 sum(CASE WHEN complete_person_fields THEN 1 ELSE 0 END) AS complete_person_fields,
 sum(CASE WHEN distress THEN 1 ELSE 0 END) AS distress,
 sum(CASE WHEN estate_kw THEN 1 ELSE 0 END) AS original_estate,
 sum(CASE WHEN estate_strict THEN 1 ELSE 0 END) AS strict_estate,
 sum(CASE WHEN executive_kw THEN 1 ELSE 0 END) AS executive_keyword"

save_query <- function(sql, label, st) {
  result <- dbGetQuery(con, sql)
  result$state <- rep(st,nrow(result))
  write_csv(result, path(audit_dir, paste0(label, "_", st, ".csv")))
  invisible(result)
}

# Auditing one state at a time ----
for (st in state_names) {
  message(format(Sys.time()), " | auditing ", st)
  register_corelogic_parquet(con, states = st, view_name = "source_ot")
  # Global deduplication can assign a parcel-date seen in multiple source
  # states to only one state. Anchor this diagnostic to that selected sample.
  existing_path <- sql_quote_path(path(data_dir, "events.parquet"))
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE panel_state AS
    SELECT clip,sale_raw,class,interfam,amt FROM read_parquet('{existing_path}')
    WHERE state='{st}' AND sale_year BETWEEN 2007 AND 2023"))
  dbExecute(con, "DROP TABLE IF EXISTS audit_events")
  dbExecute(con, glue("CREATE TEMP TABLE audit_events AS
    WITH slim AS (
      SELECT TRY_CAST(clip AS BIGINT) AS clip,
        TRY_CAST(sale_derived_date AS BIGINT) AS sale_raw,
        CAST(floor(TRY_CAST(sale_derived_date AS BIGINT)/10000) AS INTEGER) AS sale_year,
        primary_category_code AS pcat, deed_category_type_code AS dtype,
        coalesce(sale_document_type_code,'(missing)') AS doc,
        TRY_CAST(interfamily_related_indicator AS INTEGER) AS interfam,
        TRY_CAST(sale_amount AS DOUBLE) AS amt,
        {name_select},
        upper(coalesce(CAST(buyer_1_last_name AS VARCHAR),'')) AS bl,
        upper(coalesce(CAST(seller_1_last_name AS VARCHAR),'')) AS sl,
        upper(coalesce(CAST(buyer_1_first_name_and_middle_initial AS VARCHAR),'')) AS bf,
        upper(coalesce(CAST(seller_1_first_name AS VARCHAR),'')) AS sf,
        coalesce(CAST(buyer_1_corporate_indicator AS VARCHAR),'')='Y' AS corp_buyer,
        ({corp_any}) AS corp_any,
        (coalesce(TRY_CAST(foreclosure_reo_indicator AS INTEGER),0)=1 OR
         coalesce(TRY_CAST(foreclosure_reo_sale_indicator AS INTEGER),0)=1) AS distress,
        coalesce(CAST(buyer_relationship_type_code AS VARCHAR),'(missing)') AS buyer_relationship,
        coalesce(CAST(buyer_ownership_rights_code AS VARCHAR),'(missing)') AS buyer_rights
      FROM source_ot
      WHERE residential_indicator='Y' AND TRY_CAST(clip AS BIGINT) IS NOT NULL
        AND TRY_CAST(sale_derived_date AS BIGINT) BETWEEN 20070101 AND 20231231
        AND CAST(floor(TRY_CAST(sale_derived_date AS BIGINT)/100)%100 AS INTEGER) BETWEEN 1 AND 12
        AND EXISTS (SELECT 1 FROM panel_state p WHERE p.clip=TRY_CAST(source_ot.clip AS BIGINT)
          AND p.sale_raw=TRY_CAST(source_ot.sale_derived_date AS BIGINT))
    ), names AS (SELECT *, {norm_select} FROM slim),
    flags AS (SELECT *,
      (b1 LIKE '%TRUST%' OR s1 LIKE '%TRUST%') AS trust_kw,
      (b1 LIKE '%ESTATE OF%' OR s1 LIKE '%ESTATE OF%' OR
       b1 LIKE '%EXECUT%' OR s1 LIKE '%EXECUT%' OR
       b1 LIKE '%PROBATE%' OR s1 LIKE '%PROBATE%' OR
       b1 LIKE '%ADMINISTRATOR%' OR s1 LIKE '%ADMINISTRATOR%' OR
       b1 LIKE '%ADMINISTRATRIX%' OR s1 LIKE '%ADMINISTRATRIX%') AS estate_kw,
      (bl<>'' AND bl=sl) AS same_surname,
      ((b1<>'' AND b1=s1) OR (b2<>'' AND b2=s1)) AS same_person,
      ({overlap}) AS any_name_overlap,
      (corp_any OR regexp_matches({all_names},'{entity_pattern}')) AS entity_any,
      regexp_matches({all_names},'{trust_pattern}') AS trust_expanded,
      regexp_matches(concat_ws(' | ',b1,s1),'{estate_pattern}') AS estate_strict,
      (b1 LIKE '%EXECUTIVE%' OR s1 LIKE '%EXECUTIVE%') AS executive_kw,
      (bl<>'' AND sl<>'' AND bf<>'' AND sf<>'') AS complete_person_fields
    FROM names),
    classified AS (SELECT *, CASE
      WHEN interfam=1 AND trust_kw THEN 'family_trust'
      WHEN interfam=1 AND same_person THEN 'family_retitle'
      WHEN interfam=1 AND same_surname THEN 'family_person'
      WHEN interfam=1 AND estate_kw THEN 'family_estate'
      WHEN interfam=1 THEN 'family_other'
      WHEN pcat='A' THEN 'market_sale'
      WHEN estate_kw THEN 'estate_noninterfam'
      ELSE 'other_nonarms' END AS class
    FROM flags),
    ranked AS (SELECT *, row_number() OVER (PARTITION BY clip,sale_raw
      ORDER BY CASE WHEN pcat='A' THEN 0 ELSE 1 END, amt DESC NULLS LAST,
      class ASC, dtype ASC NULLS LAST,b1 ASC,s1 ASC) AS rn
      FROM classified)
    SELECT * EXCLUDE(b1,b2,b3,b4,s1,s2,b1n,b2n,b3n,b4n,s1n,s2n,bl,sl,bf,sf,rn),
      (class IN ('family_person','family_other','family_estate')) AS broad,
      (NOT entity_any AND NOT trust_expanded AND NOT trust_kw
       AND NOT any_name_overlap AND NOT distress) AS person_screen,
      CASE WHEN amt IS NULL THEN 'missing' WHEN amt=0 THEN 'zero'
           WHEN amt<0 THEN 'negative' WHEN amt<10000 THEN 'positive_lt10k'
           ELSE 'ge10k' END AS price_group
    FROM ranked WHERE rn=1"))

  save_query(glue("SELECT class, interfam, {profile_sql} FROM audit_events GROUP BY class,interfam"), "class_profile", st)
  save_query(glue("SELECT doc,interfam,class,{profile_sql} FROM audit_events GROUP BY doc,interfam,class"), "document_profile", st)
  save_query("SELECT pcat,interfam,price_group,count(*) AS n FROM audit_events GROUP BY ALL", "category_price", st)
  save_query("SELECT interfam,trust_kw,same_person,same_surname,entity_any,any_name_overlap,complete_person_fields,count(*) AS n FROM audit_events GROUP BY ALL", "flag_features", st)
  save_query("SELECT interfam,buyer_relationship,buyer_rights,count(*) AS n FROM audit_events GROUP BY ALL", "ownership_codes", st)
  save_query("SELECT sale_year,interfam,class,count(*) AS n FROM audit_events GROUP BY ALL", "year_class", st)

  # Mutually exclusive accounting of the current broad family aggregate.
  save_query("SELECT class, CASE
    WHEN entity_any THEN '1_entity_signal'
    WHEN trust_kw OR trust_expanded THEN '2_trust_signal'
    WHEN any_name_overlap THEN '3_continuing_named_party'
    WHEN distress THEN '4_reo_signal'
    WHEN NOT complete_person_fields THEN '5_incomplete_person_fields'
    ELSE '6_named_person_candidate' END AS audit_bucket,
    count(*) AS n FROM audit_events WHERE broad GROUP BY ALL", "broad_buckets", st)

  definitions <- c(
    all_flagged = "interfam=1",
    current_broad = "broad",
    current_same_surname = "class='family_person'",
    broad_screened = "broad AND person_screen",
    broad_screened_complete = "broad AND person_screen AND complete_person_fields",
    surname_screened = "class='family_person' AND person_screen",
    surname_screened_complete = "class='family_person' AND person_screen AND complete_person_fields",
    surname_screened_zero_or_missing = "class='family_person' AND person_screen AND (amt=0 OR amt IS NULL)",
    other_screened_complete = "class='family_other' AND person_screen AND complete_person_fields",
    estate_screened_complete = "class='family_estate' AND person_screen AND complete_person_fields",
    unflagged_surname_screened = "interfam=0 AND same_surname AND person_screen",
    flagged_gift_distribution_screened = "interfam=1 AND doc IN ('GF','DI') AND person_screen",
    flagged_probate_representative_screened = "interfam=1 AND doc IN ('EX','AD','PR') AND person_screen")
  candidates <- bind_rows(lapply(names(definitions), function(label) {
    dbGetQuery(con, glue("SELECT '{label}' AS definition, sale_year,count(*) AS n
      FROM audit_events WHERE {definitions[[label]]} GROUP BY sale_year"))
  }))
  candidates$state <- rep(st,nrow(candidates))
  write_csv(candidates, path(audit_dir, paste0("candidate_counts_", st, ".csv")))
  # Check one row per parcel/date and exact membership against the live panel.
  check <- dbGetQuery(con, glue("SELECT count(*) AS reconstructed_n,
    sum(CASE WHEN p.clip IS NULL THEN 1 ELSE 0 END) AS unmatched_key,
    sum(CASE WHEN p.class IS DISTINCT FROM a.class OR p.interfam IS DISTINCT FROM a.interfam
       OR p.amt IS DISTINCT FROM a.amt THEN 1 ELSE 0 END) AS class_flag_price_mismatch
    FROM audit_events a LEFT JOIN
    (SELECT clip,sale_raw,class,interfam,amt FROM read_parquet('{existing_path}')
     WHERE state='{st}' AND sale_year BETWEEN 2007 AND 2023) p USING(clip,sale_raw)"))
  check$state <- st
  check$existing_n <- dbGetQuery(con, glue("SELECT count(*) AS n FROM read_parquet('{existing_path}')
    WHERE state='{st}' AND sale_year BETWEEN 2007 AND 2023"))$n
  write_csv(check, path(audit_dir, paste0("reconciliation_", st, ".csv")))
  stopifnot(check$reconstructed_n > 0, check$unmatched_key == 0,
            check$class_flag_price_mismatch == 0, check$existing_n == check$reconstructed_n)
  message("Verified ", format(check$reconstructed_n, big.mark=","), " events in ", st)
}

# Combining aggregate output ----
labels <- c("class_profile","document_profile","category_price","flag_features",
            "ownership_codes","year_class","broad_buckets","candidate_counts","reconciliation")
for (label in labels) {
  files <- path(audit_dir, paste0(label, "_", state_names, ".csv"))
  pieces <- lapply(files, function(f) read_csv(f, na=character(),show_col_types=FALSE))
  nonempty <- Filter(function(x) nrow(x)>0,pieces)
  result <- if (length(nonempty)) bind_rows(nonempty) else pieces[[1]]
  write_csv(result, path(audit_dir,paste0(label,"_combined.csv")))
}
writeLines(c(paste("Completed:", Sys.time()), paste("States:",paste(state_names,collapse=",")),
             "Source through register_corelogic_parquet; residential, 2007-2023, deduplicated parcel-date.",
             "Current market definition: category A, no price floor. Name/deed screens are diagnostics, not validated kinship."),
           path(audit_dir,"run_manifest.txt"))
dbDisconnect(con, shutdown=TRUE)
