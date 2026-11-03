#======================================================================#
# Purpose : Find surname evidence missed by the primary-buyer rule
# Name    : Saani Rawat
# Created : 10/31/2026
# Log     : 10/31/2026: audit secondary buyer surnames in family_other.
# Inputs  : Shared Owner Transfer loader; existing panel family_other keys
# Outputs : Aggregate secondary_surname CSVs only; no names or keys exported
#======================================================================#
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
audit_dir <- path(tables_out_dir,"interfamily_audit_2026-10-31")
con <- open_corelogic_duckdb(memory_limit="2GB",threads=1)
dbExecute(con,"SET preserve_insertion_order=false")
panel_file <- sql_quote_path(path(data_dir,"events.parquet"))
states <- dbGetQuery(con,glue("SELECT DISTINCT state FROM read_parquet('{panel_file}')
 WHERE sale_year BETWEEN 2007 AND 2023 AND class='family_other'"))$state
results <- list()
for (st in sort(states)) {
  message("Secondary surnames: ",st)
  register_corelogic_parquet(con,states=st,view_name="source_ot")
  dbExecute(con,glue("CREATE OR REPLACE TEMP TABLE targets AS
    SELECT clip,sale_raw,amt,pcat,dtype FROM read_parquet('{panel_file}')
    WHERE state='{st}' AND sale_year BETWEEN 2007 AND 2023 AND class='family_other'"))
  n_target <- dbGetQuery(con,"SELECT count(*) AS n FROM targets")$n
  result <- dbGetQuery(con,"WITH source_names AS (
    SELECT p.clip,p.sale_raw,
      upper(coalesce(CAST(buyer_1_full_name AS VARCHAR),'')) AS b1,
      upper(coalesce(CAST(buyer_2_full_name AS VARCHAR),'')) AS b2,
      upper(coalesce(CAST(seller_1_full_name AS VARCHAR),'')) AS s1,
      upper(coalesce(CAST(buyer_1_last_name AS VARCHAR),'')) AS bl1,
      upper(coalesce(CAST(buyer_2_last_name AS VARCHAR),'')) AS bl2,
      upper(coalesce(CAST(buyer_3_last_name AS VARCHAR),'')) AS bl3,
      upper(coalesce(CAST(buyer_4_last_name AS VARCHAR),'')) AS bl4,
      upper(coalesce(CAST(seller_1_last_name AS VARCHAR),'')) AS sl
    FROM source_ot s JOIN targets p
      ON TRY_CAST(s.clip AS BIGINT)=p.clip AND TRY_CAST(s.sale_derived_date AS BIGINT)=p.sale_raw
      AND TRY_CAST(s.sale_amount AS DOUBLE) IS NOT DISTINCT FROM p.amt
      AND s.primary_category_code IS NOT DISTINCT FROM p.pcat
      AND s.deed_category_type_code IS NOT DISTINCT FROM p.dtype
    WHERE s.residential_indicator='Y' AND TRY_CAST(s.interfamily_related_indicator AS INTEGER)=1
  ), eligible AS (
    SELECT * FROM source_names WHERE
      NOT (b1 LIKE '%TRUST%' OR s1 LIKE '%TRUST%')
      AND NOT ((b1<>'' AND b1=s1) OR (b2<>'' AND b2=s1))
      AND NOT (bl1<>'' AND bl1=sl)
      AND NOT (b1 LIKE '%ESTATE OF%' OR s1 LIKE '%ESTATE OF%' OR
       b1 LIKE '%EXECUT%' OR s1 LIKE '%EXECUT%' OR
       b1 LIKE '%PROBATE%' OR s1 LIKE '%PROBATE%' OR
       b1 LIKE '%ADMINISTRATOR%' OR s1 LIKE '%ADMINISTRATOR%' OR
       b1 LIKE '%ADMINISTRATRIX%' OR s1 LIKE '%ADMINISTRATRIX%')
  ), ranked AS (
    SELECT *,row_number() OVER(PARTITION BY clip,sale_raw ORDER BY b1,s1) AS rn
    FROM eligible
  ) SELECT
    (bl1<>'') AS primary_buyer_surname_present,(sl<>'') AS seller_surname_present,
    (sl<>'' AND (sl=bl2 OR sl=bl3 OR sl=bl4)) AS secondary_buyer_surname_match,
    count(*) AS n FROM ranked WHERE rn=1 GROUP BY ALL")
  stopifnot(sum(result$n)==n_target)
  result$state <- st
  results[[st]] <- result
  write_csv(result,path(audit_dir,paste0("secondary_surname_",st,".csv")))
}
combined <- bind_rows(results)
write_csv(combined,path(audit_dir,"secondary_surname_combined.csv"))
print(combined %>% group_by(primary_buyer_surname_present,seller_surname_present,
 secondary_buyer_surname_match) %>% summarize(n=sum(n),.groups="drop"))
dbDisconnect(con,shutdown=TRUE)
