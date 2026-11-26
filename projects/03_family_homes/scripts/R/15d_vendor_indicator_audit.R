#======================================================================#
# Purpose : Compare vendor indicators on source residential records
# Name    : Saani Rawat
# Created : 10/31/2026
# Log     : 10/31/2026: add source-level flag/consideration/finance profiles.
#           11/01/2026: correct conditional totals; add aggregate reuse.
# Inputs  : Shared Owner Transfer loader, 2007-2023 residential records
# Outputs : vendor_indicator_source_records.csv (NOT deduplicated events)
#======================================================================#
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
audit_dir <- path(tables_out_dir,"interfamily_audit_2026-10-31")
con <- open_corelogic_duckdb(memory_limit="1GB",threads=1)
register_corelogic_parquet(con,view_name="source_ot")
cached_file <- path(audit_dir,"vendor_indicator_source_records.csv")
profile <- if (Sys.getenv("FAM_INDICATOR_REUSE", "0")=="1") {
  read_csv(cached_file,show_col_types=FALSE,na=character())
} else dbGetQuery(con,"SELECT state,
  TRY_CAST(interfamily_related_indicator AS INTEGER) AS interfam,
  coalesce(CAST(primary_category_code AS VARCHAR),'(missing)') AS pcat,
  coalesce(CAST(sale_type_code AS VARCHAR),'(missing)') AS sale_type,
  coalesce(CAST(partial_interest_indicator AS VARCHAR),'(missing)') AS partial_indicator,
  TRY_CAST(cash_purchase_indicator AS INTEGER) AS cash_purchase,
  TRY_CAST(mortgage_purchase_indicator AS INTEGER) AS mortgage_purchase,
  TRY_CAST(investor_purchase_indicator AS INTEGER) AS investor_purchase,
  CASE WHEN TRY_CAST(sale_amount AS DOUBLE) IS NULL THEN 'missing'
    WHEN TRY_CAST(sale_amount AS DOUBLE)=0 THEN 'zero'
    WHEN TRY_CAST(sale_amount AS DOUBLE)<0 THEN 'negative'
    WHEN TRY_CAST(sale_amount AS DOUBLE)<10000 THEN 'positive_lt10k'
    ELSE 'ge10k' END AS price_group,
  coalesce(CAST(ownership_transfer_percentage AS VARCHAR),'(missing)') AS transfer_percent,
  count(*) AS n
  FROM source_ot WHERE residential_indicator='Y' AND TRY_CAST(clip AS BIGINT) IS NOT NULL
    AND TRY_CAST(sale_derived_date AS BIGINT) BETWEEN 20070101 AND 20231231
    AND CAST(floor(TRY_CAST(sale_derived_date AS BIGINT)/100)%100 AS INTEGER) BETWEEN 1 AND 12
  GROUP BY ALL")
stopifnot(sum(profile$n)>1e8)
write_csv(profile,path(audit_dir,"vendor_indicator_source_records.csv"))
summary <- profile %>% group_by(interfam) %>% summarize(
  cash=sum(n[cash_purchase==1],na.rm=TRUE),
  mortgage=sum(n[mortgage_purchase==1],na.rm=TRUE),
  investor=sum(n[investor_purchase==1],na.rm=TRUE),
  partial_y=sum(n[toupper(partial_indicator)=='Y']),
  partial_missing=sum(n[partial_indicator=='(missing)']),n=sum(n),.groups="drop")
stopifnot(!anyNA(summary),all(summary$cash<=summary$n),all(summary$mortgage<=summary$n))
write_csv(summary,path(audit_dir,"vendor_indicator_source_summary.csv"))
print(summary)
print(profile %>% group_by(sale_type,interfam) %>% summarize(n=sum(n),.groups="drop"),n=40)
dbDisconnect(con,shutdown=TRUE)
