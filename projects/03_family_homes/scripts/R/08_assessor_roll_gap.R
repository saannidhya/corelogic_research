#======================================================================#
# Purpose : Match deeds in Los Angeles and San Francisco counties to the
#           county assessment rolls and measure the gap between a home's
#           assessed value and its market value, for family transfers and
#           for market sales. The rolls show what the assessor did after
#           each deed: a market sale is reassessed to its price, and a
#           family transfer that kept the old assessment is not.
# Name    : Saani Rawat
# Created : 11/18/2026
# Log     : 11/18/2026: first version (crosswalk, timing check on market
#                       sales, gap by route).
# Inputs  : data/derived/03_family_homes/events.parquet
#           data/corelogic_extracts/by_state/ot/state=CA  (parcel numbers on deeds)
#           data/external/ca_assessor_rolls/              (01f_ca_assessor_rolls.R)
# Outputs : data/derived/03_family_homes/ca_deed_roll.parquet    one row per matched deed
#           _outputs/tables/roll_match_quality.csv
#           _outputs/tables/roll_timing_check.csv
#           _outputs/tables/roll_gap_by_route.csv
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

#============================================#
# Settings ----
#============================================#

counties <- tribble(
  ~county,          ~fips5,
  "los_angeles",    "06037",
  "san_francisco",  "06075"
)
deed_years <- 2009:2022      # rolls from the year before the deed to three years after
min_price  <- 10000          # a recorded price below this is not treated as a sale price
roll_root  <- normalizePath(here("data", "external", "ca_assessor_rolls"), winslash = "/")
fips_sql   <- paste0("('", paste(counties$fips5, collapse = "','"), "')")

con <- open_corelogic_duckdb(memory_limit = "20GB")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

#============================================#
# Importing the rolls ----
#============================================#

# one table for both counties: parcel number, roll year, assessed land plus improvements,
# base year (Los Angeles only), living area, a local area key and the homeowner exemption
dbExecute(con, glue("
  CREATE TEMP TABLE rolls AS
  SELECT '06037' AS fips5, ain AS apn, roll_year,
         coalesce(land_value, 0) + coalesce(improvement_value, 0) AS assessed,
         land_base_year AS base_year, NULLIF(sqft_main, 0) AS sqft, situs_zip5 AS area,
         (homeowners_exemption > 0)::INT AS hox, recording_date AS roll_date
  FROM read_parquet('{roll_root}/los_angeles/*/*.parquet', hive_partitioning = true)
  UNION ALL
  SELECT '06075', parcel_number, roll_year,
         coalesce(assessed_land_value, 0) + coalesce(assessed_improvement_value, 0),
         NULL, NULLIF(property_area, 0), assessor_neighborhood_code,
         (homeowner_exemption_value > 0)::INT, TRY_CAST(substr(current_sales_date, 1, 10) AS DATE)
  FROM read_parquet('{roll_root}/san_francisco/*/*.parquet', hive_partitioning = true)"))

#============================================#
# Crosswalk: deed parcel ID to county parcel number ----
#============================================#

# The deeds carry the county's parcel number. Keep letters and digits only, so that
# San Francisco's '0166 012' equals the roll's '0166012'. A clip can carry more than one
# number over 17 years; the most frequent is kept and the rest are counted.
ot_ca <- sub("state=\\*", "state=CA", ot_parquet_glob())
dbExecute(con, glue("
  CREATE TEMP TABLE deed_apn AS
  SELECT TRY_CAST(clip AS BIGINT) AS clip,
         lpad(regexp_replace(CAST(fips_code AS VARCHAR), '\\.0$', ''), 5, '0') AS fips5,
         regexp_replace(upper(regexp_replace(CAST(apn_parcel_number_unformatted AS VARCHAR), '\\.0$', '')),
                        '[^A-Z0-9]', '', 'g') AS apn,
         TRY_CAST(sale_derived_date AS BIGINT) AS sale_raw,
         (trim(coalesce(CAST(multi_or_split_parcel_code AS VARCHAR), '')) <> '')::INT AS multi_parcel
  FROM read_parquet('{sql_quote_path(ot_ca)}', union_by_name = true)
  WHERE residential_indicator = 'Y' AND TRY_CAST(clip AS BIGINT) IS NOT NULL"))
dbExecute(con, glue("
  CREATE TEMP TABLE xwalk AS
  SELECT clip, fips5, apn, n_apn FROM (
    SELECT clip, fips5, apn, COUNT(*) AS n, COUNT(*) OVER (PARTITION BY clip) AS n_apn,
           ROW_NUMBER() OVER (PARTITION BY clip ORDER BY COUNT(*) DESC, apn) AS rk
    FROM deed_apn WHERE fips5 IN {fips_sql} AND apn <> '' GROUP BY clip, fips5, apn
  ) WHERE rk = 1"))
xw <- dbGetQuery(con, "
  SELECT x.fips5, COUNT(*) AS clips, AVG((x.n_apn > 1)::INT) AS clips_with_2plus_numbers,
         AVG((r.apn IS NOT NULL)::INT) AS on_some_roll
  FROM xwalk x LEFT JOIN (SELECT DISTINCT fips5, apn FROM rolls) r USING (fips5, apn) GROUP BY 1 ORDER BY 1")
print(xw)
stopifnot(all(xw$on_some_roll > 0.8))

#============================================#
# Deeds with their assessed values before and after ----
#============================================#

# homes only (single-family and condominium), family transfers and market sales
dbExecute(con, glue("
  CREATE TEMP TABLE deeds AS
  SELECT e.clip, e.fips5, e.sale_raw, e.rec_raw, e.sale_year, e.sale_month, e.deed_cat, e.deed_subcat,
         e.doc_type, e.amt,
         CASE WHEN e.deed_cat = 'market_sale' THEN 'market_sale'
              WHEN e.fam_route IN ('lifetime','unknown') THEN 'family_lifetime'
              WHEN e.fam_route = 'trust' THEN 'family_trust'
              ELSE 'family_at_death' END AS route,
         (coalesce(e.doc_type, '') IN ('IT','IC'))::INT AS spouse_doc,
         x.apn, coalesce(m.multi_parcel, 0) AS multi_parcel
  FROM read_parquet('{sql_quote_path(events_panel_path)}') e
  LEFT JOIN xwalk x ON x.clip = e.clip
  LEFT JOIN (SELECT clip, sale_raw, MAX(multi_parcel) AS multi_parcel FROM deed_apn GROUP BY 1, 2) m
    ON m.clip = e.clip AND m.sale_raw = e.sale_raw
  WHERE e.fips5 IN {fips_sql} AND e.sale_year BETWEEN {min(deed_years)} AND {max(deed_years)}
    AND e.deed_cat IN ('family_transfer', 'market_sale')
    AND e.property_indicator_code_static IN ('10', '11')"))

# k = roll year minus deed year. A roll year's values are those of its 1 January lien date.
ks <- -1:3
k_cols <- paste(c(
  glue("MAX(CASE WHEN r.roll_year = d.sale_year + ({ks}) THEN r.assessed END) AS assessed_k{sub('-', 'm', ks)}"),
  glue("MAX(CASE WHEN r.roll_year = d.sale_year + ({ks}) THEN r.base_year END) AS base_k{sub('-', 'm', ks)}"),
  "MAX(CASE WHEN r.roll_year = d.sale_year - 1 THEN r.sqft END) AS sqft",
  "MAX(CASE WHEN r.roll_year = d.sale_year - 1 THEN r.area END) AS area",
  "MAX(CASE WHEN r.roll_year = d.sale_year - 1 THEN r.hox END) AS hox_before",
  "MAX(CASE WHEN r.roll_year = d.sale_year + 2 THEN r.hox END) AS hox_after",
  "MAX(CASE WHEN r.roll_year = d.sale_year + 2 THEN r.roll_date END) AS roll_date_after"), collapse = ",\n         ")
dbExecute(con, glue("
  CREATE TEMP TABLE deed_roll AS
  SELECT d.*, {k_cols}
  FROM deeds d LEFT JOIN rolls r
    ON r.fips5 = d.fips5 AND r.apn = d.apn AND r.roll_year BETWEEN d.sale_year - 1 AND d.sale_year + 3
  GROUP BY ALL"))

# |- match quality ----
match_quality <- dbGetQuery(con, "
  SELECT fips5, sale_year, COUNT(*) AS deeds,
         ROUND(AVG((apn IS NOT NULL)::INT), 3) AS has_parcel_number,
         ROUND(AVG((assessed_km1 > 0)::INT), 3) AS on_roll_before,
         ROUND(AVG((assessed_k2 > 0)::INT), 3) AS on_roll_two_years_after,
         ROUND(AVG((assessed_km1 > 0 AND assessed_k2 > 0)::INT), 3) AS both
  FROM deed_roll GROUP BY 1, 2 ORDER BY 1, 2") %>% as_tibble()
print(as.data.frame(match_quality))
write_csv_strict(match_quality, path(tables_out_dir, "roll_match_quality.csv"))
stopifnot(mean(match_quality$both) > 0.7)

#============================================#
# Timing check on market sales ----
#============================================#

# After a priced sale the assessor enrols the price. The roll year at which assessed value
# equals the price shows the lag between a deed and the roll, and validates the match.
timing <- dbGetQuery(con, glue("
  SELECT fips5, CASE WHEN sale_month <= 6 THEN 'Jan-Jun' ELSE 'Jul-Dec' END AS deed_half, COUNT(*) AS sales,
         ROUND(MEDIAN(assessed_km1 / amt), 3) AS assessed_over_price_km1,
         ROUND(MEDIAN(assessed_k0 / amt), 3) AS k0, ROUND(MEDIAN(assessed_k1 / amt), 3) AS k1,
         ROUND(MEDIAN(assessed_k2 / amt), 3) AS k2, ROUND(MEDIAN(assessed_k3 / amt), 3) AS k3,
         ROUND(AVG((abs(assessed_k1 / amt - 1) < 0.05)::INT), 3) AS within_5pct_k1,
         ROUND(AVG((abs(assessed_k2 / amt - 1) < 0.08)::INT), 3) AS within_8pct_k2,
         ROUND(AVG((base_k2 - sale_year = 0)::INT), 3) AS base_same_year,
         ROUND(AVG((base_k2 - sale_year = 1)::INT), 3) AS base_next_year,
         ROUND(AVG((base_k2 < sale_year)::INT), 3) AS base_older
  FROM deed_roll
  WHERE route = 'market_sale' AND amt >= {min_price} AND multi_parcel = 0 AND assessed_km1 > 0 AND assessed_k2 > 0
  GROUP BY 1, 2 ORDER BY 1, 2")) %>% as_tibble()
print(as.data.frame(timing))
write_csv_strict(timing, path(tables_out_dir, "roll_timing_check.csv"))

#============================================#
# Market value benchmark and the gap ----
#============================================#

# A family transfer has no price. Its market value is taken as the home's living area times
# the median price per square foot of market sales in the same area and year (ZIP code in
# Los Angeles, assessor neighbourhood in San Francisco).
dbExecute(con, glue("
  CREATE TEMP TABLE area_price AS
  SELECT fips5, area, sale_year, MEDIAN(amt / sqft) AS price_sqft, COUNT(*) AS n_sales
  FROM deed_roll
  WHERE route = 'market_sale' AND amt >= {min_price} AND multi_parcel = 0 AND sqft BETWEEN 300 AND 10000
    AND area IS NOT NULL
  GROUP BY 1, 2, 3 HAVING COUNT(*) >= 10"))

# reassessed: Los Angeles's base year two (or three) rolls later is the deed year or later.
# San Francisco has no base year, so only the change in assessed value is shown there.
dbExecute(con, "
  CREATE TEMP TABLE gap AS
  SELECT d.*, p.price_sqft * d.sqft AS market_value,
         d.assessed_km1 / NULLIF(p.price_sqft * d.sqft, 0) AS ratio_before,
         d.assessed_k2 / NULLIF(p.price_sqft * d.sqft, 0) AS ratio_after,
         d.assessed_k2 / NULLIF(d.assessed_km1, 0) AS assessed_growth,
         (d.base_k2 >= d.sale_year)::INT AS reassessed_k2, (d.base_k3 >= d.sale_year)::INT AS reassessed_k3,
         d.sale_year - 1 - d.base_km1 AS years_since_reassessment
  FROM deed_roll d LEFT JOIN area_price p ON p.fips5 = d.fips5 AND p.area = d.area AND p.sale_year = d.sale_year
  WHERE d.assessed_km1 > 0 AND d.assessed_k2 > 0")
dbExecute(con, glue("COPY gap TO '{sql_quote_path(path(data_dir, 'ca_deed_roll.parquet'))}' (FORMAT PARQUET, COMPRESSION ZSTD)"))

gap_stats <- "COUNT(*) AS deeds,
   ROUND(MEDIAN(assessed_km1)) AS assessed_before, ROUND(MEDIAN(market_value)) AS market_value,
   ROUND(MEDIAN(ratio_before), 3) AS ratio_before, ROUND(MEDIAN(ratio_after), 3) AS ratio_after,
   ROUND(AVG((ratio_before < 0.5)::INT), 3) AS below_half_before,
   ROUND(MEDIAN(assessed_growth), 3) AS assessed_growth,
   ROUND(AVG(reassessed_k2), 3) AS reassessed_k2, ROUND(AVG(reassessed_k3), 3) AS reassessed_k3,
   ROUND(MEDIAN(years_since_reassessment)) AS years_since_reassessment,
   ROUND(AVG(hox_before), 3) AS hox_before, ROUND(AVG(hox_after), 3) AS hox_after"
gap_by_route <- dbGetQuery(con, glue("
  SELECT fips5, CASE WHEN route = 'family_lifetime' AND spouse_doc = 1 THEN 'family_lifetime_spouse_doc'
                     WHEN route = 'family_lifetime' THEN 'family_lifetime_other' ELSE route END AS route, {gap_stats}
  FROM gap WHERE market_value > 0 GROUP BY 1, 2
  UNION ALL
  SELECT fips5, 'family_all', {gap_stats} FROM gap WHERE market_value > 0 AND route <> 'market_sale' GROUP BY 1
  ORDER BY 1, 2")) %>% as_tibble()
print(as.data.frame(gap_by_route))
write_csv_strict(gap_by_route, path(tables_out_dir, "roll_gap_by_route.csv"))

# market sales against their own price, the direct version of the gap
own_price <- dbGetQuery(con, glue("
  SELECT fips5, COUNT(*) AS sales, ROUND(MEDIAN(assessed_km1 / amt), 3) AS seller_assessed_over_price,
         ROUND(MEDIAN(assessed_k2 / amt), 3) AS buyer_assessed_over_price
  FROM gap WHERE route = 'market_sale' AND amt >= {min_price} AND multi_parcel = 0 GROUP BY 1 ORDER BY 1"))
print(own_price)
message("Wrote: ", path(data_dir, "ca_deed_roll.parquet"))
