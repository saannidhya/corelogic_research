# ============================================================
# Purpose : Compare family deed flows and resale by recorded property type.
# Author  : Saani Rawat
# Created : 10/02/2026
# Log     : 10/02/2026: added descriptive property-type extension.
# Inputs  : rebuilt events.parquet with two static property codes.
# Outputs : property_type_*.csv/rds; supplemental table and figure.
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
set.seed(20261002)
con <- open_corelogic_duckdb(memory_limit = "6GB", threads = 2L)
dbExecute(con, glue("SET temp_directory='{sql_quote_path(path(data_dir, 'duckdb_tmp'))}'"))
fam_classes <- "('family_person','family_other','family_estate')"
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")
ev <- glue("read_parquet('{sql_quote_path(path(data_dir, 'events.parquet'))}')")

# ============================================================
# Importing a partial, documented crosswalk ----
# ============================================================
# Research crosswalk Table 1: https://www.osti.gov/servlets/purl/1606747.
# Keep all remaining codes separate and unlabeled; no full licensed lookup.
# Condominiums are units; apartment parcels are not comparable unit counts.
type_labels <- c("163" = "Single-family", "102" = "Townhouse/rowhouse",
                 "112" = "Condominium", "115" = "Duplex", "106" = "Apartment")
saveRDS(type_labels, path(out_dir, "property_type_labels.rds"))
dbExecute(con, glue("CREATE TEMP VIEW typed_events AS
  SELECT *, CASE land_use_code_static
    WHEN '163' THEN 'Single-family' WHEN '102' THEN 'Townhouse/rowhouse'
    WHEN '112' THEN 'Condominium' WHEN '115' THEN 'Duplex'
    WHEN '106' THEN 'Apartment' ELSE 'Other/unverified codes' END AS property_type
  FROM {ev}"))

# ============================================================
# National and state-year composition ----
# ============================================================
message("Aggregating property types: ", Sys.time())
code_class <- dbGetQuery(con, glue("SELECT land_use_code_static, class,
  COUNT(*) AS n, SUM(CASE WHEN absentee_buyer=1 THEN 1 ELSE 0 END) AS absentee,
  SUM(CASE WHEN absentee_buyer IS NOT NULL THEN 1 ELSE 0 END) AS absentee_observed
  FROM typed_events WHERE sale_year BETWEEN 2007 AND 2023
  GROUP BY ALL ORDER BY land_use_code_static, class"))
write_csv_strict(code_class, path(tables_out_dir, "property_type_code_class.csv"))
indicator_crosswalk <- dbGetQuery(con, "SELECT land_use_code_static,
  property_indicator_code_static, COUNT(*) AS n FROM typed_events
  WHERE sale_year BETWEEN 2007 AND 2023 GROUP BY ALL ORDER BY n DESC")
write_csv_strict(indicator_crosswalk, path(tables_out_dir, "property_type_indicator_crosswalk.csv"))
sy <- dbGetQuery(con, glue("SELECT property_type,state,sale_year,COUNT(*) AS n_all,
  SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS family,
  SUM(CASE WHEN class='family_person' THEN 1 ELSE 0 END) AS same_surname,
  SUM(CASE WHEN class='family_trust' THEN 1 ELSE 0 END) AS trust,
  SUM(CASE WHEN class='market_sale' THEN 1 ELSE 0 END) AS market,
  SUM(CASE WHEN class IN {fam_classes} AND absentee_buyer=1 THEN 1 ELSE 0 END) AS family_absentee,
  SUM(CASE WHEN class IN {fam_classes} AND absentee_buyer IS NOT NULL THEN 1 ELSE 0 END) AS family_absentee_observed
  FROM typed_events WHERE sale_year BETWEEN 2007 AND 2023
  GROUP BY ALL ORDER BY property_type,state,sale_year"))
national <- sy %>%
  group_by(property_type) %>%
  summarize(across(c(n_all, family, same_surname, trust, market,
                     family_absentee, family_absentee_observed), sum), .groups = "drop") %>%
  mutate(family_market_ratio = family / market,
         family_share_family_market = family / (family + market),
         family_share_all_deeds = family / n_all,
         family_absentee_share = family_absentee / family_absentee_observed)
fact_counts <- read_csv(path(tables_out_dir, "fact_national_by_year_class.csv"),
                        show_col_types = FALSE)
stopifnot(sum(national$n_all) == sum(fact_counts$n),
          sum(national$family) == sum(fact_counts$n[
            fact_counts$class %in% c("family_person", "family_other", "family_estate")]))
write_csv_strict(sy, path(tables_out_dir, "property_type_state_year.csv"))
write_csv_strict(national, path(tables_out_dir, "property_type_national.csv"))
saveRDS(list(national = national, state_year = sy, code_class = code_class,
             indicator_crosswalk = indicator_crosswalk), path(out_dir, "property_type_composition.rds"))

# Pairwise standardization against SF using SF family+market cell weights.
# Each comparison has its own common state-year support; this is descriptive.
sf_cells <- sy %>% filter(property_type == "Single-family", !state %in% territories) %>%
  transmute(state, sale_year, sf_family = family, sf_market = market)
standardized <- sy %>%
  filter(property_type != "Other/unverified codes", !state %in% territories) %>%
  inner_join(sf_cells, by = c("state", "sale_year")) %>%
  filter(family + market >= 100, sf_family + sf_market >= 100) %>%
  mutate(weight = sf_family + sf_market) %>%
  group_by(property_type) %>%
  summarize(n_common_cells = n(), type_events = sum(family + market),
            standardized_family_share = weighted.mean(family / (family + market), weight),
            sf_share_same_cells = weighted.mean(sf_family / (sf_family + sf_market), weight),
            difference_pp = 100 * (standardized_family_share - sf_share_same_cells), .groups = "drop")
write_csv_strict(standardized, path(tables_out_dir, "property_type_standardized.csv"))
saveRDS(standardized, path(out_dir, "property_type_standardized.rds"))

# ============================================================
# Exploratory California deadline response within each type ----
# ============================================================
monthly <- dbGetQuery(con, glue("SELECT property_type,state,ym,sale_year,sale_month,
  SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS family,
  SUM(CASE WHEN class='market_sale' THEN 1 ELSE 0 END) AS market
  FROM typed_events WHERE sale_year BETWEEN 2018 AND 2023 GROUP BY ALL")) %>%
  filter(!state %in% territories, property_type != "Other/unverified codes") %>%
  complete(property_type, state, nesting(ym, sale_year, sale_month),
           fill = list(family = 0, market = 0))
ca <- monthly %>% filter(state == "CA") %>%
  select(property_type, ym, sale_year, sale_month, actual = family)
donors <- monthly %>% filter(state != "CA") %>%
  group_by(property_type, ym) %>% summarize(donor = sum(family), .groups = "drop")
base_ca <- ca %>% filter(sale_year == 2019) %>%
  select(property_type, sale_month, base_ca = actual)
base_dn <- monthly %>% filter(state != "CA", sale_year == 2019) %>%
  group_by(property_type, sale_month) %>% summarize(base_donor = sum(family), .groups = "drop")
type_cf <- ca %>%
  left_join(donors, by = c("property_type", "ym")) %>%
  left_join(base_ca, by = c("property_type", "sale_month")) %>%
  left_join(base_dn, by = c("property_type", "sale_month")) %>%
  mutate(counterfactual = base_ca * donor / base_donor, excess = actual - counterfactual)
stopifnot(all(is.finite(type_cf$base_donor)), all(type_cf$base_donor > 0),
          all(is.finite(type_cf$counterfactual)), all(type_cf$counterfactual >= 0))
deadline <- type_cf %>% group_by(property_type) %>%
  summarize(actual_anticipation = sum(actual[ym >= 202011 & ym <= 202102]),
            cf_anticipation = sum(counterfactual[ym >= 202011 & ym <= 202102]),
            excess_anticipation = actual_anticipation - cf_anticipation,
            anticipation_pct = if_else(cf_anticipation > 0,
                                       100 * excess_anticipation / cf_anticipation, NA_real_),
            actual_steady = sum(actual[ym >= 202201 & ym <= 202312]),
            cf_steady = sum(counterfactual[ym >= 202201 & ym <= 202312]),
            steady_gap_pct = if_else(cf_steady > 0,
                                    100 * (actual_steady / cf_steady - 1), NA_real_),
            .groups = "drop")
write_csv_strict(type_cf, path(tables_out_dir, "property_type_ca_counterfactual.csv"))
write_csv_strict(deadline, path(tables_out_dir, "property_type_ca_deadline.csv"))
saveRDS(list(monthly = monthly, counterfactual = type_cf, deadline = deadline),
        path(out_dir, "property_type_ca_deadline.rds"))

# ============================================================
# Five-year resale: same sample and calendar construction as 03 ----
# ============================================================
message("Building type-specific resale spells: ", Sys.time())
create_dated_events(con, "typed_events")
dbExecute(con, "CREATE TEMP TABLE type_market_sales AS
  SELECT clip,sale_date FROM ev_dated WHERE class='market_sale'")
resale_cell_list <- list()
exit_list <- list()
for (st in sort(unique(sy$state))) {
  message("Resale spells: ", st)
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE type_spells AS
  SELECT c.clip,c.state,c.sale_year,c.sale_date,c.property_type,c.class,
    MIN(m.sale_date) AS next_market_date
  FROM ev_dated c LEFT JOIN type_market_sales m ON m.clip=c.clip
    AND m.sale_date>c.sale_date
  WHERE c.state='{st}' AND c.sale_year BETWEEN 2008 AND 2018
    AND c.class IN ('family_person','family_other','family_estate','market_sale')
  GROUP BY ALL"))
  resale_cell_list[[st]] <- dbGetQuery(con, glue("SELECT property_type,state,sale_year,
  CASE WHEN class IN {fam_classes} THEN 'Family broad' ELSE 'Market' END AS comparison,
  COUNT(*) AS n,
  AVG(CASE WHEN next_market_date <= sale_date + INTERVAL 60 MONTH THEN 1.0 ELSE 0 END) AS sold_60m
  FROM type_spells GROUP BY ALL"))
  exit_list[[st]] <- dbGetQuery(con, glue("WITH comparison_spells AS (
    SELECT property_type,'Family broad' AS comparison,sale_date,next_market_date
    FROM type_spells WHERE class IN {fam_classes}
    UNION ALL SELECT property_type,'Same surname',sale_date,next_market_date
    FROM type_spells WHERE class='family_person'
    UNION ALL SELECT property_type,'Market',sale_date,next_market_date
    FROM type_spells WHERE class='market_sale'
  ), duration AS (
    SELECT *,datediff('month',sale_date,next_market_date) AS fail_m,
      datediff('month',sale_date,DATE '2024-06-30') AS censor_m
    FROM comparison_spells
  ) SELECT property_type,comparison,
    CASE WHEN fail_m IS NOT NULL AND fail_m<=censor_m THEN 'fail' ELSE 'censor' END AS exit_type,
    CASE WHEN fail_m IS NOT NULL AND fail_m<=censor_m THEN GREATEST(fail_m,1)
      ELSE GREATEST(censor_m,1) END AS exit_m,COUNT(*) AS n
    FROM duration WHERE censor_m>=1 GROUP BY ALL"))
}
resale_cells <- bind_rows(resale_cell_list)
hazard_counts <- read_csv(path(tables_out_dir, "hazard_sold_within.csv"),
                         show_col_types = FALSE)
stopifnot(sum(resale_cells$n) == sum(hazard_counts$n[
  hazard_counts$class %in% c("family_person", "family_other", "family_estate", "market_sale")]))
resale <- resale_cells %>% group_by(property_type, comparison) %>%
  summarize(sold_60m = weighted.mean(sold_60m, n), n = sum(n), .groups = "drop")
# Within-type comparisons use common state/cohort weights from market events.
resale_std <- resale_cells %>% filter(!state %in% territories) %>%
  pivot_wider(names_from = comparison, values_from = c(n, sold_60m)) %>%
  filter(`n_Family broad` >= 20, n_Market >= 20) %>%
  group_by(property_type) %>%
  summarize(n_common_cells = n(),
            family_sold_60m = weighted.mean(`sold_60m_Family broad`, n_Market),
            market_sold_60m = weighted.mean(sold_60m_Market, n_Market),
            family_minus_market_pp = 100 * (family_sold_60m - market_sold_60m), .groups = "drop")
write_csv_strict(resale_cells, path(tables_out_dir, "property_type_resale_cells.csv"))
write_csv_strict(resale, path(tables_out_dir, "property_type_resale.csv"))
write_csv_strict(resale_std, path(tables_out_dir, "property_type_resale_standardized.csv"))
saveRDS(list(cells = resale_cells, pooled = resale, standardized = resale_std),
        path(out_dir, "property_type_resale.rds"))
exits <- bind_rows(exit_list) %>%
  group_by(property_type, comparison, exit_type, exit_m) %>%
  summarize(n = sum(n), .groups = "drop")
km <- exits %>%
  pivot_wider(names_from = exit_type, values_from = n, values_fill = 0) %>%
  group_by(property_type, comparison) %>%
  complete(exit_m = 1:max(exit_m), fill = list(fail = 0, censor = 0)) %>%
  arrange(exit_m, .by_group = TRUE) %>%
  mutate(n_total = sum(fail) + sum(censor),
         risk = n_total - lag(cumsum(fail + censor), default = 0),
         survival = cumprod(1 - if_else(risk > 0, fail / risk, 0))) %>%
  ungroup() %>% filter(exit_m <= 120)
write_csv_strict(km, path(tables_out_dir, "property_type_km.csv"))
write_csv_strict(km %>% filter(exit_m %in% c(24, 60, 120)),
                 path(tables_out_dir, "property_type_km_headlines.csv"))
saveRDS(list(exits = exits, curves = km), path(out_dir, "property_type_km.rds"))

# ============================================================
# Exporting supplemental exhibits ----
# ============================================================
plot_data <- national %>% filter(property_type != "Other/unverified codes") %>%
  mutate(property_type = factor(property_type, levels = unname(type_labels)))
type_plot <- ggplot(plot_data, aes(property_type, family_share_family_market)) +
  geom_col(fill = "#012169", width = .65) +
  geom_text(aes(label = percent(family_share_family_market, accuracy = .1)), vjust = -.35) +
  scale_y_continuous(labels = percent, expand = expansion(mult = c(0, .12))) +
  labs(x = NULL, y = "Family share of family plus market deeds",
       subtitle = "2007–2023; recorded land-use codes; apartment sample is incomplete") +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))
for (extension in c("pdf", "png")) {
  ggsave(path(figures_dir, paste0("F9_property_types.", extension)), type_plot,
         width = 8, height = 4.5, bg = "transparent", dpi = 300)
}
tex_rows <- plot_data %>%
  mutate(row = glue("{property_type} & {formatC(family,format='f',digits=0,big.mark=',')} & ",
                    "{formatC(market,format='f',digits=0,big.mark=',')} & ",
                    "{sprintf('%.1f',100*family_share_family_market)} & ",
                    "{sprintf('%.2f',family_market_ratio)} \\\\")) %>% pull(row)
writeLines(c("\\begin{table}[!ht]\\centering\\small",
  "\\caption{Recorded property types and family deed flows, 2007--2023}",
  "\\label{tab:propertytypes}", "\\begin{tabular}{lrrrr}\\toprule",
  "Recorded type & Family deeds & Market deeds & Family share (\\%) & Family/market \\\\",
  "\\midrule", tex_rows, "\\bottomrule\\end{tabular}",
  "\\begin{minipage}{.95\\linewidth}\\footnotesize\\textit{Notes:} Family deeds use the broad definition, excluding trust self-transfers. Shares divide family deeds by family plus market deeds. Partial published numeric crosswalk: 163 single-family, 102 townhouse/rowhouse, 112 condominium, 115 duplex, 106 apartment. Other codes remain unverified and are omitted from this exhibit, not the aggregate sample. Types are static recorded characteristics. Apartments are selectively covered: only 13\\% of source apartment rows pass the existing residential flag. Parcel deeds count neither dwelling units nor occupancy changes.\\end{minipage}",
  "\\end{table}"), path(tables_dir, "T7_property_types.tex"))
ten_year <- km %>% filter(exit_m == 120, comparison %in% c("Same surname", "Market"),
                         property_type != "Other/unverified codes", property_type != "Apartment") %>%
  select(property_type, comparison, survival) %>%
  pivot_wider(names_from = comparison, values_from = survival) %>%
  mutate(gap_pp = 100 * (`Same surname` - Market))
resale_rows <- ten_year %>%
  mutate(row = glue("{property_type} & {sprintf('%.1f',100*`Same surname`)} & ",
                    "{sprintf('%.1f',100*Market)} & {sprintf('%.1f',gap_pp)} \\\\")) %>% pull(row)
writeLines(c("\\begin{table}[!ht]\\centering\\small",
  "\\caption{Ten-year retention by recorded property type}",
  "\\label{tab:propertyretention}", "\\begin{tabular}{lrrr}\\toprule",
  " & \\multicolumn{2}{c}{Unsold at ten years (\\%)} & Gap \\\\",
  "Recorded type & Same-surname family & Market purchase & (pp) \\\\",
  "\\midrule", resale_rows, "\\bottomrule\\end{tabular}",
  "\\begin{minipage}{.95\\linewidth}\\footnotesize\\textit{Notes:} Kaplan--Meier survival to the next market sale for 2008--2018 deed cohorts, censored June 2024, using the monthly exit construction of the aggregate analysis. These are pooled within-type comparisons; state/cohort composition, recipient selection, and occupancy histories are not controlled. Apartment results are omitted because of selective coverage.\\end{minipage}",
  "\\end{table}"), path(tables_dir, "T8_property_retention.tex"))
dbDisconnect(con, shutdown = TRUE)
message("Finished 13_property_types: ", Sys.time())
