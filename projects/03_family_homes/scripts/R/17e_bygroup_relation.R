# ============================================================
# 17e: By-group relation sensitivity (section at the top of the gate-1 results)
# Author: Saani Rawat
# Purpose: After the relation cleanup, the main measure in 2017-19 is about a third
#          same-person retitles (owner_retained) and under a tenth unrelated_entity.
#          Do Section 4 results survive without them, and what do the national
#          family-deeds-per-100 facts become without them?
# Inputs:  _outputs/tables/bygroup_*.csv; data/derived/03_family_homes/bygroup/cube_state_month.parquet
# Outputs: quality_reports/gate1/2026-11-11_wpA_results.md ("Relation sensitivity")
# Log:     11/12/2026: created.
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
library(knitr)
T <- function(f) path(tables_out_dir, f)
res_file <- path(project_dir, "quality_reports", "gate1", "2026-11-11_wpA_results.md")
k <- function(d, ...) paste(kable(d, format = "pipe", ...), collapse = "\n")
main_g <- "main_noaf_trust_heir"
G <- c(main_g, "main_noaf_trust_heir_xor", "main_noaf_trust_heir_xore", "main_owner_retained",
       "main_unrelated_entity")

# ---- annual deeds 2017-19 and shares of the main measure ----
cnt <- read_csv(T("bygroup_state_month_counts.csv"), show_col_types = FALSE) %>%
  filter(group %in% G, between(sale_year, 2017L, 2019L)) %>%
  mutate(region = if_else(state == "CA", "CA", "other")) %>%
  group_by(group, region) %>% summarise(per_year = sum(n) / 3, .groups = "drop") %>%
  pivot_wider(names_from = region, values_from = per_year, names_prefix = "deeds_per_year_")
main_ca <- cnt$deeds_per_year_CA[cnt$group == main_g]; main_ot <- cnt$deeds_per_year_other[cnt$group == main_g]
cnt <- cnt %>% mutate(share_of_main_CA = deeds_per_year_CA / main_ca,
                      share_of_main_other = deeds_per_year_other / main_ot)

ru <- read_csv(T("bygroup_rush_table.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", window == "jan_feb_2021", group %in% G) %>%
  select(group, jan_feb_2021_ratio = relative_ratio)
dc <- read_csv(T("bygroup_suppl_deadline_discontinuity.csv"), show_col_types = FALSE) %>%
  filter(group %in% G, basis == "signing 17-28 over 1-16, all days (sensitivity)",
         state_class == "CA", year == 2021) %>% select(group, deadline_CA_2021 = ratio)
dd <- read_csv(T("bygroup_ddd.csv"), show_col_types = FALSE) %>% filter(group %in% G) %>%
  select(group, triple_diff = ddd, se = ddd_se, nobs, n_states)
es <- read_csv(T("bygroup_event_study_monthly.csv"), show_col_types = FALSE) %>%
  filter(group %in% G) %>% group_by(group) %>%
  summarise(es_2022 = mean(estimate[ym >= 202201 & ym <= 202212]),
            es_2023 = mean(estimate[ym >= 202301 & ym <= 202312]), .groups = "drop")
ho <- read_csv(T("bygroup_rush_hole.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", end_label == "main_202406", group %in% G) %>%
  select(group, rush = rush_excess, shortfall, rush_over_shortfall = harvest_ratio)
tab <- tibble(group = G) %>% left_join(cnt, by = "group") %>% left_join(ru, by = "group") %>%
  left_join(dc, by = "group") %>% left_join(dd, by = "group") %>% left_join(es, by = "group") %>%
  left_join(ho, by = "group")

# ---- national facts measure (main_proxy): family deeds per 100 market sales / per 100 deeds ----
con <- open_corelogic_duckdb()
cube <- path(data_dir, "bygroup", "cube_state_month.parquet")
fam <- "class IN ('family_person','family_estate','family_other')"
qry <- function(region_filter, label) {
  dbGetQuery(con, glue("
    SELECT sale_year,
      SUM(CASE WHEN {fam} THEN n ELSE 0 END) AS fam_all,
      SUM(CASE WHEN {fam} AND coalesce(fam_relation_nm,'') <> 'owner_retained' THEN n ELSE 0 END) AS fam_xor,
      SUM(CASE WHEN {fam} AND coalesce(fam_relation_nm,'') NOT IN ('owner_retained','unrelated_entity') THEN n ELSE 0 END) AS fam_xore,
      SUM(CASE WHEN class = 'market_sale' THEN n ELSE 0 END) AS market,
      SUM(n) AS all_deeds
    FROM read_parquet('{sql_quote_path(cube)}')
    WHERE sale_year BETWEEN 2017 AND 2023 AND {region_filter}
    GROUP BY sale_year ORDER BY sale_year")) %>% mutate(region = label)
}
yr <- bind_rows(qry("TRUE", "National (all rows)"), qry("state = 'CA'", "California")) %>%
  mutate(across(c(fam_all, fam_xor, fam_xore, market, all_deeds), as.numeric))
dbDisconnect(con, shutdown = TRUE)
nat <- yr %>% pivot_longer(c(fam_all, fam_xor, fam_xore), names_to = "variant", values_to = "fam") %>%
  mutate(per100_market = 100 * fam / market, per100_all = 100 * fam / all_deeds,
         variant = recode(variant, fam_all = "main_proxy", fam_xor = "main_proxy without owner_retained",
                          fam_xore = "main_proxy without owner_retained and unrelated_entity")) %>%
  group_by(region, variant) %>%
  summarise(per_100_market_sales = mean(per100_market), per_100_all_deeds = mean(per100_all),
            pooled_per_100_market = 100 * sum(fam) / sum(market), .groups = "drop") %>%
  arrange(desc(region), match(variant, c("main_proxy", "main_proxy without owner_retained",
                                         "main_proxy without owner_retained and unrelated_entity")))

sec <- c("## Relation sensitivity", "",
"After the relation cleanup, `main_noaf_trust_heir` (the main measure) is a third same-person retitles (`main_owner_retained`: `fam_relation_nm = 'owner_retained'`) and under a tenth deeds to unrelated entities (`main_unrelated_entity`). `main_noaf_trust_heir_xor` removes the first; `main_noaf_trust_heir_xore` removes both. Jan-Feb 2021 ratio is relative to the counterfactual (other states, 2019); the deadline ratio is the signing-date mean for 17-28 over 1-16 February (California 2021); the triple difference is 2022-23 against 2017-19, net of market sales, state-clustered SE; event-study means are log points; rush and shortfall are deeds through June 2024. Sources: `bygroup_state_month_counts.csv` (annual deeds 2017-19, shares of the main measure), `bygroup_rush_table.csv`, `bygroup_suppl_deadline_discontinuity.csv`, `bygroup_ddd.csv`, `bygroup_event_study_monthly.csv`, `bygroup_rush_hole.csv`. Note: the `main_owner_retained` and `main_unrelated_entity` rows are the components inside the main measure; the older `owner_retained` group (all direct same-person deeds, including `family_retitle`) is a different, larger series.", "",
k(tab, digits = 3, format.args = list(big.mark = ",")), "",
"### National facts measure (`main_proxy`): family deeds per 100, 2017-2023",
"Average of the seven annual ratios (`pooled_per_100_market` is the pooled 2017-2023 ratio to market sales). Per 100 market sales = family deeds over market sales; per 100 all deeds = family deeds over every deed in the panel (all classes). National counts include every row of the panel (territories too). Source: `cube_state_month.parquet` (16).",
k(nat, digits = 2), "")
old <- readLines(res_file, warn = FALSE)
i0 <- which(old == "## Relation sensitivity")
i1 <- which(old == "## Cross-surname sensitivity")[1]
if (length(i0)) old <- old[-seq(i0[1], i1 - 1L)]
i1 <- which(old == "## Cross-surname sensitivity")[1]
writeLines(c(old[seq_len(i1 - 1L)], sec, old[i1:length(old)]), res_file)
message("Relation sensitivity section written")
