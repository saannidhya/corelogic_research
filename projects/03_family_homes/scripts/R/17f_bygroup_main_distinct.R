# ============================================================
# 17f: By-group results for the distinct-people main measure (top of the gate-1 results)
# Author: Saani Rawat
# Purpose: Section "Main measure: distinct people": main_distinct and main_distinct_hi, with the
#          previous main measure (main_noaf_trust_heir, which includes same-person and entity
#          deeds) and the excluded series beside them.
# Inputs:  _outputs/tables/bygroup_*.csv (16, 17, 17b, 18, 18b)
# Outputs: quality_reports/gate1/2026-11-11_wpA_results.md ("Main measure: distinct people")
# Log:     11/12/2026: created.
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
library(knitr)
T <- function(f) path(tables_out_dir, f)
res_file <- path(project_dir, "quality_reports", "gate1", "2026-11-11_wpA_results.md")
k <- function(d, ...) paste(kable(d, format = "pipe", ...), collapse = "\n")
G <- c("main_distinct", "main_distinct_hi", "main_noaf_trust_heir", "main_owner_retained",
       "main_unrelated_entity")
stopifnot(bygroup_main == "main_distinct")

for (f in c("bygroup_resale_dd.csv", "bygroup_occupancy_dd.csv")) {
  if (file.mtime(T(f)) < as.POSIXct("2026-11-12 08:00:00")) stop(f, " not regenerated after the main-measure switch")
}
lc <- read_csv(T("bygroup_suppl_ca_log_changes.csv"), show_col_types = FALSE) %>%
  mutate(group = sub(" (retitle, mixed)", "", group, fixed = TRUE))
ca_pre <- function(g) lc$mean_2017_19[lc$group == g]
sp_share <- (ca_pre("main_distinct") - ca_pre("main_distinct_xspouse_ca")) / ca_pre("main_distinct")
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
rs <- read_csv(T("bygroup_resale_dd.csv"), show_col_types = FALSE) %>%
  filter(group %in% G, contrast == "rush_vs_ref_pooled") %>% select(group, resale_dd_pp = dd_pp)
oc <- read_csv(T("bygroup_occupancy_dd.csv"), show_col_types = FALSE)
mk_occ <- 100 * oc$dd_post[oc$group == "market_sale"]
oc <- oc %>% filter(group %in% G) %>%
  transmute(group, occupancy_dd_pp = 100 * dd_post, occupancy_net_of_market_pp = 100 * dd_post - mk_occ)
cnt <- read_csv(T("bygroup_state_month_counts.csv"), show_col_types = FALSE) %>%
  filter(group %in% G, state == "CA", between(sale_year, 2017L, 2019L)) %>%
  group_by(group) %>% summarise(ca_deeds_per_year_2017_19 = sum(n) / 3, .groups = "drop")
tab <- tibble(group = G) %>% left_join(cnt, by = "group") %>%
  mutate(spouse_share_CA_2017_19 = if_else(group == "main_distinct", sp_share, NA_real_)) %>%
  left_join(ru, by = "group") %>% left_join(dc, by = "group") %>% left_join(dd, by = "group") %>%
  left_join(es, by = "group") %>% left_join(ho, by = "group") %>% left_join(rs, by = "group") %>%
  left_join(oc, by = "group")

sec <- c("## Main measure: distinct people", "",
"The Section 4 main measure is now `main_distinct` (author's decision 11/12): the current proxy without death affidavits, without same-person (`owner_retained`) and entity (`unrelated_entity`) deeds, plus chain-verified trust-to-heir deeds (kept whatever their name-rule relation, because the chain itself establishes two distinct people). Spouse deeds stay. `main_distinct_hi` adds trust-to-other-person deeds that are not same-person (the upper bound). `main_noaf_trust_heir` is the previous main measure including same-person and entity deeds; `main_owner_retained` and `main_unrelated_entity` are the two excluded components. The legacy `main_proxy` repro check compares with `_outputs/_v1_snapshot_2026-11-11/tables/` and passes (16 items). Chain groups use 2017 onward only. Jan-Feb 2021 ratio is relative to the counterfactual (other states, 2019); deadline ratio is the signing-date mean 17-28 over 1-16 February (California 2021); triple difference is 2022-23 against 2017-19, net of market sales, state-clustered SE; event-study means are log points; rush and shortfall are deeds through June 2024; resale difference in differences is the rush cohort (Jun 2020-Feb 2021) against the pooled 2018-19 cohorts, California minus other states, in percentage points (no standard errors); occupancy is the change in the share with mailing address equal to property address, California minus other states, 2022-23 against 2017-19 (percentage points; net of the market-sale change). Sources: `bygroup_state_month_counts.csv`, `bygroup_suppl_ca_log_changes.csv`, `bygroup_rush_table.csv`, `bygroup_suppl_deadline_discontinuity.csv`, `bygroup_ddd.csv`, `bygroup_event_study_monthly.csv`, `bygroup_rush_hole.csv`, `bygroup_resale_dd.csv`, `bygroup_occupancy_dd.csv`.", "",
k(tab, digits = 3, format.args = list(big.mark = ",")), "")
old <- readLines(res_file, warn = FALSE)
i0 <- which(old == "## Main measure: distinct people")
nxt <- which(startsWith(old, "## ") & old != "## Main measure: distinct people")
if (length(i0)) { i1 <- min(nxt[nxt > i0[1]]); old <- old[-seq(i0[1], i1 - 1L)] }
first <- which(startsWith(old, "## "))[1]
writeLines(c(old[seq_len(first - 1L)], sec, old[first:length(old)]), res_file)
message("Main measure section written")
