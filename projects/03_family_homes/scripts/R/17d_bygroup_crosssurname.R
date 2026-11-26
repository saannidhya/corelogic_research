# ============================================================
# 17d: By-group cross-surname sensitivity (section at the top of the gate-1 results)
# Author: Saani Rawat
# Purpose: How much of the main measure is cross-surname deeds (fam_relation_nm =
#          'cross_surname', from family_other; a read of 100 found 33% correctly
#          labelled), and do the Section 4 results survive without them?
# Inputs:  _outputs/tables/bygroup_*.csv (16, 17, 17b)
# Outputs: quality_reports/gate1/2026-11-11_wpA_results.md ("Cross-surname sensitivity")
# Log:     11/11/2026: created.
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
library(knitr)
T <- function(f) path(tables_out_dir, f)
res_file <- path(project_dir, "quality_reports", "gate1", "2026-11-11_wpA_results.md")
k <- function(d, ...) paste(kable(d, format = "pipe", ...), collapse = "\n")
G <- c("main_noaf_trust_heir", "main_noaf_trust_heir_xcs", "cross_surname")
main_g <- "main_noaf_trust_heir"
cube <- arrow::read_parquet(path(data_dir, "bygroup", "cube_state_month.parquet"),
                            col_select = c("class", "fam_relation_nm", "fam_vehicle", "n"))
cs_cls <- cube %>% filter(fam_relation_nm == "cross_surname") %>% group_by(class, fam_vehicle) %>%
  summarise(n = sum(n), .groups = "drop")
n_unrel <- sum(cube$n[cube$class == "family_other" & cube$fam_relation_nm == "unrelated_entity"])
rm(cube)

# ---- share of cross-surname deeds in the main measure ----
cnt <- read_csv(T("bygroup_state_month_counts.csv"), show_col_types = FALSE) %>%
  filter(group %in% c(main_g, "cross_surname", "main_noaf_trust_heir_xcs"))
per <- function(d, lo, hi) d %>% filter(sale_year >= lo, sale_year <= hi) %>%
  mutate(region = if_else(state == "CA", "California", "Other states")) %>%
  group_by(region, group) %>% summarise(n = sum(n), .groups = "drop")
shares <- bind_rows(per(cnt, 2017, 2019) %>% mutate(period = "2017-19"),
                    per(cnt, 2022, 2023) %>% mutate(period = "2022-23")) %>%
  pivot_wider(names_from = group, values_from = n) %>%
  transmute(period, region, main_measure = .data[[main_g]], cross_surname,
            without_cross_surname = main_noaf_trust_heir_xcs,
            share_cross_surname = cross_surname / main_measure)
stopifnot(all(abs(shares$main_measure - shares$without_cross_surname - shares$cross_surname) < 1))

# ---- the results table ----
ru <- read_csv(T("bygroup_rush_table.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", window == "jan_feb_2021", group %in% G) %>%
  select(group, jan_feb_2021_ratio = relative_ratio)
dc <- read_csv(T("bygroup_suppl_deadline_discontinuity.csv"), show_col_types = FALSE) %>%
  filter(group %in% G, basis == "signing 17-28 over 1-16, all days (sensitivity)") %>%
  mutate(col = paste("deadline", state_class, year)) %>% select(group, col, ratio) %>%
  pivot_wider(names_from = col, values_from = ratio) %>%
  select(group, `deadline CA 2021`, `deadline CA 2019`, `deadline other 2021`)
dd <- read_csv(T("bygroup_ddd.csv"), show_col_types = FALSE) %>% filter(group %in% G) %>%
  select(group, triple_diff = ddd, se = ddd_se, nobs, n_states)
es <- read_csv(T("bygroup_event_study_monthly.csv"), show_col_types = FALSE) %>%
  filter(group %in% G) %>% group_by(group) %>%
  summarise(es_2022 = mean(estimate[ym >= 202201 & ym <= 202212]),
            es_2023 = mean(estimate[ym >= 202301 & ym <= 202312]), .groups = "drop")
ho <- read_csv(T("bygroup_rush_hole.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", end_label == "main_202406", group %in% G) %>%
  select(group, rush = rush_excess, shortfall, rush_over_shortfall = harvest_ratio)
tab <- tibble(group = G) %>% left_join(ru, by = "group") %>% left_join(dc, by = "group") %>%
  left_join(dd, by = "group") %>% left_join(es, by = "group") %>% left_join(ho, by = "group")

sec <- c("## Cross-surname sensitivity", "",
paste0("Cross-surname deeds (`cross_surname`: `fam_relation_nm = 'cross_surname'`, flagged interfamily with different surnames) come from: ", paste(sprintf("%s / %s (%s deeds 2016-2024)", cs_cls$class, cs_cls$fam_vehicle, format(cs_cls$n, big.mark = ",")), collapse = "; "), "; `family_other` also holds ", format(n_unrel, big.mark = ","), " `unrelated_entity` deeds (corporate buyers). The earlier 33% read score judged the label, not family status; after the relation rule the remaining cross-surname deeds read 73% correct as distinct people with different surnames. `main_noaf_trust_heir_xcs` is the main measure without them. Sources: `bygroup_state_month_counts.csv`, `bygroup_rush_table.csv`, `bygroup_suppl_deadline_discontinuity.csv`, `bygroup_ddd.csv`, `bygroup_event_study_monthly.csv`, `bygroup_rush_hole.csv`."), "",
"### Share of cross-surname deeds in the main measure (deeds, by signing year)",
k(shares, digits = 3, format.args = list(big.mark = ",")), "",
"### Results with and without cross-surname deeds",
"Jan-Feb 2021 ratio is relative to the counterfactual (other states, 2019). Deadline ratios are signing-date means 17-28 over 1-16 February (CA 2021, CA 2019, other states 2021). Triple difference: 2022-23 against 2017-19, net of market sales, state-clustered SE. Event-study means are log points; rush and shortfall are deeds through June 2024.",
k(tab, digits = 3, format.args = list(big.mark = ",")), "")
old <- readLines(res_file, warn = FALSE)
i0 <- which(old == "## Cross-surname sensitivity")
i1 <- which(old == "## Final measure (Gate 1 decision)")[1]
if (length(i0)) old <- old[-seq(i0[1], i1 - 1L)]
i1 <- which(old == "## Final measure (Gate 1 decision)")[1]
writeLines(c(old[seq_len(i1 - 1L)], sec, old[i1:length(old)]), res_file)
message("Cross-surname section written")
