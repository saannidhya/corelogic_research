# ============================================================
# 17c: By-group final measure section (Gate 1 decision)
# Author: Saani Rawat
# Purpose: Insert "Final measure (Gate 1 decision)" at the top of the gate-1 results file:
#          rush and decline tables for the Section 4 family measure
#          (main_noaf_trust_heir) and its comparators. Reads outputs of 16, 17 and 17b only.
# Inputs:  _outputs/tables/bygroup_*.csv
# Outputs: quality_reports/gate1/2026-11-11_wpA_results.md (section replaced if present)
# Log:     11/11/2026: created.
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
library(knitr)
T <- function(f) path(tables_out_dir, f)
res_file <- path(project_dir, "quality_reports", "gate1", "2026-11-11_wpA_results.md")
k <- function(d, ...) paste(kable(d, format = "pipe", ...), collapse = "\n")
G <- c("main_distinct","main_distinct_hi","main_noaf_trust_heir","main_noaf","main_proxy","trust_heir_chain",
       "trust_heir_nochain","lifetime_pc","spouse_ca","owner_retained","trust_to_self")
lab <- function(x) ifelse(x == "owner_retained", "owner_retained (retitle, mixed)", x)

ru <- read_csv(T("bygroup_rush_table.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", group %in% G) %>%
  mutate(v = if_else(scope == "ca_only", raw_ratio, relative_ratio)) %>%
  select(group, window, v, ca_base) %>%
  pivot_wider(names_from = window, values_from = c(v, ca_base)) %>%
  transmute(group, `CA base Jan-Feb` = ca_base_jan_feb_2021, `Jan-Feb 2021` = v_jan_feb_2021,
            `Mar-Apr 2021` = v_mar_apr_2021, `Nine months` = v_nine_month)
dc <- read_csv(T("bygroup_suppl_deadline_discontinuity.csv"), show_col_types = FALSE) %>%
  filter(group %in% G)
dw <- function(b, nm) dc %>% filter(basis == b) %>% mutate(col = paste(nm, state_class, year)) %>%
  select(group, col, ratio) %>% pivot_wider(names_from = col, values_from = ratio)
dl <- dw("signing 16-28 over 1-15, all days (primary)", "sign") %>%
  left_join(dw("signing 17-28 over 1-16, all days (sensitivity)", "sign16") %>%
              select(group, `sign16 CA 2021`), by = "group") %>%
  left_join(dw("recording 17-28 over 1-16, business days", "rec"), by = "group") %>%
  select(group, `sign CA 2021`, `sign16 CA 2021`, `sign CA 2019`, `sign other 2021`,
         `rec CA 2021`, `rec CA 2019`, `rec other 2021`)
rush_tab <- ru %>% left_join(dl, by = "group") %>% arrange(match(group, G)) %>%
  mutate(group = lab(group))

dd <- read_csv(T("bygroup_ddd.csv"), show_col_types = FALSE) %>% filter(group %in% G) %>%
  transmute(group, triple_diff = ddd, se = ddd_se, nobs, n_states, n_dropped = n_states_dropped,
            ca_deeds_pre, ca_deeds_post)
es <- read_csv(T("bygroup_event_study_monthly.csv"), show_col_types = FALSE) %>%
  filter(group %in% G) %>% group_by(group) %>%
  summarise(mar_dec_2021 = mean(estimate[ym >= 202103 & ym <= 202112]),
            y2022 = mean(estimate[ym >= 202201 & ym <= 202212]),
            y2023 = mean(estimate[ym >= 202301 & ym <= 202312]), .groups = "drop")
cf <- read_csv(T("bygroup_cf_monthly.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", group %in% G) %>% group_by(group) %>%
  summarise(h1_2024_cf_gap = log(sum(ca_actual[ym >= 202401 & ym <= 202406]) /
                                   sum(cf[ym >= 202401 & ym <= 202406])), .groups = "drop")
lc <- read_csv(T("bygroup_suppl_ca_log_changes.csv"), show_col_types = FALSE) %>%
  mutate(group = sub(" (retitle, mixed)", "", group, fixed = TRUE)) %>% filter(group %in% G) %>%
  select(group, less_spouse = minus_spouse_vs_2017_19, less_market = minus_market_vs_2017_19)
ho <- read_csv(T("bygroup_rush_hole.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", end_label == "main_202406", group %in% G) %>%
  select(group, rush = rush_excess, shortfall, ratio = harvest_ratio)
dec_tab <- tibble(group = G) %>% left_join(dd, by = "group") %>% left_join(es, by = "group") %>%
  left_join(cf, by = "group") %>% left_join(lc, by = "group") %>% left_join(ho, by = "group") %>%
  mutate(group = lab(group))

sec <- c(
"## Final measure (Gate 1 decision)", "",
"The Section 4 family measure is `main_distinct` (author's decision 11/12; see the section Main measure: distinct people): `main_noaf_trust_heir` (the previous main measure) without same-person and entity deeds. The previous measure is the current proxy without death affidavits (`main_noaf`) plus chain-verified trust-to-heir deeds (`trust_heir_chain`: an earlier non-market deed on the same parcel moved the home from person A into a trust with no market sale in between, and the trust then deeded it to B, same surname, different person; 10-year look-back; 89% of 99 read deeds correct). `main_noaf_trust_heir_hi` adds all other trust-to-other-person deeds (the upper bound); `trust_heir_nochain` is the trust-to-other-person remainder. The chain groups are used from 2017 on only (the look-back is complete only from 2017); no design below uses earlier months for them. Panel: `events.parquet` rebuilt 2026-11-11 15:46.", "",
"### Rush (relative to the counterfactual built from other states and 2019; `spouse_ca` shows raw California ratios) and deadline ratios",
"Deadline ratios are mean deeds per day after over before the deadline: signing date 16-28 over 1-15 February (all calendar days; `sign16` moves 16 February to the pre side), recording date 17-28 over 1-16 February (business days). Sources: `bygroup_rush_table.csv`, `bygroup_suppl_deadline_discontinuity.csv`.",
k(rush_tab, digits = 2, format.args = list(big.mark = ",")), "",
"### Decline (log points; state-clustered SE)",
"Triple difference: 2022-23 against 2017-19, California against other states, net of market sales. Event-study means are California relative to other states (reference 2018-19); the 2024 column is log(actual / counterfactual) for January-June 2024 (the event study ends December 2023). `less_spouse` and `less_market` are California-only changes in log annual deeds (2022-23 against 2017-19) minus the same change for spouse deeds or market sales. Rush and shortfall are in deeds through June 2024. Sources: `bygroup_ddd.csv`, `bygroup_event_study_monthly.csv`, `bygroup_cf_monthly.csv`, `bygroup_suppl_ca_log_changes.csv`, `bygroup_rush_hole.csv`.",
k(dec_tab, digits = 3, format.args = list(big.mark = ",")), "")
old <- readLines(res_file, warn = FALSE)
i0 <- which(old == "## Final measure (Gate 1 decision)")
i1 <- which(startsWith(old, "## 0."))[1]
if (length(i0)) old <- old[-seq(i0[1], i1 - 1L)]
i1 <- which(startsWith(old, "## 0."))[1]
writeLines(c(old[seq_len(i1 - 1L)], sec, old[i1:length(old)]), res_file)
message("Final-measure section written")
