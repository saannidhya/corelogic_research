# ============================================================
# 17b: By-group supplement to 17 (Gate 1 memo cuts). No new regressions.
# Author: Saani Rawat
# Purpose: (1) deadline discontinuity by group (daily means, 1-15 vs 16-28 Feb 2021,
#              Feb 2019, other states 2021); (2) is the gap closing (event-study
#              coefficient means by period); (3) standalone trust-group triple
#              differences; (4) California-only log changes with spouse and market
#              differences; (5) California classes, annual averages 2017-19, 2021,
#              2022-23. Appends a "Supplement" section to the gate-1 results file.
# Inputs:  _outputs/tables/bygroup_*.csv (16, 17); events.parquet for Feb 2019
#          daily counts (one light scan).
# Outputs: _outputs/tables/bygroup_suppl_*.csv; quality_reports/gate1/2026-11-11_wpA_results.md
# Log:     11/11/2026: created (Phase 3, WP-A supplement).
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(here::here("projects/03_family_homes/scripts/R/00_groups.R"))
source(here::here("projects/03_family_homes/scripts/R/00_group_helpers.R"))
library(knitr)

panel_path <- events_panel_path
stopifnot(file_exists(panel_path))
res_file <- path(project_dir, "quality_reports", "gate1", "2026-11-11_wpA_results.md")
T <- function(f) path(tables_out_dir, f)
k <- function(d, ...) paste(kable(d, format = "pipe", ...), collapse = "\n")
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")
# owner_retained mixes refinance retitles with a parent adding a child to title
mixed_label <- c(owner_retained = "owner_retained (retitle, mixed)")
lab <- function(x) ifelse(x %in% names(mixed_label), mixed_label[x], x)

# ---- 1. deadline discontinuity ----
# Signing date (sale_raw), primary: all calendar days, mean 16-28 Feb over mean 1-15 Feb.
# Recording date (rec_raw): the Board of Equalization treats the recording date as the date of
# transfer, and 15 Feb 2021 was a holiday, so deeds recorded through 16 Feb could still qualify:
# recording series use 17-28 Feb over 1-16 Feb, business days. Signing sensitivity: 17-28 over 1-16.
g1 <- c("main_distinct","main_distinct_hi","main_noaf_trust_heir","main_proxy","main_noaf","trust_heir_chain","lifetime_pc",
        "trust_to_other","trust_to_self","owner_retained","into_own_trust","spouse_ca",
        "death_affidavit_all","market_sale","main_noaf_trust_heir_xcs","cross_surname","main_noaf_trust_heir_xor","main_noaf_trust_heir_xore",
        "main_owner_retained","main_unrelated_entity")
g_daily <- c("trust_heir_chain","main_distinct","lifetime_pc","trust_to_self","spouse_ca")
con <- open_corelogic_duckdb(memory_limit = "12GB")
dir_create("C:/duckdb_spill_wpA")
invisible(dbExecute(con, "PRAGMA temp_directory='C:/duckdb_spill_wpA'"))
src <- glue("read_parquet('{sql_quote_path(panel_path)}')")
to_long <- function(w, key, yr) w %>% filter(!state %in% territories) %>%
  pivot_longer(starts_with("g_"), names_to = "group", names_prefix = "g_", values_to = "n") %>%
  mutate(state_class = if_else(state == "CA", "CA", "other"), year = yr) %>%
  rename(day_raw = all_of(key)) %>%
  group_by(group, state_class, day_raw, year) %>% summarise(n = sum(as.numeric(n)), .groups = "drop")
scan <- function(col, lo, hi, yr) to_long(bygroup_count_query(
  con, src, where = glue("{col} BETWEEN {lo} AND {hi}"), by = c("state", col)), col, yr)
d19 <- scan("sale_raw", 20190201, 20190228, 2019L)
r19 <- scan("rec_raw", 20190201, 20190228, 2019L)
r21 <- scan("rec_raw", 20210201, 20210228, 2021L)
dbDisconnect(con, shutdown = TRUE)
chain_g <- bygroup_chain_groups   # chain groups: 2017 onward only (look-back); 2019 and 2021 are fine
d21 <- read_csv(T("bygroup_daily_20210101_20210415.csv"), show_col_types = FALSE) %>%
  filter(sale_raw >= 20210201, sale_raw <= 20210228) %>% transmute(group, state_class, day_raw = sale_raw, year = 2021L, n)
holidays <- as.Date(c("2019-02-18", "2021-02-15"))
prep <- function(d, split = 15L) d %>%
  mutate(date = as.Date(as.character(day_raw), "%Y%m%d"),
         half = if_else(as.integer(format(date, "%d")) <= split, "a_pre", "b_post"),
         weekday = !format(date, "%u") %in% c("6", "7") & !date %in% holidays)
ratio_tbl <- function(d, wd_only, basis) {
  d %>% filter(!wd_only | weekday) %>% group_by(group, state_class, year, half) %>%
    summarise(m = mean(n), .groups = "drop") %>% pivot_wider(names_from = half, values_from = m) %>%
    mutate(ratio = b_post / a_pre, basis = basis)
}
sign15 <- prep(bind_rows(d21, d19), 15L); sign16 <- prep(bind_rows(d21, d19), 16L)
rec16 <- prep(bind_rows(r19, r21), 16L)
disc <- bind_rows(
  ratio_tbl(sign15, FALSE, "signing 16-28 over 1-15, all days (primary)"),
  ratio_tbl(sign15, TRUE, "signing 16-28 over 1-15, weekdays (sensitivity)"),
  ratio_tbl(sign16, FALSE, "signing 17-28 over 1-16, all days (sensitivity)"),
  ratio_tbl(rec16, TRUE, "recording 17-28 over 1-16, business days"))
write_csv_strict(disc, T("bygroup_suppl_deadline_discontinuity.csv"))
rw <- function(b, nm) disc %>% filter(basis == b, group %in% g1) %>%
  mutate(col = paste(state_class, year)) %>% select(group, col, ratio) %>%
  pivot_wider(names_from = col, values_from = ratio) %>%
  rename_with(~ paste0(nm, " ", .x), -group)
tab1 <- rw("signing 16-28 over 1-15, all days (primary)", "sign") %>%
  left_join(rw("signing 17-28 over 1-16, all days (sensitivity)", "sign16") %>% select(group, `sign16 CA 2021`), by = "group") %>%
  left_join(rw("signing 16-28 over 1-15, weekdays (sensitivity)", "wkday") %>% select(group, `wkday CA 2021`), by = "group") %>%
  left_join(rw("recording 17-28 over 1-16, business days", "rec"), by = "group") %>%
  transmute(group = lab(group), `sign CA 2021`, `sign16 CA 2021`, `sign CA 2019`, `sign other 2021`,
            `wkday CA 2021`, `rec CA 2021`, `rec CA 2019`, `rec other 2021`) %>%
  arrange(match(sub(" (retitle, mixed)", "", group, fixed = TRUE), g1))
# daily counts 10-19 Feb 2021 by signing and by recording date
dly <- bind_rows(
  sign15 %>% filter(year == 2021, between(day_raw, 20210210, 20210219)) %>% mutate(kind = "signing"),
  rec16 %>% filter(year == 2021, between(day_raw, 20210210, 20210219)) %>% mutate(kind = "recording"))
write_csv_strict(dly %>% select(kind, group, state_class, day_raw, n), T("bygroup_suppl_daily_10_19feb2021.csv"))
daily_tab <- function(kind_) dly %>% filter(kind == kind_, state_class == "CA", group %in% g_daily) %>%
  mutate(day = format(date, "%a %d")) %>% select(group, day, n) %>%
  pivot_wider(names_from = day, values_from = n) %>% arrange(match(group, g_daily))
tab1b <- daily_tab("signing"); tab1c <- daily_tab("recording")

# ---- 2. is the gap closing ----
g2 <- c("main_distinct","main_distinct_hi","main_proxy","main_noaf","main_noaf_trust_heir","trust_heir_chain",
        "lifetime_pc","trust_to_other","owner_retained","trust_to_self")
es <- read_csv(T("bygroup_event_study_monthly.csv"), show_col_types = FALSE) %>% filter(group %in% g2)
per <- function(d, a, b) d %>% filter(ym >= a, ym <= b)
es_tab <- map_dfr(g2, function(g) {
  d <- es %>% filter(group == g)
  f <- function(a, b) { x <- per(d, a, b); c(mean(x$estimate), mean(x$std.error)) }
  p1 <- f(202103, 202112); p2 <- f(202201, 202212); p3 <- f(202301, 202312)
  tibble(group = lab(g), mar_dec_2021 = p1[1], se_mar_dec_2021 = p1[2],
         y2022 = p2[1], se_2022 = p2[2], y2023 = p3[1], se_2023 = p3[2])
})
cf <- read_csv(T("bygroup_cf_monthly.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", group %in% g2) %>%
  group_by(group) %>% summarise(
    cf_gap_2024h1_logpts = log(sum(ca_actual[ym >= 202401 & ym <= 202406]) /
                                 sum(cf[ym >= 202401 & ym <= 202406])),
    cf_gap_2023_logpts = log(sum(ca_actual[ym >= 202301 & ym <= 202312]) /
                               sum(cf[ym >= 202301 & ym <= 202312])), .groups = "drop") %>%
  mutate(group = lab(group))
es_tab <- left_join(es_tab, cf, by = "group")
write_csv_strict(es_tab, T("bygroup_suppl_gap_closing.csv"))

# ---- 3. standalone trust groups ----
g3 <- c("trust_lo","trust_hi","into_trust_others","into_own_trust","trust_to_trust")
ddd <- read_csv(T("bygroup_ddd.csv"), show_col_types = FALSE)
tab3 <- ddd %>% filter(group %in% g3) %>%
  select(group, did, did_se, ddd, ddd_se, nobs, n_states, n_states_dropped, ca_deeds_pre, ca_deeds_post) %>%
  arrange(factor(group, g3))
comp <- ddd %>% filter(group %in% c("main_noaf","main_noaf_trust_lo","main_noaf_trust_hi","main_noaf_trust_t2p")) %>%
  select(group, ddd, ddd_se, nobs, n_states)

# ---- 4. California-only descriptive log changes ----
cnt <- read_csv(T("bygroup_state_month_counts.csv"), show_col_types = FALSE) %>%
  filter(state == "CA", between(sale_year, 2017L, 2023L)) %>%
  group_by(group, sale_year) %>% summarise(n = sum(n), .groups = "drop")
ann <- cnt %>% pivot_wider(names_from = sale_year, values_from = n, names_prefix = "y")
lc <- ann %>% mutate(
  mean_2017_19 = (y2017 + y2018 + y2019) / 3, mean_2022_23 = (y2022 + y2023) / 2,
  lchg_vs_2017_19 = log(mean_2022_23 / mean_2017_19), lchg_vs_2019 = log(mean_2022_23 / y2019))
sp <- lc %>% filter(group == "spouse_ca"); mk <- lc %>% filter(group == "market_sale")
lc <- lc %>% mutate(
  minus_spouse_vs_2017_19 = lchg_vs_2017_19 - sp$lchg_vs_2017_19,
  minus_market_vs_2017_19 = lchg_vs_2017_19 - mk$lchg_vs_2017_19,
  minus_spouse_vs_2019 = lchg_vs_2019 - sp$lchg_vs_2019,
  minus_market_vs_2019 = lchg_vs_2019 - mk$lchg_vs_2019,
  base_note = if_else(group %in% c("main_allaf","death_affidavit_all", "main_proxy"),
                      "2017-19 base contains the 2018 vendor break", ""),
  group = lab(group))
write_csv_strict(lc %>% select(group, mean_2017_19, y2019, mean_2022_23, starts_with("lchg"),
                               starts_with("minus"), base_note), T("bygroup_suppl_ca_log_changes.csv"))

# ---- 5. California classes: annual averages ----
cls <- read_csv(T("bygroup_ca_class_before_after.csv"), show_col_types = FALSE) %>%
  transmute(cell, `avg 2017-19` = mean_2017_19, `2021` = y2021, `avg 2022-23` = mean_2022_23)
write_csv_strict(cls, T("bygroup_suppl_ca_class_averages.csv"))

# ---- write the Supplement section (replace if present) ----
marker <- "## Supplement (cuts requested before Gate 1)"
old <- readLines(res_file, warn = FALSE)
if (any(old == marker)) old <- old[seq_len(which(old == marker)[1] - 1L)]
sup <- c(marker, "",
 "No new regressions. `owner_retained` is labelled \"retitle, mixed\": a parent adding a child to title is also \"buyer is also the seller\", so it is not a clean refinance placebo (the group list keeps its old role string; frozen file). Scripts: `17b_bygroup_supplement.R`; outputs `bygroup_suppl_*.csv`.",
 "",
 "### S1. Deadline discontinuity",
 "Source: `bygroup_suppl_deadline_discontinuity.csv` (2021 signing counts from `bygroup_daily_20210101_20210415.csv`; 2019 signing and all recording counts from scans of `events.parquet`). **Signing date (`sale_raw`), primary: 16-28 Feb over 1-15 Feb, all calendar days.** `sign16` moves 16 Feb to the pre side (17-28 over 1-16), a sensitivity. **Recording date (`rec_raw`): 17-28 Feb over 1-16 Feb, business days**, because the Board of Equalization treats the recording date as the transfer date and 15 Feb 2021 was a holiday, so deeds recorded through 16 Feb could still qualify. `wkday` = signing, weekdays only. Ratio below 1 = fewer deeds per day after the deadline. `spouse_ca` is California-only.",
 k(tab1, digits = 2), "",
 "Daily counts, California, 10-19 February 2021, by SIGNING date (Mon 15 Feb is the holiday):",
 k(tab1b), "",
 "Daily counts, California, 10-19 February 2021, by RECORDING date (a spike on Tue 16 Feb would show families used the extra day):",
 k(tab1c), "",
 "### S2. Is the gap closing? Event-study mean coefficients (log points, California relative to other states)",
 "Source: `bygroup_event_study_monthly.csv`. The `se_*` columns are the average of the monthly standard errors, not the standard error of the period mean (the saved output has no covariance between months). The event study ends in December 2023; January-June 2024 is not in it, so the last two columns are log(actual / counterfactual) from the 2019-base counterfactual (`bygroup_cf_monthly.csv`), a different scale (not relative to the 2018-19 pooled reference).",
 k(es_tab, digits = 3), "",
 "### S3. Triple difference, standalone trust groups",
 "Source: `bygroup_ddd.csv`. The sentence \"neither trust bound shows a decline\" referred to the standalone bounds `trust_lo` and `trust_hi` (rows below). The composites `main_noaf_trust_*` do decline, because they contain `main_noaf` (second table).",
 k(tab3, digits = 3, format.args = list(big.mark = ",")), "",
 k(comp, digits = 3), "",
 "### S4. California-only log changes in annual deeds (descriptive; no standard errors)",
 "Source: `bygroup_suppl_ca_log_changes.csv` (from `bygroup_state_month_counts.csv`). Log change of the 2022-23 mean annual count against the 2017-19 mean and against 2019; then minus the spouse change and minus the market-sale change. Groups with a 2018-break base are noted.",
 k(lc %>% select(group, mean_2017_19, mean_2022_23, lchg_vs_2017_19, lchg_vs_2019,
                 minus_spouse_vs_2017_19, minus_market_vs_2017_19, minus_spouse_vs_2019,
                 minus_market_vs_2019, base_note), digits = 3, format.args = list(big.mark = ",")), "",
 "### S5. California deed classes: average annual deeds",
 "Source: `bygroup_suppl_ca_class_averages.csv` (from `bygroup_ca_class_before_after.csv`).",
 k(cls, digits = 0, format.args = list(big.mark = ",")), "")
writeLines(c(old, sup), res_file)
message("Supplement written: ", res_file)
