# ============================================================
# 22: Rush placebos -- in-time placebo cutoffs, bandwidth and donut, Table 5 on one basis
# Author: Saani Rawat
# Purpose: JUE review item B1 (methods referee, Concern 1; domain referee, Concern 2 item 3).
#          (a) The deadline ratio -- deeds per business day on 17-28 February over 1-16
#              February -- at 16 February of every year 2008-2021, and at every business-day
#              split date in January-March 2021 (16 calendar days up to and including the split
#              date against the 12 after it), California and the other states pooled. Where
#              2021 falls is a comparison of California with ITSELF across years, not a rank of
#              states (no state-permutation or placebo-rank inference).
#          (b) Bandwidth and donut: the ratio for 5, 10, 15, 20 business days either side of
#              16 February, with and without dropping 12-17 February, every year 2008-2021.
#          (c) Table 5 rebuilt on one basis: California 2021 over the same calendar months of
#              2019, deeds per business day, for every group (spouse deeds included); other
#              states the same way; each ratio netted of the same ratio for spouse deeds and
#              for all affidavits of death (the netted ratios do not depend on the day basis).
# Inputs:  events.parquet (one scan: daily counts, December-May of 2007/08-2021);
#          _outputs/tables/bygroup_state_month_counts.csv (16; monthly counts for (c));
#          _outputs/tables/bygroup_suppl_deadline_discontinuity.csv, bygroup_rush_table.csv
#          (17b, 17; reproduction checks only).
# Outputs: _outputs/tables/r1_daily_dec_may_2008_2021.csv, r1_placebo_cutoffs_fixed.csv,
#          r1_placebo_cutoffs_summary.csv, r1_placebo_split_dates_2021.csv,
#          r1_placebo_split_summary.csv, r1_bandwidth_donut.csv, r1_bandwidth_donut_summary.csv,
#          r1_rush_one_basis.csv, TR1_rush_one_basis.tex;
#          _outputs/figures/FR1_placebo_years.png, FR1_placebo_split_dates.png,
#          FR1_bandwidth_donut.png; RONE macros (script "22") in _outputs/r1_numbers.tex.
# Run:     Rscript projects/03_family_homes/scripts/R/22_rush_placebos.R
# Log:     11/13/2026: created (R1 of plans/2026-11-12_review-analyses.md).
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_group_helpers.R"))
source(path(r_dir, "00_r1_helpers.R"))

set.seed(20261112)

log_file <- path(logs_dir, "22_rush_placebos.log")
sink(log_file, split = TRUE)
message("Starting 22_rush_placebos at ", Sys.time())
TT <- function(f) path(tables_out_dir, f)
FF <- function(f) path(figures_out_dir, f)

# ---- groups ----
# Placebo-cutoff groups; main_distinct_noheir is the main measure without the chain heirs,
# defined the same way before 2017 (the chain look-back is complete only from 2017).
g_disc <- c("main_distinct", "main_distinct_noheir", "lifetime_pc", "trust_heir_chain",
            "spouse_ca", "death_affidavit_all", "market_sale")
g_tab5 <- c("main_distinct", "main_distinct_hi", "main_noaf", "trust_heir_chain", "lifetime_pc",
            "spouse_ca", "owner_retained", "trust_to_self", "death_affidavit_all")
g_scan <- union(g_disc, g_tab5)
chain_g <- intersect(g_scan, bygroup_chain_groups)
lab <- c(main_distinct = "Section 4 measure", main_distinct_noheir = "Section 4 measure, no chain heirs",
         lifetime_pc = "Parent-child lifetime", trust_heir_chain = "Chain-verified trust-to-heir",
         spouse_ca = "Spouse (California)", death_affidavit_all = "All affidavits of death",
         market_sale = "Market sales", main_distinct_hi = "Trust-inclusive measure",
         main_noaf = "Proxy without death affidavits", owner_retained = "Same-person retitles (mixed)",
         trust_to_self = "Trust-to-settlor")
code <- c(main_distinct = "Main", main_distinct_noheir = "MainNoheir", lifetime_pc = "Pc",
          trust_heir_chain = "Heir", spouse_ca = "Spouse", death_affidavit_all = "Affidavit",
          market_sale = "Market", main_distinct_hi = "Upper", main_noaf = "Noaf",
          owner_retained = "Retitle", trust_to_self = "Settlor")
stopifnot(all(g_scan %in% bygroup_defs$name), all(g_scan %in% names(lab)))

# ============================================================
# 0. Daily counts by signing date (one scan) ----
# ============================================================
cal <- r1_calendar("2007-12-01", "2021-05-31")
terr_sql <- paste0("'", r1_territories, "'", collapse = ",")
con <- r1_open_duckdb()
src <- glue("(SELECT *, CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS state_class
              FROM read_parquet('{sql_quote_path(events_panel_path)}')
              WHERE state NOT IN ({terr_sql}))")
t0 <- Sys.time()
# day 00 (day unknown) cannot be placed before or after a cutoff: excluded here, as in 17b
daily_wide <- bygroup_count_query(
  con, src, names = g_scan, by = c("state_class", "sale_raw"),
  where = paste("sale_year BETWEEN 2007 AND 2021 AND sale_month IN (12, 1, 2, 3, 4, 5)",
                "AND sale_raw % 100 BETWEEN 1 AND 31"))
dbDisconnect(con, shutdown = TRUE)
message("Daily scan: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
cal_sub <- cal %>% filter(as.integer(format(date, "%m")) %in% c(12L, 1:5))
daily <- daily_wide %>%
  pivot_longer(starts_with("g_"), names_to = "group", names_prefix = "g_", values_to = "n") %>%
  mutate(n = as.numeric(n), day_raw = as.integer(sale_raw)) %>%
  select(group, state_class, day_raw, n) %>%
  right_join(expand_grid(group = g_scan, state_class = c("CA", "other"),
                         day_raw = cal_sub$day_raw), by = c("group", "state_class", "day_raw")) %>%
  mutate(n = coalesce(n, 0)) %>%
  left_join(cal_sub %>% select(day_raw, date, year, is_bday), by = "day_raw") %>%
  filter(!(group == "spouse_ca" & state_class == "other")) %>%
  arrange(group, state_class, date)
stopifnot(!anyNA(daily$date), sum(daily$n[daily$group == "main_distinct"]) > 1e5)
write_csv_strict(daily, TT("r1_daily_dec_may_2008_2021.csv"))
ser <- function(g, sc) daily[daily$group == g & daily$state_class == sc, c("date", "n")]
D <- function(y, m, d) as.Date(sprintf("%d-%02d-%02d", y, m, d))

# ---- reproduction: the paper's calendar-day ratio (17b) from these counts ----
win_mean_cal <- function(s, from, to) mean(s$n[s$date >= from & s$date <= to])
old_disc <- read_csv(TT("bygroup_suppl_deadline_discontinuity.csv"), show_col_types = FALSE) %>%
  filter(basis == "signing 17-28 over 1-16, all days (sensitivity)", group %in% g_scan,
         !(group == "spouse_ca" & state_class == "other"))   # spouse deeds: California only
chk <- old_disc %>% rowwise() %>%
  mutate(new = { s <- ser(group, state_class)
                 win_mean_cal(s, D(year, 2, 17), D(year, 2, 28)) / win_mean_cal(s, D(year, 2, 1), D(year, 2, 16)) },
         diff = abs(new - ratio)) %>% ungroup()
print(chk %>% select(group, state_class, year, ratio, new, diff), n = 50)
stopifnot(nrow(chk) >= 30L, all(chk$diff < 1e-9))
message("Reproduced the 17b calendar-day deadline ratio for ", nrow(chk), " group-year cells")

# ============================================================
# (a) Placebo cutoffs ----
# ============================================================
#' After/before ratio at a cutoff: 16 calendar days up to the cutoff day against the 12 after
ratio_at <- function(s, cut, pre_days = 16L, post_days = 12L) {
  pre <- r1_per_bday(s, cal, cut - (pre_days - 1L), cut)
  post <- r1_per_bday(s, cal, cut + 1L, cut + post_days)
  c(pre = pre, post = post, ratio = post / pre)
}
series_keys <- distinct(daily, group, state_class)
fixed <- series_keys %>% expand_grid(year = 2008:2021) %>% rowwise() %>%
  mutate(r = list(ratio_at(ser(group, state_class), D(year, 2, 16))),
         pre_per_bday = r[["pre"]], post_per_bday = r[["post"]], ratio_bday = r[["ratio"]],
         ratio_calday = { s <- ser(group, state_class)
                          win_mean_cal(s, D(year, 2, 17), D(year, 2, 28)) /
                            win_mean_cal(s, D(year, 2, 1), D(year, 2, 16)) },
         n_pre = { s <- ser(group, state_class); sum(s$n[s$date >= D(year, 2, 1) & s$date <= D(year, 2, 16)]) },
         n_post = { s <- ser(group, state_class); sum(s$n[s$date >= D(year, 2, 17) & s$date <= D(year, 2, 28)]) }) %>%
  ungroup() %>% select(-r) %>%
  mutate(lookback_complete = !(group %in% chain_g) | year >= 2017L,
         bdays_pre = vapply(year, function(y) sum(cal$is_bday[cal$date >= D(y, 2, 1) & cal$date <= D(y, 2, 16)]), 1L),
         bdays_post = vapply(year, function(y) sum(cal$is_bday[cal$date >= D(y, 2, 17) & cal$date <= D(y, 2, 28)]), 1L))
stopifnot(all(is.finite(fixed$ratio_bday[fixed$n_pre > 0])))
write_csv_strict(fixed, TT("r1_placebo_cutoffs_fixed.csv"))

#' Where 2021 falls among a series' own placebo years (1 = lowest)
place_2021 <- function(d) {
  r21 <- d$ratio_bday[d$year == 2021L]
  pl <- d %>% filter(year <= 2020L)
  plc <- pl %>% filter(lookback_complete)
  one <- function(p, tag) {
    lp <- log(p$ratio_bday)
    tibble(!!paste0("n_placebo", tag) := nrow(p),
           !!paste0("rank_2021", tag) := sum(p$ratio_bday < r21) + 1L,
           !!paste0("placebo_min", tag) := min(p$ratio_bday),
           !!paste0("placebo_max", tag) := max(p$ratio_bday),
           !!paste0("placebo_min_year", tag) := p$year[which.min(p$ratio_bday)],
           !!paste0("z_log", tag) := if (nrow(p) >= 3L) (log(r21) - mean(lp)) / sd(lp) else NA_real_)
  }
  bind_cols(tibble(ratio_2021 = r21, ratio_2019 = d$ratio_bday[d$year == 2019L],
                   ratio_calday_2021 = d$ratio_calday[d$year == 2021L]),
            one(pl, "_all"), one(plc, "_complete"))
}
placebo_sum <- fixed %>% filter(group %in% g_disc) %>% group_by(group, state_class) %>%
  group_modify(~ place_2021(.x)) %>% ungroup() %>%
  arrange(match(group, g_disc), state_class)
write_csv_strict(placebo_sum, TT("r1_placebo_cutoffs_summary.csv"))
print(placebo_sum %>% select(group, state_class, ratio_2021, rank_2021_all, n_placebo_all,
                             placebo_min_all, placebo_max_all, z_log_all, rank_2021_complete,
                             n_placebo_complete), n = 30, width = 200)

# ---- split dates in January-March 2021 ----
split_days <- cal$date[cal$is_bday & cal$date >= D(2021, 1, 1) & cal$date <= D(2021, 3, 31)]
deadline <- D(2021, 2, 16)
splits <- series_keys %>% filter(group %in% g_disc) %>% expand_grid(split = split_days) %>%
  rowwise() %>% mutate(ratio_bday = ratio_at(ser(group, state_class), split)[["ratio"]]) %>%
  ungroup() %>%
  # a split's windows straddle the 16/17 February boundary when split - 15 <= 16 Feb < split + 12
  mutate(contains_deadline = split - 15L <= deadline & deadline < split + 12L,
         is_deadline = split == deadline)
write_csv_strict(splits, TT("r1_placebo_split_dates_2021.csv"))
split_sum <- splits %>% group_by(group, state_class) %>%
  summarise(ratio_16feb = ratio_bday[is_deadline], n_splits = n(),
            rank_16feb = sum(ratio_bday < ratio_bday[is_deadline]) + 1L,
            min_date = split[which.min(ratio_bday)], min_ratio = min(ratio_bday),
            n_clean = sum(!contains_deadline),
            clean_min = min(ratio_bday[!contains_deadline]),
            clean_min_date = split[!contains_deadline][which.min(ratio_bday[!contains_deadline])],
            clean_max = max(ratio_bday[!contains_deadline]), .groups = "drop") %>%
  arrange(match(group, g_disc), state_class)
write_csv_strict(split_sum, TT("r1_placebo_split_summary.csv"))
print(split_sum, n = 30, width = 200)

# ============================================================
# (b) Bandwidth and donut ----
# ============================================================
bdays_all <- cal$date[cal$is_bday]
#' Windows of k business days either side of 16 February; donut drops 12-17 February
bw_windows <- function(y, k, donut) {
  pre_end <- if (donut) D(y, 2, 11) else D(y, 2, 16)
  post_start <- if (donut) D(y, 2, 18) else D(y, 2, 17)
  pre_b <- tail(bdays_all[bdays_all <= pre_end], k)
  post_b <- head(bdays_all[bdays_all >= post_start], k)
  stopifnot(length(pre_b) == k, length(post_b) == k)
  list(pre_from = min(pre_b), pre_to = pre_end, post_from = post_start, post_to = max(post_b))
}
bw <- series_keys %>% expand_grid(year = 2008:2021, k = c(5L, 10L, 15L, 20L), donut = c(FALSE, TRUE)) %>%
  rowwise() %>%
  mutate(w = list(bw_windows(year, k, donut)),
         n_pre = { s <- ser(group, state_class); sum(s$n[s$date >= w$pre_from & s$date <= w$pre_to]) },
         n_post = { s <- ser(group, state_class); sum(s$n[s$date >= w$post_from & s$date <= w$post_to]) },
         pre_from = w$pre_from, post_to = w$post_to) %>%
  ungroup() %>% select(-w) %>%
  mutate(ratio_bday = n_post / n_pre,          # k business days on each side: the k's cancel
         lookback_complete = !(group %in% chain_g) | year >= 2017L)
write_csv_strict(bw, TT("r1_bandwidth_donut.csv"))
bw_sum <- bw %>% filter(group %in% g_disc) %>% group_by(group, state_class, k, donut) %>%
  summarise(ratio_2021 = ratio_bday[year == 2021L],
            ratio_2019 = ratio_bday[year == 2019L],
            placebo_min = min(ratio_bday[year <= 2020L & lookback_complete]),
            placebo_max = max(ratio_bday[year <= 2020L & lookback_complete]),
            n_placebo = sum(year <= 2020L & lookback_complete),
            rank_2021 = sum(ratio_bday[year <= 2020L & lookback_complete] < ratio_2021) + 1L,
            .groups = "drop") %>%
  arrange(match(group, g_disc), state_class, donut, k)
write_csv_strict(bw_sum, TT("r1_bandwidth_donut_summary.csv"))
print(bw_sum %>% filter(state_class == "CA"), n = 60, width = 200)

# ============================================================
# (c) Table 5 on one basis: deeds per business day, 2021 over the same months of 2019 ----
# ============================================================
cnt <- read_csv(TT("bygroup_state_month_counts.csv"), show_col_types = FALSE) %>%
  filter(group %in% g_tab5, !state %in% r1_territories) %>%
  filter(!(group %in% bygroup_chain_groups & sale_year < 2017L)) %>%
  mutate(state_class = if_else(state == "CA", "CA", "other")) %>%
  group_by(group, state_class, ym, sale_year) %>% summarise(n = sum(n), .groups = "drop")
bd_month <- r1_calendar("2016-01-01", "2021-12-31") %>% group_by(ym) %>%
  summarise(bdays = sum(is_bday), cdays = n(), .groups = "drop")
cnt <- cnt %>% left_join(bd_month, by = "ym")
stopifnot(!anyNA(cnt$bdays[cnt$sale_year <= 2021L]))
rush_windows <- list(jan_feb_2021 = c(202101L, 202102L), mar_apr_2021 = c(202103L, 202104L),
                     nine_month = c(202006L, 202102L))
base_sets <- list(`2019` = 2019L, `2018` = 2018L, `2017_2019` = 2017:2019)
#' Per-business-day (and per-calendar-month) rate of one series over a window and its base
rate_ratio <- function(g, sc, win, base_years) {
  months <- r1_months(win[1], win[2])
  d <- cnt %>% filter(group == g, state_class == sc)
  w <- d %>% filter(ym %in% months)
  b <- d %>% filter(sale_year %in% base_years, (ym %% 100L) %in% (months %% 100L))
  stopifnot(nrow(w) == length(months), nrow(b) == length(months) * length(base_years))
  tibble(n_win = sum(w$n), bdays_win = sum(w$bdays), n_base = sum(b$n),
         bdays_base = sum(b$bdays), base_years_n = length(base_years),
         ratio_bday = (sum(w$n) / sum(w$bdays)) / (sum(b$n) / sum(b$bdays)),
         ratio_month = sum(w$n) / (sum(b$n) / length(base_years)))
}
rb <- expand_grid(group = g_tab5, state_class = c("CA", "other"), window = names(rush_windows),
                  base = names(base_sets)) %>%
  filter(!(group == "spouse_ca" & state_class == "other")) %>% rowwise() %>%
  mutate(rr = list(rate_ratio(group, state_class, rush_windows[[window]], base_sets[[base]]))) %>%
  ungroup() %>% unnest(rr)
# 17's raw California ratio is the calendar-month ratio: check it on the 2019 base
old_rush <- read_csv(TT("bygroup_rush_table.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", group %in% g_tab5)
chk_r <- rb %>% filter(base == "2019", state_class == "CA") %>%
  inner_join(old_rush %>% select(group, window, raw_ratio, relative_ratio), by = c("group", "window"))
stopifnot(nrow(chk_r) == length(g_tab5) * 3L, all(abs(chk_r$ratio_month - chk_r$raw_ratio) < 1e-9))
message("Reproduced 17's raw California ratio for ", nrow(chk_r), " cells")

wide <- rb %>% select(group, state_class, window, base, ratio_bday, ratio_month, n_win, n_base) %>%
  pivot_wider(names_from = state_class, values_from = c(ratio_bday, ratio_month, n_win, n_base)) %>%
  mutate(rel_bday = ratio_bday_CA / ratio_bday_other)
ref <- function(g) {
  x <- wide %>% filter(group == g) %>% select(window, base, ratio_bday_CA, rel_bday)
  names(x)[3:4] <- paste0(c("ca_", "rel_"), g)
  x
}
one_basis <- wide %>%
  left_join(ref("spouse_ca") %>% select(-rel_spouse_ca), by = c("window", "base")) %>%
  left_join(ref("death_affidavit_all"), by = c("window", "base")) %>%
  mutate(ca_net_spouse = ratio_bday_CA / ca_spouse_ca,
         ca_net_affidavit = ratio_bday_CA / ca_death_affidavit_all,
         rel_net_affidavit = rel_bday / rel_death_affidavit_all,
         affidavit_base_break = base != "2019") %>%   # all-affidavit counts break in 2018 (California)
  left_join(old_rush %>% select(group, window, paper_relative_ratio = relative_ratio,
                                paper_raw_ratio = raw_ratio), by = c("group", "window")) %>%
  mutate(paper_relative_ratio = if_else(base == "2019", paper_relative_ratio, NA_real_),
         paper_raw_ratio = if_else(base == "2019", paper_raw_ratio, NA_real_))
# deadline ratio columns (per business day, 16 February)
dlr <- fixed %>% filter(year %in% c(2019L, 2021L)) %>%
  select(group, state_class, year, ratio_bday) %>%
  pivot_wider(names_from = c(state_class, year), values_from = ratio_bday, names_prefix = "deadline_")
sp_d <- dlr$deadline_CA_2021[dlr$group == "spouse_ca"]
af_d <- dlr$deadline_CA_2021[dlr$group == "death_affidavit_all"]
af_d_rel <- af_d / dlr$deadline_other_2021[dlr$group == "death_affidavit_all"]
dlr <- dlr %>% mutate(deadline_CA_2021_over_2019 = deadline_CA_2021 / deadline_CA_2019,
                      deadline_net_spouse = deadline_CA_2021 / sp_d,
                      deadline_net_affidavit = deadline_CA_2021 / af_d,
                      deadline_rel_other = deadline_CA_2021 / deadline_other_2021,
                      deadline_rel_net_affidavit = deadline_rel_other / af_d_rel)
one_basis <- one_basis %>% left_join(dlr, by = "group") %>% arrange(match(group, g_tab5), window, base)
write_csv_strict(one_basis, TT("r1_rush_one_basis.csv"))
print(one_basis %>% filter(base == "2019") %>%
        select(group, window, ratio_bday_CA, ratio_month_CA, rel_bday, paper_relative_ratio,
               ca_net_spouse, ca_net_affidavit, rel_net_affidavit), n = 40, width = 200)

# LaTeX table (not a manuscript file): California, 2019 base
num <- function(x, d = 2) ifelse(is.na(x), "--", gsub("-", "$-$", formatC(x, format = "f", digits = d)))
tb <- one_basis %>% filter(base == "2019") %>%
  select(group, window, ratio_bday_CA, ca_net_spouse, ca_net_affidavit, deadline_CA_2021,
         deadline_CA_2021_over_2019, deadline_net_spouse, deadline_net_affidavit) %>%
  pivot_wider(names_from = window, values_from = c(ratio_bday_CA, ca_net_spouse, ca_net_affidavit))
rows <- vapply(g_tab5, function(g) {
  r <- tb[tb$group == g, ]
  paste0(paste(c(lab[[g]], num(r$ratio_bday_CA_jan_feb_2021), num(r$ratio_bday_CA_mar_apr_2021),
                 num(r$ratio_bday_CA_nine_month), num(r$ca_net_spouse_nine_month),
                 num(r$ca_net_affidavit_nine_month), num(r$deadline_CA_2021),
                 num(r$deadline_CA_2021_over_2019), num(r$deadline_net_spouse),
                 num(r$deadline_net_affidavit)), collapse = " & "), " \\\\")
}, "")
writeLines(c("\\begin{table}[!ht]\\centering", "\\caption{The deadline rush on one basis: deeds per business day}",
  "\\label{tab:r1rushonebasis}", "{\\small", "\\begin{adjustbox}{max width = \\textwidth}",
  "\\begin{tabular}{lrrrrrrrrr}", "\\toprule",
  " & \\multicolumn{5}{c}{California 2021 over same months of 2019} & \\multicolumn{4}{c}{Deadline ratio, California} \\\\",
  "\\cmidrule(lr){2-6}\\cmidrule(lr){7-10}",
  "Group & Jan--Feb & Mar--Apr & Jun--Feb & Jun--Feb net spouse & Jun--Feb net affidavits & 2021 & 2021 / 2019 & Net spouse & Net affidavits \\\\",
  "\\midrule", rows, "\\bottomrule", "\\end{tabular}", "\\end{adjustbox}}",
  paste0("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Deeds per business day ",
         "(weekdays less federal holidays; every deed signed in the window counts). Rush columns: California deeds per ",
         "business day in the 2021 window over the same calendar months of 2019. Net spouse and net affidavits divide by ",
         "the same ratio for California spouse deeds and for all California affidavits of death; these quotients do not ",
         "depend on the day basis. Deadline ratio: deeds per business day signed 17--28 February over 1--16 February. ",
         "Source: r1\\_rush\\_one\\_basis.csv (22\\_rush\\_placebos.R).}\\end{minipage}"),
  "\\end{table}"), TT("TR1_rush_one_basis.tex"))

# ============================================================
# Figures ----
# ============================================================
fig_g <- c("main_distinct", "lifetime_pc", "trust_heir_chain", "spouse_ca",
           "death_affidavit_all", "market_sale")
fy <- fixed %>% filter(group %in% fig_g) %>%
  mutate(label = factor(lab[group], levels = lab[fig_g]),
         kind = case_when(year == 2021L ~ "2021", !lookback_complete ~ "Before 2017 (chain look-back incomplete)",
                          TRUE ~ "2008-2020"),
         where = if_else(state_class == "CA", "California", "Other states"))
p_years <- ggplot(fy, aes(year, ratio_bday)) +
  geom_hline(yintercept = 1, color = "grey55", linewidth = 0.3) +
  geom_line(aes(linetype = where), color = "grey45", linewidth = 0.35) +
  geom_point(aes(shape = where, color = kind), size = 1.8) +
  scale_color_manual(values = c(`2021` = palette_paper()[2], `2008-2020` = palette_paper()[1],
                                `Before 2017 (chain look-back incomplete)` = "grey65")) +
  scale_y_log10() + scale_x_continuous(breaks = seq(2008, 2021, 2)) +
  facet_wrap(~label, ncol = 3) +
  labs(x = "Year (cutoff 16 February)", y = "Deeds per business day, 17-28 Feb over 1-16 Feb (log scale)",
       color = NULL, shape = NULL, linetype = NULL) +
  theme(legend.position = "bottom")
ggsave(FF("FR1_placebo_years.png"), p_years, width = 10, height = 6.5, dpi = 300, bg = "transparent")

fs <- splits %>% filter(group %in% fig_g) %>%
  mutate(label = factor(lab[group], levels = lab[fig_g]),
         where = if_else(state_class == "CA", "California", "Other states"))
p_split <- ggplot(fs, aes(split, ratio_bday, color = where)) +
  # shaded: split dates whose two windows straddle the 16/17 February boundary
  annotate("rect", xmin = deadline - 11, xmax = deadline + 15, ymin = 0, ymax = Inf,
           fill = "grey92") +
  geom_hline(yintercept = 1, color = "grey55", linewidth = 0.3) +
  geom_line(linewidth = 0.5) + geom_point(size = 0.8) +
  geom_vline(xintercept = deadline, linetype = "dashed", color = "grey30") +
  scale_color_manual(values = palette_paper()[c(2, 1)]) +
  scale_y_log10() +
  facet_wrap(~label, ncol = 3) +
  labs(x = "Split date, 2021 (last day of the 16-day before window)",
       y = "Deeds per business day, 12 days after over 16 days to the split (log scale)", color = NULL) +
  theme(legend.position = "bottom")
ggsave(FF("FR1_placebo_split_dates.png"), p_split, width = 10, height = 6.5, dpi = 300, bg = "transparent")

fb <- bw %>% filter(group %in% fig_g, state_class == "CA" | group != "spouse_ca") %>%
  mutate(label = factor(lab[group], levels = lab[fig_g]),
         donut_lab = if_else(donut, "Donut: 12-17 Feb dropped", "No donut"))
p_bw <- ggplot() +
  geom_hline(yintercept = 1, color = "grey55", linewidth = 0.3) +
  geom_point(data = fb %>% filter(state_class == "CA", year <= 2020L, lookback_complete),
             aes(k, ratio_bday), color = "grey60", size = 1,
             position = position_jitter(width = 0.4, height = 0, seed = 20261112)) +
  geom_line(data = fb %>% filter(year == 2021L),
            aes(k, ratio_bday, color = state_class, linetype = donut_lab), linewidth = 0.7) +
  geom_point(data = fb %>% filter(year == 2021L), aes(k, ratio_bday, color = state_class), size = 1.6) +
  scale_color_manual(values = c(CA = palette_paper()[2], other = palette_paper()[1]),
                     labels = c(CA = "California 2021", other = "Other states 2021")) +
  scale_x_continuous(breaks = c(5, 10, 15, 20)) + scale_y_log10() +
  facet_wrap(~label, ncol = 3) +
  labs(x = "Business days either side of 16 February", color = NULL, linetype = NULL,
       y = "Deeds after over deeds before (log scale); grey: California 2008-2020") +
  theme(legend.position = "bottom")
ggsave(FF("FR1_bandwidth_donut.png"), p_bw, width = 10, height = 6.5, dpi = 300, bg = "transparent")

# ============================================================
# RONE macros ----
# ============================================================
S_fx <- "r1_placebo_cutoffs_summary.csv"; S_sp <- "r1_placebo_split_summary.csv"
S_bw <- "r1_bandwidth_donut_summary.csv"; S_ob <- "r1_rush_one_basis.csv"
nums <- list()
add <- function(...) nums[[length(nums) + 1L]] <<- r1_number(...)
kword <- c(`5` = "Five", `10` = "Ten", `15` = "Fifteen", `20` = "Twenty")
for (g in g_disc) {
  cd <- code[[g]]
  r <- placebo_sum %>% filter(group == g, state_class == "CA")
  add(paste0("RONE", cd, "DeadlineBday"), r$ratio_2021, S_fx)
  add(paste0("RONE", cd, "DeadlineBdayNineteen"), r$ratio_2019, S_fx)
  add(paste0("RONE", cd, "PlaceboRank"), r$rank_2021_all, S_fx, 0)
  add(paste0("RONE", cd, "PlaceboYears"), r$n_placebo_all, S_fx, 0)
  add(paste0("RONE", cd, "PlaceboMin"), r$placebo_min_all, S_fx)
  add(paste0("RONE", cd, "PlaceboMax"), r$placebo_max_all, S_fx)
  add(paste0("RONE", cd, "PlaceboZ"), r$z_log_all, S_fx, 1)
  if (g %in% chain_g) {
    add(paste0("RONE", cd, "PlaceboRankComplete"), r$rank_2021_complete, S_fx, 0)
    add(paste0("RONE", cd, "PlaceboYearsComplete"), r$n_placebo_complete, S_fx, 0)
    add(paste0("RONE", cd, "PlaceboMinComplete"), r$placebo_min_complete, S_fx)
  }
  if (g != "spouse_ca") {
    o <- placebo_sum %>% filter(group == g, state_class == "other")
    add(paste0("RONE", cd, "OtherDeadlineBday"), o$ratio_2021, S_fx)
    add(paste0("RONE", cd, "OtherPlaceboRank"), o$rank_2021_all, S_fx, 0)
  }
  s <- split_sum %>% filter(group == g, state_class == "CA")
  add(paste0("RONE", cd, "SplitRank"), s$rank_16feb, S_sp, 0)
  add(paste0("RONE", cd, "SplitN"), s$n_splits, S_sp, 0)
  add(paste0("RONE", cd, "SplitCleanMin"), s$clean_min, S_sp)
  add(paste0("RONE", cd, "SplitCleanMax"), s$clean_max, S_sp)
  b <- bw_sum %>% filter(group == g, state_class == "CA")
  for (k in c(5L, 10L, 15L, 20L)) {
    add(paste0("RONE", cd, "Bw", kword[[as.character(k)]]), b$ratio_2021[b$k == k & !b$donut], S_bw)
    add(paste0("RONE", cd, "Bw", kword[[as.character(k)]], "Donut"), b$ratio_2021[b$k == k & b$donut], S_bw)
  }
  add(paste0("RONE", cd, "BwBelowAll"), sum(b$rank_2021 == 1L), S_bw, 0)
  add(paste0("RONE", cd, "BwPlaceboMin"), min(b$placebo_min), S_bw)
}
ob19 <- one_basis %>% filter(base == "2019")
wn <- c(jan_feb_2021 = "JanFeb", mar_apr_2021 = "MarApr", nine_month = "Nine")
for (g in g_tab5) {
  cd <- code[[g]]
  for (w in names(wn)) {
    r <- ob19 %>% filter(group == g, window == w)
    add(paste0("RONE", cd, wn[[w]], "Bday"), r$ratio_bday_CA, S_ob)
    if (g != "spouse_ca") add(paste0("RONE", cd, wn[[w]], "NetSpouse"), r$ca_net_spouse, S_ob)
    if (g != "death_affidavit_all") add(paste0("RONE", cd, wn[[w]], "NetAff"), r$ca_net_affidavit, S_ob)
    if (!g %in% c("spouse_ca", "death_affidavit_all")) {
      add(paste0("RONE", cd, wn[[w]], "RelBday"), r$rel_bday, S_ob)
      add(paste0("RONE", cd, wn[[w]], "RelNetAff"), r$rel_net_affidavit, S_ob)
    }
  }
  r <- ob19 %>% filter(group == g, window == "nine_month")
  add(paste0("RONE", cd, "DeadlineVsNineteen"), r$deadline_CA_2021_over_2019, S_ob)
  if (g != "spouse_ca") add(paste0("RONE", cd, "DeadlineNetSpouse"), r$deadline_net_spouse, S_ob)
  if (g != "death_affidavit_all") add(paste0("RONE", cd, "DeadlineNetAff"), r$deadline_net_affidavit, S_ob)
}
r1_save_numbers(bind_rows(nums), "22")

message("Finished 22_rush_placebos at ", Sys.time())
sink()
