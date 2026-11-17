# ============================================================
# 23: Counterfactual in years without a reform, netted rush and shortfall, 2017-19 dynamics
# Author: Saani Rawat
# Purpose: JUE review items A2 (domain referee, Concern 2) and B2 (methods referee, Concern 2).
#          (a) The paper's monthly counterfactual -- California's count in the same calendar
#              month of a base year, scaled by the other states' change -- run as if a deadline
#              fell in February 2017, 2018 or 2019 with no reform. Base year = deadline year - 2
#              (2019 for the real 2021 deadline), rush = June of the year before through February,
#              shortfall = minus the excess from March on, over 12, 24, 36 and 40 months. Same
#              arithmetic as bg_cf() in 17_bygroup_designs.R (r1_cf()). Placebo horizons are
#              kept only while they end by May 2020 (anticipation starts June 2020).
#              Years used: the main measure needs a complete chain look-back in its base year,
#              so main_distinct is run only for the 2019 placebo (base 2017); the main measure
#              without chain heirs (main_distinct_noheir) excludes affidavits of death and
#              needs no look-back, so the 2018 affidavit coding break does not touch it and it
#              runs for all three placebo years. All affidavits of death cross the 2018 break in
#              every placebo base year (2015-2017), so their placebo rows are flagged and unused.
#          (b) The real rush and shortfall net of untargeted California series: spouse deeds,
#              all affidavits of death and market sales. Within California: the group's 2019
#              month scaled by the series' own change since 2019. Cross-state (as the paper's
#              market adjustment): the counterfactual scaled by the series' California gap.
#          (c) Monthly event study 2017-2023 (state + month fixed effects, state-clustered, as
#              17 section E) with every month estimated relative to one month and re-centred
#              on the 2017-19 mean; California's own pre-period dispersion = SD of the 36
#              monthly 2017-19 coefficients; post-period coefficients in those SD units.
# Inputs:  events.parquet (one scan: state-month counts 2013-2024);
#          _outputs/tables/bygroup_state_month_counts.csv, bygroup_rush_hole.csv,
#          bygroup_event_study_monthly.csv (16, 17; reproduction checks only).
# Outputs: _outputs/tables/r1_state_month_counts_2013_2024.csv, r1_ca_annual_2013_2023.csv,
#          r1_cf_placebo.csv, r1_cf_netted_2021.csv, r1_es2017_monthly.csv, r1_es2017_summary.csv;
#          _outputs/figures/FR1_cf_placebo_paths.png, FR1_event_study_2017_sd.png;
#          RONE macros (script "23") in _outputs/r1_numbers.tex.
# Run:     Rscript projects/03_family_homes/scripts/R/23_pretrend_cf_placebo.R
# Log:     11/13/2026: created (R1 of plans/2026-11-12_review-analyses.md).
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_group_helpers.R"))
source(path(r_dir, "00_r1_helpers.R"))
suppressPackageStartupMessages(library(fixest))

set.seed(20261112)

log_file <- path(logs_dir, "23_pretrend_cf_placebo.log")
sink(log_file, split = TRUE)
message("Starting 23_pretrend_cf_placebo at ", Sys.time())
TT <- function(f) path(tables_out_dir, f)
FF <- function(f) path(figures_out_dir, f)

g23 <- c("market_sale", "main_distinct", "main_distinct_noheir", "lifetime_pc", "trust_heir_chain",
         "death_affidavit_all", "spouse_ca", "main_noaf", "owner_retained", "trust_to_self")
chain_g <- intersect(g23, bygroup_chain_groups)
code <- c(main_distinct = "Main", main_distinct_noheir = "MainNoheir", lifetime_pc = "Pc",
          trust_heir_chain = "Heir", market_sale = "Market", death_affidavit_all = "Affidavit",
          spouse_ca = "Spouse", main_noaf = "Noaf", owner_retained = "Retitle", trust_to_self = "Settlor")
lab <- c(main_distinct = "Section 4 measure", main_distinct_noheir = "Section 4 measure, no chain heirs",
         lifetime_pc = "Parent-child lifetime", trust_heir_chain = "Chain-verified trust-to-heir",
         market_sale = "Market sales")
end_main <- bygroup_windows$hole_end_main          # 202406
last_clean_placebo <- prop19_pre_end_ym             # 202005

# ============================================================
# 0. State-month counts 2013-2024 (one scan) and checks ----
# ============================================================
terr_sql <- paste0("'", r1_territories, "'", collapse = ",")
con <- r1_open_duckdb()
src <- glue("(SELECT * FROM read_parquet('{sql_quote_path(events_panel_path)}')
              WHERE state NOT IN ({terr_sql}))")
t0 <- Sys.time()
counts_wide <- bygroup_count_query(con, src, names = g23, by = c("state", "ym", "sale_year"),
                                   where = "sale_year BETWEEN 2013 AND 2024")
dbDisconnect(con, shutdown = TRUE)
message("Monthly scan: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")
counts_all <- counts_wide %>%
  pivot_longer(starts_with("g_"), names_to = "group", names_prefix = "g_", values_to = "n") %>%
  mutate(n = as.numeric(n), ym = as.integer(ym), sale_year = as.integer(sale_year)) %>%
  arrange(group, state, ym)
stopifnot(n_distinct(counts_all$state) == 51L, "CA" %in% counts_all$state)
write_csv_strict(counts_all, TT("r1_state_month_counts_2013_2024.csv"))
# Reproduction: identical to 16's cube on 2016-2024 for every group both files carry
cube <- read_csv(TT("bygroup_state_month_counts.csv"), show_col_types = FALSE) %>%
  filter(group %in% g23)
chk <- full_join(cube %>% select(state, ym, group, n_cube = n),
                 counts_all %>% filter(sale_year >= 2016L, group %in% unique(cube$group)) %>%
                   select(state, ym, group, n_new = n), by = c("state", "ym", "group"))
stopifnot(nrow(chk) > 1e4, all(!is.na(chk$n_cube)), all(!is.na(chk$n_new)),
          all(chk$n_cube == chk$n_new))
message("Counts equal 16's cube on ", nrow(chk), " state-month-group cells (2016-2024)")
# chain groups: the 10-year look-back is complete only from 2017
counts <- counts_all %>% filter(!(group %in% chain_g & sale_year < 2017L))

ca_annual <- counts_all %>% filter(state == "CA", sale_year <= 2023L) %>%
  group_by(group, sale_year) %>% summarise(n = sum(n), .groups = "drop") %>%
  mutate(lookback_complete = !(group %in% chain_g) | sale_year >= 2017L) %>%
  group_by(group) %>% mutate(log_change = log(n / lag(n))) %>% ungroup()
write_csv_strict(ca_annual, TT("r1_ca_annual_2013_2023.csv"))
print(ca_annual %>% select(group, sale_year, n) %>%
        pivot_wider(names_from = sale_year, values_from = n), width = 250)

# ============================================================
# (a) + (b) Counterfactual in placebo years and netted versions ----
# ============================================================
#' Within-California counterfactual: group's base-year month scaled by series X's own change
cf_within <- function(g, x, base_year) {
  ca <- function(gg) counts %>% filter(group == gg, state == "CA") %>%
    group_by(ym, sale_year) %>% summarise(n = sum(n), .groups = "drop") %>% mutate(m = ym %% 100L)
  a <- ca(g); xx <- ca(x)
  base_a <- a %>% filter(sale_year == base_year) %>% select(m, a_base = n)
  base_x <- xx %>% filter(sale_year == base_year) %>% select(m, x_base = n)
  stopifnot(nrow(base_a) == 12L, nrow(base_x) == 12L, all(base_x$x_base > 0))
  a %>% select(ym, m, ca_actual = n) %>%
    left_join(xx %>% select(ym, x_actual = n), by = "ym") %>%
    left_join(base_a, by = "m") %>% left_join(base_x, by = "m") %>%
    mutate(cf = a_base * x_actual / x_base, excess = ca_actual - cf) %>% arrange(ym)
}
#' Rush, Jan-Feb ratio and shortfall over horizons from one monthly counterfactual path
summarise_path <- function(p, Y, last_ok) {
  rush_from <- (Y - 1L) * 100L + 6L; rush_to <- Y * 100L + 2L
  hole_from <- Y * 100L + 3L
  base <- tibble(rush = r1_wsum(p, "excess", rush_from, rush_to),
                 rush_cf = r1_wsum(p, "cf", rush_from, rush_to),
                 janfeb_ratio = r1_wsum(p, "ca_actual", Y * 100L + 1L, rush_to) /
                   r1_wsum(p, "cf", Y * 100L + 1L, rush_to),
                 nine_ratio = r1_wsum(p, "ca_actual", rush_from, rush_to) / r1_wsum(p, "cf", rush_from, rush_to))
  hz <- map_dfr(c(12L, 24L, 36L, 40L), function(h) {
    to <- r1_add_months(hole_from, h - 1L)
    ok <- to <= last_ok
    tibble(horizon = h, hole_end = to, clean = ok,
           shortfall = if (ok) -r1_wsum(p, "excess", hole_from, to) else NA_real_,
           shortfall_cf = if (ok) r1_wsum(p, "cf", hole_from, to) else NA_real_)
  })
  bind_cols(base, hz) %>%
    mutate(rush_pct = 100 * rush / rush_cf, shortfall_pct = 100 * shortfall / shortfall_cf)
}
deadline_years <- c(2017L, 2018L, 2019L, 2021L)
cf_groups <- c("main_distinct", "main_distinct_noheir", "lifetime_pc", "trust_heir_chain",
               "main_noaf", "owner_retained", "trust_to_self", "death_affidavit_all", "market_sale")
rows <- list(); paths <- list()
for (Y in deadline_years) {
  B <- Y - 2L
  last_ok <- if (Y == 2021L) end_main else last_clean_placebo
  mkt <- r1_cf(counts, "market_sale", B, B * 100L + 1L, end_main) %>%
    transmute(ym, mkt_factor = ca_actual / cf)
  aff <- if (B >= 2019L) r1_cf(counts, "death_affidavit_all", B, B * 100L + 1L, end_main) %>%
    transmute(ym, aff_factor = ca_actual / cf) else NULL
  for (g in cf_groups) {
    if (g %in% chain_g && B < 2017L) next             # base year needs a complete look-back
    p <- r1_cf(counts, g, B, B * 100L + 1L, end_main) %>% left_join(mkt, by = "ym")
    variants <- list(
      cf = p,
      cross_net_market = p %>% mutate(cf = cf * mkt_factor, excess = ca_actual - cf))
    if (!is.null(aff) && g != "death_affidavit_all") {
      variants$cross_net_affidavit <- p %>% left_join(aff, by = "ym") %>%
        mutate(cf = cf * aff_factor, excess = ca_actual - cf)
    }
    for (x in c("spouse_ca", "death_affidavit_all", "market_sale")) {
      if (x == g) next
      variants[[paste0("within_net_", sub("_ca$|_all$|_sale$", "", x))]] <- cf_within(g, x, B)
    }
    for (v in names(variants)) {
      pv <- variants[[v]] %>% filter(ym >= B * 100L + 1L, ym <= end_main)
      rows[[length(rows) + 1L]] <- summarise_path(pv, Y, last_ok) %>%
        mutate(group = g, deadline_year = Y, base_year = B, method = v, .before = 1)
      if (v %in% c("cf", "cross_net_market")) {
        paths[[length(paths) + 1L]] <- pv %>% filter(ym >= (Y - 1L) * 100L + 6L, ym <= last_ok) %>%
          transmute(group = g, deadline_year = Y, method = v, ym,
                    month_rel = (ym %/% 100L - Y) * 12L + ym %% 100L - 2L,   # 0 = February of Y
                    excess, cf)
      }
    }
  }
}
cfp <- bind_rows(rows) %>%
  mutate(placebo = deadline_year != 2021L,
         # all affidavits of death break in 2018 in California: unusable in any placebo base year,
         # and as a netting series wherever the base year precedes 2019
         affidavit_break = (group == "death_affidavit_all" | grepl("affidavit", method)) & base_year < 2019L,
         lookback_note = if_else(group %in% chain_g, "chain heirs: base year >= 2017 only", "")) %>%
  arrange(group, method, deadline_year, horizon)
write_csv_strict(cfp, TT("r1_cf_placebo.csv"))
paths <- bind_rows(paths)

# Reproduction of the paper's 2021 numbers (17, bygroup_rush_hole.csv, base 2019, June 2024)
hole <- read_csv(TT("bygroup_rush_hole.csv"), show_col_types = FALSE) %>%
  filter(base_window == "2019", end_label == "main_202406",
         group %in% c("main_distinct", "lifetime_pc", "trust_heir_chain", "main_noaf"))
mine <- cfp %>% filter(deadline_year == 2021L, horizon == 40L, method %in% c("cf", "cross_net_market")) %>%
  select(group, method, rush, shortfall) %>%
  pivot_wider(names_from = method, values_from = c(rush, shortfall))
rc <- inner_join(hole, mine, by = "group") %>%
  mutate(d1 = abs(rush_cf - rush_excess), d2 = abs(shortfall_cf - shortfall),
         d3 = abs(rush_cross_net_market - rush_excess_adj), d4 = abs(shortfall_cross_net_market - shortfall_adj))
print(rc %>% select(group, rush_excess, rush_cf, shortfall, shortfall_cf, shortfall_adj, shortfall_cross_net_market))
stopifnot(nrow(rc) == 4L, all(c(rc$d1, rc$d2, rc$d3, rc$d4) < 1e-6))
message("Reproduced the paper's rush and shortfall (raw and net of market) for 4 groups")

# Netted 2021 table (b)
net21 <- cfp %>% filter(deadline_year == 2021L) %>%
  select(group, method, horizon, hole_end, rush, rush_pct, janfeb_ratio, nine_ratio,
         shortfall, shortfall_pct) %>%
  arrange(match(group, cf_groups), method, horizon)
write_csv_strict(net21, TT("r1_cf_netted_2021.csv"))
print(net21 %>% filter(horizon == 40L, group %in% c("main_distinct", "lifetime_pc", "trust_heir_chain")) %>%
        select(group, method, rush, rush_pct, janfeb_ratio, shortfall, shortfall_pct), n = 40, width = 200)
print(cfp %>% filter(horizon == 12L, method %in% c("cf", "cross_net_market", "within_net_spouse"),
                     group %in% c("main_distinct", "main_distinct_noheir", "lifetime_pc", "trust_heir_chain")) %>%
        select(group, method, deadline_year, rush, rush_pct, janfeb_ratio, shortfall, shortfall_pct),
      n = 60, width = 200)

# Figure: cumulative excess from June of the year before, placebo years against 2021
pf <- paths %>% filter(group %in% c("main_distinct", "main_distinct_noheir", "lifetime_pc", "trust_heir_chain")) %>%
  arrange(group, method, deadline_year, ym) %>%
  group_by(group, method, deadline_year) %>%
  mutate(cum_pct = 100 * cumsum(excess) / mean(cf)) %>% ungroup() %>%
  mutate(label = factor(lab[group], levels = lab[c("main_distinct", "main_distinct_noheir", "lifetime_pc", "trust_heir_chain")]),
         method = if_else(method == "cf", "Counterfactual (paper)", "Net of market sales"),
         year_lab = factor(deadline_year))
p_cf <- ggplot(pf, aes(month_rel, cum_pct, color = year_lab)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey30") +
  geom_line(linewidth = 0.6) +
  scale_color_manual(values = c(`2017` = "grey65", `2018` = palette_paper()[1], `2019` = palette_paper()[3],
                                `2021` = palette_paper()[2])) +
  facet_grid(method ~ label) +
  labs(x = "Months after February of the deadline year (placebo years stop at May 2020)",
       y = "Cumulative excess, in months of mean counterfactual", color = "Deadline (Feb)") +
  theme(legend.position = "bottom")
ggsave(FF("FR1_cf_placebo_paths.png"), p_cf, width = 11, height = 6.5, dpi = 300, bg = "transparent")

# ============================================================
# (c) Monthly event study 2017-2023, re-centred on the 2017-19 mean ----
# ============================================================
es_groups <- c("main_distinct", "lifetime_pc", "trust_heir_chain", "main_distinct_noheir", "market_sale")
ref_month <- 201912L
pre17 <- r1_months(201701L, 201912L); pre18 <- r1_months(201801L, 201912L)
all_m <- r1_months(201701L, 202312L)
es_rows <- list(); es_sum <- list()
old_es <- read_csv(TT("bygroup_event_study_monthly.csv"), show_col_types = FALSE)
for (g in es_groups) {
  ms <- counts %>% filter(group == g, between(sale_year, 2017L, 2023L)) %>%
    select(state, ym, n_fam = n)
  panel <- ms %>% complete(state, ym = all_m, fill = list(n_fam = 0)) %>%
    mutate(ca = as.integer(state == "CA"))
  stopifnot(n_distinct(panel$state) == 51L, nrow(panel) == 51L * length(all_m))
  n_zero <- sum(panel$n_fam == 0)
  panel$lnf <- if (n_zero == 0L) log(panel$n_fam) else log1p(panel$n_fam)
  m <- feols(lnf ~ i(ym, ca, ref = ref_month) | state + ym, data = panel, cluster = ~state)
  cf_names <- paste0("ym::", all_m, ":ca")
  b <- setNames(rep(0, length(all_m)), cf_names)
  est <- coef(m); stopifnot(all(setdiff(cf_names, paste0("ym::", ref_month, ":ca")) %in% names(est)))
  b[names(est)] <- est
  V <- matrix(0, length(all_m), length(all_m), dimnames = list(cf_names, cf_names))
  vc <- vcov(m); V[rownames(vc), colnames(vc)] <- vc
  # theta = W beta: each month minus the mean of the reference months (pre-period mean = 0)
  W_of <- function(ref_set) diag(length(all_m)) - outer(rep(1, length(all_m)), as.numeric(all_m %in% ref_set)) / length(ref_set)
  W17 <- W_of(pre17); W18 <- W_of(pre18)
  th17 <- as.numeric(W17 %*% b); se17 <- sqrt(pmax(diag(W17 %*% V %*% t(W17)), 0))
  th18 <- as.numeric(W18 %*% b); se18 <- sqrt(pmax(diag(W18 %*% V %*% t(W18)), 0))
  sd17 <- sd(th17[all_m %in% pre17]); sd18 <- sd(th18[all_m %in% pre18])
  d <- tibble(group = g, ym = all_m, beta_ref_201912 = as.numeric(b), theta_1719 = th17, se_1719 = se17,
              theta_1819 = th18, se_1819 = se18, sd_units_1719 = th17 / sd17,
              transform = if (n_zero == 0L) "log" else "log1p")
  es_rows[[g]] <- d
  mm <- function(x, from, to) mean(x[d$ym >= from & d$ym <= to])
  pre_t <- (seq_along(pre17) - 1) / 12
  fit <- lm(d$theta_1719[d$ym %in% pre17] ~ pre_t)
  slope <- unname(coef(fit)[2]); sd_resid <- sd(residuals(fit))
  # linear 2017-19 trend carried forward (years since January 2017); a strong assumption over
  # four to six years, reported beside the plain numbers, not instead of them
  t_all <- (seq_along(all_m) - 1) / 12
  d <- d %>% mutate(theta_detrended = theta_1719 - (coef(fit)[1] + coef(fit)[2] * t_all))
  es_rows[[g]] <- d
  post <- d %>% filter(ym >= 202201L, ym <= 202312L)
  oe <- old_es %>% filter(group == g)
  es_sum[[g]] <- tibble(
    group = g, transform = d$transform[1], n_zero_cells = n_zero, nobs = nobs(m),
    sd_pre_1719 = sd17, sd_pre_1819 = sd18,
    pre_max_abs_sd = max(abs(d$theta_1719[d$ym %in% pre17])) / sd17,
    pre_slope_per_year = slope, sd_pre_detrended = sd_resid,
    post_theta_detrended = mean(post$theta_detrended), post_detrended_sd = mean(post$theta_detrended) / sd_resid,
    lead_2017_vs_1819 = mm(d$theta_1819, 201701L, 201712L),
    feb2021_theta = d$theta_1719[d$ym == 202102L], feb2021_sd = d$theta_1719[d$ym == 202102L] / sd17,
    janfeb2021_theta = mm(d$theta_1719, 202101L, 202102L),
    marDec2021_theta = mm(d$theta_1719, 202103L, 202112L),
    y2022_theta = mm(d$theta_1719, 202201L, 202212L), y2023_theta = mm(d$theta_1719, 202301L, 202312L),
    post_theta = mean(post$theta_1719),
    y2022_sd = mm(d$theta_1719, 202201L, 202212L) / sd17, y2023_sd = mm(d$theta_1719, 202301L, 202312L) / sd17,
    post_sd = mean(post$theta_1719) / sd17,
    post_months = nrow(post), post_months_below_2sd = sum(post$theta_1719 < -2 * sd17),
    post_months_beyond_2sd = sum(abs(post$theta_1719) > 2 * sd17),
    y2022_theta_1819 = mm(d$theta_1819, 202201L, 202212L), y2023_theta_1819 = mm(d$theta_1819, 202301L, 202312L),
    y2022_paper = if (nrow(oe)) mean(oe$estimate[oe$ym >= 202201 & oe$ym <= 202212]) else NA_real_,
    y2023_paper = if (nrow(oe)) mean(oe$estimate[oe$ym >= 202301 & oe$ym <= 202312]) else NA_real_)
}
es_tbl <- bind_rows(es_rows); es_s <- bind_rows(es_sum)
write_csv_strict(es_tbl, TT("r1_es2017_monthly.csv"))
write_csv_strict(es_s, TT("r1_es2017_summary.csv"))
print(as.data.frame(es_s), digits = 3)
# the 2018-19-centred means should sit close to the paper's (17 pools 2018-19 as its reference and
# estimates on 2018-2023; differences come only from California's reference months entering the
# month effects differently)
stopifnot(all(abs(es_s$y2022_theta_1819 - es_s$y2022_paper) < 0.02, na.rm = TRUE))
# Net of market sales, month by month (the triple-difference analogue): group minus market
mk <- es_tbl %>% filter(group == "market_sale") %>% select(ym, th_mkt = theta_1719)
es_net <- es_tbl %>% filter(group != "market_sale") %>% left_join(mk, by = "ym") %>%
  mutate(net = theta_1719 - th_mkt)
es_net_s <- es_net %>% group_by(group) %>%
  summarise(sd_pre = sd(net[ym %in% pre17]),
            slope_per_year = unname(coef(lm(net[ym %in% pre17] ~ I((seq_along(pre17) - 1) / 12)))[2]),
            feb2021_sd = net[ym == 202102L] / sd_pre,
            post_net = mean(net[ym >= 202201L]), post_sd = post_net / sd_pre,
            post_months_below_2sd = sum(net[ym >= 202201L] < -2 * sd_pre), .groups = "drop")
write_csv_strict(es_net, TT("r1_es2017_net_market_monthly.csv"))
write_csv_strict(es_net_s, TT("r1_es2017_net_market_summary.csv"))
print(as.data.frame(es_net_s), digits = 3)

pe <- es_tbl %>% filter(group %in% es_groups[1:3]) %>%
  left_join(es_s %>% select(group, sd_pre_1719), by = "group") %>%
  mutate(label = factor(lab[group], levels = lab[es_groups[1:3]]), date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)))
p_es <- ggplot(pe, aes(date, theta_1719)) +
  annotate("rect", xmin = as.Date("2017-01-01"), xmax = as.Date("2019-12-31"), ymin = -Inf, ymax = Inf, fill = "grey93") +
  geom_ribbon(aes(ymin = -2 * sd_pre_1719, ymax = 2 * sd_pre_1719), fill = palette_paper()[1], alpha = 0.15) +
  geom_ribbon(aes(ymin = -sd_pre_1719, ymax = sd_pre_1719), fill = palette_paper()[1], alpha = 0.2) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_line(color = palette_paper()[2], linewidth = 0.5) + geom_point(color = palette_paper()[2], size = 0.7) +
  geom_vline(xintercept = prop19_event_dates[["effective"]], linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = prop19_event_dates[["ballot"]], linetype = "dotted", color = "grey30") +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "California relative to other states, log points (2017-19 mean = 0)\nbands: 1 and 2 SD of the 2017-19 monthly coefficients")
ggsave(FF("FR1_event_study_2017_sd.png"), p_es, width = 9, height = 8, dpi = 300, bg = "transparent")

# ============================================================
# RONE macros ----
# ============================================================
S_cf <- "r1_cf_placebo.csv"; S_es <- "r1_es2017_summary.csv"
yw <- c(`2017` = "Seventeen", `2018` = "Eighteen", `2019` = "Nineteen", `2021` = "TwentyOne")
hw <- c(`12` = "Twelve", `24` = "TwentyFour", `36` = "ThirtySix", `40` = "Forty")
mw <- c(cf = "", cross_net_market = "NetMkt", within_net_spouse = "WNetSpouse",
        within_net_death_affidavit = "WNetAff", within_net_market = "WNetMkt",
        cross_net_affidavit = "NetAff")
stopifnot(all(unique(cfp$method) %in% names(mw)))
nums <- list()
add <- function(...) nums[[length(nums) + 1L]] <<- r1_number(...)
use <- cfp %>% filter(group %in% c("main_distinct", "main_distinct_noheir", "lifetime_pc", "trust_heir_chain"),
                      !affidavit_break)
for (i in seq_len(nrow(use))) {
  r <- use[i, ]
  stem <- paste0("RONECf", code[[r$group]], yw[[as.character(r$deadline_year)]], mw[[r$method]])
  if (r$horizon == 12L) {        # rush quantities do not depend on the horizon: write once
    add(paste0(stem, "Rush"), r$rush, S_cf, 0)
    add(paste0(stem, "RushPct"), r$rush_pct, S_cf, 1)
    add(paste0(stem, "JanFeb"), r$janfeb_ratio, S_cf, 2)
  }
  if (isTRUE(r$clean)) {
    add(paste0(stem, "Short", hw[[as.character(r$horizon)]]), r$shortfall, S_cf, 0)
    add(paste0(stem, "Short", hw[[as.character(r$horizon)]], "Pct"), r$shortfall_pct, S_cf, 1)
  }
}
S_ca <- "r1_ca_annual_2013_2023.csv"
for (g in c("main_distinct_noheir", "death_affidavit_all")) {
  for (yy in 2017:2018) add(paste0("RONECA", code[[g]], c(`2017` = "Seventeen", `2018` = "Eighteen")[[as.character(yy)]]),
                            ca_annual$n[ca_annual$group == g & ca_annual$sale_year == yy], S_ca, 0)
}
for (g in es_groups) {
  r <- es_s %>% filter(group == g); cd <- code[[g]]
  add(paste0("RONEEs", cd, "PreSD"), r$sd_pre_1719, S_es, 3)
  add(paste0("RONEEs", cd, "PreSDEighteen"), r$sd_pre_1819, S_es, 3)
  add(paste0("RONEEs", cd, "PreMaxSD"), r$pre_max_abs_sd, S_es, 1)
  add(paste0("RONEEs", cd, "PreSlope"), r$pre_slope_per_year, S_es, 3)
  add(paste0("RONEEs", cd, "LeadSeventeen"), r$lead_2017_vs_1819, S_es, 3)
  add(paste0("RONEEs", cd, "FebTheta"), r$feb2021_theta, S_es, 3)
  add(paste0("RONEEs", cd, "FebSD"), r$feb2021_sd, S_es, 1)
  add(paste0("RONEEs", cd, "TwentyTwoTheta"), r$y2022_theta, S_es, 3)
  add(paste0("RONEEs", cd, "TwentyThreeTheta"), r$y2023_theta, S_es, 3)
  add(paste0("RONEEs", cd, "TwentyTwoSD"), r$y2022_sd, S_es, 1)
  add(paste0("RONEEs", cd, "TwentyThreeSD"), r$y2023_sd, S_es, 1)
  add(paste0("RONEEs", cd, "PostSD"), r$post_sd, S_es, 1)
  add(paste0("RONEEs", cd, "PostBelowTwoSD"), r$post_months_below_2sd, S_es, 0)
  add(paste0("RONEEs", cd, "PostTheta"), r$post_theta, S_es, 3)
  add(paste0("RONEEs", cd, "PostDetrended"), r$post_theta_detrended, S_es, 3)
  add(paste0("RONEEs", cd, "PostDetrendedSD"), r$post_detrended_sd, S_es, 1)
  if (g != "market_sale") {
    rn <- es_net_s %>% filter(group == g); S_n <- "r1_es2017_net_market_summary.csv"
    add(paste0("RONEEsNet", cd, "PreSD"), rn$sd_pre, S_n, 3)
    add(paste0("RONEEsNet", cd, "PreSlope"), rn$slope_per_year, S_n, 3)
    add(paste0("RONEEsNet", cd, "Post"), rn$post_net, S_n, 3)
    add(paste0("RONEEsNet", cd, "PostSD"), rn$post_sd, S_n, 1)
    add(paste0("RONEEsNet", cd, "FebSD"), rn$feb2021_sd, S_n, 1)
    add(paste0("RONEEsNet", cd, "PostBelowTwoSD"), rn$post_months_below_2sd, S_n, 0)
  }
}
r1_save_numbers(bind_rows(nums), "23")

message("Finished 23_pretrend_cf_placebo at ", Sys.time())
sink()
