# ============================================================
# 17: By-group re-runs, step 2 -- rush, triple difference, event study, hole
# Author: Saani Rawat
# Purpose: The existing designs, unchanged, looped over the group list:
#          Item 2: rush table (Jan-Feb 2021 alone, Mar-Apr 2021, nine months), raw
#                  California ratio and ratio relative to the same group in other
#                  states; monthly gap figures.
#          Item 3: triple difference by group (07 l.55-93: log(n) ~ ca:post:fam +
#                  ca:post | state^grp + sale_year^grp, state-clustered; DiD alone
#                  as 04 l.351: log(n_fam) ~ ca:post | state + sale_year).
#          Item 4: monthly event study by group (09b l.60-109: i(ym, ca, ref = pooled
#                  pre months) | state + ym, state-clustered, log or log1p).
#          Item 5: rush against hole -- cumulative excess and shortfall from the
#                  04 counterfactual CA(m) = CA(2019, m) * donors(m) / donors(2019, m),
#                  raw and market-adjusted, through 202406 (sensitivities 202312,
#                  202405; legacy window 202212).
#          Inference is state-clustered only: no state permutation or placebo ranks.
#          The main_proxy group must reproduce the existing outputs to 5e-5 (v2 vs v1 tie-breaks)
#          (bygroup_repro_check); on the dev panel the check is computed, not enforced.
# Inputs:  _outputs/tables/[dev/]bygroup_state_month_counts.csv (from 16)
# Outputs: bygroup_rush_table, bygroup_ddd, bygroup_event_study_{monthly,phases},
#          bygroup_rush_hole, bygroup_cum_excess_path, bygroup_dropped_states,
#          bygroup_below_threshold, bygroup_lag_diagnostic (.csv; .rds; .tex; .png)
# Run:     Dev (default):  Rscript projects/03_family_homes/scripts/R/17_bygroup_designs.R
#          Full:           BYGROUP_FULL=1 FAM_EXHIBIT_DIR=<dir> Rscript .../17_bygroup_designs.R
# Log:     11/11/2026: created (Phase 3, WP-A).
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(here::here("projects/03_family_homes/scripts/R/00_groups.R"))
source(here::here("projects/03_family_homes/scripts/R/00_group_helpers.R"))
suppressPackageStartupMessages(library(fixest))

set.seed(20261111)

# ---- panel switch (one place): dev now; full panel with BYGROUP_FULL=1 ----
bygroup_full <- identical(Sys.getenv("BYGROUP_FULL", "0"), "1")
dev_tag      <- if (bygroup_full) "" else "_dev"
expected_states <- if (bygroup_full) 51L else 3L   # dev panel: CA, VT, RI
if (bygroup_full) {
  if (!nzchar(Sys.getenv("FAM_EXHIBIT_DIR", ""))) {
    stop("Full run: set FAM_EXHIBIT_DIR so exhibits do not land in the manuscript")
  }
  bg_tab_dir <- tables_out_dir
  bg_rds_dir <- out_dir
} else {
  bg_tab_dir  <- path(tables_out_dir, "dev")
  bg_rds_dir  <- path(out_dir, "dev")
  tables_dir  <- path(out_dir, "dev", "exhibits", "tables")
  figures_dir <- path(out_dir, "dev", "exhibits", "figures")
}
for (d in c(bg_tab_dir, bg_rds_dir, tables_dir, figures_dir)) dir_create(d)

log_file <- path(logs_dir, paste0("17_bygroup_designs", dev_tag, ".log"))
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 17_bygroup_designs at ", Sys.time(), if (!bygroup_full) " (DEV PANEL)")

counts_path <- path(bg_tab_dir, "bygroup_state_month_counts.csv")
stopifnot(file_exists(counts_path))
counts <- read_csv(counts_path, show_col_types = FALSE)
# The chain groups use a 10-year look-back, complete only for deeds from 2017 on: drop 2016
# rows for them (none of the designs below uses earlier months for these groups).
counts <- counts %>% filter(!(group %in% bygroup_chain_groups & sale_year < 2017L))
states <- sort(unique(counts$state))
stopifnot("CA" %in% states, length(states) == expected_states)
message("States in panel: ", length(states))

cross_groups <- bygroup_defs$name[bygroup_defs$scope == "cross"]
reg_groups   <- setdiff(cross_groups, "market_sale")
role_of  <- setNames(bygroup_defs$role, bygroup_defs$name)
label_of <- setNames(bygroup_defs$label, bygroup_defs$name)
pre_of   <- setNames(bygroup_defs$pre_years, bygroup_defs$name)
ref_of   <- setNames(bygroup_defs$es_ref_from, bygroup_defs$name)
end_main <- bygroup_windows$hole_end_main
role_class <- function(g) {
  r <- role_of[g]
  case_when(grepl("exposed", r) ~ "Exposed", grepl("placebo", r) ~ "Refinance placebo",
            grepl("comparison", r) ~ "Comparison", TRUE ~ "Reported only / comparator")
}

# ============================================================
# A. Counterfactual by group (04 l.169-207, group-generic) ----
# ============================================================
#' CA counterfactual for one group: base_CA(m) * donors(m) / base_donors(m)
bg_cf <- function(g, base_years, end_ym = end_main) {
  d <- counts %>% filter(group == g, ym >= 201801L, ym <= end_ym)
  ca <- d %>% filter(state == "CA") %>% group_by(ym, sale_year) %>%
    summarise(ca_actual = sum(n), .groups = "drop")
  dn <- d %>% filter(state != "CA") %>% group_by(ym) %>%
    summarise(dn_actual = sum(n), .groups = "drop")
  full <- ca %>% left_join(dn, by = "ym") %>%
    mutate(dn_actual = coalesce(dn_actual, 0), sale_month = ym %% 100L)
  base <- full %>% filter(sale_year %in% base_years) %>% group_by(sale_month) %>%
    summarise(ca_base = mean(ca_actual), dn_base = mean(dn_actual), .groups = "drop")
  full %>% left_join(base, by = "sale_month") %>%
    mutate(cf = if_else(dn_base > 0, ca_base * dn_actual / dn_base, NA_real_),
           excess = ca_actual - cf,
           gap = if_else(!is.na(cf) & cf > 0, ca_actual / cf - 1, NA_real_)) %>%
    arrange(ym)
}
wsum <- function(df, col, from, to) {
  sum(df[[col]][df$ym >= from & df$ym <= to])
}
base_list <- function(g) {
  b <- list(`2019` = 2019L)
  if (identical(pre_of[[g]], 2017:2019)) b[["2018_2019"]] <- 2018:2019   # not for AF-all groups
  b
}
all_groups <- bygroup_defs$name

cf_all <- list()
for (g in all_groups) {
  for (b in names(base_list(g))) {
    cf_all[[paste(g, b, sep = "|")]] <-
      bg_cf(g, base_list(g)[[b]]) %>% mutate(group = g, base_window = b, .before = 1)
  }
}
cf_tbl <- bind_rows(cf_all)
expected_months <- length(unique(cf_tbl$ym))
stopifnot(all(count(cf_tbl, group, base_window)$n == expected_months))
write_csv_strict(cf_tbl, path(bg_tab_dir, "bygroup_cf_monthly.csv"))

# ============================================================
# B. Item 2: rush table ----
# ============================================================
rush_windows <- tribble(
  ~window,           ~from,                            ~to,
  "jan_feb_2021",    bygroup_windows$jan_feb_2021[1],  bygroup_windows$jan_feb_2021[2],
  "mar_apr_2021",    bygroup_windows$mar_apr_2021[1],  bygroup_windows$mar_apr_2021[2],
  "nine_month",      bygroup_windows$nine_month[1],    bygroup_windows$nine_month[2]
)
rush_rows <- list()
for (key in names(cf_all)) {
  cf <- cf_all[[key]]; g <- cf$group[1]; b <- cf$base_window[1]
  pre_start <- ref_of[[g]]
  pre_gap <- cf$gap[cf$ym >= pre_start & cf$ym <= prop19_pre_end_ym]
  rmspe <- sqrt(mean(pre_gap^2, na.rm = TRUE))
  for (i in seq_len(nrow(rush_windows))) {
    w <- rush_windows[i, ]
    ca_act <- wsum(cf, "ca_actual", w$from, w$to); ca_bs <- wsum(cf, "ca_base", w$from, w$to)
    dn_act <- wsum(cf, "dn_actual", w$from, w$to); dn_bs <- wsum(cf, "dn_base", w$from, w$to)
    cf_sum <- wsum(cf, "cf", w$from, w$to)
    rush_rows[[length(rush_rows) + 1L]] <- tibble(
      group = g, base_window = b, window = w$window, ca_actual = ca_act, ca_base = ca_bs,
      raw_ratio = ca_act / ca_bs, other_states_ratio = if (dn_bs > 0) dn_act / dn_bs else NA_real_,
      relative_ratio = ca_act / cf_sum, relative_pct = 100 * (ca_act / cf_sum - 1),
      pre_rmspe_pct = 100 * rmspe, n_pre_months_used = sum(!is.na(pre_gap)),
      scope = bygroup_defs$scope[bygroup_defs$name == g])
  }
}
rush_tbl <- bind_rows(rush_rows) %>%
  mutate(role = role_of[group], label = label_of[group])
# relative_ratio = actual / counterfactual (reproduces the 17.4% nine-month figure).
# ratio_of_ratios = CA raw ratio / other-state raw ratio. The two coincide for a single
# month and differ over multi-month windows (cf weights months by the CA base).
rush_tbl <- rush_tbl %>% mutate(ratio_of_ratios = raw_ratio / other_states_ratio)
write_csv_strict(rush_tbl, path(bg_tab_dir, "bygroup_rush_table.csv"))
rush_main <- rush_tbl %>% filter(base_window == "2019") %>%
  select(Group = group, Window = window, `CA base` = ca_base, `CA ratio` = raw_ratio,
         `Other states` = other_states_ratio, Relative = relative_ratio, `Pre RMSPE %` = pre_rmspe_pct)
writeLines(knitr::kable(rush_main, format = "latex", booktabs = TRUE, digits = 2,
                        format.args = list(big.mark = ","),
                        caption = "Deadline rush by deed group"),
           path(tables_dir, "T_bygroup_rush.tex"))
print(rush_tbl %>% filter(base_window == "2019", group %in% c("main_proxy", "main_noaf")) %>%
        select(group, window, ca_actual, ca_base, raw_ratio, relative_ratio), n = 20)

# Figures: monthly gap and Jan-Feb relative ratio
gap_plot <- cf_tbl %>%
  filter(base_window == "2019", group %in% reg_groups, ym >= 202001L) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)), panel = role_class(group))
p_gap <- ggplot(gap_plot, aes(date, gap, color = group)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 0.4) + prop19_event_layers(label_y = Inf, show_labels = FALSE) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "California deeds relative to counterfactual - 1", color = NULL) +
  theme(legend.position = "bottom")
ggsave(path(figures_dir, "F_bygroup_rush_monthly.png"), p_gap, width = 9, height = 9,
       dpi = 300, bg = "transparent")
dot <- rush_tbl %>% filter(base_window == "2019", window == "jan_feb_2021", scope == "cross") %>%
  mutate(panel = role_class(group), group = reorder(group, relative_ratio))
p_dot <- ggplot(dot, aes(relative_ratio, group, color = panel)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") + geom_point(size = 2) +
  labs(x = "Jan-Feb 2021: California over counterfactual", y = NULL, color = NULL) +
  theme(legend.position = "bottom")
ggsave(path(figures_dir, "F_bygroup_rush_ratio.png"), p_dot, width = 8, height = 6,
       dpi = 300, bg = "transparent")

# ============================================================
# C. Item 5: rush against hole ----
# ============================================================
cf_mkt <- cf_tbl %>% filter(group == "market_sale") %>%
  transmute(base_window, ym, mkt_factor = ca_actual / cf)
hole_T <- c(legacy_202212 = 202212L, sens_202312 = bygroup_windows$hole_end_sens[1],
            sens_202405 = bygroup_windows$hole_end_sens[2], main_202406 = end_main)
hole_rows <- list(); path_rows <- list()
for (key in names(cf_all)) {
  cf <- cf_all[[key]]; g <- cf$group[1]; b <- cf$base_window[1]
  if (bygroup_defs$scope[bygroup_defs$name == g] != "cross") next    # needs donor states
  cf <- cf %>% left_join(cf_mkt %>% filter(base_window == b) %>% select(ym, mkt_factor), by = "ym") %>%
    mutate(excess_adj = ca_actual - cf * mkt_factor)
  pre_gap <- cf$gap[cf$ym >= ref_of[[g]] & cf$ym <= prop19_pre_end_ym]
  rmspe <- sqrt(mean(pre_gap^2, na.rm = TRUE))
  E <- wsum(cf, "excess", prop19_antic_start_ym, prop19_antic_end_ym)
  Ea <- wsum(cf, "excess_adj", prop19_antic_start_ym, prop19_antic_end_ym)
  for (tn in names(hole_T)) {
    Tm <- hole_T[[tn]]
    S <- -wsum(cf, "excess", bygroup_windows$hole_start, Tm)
    Sa <- -wsum(cf, "excess_adj", bygroup_windows$hole_start, Tm)
    cf_hole <- wsum(cf, "cf", bygroup_windows$hole_start, Tm)
    hole_rows[[length(hole_rows) + 1L]] <- tibble(
      group = g, base_window = b, hole_end = Tm, end_label = tn,
      rush_excess = E, shortfall = S, net_position = E - S, harvest_ratio = E / S,
      shortfall_pct_of_cf = 100 * S / cf_hole,
      rush_excess_adj = Ea, shortfall_adj = Sa, net_position_adj = Ea - Sa,
      harvest_ratio_adj = Ea / Sa,
      pre_rmspe_pct = 100 * rmspe,
      monthly_noise_deeds = rmspe * mean(cf$cf[cf$ym >= bygroup_windows$hole_start & cf$ym <= Tm]),
      n_months_hole = sum(cf$ym >= bygroup_windows$hole_start & cf$ym <= Tm))
  }
  path_rows[[key]] <- cf %>%
    filter(ym >= prop19_antic_start_ym) %>%
    transmute(group = g, base_window = b, ym, excess, excess_adj,
              cum_excess = cumsum(excess), cum_excess_adj = cumsum(excess_adj))
}
hole_tbl <- bind_rows(hole_rows) %>% mutate(role = role_of[group])
write_csv_strict(hole_tbl, path(bg_tab_dir, "bygroup_rush_hole.csv"))
path_tbl <- bind_rows(path_rows)
write_csv_strict(path_tbl, path(bg_tab_dir, "bygroup_cum_excess_path.csv"))
writeLines(knitr::kable(
  hole_tbl %>% filter(base_window == "2019", end_label == "main_202406") %>%
    select(Group = group, Rush = rush_excess, Hole = shortfall, Net = net_position,
           `Rush/hole` = harvest_ratio, `Rush (adj)` = rush_excess_adj, `Hole (adj)` = shortfall_adj),
  format = "latex", booktabs = TRUE, digits = 2, format.args = list(big.mark = ","),
  caption = "Rush against hole, through June 2024"),
  path(tables_dir, "T_bygroup_rush_hole.tex"))
p_cum <- path_tbl %>%
  filter(base_window == "2019", group %in% reg_groups) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)), panel = role_class(group),
         recent = ym > bygroup_windows$hole_end_sens[2]) %>%
  ggplot(aes(date, cum_excess, color = group)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_line(linewidth = 0.4) +
  prop19_event_layers(label_y = Inf, show_labels = FALSE) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "Cumulative excess deeds since June 2020", color = NULL) +
  theme(legend.position = "bottom")
ggsave(path(figures_dir, "F_bygroup_cum_excess.png"), p_cum, width = 9, height = 9,
       dpi = 300, bg = "transparent")

# Lag diagnostic: CA share of national deeds by month, last 18 months
lag_diag <- counts %>%
  filter(group %in% c("market_sale", "main_noaf", "death_direct_noaf"), ym >= 202301L) %>%
  group_by(group, ym) %>%
  summarise(ca = sum(n[state == "CA"]), other = sum(n[state != "CA"]),
            ca_share = ca / (ca + other), .groups = "drop")
write_csv_strict(lag_diag, path(bg_tab_dir, "bygroup_lag_diagnostic.csv"))

# ============================================================
# D. Item 3: triple difference by group (07 l.55-93) ----
# ============================================================
below_rows <- list(); drop_rows <- list(); ddd_rows <- list(); ddd_models <- list()
post_years <- 2022:2023
for (g in reg_groups) {
  pre_years <- pre_of[[g]]
  yrs <- c(pre_years, post_years)
  cnt <- counts %>% filter(sale_year %in% yrs) %>% filter(group %in% c(g, "market_sale")) %>%
    group_by(group, state, sale_year) %>% summarise(n = sum(n), .groups = "drop")
  # reporting threshold: California deeds a year in both periods
  bt <- bygroup_below_threshold(cnt %>% filter(group == g), pre_years = pre_years,
                                post_years = post_years)
  below_rows[[g]] <- bt
  if (bt$below) {
    message("[", g, "] below the ", bygroup_min_ca_deeds, "-deed threshold: descriptive only")
    next
  }
  gp <- cnt %>% filter(group == g) %>% select(state, sale_year, n)
  zr <- bygroup_drop_zero_states(gp, "n", yrs, g)
  drop_rows[[g]] <- zr$summary
  keep <- unique(zr$data$state)
  sy <- zr$data %>% rename(n_fam = n) %>%
    left_join(cnt %>% filter(group == "market_sale") %>% select(state, sale_year, n_market = n),
              by = c("state", "sale_year")) %>%
    mutate(post = as.integer(sale_year >= min(post_years)), ca = as.integer(state == "CA"))
  stopifnot("CA" %in% keep, all(sy$n_market > 0))
  stacked <- sy %>% select(state, sale_year, post, ca, n_fam, n_market) %>%
    pivot_longer(c(n_fam, n_market), names_to = "grp", values_to = "n") %>%
    mutate(fam = as.integer(grp == "n_fam"))
  m_ddd <- tryCatch(
    bygroup_feols(log(n) ~ ca:post:fam + ca:post | state^grp + sale_year^grp, stacked, g,
                  "DDD", n_col = "n", pre_years = pre_years, post_years = post_years),
    error = function(e) { message("[", g, "] DDD failed: ", conditionMessage(e)); NULL })
  m_did <- tryCatch(
    bygroup_feols(log(n_fam) ~ ca:post | state + sale_year, sy, g, "DiD", n_col = "n_fam",
                  pre_years = pre_years, post_years = post_years),
    error = function(e) NULL)
  m_mkt <- tryCatch(
    bygroup_feols(log(n_market) ~ ca:post | state + sale_year, sy, g, "market DiD",
                  n_col = "n_market", pre_years = pre_years, post_years = post_years),
    error = function(e) NULL)
  if (is.null(m_ddd) || is.null(m_did) || is.null(m_mkt) ||
      !"ca:post:fam" %in% names(coef(m_ddd$model))) next
  ddd <- unname(coef(m_ddd$model)["ca:post:fam"])
  did <- unname(coef(m_did$model)["ca:post"]); did_m <- unname(coef(m_mkt$model)["ca:post"])
  stopifnot(abs(ddd - (did - did_m)) < 1e-8)        # 07 l.78-80 identity
  ddd_rows[[g]] <- tibble(
    group = g, role = role_of[g], pre_years = paste(range(pre_years), collapse = "-"),
    did = did, did_se = unname(se(m_did$model)["ca:post"]),
    ddd = ddd, ddd_se = unname(se(m_ddd$model)["ca:post:fam"]),
    pct_effect = 100 * (exp(ddd) - 1), nobs = m_ddd$info$nobs, n_states = length(keep),
    n_states_dropped = zr$summary$n_dropped,
    ca_deeds_pre = m_ddd$info$ca_deeds_pre, ca_deeds_post = m_ddd$info$ca_deeds_post)
  ddd_models[[g]] <- m_ddd$model
}
below_tbl <- bind_rows(below_rows, .id = "name") %>% select(-group) %>% rename(group = name)
write_csv_strict(below_tbl, path(bg_tab_dir, "bygroup_below_threshold.csv"))
message("Below the CA-deed threshold (descriptive only): ",
        paste(below_tbl$group[below_tbl$below], collapse = ", "))
bygroup_save_dropped(bind_rows(drop_rows), path(bg_tab_dir, "bygroup_dropped_states.csv"))
ddd_tbl <- bind_rows(ddd_rows)
assert_rows(ddd_tbl, 1, "bygroup_ddd")
write_csv_strict(ddd_tbl, path(bg_tab_dir, "bygroup_ddd.csv"))
saveRDS(list(summary = ddd_tbl, models = ddd_models), path(bg_rds_dir, "bygroup_ddd.rds"))
writeLines(knitr::kable(
  ddd_tbl %>% select(Group = group, `DiD` = did, DDD = ddd, SE = ddd_se, `%` = pct_effect,
                     N = nobs, States = n_states, Dropped = n_states_dropped),
  format = "latex", booktabs = TRUE, digits = 3, caption = "Triple difference by deed group"),
  path(tables_dir, "T_bygroup_ddd.tex"))
p_ddd <- ddd_tbl %>%
  mutate(panel = role_class(group), group = reorder(group, ddd),
         lo = ddd - 1.96 * ddd_se, hi = ddd + 1.96 * ddd_se) %>%
  ggplot(aes(ddd, group, color = panel)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_pointrange(aes(xmin = lo, xmax = hi), size = 0.3) +
  labs(x = "CA x Post, net of market sales (log points; state-clustered 95% CI)",
       y = NULL, color = NULL) + theme(legend.position = "bottom")
ggsave(path(figures_dir, "F_bygroup_ddd.png"), p_ddd, width = 8, height = 7, dpi = 300,
       bg = "transparent")

# ============================================================
# E. Item 4: monthly event study by group (09b l.60-109) ----
# ============================================================
phase_windows <- tribble(
  ~phase,                        ~start_ym,             ~end_ym,
  "anticipation_202006_202102",  prop19_antic_start_ym, prop19_antic_end_ym,
  "rush_202011_202102",          202011L,               prop19_antic_end_ym,
  "deadline_202102",             prop19_antic_end_ym,   prop19_antic_end_ym,
  "transition_202103_202112",    202103L,               202112L,
  "steady_202201_202312",        202201L,               202312L
)
parse_es <- function(m, label) {
  broom::tidy(m, conf.int = TRUE) %>%
    filter(str_detect(term, "^ym::")) %>%
    transmute(group = label, ym = as.integer(str_extract(term, "(?<=ym::)[0-9]+")),
              estimate, std.error, conf.low, conf.high)
}
below_names <- below_tbl$group[below_tbl$below]
es_rows <- list(); ph_rows <- list(); es_info <- list()
for (g in c("market_sale", setdiff(reg_groups, below_names))) {
  ms <- counts %>% filter(group == g, between(sale_year, 2018L, 2023L)) %>%
    group_by(state, ym, sale_year) %>% summarise(n_fam = sum(n), .groups = "drop") %>%
    mutate(sale_month = ym %% 100L)
  panel <- ms %>%
    complete(state, nesting(ym, sale_year, sale_month), fill = list(n_fam = 0)) %>%
    mutate(ca = as.integer(state == "CA"))
  n_zero <- sum(panel$n_fam == 0)
  panel$lnf <- if (n_zero == 0L) log(panel$n_fam) else log1p(panel$n_fam)
  stopifnot("CA" %in% panel$state, n_distinct(panel$state) == expected_states)
  ref_set <- sort(unique(panel$ym[panel$ym >= ref_of[[g]] & panel$ym <= 201912L]))
  f_es <- as.formula(paste0("lnf ~ i(ym, ca, ref = c(", paste(ref_set, collapse = ","),
                            ")) | state + ym"))
  m <- tryCatch(feols(f_es, data = panel, cluster = ~state), error = function(e) {
    message("[", g, "] event study failed: ", conditionMessage(e)); NULL })
  if (is.null(m)) next
  message(sprintf("[%s] event study: nobs=%d, states=%d, CA deeds 2018-23=%s, zero cells=%d (%s)",
                  g, nobs(m), n_distinct(panel$state),
                  format(sum(panel$n_fam[panel$ca == 1]), big.mark = ","), n_zero,
                  if (n_zero == 0L) "log" else "log1p"))
  es <- parse_es(m, g) %>% arrange(ym) %>%
    mutate(date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)))
  es_rows[[g]] <- es
  ph_rows[[g]] <- phase_windows %>% rowwise() %>%
    mutate(group = g, n_months = sum(es$ym >= start_ym & es$ym <= end_ym),
           mean_log_points = mean(es$estimate[es$ym >= start_ym & es$ym <= end_ym]),
           pct_change = 100 * (exp(mean_log_points) - 1),
           transform = if (n_zero == 0L) "log" else "log1p", n_zero_cells = n_zero,
           nobs = nobs(m)) %>% ungroup()
}
es_tbl <- bind_rows(es_rows); ph_tbl <- bind_rows(ph_rows)
assert_rows(es_tbl, 10, "bygroup_event_study")
write_csv_strict(es_tbl, path(bg_tab_dir, "bygroup_event_study_monthly.csv"))
write_csv_strict(ph_tbl, path(bg_tab_dir, "bygroup_event_study_phases.csv"))
saveRDS(list(es = es_tbl, phases = ph_tbl), path(bg_rds_dir, "bygroup_event_study.rds"))
p_es <- es_tbl %>% mutate(panel = role_class(group)) %>%
  ggplot(aes(date, estimate)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  prop19_event_layers(label_y = Inf, show_labels = FALSE) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = 0.12, linewidth = 0.2) +
  facet_wrap(~group, ncol = 4, scales = "free_y") +
  labs(x = NULL, y = "California relative to other states (log points)")
ggsave(path(figures_dir, "F_bygroup_event_study.png"), p_es, width = 12, height = 12, dpi = 300,
       bg = "transparent")

# ============================================================
# F. Reproduction check: main_proxy against the EXISTING outputs ----
# ============================================================
mp19 <- cf_all[["main_proxy|2019"]]
E_mp  <- wsum(mp19, "excess", prop19_antic_start_ym, prop19_antic_end_ym)
cf_ant <- wsum(mp19, "cf", prop19_antic_start_ym, prop19_antic_end_ym)
miss_w <- mp19 %>% filter(ym >= 202103L, ym <= 202212L)
res <- list(
  did_fam = ddd_tbl$did[ddd_tbl$group == "main_proxy"],
  ddd = ddd_tbl$ddd[ddd_tbl$group == "main_proxy"],
  nobs_ddd = ddd_tbl$nobs[ddd_tbl$group == "main_proxy"],
  n_states_ddd = ddd_tbl$n_states[ddd_tbl$group == "main_proxy"],
  excess_deeds = E_mp, antic_actual = wsum(mp19, "ca_actual", prop19_antic_start_ym, prop19_antic_end_ym),
  antic_cf = cf_ant, excess_pct = 100 * E_mp / cf_ant,
  missing_post = -sum(miss_w$excess), missing_pct = 100 * (-sum(miss_w$excess)) / sum(miss_w$cf),
  retiming_ratio = E_mp / (-sum(miss_w$excess)),
  es_phases = ph_tbl %>% filter(group == "main_proxy") %>% select(phase, mean_log_points))
if (bygroup_full) {
  # v2 vs v1: 232 cross-state duplicate keys changed state; tolerances typed by kind
  # legacy outputs are being switched to bygroup_main, so compare with the pre-switch snapshot
  rc <- bygroup_repro_check(res, tables_dir = path(out_dir, "_v1_snapshot_2026-11-11", "tables"))
  print(rc %>% mutate(diff = signif(diff, 3)))
  above <- rc %>% filter(diff > 1e-5)
  message("Items with |diff| > 1e-5: ", if (nrow(above)) paste(above$item, collapse = ", ") else "none")
} else {
  message("DEV PANEL: reproduction check not enforced (dev has 3 states). Values computed:")
  print(unlist(res[setdiff(names(res), "es_phases")]))
}
saveRDS(res, path(bg_rds_dir, "bygroup_main_proxy_repro_values.rds"))

message("Finished 17_bygroup_designs at ", Sys.time())
