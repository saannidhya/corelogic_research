#======================================================================#
# Purpose : Resale of the California deadline-rush cohort as a triple difference
#           (simulated JUE review, item B4; plan 2026-11-12_review-analyses.md, WP R3).
#           Outcome sold24 = a market sale of the same parcel strictly after the deed
#           and within 24 months (the logic of 18_bygroup_resale.R, reproduced exactly).
#           Comparisons, rush cohort (Jun 2020 - Feb 2021) against the pooled reference
#           cohorts (Jun-Feb one and two years earlier):
#             (1) main measure, California vs other states: the existing difference
#                 in differences (-3.57 pp), now with standard errors;
#             (2) within California, main measure vs California spouse deeds
#                 (spouse deeds sit inside the main measure, so also the
#                 non-overlapping split: main measure without spouse vs spouse);
#             (3) within California, main measure vs market sales;
#             (4) triple: (main - market) x (California - other states) x (rush - ref).
#           Linear probability models fitted on county x group x cohort cells weighted
#           by deeds: the coefficients and the clustered errors equal the deed-level
#           ones (checked below on the California deeds). Errors are state-clustered
#           where California is compared with other states (one treated cluster, so
#           these errors are limited) and county-clustered within California.
#           Then the marginal-rusher bound (referee B, Concern 4): the resale rate the
#           excess (rushed) deeds alone would need to produce the cohort difference.
# Name    : Saani Rawat
# Created : 11/13/2026
# Log     : 11/13/2026: created (WP R3).
# Inputs  : data/derived/03_family_homes/events.parquet (events_panel_path);
#           00_groups.R, 00_group_helpers.R (read only);
#           _outputs/tables/bygroup_resale_cohort_means.csv (reproduction check),
#           bygroup_cf_monthly.csv and bygroup_rush_hole.csv (rush excess, 17_bygroup_designs.R)
# Outputs : _outputs/tables/r3_resale_cells.csv, r3_resale_means.csv,
#           r3_resale_estimates.csv, r3_resale_marginal_rusher.csv, r3_macros_27.csv;
#           rebuilds r3_numbers.tex and r3_number_sources.csv from all r3_macros_*.csv
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_group_helpers.R"))
suppressPackageStartupMessages(library(fixest))

set.seed(20261113)

#============================================#
# Settings ----
#============================================#

# DuckDB capped at about 25% of the 64 GB machine (R1 and R2 run at the same time);
# spill goes to a local disk outside the OneDrive tree
duck_mem     <- Sys.getenv("FAM_DUCKDB_MEM_R3", "15GB")
duck_threads <- 6L
spill_dir    <- "C:/Temp/wpR3_duckdb"
dir_create(spill_dir)

# cohorts by deed month (YYYYMM); ref_pooled spans two ranges, as in 18
cohort_ranges <- tribble(
  ~cohort,        ~lo,     ~hi,
  "rush",         202006L, 202102L,   # ballot placement to deadline month
  "jan_feb_2021", 202101L, 202102L,   # last two rush months
  "mar_apr_2021", 202103L, 202104L,   # first two months under the new rules
  "ref_pooled",   201806L, 201902L,
  "ref_pooled",   201906L, 202002L
)

# deed groups (00_groups.R names); the two *_ca groups exist only in California
groups_r3 <- c(main = bygroup_main,                       # main_distinct, the Section 4 measure
               main_xspouse = "main_distinct_xspouse_ca",
               spouse = "spouse_ca",
               market = "market_sale")
stopifnot(all(groups_r3 %in% bygroup_defs$name))

horizon_months <- 24L
censor_ym   <- bygroup_windows$hole_end_main                                  # 202406
censor_date <- seq(as.Date(sprintf("%d-%02d-01", censor_ym %/% 100L, censor_ym %% 100L)),
                   by = "month", length.out = 2)[2] - 1
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")
z95 <- qnorm(0.975)

log_file <- path(logs_dir, "27_resale_ddd.log")
sink(log_file, split = TRUE)
message("Starting 27_resale_ddd at ", Sys.time(), " | panel: ", events_panel_path)

fmt_num <- function(x, digits = 2) {
  out <- formatC(abs(x), digits = digits, format = "f", big.mark = ",")
  ifelse(x < 0 & round(abs(x), digits) > 0, paste0("\\ensuremath{-}", out), out)
}

#' Rebuild r3_numbers.tex and r3_number_sources.csv from every r3_macros_*.csv fragment
#'
#' Each R3 script saves its own fragment; the macro file always holds the union, so the
#' scripts can run in any order. Stops on a duplicated or malformed macro name.
#' @param macros Tibble: macro, value, source, definition.
#' @param tag Script tag used in the fragment name (e.g. "27").
write_r3_macros <- function(macros, tag) {
  stopifnot(all(grepl("^RTHREE[A-Za-z]+$", macros$macro)), !anyDuplicated(macros$macro),
            all(nzchar(macros$value)))
  write_csv_strict(macros %>% mutate(script = tag),
                   path(tables_out_dir, paste0("r3_macros_", tag, ".csv")))
  frags <- dir_ls(tables_out_dir, regexp = "r3_macros_[0-9a-z]+\\.csv$")
  all_macros <- purrr::map_dfr(frags, ~ read_csv(.x, show_col_types = FALSE,
                                                 col_types = cols(.default = "c")))
  stopifnot(!anyDuplicated(all_macros$macro))
  writeLines(c("% Generated by 27_resale_ddd.R, 11c_welfare_stock.R, 28_fact1_parcels.R (WP R3). Do not edit.",
               paste0("\\newcommand{\\", all_macros$macro, "}{", all_macros$value, "}")),
             path(tables_out_dir, "r3_numbers.tex"))
  write_csv_strict(all_macros, path(tables_out_dir, "r3_number_sources.csv"))
  message("r3_numbers.tex: ", nrow(all_macros), " macros from ", length(frags), " fragment(s)")
}

#============================================#
# Importing cohort deeds and market sales (duckdb) ----
#============================================#

con <- open_corelogic_duckdb(memory_limit = duck_mem, threads = duck_threads)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(spill_dir)}'"))

ev <- glue("read_parquet('{sql_quote_path(events_panel_path)}')")
terr_sql   <- paste0("('", paste(territories, collapse = "','"), "')")
months_sql <- paste(sprintf("(ym BETWEEN %d AND %d)", cohort_ranges$lo, cohort_ranges$hi),
                    collapse = " OR ")

# deeds in any cohort month (same population as 18: no corporate buyer, no territories)
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE coh_all AS
  SELECT clip, state, fips5, ym, class, doc_type, fam_vehicle, fam_trigger, fam_relation,
         fam_relation_nm, trust_fam_lo, trust_fam_hi, trust_chain, trust_chain_gap_days,
         absentee_buyer, {event_date_sql('sale_raw')} AS sale_date
  FROM {ev}
  WHERE corp_buyer = 0 AND state NOT IN {terr_sql} AND ({months_sql})"))
n_audit <- dbGetQuery(con, glue("
  SELECT COUNT(*) AS n, SUM(CASE WHEN sale_date IS NULL THEN 1 ELSE 0 END) AS n_invalid,
         SUM(CASE WHEN sale_date + INTERVAL {horizon_months} MONTH > DATE '{censor_date}'
                  THEN 1 ELSE 0 END) AS n_censored
  FROM coh_all"))
print(n_audit)
stopifnot(n_audit$n > 1e7, n_audit$n_invalid / n_audit$n < 0.001, n_audit$n_censored == 0)
dbExecute(con, "CREATE OR REPLACE TEMP TABLE coh_base AS SELECT * FROM coh_all WHERE sale_date IS NOT NULL")

# market sales: the outcome events (distinct parcel-date)
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE ms AS
  SELECT DISTINCT clip, {event_date_sql('sale_raw')} AS msale_date
  FROM {ev} WHERE class = 'market_sale' AND ym >= 201701"))
dbExecute(con, "DELETE FROM ms WHERE msale_date IS NULL")
dbWriteTable(con, "cohort_ranges", as.data.frame(cohort_ranges), temporary = TRUE, overwrite = TRUE)

# One deed = one (clip, state, month, date, absentee flag) within a group, as in 18;
# the county is attached as the smallest fips5 among identical deeds.
deed_sql <- function(filter_sql) {
  glue("
    WITH c AS (
      SELECT clip, state, ym, sale_date, absentee_buyer, MIN(fips5) AS fips5
      FROM coh_base WHERE coalesce({filter_sql}, FALSE)
      GROUP BY clip, state, ym, sale_date, absentee_buyer
    )
    SELECT c.*, ms.msale_date,
           CASE WHEN ms.msale_date <= c.sale_date + INTERVAL {horizon_months} MONTH
                THEN 1 ELSE 0 END AS sold24
    FROM c ASOF LEFT JOIN ms ON c.clip = ms.clip AND c.sale_date < ms.msale_date")
}

#============================================#
# County cells by group and cohort ----
#============================================#

cells_list <- vector("list", length(groups_r3))
for (i in seq_along(groups_r3)) {
  t0 <- Sys.time()
  cells_list[[i]] <- dbGetQuery(con, glue("
    WITH d AS ({deed_sql(bygroup_filter(groups_r3[[i]]))})
    SELECT d.state, coalesce(d.fips5, '') AS fips5, r.cohort,
           COUNT(*) AS n, SUM(sold24) AS n_sold24
    FROM d JOIN cohort_ranges r ON d.ym BETWEEN r.lo AND r.hi
    GROUP BY ALL")) %>%
    as_tibble() %>%
    mutate(group = names(groups_r3)[i], group_name = groups_r3[[i]], .before = 1)
  message(sprintf("  [%s] %s cells, %s deeds in %.1fs", groups_r3[[i]],
                  format(nrow(cells_list[[i]]), big.mark = ","),
                  format(sum(cells_list[[i]]$n), big.mark = ","),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}
cells <- bind_rows(cells_list) %>%
  mutate(n = as.numeric(n), n_sold24 = as.numeric(n_sold24))

# A few deeds carry a county code of another state (cross-state duplicate keys in the
# panel); they are pooled into one cluster per state rather than counted as extra counties.
state_fips <- cells %>%
  filter(fips5 != "") %>%
  group_by(state, prefix = substr(fips5, 1, 2)) %>%
  summarise(n = sum(n), .groups = "drop_last") %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(state, state_prefix = prefix)
clean_county <- function(df) {
  df %>%
    left_join(state_fips, by = "state") %>%
    mutate(county = if_else(fips5 != "" & substr(fips5, 1, 2) == state_prefix, fips5,
                            paste0(state, "_xfips"))) %>%
    select(-state_prefix)
}
cells <- cells %>%
  clean_county() %>%
  mutate(ca = as.integer(state == "CA"), y = n_sold24 / n)
stopifnot(state_fips$state_prefix[state_fips$state == "CA"] == "06")
message("Deeds whose county code is missing or belongs to another state: ",
        format(sum(cells$n[grepl("_xfips$", cells$county)]), big.mark = ","), " of ",
        format(sum(cells$n), big.mark = ","))
stopifnot(nrow(cells) > 1000, all(cells$y >= 0 & cells$y <= 1))
write_csv_strict(cells %>% select(-y), path(tables_out_dir, "r3_resale_cells.csv"))

# deed-level California rows (rush and reference cohorts): the check of the cell
# regressions and the county fixed-effect versions, which fixest cannot fit on cells
# (four cells per county leave no within-county variation for the error)
ca_union <- paste(vapply(names(groups_r3), function(g) {
  glue("SELECT '{g}' AS grp, * FROM ({deed_sql(bygroup_filter(groups_r3[[g]]))}) WHERE state = 'CA'")
}, character(1)), collapse = "\nUNION ALL\n")
ca_deeds <- dbGetQuery(con, glue("
  WITH d AS ({ca_union})
  SELECT d.grp AS \"group\", d.state, coalesce(d.fips5, '') AS fips5, r.cohort, d.sold24
  FROM d JOIN cohort_ranges r ON d.ym BETWEEN r.lo AND r.hi
  WHERE r.cohort IN ('rush', 'ref_pooled')")) %>%
  as_tibble() %>%
  clean_county()
dbDisconnect(con, shutdown = TRUE)
stopifnot(sum(ca_deeds$group == "main") ==
            sum(cells$n[cells$group == "main" & cells$ca == 1L & cells$cohort %in% c("rush", "ref_pooled")]))

#============================================#
# Reproducing 18_bygroup_resale.R cohort means ----
#============================================#

means <- cells %>%
  mutate(region = if_else(ca == 1L, "CA", "other")) %>%
  group_by(group, group_name, region, cohort) %>%
  summarise(n = sum(n), n_sold24 = sum(n_sold24), n_counties = n_distinct(county),
            .groups = "drop") %>%
  mutate(sold24 = n_sold24 / n)
write_csv_strict(means, path(tables_out_dir, "r3_resale_means.csv"))

old_means <- read_csv(path(tables_out_dir, "bygroup_resale_cohort_means.csv"), show_col_types = FALSE)
repro <- means %>%
  filter(cohort %in% c("rush", "jan_feb_2021", "ref_pooled")) %>%
  inner_join(old_means %>% select(group_name = group, region, cohort, n_18 = n, n_sold24_18 = n_sold24),
             by = c("group_name", "region", "cohort"))
print(as.data.frame(repro %>% select(group_name, region, cohort, n, n_18, n_sold24, n_sold24_18)))
stopifnot(nrow(repro) == 18L, all(repro$n == repro$n_18), all(repro$n_sold24 == repro$n_sold24_18))
message("Cohort means reproduce bygroup_resale_cohort_means.csv exactly (", nrow(repro), " cells)")

#============================================#
# Estimating the contrasts ----
#============================================#

#' One contrast on the county cells: weighted LPM, clustered errors
#'
#' g = 1 for the treated group (g1), 0 for the comparison group (g0); post = 1 for the
#' treated cohort; ca = 1 for California. The coefficient on the full interaction is the
#' difference in differences (or triple difference) of the cell means.
#' @param spec Short label.
#' @param g1,g0 Group short names (g0 NA: one group, California vs other states).
#' @param treat,ref Cohorts.
#' @param ca_only TRUE keeps California cells only.
#' @param clusters Cluster variables to report ("state", "county").
#' @param county_fe TRUE adds county x group and county x cohort fixed effects (fitted on
#'   the deed-level California rows: a within-county difference in differences).
#' @return Tibble, one row per cluster variable.
fit_contrast <- function(spec, g1, g0 = NA_character_, treat = "rush", ref = "ref_pooled",
                         ca_only = TRUE, clusters = "county", county_fe = FALSE) {
  rhs <- c(if (!is.na(g0)) "g", if (!ca_only) "ca", "post")
  term <- paste(rhs, collapse = ":")
  if (county_fe) {
    stopifnot(ca_only, !is.na(g0), treat == "rush", ref == "ref_pooled")
    d <- ca_deeds %>%
      filter(group %in% c(g1, g0)) %>%
      mutate(post = as.integer(cohort == treat), g = as.integer(group == g1), n = 1)
    fml <- sold24 ~ g:post | county^g + county^post
    wts <- NULL
  } else {
    d <- cells %>%
      filter(cohort %in% c(treat, ref), group %in% na.omit(c(g1, g0))) %>%
      mutate(post = as.integer(cohort == treat), g = as.integer(group == g1))
    if (ca_only) d <- d %>% filter(ca == 1L)
    fml <- as.formula(paste("y ~", paste(rhs, collapse = " * ")))
    wts <- ~n
  }
  purrr::map_dfr(clusters, function(cl) {
    m <- feols(fml, data = d, weights = wts, cluster = as.formula(paste0("~", cl)),
               ssc = ssc(K.adj = FALSE))
    tibble(spec = spec, treated_group = groups_r3[[g1]],
           comparison_group = if (is.na(g0)) "same group, other states" else groups_r3[[g0]],
           treat_cohort = treat, ref_cohort = ref,
           regions = if (ca_only) "CA" else "CA vs other states", county_fe = county_fe,
           term = term, estimate = unname(coef(m)[term]), se = unname(se(m)[term]),
           cluster = cl, n_clusters = n_distinct(d[[cl]]), n_deeds = sum(d$n),
           n_cells = if (county_fe) NA_integer_ else nrow(d))
  })
}

est <- bind_rows(
  # (1) the existing difference in differences, with errors
  fit_contrast("dd_ca_vs_other", "main", ca_only = FALSE, clusters = c("state", "county")),
  # (2) within California against spouse deeds
  fit_contrast("ca_main_vs_spouse", "main", "spouse"),
  fit_contrast("ca_main_vs_spouse", "main", "spouse", county_fe = TRUE),
  fit_contrast("ca_main_xspouse_vs_spouse", "main_xspouse", "spouse"),
  fit_contrast("ca_main_xspouse_vs_spouse", "main_xspouse", "spouse", county_fe = TRUE),
  # (3) within California against market sales
  fit_contrast("ca_main_vs_market", "main", "market"),
  fit_contrast("ca_main_vs_market", "main", "market", county_fe = TRUE),
  # (4) triple difference: (main - market) x (CA - other states) x (rush - ref)
  fit_contrast("ddd_main_vs_market_ca_vs_other", "main", "market", ca_only = FALSE,
               clusters = c("state", "county")),
  # market sales alone, California vs other states (the market leg of (4))
  fit_contrast("dd_market_ca_vs_other", "market", ca_only = FALSE, clusters = c("state", "county")),
  # the last rush months against the first months under the new rules (both in 2021)
  fit_contrast("jf21_vs_ma21_dd_ca_vs_other", "main", treat = "jan_feb_2021", ref = "mar_apr_2021",
               ca_only = FALSE, clusters = c("state", "county")),
  fit_contrast("jf21_vs_ma21_ca_main_vs_spouse", "main", "spouse", treat = "jan_feb_2021",
               ref = "mar_apr_2021"),
  fit_contrast("jf21_vs_ma21_ca_main_vs_market", "main", "market", treat = "jan_feb_2021",
               ref = "mar_apr_2021"),
  fit_contrast("jf21_vs_ma21_ddd_main_vs_market", "main", "market", treat = "jan_feb_2021",
               ref = "mar_apr_2021", ca_only = FALSE, clusters = c("state", "county"))
) %>%
  mutate(estimate_pp = 100 * estimate, se_pp = 100 * se,
         ci_lo_pp = estimate_pp - z95 * se_pp, ci_hi_pp = estimate_pp + z95 * se_pp)

# the cell-mean arithmetic behind each coefficient (no fixed effects)
mean_of <- function(g, region, coh) {
  v <- means$sold24[means$group == g & means$region == region & means$cohort == coh]
  stopifnot(length(v) == 1L); v
}
chg <- function(g, region = "CA", treat = "rush", ref = "ref_pooled") mean_of(g, region, treat) - mean_of(g, region, ref)
arith <- c(
  dd_ca_vs_other = chg("main") - chg("main", "other"),
  ca_main_vs_spouse = chg("main") - chg("spouse"),
  ca_main_xspouse_vs_spouse = chg("main_xspouse") - chg("spouse"),
  ca_main_vs_market = chg("main") - chg("market"),
  ddd_main_vs_market_ca_vs_other = (chg("main") - chg("market")) - (chg("main", "other") - chg("market", "other")),
  dd_market_ca_vs_other = chg("market") - chg("market", "other"))
chk <- est %>% filter(!county_fe, spec %in% names(arith)) %>%
  mutate(arith = arith[spec], gap = abs(estimate - arith))
stopifnot(all(chk$gap < 1e-10))
stopifnot(abs(100 * arith[["dd_ca_vs_other"]] - (-3.5743202569612387)) < 1e-9)   # bygroup_resale_dd.csv
message("Coefficients equal the cell-mean differences; the DD reproduces -3.574 pp")

# deed-level check: the cell regression gives the deed-level coefficient and clustered error
dl <- ca_deeds %>% filter(group %in% c("main", "spouse")) %>%
  mutate(g = as.integer(group == "main"), post = as.integer(cohort == "rush"))
m_dl <- feols(sold24 ~ g * post, data = dl, cluster = ~county, ssc = ssc(K.adj = FALSE))
cell_row <- est %>% filter(spec == "ca_main_vs_spouse", !county_fe)
message(sprintf("Deed level: %.8f (%.8f); cells: %.8f (%.8f); deeds %s",
                coef(m_dl)[["g:post"]], se(m_dl)[["g:post"]], cell_row$estimate, cell_row$se,
                format(nobs(m_dl), big.mark = ",")))
stopifnot(abs(coef(m_dl)[["g:post"]] - cell_row$estimate) < 1e-10,
          abs(se(m_dl)[["g:post"]] - cell_row$se) < 1e-8, nobs(m_dl) == cell_row$n_deeds)
rm(ca_deeds, dl)

write_csv_strict(est, path(tables_out_dir, "r3_resale_estimates.csv"))
print(as.data.frame(est %>% select(spec, county_fe, cluster, n_clusters, estimate_pp, se_pp, n_deeds)),
      digits = 4)

#============================================#
# Marginal-rusher bound ----
#============================================#

# Rush excess and actual deeds over the same nine months (Jun 2020 - Feb 2021), main measure,
# monthly 2019-scaled counterfactual of 17_bygroup_designs.R
hole <- read_csv(path(tables_out_dir, "bygroup_rush_hole.csv"), show_col_types = FALSE) %>%
  filter(group == bygroup_main, base_window == "2019", end_label == "main_202406")
cfm <- read_csv(path(tables_out_dir, "bygroup_cf_monthly.csv"), show_col_types = FALSE) %>%
  filter(group == bygroup_main, as.character(base_window) == "2019",
         ym >= bygroup_windows$nine_month[1], ym <= bygroup_windows$nine_month[2])
stopifnot(nrow(hole) == 1L, nrow(cfm) == 9L,
          abs(sum(cfm$ca_actual - cfm$cf) - hole$rush_excess) < 1)
rush_actual <- sum(cfm$ca_actual)

r_obs   <- mean_of("main", "CA", "rush")
n_rush  <- means$n[means$group == "main" & means$region == "CA" & means$cohort == "rush"]
excess_cases <- tibble(excess_basis = c("counterfactual", "net_of_market"),
                       excess_deeds = c(hole$rush_excess, hole$rush_excess_adj))

# Under a counterfactual, the cohort would have resold at r0 = r_obs - effect. If only the
# excess share s of the cohort differs, the excess deeds must resell at
#   r_m = r0 + effect / s,
# and the largest fall the excess can produce (r_m = 0) is s * r0.
bound_specs <- est %>%
  filter(treated_group == bygroup_main, treat_cohort == "rush", !county_fe,
         spec %in% c("dd_ca_vs_other", "ca_main_vs_spouse", "ca_main_vs_market",
                     "ddd_main_vs_market_ca_vs_other")) %>%
  filter(!(regions == "CA vs other states" & cluster == "county"))
bound <- crossing(bound_specs %>% select(spec, comparison_group, cluster, estimate, se),
                  excess_cases) %>%
  mutate(rush_actual_deeds = rush_actual, resale_cohort_deeds = n_rush,
         excess_share = excess_deeds / rush_actual,
         r_obs = r_obs, r0 = r_obs - estimate,
         max_fall_pp = 100 * excess_share * r0,
         r_marginal = r0 + estimate / excess_share,
         r_marginal_lo = r0 + (estimate - z95 * se) / excess_share,
         r_marginal_hi = r0 + (estimate + z95 * se) / excess_share,
         feasible = r_marginal >= 0 & r_marginal <= 1,
         excess_share_needed = pmax(-estimate, 0) / r0,
         excess_deeds_needed = excess_share_needed * rush_actual)
# the referee's own arithmetic: 23,367 of 126,977 deeds, counterfactual rate 16.0 percent
referee <- tibble(spec = "referee_arithmetic", comparison_group = "as stated in the report",
                  cluster = NA_character_, estimate = -0.0357, se = NA_real_,
                  excess_basis = "counterfactual", excess_deeds = 23367,
                  rush_actual_deeds = 126977, resale_cohort_deeds = 126977,
                  excess_share = 23367 / 126977, r_obs = NA_real_, r0 = 0.164 - 0.004) %>%
  mutate(max_fall_pp = 100 * excess_share * r0, r_marginal = r0 + estimate / excess_share,
         feasible = r_marginal >= 0 & r_marginal <= 1)
bound <- bind_rows(bound, referee)
write_csv_strict(bound, path(tables_out_dir, "r3_resale_marginal_rusher.csv"))
print(as.data.frame(bound %>% select(spec, cluster, excess_basis, excess_share, r0, max_fall_pp,
                                     estimate, r_marginal, r_marginal_lo, r_marginal_hi, feasible)),
      digits = 4)

#============================================#
# Macros ----
#============================================#

e_get <- function(s, cl, fe = FALSE, col = "estimate_pp") {
  v <- est[[col]][est$spec == s & est$cluster == cl & est$county_fe == fe]
  stopifnot(length(v) == 1L); v
}
b_get <- function(s, basis, col) {
  v <- bound[[col]][bound$spec == s & bound$excess_basis == basis]
  stopifnot(length(v) == 1L); v
}
S_e <- "r3_resale_estimates.csv"; S_b <- "r3_resale_marginal_rusher.csv"; S_m <- "r3_resale_means.csv"
macros <- tribble(
  ~macro, ~value, ~source, ~definition,
  "RTHREEResaleDD", fmt_num(e_get("dd_ca_vs_other", "state")), S_e,
    "24-month resale, rush vs reference cohorts, main measure, CA minus other states (pp)",
  "RTHREEResaleDDSEState", fmt_num(e_get("dd_ca_vs_other", "state", col = "se_pp")), S_e,
    "its state-clustered SE (51 clusters, one treated: limited)",
  "RTHREEResaleDDSECounty", fmt_num(e_get("dd_ca_vs_other", "county", col = "se_pp")), S_e,
    "its county-clustered SE",
  "RTHREEResaleVsSpouse", fmt_num(e_get("ca_main_vs_spouse", "county")), S_e,
    "within CA: main-measure change minus spouse-deed change (spouse deeds inside the main measure) (pp)",
  "RTHREEResaleVsSpouseSE", fmt_num(e_get("ca_main_vs_spouse", "county", col = "se_pp")), S_e,
    "its county-clustered SE",
  "RTHREEResaleXSpouseVsSpouse", fmt_num(e_get("ca_main_xspouse_vs_spouse", "county")), S_e,
    "within CA: main measure without spouse deeds minus spouse deeds (no overlap) (pp)",
  "RTHREEResaleXSpouseVsSpouseSE", fmt_num(e_get("ca_main_xspouse_vs_spouse", "county", col = "se_pp")), S_e,
    "its county-clustered SE",
  "RTHREEResaleVsSpouseCountyFE", fmt_num(e_get("ca_main_vs_spouse", "county", fe = TRUE)), S_e,
    "within CA, main minus spouse, county x group and county x cohort fixed effects (pp)",
  "RTHREEResaleVsSpouseCountyFESE", fmt_num(e_get("ca_main_vs_spouse", "county", fe = TRUE, col = "se_pp")), S_e,
    "its county-clustered SE",
  "RTHREEResaleVsMarketCACountyFE", fmt_num(e_get("ca_main_vs_market", "county", fe = TRUE)), S_e,
    "within CA, main minus market sales, county fixed effects as above (pp)",
  "RTHREEResaleVsMarketCA", fmt_num(e_get("ca_main_vs_market", "county")), S_e,
    "within CA: main-measure change minus market-sale change (pp)",
  "RTHREEResaleVsMarketCASE", fmt_num(e_get("ca_main_vs_market", "county", col = "se_pp")), S_e,
    "its county-clustered SE",
  "RTHREEResaleDDD", fmt_num(e_get("ddd_main_vs_market_ca_vs_other", "state")), S_e,
    "triple difference: (main - market) x (CA - other states) x (rush - reference) (pp)",
  "RTHREEResaleDDDSEState", fmt_num(e_get("ddd_main_vs_market_ca_vs_other", "state", col = "se_pp")), S_e,
    "its state-clustered SE (limited)",
  "RTHREEResaleDDDSECounty", fmt_num(e_get("ddd_main_vs_market_ca_vs_other", "county", col = "se_pp")), S_e,
    "its county-clustered SE",
  "RTHREEResaleMarketDD", fmt_num(e_get("dd_market_ca_vs_other", "state")), S_e,
    "market sales alone: CA minus other states, rush vs reference (pp)",
  "RTHREEResaleCARush", fmt_num(100 * r_obs, 1), S_m, "CA main-measure rush cohort sold within 24 months (percent)",
  "RTHREEResaleCohortDeeds", fmt_num(n_rush, 0), S_m, "CA main-measure rush cohort, deeds in the resale population",
  "RTHREERushActualDeeds", fmt_num(rush_actual, 0), "bygroup_cf_monthly.csv",
    "CA main-measure deeds, Jun 2020 - Feb 2021 (the counterfactual's actual count)",
  "RTHREERushShare", fmt_num(100 * b_get("dd_ca_vs_other", "counterfactual", "excess_share"), 1), S_b,
    "rush excess (23,367) as a percent of actual rush-window deeds",
  "RTHREERushShareNetMarket", fmt_num(100 * b_get("dd_ca_vs_other", "net_of_market", "excess_share"), 1), S_b,
    "rush excess net of market sales (18,869) as a percent of actual rush-window deeds",
  "RTHREEMaxFallDD", fmt_num(b_get("dd_ca_vs_other", "counterfactual", "max_fall_pp")), S_b,
    "largest fall the rush excess could produce under the DD counterfactual (pp)",
  "RTHREEMarginalRateDD", fmt_num(100 * b_get("dd_ca_vs_other", "counterfactual", "r_marginal"), 1), S_b,
    "resale rate the excess deeds would need under the DD counterfactual (percent)",
  "RTHREEMarginalRateDDNetMarket", fmt_num(100 * b_get("dd_ca_vs_other", "net_of_market", "r_marginal"), 1), S_b,
    "same, excess net of market sales (percent)",
  "RTHREEMaxFallDDD", fmt_num(b_get("ddd_main_vs_market_ca_vs_other", "counterfactual", "max_fall_pp")), S_b,
    "largest fall the rush excess could produce under the triple-difference counterfactual (pp)",
  "RTHREEMarginalRateDDD", fmt_num(100 * b_get("ddd_main_vs_market_ca_vs_other", "counterfactual", "r_marginal"), 1), S_b,
    "resale rate the excess deeds would need under the triple-difference counterfactual (percent)",
  "RTHREEMarginalRateDDDNetMarket", fmt_num(100 * b_get("ddd_main_vs_market_ca_vs_other", "net_of_market", "r_marginal"), 1), S_b,
    "same, excess net of market sales (percent)",
  "RTHREEMarginalRateSpouse", fmt_num(100 * b_get("ca_main_vs_spouse", "counterfactual", "r_marginal"), 1), S_b,
    "resale rate the excess deeds would need under the spouse-deed counterfactual (percent)",
  "RTHREEMaxFallReferee", fmt_num(b_get("referee_arithmetic", "counterfactual", "max_fall_pp")), S_b,
    "the referee's arithmetic: 23,367 / 126,977 x 16.0 percent (pp)",
  "RTHREEResaleJanFebDD", fmt_num(e_get("jf21_vs_ma21_dd_ca_vs_other", "state")), S_e,
    "Jan-Feb 2021 vs Mar-Apr 2021 deeds, main measure, CA minus other states (pp)",
  "RTHREEResaleJanFebVsSpouse", fmt_num(e_get("jf21_vs_ma21_ca_main_vs_spouse", "county")), S_e,
    "Jan-Feb 2021 vs Mar-Apr 2021, within CA, main minus spouse (pp)",
  "RTHREEResaleJanFebVsSpouseSE", fmt_num(e_get("jf21_vs_ma21_ca_main_vs_spouse", "county", col = "se_pp")), S_e,
    "its county-clustered SE"
)
write_r3_macros(macros, "27")

message("Finished 27_resale_ddd at ", Sys.time())
sink()
