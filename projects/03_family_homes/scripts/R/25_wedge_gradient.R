#======================================================================#
# Purpose : Within-California dose-response of family transfers to the
#           parcel property-tax wedge (WP-R2; referee items A3 = B3).
#           Unit: California single-family and condominium parcels at risk
#           of a family transfer. The wedge w = t * max(p - A0, 0) is the
#           one built by 04a at the January 1, 2019 lien date (cohort
#           d2019), before Proposition 19 reached the ballot (June 24, 2020).
#           Bins: zero wedge on its own; deciles of the positive wedge; the
#           2021 gap p - A0 against the $1 million value cap (split by the
#           recipient's mailing-address occupancy proxy); tenure; homeowner
#           exemption; the parent's gain p - b where b (last market
#           purchase price) is observed. For each bin:
#             (a) family transfers per 1,000 parcels at risk, Jan-Feb 2021
#                 against Jan-Feb 2019 (rush);
#             (b) deadline ratio, deeds per signing day 17-28 Feb over
#                 1-16 Feb (2021, and 2019 as reference);
#             (c) 24-month resale of rushed deeds against the reference
#                 cohorts (logic of 18_bygroup_resale.R);
#             (d) 2022-23 against 2017-19 change in the transfer rate, with the
#                 wedge at the January 1, 2017 lien date (04a cohort d2017),
#                 so that the risk sets condition on past sales only.
#           Main measure (bygroup_main) and placebos (spouse deeds,
#           affidavits of death). Dose-response lines with county fixed
#           effects and county-clustered errors.
# Name    : Saani Rawat
# Created : 11/13/2026
# Log     : 11/13/2026: created (WP-R2 of plans/2026-11-12_review-analyses.md).
#           11/13/2026: decline moved to the 2017 lien date; the 2019 lien date made
#           the 2017-18 risk sets drop parcels sold later in 2017-18. Resale reference
#           cohort (from June 2018) takes its bins from the 2017 lien date.
# Inputs  : data/derived/03_family_homes/events.parquet (events_panel_path)
#           data/derived/03_family_homes/ca_parcel_wedge.parquet (04a, d2019 and d2017; read only)
#           _outputs/tables/bygroup_resale_cohort_means.csv (18; reproduction check)
# Outputs : data/derived/03_family_homes/r2_parcel_wedge_bins.parquet (read by 26)
#           data/derived/03_family_homes/r2_wedge_cells.parquet
#           _outputs/tables/r2_wedge_*.csv
#           _outputs/tables/r2_numbers_sources.csv (shared with 26)
#           manuscript/tables/r2_numbers.tex (RTWO macros; shared with 26)
#           _outputs/logs/25_wedge_gradient.log
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_group_helpers.R"))

suppressPackageStartupMessages({
  library(fixest)
})

script_name <- "25_wedge_gradient.R"
log_file <- path(logs_dir, "25_wedge_gradient.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 25_wedge_gradient at ", Sys.time())
t_start <- Sys.time()

#============================================#
# Settings ----
#============================================#

# DuckDB capped at ~25% of the 63 GB machine (other workers run at the same time);
# spill goes to a local disk outside the OneDrive tree
duck_mem     <- "15GB"
duck_threads <- 8L
spill_dir    <- "C:/Temp/r2_duckdb"

# lien dates of the wedge (04a cohorts). Rush, deadline ratio, resale, cap, gain and tenure use
# January 1, 2019. The 2022-23 against 2017-19 decline uses January 1, 2017, so that every
# window starts on or after its lien date and the risk set conditions only on past sales:
# with the 2019 lien date, a 2017-18 risk set would drop parcels sold later in 2017-18 and
# with them the spouse deeds that accompany purchases (seen in the first run, 11/13/2026).
lien_date      <- as.Date("2019-01-01")
wedge_cohort   <- "d2019"
lien_date17    <- as.Date("2017-01-01")
wedge_cohort17 <- "d2017"
value_cap      <- 1e6

# deed groups (00_groups.R), short codes used in column names
group_codes <- c(main   = bygroup_main,                 # Section 4 measure (main_distinct)
                 interg = "main_distinct_xspouse_ca",    # main measure without spouse deeds
                 spouse = "spouse_ca",                   # placebo: law does not touch them
                 af     = "death_affidavit_all",         # placebo: death cannot be scheduled
                 pc     = "lifetime_pc",                 # component: parent-child lifetime deeds
                 heir   = "trust_heir_chain",            # component: chain-verified trust-to-heir
                 market = "market_sale")                 # benchmark
stopifnot(all(group_codes %in% bygroup_defs$name))
placebo_codes <- c("spouse", "af")

# base years of the decline: the group's own pre_years (affidavits: 2019 only, 2018 coding break)
base_years <- set_names(map(group_codes, ~ bygroup_defs$pre_years[[which(bygroup_defs$name == .x)]]),
                        names(group_codes))
post_years <- 2022:2023

# windows by signing date (sale_raw); a parcel is at risk in a window if the owner line of the
# lien date holds it at the window's risk date (January 1 of the window's year)
windows <- bind_rows(
  tribble(
    ~win,           ~start_date,   ~end_date,
    "jf2019",       "2019-01-01",  "2019-02-28",
    "jf2021",       "2021-01-01",  "2021-02-28",
    "feb2019_pre",  "2019-02-01",  "2019-02-16",
    "feb2019_post", "2019-02-17",  "2019-02-28",
    "feb2021_pre",  "2021-02-01",  "2021-02-16",   # a deed had to be made by 16 Feb 2021
    "feb2021_post", "2021-02-17",  "2021-02-28"
  ),
  tibble(win = paste0("y", 2017:2023),
         start_date = paste0(2017:2023, "-01-01"),
         end_date = paste0(2017:2023, "-12-31"))
) %>%
  mutate(start_date = as.Date(start_date), end_date = as.Date(end_date),
         risk_date = as.Date(paste0(format(start_date, "%Y"), "-01-01")),
         days = as.integer(end_date - start_date) + 1L)

# resale cohorts by deed month (as 18_bygroup_resale.R)
resale_horizon <- 24L
resale_censor  <- as.Date("2024-06-30")   # last usable month 202406 (bygroup_windows$hole_end_main)
rush_months    <- c(202006L, 202102L)
jf21_months    <- c(202101L, 202102L)
ref_months     <- list(c(201806L, 201902L), c(201906L, 202002L))

# 2021 gap bins around the $1 million cap (p - A0 projected to 2021, as over_cap_2021 in 04a)
gap_breaks <- c(-Inf, 0, 250e3, 500e3, 750e3, 1e6, 1.5e6, Inf)
gap_labels <- c("C0_none", "C1_0_250k", "C2_250_500k", "C3_500_750k", "C4_750k_1m",
                "C5_1_1.5m", "C6_1.5m_plus")

# output files
f_bins      <- path(tables_out_dir, "r2_wedge_gradient_bins.csv")
f_doses     <- path(tables_out_dir, "r2_wedge_bin_doses.csv")
f_slopes    <- path(tables_out_dir, "r2_wedge_dose_slopes.csv")
f_bincoef   <- path(tables_out_dir, "r2_wedge_bin_coefs.csv")
f_capocc    <- path(tables_out_dir, "r2_wedge_cap_occupancy.csv")
f_gain      <- path(tables_out_dir, "r2_wedge_gain_tenure.csv")
f_resale    <- path(tables_out_dir, "r2_wedge_resale_bins.csv")
f_coverage  <- path(tables_out_dir, "r2_wedge_coverage.csv")
f_risk      <- path(tables_out_dir, "r2_wedge_risk_sets.csv")
f_parcels   <- path(data_dir, "r2_parcel_wedge_bins.parquet")
f_parcels17 <- path(data_dir, "r2_parcel_wedge_bins_lien2017.parquet")
f_cells     <- path(data_dir, "r2_wedge_cells.parquet")
f_cells17   <- path(data_dir, "r2_wedge_cells_lien2017.parquet")
f_decline17 <- path(tables_out_dir, "r2_wedge_decline_lien2017_bins.csv")
f_resale_deeds <- path(data_dir, "r2_wedge_resale_deeds.parquet")
f_sources   <- path(tables_out_dir, "r2_numbers_sources.csv")
f_macros    <- path(tables_dir, "r2_numbers.tex")

#============================================#
# Importing the parcel wedge (04a, d2019) ----
#============================================#

wedge_path <- path(data_dir, "ca_parcel_wedge.parquet")
stopifnot(file_exists(wedge_path))
wedge_cols <- c("clip", "fips5", "wedge", "market_d0", "assessed_d0", "market_2021",
                "assessed_2021", "over_cap_2021", "tenure", "hox", "reset_date", "reset_price")
parcels <- arrow::open_dataset(wedge_path) %>%
  filter(cohort == wedge_cohort) %>%
  select(all_of(wedge_cols)) %>%
  collect()
stopifnot(nrow(parcels) > 7e6, !anyDuplicated(parcels$clip), all(is.finite(parcels$wedge)))
message("NOTE: ", format(nrow(parcels), big.mark = ","), " parcels with a wedge at ", lien_date)

# |- wedge bins: zero on its own, deciles of the positive wedge ----
pos_cuts <- quantile(parcels$wedge[parcels$wedge > 0], probs = seq(0.1, 0.9, 0.1), names = FALSE)
parcels <- parcels %>%
  mutate(
    wdec = if_else(wedge > 0, findInterval(wedge, pos_cuts, left.open = TRUE) + 1L, 0L),
    wbin = sprintf("W%02d", wdec),
    # 2021 gap above the factored base, against the occupant heir's $1 million allowance
    gap21 = market_2021 - assessed_2021,
    capbin = as.character(cut(gap21, gap_breaks, labels = gap_labels, right = TRUE)),
    # parent's basis b: last market purchase on or after the 2008-09 roll date, before the lien date
    basis = if_else(!is.na(reset_date), reset_price, NA_real_),
    gain = market_d0 - basis
  )
stopifnot(all(between(parcels$wdec, 0L, 10L)), !anyNA(parcels$capbin),
          all((parcels$gap21 > value_cap) == (parcels$over_cap_2021 == 1L)))

# |- gain terciles within each wedge decile (parcels with an observed basis) ----
gain_terciles <- parcels %>%
  filter(!is.na(gain)) %>%
  group_by(wdec) %>%
  mutate(gain_t = ntile(gain, 3L)) %>%
  ungroup() %>%
  select(clip, gain_t)
parcels <- parcels %>%
  left_join(gain_terciles, by = "clip") %>%
  mutate(gainbin = if_else(is.na(gain_t), "B0_unmeasured", paste0("B", gain_t)))

# |- bin descriptors (doses) ----
bin_doses <- parcels %>%
  group_by(wbin, wdec) %>%
  summarise(n_parcels = n(),
            wedge_min = min(wedge), wedge_max = max(wedge),
            wedge_mean = mean(wedge), wedge_median = median(wedge),
            gap21_median = median(gap21),
            share_long_held = mean(tenure == "long_held"),
            share_hox = mean(hox == 1L),
            share_over_cap = mean(over_cap_2021 == 1L),
            share_basis = mean(!is.na(basis)),
            .groups = "drop") %>%
  arrange(wdec)
print(bin_doses, width = Inf)
dose_usd <-set_names(if_else(bin_doses$wdec == 0L, 0, bin_doses$wedge_mean / 1000), bin_doses$wbin)

arrow::write_parquet(
  parcels %>% select(clip, fips5, wedge, wdec, wbin, capbin, gainbin, tenure, hox, gap21, basis, gain),
  f_parcels, compression = "zstd")
message("Wrote: ", f_parcels)

# |- the same bins at the January 2017 lien date (decline design) ----
parcels17 <- arrow::open_dataset(wedge_path) %>%
  filter(cohort == wedge_cohort17) %>%
  select(clip, fips5, wedge, tenure) %>%
  collect()
stopifnot(nrow(parcels17) == nrow(parcels), setequal(parcels17$clip, parcels$clip))
pos_cuts17 <- quantile(parcels17$wedge[parcels17$wedge > 0], probs = seq(0.1, 0.9, 0.1), names = FALSE)
parcels17 <- parcels17 %>%
  mutate(wdec = if_else(wedge > 0, findInterval(wedge, pos_cuts17, left.open = TRUE) + 1L, 0L),
         wbin = sprintf("W%02d", wdec))
bin_doses17 <- parcels17 %>%
  group_by(wbin, wdec) %>%
  summarise(n_parcels = n(), wedge_min = min(wedge), wedge_max = max(wedge),
            wedge_mean = mean(wedge), wedge_median = median(wedge),
            share_long_held = mean(tenure == "long_held"), .groups = "drop") %>%
  arrange(wdec)
dose_usd17 <- set_names(if_else(bin_doses17$wdec == 0L, 0, bin_doses17$wedge_mean / 1000), bin_doses17$wbin)
arrow::write_parquet(parcels17 %>% select(clip, fips5, wedge, wdec, wbin, tenure), f_parcels17,
                     compression = "zstd")
write_csv_strict(bind_rows(bin_doses %>% mutate(lien = "2019-01-01"),
                           bin_doses17 %>% mutate(lien = "2017-01-01")), f_doses)

#============================================#
# DuckDB: California deeds, risk sets, window counts ----
#============================================#

# R2_USE_CACHE=1 (env) reuses the cells and resale deeds of an earlier full run (debugging only)
use_cache <- Sys.getenv("R2_USE_CACHE", "0") == "1" &&
  all(file_exists(c(f_cells, f_cells17, f_resale_deeds, f_coverage)))
if (!use_cache) {   # DuckDB stage, left unindented; closes after dbDisconnect below

dir_create(spill_dir)
con <- open_corelogic_duckdb(memory_limit = duck_mem, threads = duck_threads)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(spill_dir)}'"))
ev_src <- glue("read_parquet('{sql_quote_path(events_panel_path)}')")

# |- California deeds 2016-2024 ----
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE ca AS
  SELECT * FROM {ev_src} WHERE state = 'CA' AND sale_year BETWEEN 2016 AND 2024"))
n_ca <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM ca")$n
message("NOTE: ", format(n_ca, big.mark = ","), " California deeds 2016-2024")

# |- parcels with their market-sale dates around the lien date ----
duckdb::duckdb_register(con, "parcels_df",
                        parcels %>% select(clip, fips5, wbin, capbin, gainbin, tenure, hox))
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE ms_clip AS
  SELECT clip,
         MAX(CASE WHEN msale_date <  DATE '{lien_date}' THEN msale_date END) AS ms_last_pre,
         MIN(CASE WHEN msale_date >= DATE '{lien_date}' THEN msale_date END) AS ms_first_post,
         MIN(CASE WHEN msale_date >= DATE '{lien_date17}' THEN msale_date END) AS ms_first_post17
  FROM (SELECT clip, {event_date_sql('sale_raw')} AS msale_date FROM ca WHERE class = 'market_sale')
  WHERE msale_date IS NOT NULL
  GROUP BY clip"))
dbExecute(con, "CREATE OR REPLACE TEMP TABLE P AS
  SELECT CAST(p.clip AS BIGINT) AS clip, p.fips5, p.wbin, p.capbin, p.gainbin, p.tenure, p.hox,
         m.ms_last_pre, m.ms_first_post
  FROM parcels_df p LEFT JOIN ms_clip m ON CAST(p.clip AS BIGINT) = m.clip")
duckdb::duckdb_unregister(con, "parcels_df")
stopifnot(dbGetQuery(con, "SELECT COUNT(*) AS n FROM P")$n == nrow(parcels))

# the same parcels with their 2017 lien-date bins (decline design)
duckdb::duckdb_register(con, "parcels17_df", parcels17 %>% select(clip, fips5, wbin, tenure))
dbExecute(con, "CREATE OR REPLACE TEMP TABLE P17 AS
  SELECT CAST(p.clip AS BIGINT) AS clip, p.fips5, p.wbin, p.tenure, m.ms_first_post17
  FROM parcels17_df p LEFT JOIN ms_clip m ON CAST(p.clip AS BIGINT) = m.clip")
duckdb::duckdb_unregister(con, "parcels17_df")

# |- grouped deeds on universe parcels, 2017-2023 ----
flag_sql <- paste(sprintf("coalesce(%s, FALSE) AS f_%s", map_chr(group_codes, bygroup_filter),
                          names(group_codes)), collapse = ",\n           ")
any_flag <- paste(sprintf("f_%s", names(group_codes)), collapse = " OR ")
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE ev AS
  SELECT * FROM (
    SELECT clip, {event_date_sql('sale_raw')} AS sale_date, mail_eq_situs,
           {flag_sql}
    FROM ca
    WHERE sale_year BETWEEN 2017 AND 2023 AND clip IN (SELECT clip FROM P)
  ) WHERE sale_date IS NOT NULL AND ({any_flag})"))

# |- coverage: share of California deeds of each group on universe parcels ----
coverage_sql <- paste(map_chr(names(group_codes), function(k) glue(
  "SUM(CASE WHEN coalesce({bygroup_filter(group_codes[[k]])}, FALSE) THEN 1 ELSE 0 END) AS all_{k},
   SUM(CASE WHEN coalesce({bygroup_filter(group_codes[[k]])}, FALSE) AND in_universe THEN 1 ELSE 0 END) AS in_{k}")),
  collapse = ",\n")
coverage <- as_tibble(dbGetQuery(con, glue("
  SELECT CASE WHEN sale_year BETWEEN 2017 AND 2019 THEN '2017_19'
              WHEN sale_year = 2021 AND sale_month <= 2 THEN 'jf2021'
              WHEN sale_year BETWEEN 2022 AND 2023 THEN '2022_23' ELSE 'other' END AS period,
         {coverage_sql}
  FROM (SELECT *, clip IN (SELECT clip FROM P) AS in_universe FROM ca WHERE sale_year BETWEEN 2017 AND 2023)
  GROUP BY ALL"))) %>%
  filter(period != "other") %>%
  pivot_longer(-period, names_to = c(".value", "group"), names_pattern = "^(all|in)_(.+)$") %>%
  mutate(share_in_universe = `in` / all)
print(as.data.frame(coverage))
write_csv_strict(coverage, f_coverage)

# |- parcels with at least one deed per window ----
win_df <- windows %>% select(win, start_date, end_date, risk_date)
dbWriteTable(con, "W", as.data.frame(win_df), temporary = TRUE, overwrite = TRUE)
win_agg_sql <- paste(map_chr(names(group_codes), function(k) glue(
  "bool_or(f_{k}) AS e_{k}, ",
  "bool_or(f_{k} AND coalesce(mail_eq_situs = 1, FALSE)) AS e1_{k}, ",
  "bool_or(f_{k} AND coalesce(mail_eq_situs = 0, FALSE)) AS e0_{k}")), collapse = ",\n           ")
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE ev_win AS
  SELECT W.win, ev.clip,
           {win_agg_sql}
  FROM ev JOIN W ON ev.sale_date BETWEEN W.start_date AND W.end_date
  GROUP BY ALL"))

# |- cells: county x bins x window, at-risk parcels and parcels with a deed ----
# At risk: the lien-date owner line still holds the parcel at the risk date, i.e. no market
# sale between the risk date and the lien date (windows before it) or between the lien date and
# the risk date (windows after it). n_all ignores the rule (fixed-universe sensitivity).
risk_sql <- glue("CASE WHEN W.risk_date <= DATE '{lien_date}'
                       THEN (P.ms_last_pre IS NULL OR P.ms_last_pre < W.risk_date)
                       ELSE (P.ms_first_post IS NULL OR P.ms_first_post >= W.risk_date) END")
cell_sum_sql <- paste(map_chr(names(group_codes), function(k) glue(
  "SUM(CASE WHEN risk AND coalesce(x.e_{k}, FALSE) THEN 1 ELSE 0 END) AS ev_{k}, ",
  "SUM(CASE WHEN risk AND coalesce(x.e1_{k}, FALSE) THEN 1 ELSE 0 END) AS ev1_{k}, ",
  "SUM(CASE WHEN risk AND coalesce(x.e0_{k}, FALSE) THEN 1 ELSE 0 END) AS ev0_{k}, ",
  "SUM(CASE WHEN coalesce(x.e_{k}, FALSE) THEN 1 ELSE 0 END) AS evall_{k}")), collapse = ",\n         ")
t0 <- Sys.time()
cells <- as_tibble(dbGetQuery(con, glue("
  WITH pw AS (
    SELECT W.win, P.clip, P.fips5, P.wbin, P.capbin, P.gainbin, P.tenure, P.hox, {risk_sql} AS risk
    FROM P CROSS JOIN W)
  SELECT pw.win, pw.fips5, pw.wbin, pw.capbin, pw.gainbin, pw.tenure, pw.hox,
         COUNT(*) AS n_all, SUM(CASE WHEN risk THEN 1 ELSE 0 END) AS n_risk,
         {cell_sum_sql}
  FROM pw LEFT JOIN ev_win x ON x.clip = pw.clip AND x.win = pw.win
  GROUP BY ALL"))) %>%
  mutate(across(where(is.numeric), as.numeric))
message("NOTE: ", format(nrow(cells), big.mark = ","), " cells in ",
        round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
arrow::write_parquet(cells, f_cells, compression = "zstd")

# |- cells at the 2017 lien date: annual windows 2017-2019 and 2022-2023 ----
# At risk: no market sale between January 1, 2017 and the window's risk date (past sales only).
cell17_sum_sql <- paste(map_chr(names(group_codes), function(k) glue(
  "SUM(CASE WHEN risk AND coalesce(x.e_{k}, FALSE) THEN 1 ELSE 0 END) AS ev_{k}, ",
  "SUM(CASE WHEN coalesce(x.e_{k}, FALSE) THEN 1 ELSE 0 END) AS evall_{k}")), collapse = ",\n         ")
cells17 <- as_tibble(dbGetQuery(con, glue("
  WITH pw AS (
    SELECT W.win, P17.clip, P17.fips5, P17.wbin, P17.tenure,
           (P17.ms_first_post17 IS NULL OR P17.ms_first_post17 >= W.risk_date) AS risk
    FROM P17 CROSS JOIN W
    WHERE W.win IN ('y2017', 'y2018', 'y2019', 'y2022', 'y2023'))
  SELECT pw.win, pw.fips5, pw.wbin, pw.tenure,
         COUNT(*) AS n_all, SUM(CASE WHEN risk THEN 1 ELSE 0 END) AS n_risk,
         {cell17_sum_sql}
  FROM pw LEFT JOIN ev_win x ON x.clip = pw.clip AND x.win = pw.win
  GROUP BY ALL"))) %>%
  mutate(across(where(is.numeric), as.numeric))
arrow::write_parquet(cells17, f_cells17, compression = "zstd")

#============================================#
# Resale cohorts (logic of 18_bygroup_resale.R) ----
#============================================#

resale_groups <- group_codes[c("main", "interg", "spouse", "af", "pc", "heir")]
ref_sql <- paste(sprintf("ym BETWEEN %d AND %d", map_int(ref_months, 1), map_int(ref_months, 2)),
                 collapse = " OR ")
coh_union <- paste(map_chr(names(resale_groups), function(k) glue(
  "SELECT '{k}' AS g, clip, state, ym, {event_date_sql('sale_raw')} AS sale_date, absentee_buyer
   FROM ca
   WHERE corp_buyer = 0 AND coalesce({bygroup_filter(resale_groups[[k]])}, FALSE)
     AND (ym BETWEEN {rush_months[1]} AND {rush_months[2]} OR {ref_sql})")),
  collapse = "\nUNION ALL\n")
# identical deeds collapsed within a group (DISTINCT clip, state, ym, sale_date, absentee_buyer)
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE coh AS
  SELECT DISTINCT * FROM ({coh_union})
  WHERE sale_date IS NOT NULL
    AND sale_date + INTERVAL {resale_horizon} MONTH <= DATE '{resale_censor}'"))
# outcome events: market sales of the whole panel from 2017 (as 18)
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE ms AS
  SELECT DISTINCT clip, {event_date_sql('sale_raw')} AS msale_date
  FROM {ev_src} WHERE class = 'market_sale' AND ym >= 201701"))
dbExecute(con, "DELETE FROM ms WHERE msale_date IS NULL")
resale_deeds <- as_tibble(dbGetQuery(con, glue("
  WITH j AS (
    SELECT c.*, ms.msale_date
    FROM coh c ASOF LEFT JOIN ms ON c.clip = ms.clip AND c.sale_date < ms.msale_date)
  SELECT j.g, j.clip, j.ym, j.sale_date,
         CASE WHEN msale_date <= sale_date + INTERVAL {resale_horizon} MONTH THEN 1 ELSE 0 END AS sold24,
         P.fips5, P.wbin AS wbin19, P.tenure AS tenure19, P.hox, P17.wbin AS wbin17, P17.tenure AS tenure17
  FROM j LEFT JOIN P ON j.clip = P.clip LEFT JOIN P17 ON j.clip = P17.clip"))) %>%
  mutate(cohort = if_else(between(ym, rush_months[1], rush_months[2]), "rush", "ref"),
         jf21 = as.integer(between(ym, jf21_months[1], jf21_months[2])),
         # bins from the last lien date before the cohort's first deed: the reference cohort
         # starts June 2018, so its 2019 wedge would follow some of its own resales (a sale
         # resets the base); it takes the 2017 bins, the rush cohort the 2019 bins
         lien = if_else(cohort == "ref", "2017", "2019"),
         wbin = if_else(cohort == "ref", wbin17, wbin19),
         tenure = if_else(cohort == "ref", tenure17, tenure19),
         in_universe = !is.na(wbin))
dbDisconnect(con, shutdown = TRUE)
arrow::write_parquet(resale_deeds, f_resale_deeds, compression = "zstd")

} else {
  message("NOTE: R2_USE_CACHE=1, reading cells and resale deeds of the earlier run")
  cells <- arrow::read_parquet(f_cells)
  cells17 <- arrow::read_parquet(f_cells17)
  resale_deeds <- arrow::read_parquet(f_resale_deeds)
  coverage <- read_csv(f_coverage, show_col_types = FALSE)
}

stopifnot(all(cells %>% count(win, wt = n_all) %>% pull(n) == nrow(parcels)),
          all(cells17 %>% count(win, wt = n_all) %>% pull(n) == nrow(parcels17)))

# risk-set audit: parcels at risk by window
risk_audit <- bind_rows(
  cells %>% mutate(lien = "2019-01-01"),
  cells17 %>% mutate(lien = "2017-01-01")) %>%
  group_by(lien, win) %>%
  summarise(n_all = sum(n_all), n_risk = sum(n_risk), ev_main = sum(ev_main), .groups = "drop") %>%
  mutate(share_at_risk = n_risk / n_all)
print(as.data.frame(risk_audit))
write_csv_strict(risk_audit, f_risk)

# |- reproduction of 18 (California, all parcels) ----
repro_18 <- resale_deeds %>%
  filter(g == "main") %>%
  { bind_rows(
      group_by(., cohort) %>% summarise(n = n(), n_sold24 = sum(sold24), .groups = "drop") %>%
        mutate(cohort = if_else(cohort == "ref", "ref_pooled", cohort)),
      filter(., jf21 == 1L) %>% summarise(n = n(), n_sold24 = sum(sold24)) %>%
        mutate(cohort = "jan_feb_2021")) }
f18 <- path(tables_out_dir, "bygroup_resale_cohort_means.csv")
if (file_exists(f18)) {
  saved_18 <- read_csv(f18, show_col_types = FALSE) %>%
    filter(group == bygroup_main, region == "CA", cohort %in% repro_18$cohort) %>%
    select(cohort, n_18 = n, n_sold24_18 = n_sold24)
  repro_cmp <- inner_join(repro_18, saved_18, by = "cohort")
  print(as.data.frame(repro_cmp))
  stopifnot(nrow(repro_cmp) == 3L, all(repro_cmp$n == repro_cmp$n_18),
            all(repro_cmp$n_sold24 == repro_cmp$n_sold24_18))
  message("NOTE: California resale cohorts reproduce 18_bygroup_resale.R exactly")
}
message("NOTE: resale deeds on universe parcels: ",
        round(100 * mean(resale_deeds$in_universe[resale_deeds$g == "main"]), 1), "% (main)")

#============================================#
# Bin tables: rush, deadline ratio, decline ----
#============================================#

wgroup_of <- function(wbin) {
  case_when(wbin == "W00" ~ "Wzero", wbin <= "W03" ~ "Wlow_D1_3",
            wbin <= "W07" ~ "Wmid_D4_7", TRUE ~ "Whigh_D8_10")
}
bin_systems <- list(
  all         = function(d) rep("all", nrow(d)),
  wedge       = function(d) d$wbin,
  cap         = function(d) d$capbin,
  over_cap    = function(d) if_else(d$capbin %in% c("C5_1_1.5m", "C6_1.5m_plus"), "above_cap", "below_cap"),
  tenure      = function(d) d$tenure,
  hox         = function(d) paste0("hox", d$hox),
  gain        = function(d) d$gainbin,
  wedge3_gain = function(d) paste(wgroup_of(d$wbin), d$gainbin, sep = "_"),
  wedge3_tenure = function(d) paste(wgroup_of(d$wbin), d$tenure, sep = "_")
)

#' Rates, rush, deadline ratio and decline for one bin system
#' @param cells Cell table (county x bins x window).
#' @param system Name in bin_systems.
#' @return Tibble: one row per bin x kind (ev, ev1, ev0, evall) x group.
bin_stats <- function(cells, system) {
  cells %>%
    mutate(bin = bin_systems[[system]](cells)) %>%
    group_by(bin, win) %>%
    summarise(across(c(n_all, n_risk, matches("^ev")), sum), .groups = "drop") %>%
    pivot_longer(matches("^ev"), names_to = c("kind", "group"),
                 names_pattern = "^(ev|ev1|ev0|evall)_(.+)$", values_to = "cnt") %>%
    mutate(denom = if_else(kind == "evall", n_all, n_risk)) %>%
    select(bin, win, kind, group, cnt, denom) %>%
    pivot_wider(names_from = win, values_from = c(cnt, denom)) %>%
    mutate(
      system = system,
      rate_jf2019 = 1000 * cnt_jf2019 / denom_jf2019,
      rate_jf2021 = 1000 * cnt_jf2021 / denom_jf2021,
      rush_ratio = rate_jf2021 / rate_jf2019,
      rush_diff = rate_jf2021 - rate_jf2019,
      # deeds per signing day after over before the deadline (12 and 16 days)
      deadline_2021 = (cnt_feb2021_post / 12) / (cnt_feb2021_pre / 16),
      deadline_2019 = (cnt_feb2019_post / 12) / (cnt_feb2019_pre / 16),
      deadline_rel = deadline_2021 / deadline_2019,
      rate_y2017 = 1000 * cnt_y2017 / denom_y2017,
      rate_y2018 = 1000 * cnt_y2018 / denom_y2018,
      rate_y2019 = 1000 * cnt_y2019 / denom_y2019,
      rate_y2020 = 1000 * cnt_y2020 / denom_y2020,
      rate_y2021 = 1000 * cnt_y2021 / denom_y2021,
      rate_y2022 = 1000 * cnt_y2022 / denom_y2022,
      rate_y2023 = 1000 * cnt_y2023 / denom_y2023,
      # sensitivity decline at this lien date: 2022-23 against 2019 only (2017-18 precede the
      # lien date; their rates here are not comparable and are kept for inspection only)
      rate_post = (rate_y2022 + rate_y2023) / 2,
      decline19_diff = rate_post - rate_y2019,
      decline19_log = log(rate_post / rate_y2019)
    ) %>%
    relocate(system, bin, kind, group)
}
stopifnot(identical(base_years$af, 2019L), all(map_lgl(base_years[setdiff(names(base_years), "af")],
                                                     ~ identical(.x, 2017:2019))))

bins_all <- map_dfr(names(bin_systems), ~ bin_stats(cells, .x))
write_csv_strict(bins_all, f_bins)

# |- decline at the 2017 lien date: 2022-23 against 2017-19 (affidavits: 2019) ----
#' Decline by bin system for the 2017 lien-date cells
decline17_stats <- function(by_fun, system) {
  cells17 %>%
    mutate(bin = by_fun(cells17)) %>%
    group_by(bin, win) %>%
    summarise(across(c(n_all, n_risk, matches("^ev")), sum), .groups = "drop") %>%
    pivot_longer(matches("^ev"), names_to = c("kind", "group"),
                 names_pattern = "^(ev|evall)_(.+)$", values_to = "cnt") %>%
    mutate(denom = if_else(kind == "evall", n_all, n_risk), rate = 1000 * cnt / denom) %>%
    select(bin, win, kind, group, cnt, denom, rate) %>%
    pivot_wider(names_from = win, values_from = c(cnt, denom, rate)) %>%
    mutate(system = system,
           base = map_chr(group, ~ paste(range(base_years[[.x]]), collapse = "-")),
           rate_pre = if_else(group == "af", rate_y2019, (rate_y2017 + rate_y2018 + rate_y2019) / 3),
           rate_post = (rate_y2022 + rate_y2023) / 2,
           decline_diff = rate_post - rate_pre,
           decline_log = log(rate_post / rate_pre)) %>%
    relocate(system, bin, kind, group)
}
decline17 <- bind_rows(decline17_stats(function(d) rep("all", nrow(d)), "all"),
                       decline17_stats(function(d) d$wbin, "wedge"),
                       decline17_stats(function(d) d$tenure, "tenure"))
write_csv_strict(decline17, f_decline17)
message("Decline 2022-23 against 2017-19, wedge at the January 2017 lien date (at-risk parcels):")
decline17 %>% filter(kind == "ev", group %in% c("main", "interg", "spouse", "af", "market"),
                     system %in% c("all", "wedge")) %>%
  select(group, bin, denom_y2019, rate_pre, rate_post, decline_log) %>%
  mutate(across(where(is.numeric), ~ signif(.x, 4))) %>% as.data.frame() %>% print(row.names = FALSE)

show_bins <- function(sys, kind_ = "ev", groups = c("main", "spouse", "af")) {
  bins_all %>%
    filter(system == sys, kind == kind_, group %in% groups) %>%
    select(group, bin, denom_jf2021, cnt_jf2019, cnt_jf2021, rate_jf2019, rate_jf2021, rush_ratio,
           deadline_2021, deadline_2019, rate_y2019, rate_post, decline19_log) %>%
    mutate(across(where(is.numeric), ~ signif(.x, 4))) %>%
    as.data.frame() %>% print(row.names = FALSE)
}
message("Rush, deadline ratio and decline by wedge bin (at-risk parcels):")
show_bins("wedge")
show_bins("all", groups = names(group_codes))

#============================================#
# Dose-response regressions (county FE, county-clustered) ----
#============================================#

#' Cells aggregated to county x wedge bin x window for one group
#' @param kind "ev" (at-risk) or "evall" (fixed universe).
#' @param lien "2019" (cells) or "2017" (cells17, decline design).
wedge_cells <- function(group, wins, kind = "ev", lien = "2019") {
  cnt_col <- paste0(kind, "_", group)
  den_col <- if (kind == "evall") "n_all" else "n_risk"
  src <- if (lien == "2017") cells17 else cells
  doses <- if (lien == "2017") dose_usd17 else dose_usd
  src %>%
    filter(win %in% wins) %>%
    group_by(fips5, wbin, win) %>%
    summarise(cnt = sum(.data[[cnt_col]]), n_risk = sum(.data[[den_col]]), .groups = "drop") %>%
    filter(n_risk > 0) %>%
    mutate(group = group, wdec = as.integer(substr(wbin, 2, 3)), zero = as.integer(wdec == 0L),
           dose_rank = wdec, dose_usd = unname(doses[wbin]), rate = 1000 * cnt / n_risk)
}

#' Tidy coefficients of a fixest model, with the number of county clusters used
tidy_fit <- function(m, outcome, model, group, dose) {
  ct <- coeftable(m)
  n_g <- tryCatch(as.integer(min(attr(vcov(m, attr = TRUE), "G"))), error = function(e) NA_integer_)
  tibble(outcome = outcome, model = model, group = group, dose = dose,
         term = rownames(ct), estimate = ct[, 1], se = ct[, 2], pval = ct[, 4],
         nobs = nobs(m), n_clusters = n_g)
}
n_counties <- n_distinct(parcels$fips5)

#' Dose-response line for a two-period contrast (rush, decline)
#'
#' Poisson (proportional: log rush ratio per step) and weighted LPM (levels: transfers per
#' 1,000 parcels per step), county x window and county x bin effects, county-clustered.
#' The zero-wedge bin has its own post indicator, so the line is fitted on the deciles.
fit_dose_line <- function(d, outcome, post_wins) {
  d <- d %>% mutate(post = as.integer(win %in% post_wins))
  out <- list()
  for (dose in c("rank", "usd")) {
    d$pd <- d$post * d[[paste0("dose_", dose)]]
    d$pz <- d$post * d$zero
    m_p <- fepois(cnt ~ pd + pz | fips5^win + fips5^wbin, offset = ~ log(n_risk),
                  cluster = ~ fips5, data = d, notes = FALSE)
    m_o <- feols(rate ~ pd + pz | fips5^win + fips5^wbin, weights = ~ n_risk,
                 cluster = ~ fips5, data = d, notes = FALSE)
    out[[length(out) + 1]] <- tidy_fit(m_p, outcome, "poisson", d$group[1], dose)
    out[[length(out) + 1]] <- tidy_fit(m_o, outcome, "lpm_per1000", d$group[1], dose)
  }
  bind_rows(out)
}

#' Main minus placebo slope from a stacked model (same fixed effects by group)
fit_dose_diff <- function(d_main, d_plac, outcome, post_wins, dose = "rank") {
  d <- bind_rows(d_main %>% mutate(is_main = 1L), d_plac %>% mutate(is_main = 0L)) %>%
    mutate(post = as.integer(win %in% post_wins),
           pd = post * .data[[paste0("dose_", dose)]], pz = post * zero,
           pd_main = pd * is_main, pz_main = pz * is_main)
  m <- fepois(cnt ~ pd + pz + pd_main + pz_main | fips5^win^is_main + fips5^wbin^is_main,
              offset = ~ log(n_risk), cluster = ~ fips5, data = d, notes = FALSE)
  tidy_fit(m, outcome, "poisson_main_minus_placebo",
           paste0(d_main$group[1], "_minus_", d_plac$group[1]), dose)
}

#' Per-bin post effects relative to the zero-wedge bin (Poisson)
fit_bin_coefs <- function(d, outcome, post_wins) {
  d <- d %>% mutate(post = as.integer(win %in% post_wins))
  m <- fepois(cnt ~ i(wbin, post, ref = "W00") | fips5^win + fips5^wbin,
              offset = ~ log(n_risk), cluster = ~ fips5, data = d, notes = FALSE)
  tidy_fit(m, outcome, "poisson_bins_vs_zero", d$group[1], "bins")
}

slope_groups <- c("main", "interg", "spouse", "af", "pc", "heir")
rush_wins <- c("jf2019", "jf2021")
decline_wins_of <- function(g) paste0("y", c(base_years[[g]], post_years))

slopes <- list(); bin_coefs <- list()
for (g in slope_groups) {
  # (a) rush
  d_rush <- wedge_cells(g, rush_wins)
  slopes[[length(slopes) + 1]] <- fit_dose_line(d_rush, "rush_jf2021_vs_jf2019", "jf2021")
  bin_coefs[[length(bin_coefs) + 1]] <- fit_bin_coefs(d_rush, "rush_jf2021_vs_jf2019", "jf2021")
  # fixed-universe sensitivity (no risk-set rule)
  d_rush_fx <- wedge_cells(g, rush_wins, kind = "evall")
  slopes[[length(slopes) + 1]] <- fit_dose_line(d_rush_fx, "rush_jf2021_vs_jf2019_fixed_universe", "jf2021")
  # (d) decline: 2022-23 against the group's base years, wedge at the 2017 lien date
  d_dec <- wedge_cells(g, decline_wins_of(g), lien = "2017")
  slopes[[length(slopes) + 1]] <- fit_dose_line(d_dec, "decline_2022_23_vs_base_lien2017", paste0("y", post_years))
  bin_coefs[[length(bin_coefs) + 1]] <- fit_bin_coefs(d_dec, "decline_2022_23_vs_base_lien2017",
                                                      paste0("y", post_years))
  d_dec_fx <- wedge_cells(g, decline_wins_of(g), kind = "evall", lien = "2017")
  slopes[[length(slopes) + 1]] <- fit_dose_line(d_dec_fx, "decline_2022_23_vs_base_lien2017_fixed_universe",
                                                paste0("y", post_years))
  # sensitivity: 2019 lien date, 2022-23 against 2019 only
  d_dec19 <- wedge_cells(g, paste0("y", c(2019L, post_years)))
  slopes[[length(slopes) + 1]] <- fit_dose_line(d_dec19, "decline_2022_23_vs_2019_lien2019", paste0("y", post_years))
}

# |- main minus placebo slopes ----
for (p in placebo_codes) {
  for (mg in c("main", "interg")) {
    slopes[[length(slopes) + 1]] <- fit_dose_diff(wedge_cells(mg, rush_wins), wedge_cells(p, rush_wins),
                                                  "rush_jf2021_vs_jf2019", "jf2021")
    slopes[[length(slopes) + 1]] <- fit_dose_diff(wedge_cells(mg, decline_wins_of(mg), lien = "2017"),
                                                  wedge_cells(p, decline_wins_of(p), lien = "2017"),
                                                  "decline_2022_23_vs_base_lien2017", paste0("y", post_years))
  }
}

# |- (b) deadline ratio: after x 2021 x dose, against February 2019 ----
fit_deadline <- function(g) {
  d <- wedge_cells(g, c("feb2019_pre", "feb2019_post", "feb2021_pre", "feb2021_post")) %>%
    mutate(year = substr(win, 4, 7), after = as.integer(grepl("post", win)),
           y21 = as.integer(year == "2021"),
           days = if_else(after == 1L, 12, 16),
           ad = after * dose_rank, az = after * zero, ad21 = ad * y21, az21 = az * y21)
  m_rel <- fepois(cnt ~ ad + az + ad21 + az21 | fips5^wbin^year + fips5^after^year,
                  offset = ~ log(days), cluster = ~ fips5, data = d, notes = FALSE)
  m_21 <- fepois(cnt ~ ad + az | fips5^wbin + fips5^after, offset = ~ log(days),
                 cluster = ~ fips5, data = d %>% filter(y21 == 1L), notes = FALSE)
  bind_rows(tidy_fit(m_rel, "deadline_2021_vs_2019", "poisson", g, "rank"),
            tidy_fit(m_21, "deadline_2021_only", "poisson", g, "rank"))
}
for (g in slope_groups) slopes[[length(slopes) + 1]] <- fit_deadline(g)

# |- (c) resale: rushed deeds against the reference cohorts, by wedge ----
resale_univ <- resale_deeds %>%
  filter(in_universe) %>%
  mutate(wdec = as.integer(substr(wbin, 2, 3)), zero = as.integer(wdec == 0L),
         dose_rank = wdec,
         dose_usd = if_else(lien == "2017", unname(dose_usd17[wbin]), unname(dose_usd[wbin])),
         sold24_pp = 100 * sold24)
fit_resale <- function(g, treat = c("rush", "jf21")) {
  treat <- match.arg(treat)
  d <- resale_univ %>% filter(g == !!g)
  if (treat == "jf21") d <- d %>% filter(cohort == "ref" | jf21 == 1L)
  d <- d %>% mutate(tr = as.integer(cohort == "rush"), td = tr * dose_rank, tz = tr * zero)
  m <- feols(sold24_pp ~ td + tz | fips5^tr + fips5^wbin, cluster = ~ fips5, data = d, notes = FALSE)
  tidy_fit(m, paste0("resale24_", treat, "_vs_ref"), "lpm_pp", g, "rank")
}
fit_resale_diff <- function(g_main, g_plac) {
  d <- resale_univ %>% filter(g %in% c(g_main, g_plac)) %>%
    mutate(is_main = as.integer(g == g_main), tr = as.integer(cohort == "rush"),
           td = tr * dose_rank, tz = tr * zero, td_main = td * is_main, tz_main = tz * is_main)
  m <- feols(sold24_pp ~ td + tz + td_main + tz_main | fips5^tr^is_main + fips5^wbin^is_main,
             cluster = ~ fips5, data = d, notes = FALSE)
  tidy_fit(m, "resale24_rush_vs_ref", "lpm_pp_main_minus_placebo", paste0(g_main, "_minus_", g_plac), "rank")
}
for (g in slope_groups) {
  slopes[[length(slopes) + 1]] <- fit_resale(g, "rush")
  slopes[[length(slopes) + 1]] <- fit_resale(g, "jf21")
}
for (mg in c("main", "interg")) slopes[[length(slopes) + 1]] <- fit_resale_diff(mg, "spouse")

slopes_tbl <- bind_rows(slopes)
write_csv_strict(slopes_tbl, f_slopes)
bin_coefs_tbl <- bind_rows(bin_coefs)
write_csv_strict(bin_coefs_tbl, f_bincoef)

message("Dose-response slopes (rank = one decile step; terms pd/td/ad21):")
slopes_tbl %>%
  filter(dose == "rank", term %in% c("pd", "td", "ad21", "pd_main", "td_main")) %>%
  select(outcome, model, group, term, estimate, se, pval, nobs) %>%
  mutate(across(c(estimate, se, pval), ~ signif(.x, 3))) %>%
  as.data.frame() %>% print(row.names = FALSE)

# |- resale bin table ----
resale_bins <- resale_univ %>%
  mutate(wgroup = wgroup_of(wbin)) %>%
  { bind_rows(mutate(., bin = wbin, system = "wedge"),
              mutate(., bin = "all", system = "all"),
              mutate(., bin = tenure, system = "tenure")) } %>%
  group_by(system, bin, g) %>%
  summarise(n_rush = sum(cohort == "rush"), n_jf21 = sum(jf21 == 1L), n_ref = sum(cohort == "ref"),
            sold24_rush = mean(sold24[cohort == "rush"]), sold24_jf21 = mean(sold24[jf21 == 1L]),
            sold24_ref = mean(sold24[cohort == "ref"]), .groups = "drop") %>%
  mutate(diff_rush_pp = 100 * (sold24_rush - sold24_ref),
         diff_jf21_pp = 100 * (sold24_jf21 - sold24_ref))
resale_bins <- resale_bins %>%
  left_join(resale_bins %>% filter(g == "spouse") %>%
              select(system, bin, diff_rush_spouse_pp = diff_rush_pp), by = c("system", "bin")) %>%
  mutate(dd_vs_spouse_pp = diff_rush_pp - diff_rush_spouse_pp)
write_csv_strict(resale_bins, f_resale)
message("Resale within 24 months by wedge bin (main and spouse):")
resale_bins %>% filter(system %in% c("all", "wedge"), g %in% c("main", "spouse")) %>%
  select(g, bin, n_rush, n_ref, sold24_rush, sold24_ref, diff_rush_pp, dd_vs_spouse_pp) %>%
  mutate(across(where(is.double), ~ signif(.x, 4))) %>%
  as.data.frame() %>% print(row.names = FALSE)

#============================================#
# Value cap x recipient occupancy; parent's gain; tenure ----
#============================================#

# |- cap: occupant vs non-occupant recipients, gap $500k-$1m against above $1m ----
# Framework: an occupant heir keeps the base up to the cap, so the deadline saving for an
# occupant is t * max(p - A0 - L, 0): rush above the cap only. A non-occupant loses the full
# wedge, so the rush should not jump at the cap. Test: post x above x occupant.
cap_cells <- function(g) {
  cells %>%
    filter(win %in% rush_wins, capbin %in% c("C3_500_750k", "C4_750k_1m", "C5_1_1.5m", "C6_1.5m_plus")) %>%
    group_by(fips5, capbin, win) %>%
    summarise(occ1 = sum(.data[[paste0("ev1_", g)]]), occ0 = sum(.data[[paste0("ev0_", g)]]),
              n_risk = sum(n_risk), .groups = "drop") %>%
    pivot_longer(c(occ1, occ0), names_to = "occ", values_to = "cnt") %>%
    filter(n_risk > 0) %>%
    mutate(occ1 = as.integer(occ == "occ1"), post = as.integer(win == "jf2021"),
           above = as.integer(capbin %in% c("C5_1_1.5m", "C6_1.5m_plus")),
           pa = post * above, pa_occ = pa * occ1)
}
cap_fits <- map_dfr(c("main", "interg", "pc", "spouse"), function(g) {
  d <- cap_cells(g)
  m_occ <- fepois(cnt ~ pa + pa_occ | fips5^win^occ + fips5^capbin^occ, offset = ~ log(n_risk),
                  cluster = ~ fips5, data = d, notes = FALSE)
  m_o1 <- fepois(cnt ~ pa | fips5^win + fips5^capbin, offset = ~ log(n_risk), cluster = ~ fips5,
                 data = d %>% filter(occ1 == 1L), notes = FALSE)
  m_o0 <- fepois(cnt ~ pa | fips5^win + fips5^capbin, offset = ~ log(n_risk), cluster = ~ fips5,
                 data = d %>% filter(occ1 == 0L), notes = FALSE)
  bind_rows(tidy_fit(m_occ, "rush_cap_x_occupancy", "poisson", g, "above_cap"),
            tidy_fit(m_o1, "rush_cap_occupant_recipients", "poisson", g, "above_cap"),
            tidy_fit(m_o0, "rush_cap_nonoccupant_recipients", "poisson", g, "above_cap"))
})
cap_bins <- bins_all %>%
  filter(system %in% c("cap", "over_cap"), kind %in% c("ev", "ev1", "ev0"),
         group %in% c("main", "interg", "pc", "spouse")) %>%
  select(system, bin, kind, group, denom_jf2021, cnt_jf2019, cnt_jf2021, rate_jf2019, rate_jf2021,
         rush_ratio, deadline_2021, deadline_2019)
write_csv_strict(bind_rows(cap_bins %>% mutate(table = "bins"),
                           cap_fits %>% mutate(table = "fits")), f_capocc)
message("Cap x occupancy (Poisson, log points; pa_occ = extra above-cap rush for occupant recipients):")
cap_fits %>% select(outcome, group, term, estimate, se, pval, nobs) %>%
  mutate(across(c(estimate, se, pval), ~ signif(.x, 3))) %>% as.data.frame() %>% print(row.names = FALSE)

# |- parent's gain within wedge deciles, and tenure ----
#' Gain-tercile cells (county x wedge decile x gain tercile x window), parcels with a basis
gain_cells <- function(g) {
  cells %>%
    filter(win %in% rush_wins, gainbin != "B0_unmeasured") %>%
    group_by(fips5, wbin, gainbin, win) %>%
    summarise(cnt = sum(.data[[paste0("ev_", g)]]), n_risk = sum(n_risk), .groups = "drop") %>%
    filter(n_risk > 0) %>%
    mutate(post = as.integer(win == "jf2021"), grank = as.integer(substr(gainbin, 2, 2)),
           pg = post * grank, pg3 = post * as.integer(grank == 3L), pg2 = post * as.integer(grank == 2L))
}
gain_tenure_fits <- map_dfr(c("main", "interg", "pc", "spouse", "af"), function(g) {
  d_gain <- gain_cells(g)
  m_g <- fepois(cnt ~ pg | fips5^wbin^win + fips5^wbin^gainbin, offset = ~ log(n_risk),
                cluster = ~ fips5, data = d_gain, notes = FALSE)
  m_g3 <- fepois(cnt ~ pg2 + pg3 | fips5^wbin^win + fips5^wbin^gainbin, offset = ~ log(n_risk),
                 cluster = ~ fips5, data = d_gain, notes = FALSE)
  d_ten <- cells %>%
    filter(win %in% rush_wins) %>%
    group_by(fips5, wbin, tenure, win) %>%
    summarise(cnt = sum(.data[[paste0("ev_", g)]]), n_risk = sum(n_risk), .groups = "drop") %>%
    filter(n_risk > 0) %>%
    mutate(post = as.integer(win == "jf2021"),
           p_long = post * as.integer(tenure == "long_held"),
           p_0712 = post * as.integer(tenure == "bought_2007_2012"))
  m_t <- fepois(cnt ~ p_long + p_0712 | fips5^win + fips5^tenure, offset = ~ log(n_risk),
                cluster = ~ fips5,
                data = d_ten %>% group_by(fips5, tenure, win, post, p_long, p_0712) %>%
                  summarise(cnt = sum(cnt), n_risk = sum(n_risk), .groups = "drop"), notes = FALSE)
  m_tw <- fepois(cnt ~ p_long + p_0712 | fips5^wbin^win + fips5^wbin^tenure, offset = ~ log(n_risk),
                 cluster = ~ fips5, data = d_ten, notes = FALSE)
  bind_rows(tidy_fit(m_g, "rush_gain_tercile_within_wedge_decile", "poisson", g, "gain_rank"),
            tidy_fit(m_g3, "rush_gain_tercile_within_wedge_decile", "poisson", g, "gain_terciles"),
            tidy_fit(m_t, "rush_tenure", "poisson", g, "tenure_vs_bought_2013_plus"),
            tidy_fit(m_tw, "rush_tenure_within_wedge_decile", "poisson", g, "tenure_vs_bought_2013_plus"))
})
# main minus placebo gain slopes (stacked, effects by group)
gain_diff_fits <- map_dfr(c("main", "interg"), function(mg) {
  map_dfr(placebo_codes, function(p) {
    d <- bind_rows(gain_cells(mg) %>% mutate(is_main = 1L), gain_cells(p) %>% mutate(is_main = 0L)) %>%
      mutate(pg_main = pg * is_main)
    m <- fepois(cnt ~ pg + pg_main | fips5^wbin^win^is_main + fips5^wbin^gainbin^is_main,
                offset = ~ log(n_risk), cluster = ~ fips5, data = d, notes = FALSE)
    tidy_fit(m, "rush_gain_tercile_within_wedge_decile", "poisson_main_minus_placebo",
             paste0(mg, "_minus_", p), "gain_rank")
  })
})
gain_tenure_fits <- bind_rows(gain_tenure_fits, gain_diff_fits)
gain_tenure_bins <- bins_all %>%
  filter(system %in% c("gain", "tenure", "wedge3_gain", "wedge3_tenure", "hox"), kind == "ev",
         group %in% c("main", "interg", "pc", "spouse", "af")) %>%
  select(system, bin, group, denom_jf2021, cnt_jf2019, cnt_jf2021, rate_jf2019, rate_jf2021,
         rush_ratio, deadline_2021, deadline_2019, rate_y2019, rate_post, decline19_log)
write_csv_strict(bind_rows(gain_tenure_bins %>% mutate(table = "bins"),
                           gain_tenure_fits %>% mutate(table = "fits")), f_gain)
message("Parent's gain and tenure (Poisson, log points):")
gain_tenure_fits %>% select(outcome, group, dose, term, estimate, se, pval, nobs) %>%
  mutate(across(c(estimate, se, pval), ~ signif(.x, 3))) %>% as.data.frame() %>% print(row.names = FALSE)

#============================================#
# Number macros (RTWO) and sources ----
#============================================#

macro_rows <- list()
#' Record one macro with its source file
add_macro <- function(name, value, digits, source_file, note = "") {
  stopifnot(grepl("^RTWO[A-Za-z]+$", name), length(value) == 1L, is.finite(value))
  macro_rows[[name]] <<- tibble(macro = name, value = value, digits = digits,
                                formatted = formatC(value, format = "f", digits = digits, big.mark = ","),
                                source_file = path_rel(source_file, project_dir),
                                script = script_name, note = note)
}
bget <- function(sys, bin_, group_, col, kind_ = "ev") {
  v <- bins_all[[col]][bins_all$system == sys & bins_all$bin == bin_ &
                         bins_all$group == group_ & bins_all$kind == kind_]
  stopifnot(length(v) == 1L)
  v
}
sget <- function(outcome_, group_, term_, model_ = "poisson", dose_ = "rank", col = "estimate", tbl = slopes_tbl) {
  v <- tbl[[col]][tbl$outcome == outcome_ & tbl$group == group_ & tbl$term == term_ &
                    tbl$model == model_ & tbl$dose == dose_]
  stopifnot(length(v) == 1L)
  v
}
word <- c(main = "Main", interg = "Interg", spouse = "Spouse", af = "Affidavit", pc = "ParentChild", heir = "Heir")

add_macro("RTWOParcels", nrow(parcels), 0, f_doses, "parcels with a 2019 wedge")
add_macro("RTWOZeroShare", 100 * mean(parcels$wedge <= 0), 1, f_doses, "percent zero wedge")
add_macro("RTWOAtRiskJFTwentyOne", risk_audit$n_risk[risk_audit$win == "jf2021" & risk_audit$lien == "2019-01-01"],
          0, f_risk,
          "parcels at risk Jan 2021")
add_macro("RTWOCoverageMainJF", 100 * coverage$share_in_universe[coverage$period == "jf2021" & coverage$group == "main"],
          1, f_coverage, "percent of CA main-measure deeds Jan-Feb 2021 on universe parcels")
for (b in c("W00", "W01", "W05", "W10")) {
  tag <- c(W00 = "Zero", W01 = "DOne", W05 = "DFive", W10 = "DTen")[[b]]
  for (g in c("main", "spouse", "af")) {
    add_macro(paste0("RTWORush", word[[g]], tag), bget("wedge", b, g, "rush_ratio"), 2, f_bins)
    add_macro(paste0("RTWODeadline", word[[g]], tag), bget("wedge", b, g, "deadline_2021"), 2, f_bins)
    v <- decline17$decline_log[decline17$system == "wedge" & decline17$bin == b &
                                 decline17$group == g & decline17$kind == "ev"]
    stopifnot(length(v) == 1L)
    add_macro(paste0("RTWODecline", word[[g]], tag), v, 3, f_decline17, "log change, 2017 lien date")
  }
  add_macro(paste0("RTWOMeanWedge", tag), bin_doses$wedge_mean[bin_doses$wbin == b], 0, f_doses)
}
for (g in c("main", "interg", "spouse", "af")) {
  add_macro(paste0("RTWORushSlope", word[[g]]), sget("rush_jf2021_vs_jf2019", g, "pd"), 3, f_slopes,
            "Poisson log points per decile")
  add_macro(paste0("RTWORushSlope", word[[g]], "SE"), sget("rush_jf2021_vs_jf2019", g, "pd", col = "se"), 3, f_slopes)
  add_macro(paste0("RTWORushLevelSlope", word[[g]]),
            sget("rush_jf2021_vs_jf2019", g, "pd", model_ = "lpm_per1000"), 3, f_slopes, "per 1,000 parcels per decile")
  add_macro(paste0("RTWORushLevelSlope", word[[g]], "SE"),
            sget("rush_jf2021_vs_jf2019", g, "pd", model_ = "lpm_per1000", col = "se"), 3, f_slopes)
  add_macro(paste0("RTWODeadlineSlope", word[[g]]), sget("deadline_2021_vs_2019", g, "ad21"), 3, f_slopes)
  add_macro(paste0("RTWODeadlineSlope", word[[g]], "SE"), sget("deadline_2021_vs_2019", g, "ad21", col = "se"), 3, f_slopes)
  add_macro(paste0("RTWODeclineSlope", word[[g]]), sget("decline_2022_23_vs_base_lien2017", g, "pd"), 3, f_slopes)
  add_macro(paste0("RTWODeclineSlope", word[[g]], "SE"),
            sget("decline_2022_23_vs_base_lien2017", g, "pd", col = "se"), 3, f_slopes)
  add_macro(paste0("RTWOResaleSlope", word[[g]]), sget("resale24_rush_vs_ref", g, "td", model_ = "lpm_pp"), 3, f_slopes)
  add_macro(paste0("RTWOResaleSlope", word[[g]], "SE"),
            sget("resale24_rush_vs_ref", g, "td", model_ = "lpm_pp", col = "se"), 3, f_slopes)
}
for (p in placebo_codes) {
  add_macro(paste0("RTWORushSlopeMainMinus", word[[p]]),
            sget("rush_jf2021_vs_jf2019", paste0("main_minus_", p), "pd_main", model_ = "poisson_main_minus_placebo"),
            3, f_slopes)
  add_macro(paste0("RTWORushSlopeMainMinus", word[[p]], "SE"),
            sget("rush_jf2021_vs_jf2019", paste0("main_minus_", p), "pd_main", model_ = "poisson_main_minus_placebo",
                 col = "se"), 3, f_slopes)
}
add_macro("RTWOCapOccMain",cap_fits$estimate[cap_fits$outcome == "rush_cap_x_occupancy" &
                                                cap_fits$group == "main" & cap_fits$term == "pa_occ"], 3, f_capocc)
add_macro("RTWOCapOccMainSE", cap_fits$se[cap_fits$outcome == "rush_cap_x_occupancy" &
                                            cap_fits$group == "main" & cap_fits$term == "pa_occ"], 3, f_capocc)
add_macro("RTWOGainSlopeMain", sget("rush_gain_tercile_within_wedge_decile", "main", "pg", dose_ = "gain_rank",
                                    tbl = gain_tenure_fits), 3, f_gain)
add_macro("RTWOGainSlopeMainSE", sget("rush_gain_tercile_within_wedge_decile", "main", "pg", dose_ = "gain_rank",
                                      col = "se", tbl = gain_tenure_fits), 3, f_gain)

#' Merge this script's macros into the shared sources CSV and rewrite the .tex file
write_r2_macros <- function(rows, script) {
  old <- if (file_exists(f_sources)) read_csv(f_sources, show_col_types = FALSE,
                                              col_types = cols(.default = "c")) else NULL
  new <- rows %>% mutate(across(everything(), as.character))
  all_rows <- bind_rows(if (!is.null(old)) old %>% filter(script != !!script), new) %>% arrange(macro)
  stopifnot(!anyDuplicated(all_rows$macro))
  write_csv_strict(all_rows, f_sources)
  writeLines(c("% Generated by 25_wedge_gradient.R and 26_heir_sale_hazard.R (WP-R2); do not edit by hand.",
               "% Sources: scripts/R/_outputs/tables/r2_numbers_sources.csv",
               paste0("\\newcommand{\\", all_rows$macro, "}{", all_rows$formatted, "}")), f_macros)
  message("Wrote: ", f_macros, " (", nrow(all_rows), " macros)")
}
write_r2_macros(bind_rows(macro_rows), script_name)

message("Finished 25_wedge_gradient at ", Sys.time(), " (",
        round(difftime(Sys.time(), t_start, units = "mins"), 1), " min)")
