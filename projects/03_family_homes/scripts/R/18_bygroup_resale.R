# ============================================================
# 18: Resale of rushed transfers, by deed group (Phase 3, item 6)
# Name   : Saani Rawat
# Created: 11/11/2026
# Purpose: Share of deeds followed by a market sale of the same parcel within
#          24 months, by group, for the deadline-rush cohort against earlier
#          reference cohorts, California against the other states pooled.
#          Descriptive: cohort means and a difference of differences in means;
#          no new regression.
# Inputs : events.parquet (see `panel_file` switch), 00_groups.R,
#          00_group_helpers.R, 00_date_helpers.R
# Outputs: _outputs/tables/bygroup_resale_cohort_means.csv
#          _outputs/tables/bygroup_resale_dd.csv
#          _outputs/tables/bygroup_resale_censoring.csv
#          (dev runs write into _outputs/tables/dev/ instead)
# Log    : 11/11/2026: created.
#
# Outcome (same logic as 04_prop19.R l.387-413): sold24 = 1 if the same parcel
#   (clip) has a market_sale deed strictly after the deed's sale_date and on or
#   before sale_date + 24 calendar months (month-end clamped).
# Population: deeds of the group with corp_buyer = 0 and a valid sale date,
#   50 states + DC (no territories); identical deeds collapsed as in
#   join_next_market_sale() (distinct on clip, state, ym, sale_date,
#   absentee_buyer, within the group).
# Censoring rule: a deed is kept only if sale_date + 24 months <= 30 June 2024
#   (last usable month 202406; the data end in mid-2024). No cohort used here
#   extends past 202206, so the rule drops nothing in the main cohorts; the
#   count dropped is printed and saved. A deed with no market sale within 24
#   months, or none at all, counts as 0.
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_groups.R"))
source(path(r_dir, "00_group_helpers.R"))


# ============================================================
# 1. Settings ----
# ============================================================

# ONE switch: panel to read. Dev panel = VT, RI, CA only (not results).
panel_file <- events_panel_file     # dev: "events_dev.parquet"
panel_path <- path(data_dir, panel_file)
is_dev     <- grepl("dev", panel_file)

# dev numbers go to a subfolder so they cannot be mistaken for results
out_tables <- if (is_dev) path(tables_out_dir, "dev") else tables_out_dir
log_dir    <- if (is_dev) path(logs_dir, "dev") else logs_dir
dir_create(out_tables)
dir_create(log_dir)

# months (YYYYMM) of the cohorts, by deed month ym
cohort_ranges <- tribble(
  ~cohort,        ~lo,    ~hi,
  "rush",         202006L, 202102L,   # ballot placement to deadline month
  "jan_feb_2021", 202101L, 202102L,   # sub-cohort of the rush
  "ref_pooled",   201806L, 201902L,   # same calendar months, two years earlier
  "ref_pooled",   201906L, 202002L,   # same calendar months, one year earlier
  "ref_A",        201806L, 201902L,   # sensitivity: first reference alone
  "ref_B",        201906L, 202002L,   # second reference alone
  "legacy_pre",   201701L, 201812L,   # existing cohort design (reproduction)
  "legacy_post",  202107L, 202206L
)

# (treated cohort, reference cohort) pairs for the difference of differences
contrasts <- tribble(
  ~treat,         ~ref,
  "rush",         "ref_pooled",
  "rush",         "ref_A",
  "jan_feb_2021", "ref_pooled",
  "jan_feb_2021", "ref_A",
  "legacy_post",  "legacy_pre"
)

horizon_months <- 24L
censor_ym      <- bygroup_windows$hole_end_main                    # 202406
censor_date    <- as.Date(sprintf("%d-%02d-01", censor_ym %/% 100L, censor_ym %% 100L))
censor_date    <- seq(censor_date, by = "month", length.out = 2)[2] - 1  # month end
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")
min_ca_per_year <- bygroup_min_ca_deeds

# cross-state groups plus the California-only series (spouse_ca etc.: no "other" rows)
groups_run <- bygroup_defs

log_file <- path(log_dir, "18_bygroup_resale.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 18_bygroup_resale at ", Sys.time(), " | panel: ", panel_file,
        if (is_dev) "  ** DEV PANEL: numbers are not results **" else "")
message("Censoring rule: keep deeds with sale_date + ", horizon_months,
        " months <= ", censor_date)


# ============================================================
# 2. Cohort deeds and market sales (duckdb) ----
# ============================================================

# memory capped at ~20% of the 63 GB machine (another pipeline runs at the same time);
# spill goes to a local disk outside the OneDrive tree
duck_mem     <- "12GB"
duck_threads <- 8L
spill_dir    <- "C:/Temp/wpB_duckdb"
dir_create(spill_dir)

stopifnot(file_exists(panel_path))
con <- open_corelogic_duckdb(memory_limit = duck_mem, threads = duck_threads)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(spill_dir)}'"))

ev <- glue("read_parquet('{sql_quote_path(panel_path)}')")
terr_sql  <- paste0("('", paste(territories, collapse = "','"), "')")
months_sql <- paste(sprintf("(ym BETWEEN %d AND %d)", cohort_ranges$lo, cohort_ranges$hi),
                    collapse = " OR ")

# deeds in any cohort month, with a parsed sale date
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE coh_all AS
  SELECT clip, state, ym, class, doc_type, fam_vehicle, fam_trigger, fam_relation,
         fam_relation_nm, trust_fam_lo, trust_fam_hi, trust_chain, trust_chain_gap_days,
         absentee_buyer,
         {event_date_sql('sale_raw')} AS sale_date
  FROM {ev}
  WHERE corp_buyer = 0 AND state NOT IN {terr_sql} AND ({months_sql})
"))

# date and censoring audit, in the spirit of 04_prop19.R l.377-384
audit <- dbGetQuery(con, glue("
  SELECT COUNT(*) AS n_cohort_months,
         SUM(CASE WHEN sale_date IS NULL THEN 1 ELSE 0 END) AS n_invalid_date,
         SUM(CASE WHEN sale_date IS NOT NULL
                   AND sale_date + INTERVAL {horizon_months} MONTH > DATE '{censor_date}'
                  THEN 1 ELSE 0 END) AS n_censored
  FROM coh_all"))
print(audit)
stopifnot(audit$n_cohort_months > 0, audit$n_invalid_date / audit$n_cohort_months < 0.001)
write_csv_strict(audit, path(out_tables, "bygroup_resale_censoring.csv"))

dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE coh_base AS
  SELECT * FROM coh_all
  WHERE sale_date IS NOT NULL
    AND sale_date + INTERVAL {horizon_months} MONTH <= DATE '{censor_date}'"))

# market sales: the only outcome events (distinct clip-date, as in the R helper)
dbExecute(con, glue("
  CREATE OR REPLACE TEMP TABLE ms AS
  SELECT DISTINCT clip, {event_date_sql('sale_raw')} AS msale_date
  FROM {ev}
  WHERE class = 'market_sale' AND ym >= 201701"))
dbExecute(con, "DELETE FROM ms WHERE msale_date IS NULL")
message("Market-sale dates: ", format(dbGetQuery(con, "SELECT COUNT(*) n FROM ms")$n, big.mark = ","))

dbWriteTable(con, "cohort_ranges", as.data.frame(cohort_ranges), temporary = TRUE, overwrite = TRUE)


# ============================================================
# 3. Cohort means by group and region ----
# ============================================================

# one ASOF join per group: next market sale strictly after the deed date
resale_query <- function(filter_sql) {
  glue("
    WITH c AS (
      SELECT DISTINCT clip, state, ym, sale_date, absentee_buyer
      FROM coh_base WHERE coalesce({filter_sql}, FALSE)
    ), j AS (
      SELECT c.*, ms.msale_date
      FROM c ASOF LEFT JOIN ms ON c.clip = ms.clip AND c.sale_date < ms.msale_date
    )
    SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS region,
           r.cohort,
           COUNT(*) AS n,
           SUM(CASE WHEN msale_date <= sale_date + INTERVAL {horizon_months} MONTH
                    THEN 1 ELSE 0 END) AS n_sold24,
           COUNT(DISTINCT state) AS n_states
    FROM j JOIN cohort_ranges r ON j.ym BETWEEN r.lo AND r.hi
    GROUP BY ALL")
}

means_list <- vector("list", nrow(groups_run))
for (i in seq_len(nrow(groups_run))) {
  g <- groups_run$name[i]
  t0 <- Sys.time()
  means_list[[i]] <- dbGetQuery(con, resale_query(bygroup_filter(g))) %>%
    as_tibble() %>% mutate(group = g, .before = 1)
  message(sprintf("  [%s] %d rows in %.1fs", g, nrow(means_list[[i]]),
                  as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

cohort_means <- bind_rows(means_list) %>%
  mutate(sold24 = n_sold24 / n) %>%
  arrange(factor(group, levels = groups_run$name), region, cohort)
stopifnot(nrow(cohort_means) > 0, all(cohort_means$sold24 >= 0 & cohort_means$sold24 <= 1))
write_csv_strict(cohort_means, path(out_tables, "bygroup_resale_cohort_means.csv"))

# cohort sizes, as the plan asks (rush, Jan-Feb 2021, reference cohorts)
message("Cohort sizes (deeds) by group and region:")
cohort_means %>%
  filter(cohort %in% c("rush", "jan_feb_2021", "ref_pooled", "ref_A", "ref_B")) %>%
  select(group, region, cohort, n) %>%
  pivot_wider(names_from = cohort, values_from = n) %>%
  as.data.frame() %>% print()


# ============================================================
# 4. Difference of differences in means ----
# ============================================================

wide <- cohort_means %>%
  select(group, region, cohort, n, sold24) %>%
  pivot_wider(names_from = region, values_from = c(n, sold24))

months_by_cohort <- cohort_ranges %>%
  group_by(cohort) %>%
  summarise(months = sum((hi %/% 100L * 12L + hi %% 100L) -
                         (lo %/% 100L * 12L + lo %% 100L) + 1L), .groups = "drop")

dd <- crossing(contrasts, group = groups_run$name) %>%
  mutate(contrast = paste0(treat, "_vs_", ref)) %>%
  left_join(wide %>% rename_with(~ paste0(.x, "_t"), -c(group, cohort)) %>%
              rename(treat = cohort), by = c("group", "treat")) %>%
  left_join(wide %>% rename_with(~ paste0(.x, "_r"), -c(group, cohort)) %>%
              rename(ref = cohort), by = c("group", "ref")) %>%
  transmute(
    group, contrast, treat, ref,
    n_ca_treat = n_CA_t, n_ca_ref = n_CA_r, n_other_treat = n_other_t, n_other_ref = n_other_r,
    ca_treat = sold24_CA_t, ca_ref = sold24_CA_r,
    other_treat = sold24_other_t, other_ref = sold24_other_r,
    diff_ca = ca_treat - ca_ref,
    diff_other = other_treat - other_ref,
    dd = diff_ca - diff_other,                 # difference of differences, share points
    dd_pp = 100 * dd
  ) %>%
  # California deeds a year implied by the treated cohort (rush = 9 months)
  left_join(months_by_cohort, by = c("treat" = "cohort")) %>%
  mutate(ca_per_year_treat = n_ca_treat * 12 / months,
         below_threshold = is.na(ca_per_year_treat) | ca_per_year_treat < min_ca_per_year) %>%
  arrange(factor(group, levels = groups_run$name), contrast)
write_csv_strict(dd, path(out_tables, "bygroup_resale_dd.csv"))

message("Difference of differences in sold24 (percentage points), rush vs pooled reference:")
dd %>% filter(contrast == "rush_vs_ref_pooled") %>%
  select(group, n_ca_treat, ca_treat, ca_ref, other_treat, other_ref, dd_pp, below_threshold) %>%
  as.data.frame() %>% print(digits = 4)


# ============================================================
# 5. Checks ----
# ============================================================

# (a) duckdb ASOF logic against the existing R helpers (dev panel only: the
#     full cohort table is too large to pull into R)
if (is_dev) {
  g_chk <- "main_proxy"
  chk_c <- dbGetQuery(con, glue("
    SELECT DISTINCT clip, state, ym, sale_date, absentee_buyer
    FROM coh_base WHERE coalesce({bygroup_filter(g_chk)}, FALSE)")) %>% as_tibble()
  chk_ms <- dbGetQuery(con, "SELECT clip, msale_date FROM ms") %>% as_tibble()
  chk_r <- join_next_market_sale(chk_c, chk_ms) %>%
    mutate(sold24 = coalesce(next_market_date <= add_calendar_months(sale_date, horizon_months),
                             FALSE),
           region = if_else(state == "CA", "CA", "other"))
  chk_r_cell <- cohort_ranges %>%
    filter(cohort %in% c("rush", "ref_pooled", "legacy_pre", "legacy_post")) %>%
    inner_join(chk_r, by = join_by(between(y$ym, x$lo, x$hi))) %>%
    group_by(region, cohort) %>%
    summarise(n_r = n(), n_sold24_r = sum(sold24), .groups = "drop")
  cmp <- cohort_means %>% filter(group == g_chk) %>%
    inner_join(chk_r_cell, by = c("region", "cohort"))
  print(as.data.frame(cmp %>% select(region, cohort, n, n_r, n_sold24, n_sold24_r)))
  stopifnot(nrow(cmp) > 0, all(cmp$n == cmp$n_r), all(cmp$n_sold24 == cmp$n_sold24_r))
  message("duckdb ASOF logic equals join_next_market_sale() on the dev panel (", nrow(cmp), " cells)")
}

# (b) existing raw 2x2 for the legacy cohorts (full panel only; the legacy file
#     comes from the full panel, so the comparison is meaningless on dev)
legacy_file <- path(tables_out_dir, "prop19_raw_2x2_fam.csv")
if (!is_dev && file_exists(legacy_file)) {
  leg <- readr::read_csv(legacy_file, show_col_types = FALSE)
  new <- cohort_means %>% filter(group == "main_proxy", cohort %in% c("legacy_pre", "legacy_post")) %>%
    transmute(ca = as.integer(region == "CA"), post = as.integer(cohort == "legacy_post"),
              n_new = n, sold24_new = sold24)
  # legacy pools the 50 states + DC "other" cells the same way (deed-weighted)
  cmp <- inner_join(leg, new, by = c("ca", "post"))
  print(as.data.frame(cmp), digits = 12)
  # the panel uses a deterministic dedupe, so a deed or two can sit in a different
  # cell than in the October 2026 panel: allow a handful of deeds and a 1e-5 gap in rates
  message("Max cell-size gap vs legacy: ", max(abs(cmp$n - cmp$n_new)),
          " deeds; max rate gap: ", signif(max(abs(cmp$sold24 - cmp$sold24_new)), 3))
  stopifnot(nrow(cmp) == 4L, all(abs(cmp$n - cmp$n_new) <= 5L),
            all(abs(cmp$sold24 - cmp$sold24_new) < 1e-5))
  message("main_proxy legacy cohorts reproduce prop19_raw_2x2_fam.csv up to the dedupe difference")
} else if (is_dev) {
  message("Legacy 2x2 reproduction skipped (dev panel).")
}

message("Finished 18_bygroup_resale at ", Sys.time())
