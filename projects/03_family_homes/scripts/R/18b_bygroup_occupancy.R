# ============================================================
# 18b: Occupant recipients after the reform, by deed group (Table 4 column (6))
# Name   : Saani Rawat
# Created: 11/11/2026
# Purpose: The model predicts that after the reform, transfers to heirs who do
#          not move in are deterred more (only an occupant heir keeps the low
#          assessment), so the share of family recipients who live in the home
#          should rise in California after the reform relative to other states.
#          Measure: share of deeds with mail_eq_situs = 1 (recipient mailing
#          address equals the property address), among deeds with the flag
#          non-missing. Descriptive means; no regression.
#          Also the same difference of differences with the old
#          absentee_buyer column (main_proxy), set against Table 4 column (6).
# Inputs : events.parquet (see `panel_file` switch), 00_groups.R,
#          prop19_absentee_did.csv (existing column (6) coefficient)
# Outputs: _outputs/tables/bygroup_occupancy_dd.csv    (one row per group x measure)
#          _outputs/tables/bygroup_occupancy_cells.csv (region x period cells, n, fill rate)
#          (dev runs write into _outputs/tables/dev/ instead)
# Log    : 11/11/2026: created.
#
# Population: deeds of the group with corp_buyer = 0, 50 states + DC (no
#   territories), sale_year in 2017-2019 (pre), 2021, 2022-2023 (post). Deeds
#   are not deduplicated here (counts are deeds), except the legacy-cohort
#   absentee comparison, which collapses identical cohort rows exactly as
#   04_prop19.R does. DD = (CA post - CA pre) - (other post - other pre), in
#   share points; the 2021 DD uses the same pre-period.
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

out_tables <- if (is_dev) path(tables_out_dir, "dev") else tables_out_dir
log_dir    <- if (is_dev) path(logs_dir, "dev") else logs_dir
dir_create(out_tables)
dir_create(log_dir)

# memory capped at ~20% of the 63 GB machine (another pipeline runs at the same time)
duck_mem     <- "12GB"
duck_threads <- 8L
spill_dir    <- "C:/Temp/wpB_duckdb"
dir_create(spill_dir)

territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# groups for the mail_eq_situs measure
occ_groups <- c(bygroup_main, "main_noaf_trust_heir", "main_noaf", "lifetime_pc", "trust_heir_chain",
                "market_sale", "spouse_ca")
stopifnot(all(occ_groups %in% bygroup_defs$name))

# legacy absentee design (04_prop19.R l.387-401, 3c): cohort months and group
legacy_pre  <- c(201701L, 201812L)
legacy_post <- c(202107L, 202206L)

log_file <- path(log_dir, "18b_bygroup_occupancy.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 18b_bygroup_occupancy at ", Sys.time(), " | panel: ", panel_file,
        if (is_dev) "  ** DEV PANEL: numbers are not results **" else "")


# ============================================================
# 2. Counts by group, region and period (duckdb) ----
# ============================================================

stopifnot(file_exists(panel_path))
con <- open_corelogic_duckdb(memory_limit = duck_mem, threads = duck_threads)
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(spill_dir)}'"))

ev <- glue("read_parquet('{sql_quote_path(panel_path)}')")
terr_sql <- paste0("('", paste(territories, collapse = "','"), "')")

# one scan: for every group, deeds (n), deeds with the flag non-missing (n_obs)
# and the sum of the flag, plus absentee_buyer counts for main_proxy
agg_sel <- function(groups, col, tag) {
  vapply(groups, function(g) {
    f <- bygroup_filter(g)
    glue("COUNT(*) FILTER (WHERE {f}) AS n_{tag}_{g}, ",
         "COUNT({col}) FILTER (WHERE {f}) AS nobs_{tag}_{g}, ",
         "SUM({col}) FILTER (WHERE {f}) AS sum_{tag}_{g}")
  }, character(1))
}
sel <- c(agg_sel(occ_groups, "mail_eq_situs", "occ"),
         agg_sel("main_proxy", "absentee_buyer", "abs"))

cells_wide <- as_tibble(dbGetQuery(con, glue("
  SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS region,
         CASE WHEN sale_year BETWEEN 2017 AND 2019 THEN 'pre_2017_19'
              WHEN sale_year = 2021 THEN 'y2021' ELSE 'post_2022_23' END AS period,
         {paste(sel, collapse = ',\n         ')}
  FROM {ev}
  WHERE corp_buyer = 0 AND state NOT IN {terr_sql}
    AND sale_year IN (2017, 2018, 2019, 2021, 2022, 2023)
  GROUP BY ALL")))

cells <- cells_wide %>%
  pivot_longer(-c(region, period), names_to = c(".value", "measure", "group"),
               names_pattern = "^(n|nobs|sum)_(occ|abs)_(.+)$") %>%
  mutate(measure = if_else(measure == "occ", "mail_eq_situs", "absentee_buyer"),
         share = if_else(nobs > 0, sum / nobs, NA_real_),
         fill_rate = nobs / n) %>%
  rename(n_obs = nobs, n_flag = sum) %>%
  filter(!(group == "spouse_ca" & region == "other"), n > 0 | region == "CA") %>%
  mutate(period = factor(period, levels = c("pre_2017_19", "y2021", "post_2022_23")))
write_csv_strict(cells %>% arrange(measure, group, region, period),
                 path(out_tables, "bygroup_occupancy_cells.csv"))

message("Cells (n deeds, n with flag, fill rate, share):")
cells %>% arrange(measure, group, region, period) %>%
  mutate(across(c(share, fill_rate), ~ round(.x, 4))) %>%
  as.data.frame() %>% print(row.names = FALSE)


# ============================================================
# 3. Difference of differences ----
# ============================================================

dd_tab <- cells %>%
  mutate(cell = paste(region, period, sep = "_")) %>%
  select(group, measure, cell, n, n_obs, fill_rate, share) %>%
  pivot_wider(names_from = cell, values_from = c(n, n_obs, fill_rate, share)) %>%
  mutate(
    diff_ca_post = share_CA_post_2022_23 - share_CA_pre_2017_19,
    diff_other_post = share_other_post_2022_23 - share_other_pre_2017_19,
    dd_post = diff_ca_post - diff_other_post,
    diff_ca_2021 = share_CA_y2021 - share_CA_pre_2017_19,
    diff_other_2021 = share_other_y2021 - share_other_pre_2017_19,
    dd_2021 = diff_ca_2021 - diff_other_2021
  ) %>%
  arrange(measure, factor(group, levels = c(occ_groups, "main_proxy")))

# ---- legacy absentee comparison: Table 4 column (6) cohorts, main_proxy ----
# collapsed identical rows, valid dates, corp_buyer = 0 (as in 04_prop19.R)
leg <- as_tibble(dbGetQuery(con, glue("
  WITH c AS (
    SELECT DISTINCT clip, state, ym, absentee_buyer,
           {event_date_sql('sale_raw')} AS sale_date
    FROM {ev}
    WHERE corp_buyer = 0 AND state NOT IN {terr_sql}
      AND coalesce({bygroup_filter('main_proxy')}, FALSE)
      AND (ym BETWEEN {legacy_pre[1]} AND {legacy_pre[2]}
           OR ym BETWEEN {legacy_post[1]} AND {legacy_post[2]})
  )
  SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS region,
         CASE WHEN ym >= {legacy_post[1]} THEN 'post' ELSE 'pre' END AS period,
         COUNT(*) AS n, COUNT(absentee_buyer) AS n_obs, SUM(absentee_buyer) AS n_abs
  FROM c WHERE sale_date IS NOT NULL
  GROUP BY ALL"))) %>%
  mutate(share = n_abs / n_obs)
print(as.data.frame(leg))
lg <- function(r, p) leg$share[leg$region == r & leg$period == p]
legacy_dd <- (lg("CA", "post") - lg("CA", "pre")) - (lg("other", "post") - lg("other", "pre"))

abs_file <- path(tables_out_dir, "prop19_absentee_did.csv")
t4_col6 <- if (file_exists(abs_file)) readr::read_csv(abs_file, show_col_types = FALSE) else NULL
message("Table 4 column (6) as saved (regression, state + cohort-month effects, weights = cells with flag):")
if (!is.null(t4_col6)) print(as.data.frame(t4_col6))
message(sprintf("Descriptive DD, absentee_buyer, main_proxy, legacy cohorts (2017-18 vs 2021H2-2022H1): %.4f",
                legacy_dd))
message(sprintf("Descriptive DD, absentee_buyer, main_proxy, 2017-19 vs 2022-23 (deeds): %.4f",
                dd_tab$dd_post[dd_tab$measure == "absentee_buyer" & dd_tab$group == "main_proxy"]))

dd_tab <- dd_tab %>%
  mutate(legacy_cohort_dd_absentee = if_else(measure == "absentee_buyer" & group == "main_proxy",
                                             legacy_dd, NA_real_),
         table4_col6_saved = if_else(measure == "absentee_buyer" & group == "main_proxy",
                                     if (!is.null(t4_col6)) t4_col6$coef[1] else NA_real_,
                                     NA_real_))
write_csv_strict(dd_tab, path(out_tables, "bygroup_occupancy_dd.csv"))

message("Difference of differences in the occupant share (post 2022-23 minus pre 2017-19):")
dd_tab %>%
  select(group, measure, share_CA_pre_2017_19, share_CA_post_2022_23, share_other_pre_2017_19,
         share_other_post_2022_23, dd_post, dd_2021, fill_rate_CA_pre_2017_19,
         fill_rate_CA_post_2022_23, fill_rate_other_pre_2017_19, fill_rate_other_post_2022_23) %>%
  mutate(across(where(is.numeric), ~ round(.x, 4))) %>%
  as.data.frame() %>% print(row.names = FALSE)

message("Finished 18b_bygroup_occupancy at ", Sys.time())
