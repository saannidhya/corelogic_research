# ============================================================
# 19: Deeds to parcels, by deed group (Phase 3, item 7)
# Name   : Saani Rawat
# Created: 11/11/2026
# Purpose: Deeds per distinct parcel (clip) by group and year, California
#          against the other states, extending the national count in
#          07_referee_checks.R (1.19 deeds per family parcel, main proxy,
#          2007-2023). Restates the rush and post-reform windows in parcels:
#          distinct parcels with at least one deed of the group in the window.
# Inputs : events.parquet (see `panel_file` switch), 00_groups.R,
#          00_group_helpers.R, ref_reconciliation.csv (reproduction check)
# Outputs: _outputs/tables/bygroup_deeds_per_parcel.csv       (group x region x year/period)
#          _outputs/tables/bygroup_parcels_windows.csv        (rush / post windows in parcels)
#          (dev runs write into _outputs/tables/dev/ instead)
# Log    : 11/11/2026: created.
#
# Regions: CA; other = the 50 other states + DC (the donor pool); all = every
#   row in the panel including territories, which is what 07_referee_checks.R
#   counts. The reproduction check uses "all".
# Stops with an error (full panel) if the main-proxy national 2007-2023 deed
#   and parcel counts differ from the saved 07 values.
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

territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")
years_used  <- 2007L:2023L          # same span as 07_referee_checks.R (reconciliation)

# multi-year and multi-month periods (SQL condition on the panel)
base_rush_months <- c(201901L, 201902L, 201906L:201912L)   # same calendar months of 2019
periods <- list(
  span_2007_2023   = "sale_year BETWEEN 2007 AND 2023",
  pre_2017_2019    = "sale_year BETWEEN 2017 AND 2019",
  rush             = "ym BETWEEN 202006 AND 202102",
  jan_feb_2021     = "ym BETWEEN 202101 AND 202102",
  base_rush_2019   = paste0("ym IN (", paste(base_rush_months, collapse = ","), ")"),
  base_jf_2019     = "ym IN (201901, 201902)",
  post_year        = "ym BETWEEN 202103 AND 202202"       # first year after the deadline
)
period_months <- c(rush = 9L, jan_feb_2021 = 2L, base_rush_2019 = 9L, base_jf_2019 = 2L,
                   post_year = 12L)

groups_run <- bygroup_defs %>% filter(name != "market_sale")   # comparator not needed here

log_file <- path(log_dir, "19_bygroup_parcels.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 19_bygroup_parcels at ", Sys.time(), " | panel: ", panel_file,
        if (is_dev) "  ** DEV PANEL: numbers are not results **" else "")


# ============================================================
# 2. Counting function ----
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
terr_sql <- paste0("('", paste(territories, collapse = "','"), "')")

# one scan: deeds D and distinct parcels K for every group, by region
# (CA / other / territory; grand total = "all") and optionally by year
count_groups <- function(where, by_year = FALSE) {
  d_sel <- vapply(groups_run$name, function(g) {
    f <- bygroup_filter(g)
    glue("COUNT(*) FILTER (WHERE {f}) AS d_{g}, ",
         "COUNT(DISTINCT clip) FILTER (WHERE {f}) AS k_{g}")
  }, character(1))
  any_group <- paste(vapply(groups_run$name, bygroup_filter, character(1)), collapse = " OR ")
  yr   <- if (by_year) "sale_year, " else ""
  sets <- if (by_year) "((region, sale_year), (sale_year))" else "((region), ())"
  q <- glue("
    SELECT {yr}CASE WHEN state = 'CA' THEN 'CA'
                    WHEN state IN {terr_sql} THEN 'territory' ELSE 'other' END AS region,
           GROUPING(region) AS is_total,
           {paste(d_sel, collapse = ',\n           ')}
    FROM {ev}
    WHERE ({where}) AND ({any_group})
    GROUP BY GROUPING SETS {sets}")
  # region is an alias: duckdb needs it spelled out inside GROUPING SETS
  q <- gsub("GROUPING(region)", "GROUPING(CASE WHEN state = 'CA' THEN 'CA' WHEN state IN {terr} THEN 'territory' ELSE 'other' END)", q, fixed = TRUE)
  q <- gsub("{terr}", terr_sql, q, fixed = TRUE)
  res <- as_tibble(dbGetQuery(con, q)) %>%
    mutate(region = if_else(is_total == 1L, "all", region)) %>%
    select(-is_total)
  res %>%
    pivot_longer(matches("^[dk]_"), names_to = c("stat", "group"),
                 names_pattern = "^([dk])_(.+)$", values_to = "v") %>%
    pivot_wider(names_from = stat, values_from = v) %>%
    rename(deeds = d, parcels = k) %>%
    mutate(deeds_per_parcel = if_else(parcels > 0, deeds / parcels, NA_real_))
}


# ============================================================
# 3. Counts by year and by period ----
# ============================================================

# by year: groups x region x year
message("Counting by year ...")
by_year <- count_groups(glue("sale_year BETWEEN {min(years_used)} AND {max(years_used)}"),
                        by_year = TRUE) %>%
  mutate(period = paste0("y", sale_year)) %>% select(-sale_year)

# by period: groups x region
by_period <- imap(periods, function(w, nm) {
  message("Counting period ", nm, " ...")
  count_groups(w) %>% mutate(period = nm)
}) %>% bind_rows()

deeds_per_parcel <- bind_rows(by_year, by_period) %>%
  filter(region != "territory") %>%
  mutate(group = factor(group, levels = groups_run$name),
         region = factor(region, levels = c("CA", "other", "all"))) %>%
  arrange(group, region, period) %>%
  mutate(group = as.character(group), region = as.character(region))
# chain-verified trust groups are complete only from 2017 (10-year look-back
# starts with the 2007 panel): flag every period that reaches before 2017
deeds_per_parcel <- deeds_per_parcel %>%
  mutate(lookback_incomplete = group %in% bygroup_chain_groups &
           (period == "span_2007_2023" |
              (grepl("^y[0-9]{4}$", period) & suppressWarnings(as.integer(sub("y", "", period))) < 2017L)))
write_csv_strict(deeds_per_parcel, path(out_tables, "bygroup_deeds_per_parcel.csv"))

# printed summary: California deeds per parcel, pre-period against 2022-2023
message("California deeds per parcel by group (2019, 2022, 2023, full span):")
deeds_per_parcel %>%
  filter(region == "CA", period %in% c("y2019", "y2022", "y2023", "span_2007_2023")) %>%
  select(group, period, deeds, parcels, deeds_per_parcel) %>%
  mutate(deeds_per_parcel = round(deeds_per_parcel, 3)) %>%
  pivot_wider(names_from = period, values_from = c(deeds, parcels, deeds_per_parcel)) %>%
  select(group, ends_with("y2019"), ends_with("y2022"), ends_with("span_2007_2023")) %>%
  as.data.frame() %>% print()


# ============================================================
# 4. Reproduction of the national 1.19 ----
# ============================================================

nat <- deeds_per_parcel %>%
  filter(group == "main_proxy", region == "all", period == "span_2007_2023")
message(sprintf("main_proxy, all states, 2007-2023: %s deeds / %s parcels = %.10f",
                format(nat$deeds, big.mark = ","), format(nat$parcels, big.mark = ","),
                nat$deeds_per_parcel))
legacy_file <- path(tables_out_dir, "ref_reconciliation.csv")
stopifnot(file_exists(legacy_file))
leg <- readr::read_csv(legacy_file, show_col_types = FALSE)
message(sprintf("07 reference: %s deeds / %s parcels = %.10f",
                format(leg$fam_broad_0723, big.mark = ","),
                format(leg$fam_clips_0723, big.mark = ","), leg$deeds_per_fam_clip))
if (is_dev) {
  message("Reproduction check not enforced on the dev panel (VT, RI, CA only).")
} else {
  if (nat$deeds != leg$fam_broad_0723 || nat$parcels != leg$fam_clips_0723 ||
      abs(nat$deeds_per_parcel - leg$deeds_per_fam_clip) > 1e-9) {
    stop("main_proxy national deeds per parcel does not reproduce ref_reconciliation.csv")
  }
  message("main_proxy reproduces the existing 1.19 (exact integer match on both counts)")
}


# ============================================================
# 5. Rush and post-reform windows restated in parcels ----
# ============================================================

# Base = the same calendar months of 2019 (the counterfactual base used for the
# rush); post windows use calendar 2019 (annual). Ratios compare monthly or
# annual averages; the relative ratio divides California by the other states.
p <- function(group_nm, reg, per) {
  deeds_per_parcel %>% filter(group == group_nm, region == reg, period == per)
}
win_defs <- tribble(
  ~window,         ~win_period,     ~base_period,     ~win_months, ~base_months,
  "rush",          "rush",          "base_rush_2019", 9L,          9L,
  "jan_feb_2021",  "jan_feb_2021",  "base_jf_2019",   2L,          2L,
  "post_year",     "post_year",     "y2019",          12L,         12L
)

win_long <- crossing(group = groups_run$name, reg = c("CA", "other"), win_defs) %>%
  left_join(deeds_per_parcel %>% select(group, reg = region, win_period = period,
                                        d_win = deeds, k_win = parcels),
            by = c("group", "reg", "win_period")) %>%
  left_join(deeds_per_parcel %>% select(group, reg = region, base_period = period,
                                        d_base = deeds, k_base = parcels),
            by = c("group", "reg", "base_period"))

# post_2022_2023: average of the two yearly counts, against calendar 2019
yr_avg <- deeds_per_parcel %>%
  filter(region %in% c("CA", "other"), period %in% c("y2022", "y2023")) %>%
  group_by(group, reg = region) %>%
  summarise(d_win = mean(deeds), k_win = mean(parcels), .groups = "drop") %>%
  left_join(deeds_per_parcel %>% filter(period == "y2019") %>%
              select(group, reg = region, d_base = deeds, k_base = parcels),
            by = c("group", "reg")) %>%
  mutate(window = "post_2022_2023", win_period = "y2022_y2023_mean", base_period = "y2019",
         win_months = 12L, base_months = 12L)
win_long <- bind_rows(win_long, yr_avg)

parcels_windows <- win_long %>%
  mutate(ratio_deeds = (d_win / win_months) / (d_base / base_months),
         ratio_parcels = (k_win / win_months) / (k_base / base_months)) %>%
  select(group, window, reg, d_win, k_win, d_base, k_base, ratio_deeds, ratio_parcels) %>%
  pivot_wider(names_from = reg, values_from = c(d_win, k_win, d_base, k_base,
                                                ratio_deeds, ratio_parcels)) %>%
  mutate(rel_ratio_deeds = ratio_deeds_CA / ratio_deeds_other,
         rel_ratio_parcels = ratio_parcels_CA / ratio_parcels_other,
         ca_deeds_per_parcel_win = d_win_CA / k_win_CA,
         ca_deeds_per_parcel_base = d_base_CA / k_base_CA) %>%
  arrange(factor(group, levels = groups_run$name),
          factor(window, levels = c("rush", "jan_feb_2021", "post_year", "post_2022_2023")))
write_csv_strict(parcels_windows, path(out_tables, "bygroup_parcels_windows.csv"))

message("Windows in parcels (California against other states; ratios vs same months of 2019):")
parcels_windows %>%
  filter(group %in% c("main_proxy", "lifetime_pc", "main_noaf", "death_affidavit_all")) %>%
  select(group, window, ratio_deeds_CA, ratio_parcels_CA, rel_ratio_deeds, rel_ratio_parcels,
         ca_deeds_per_parcel_win, ca_deeds_per_parcel_base) %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  as.data.frame() %>% print()

message("Finished 19_bygroup_parcels at ", Sys.time())
