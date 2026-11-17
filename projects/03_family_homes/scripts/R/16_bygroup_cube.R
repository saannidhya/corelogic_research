# ============================================================
# 16: By-group re-runs, step 1 -- count cube, group counts, item 1, daily series
# Author: Saani Rawat
# Purpose: One scan of the v2 panel builds a state-month count cube. The group
#          list (00_groups.R) is then evaluated on the cube, so 17 never touches
#          the 148M-row panel. Also writes:
#          Item 1: every California deed class before and after 2021, with total.
#          Item 2 (part): daily counts by sale_raw, 1 Jan - 15 Apr 2021, per group
#                  (deadline 15 Feb; 16 Feb onward is under the new rules), and,
#                  if the panel has rec_raw, the share of Mar-Apr 2021 recordings
#                  signed on or before 15 Feb.
# Inputs:  events.parquet (full) or events_dev.parquet (VT, RI, CA)
# Outputs: data/derived/03_family_homes/bygroup/cube_state_month[_dev].parquet
#          _outputs/tables/[dev/]bygroup_state_month_counts.csv
#          _outputs/tables/[dev/]bygroup_ca_class_before_after.csv (+ .tex)
#          _outputs/tables/[dev/]bygroup_daily_20210101_20210415.csv
#          _outputs/tables/[dev/]bygroup_rec_share_signed_by_deadline.csv (if rec_raw)
#          figures: F_bygroup_ca_classes_index.png, F_bygroup_daily_rush.png
# Run:     Dev (default):  Rscript projects/03_family_homes/scripts/R/16_bygroup_cube.R
#          Full:           BYGROUP_FULL=1 FAM_EXHIBIT_DIR=<dir> Rscript .../16_bygroup_cube.R
# Log:     11/11/2026: created (Phase 3, WP-A).
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(here::here("projects/03_family_homes/scripts/R/00_groups.R"))
source(here::here("projects/03_family_homes/scripts/R/00_group_helpers.R"))

set.seed(20261111)

# ---- panel switch (one place): dev file now; full panel with BYGROUP_FULL=1 ----
bygroup_full  <- identical(Sys.getenv("BYGROUP_FULL", "0"), "1")
panel_path    <- if (bygroup_full) events_panel_path else path(data_dir, "events_dev.parquet")
dev_tag       <- if (bygroup_full) "" else "_dev"
if (bygroup_full) {
  if (!nzchar(Sys.getenv("FAM_EXHIBIT_DIR", ""))) {
    stop("Full run: set FAM_EXHIBIT_DIR so exhibits do not land in the manuscript")
  }
  bg_tab_dir <- tables_out_dir
} else {
  # Dev numbers must never be mistaken for results: everything goes under dev/
  bg_tab_dir  <- path(tables_out_dir, "dev")
  tables_dir  <- path(out_dir, "dev", "exhibits", "tables")
  figures_dir <- path(out_dir, "dev", "exhibits", "figures")
}
bg_cube_dir <- path(data_dir, "bygroup")
for (d in c(bg_tab_dir, tables_dir, figures_dir, bg_cube_dir)) dir_create(d)
stopifnot(file_exists(panel_path))

log_file <- path(logs_dir, paste0("16_bygroup_cube", dev_tag, ".log"))
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 16_bygroup_cube at ", Sys.time(), " on ", panel_path)

territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

con <- open_corelogic_duckdb(memory_limit = if (bygroup_full) "16GB" else "12GB")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
# Spill to a local disk outside the OneDrive tree (WP-B shares the machine)
spill_dir <- Sys.getenv("BYGROUP_SPILL", "C:/duckdb_spill_wpA")
dir_create(spill_dir)
invisible(dbExecute(con, glue("PRAGMA temp_directory='{spill_dir}'")))
src <- glue("read_parquet('{sql_quote_path(panel_path)}')")

need <- c("clip", "state", "sale_raw", "sale_year", "ym", "class", "doc_type",
          "fam_relation", "fam_relation_nm", "fam_trigger", "fam_vehicle",
          "trust_fam_lo", "trust_fam_hi", "trust_chain", "trust_chain_gap_days")
have <- dbGetQuery(con, glue("SELECT column_name FROM (DESCRIBE SELECT * FROM {src})"))$column_name
stopifnot(all(need %in% have))

# ============================================================
# 1. Count cube ----
# ============================================================
# doc_type is collapsed to 'AF' (affidavit of death, AF/AJ) or NULL, which is
# all the group filters use (coalesce(doc_type,'') IN ('AF','AJ')).
cube_path <- path(bg_cube_dir, paste0("cube_state_month", dev_tag, ".parquet"))
invisible(dbExecute(con, glue("
  COPY (
    SELECT state, ym, sale_year, class, fam_relation, fam_relation_nm, fam_trigger,
           fam_vehicle, trust_fam_lo, trust_fam_hi, trust_chain,
           -- gap days collapsed to 0 (<= 3652, the 10-year look-back) or 9999: all the filters use
           CASE WHEN trust_chain_gap_days IS NULL THEN NULL
                WHEN trust_chain_gap_days <= 3652 THEN 0 ELSE 9999 END AS trust_chain_gap_days,
           CASE WHEN coalesce(doc_type, '') IN ('AF', 'AJ') THEN 'AF' END AS doc_type,
           COUNT(*) AS n
    FROM {src}
    WHERE sale_year BETWEEN 2016 AND 2024
    GROUP BY ALL
  ) TO '{sql_quote_path(cube_path)}' (FORMAT PARQUET)")))
cube_src <- glue("read_parquet('{sql_quote_path(cube_path)}')")
cube_n <- dbGetQuery(con, glue("SELECT COUNT(*) AS cells, SUM(n) AS deeds FROM {cube_src}"))
message("Cube: ", format(cube_n$cells, big.mark = ","), " cells, ",
        format(cube_n$deeds, big.mark = ","), " deeds")
stopifnot(cube_n$cells > 100, cube_n$deeds > 1e5)

# Group counts per state-month (long), territories dropped
counts_wide <- bygroup_count_query(con, cube_src, weight = "n",
                                   where = "sale_year BETWEEN 2016 AND 2024") %>%
  filter(!state %in% territories)
counts <- counts_wide %>%
  pivot_longer(starts_with("g_"), names_to = "group", names_prefix = "g_",
               values_to = "n") %>%
  mutate(n = as.numeric(n))
assert_rows(counts, 1000, "bygroup_state_month_counts")
write_csv_strict(counts, path(bg_tab_dir, "bygroup_state_month_counts.csv"))

# Groups with no rows at all would silently pass downstream
zero_groups <- counts %>% group_by(group) %>% summarise(n = sum(n), .groups = "drop") %>%
  filter(n == 0)
if (nrow(zero_groups)) message("WARNING groups with zero rows: ",
                               paste(zero_groups$group, collapse = ", "))

# ============================================================
# 2. Item 1: every California deed class, before and after 2021 ----
# ============================================================
# Cells form a partition of California's deeds, so they sum to the total.
ca_cells <- dbGetQuery(con, glue("
  SELECT sale_year,
         CASE WHEN fam_vehicle IS NULL THEN class
              WHEN fam_vehicle = 'direct' THEN 'family direct / ' || coalesce(fam_relation, 'na')
              ELSE 'family ' || fam_vehicle END AS cell,
         SUM(n) AS n
  FROM {cube_src}
  WHERE state = 'CA' AND sale_year BETWEEN 2017 AND 2024
  GROUP BY 1, 2")) %>%
  mutate(n = as.numeric(n))
ca_total <- ca_cells %>% group_by(sale_year) %>%
  summarise(n = sum(n), .groups = "drop") %>% mutate(cell = "TOTAL, all California deeds")

# The three main-proxy variants from the group counts (not part of the partition)
ca_variants <- counts %>%
  filter(state == "CA", group %in% c("main_proxy", "main_noaf", "main_allaf"),
         between(sale_year, 2017L, 2024L)) %>%
  group_by(sale_year, cell = paste("VARIANT", group)) %>%
  summarise(n = sum(n), .groups = "drop")

before_after <- bind_rows(ca_cells, ca_total, ca_variants) %>%
  pivot_wider(names_from = sale_year, values_from = n, names_prefix = "y",
              values_fill = 0) %>%
  mutate(mean_2017_19 = (y2017 + y2018 + y2019) / 3,
         mean_2022_23 = (y2022 + y2023) / 2,
         log_chg_vs_2017_19 = log(mean_2022_23 / mean_2017_19),
         log_chg_vs_2019 = log(mean_2022_23 / y2019),
         share_of_total_2019 = y2019 / y2019[cell == "TOTAL, all California deeds"],
         break_flag = if_else(grepl("death_affidavit_trust", cell) |
                                grepl("main_proxy|main_allaf", cell),
                              "2018 vendor break in 2017-19 base", "")) %>%
  arrange(desc(grepl("^TOTAL", cell)), grepl("^VARIANT", cell), desc(y2019))
# Partition check: cells sum to the total in every year
part_sum <- ca_cells %>% group_by(sale_year) %>% summarise(s = sum(n), .groups = "drop")
stopifnot(isTRUE(all.equal(part_sum$s, ca_total$n)))
write_csv_strict(before_after, path(bg_tab_dir, "bygroup_ca_class_before_after.csv"))
tex <- knitr::kable(
  before_after %>% transmute(Class = cell, `2019` = y2019, `Mean 2017-19` = mean_2017_19,
                             `Mean 2022-23` = mean_2022_23,
                             `Log chg vs 2017-19` = log_chg_vs_2017_19,
                             `Log chg vs 2019` = log_chg_vs_2019),
  format = "latex", booktabs = TRUE, digits = c(0, 0, 0, 0, 3, 3), format.args = list(big.mark = ","),
  caption = "California deeds by class, before and after 2021")
writeLines(tex, path(tables_dir, "T_bygroup_ca_class_before_after.tex"))
print(before_after %>% select(cell, y2019, mean_2017_19, mean_2022_23, log_chg_vs_2017_19), n = 40)

# Figure: monthly California series indexed to the 2019 monthly mean
idx_groups <- bygroup_defs %>%
  filter(scope == "cross", name %in% c(
    "market_sale", "main_noaf", "main_distinct", "lifetime_pc", "trust_to_other", "into_trust_others",
    "owner_retained", "trust_to_self", "death_direct_noaf", "death_affidavit_all")) %>%
  select(group = name, label, role)
idx <- counts %>% filter(state == "CA", group %in% idx_groups$group, ym >= 201801, ym <= 202406) %>%
  group_by(group) %>%
  mutate(index = 100 * n / mean(n[sale_year == 2019])) %>% ungroup() %>%
  left_join(idx_groups, by = "group") %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)),
         panel = case_when(grepl("exposed", role) ~ "Exposed", grepl("placebo", role) ~ "Refinance placebo",
                           TRUE ~ "Comparison"))
p_idx <- ggplot(idx, aes(date, index, color = label)) +
  geom_line(linewidth = 0.5) +
  prop19_event_layers(label_y = Inf, show_labels = FALSE) +
  facet_wrap(~panel, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "California deeds, index (2019 monthly mean = 100)", color = NULL) +
  theme(legend.position = "bottom")
ggsave(path(figures_dir, "F_bygroup_ca_classes_index.png"), p_idx, width = 9, height = 8,
       dpi = 300, bg = "transparent")

# ============================================================
# 3. Item 2 (part): daily counts, 1 Jan - 15 Apr 2021 ----
# ============================================================
daily_wide <- bygroup_count_query(
  con, src, where = "sale_raw BETWEEN 20210101 AND 20210415",
  by = c("state", "sale_raw"))
daily <- daily_wide %>%
  filter(!state %in% territories) %>%
  pivot_longer(starts_with("g_"), names_to = "group", names_prefix = "g_", values_to = "n") %>%
  mutate(state_class = if_else(state == "CA", "CA", "other"),
         date = as.Date(as.character(sale_raw), "%Y%m%d")) %>%
  group_by(group, state_class, sale_raw, date) %>%
  summarise(n = sum(as.numeric(n)), .groups = "drop") %>%
  arrange(group, state_class, sale_raw)
write_csv_strict(daily, path(bg_tab_dir, "bygroup_daily_20210101_20210415.csv"))
# Note: the deadline is 15 Feb 2021; 16 Feb onward is under the new rules.
deadline_date <- as.Date("2021-02-15")
daily_plot <- daily %>% filter(state_class == "CA", !is.na(date),
                               group %in% c("main_noaf", "lifetime_pc", "trust_to_other",
                                            "into_trust_others", "owner_retained",
                                            "death_direct_noaf", "spouse_ca"))
p_daily <- ggplot(daily_plot, aes(date, n)) +
  geom_line(linewidth = 0.4) +
  geom_vline(xintercept = deadline_date, linetype = "dashed", color = "grey35") +
  facet_wrap(~group, scales = "free_y", ncol = 2) +
  labs(x = "Signing date (sale_derived_date)", y = "California deeds per day")
ggsave(path(figures_dir, "F_bygroup_daily_rush.png"), p_daily, width = 9, height = 8,
       dpi = 300, bg = "transparent")

# Recording-date check, only when the panel carries rec_raw
if ("rec_raw" %in% have) {
  rec_wide <- bygroup_count_query(
    con, src, where = "rec_raw BETWEEN 20210301 AND 20210430", by = c("state"))
  signed_wide <- bygroup_count_query(
    con, src, where = "rec_raw BETWEEN 20210301 AND 20210430 AND sale_raw <= 20210215",
    by = c("state"))
  to_long <- function(w, nm) w %>% filter(!state %in% territories) %>%
    pivot_longer(starts_with("g_"), names_to = "group", names_prefix = "g_", values_to = nm) %>%
    mutate(state_class = if_else(state == "CA", "CA", "other")) %>%
    group_by(group, state_class) %>% summarise(across(all_of(nm), ~ sum(as.numeric(.x))), .groups = "drop")
  rec_share <- full_join(to_long(rec_wide, "recorded_mar_apr"),
                         to_long(signed_wide, "signed_by_15feb"),
                         by = c("group", "state_class")) %>%
    mutate(share_signed_by_deadline = signed_by_15feb / recorded_mar_apr)
  write_csv_strict(rec_share, path(bg_tab_dir, "bygroup_rec_share_signed_by_deadline.csv"))
} else {
  message("rec_raw not in this panel: recording-date share skipped")
}

message("Finished 16_bygroup_cube at ", Sys.time())
