# ============================================================
# 24: Chain-verified trust-to-heir deeds split by a prior death document on the parcel
# Author: Saani Rawat
# Purpose: JUE review item B5 (methods referee, Concern 5). The paper reads the trust-to-heir
#          rush as deeds from living parents and the later deeds as mostly following a death.
#          Test: split chain-verified trust-to-heir deeds (trust_heir_chain, 00_groups.R) by
#          whether a death document -- document group estate_probate_death, affidavits of death
#          included -- is recorded on the same parcel (clip) on or before the trust-to-heir
#          deed's recording date, within 24 months (also 12 and 36). Same-day recordings count
#          as prior (an affidavit of a trustee's death is often recorded with the trustee's
#          deed); the heir deed itself never counts. Dates: recording date (rec_raw), falling
#          back to the signing date when the recording date is missing.
#          For each part: January-February 2021 ratio to the paper's counterfactual (other
#          states, 2019; r1_cf = 17's bg_cf), the deadline ratio per business day (17-28 over
#          1-16 February), the triple difference (2022-23 against 2017-19, net of market sales,
#          state-clustered SE; 17 section D) and the 2022-23 event-study mean (17 section E).
#          2018 check: the share with a prior death document by year 2015-2023, and death
#          documents by year, to see whether the 2018 affidavit coding break moves the split.
#          The split is a parcel-level join, so its two parts cannot be a column filter in
#          00_groups.R (16 and 17 evaluate every group on panel columns); they are built here
#          from the trust_heir_chain filter and checked against 16's cube.
# Inputs:  events.parquet (heir deeds 2015-2024; death documents on their parcels);
#          _outputs/tables/bygroup_state_month_counts.csv (16: market sales, reproduction);
#          bygroup_ddd.csv, bygroup_event_study_monthly.csv, bygroup_rush_table.csv (17: checks).
# Outputs: _outputs/tables/r1_heir_split_state_month.csv, r1_heir_split_daily.csv,
#          r1_heir_split_share_by_year.csv, r1_death_docs_by_year.csv,
#          r1_heir_split_share_by_period.csv, r1_heir_split_results.csv;
#          _outputs/figures/FR1_heir_split_share.png, FR1_heir_split_event_study.png;
#          RONE macros (script "24") in _outputs/r1_numbers.tex.
# Run:     Rscript projects/03_family_homes/scripts/R/24_heir_death_split.R
# Log:     11/13/2026: created (R1 of plans/2026-11-12_review-analyses.md).
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_group_helpers.R"))
source(path(r_dir, "00_r1_helpers.R"))
suppressPackageStartupMessages(library(fixest))

set.seed(20261112)

log_file <- path(logs_dir, "24_heir_death_split.log")
sink(log_file, split = TRUE)
message("Starting 24_heir_death_split at ", Sys.time())
TT <- function(f) path(tables_out_dir, f)
FF <- function(f) path(figures_out_dir, f)
windows_m <- c(12L, 24L, 36L)
main_w <- 24L

# ============================================================
# 0. Heir deeds, death documents, flags (DuckDB) ----
# ============================================================
terr_sql <- paste0("'", r1_territories, "'", collapse = ",")
pq <- glue("read_parquet('{sql_quote_path(events_panel_path)}')")
con <- r1_open_duckdb()
t0 <- Sys.time()
rec_d <- event_date_sql("rec_raw"); sale_d <- event_date_sql("sale_raw")
invisible(dbExecute(con, glue("
  CREATE TEMP TABLE heirs AS
  SELECT row_number() OVER (ORDER BY clip, sale_raw, rec_raw, state) AS hid,
         clip, state, sale_raw, rec_raw, sale_year, ym, doc_type,
         coalesce({rec_d}, {sale_d}) AS h_date, ({rec_d}) IS NULL AS h_rec_missing
  FROM {pq}
  WHERE {bygroup_filter('trust_heir_chain')} AND sale_year BETWEEN 2015 AND 2024
    AND state NOT IN ({terr_sql})")))
invisible(dbExecute(con, glue("
  CREATE TEMP TABLE deaths AS
  SELECT d.clip, d.sale_raw, d.rec_raw, d.doc_type, coalesce({rec_d}, {sale_d}) AS d_date
  FROM {pq} d
  WHERE d.doc_grp = 'estate_probate_death' AND d.clip IN (SELECT DISTINCT clip FROM heirs)")))
win_case <- function(w) glue("MAX(CASE WHEN d.d_date <= h.h_date AND d.d_date >= h.h_date - INTERVAL {w} MONTH THEN 1 ELSE 0 END) AS death{w}")
invisible(dbExecute(con, glue("
  CREATE TEMP TABLE heirs_f AS
  SELECT h.*, f.* EXCLUDE (hid) FROM heirs h JOIN (
    SELECT h.hid,
           {paste(vapply(windows_m, win_case, ''), collapse = ',\n           ')},
           MAX(CASE WHEN d.d_date = h.h_date THEN 1 ELSE 0 END) AS death_sameday,
           MAX(CASE WHEN d.d_date <= h.h_date AND d.d_date >= h.h_date - INTERVAL 24 MONTH
                     AND d.doc_type IN ('AF','AJ') THEN 1 ELSE 0 END) AS death24_af,
           MAX(CASE WHEN d.d_date > h.h_date AND d.d_date <= h.h_date + INTERVAL 24 MONTH
                    THEN 1 ELSE 0 END) AS death_after24
    FROM heirs h LEFT JOIN deaths d
      ON d.clip = h.clip
     AND NOT (d.sale_raw IS NOT DISTINCT FROM h.sale_raw AND d.rec_raw IS NOT DISTINCT FROM h.rec_raw
              AND d.doc_type IS NOT DISTINCT FROM h.doc_type)    -- never the heir deed itself
    GROUP BY h.hid) f USING (hid)")))
nn <- dbGetQuery(con, "SELECT (SELECT COUNT(*) FROM heirs) AS heirs, (SELECT COUNT(*) FROM heirs_f) AS flagged,
                              (SELECT COUNT(*) FROM deaths) AS deaths,
                              (SELECT SUM(CAST(h_rec_missing AS INT)) FROM heirs) AS rec_missing")
print(nn)
stopifnot(nn$heirs > 1e4, nn$heirs == nn$flagged, nn$deaths > 1e4)
flag_cols <- c(paste0("death", windows_m), "death_sameday", "death24_af", "death_after24")
sums_sql <- paste0("SUM(", flag_cols, ") AS ", flag_cols, collapse = ", ")
sm <- tibble::as_tibble(dbGetQuery(con, glue(
  "SELECT state, ym, sale_year, COUNT(*) AS heir_all, {sums_sql} FROM heirs_f GROUP BY ALL")))
dly <- tibble::as_tibble(dbGetQuery(con, glue(
  "SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS state_class, sale_raw,
          COUNT(*) AS heir_all, {sums_sql}
   FROM heirs_f
   WHERE (sale_raw BETWEEN 20190101 AND 20190331 OR sale_raw BETWEEN 20210101 AND 20210331)
     AND sale_raw % 100 BETWEEN 1 AND 31
   GROUP BY ALL")))
share_year <- tibble::as_tibble(dbGetQuery(con, glue(
  "SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS state_class, sale_year,
          COUNT(*) AS n_heirs, {paste0('AVG(', flag_cols, ') AS share_', flag_cols, collapse = ', ')},
          AVG(CAST(h_rec_missing AS INT)) AS share_rec_missing
   FROM heirs_f WHERE sale_year BETWEEN 2015 AND 2023 GROUP BY ALL ORDER BY 1, 2")))
death_docs <- tibble::as_tibble(dbGetQuery(con, glue(
  "SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS state_class, sale_year,
          CASE WHEN coalesce(doc_type, '') IN ('AF','AJ') THEN 'affidavit' ELSE 'other death document' END AS kind,
          COUNT(*) AS n
   FROM {pq}
   WHERE doc_grp = 'estate_probate_death' AND sale_year BETWEEN 2012 AND 2023
     AND state NOT IN ({terr_sql})
   GROUP BY ALL ORDER BY 1, 2, 3")))
dbDisconnect(con, shutdown = TRUE)
message("DuckDB steps: ", round(as.numeric(difftime(Sys.time(), t0, units = "secs"))), " s")

# ---- long state-month counts on the full 51-state grid; check against 16's cube ----
cube <- read_csv(TT("bygroup_state_month_counts.csv"), show_col_types = FALSE)
states <- sort(unique(cube$state[!cube$state %in% r1_territories]))
stopifnot(length(states) == 51L)
grid <- expand_grid(state = states, ym = r1_months(201501L, 202406L)) %>% mutate(sale_year = ym %/% 100L)
split_names <- c("heir_all", as.vector(rbind(paste0("heir_death", windows_m), paste0("heir_nodeath", windows_m))))
sm_long <- sm %>% mutate(ym = as.integer(ym), sale_year = as.integer(sale_year)) %>%
  mutate(across(all_of(paste0("death", windows_m)), ~ heir_all - .x, .names = "no{.col}")) %>%
  rename_with(~ paste0("heir_", .x), c(paste0("death", windows_m), paste0("nodeath", windows_m))) %>%
  select(state, ym, all_of(split_names)) %>%
  right_join(grid, by = c("state", "ym")) %>%
  pivot_longer(all_of(split_names), names_to = "group", values_to = "n") %>%
  mutate(n = as.numeric(coalesce(n, 0L))) %>% filter(ym <= 202406L)
stopifnot(!anyNA(sm_long$sale_year))
write_csv_strict(sm_long, TT("r1_heir_split_state_month.csv"))
chk <- sm_long %>% filter(group == "heir_all", sale_year >= 2016L) %>%
  full_join(cube %>% filter(group == "trust_heir_chain", !state %in% r1_territories,
                            sale_year >= 2016L, ym <= 202406L) %>%
              select(state, ym, n_cube = n), by = c("state", "ym")) %>%
  mutate(n_cube = coalesce(n_cube, 0))
stopifnot(!anyNA(chk$n), all(chk$n == chk$n_cube))
message("Heir deeds equal 16's trust_heir_chain counts on ", nrow(chk), " state-months (2016-2024)")
# parts add up to the whole in every cell
add_chk <- sm_long %>% pivot_wider(names_from = group, values_from = n)
for (w in windows_m) stopifnot(all(add_chk[[paste0("heir_death", w)]] + add_chk[[paste0("heir_nodeath", w)]] == add_chk$heir_all))

# design data: chain look-back complete from 2017; market sales from the cube
counts <- bind_rows(sm_long %>% filter(sale_year >= 2017L),
                    cube %>% filter(group == "market_sale", !state %in% r1_territories) %>%
                      select(state, ym, sale_year, group, n)) %>%
  mutate(ym = as.integer(ym), sale_year = as.integer(sale_year))

# ============================================================
# 1. Share with a prior death document: by year and by period ----
# ============================================================
write_csv_strict(share_year, TT("r1_heir_split_share_by_year.csv"))
write_csv_strict(death_docs, TT("r1_death_docs_by_year.csv"))
print(share_year, n = 30, width = 200)
print(death_docs %>% pivot_wider(names_from = sale_year, values_from = n), width = 250)
periods <- tribble(~period, ~from, ~to,
  "jan_feb_2019", 201901L, 201902L, "2017_2019", 201701L, 201912L,
  "jun2020_feb2021", 202006L, 202102L, "jan_feb_2021", 202101L, 202102L,
  "mar_dec_2021", 202103L, 202112L, "2022_2023", 202201L, 202312L)
share_period <- periods %>% expand_grid(state_class = c("CA", "other"), w = windows_m) %>% rowwise() %>%
  mutate(n_heirs = sum(counts$n[counts$group == "heir_all" & counts$ym >= from & counts$ym <= to &
                                  (counts$state == "CA") == (state_class == "CA")]),
         n_with = sum(counts$n[counts$group == paste0("heir_death", w) & counts$ym >= from & counts$ym <= to &
                                 (counts$state == "CA") == (state_class == "CA")]),
         share_with = n_with / n_heirs) %>% ungroup()
write_csv_strict(share_period, TT("r1_heir_split_share_by_period.csv"))
print(share_period %>% filter(w == main_w), width = 200)

# ============================================================
# 2. Designs for each part ----
# ============================================================
cal <- r1_calendar("2016-12-01", "2021-12-31")
bd_month <- cal %>% group_by(ym) %>% summarise(bdays = sum(is_bday), .groups = "drop")
dl <- dly %>% mutate(day_raw = as.integer(sale_raw)) %>%
  mutate(across(all_of(paste0("death", windows_m)), ~ heir_all - .x, .names = "no{.col}")) %>%
  rename_with(~ paste0("heir_", .x), c(paste0("death", windows_m), paste0("nodeath", windows_m))) %>%
  select(state_class, day_raw, all_of(split_names)) %>%
  pivot_longer(all_of(split_names), names_to = "group", values_to = "n") %>%
  right_join(expand_grid(state_class = c("CA", "other"), group = split_names,
                         day_raw = cal$day_raw[cal$ym %in% c(201901:201903, 202101:202103)]),
             by = c("state_class", "group", "day_raw")) %>%
  mutate(n = as.numeric(coalesce(n, 0))) %>%
  left_join(cal %>% select(day_raw, date), by = "day_raw")
write_csv_strict(dl, TT("r1_heir_split_daily.csv"))
D <- function(y, m, d) as.Date(sprintf("%d-%02d-%02d", y, m, d))
deadline_ratio <- function(g, sc, y, bday = TRUE) {
  s <- dl %>% filter(group == g, state_class == sc)
  if (bday) {
    r1_per_bday(s, cal, D(y, 2, 17), D(y, 2, 28)) / r1_per_bday(s, cal, D(y, 2, 1), D(y, 2, 16))
  } else {
    mean(s$n[s$date >= D(y, 2, 17) & s$date <= D(y, 2, 28)]) / mean(s$n[s$date >= D(y, 2, 1) & s$date <= D(y, 2, 16)])
  }
}
# check: the whole group reproduces 17b's calendar-day deadline ratio
old_disc <- read_csv(TT("bygroup_suppl_deadline_discontinuity.csv"), show_col_types = FALSE) %>%
  filter(group == "trust_heir_chain", basis == "signing 17-28 over 1-16, all days (sensitivity)")
for (i in seq_len(nrow(old_disc))) {
  stopifnot(abs(deadline_ratio("heir_all", old_disc$state_class[i], old_disc$year[i], FALSE) - old_disc$ratio[i]) < 1e-9)
}

#' Triple difference (17 section D) for one part; returns one row (NA if it cannot be run)
run_ddd <- function(g, pre_years) {
  post_years <- 2022:2023; yrs <- c(pre_years, post_years)
  cnt <- counts %>% filter(sale_year %in% yrs, group %in% c(g, "market_sale")) %>%
    group_by(group, state, sale_year) %>% summarise(n = sum(n), .groups = "drop")
  bt <- bygroup_below_threshold(cnt %>% filter(group == g), pre_years = pre_years, post_years = post_years)
  zr <- bygroup_drop_zero_states(cnt %>% filter(group == g) %>% select(state, sale_year, n), "n", yrs, g)
  sy <- zr$data %>% rename(n_fam = n) %>%
    left_join(cnt %>% filter(group == "market_sale") %>% select(state, sale_year, n_market = n),
              by = c("state", "sale_year")) %>%
    mutate(post = as.integer(sale_year >= min(post_years)), ca = as.integer(state == "CA"))
  stacked <- sy %>% pivot_longer(c(n_fam, n_market), names_to = "grp", values_to = "n") %>%
    mutate(fam = as.integer(grp == "n_fam"))
  m_ddd <- bygroup_feols(log(n) ~ ca:post:fam + ca:post | state^grp + sale_year^grp, stacked, g, "DDD",
                         pre_years = pre_years, post_years = post_years)
  m_did <- bygroup_feols(log(n_fam) ~ ca:post | state + sale_year, sy, g, "DiD", n_col = "n_fam",
                         pre_years = pre_years, post_years = post_years)
  tibble(ddd = unname(coef(m_ddd$model)["ca:post:fam"]), ddd_se = unname(se(m_ddd$model)["ca:post:fam"]),
         did = unname(coef(m_did$model)["ca:post"]), did_se = unname(se(m_did$model)["ca:post"]),
         ddd_nobs = m_ddd$info$nobs, n_states = n_distinct(sy$state), n_states_dropped = zr$summary$n_dropped,
         ca_per_year_pre = bt$ca_per_year_pre, ca_per_year_post = bt$ca_per_year_post, below_threshold = bt$below)
}
#' Monthly event study (17 section E): 2018-2023, reference = pooled months from ref_from to 201912
run_es <- function(g, ref_from = 201801L) {
  panel <- counts %>% filter(group == g, between(sale_year, 2018L, 2023L)) %>%
    select(state, ym, n_fam = n) %>% mutate(ca = as.integer(state == "CA"))
  stopifnot(n_distinct(panel$state) == 51L, nrow(panel) == 51L * 72L)
  n_zero <- sum(panel$n_fam == 0)
  panel$lnf <- if (n_zero == 0L) log(panel$n_fam) else log1p(panel$n_fam)
  ref_set <- r1_months(ref_from, 201912L)
  m <- feols(as.formula(paste0("lnf ~ i(ym, ca, ref = c(", paste(ref_set, collapse = ","), ")) | state + ym")),
             data = panel, cluster = ~state)
  es <- broom::tidy(m) %>% filter(str_detect(term, "^ym::")) %>%
    transmute(group = g, ym = as.integer(str_extract(term, "(?<=ym::)[0-9]+")), estimate, std.error)
  list(es = es, transform = if (n_zero == 0L) "log" else "log1p", n_zero = n_zero)
}
res_rows <- list(); es_all <- list()
for (g in split_names) {
  cfp <- r1_cf(counts, g, 2019L, 201701L, 202406L)
  janfeb <- r1_wsum(cfp, "ca_actual", 202101L, 202102L) / r1_wsum(cfp, "cf", 202101L, 202102L)
  nine <- r1_wsum(cfp, "ca_actual", 202006L, 202102L) / r1_wsum(cfp, "cf", 202006L, 202102L)
  rush <- r1_wsum(cfp, "excess", 202006L, 202102L)
  short <- -r1_wsum(cfp, "excess", 202103L, 202406L)
  ca_m <- counts %>% filter(group == g, state == "CA") %>% left_join(bd_month, by = "ym")
  bday_ratio <- (sum(ca_m$n[ca_m$ym %in% 202101:202102]) / sum(ca_m$bdays[ca_m$ym %in% 202101:202102])) /
    (sum(ca_m$n[ca_m$ym %in% 201901:201902]) / sum(ca_m$bdays[ca_m$ym %in% 201901:201902]))
  d1719 <- run_ddd(g, 2017:2019); d19 <- run_ddd(g, 2019L)
  e <- run_es(g); es_all[[g]] <- e$es %>% mutate(transform = e$transform)
  e19 <- run_es(g, 201901L)     # reference 2019 alone: after the 2018 break in prior death documents
  em <- function(a, b) mean(e$es$estimate[e$es$ym >= a & e$es$ym <= b])
  res_rows[[g]] <- tibble(
    group = g, window_months = suppressWarnings(as.integer(str_extract(g, "[0-9]+$"))),
    part = case_when(g == "heir_all" ~ "all", grepl("nodeath", g) ~ "no prior death document", TRUE ~ "prior death document"),
    ca_deeds_janfeb_2021 = sum(ca_m$n[ca_m$ym %in% 202101:202102]),
    ca_deeds_janfeb_2019 = sum(ca_m$n[ca_m$ym %in% 201901:201902]),
    janfeb_2021_vs_cf = janfeb, nine_month_vs_cf = nine, ca_janfeb_bday_vs_2019 = bday_ratio,
    rush_deeds = rush, shortfall_deeds_202406 = short,
    deadline_bday_ca_2021 = deadline_ratio(g, "CA", 2021), deadline_bday_ca_2019 = deadline_ratio(g, "CA", 2019),
    deadline_bday_other_2021 = deadline_ratio(g, "other", 2021),
    deadline_calday_ca_2021 = deadline_ratio(g, "CA", 2021, FALSE),
    ddd = d1719$ddd, ddd_se = d1719$ddd_se, did = d1719$did, did_se = d1719$did_se,
    ddd_nobs = d1719$ddd_nobs, ddd_states = d1719$n_states, ddd_states_dropped = d1719$n_states_dropped,
    ca_per_year_pre = d1719$ca_per_year_pre, ca_per_year_post = d1719$ca_per_year_post,
    below_threshold = d1719$below_threshold,
    ddd_pre2019 = d19$ddd, ddd_pre2019_se = d19$ddd_se,
    es_mar_dec_2021 = em(202103L, 202112L), es_2022 = em(202201L, 202212L), es_2023 = em(202301L, 202312L),
    es_2022_2023 = em(202201L, 202312L),
    # against 2019 alone (after the 2018 break in how often a prior death document is found)
    es_2022_2023_vs_2019 = mean(e19$es$estimate[e19$es$ym >= 202201L & e19$es$ym <= 202312L]),
    es_transform = e$transform, es_zero_cells = e$n_zero)
}
res <- bind_rows(res_rows)
write_csv_strict(res, TT("r1_heir_split_results.csv"))
print(as.data.frame(res %>% select(group, ca_deeds_janfeb_2021, janfeb_2021_vs_cf, deadline_bday_ca_2021,
                                   ddd, ddd_se, ddd_pre2019, es_2022_2023, es_transform, below_threshold)), digits = 3)
# reproduction: the whole group equals 17's trust_heir_chain rush ratio, DDD and event study
old_ru <- read_csv(TT("bygroup_rush_table.csv"), show_col_types = FALSE) %>%
  filter(group == "trust_heir_chain", base_window == "2019", window == "jan_feb_2021")
old_dd <- read_csv(TT("bygroup_ddd.csv"), show_col_types = FALSE) %>% filter(group == "trust_heir_chain")
old_es <- read_csv(TT("bygroup_event_study_monthly.csv"), show_col_types = FALSE) %>%
  filter(group == "trust_heir_chain")
ra <- res %>% filter(group == "heir_all")
stopifnot(abs(ra$janfeb_2021_vs_cf - old_ru$relative_ratio) < 1e-9, abs(ra$ddd - old_dd$ddd) < 1e-9,
          abs(ra$ddd_se - old_dd$ddd_se) < 1e-9,
          abs(ra$es_2022 - mean(old_es$estimate[old_es$ym >= 202201 & old_es$ym <= 202212])) < 1e-9)
message("heir_all reproduces 17's trust_heir_chain rush ratio, DDD, SE and 2022 event-study mean")
es_all <- bind_rows(es_all)
write_csv_strict(es_all, TT("r1_heir_split_event_study_monthly.csv"))

# ============================================================
# Figures ----
# ============================================================
sy_plot <- share_year %>% select(state_class, sale_year, starts_with("share_death")) %>%
  select(-share_death_sameday, -share_death24_af, -share_death_after24) %>%
  pivot_longer(starts_with("share_death"), names_to = "window", values_to = "share") %>%
  mutate(window = paste(sub("share_death", "", window), "months"),
         where = if_else(state_class == "CA", "California", "Other states"))
p_sh <- ggplot(sy_plot, aes(sale_year, 100 * share, color = window, linetype = where)) +
  annotate("rect", xmin = 2014.6, xmax = 2016.5, ymin = -Inf, ymax = Inf, fill = "grey92") +
  geom_vline(xintercept = 2017.5, linetype = "dotted", color = "grey40") +
  geom_line(linewidth = 0.6) + geom_point(size = 1.4) +
  scale_color_manual(values = palette_paper()[c(1, 2, 3)]) +
  scale_x_continuous(breaks = 2015:2023) +
  labs(x = "Year of the trust-to-heir deed (shaded: chain look-back incomplete)",
       y = "Percent with a death document on the parcel\nrecorded in the prior window", color = "Window",
       linetype = NULL) +
  theme(legend.position = "bottom")
ggsave(FF("FR1_heir_split_share.png"), p_sh, width = 9, height = 5, dpi = 300, bg = "transparent")
pe <- es_all %>% filter(group %in% c("heir_all", paste0("heir_death", main_w), paste0("heir_nodeath", main_w))) %>%
  mutate(label = factor(case_when(group == "heir_all" ~ "All chain-verified heirs",
                                  grepl("nodeath", group) ~ "No death document in prior 24 months",
                                  TRUE ~ "Death document in prior 24 months")),
         date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)))
p_es <- ggplot(pe, aes(date, estimate)) +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_ribbon(aes(ymin = estimate - 1.96 * std.error, ymax = estimate + 1.96 * std.error), fill = palette_paper()[2], alpha = 0.2) +
  geom_line(color = palette_paper()[2], linewidth = 0.5) +
  geom_vline(xintercept = prop19_event_dates[["effective"]], linetype = "dashed", color = "grey30") +
  facet_wrap(~label, ncol = 1, scales = "free_y") +
  labs(x = NULL, y = "California relative to other states, log points (2018-19 = 0)")
ggsave(FF("FR1_heir_split_event_study.png"), p_es, width = 9, height = 7.5, dpi = 300, bg = "transparent")

# ============================================================
# RONE macros ----
# ============================================================
S_r <- "r1_heir_split_results.csv"; S_y <- "r1_heir_split_share_by_year.csv"; S_p <- "r1_heir_split_share_by_period.csv"
ww <- c(`12` = "Twelve", `24` = "TwentyFour", `36` = "ThirtySix")
nums <- list()
add <- function(...) nums[[length(nums) + 1L]] <<- r1_number(...)
for (g in split_names) {
  r <- res %>% filter(group == g)
  stem <- if (g == "heir_all") "RONEHeirAll" else paste0("RONEHeir", if (grepl("nodeath", g)) "Without" else "With",
                                                        ww[[as.character(r$window_months)]])
  add(paste0(stem, "JanFeb"), r$janfeb_2021_vs_cf, S_r)
  add(paste0(stem, "JanFebBday"), r$ca_janfeb_bday_vs_2019, S_r)
  add(paste0(stem, "DeadlineBday"), r$deadline_bday_ca_2021, S_r)
  add(paste0(stem, "DeadlineBdayNineteen"), r$deadline_bday_ca_2019, S_r)
  add(paste0(stem, "DDD"), r$ddd, S_r, 3)
  add(paste0(stem, "DDDSE"), r$ddd_se, S_r, 3)
  add(paste0(stem, "DDDPreNineteen"), r$ddd_pre2019, S_r, 3)
  add(paste0(stem, "ESPost"), r$es_2022_2023, S_r, 3)
  add(paste0(stem, "ESPostVsNineteen"), r$es_2022_2023_vs_2019, S_r, 3)
  add(paste0(stem, "DDDPreNineteenSE"), r$ddd_pre2019_se, S_r, 3)
  add(paste0(stem, "CAJanFeb"), r$ca_deeds_janfeb_2021, S_r, 0)
  add(paste0(stem, "CAPerYearPre"), r$ca_per_year_pre, S_r, 0)
}
yw <- setNames(c("Fifteen", "Sixteen", "Seventeen", "Eighteen", "Nineteen", "Twenty", "TwentyOne", "TwentyTwo",
                 "TwentyThree"), 2015:2023)
for (sc in c("CA", "other")) {
  for (yy in 2015:2023) {
    r <- share_year %>% filter(state_class == sc, sale_year == yy)
    add(paste0("RONEHeirShare", if (sc == "CA") "CA" else "Other", yw[[as.character(yy)]]), 100 * r$share_death24, S_y, 1)
  }
}
pw <- c(jan_feb_2019 = "JanFebNineteen", `2017_2019` = "PreYears", jun2020_feb2021 = "Rush",
        jan_feb_2021 = "JanFebTwentyOne", mar_dec_2021 = "MarDecTwentyOne", `2022_2023` = "Post")
for (i in seq_len(nrow(share_period))) {
  r <- share_period[i, ]
  if (r$state_class != "CA") next
  add(paste0("RONEHeirSharePeriod", pw[[r$period]], ww[[as.character(r$w)]]), 100 * r$share_with, S_p, 1)
}
r1_save_numbers(bind_rows(nums), "24")

message("Finished 24_heir_death_split at ", Sys.time())
sink()
