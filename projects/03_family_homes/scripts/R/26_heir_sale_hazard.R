#======================================================================#
# Purpose : Market-sale hazard of California homes after a family
#           transfer, before and after Proposition 19 (WP-R2; referee
#           item A1). After 16 February 2021 an heir who does not move in
#           is reassessed to market value, so non-occupant heirs should
#           sell more than before; occupant heirs keep the base (up to the
#           cap). Split by the recipient's mailing-address occupancy proxy
#           (mail_eq_situs on the transfer deed) and by the parcel's wedge bin
#           (from 25: January 2017 lien date for the pre-reform cohort,
#           January 2019 for the post-reform one), against the same split before the reform,
#           with market purchases and spouse deeds as benchmarks.
#           Cohorts by signing date, matched by a four-year shift:
#             post: 17 Feb 2021 - 30 Jun 2024, censored 30 Jun 2024 (last usable month 202406)
#             pre : 17 Feb 2017 - 30 Jun 2020, censored 30 Jun 2020 (pre-reform follow-up
#                   ends with ballot placement, 24 Jun 2020)
#           Outputs: Kaplan-Meier sale probabilities at 6-36 months;
#           fixed-horizon (complete follow-up) 12- and 24-month sale
#           shares with county-clustered LPM differences; stratified Cox
#           hazard ratios with county-clustered errors.
# Name    : Saani Rawat
# Created : 11/13/2026
# Log     : 11/13/2026: created (WP-R2 of plans/2026-11-12_review-analyses.md).
# Inputs  : data/derived/03_family_homes/events.parquet (events_panel_path)
#           data/derived/03_family_homes/r2_parcel_wedge_bins.parquet,
#           r2_parcel_wedge_bins_lien2017.parquet (25)
# Outputs : _outputs/tables/r2_heir_*.csv
#           data/derived/03_family_homes/r2_heir_sale_deeds.parquet
#           _outputs/tables/r2_numbers_sources.csv, manuscript/tables/r2_numbers.tex (shared with 25)
#           _outputs/logs/26_heir_sale_hazard.log
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_group_helpers.R"))

suppressPackageStartupMessages({
  library(fixest)
  library(survival)
})

script_name <- "26_heir_sale_hazard.R"
log_file <- path(logs_dir, "26_heir_sale_hazard.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 26_heir_sale_hazard at ", Sys.time())
t_start <- Sys.time()

#============================================#
# Settings ----
#============================================#

duck_mem     <- "15GB"               # ~25% of the 63 GB machine
duck_threads <- 8L
spill_dir    <- "C:/Temp/r2_duckdb"  # local disk outside the OneDrive tree

# entry windows (signing date) and censoring dates; pre = post shifted back four years
periods <- tribble(
  ~period, ~entry_start,  ~entry_end,    ~censor_date,
  "pre",   "2017-02-17",  "2020-06-30",  "2020-06-30",
  "post",  "2021-02-17",  "2024-06-30",  "2024-06-30"
) %>% mutate(across(-period, as.Date))
# fixed horizons with complete follow-up: entry must be at least h months before censoring
horizons <- c(12L, 24L)
km_times <- c(6, 12, 24, 36)
days_per_month <- 365.25 / 12

# groups: heirs = main measure without spouse deeds; heirs at death = the death-triggered part
# (estate/executor deeds and chain-verified trust-to-heir deeds; affidavits are outside the
# measure); benchmarks: spouse deeds (law unchanged) and market purchases
f_interg <- bygroup_filter("main_distinct_xspouse_ca")
f_death  <- paste0("(", bygroup_filter(bygroup_main), " AND coalesce(fam_trigger, '') IN ('death', 'death_likely')",
                   " AND coalesce(fam_relation, '') <> 'spouse')")
group_sql <- c(heirs       = f_interg,
               heirs_death = f_death,
               lifetime_pc = bygroup_filter("lifetime_pc"),
               spouse      = bygroup_filter("spouse_ca"),
               market      = bygroup_filter("market_sale"))
heir_groups <- c("heirs", "heirs_death", "lifetime_pc")

# outputs
f_parcels <- path(data_dir, "r2_parcel_wedge_bins.parquet")
f_parcels17 <- path(data_dir, "r2_parcel_wedge_bins_lien2017.parquet")
f_deeds  <- path(data_dir, "r2_heir_sale_deeds.parquet")
f_samples <- path(tables_out_dir, "r2_heir_samples.csv")
f_km      <- path(tables_out_dir, "r2_heir_km.csv")
f_fixed   <- path(tables_out_dir, "r2_heir_fixed_horizon.csv")
f_lpm     <- path(tables_out_dir, "r2_heir_lpm.csv")
f_cox     <- path(tables_out_dir, "r2_heir_cox.csv")
f_sources <- path(tables_out_dir, "r2_numbers_sources.csv")
f_macros  <- path(tables_dir, "r2_numbers.tex")

#============================================#
# Importing deeds and next market sales (DuckDB) ----
#============================================#

stopifnot(file_exists(f_parcels), file_exists(f_parcels17))
dir_create(spill_dir)
con <- open_corelogic_duckdb(memory_limit = duck_mem, threads = duck_threads)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(spill_dir)}'"))
ev_src <- glue("read_parquet('{sql_quote_path(events_panel_path)}')")
dbWriteTable(con, "periods", as.data.frame(periods), temporary = TRUE, overwrite = TRUE)

# |- transfer deeds in the entry windows, one row per group x parcel x signing date ----
# Duplicate deed rows of a parcel-date are collapsed; a conflicting occupancy flag becomes missing.
group_union <- paste(map_chr(names(group_sql), function(k) glue(
  "SELECT '{k}' AS g, clip, fips5, {event_date_sql('sale_raw')} AS sale_date, mail_eq_situs
   FROM {ev_src}
   WHERE state = 'CA' AND corp_buyer = 0 AND sale_year BETWEEN 2017 AND 2024
     AND coalesce({group_sql[[k]]}, FALSE)")), collapse = "\nUNION ALL\n")
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE d AS
  SELECT g, clip, sale_date, min(fips5) AS fips5,
         CASE WHEN min(mail_eq_situs) = max(mail_eq_situs) THEN min(mail_eq_situs) END AS occ,
         p.period, p.censor_date
  FROM ({group_union}) u
  JOIN periods p ON u.sale_date BETWEEN p.entry_start AND p.entry_end
  GROUP BY g, clip, sale_date, p.period, p.censor_date"))

# |- outcome: next market sale of the same parcel, strictly after the deed (as 18) ----
dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE ms AS
  SELECT DISTINCT clip, {event_date_sql('sale_raw')} AS msale_date
  FROM {ev_src} WHERE class = 'market_sale' AND ym >= 201701"))
dbExecute(con, "DELETE FROM ms WHERE msale_date IS NULL")
deeds <- as_tibble(dbGetQuery(con, "
  SELECT d.*, ms.msale_date
  FROM d ASOF LEFT JOIN ms ON d.clip = ms.clip AND d.sale_date < ms.msale_date"))
dbDisconnect(con, shutdown = TRUE)
message("NOTE: ", format(nrow(deeds), big.mark = ","), " deeds in the entry windows")

#============================================#
# Building spells ----
#============================================#

# Wedge bins from the last lien date before each cohort's first deed (25): January 2017 for
# the pre-reform cohort, January 2019 for the post-reform cohort. A 2019 wedge would follow
# some pre-reform heirs' own sales (a market sale resets the base), so it cannot be used there.
parcel_bins <- bind_rows(
  arrow::read_parquet(f_parcels17, col_select = c("clip", "fips5", "wbin")) %>% mutate(period = "pre"),
  arrow::read_parquet(f_parcels, col_select = c("clip", "fips5", "wbin")) %>% mutate(period = "post")) %>%
  rename(fips5_parcel = fips5) %>%
  mutate(clip = as.numeric(clip))

deeds <- deeds %>%
  mutate(clip = as.numeric(clip)) %>%
  left_join(parcel_bins, by = c("clip", "period")) %>%
  mutate(
    # county for clustering: the parcel file's county, else the deed's; non-California codes
    # (0.1% of deeds, miscoded) are dropped below
    fips5 = coalesce(fips5_parcel, fips5),
    fips5 = if_else(str_starts(coalesce(fips5, ""), "06"), fips5, NA_character_),
    post = as.integer(period == "post"),
    # time to the next market sale, censored at the period's censoring date
    event = as.integer(!is.na(msale_date) & msale_date <= censor_date),
    end_date = if_else(event == 1L, msale_date, censor_date),
    t_months = as.numeric(end_date - sale_date) / days_per_month,
    occ_lab = case_when(occ == 1L ~ "occupant", occ == 0L ~ "nonoccupant", TRUE ~ "missing"),
    nonocc = as.integer(occ == 0L),
    wgroup = case_when(is.na(wbin) ~ "Wnone_off_universe", wbin == "W00" ~ "Wzero",
                       wbin <= "W03" ~ "Wlow_D1_3", wbin <= "W07" ~ "Wmid_D4_7",
                       TRUE ~ "Whigh_D8_10")
  )
# fixed horizons: sold within h calendar months, entry early enough for complete follow-up
for (h in horizons) {
  deeds[[paste0("full", h)]] <- as.integer(add_calendar_months(deeds$sale_date, h) <= deeds$censor_date)
  deeds[[paste0("sold", h)]] <- as.integer(!is.na(deeds$msale_date) &
                                             deeds$msale_date <= add_calendar_months(deeds$sale_date, h))
}
# deeds signed on the censoring date itself have no follow-up
message("NOTE: dropping ", sum(deeds$sale_date >= deeds$censor_date), " deeds signed on the censoring date; ",
        sum(is.na(deeds$fips5)), " deeds without a California county")
deeds <- deeds %>% filter(sale_date < censor_date, !is.na(fips5))
stopifnot(all(deeds$t_months > 0), n_distinct(deeds$fips5) <= 58L)
arrow::write_parquet(deeds %>% select(-fips5_parcel), f_deeds, compression = "zstd")

# |- sample sizes ----
samples <- deeds %>%
  group_by(g, period, occ_lab) %>%
  summarise(n_deeds = n(), n_events = sum(event), n_full24 = sum(full24), n_full12 = sum(full12),
            share_on_universe = mean(!is.na(wbin)), share_event_within_30d = mean(event == 1L & t_months < 1),
            .groups = "drop")
write_csv_strict(samples, f_samples)
print(as.data.frame(samples))

#============================================#
# Kaplan-Meier sale probabilities ----
#============================================#

#' Cumulative sale probability at km_times by stratum
km_table <- function(df, by) {
  df <- df %>% mutate(stratum = interaction(across(all_of(by)), sep = "|", drop = TRUE))
  fit <- survfit(Surv(t_months, event) ~ stratum, data = df)
  s <- summary(fit, times = km_times, extend = TRUE)
  tibble(stratum = sub("^stratum=", "", as.character(s$strata)), months = s$time,
         n_risk = s$n.risk, sale_prob = 1 - s$surv, se = s$std.err) %>%
    mutate(sale_prob = if_else(n_risk > 0, sale_prob, NA_real_),
           se = if_else(n_risk > 0, se, NA_real_)) %>%
    separate(stratum, into = by, sep = "\\|")
}
km_occ <- km_table(deeds, c("g", "period", "occ_lab")) %>% mutate(split = "occupancy")
km_all <- km_table(deeds, c("g", "period")) %>% mutate(split = "all", occ_lab = "all")
km_wedge <- km_table(deeds %>% filter(g %in% c(heir_groups, "market")),
                     c("g", "period", "occ_lab", "wgroup")) %>% mutate(split = "occupancy_x_wedge")
km <- bind_rows(km_all, km_occ, km_wedge) %>% relocate(split, g, period, occ_lab, wgroup)
write_csv_strict(km, f_km)
message("Kaplan-Meier sale probability (24 and 36 months):")
km %>% filter(split != "occupancy_x_wedge", months %in% c(24, 36)) %>%
  mutate(across(c(sale_prob, se), ~ round(.x, 4))) %>% as.data.frame() %>% print(row.names = FALSE)

#============================================#
# Fixed-horizon sale shares and LPM differences ----
#============================================#

fixed_tbl <- map_dfr(horizons, function(h) {
  full_col <- paste0("full", h); sold_col <- paste0("sold", h)
  d <- deeds %>% filter(.data[[full_col]] == 1L)
  bind_rows(
    d %>% group_by(g, period, occ_lab) %>% summarise(n = n(), sold = mean(.data[[sold_col]]), .groups = "drop"),
    d %>% group_by(g, period) %>% summarise(n = n(), sold = mean(.data[[sold_col]]), .groups = "drop") %>%
      mutate(occ_lab = "all"),
    d %>% filter(g %in% heir_groups) %>% group_by(g, period, occ_lab, wgroup) %>%
      summarise(n = n(), sold = mean(.data[[sold_col]]), .groups = "drop")
  ) %>% mutate(horizon = h)
}) %>%
  mutate(wgroup = coalesce(wgroup, "all")) %>%
  pivot_wider(names_from = period, values_from = c(n, sold)) %>%
  mutate(change_pp = 100 * (sold_post - sold_pre)) %>%
  relocate(horizon, g, occ_lab, wgroup)
write_csv_strict(fixed_tbl, f_fixed)
message("Fixed-horizon sale shares (complete follow-up):")
fixed_tbl %>% filter(wgroup == "all") %>% mutate(across(where(is.double), ~ signif(.x, 4))) %>%
  as.data.frame() %>% print(row.names = FALSE)

#' Tidy coefficients of a fixest model with the number of county clusters
tidy_fit <- function(m, outcome, model, group) {
  ct <- coeftable(m)
  n_g <- tryCatch(as.integer(min(attr(vcov(m, attr = TRUE), "G"))), error = function(e) NA_integer_)
  tibble(outcome = outcome, model = model, group = group, term = rownames(ct),
         estimate = ct[, 1], se = ct[, 2], pval = ct[, 4], nobs = nobs(m), n_clusters = n_g)
}

lpm <- list()
for (h in horizons) {
  y <- paste0("sold", h, "_pp")
  d_h <- deeds %>%
    filter(.data[[paste0("full", h)]] == 1L, occ_lab != "missing") %>%
    mutate(!!y := 100 * .data[[paste0("sold", h)]], pn = post * nonocc)
  out_h <- paste0("sold", h)
  for (gg in names(group_sql)) {
    d <- d_h %>% filter(g == gg)
    # post-minus-pre change for each occupancy type, and their difference
    m_dd <- feols(as.formula(paste(y, "~ pn | fips5^post + fips5^nonocc")), data = d,
                  cluster = ~ fips5, notes = FALSE)
    m_occ <- feols(as.formula(paste(y, "~ post | fips5")), data = d %>% filter(nonocc == 0L),
                   cluster = ~ fips5, notes = FALSE)
    m_non <- feols(as.formula(paste(y, "~ post | fips5")), data = d %>% filter(nonocc == 1L),
                   cluster = ~ fips5, notes = FALSE)
    lpm[[length(lpm) + 1]] <- bind_rows(
      tidy_fit(m_dd, out_h, "dd_nonocc_minus_occ", gg),
      tidy_fit(m_occ, out_h, "change_occupant", gg),
      tidy_fit(m_non, out_h, "change_nonoccupant", gg))
  }
  # triple differences: heirs against market purchases and against spouse deeds
  for (hg in heir_groups) {
    for (bench in c("market", "spouse")) {
      d <- d_h %>% filter(g %in% c(hg, bench)) %>%
        mutate(heir = as.integer(g == hg), pn_h = pn * heir)
      m <- feols(as.formula(paste(y, "~ pn + pn_h | fips5^post^heir + fips5^nonocc^heir")), data = d,
                 cluster = ~ fips5, notes = FALSE)
      lpm[[length(lpm) + 1]] <- tidy_fit(m, out_h, paste0("ddd_vs_", bench), hg)
    }
    # difference by wedge group (heirs only)
    d <- d_h %>% filter(g == hg)
    m_w <- feols(as.formula(paste(y, "~ i(wgroup, pn) | fips5^post^wgroup + fips5^nonocc^wgroup")),
                 data = d, cluster = ~ fips5, notes = FALSE)
    m_wn <- feols(as.formula(paste(y, "~ i(wgroup, post) | fips5^wgroup")),
                  data = d %>% filter(nonocc == 1L), cluster = ~ fips5, notes = FALSE)
    lpm[[length(lpm) + 1]] <- bind_rows(tidy_fit(m_w, out_h, "dd_by_wedge_group", hg),
                                        tidy_fit(m_wn, out_h, "change_nonoccupant_by_wedge_group", hg))
  }
}
lpm_tbl <- bind_rows(lpm)
write_csv_strict(lpm_tbl, f_lpm)
message("LPM (percentage points, county-clustered):")
lpm_tbl %>% filter(!grepl("wedge", model)) %>%
  mutate(across(c(estimate, se, pval), ~ signif(.x, 3))) %>% as.data.frame() %>% print(row.names = FALSE)

#============================================#
# Stratified Cox hazards (full censored windows) ----
#============================================#

#' Cox hazard ratios, strata = county (x group), county-clustered robust errors
cox_fit <- function(d, rhs, label, group) {
  f <- as.formula(paste("Surv(t_months, event) ~", rhs, "+ strata(stratum)"))
  m <- coxph(f, data = d, cluster = fips5, ties = "efron")
  ct <- summary(m)$coefficients
  tibble(model = label, group = group, term = rownames(ct), log_hr = ct[, "coef"],
         hr = ct[, "exp(coef)"], robust_se = ct[, "robust se"], pval = ct[, ncol(ct)],
         nobs = m$n, n_events = m$nevent, n_clusters = n_distinct(d$fips5))
}
d_cox <- deeds %>% filter(occ_lab != "missing") %>% mutate(pn = post * nonocc)
cox <- list()
for (gg in c(heir_groups, "spouse", "market")) {
  t0 <- Sys.time()
  d <- d_cox %>% filter(g == gg) %>% mutate(stratum = fips5)
  cox[[length(cox) + 1]] <- cox_fit(d, "post + nonocc + pn", "cox_dd", gg)
  message(sprintf("  Cox [%s] %s rows in %.1f min", gg, format(nrow(d), big.mark = ","),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}
for (hg in c("heirs", "heirs_death")) {
  d <- d_cox %>% filter(g %in% c(hg, "market")) %>%
    mutate(heir = as.integer(g == hg), stratum = paste(g, fips5), p_h = post * heir,
           n_h = nonocc * heir, pn_h = pn * heir)
  cox[[length(cox) + 1]] <- cox_fit(d, "post + nonocc + pn + p_h + n_h + pn_h", "cox_ddd_vs_market", hg)
}
cox_tbl <- bind_rows(cox)
write_csv_strict(cox_tbl, f_cox)
message("Cox (hazard ratios, county-clustered robust SE):")
cox_tbl %>% mutate(across(c(log_hr, hr, robust_se, pval), ~ signif(.x, 3))) %>%
  as.data.frame() %>% print(row.names = FALSE)

#============================================#
# Number macros (RTWO) and sources ----
#============================================#

macro_rows <- list()
add_macro <- function(name, value, digits, source_file, note = "") {
  stopifnot(grepl("^RTWO[A-Za-z]+$", name), length(value) == 1L, is.finite(value))
  macro_rows[[name]] <<- tibble(macro = name, value = value, digits = digits,
                                formatted = formatC(value, format = "f", digits = digits, big.mark = ","),
                                source_file = path_rel(source_file, project_dir),
                                script = script_name, note = note)
}
fx <- function(h, g_, occ_, col) {
  v <- fixed_tbl[[col]][fixed_tbl$horizon == h & fixed_tbl$g == g_ & fixed_tbl$occ_lab == occ_ &
                          fixed_tbl$wgroup == "all"]
  stopifnot(length(v) == 1L)
  v
}
lx <- function(out_, model_, g_, term_, col = "estimate") {
  v <- lpm_tbl[[col]][lpm_tbl$outcome == out_ & lpm_tbl$model == model_ & lpm_tbl$group == g_ &
                        lpm_tbl$term == term_]
  stopifnot(length(v) == 1L)
  v
}
gword <- c(heirs = "Heir", heirs_death = "HeirDeath", lifetime_pc = "ParentChild", spouse = "Spouse",
           market = "Market")
oword <- c(occupant = "Occ", nonoccupant = "Nonocc")
for (gg in names(gword)) {
  for (oc in names(oword)) {
    for (pp in c("pre", "post")) {
      add_macro(paste0("RTWOSoldTwentyFour", gword[[gg]], oword[[oc]], if (pp == "pre") "Pre" else "Post"),
                100 * fx(24L, gg, oc, paste0("sold_", pp)), 1, f_fixed, "percent sold within 24 months")
      add_macro(paste0("RTWONTwentyFour", gword[[gg]], oword[[oc]], if (pp == "pre") "Pre" else "Post"),
                fx(24L, gg, oc, paste0("n_", pp)), 0, f_fixed, "deeds with complete 24-month follow-up")
    }
  }
  add_macro(paste0("RTWODDTwentyFour", gword[[gg]]), lx("sold24", "dd_nonocc_minus_occ", gg, "pn"), 2, f_lpm)
  add_macro(paste0("RTWODDTwentyFour", gword[[gg]], "SE"), lx("sold24", "dd_nonocc_minus_occ", gg, "pn", "se"), 2, f_lpm)
}
for (hg in c("heirs", "heirs_death")) {
  add_macro(paste0("RTWODDDMarketTwentyFour", gword[[hg]]), lx("sold24", "ddd_vs_market", hg, "pn_h"), 2, f_lpm)
  add_macro(paste0("RTWODDDMarketTwentyFour", gword[[hg]], "SE"), lx("sold24", "ddd_vs_market", hg, "pn_h", "se"), 2, f_lpm)
  hr <- cox_tbl %>% filter(model == "cox_dd", group == hg, term == "pn")
  add_macro(paste0("RTWOCoxHR", gword[[hg]]), hr$hr, 3, f_cox, "hazard ratio post x non-occupant")
  add_macro(paste0("RTWOCoxLogHRSE", gword[[hg]]), hr$robust_se, 3, f_cox)
}

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

message("Finished 26_heir_sale_hazard at ", Sys.time(), " (",
        round(difftime(Sys.time(), t_start, units = "mins"), 1), " min)")
