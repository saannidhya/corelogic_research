#======================================================================#
# Purpose : Estimate the two decisions of the framework in reduced form,
#           inside California, from the parcel's property-tax wedge.
#           Step 1, the parent: does the monthly rate of lifetime family
#           deeds respond to the wedge, and does the response change sign
#           at Proposition 19 (positive before the deadline, negative
#           after)?
#           Step 2, the heir: after a home passes at death, does the rate
#           of homes sold within 24 months rise with the wedge once the
#           heir no longer keeps the old assessment?
#           Unit: California single-family and condominium parcels with a
#           wedge fixed at the January 1, 2017 lien date (04a). A parcel is
#           at risk from January 2017 until its first market sale or its
#           first family transfer (spouse-document deeds do not end the
#           spell). Deeds come from the locked taxonomy of the panel
#           (deed_cat, fam_route), not from the classes stored in the
#           wedge file.
#           Placebos in the same cells: spouse deeds, same-owner retitles,
#           deeds into the owner's own trust, market sales.
# Name    : Saani Rawat
# Created : 11/19/2026
# Log     : 11/19/2026: first version (step 1).
#           11/19/2026: added the deadline by signing day, affidavits by trust party,
#                       and deeds that add a person to title.
#           11/19/2026: added step 2 (homes sold or kept within 24 months
#                       of the deed), the 2021 wedge, and the stacked
#                       comparison of lifetime deeds with placebo deeds.
# Inputs  : data/derived/03_family_homes/events.parquet
#           data/derived/03_family_homes/ca_parcel_wedge.parquet (04a; attributes only)
# Outputs : data/derived/03_family_homes/fw_wedge_cells.parquet   cell x month counts
#           _outputs/tables/fw_coverage.csv            deeds on the risk set, by type and year
#           _outputs/tables/fw_raw_rates.csv           deeds per 1,000 parcel-years, bin x period
#           _outputs/tables/fw_deadline_notch.csv      deeds signed 1-15 and 16-28 February, bin x year
#           _outputs/tables/fw_deadline_daily.csv      deeds by signing day, November 2020-April 2021
#           _outputs/tables/fw_gradient_bins.csv       wedge bin x period coefficients
#           _outputs/tables/fw_gradient_linear.csv     wedge ($000) x period slopes
#           _outputs/tables/fw_triple_bins.csv         lifetime deeds against placebo deeds
#           _outputs/tables/fw_heir_raw.csv            share sold within 24 months, bin x period
#           _outputs/tables/fw_heir_bins.csv           step 2: wedge bin x period coefficients
#           _outputs/tables/fw_heir_linear.csv         step 2: wedge ($000) x period slopes
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

#============================================#
# Settings ----
#============================================#

lien_cohort <- "d2017"       # wedge and attributes fixed at January 1, 2017
start_raw   <- 20170101L
end_ym      <- 202312L       # last month of deeds in step 1
heir_end_ym <- 202206L       # last month of deeds in step 2 (24 months of follow-up to June 2024)
resale_months <- 24L
wedge_cap_q <- 0.99          # winsorize the wedge at this quantile for the linear slope
ref_bin     <- "w0_0.5k"     # wedge under $500 a year: the reference group
ref_period  <- "base"
bin_levels  <- c("w0_0.5k", "w0.5_1.5k", "w1.5_3k", "w3_5k", "w5_10k", "w10k_plus")

# step 1 periods (months of the deed): the baseline is the one of the triple difference
period_of <- function(ym) {
  case_when(ym <= 201712L ~ "y2017",
            ym <= 201912L ~ "base",
            ym <  prop19_antic_start_ym ~ "early2020",
            ym <= prop19_antic_end_ym   ~ "antic",
            ym <= 202112L ~ "post2021",
            TRUE          ~ "post2223")
}
period_levels <- c("y2017", "base", "early2020", "antic", "post2021", "post2223")

# step 2 periods: deaths on or before February 15, 2021 kept the old rule whatever the date
# of the affidavit, so affidavits of March-August 2021 are a buffer
heir_period_of <- function(ym) {
  case_when(ym <= 201712L ~ "y2017",
            ym <= 201912L ~ "base",
            ym <  prop19_antic_start_ym ~ "early2020",
            ym <= prop19_antic_end_ym   ~ "antic",
            ym <= 202108L ~ "buffer",
            TRUE          ~ "post")
}
heir_period_levels <- c("y2017", "base", "early2020", "antic", "buffer", "post")

outcomes <- c(life_ns = "Lifetime family deeds, no spouse document",
              life_sur = "Lifetime family deeds, same surname",
              spouse = "Spouse-document deeds",
              affid = "Affidavits of death",
              affid_direct = "Affidavits of death, no trust party",
              affid_trust = "Affidavits of death, trust party",
              trust_fam = "Family transfers through a trust",
              sameown = "Same-owner retitles",
              sameown_only = "Same owners on both sides",
              joins = "Same owners, and a new person joins title",
              owntrust = "Deeds into the owner's own trust",
              market = "Market sales")
# step 2: the same deeds split by whether the home is sold on the market within 24 months
heir_outcomes <- c(affid_sold = "At death, sold within 24 months",
                   affid_kept = "At death, not sold within 24 months",
                   life_sold = "Lifetime deed, sold within 24 months",
                   life_kept = "Lifetime deed, not sold within 24 months",
                   mk_sold = "Market purchase, resold within 24 months",
                   mk_kept = "Market purchase, not resold within 24 months")
all_counts <- c(names(outcomes), names(heir_outcomes))

# thousands separator for messages
fmt_n <- function(x) format(round(x), big.mark = ",", trim = TRUE)

con <- open_corelogic_duckdb(memory_limit = "24GB")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

ev <- glue("read_parquet('{sql_quote_path(events_panel_path)}')")
wp <- glue("read_parquet('{sql_quote_path(path(data_dir, 'ca_parcel_wedge.parquet'))}')")

#============================================#
# 1. Risk set and deeds ----
#============================================#

# |- parcels: attributes only ----
# wk is the wedge at the lien date; wk21 the wedge at 2021 values (what a transfer in 2021
# would have locked in), both in $ thousand a year and winsorized
rate_sql <- "(CASE WHEN rate > 0.5 THEN rate / 100 ELSE rate END)"   # rate stored in percent or as a share
caps <- dbGetQuery(con, glue("
  SELECT QUANTILE_CONT(wedge, {wedge_cap_q}) AS q17,
         QUANTILE_CONT({rate_sql} * GREATEST(market_2021 - assessed_2021, 0), {wedge_cap_q}) AS q21
  FROM {wp} WHERE cohort = '{lien_cohort}'"))
dbExecute(con, glue("
  CREATE TEMP TABLE par AS
  SELECT clip, fips5, wedge_bin AS bin, hox, tenure, over_cap_2021 AS over_cap,
         LEAST(wedge, {caps$q17}) / 1000 AS wk,
         LEAST({rate_sql} * GREATEST(market_2021 - assessed_2021, 0), {caps$q21}) / 1000 AS wk21
  FROM {wp} WHERE cohort = '{lien_cohort}' AND wedge_bin IS NOT NULL"))
par_sum <- dbGetQuery(con, "SELECT COUNT(*) AS n, MEDIAN(wk) AS wk, MEDIAN(wk21) AS wk21 FROM par")
n_par <- par_sum$n
message("Parcels in the risk set: ", fmt_n(n_par), "; median wedge $", fmt_n(1000 * par_sum$wk),
        " (2017), $", fmt_n(1000 * par_sum$wk21), " (2021); winsorized at $", fmt_n(caps$q17),
        " and $", fmt_n(caps$q21))

# |- deeds on those parcels, by type ----
# is_exit ends the spell: a market sale resets the wedge, and a family transfer moves the
# home to the next owner. Spouse-document deeds keep the home with the same couple.
dbExecute(con, glue("
  CREATE TEMP TABLE ce0 AS
  SELECT e.clip, e.sale_raw, e.ym,
         (e.deed_cat = 'family_transfer' AND e.fam_route IN ('lifetime','unknown')
            AND coalesce(e.fam_relation, '') <> 'spouse')::INT                        AS life_ns,
         (e.deed_cat = 'family_transfer' AND e.fam_route IN ('lifetime','unknown')
            AND coalesce(e.fam_relation, '') <> 'spouse'
            AND e.deed_subcat = 'same_surname')::INT                                  AS life_sur,
         (e.deed_cat = 'family_transfer' AND e.fam_route IN ('lifetime','unknown')
            AND coalesce(e.fam_relation, '') = 'spouse')::INT                         AS spouse,
         (e.deed_cat = 'family_transfer' AND e.fam_route = 'death_affidavit')::INT    AS affid,
         (e.deed_cat = 'family_transfer' AND e.fam_route = 'death_affidavit'
            AND coalesce(e.deed_subcat, '') <> 'affidavit_trust_party')::INT          AS affid_direct,
         (e.deed_cat = 'family_transfer' AND e.fam_route = 'death_affidavit'
            AND coalesce(e.deed_subcat, '') = 'affidavit_trust_party')::INT           AS affid_trust,
         (e.deed_cat = 'family_transfer' AND e.fam_route = 'trust')::INT              AS trust_fam,
         (e.deed_cat = 'same_owner')::INT                                             AS sameown,
         (e.deed_cat = 'same_owner' AND coalesce(e.deed_subcat, '') = 'same_owners')::INT      AS sameown_only,
         (e.deed_cat = 'same_owner' AND coalesce(e.deed_subcat, '') = 'new_person_joins')::INT AS joins,
         (coalesce(e.deed_subcat, '') = 'into_own_trust')::INT                        AS owntrust,
         (e.deed_cat = 'market_sale')::INT                                            AS market,
         (e.deed_cat = 'market_sale'
            OR (e.deed_cat = 'family_transfer'
                AND NOT (e.fam_route IN ('lifetime','unknown')
                         AND coalesce(e.fam_relation, '') = 'spouse')))::INT          AS is_exit
  FROM {ev} e JOIN par p ON p.clip = e.clip
  WHERE e.state = 'CA' AND e.sale_raw >= {start_raw} AND e.ym <= {end_ym}"))

# |- the next market sale of the same home (through the end of the panel) ----
dbExecute(con, glue("
  CREATE TEMP TABLE mk AS
  SELECT e.clip, e.sale_raw AS m_raw, e.ym AS m_ym
  FROM {ev} e JOIN par p ON p.clip = e.clip
  WHERE e.state = 'CA' AND e.deed_cat = 'market_sale' AND e.sale_raw >= {start_raw}"))
last_market_ym <- dbGetQuery(con, "SELECT MAX(m_ym) AS ym FROM mk")$ym
stopifnot(last_market_ym >= 202406L)   # 24 months of follow-up for deeds through June 2022
dbExecute(con, glue("
  CREATE TEMP TABLE ce AS
  SELECT c.*,
         (m.m_ym IS NOT NULL
            AND (m.m_ym // 100 - c.ym // 100) * 12 + (m.m_ym % 100 - c.ym % 100) <= {resale_months})::INT AS sold
  FROM ce0 c ASOF LEFT JOIN mk m ON m.clip = c.clip AND m.m_raw > c.sale_raw"))

# |- coverage: share of California deeds of each type that fall on the risk-set parcels ----
type_sums <- paste(glue("SUM({names(outcomes)}) AS {names(outcomes)}"), collapse = ", ")
heir_sums <- "SUM(affid * sold) AS affid_sold, SUM(affid * (1 - sold)) AS affid_kept,
              SUM(life_ns * sold) AS life_sold, SUM(life_ns * (1 - sold)) AS life_kept,
              SUM(market * sold) AS mk_sold, SUM(market * (1 - sold)) AS mk_kept"
cover_on <- dbGetQuery(con, glue("SELECT ym // 100 AS year, {type_sums} FROM ce GROUP BY 1 ORDER BY 1"))
cover_all <- dbGetQuery(con, glue("
  SELECT sale_year AS year,
         SUM((deed_cat = 'family_transfer' AND fam_route IN ('lifetime','unknown')
              AND coalesce(fam_relation, '') <> 'spouse')::INT) AS life_ns_all,
         SUM((deed_cat = 'family_transfer' AND fam_route = 'death_affidavit')::INT) AS affid_all,
         SUM((deed_cat = 'same_owner')::INT) AS sameown_all,
         SUM((deed_cat = 'market_sale')::INT) AS market_all
  FROM {ev} WHERE state = 'CA' AND sale_year BETWEEN 2017 AND 2023 GROUP BY 1 ORDER BY 1"))
coverage <- left_join(cover_on, cover_all, by = "year") |>
  mutate(across(everything(), as.numeric),
         share_life_ns = life_ns / life_ns_all, share_affid = affid / affid_all,
         share_sameown = sameown / sameown_all, share_market = market / market_all)
stopifnot(nrow(coverage) == 7L, !anyNA(coverage$share_life_ns))
print(as.data.frame(coverage |> select(year, life_ns, life_ns_all, starts_with("share")) |>
                      mutate(across(starts_with("share"), ~ round(.x, 3)))))
write_csv_strict(coverage, path(tables_out_dir, "fw_coverage.csv"))

# |- spells: first exit per parcel; deeds count while the parcel is at risk ----
dbExecute(con, "CREATE TEMP TABLE ex AS
  SELECT clip, MIN(sale_raw) AS exit_raw, MIN(ym) AS exit_ym FROM ce WHERE is_exit = 1 GROUP BY 1")
keys <- "p.fips5, p.bin, p.hox, p.tenure, p.over_cap"
cell_def <- dbGetQuery(con, glue("
  SELECT {keys}, COUNT(*) AS n0, AVG(p.wk) AS wk, AVG(p.wk21) AS wk21 FROM par p GROUP BY ALL")) |>
  as_tibble()
cell_events <- dbGetQuery(con, glue("
  SELECT {keys}, c.ym, {type_sums}, {heir_sums}
  FROM ce c JOIN par p ON p.clip = c.clip LEFT JOIN ex x ON x.clip = c.clip
  WHERE x.exit_raw IS NULL OR c.sale_raw <= x.exit_raw
  GROUP BY ALL")) |> as_tibble()
cell_exits <- dbGetQuery(con, glue("
  SELECT {keys}, x.exit_ym AS ym, COUNT(*) AS exits
  FROM ex x JOIN par p ON p.clip = x.clip GROUP BY ALL")) |> as_tibble()

# |- the deadline by signing day ----
# Deeds signed 1-15 February against 16-28 February, by wedge bin and year, on parcels at
# risk. The new rules began on February 16, 2021; a refinancing cycle has no reason to break
# on that day. Daily counts around the deadline are kept for a figure.
at_risk_sql <- "x.exit_raw IS NULL OR c.sale_raw <= x.exit_raw"
notch_types <- c("life_ns", "life_sur", "spouse", "sameown_only", "joins", "affid")
notch_sums <- paste(glue("SUM({notch_types} * (CAST(c.sale_raw AS BIGINT) % 100 <= 15)::INT) AS {notch_types}_first,
                          SUM({notch_types} * (CAST(c.sale_raw AS BIGINT) % 100 BETWEEN 16 AND 28)::INT) AS {notch_types}_second"),
                    collapse = ", ")
notch <- dbGetQuery(con, glue("
  SELECT p.bin, c.ym // 100 AS year, {notch_sums},
         SUM(p.wk21 * c.life_ns) / NULLIF(SUM(c.life_ns), 0) AS wk21_life
  FROM ce c JOIN par p ON p.clip = c.clip LEFT JOIN ex x ON x.clip = c.clip
  WHERE ({at_risk_sql}) AND c.ym % 100 = 2 AND c.ym // 100 BETWEEN 2018 AND 2022
  GROUP BY ALL ORDER BY 1, 2")) |> as_tibble() |>
  mutate(across(-bin, as.numeric), bin = factor(bin, levels = bin_levels)) |> arrange(bin, year)
write_csv_strict(notch, path(tables_out_dir, "fw_deadline_notch.csv"))
print(as.data.frame(notch |> transmute(bin, year, life_first = life_ns_first, life_second = life_ns_second,
                                       life = round(life_ns_second / life_ns_first, 2),
                                       spouse = round(spouse_second / spouse_first, 2),
                                       same_owners = round(sameown_only_second / sameown_only_first, 2),
                                       joins = round(joins_second / joins_first, 2),
                                       affid = round(affid_second / affid_first, 2))))
daily <- dbGetQuery(con, glue("
  SELECT CAST(c.sale_raw AS BIGINT) AS sale_raw, p.bin, SUM(c.life_ns) AS life_ns, SUM(c.spouse) AS spouse,
         SUM(c.sameown_only) AS sameown_only, SUM(c.joins) AS joins
  FROM ce c JOIN par p ON p.clip = c.clip LEFT JOIN ex x ON x.clip = c.clip
  WHERE ({at_risk_sql}) AND c.sale_raw BETWEEN 20201101 AND 20210430
  GROUP BY ALL ORDER BY 1, 2")) |> as_tibble()
write_csv_strict(daily, path(tables_out_dir, "fw_deadline_daily.csv"))

#============================================#
# 2. Cell x month panel ----
#============================================#

key_cols <- c("fips5", "bin", "hox", "tenure", "over_cap")
months <- sort(unique(cell_events$ym))
stopifnot(length(months) == 84L)

cells <- cell_def |>
  mutate(cell_id = row_number()) |>
  cross_join(tibble(ym = months)) |>
  left_join(cell_events, by = c(key_cols, "ym")) |>
  left_join(cell_exits, by = c(key_cols, "ym")) |>
  mutate(across(c(all_of(all_counts), exits), ~ coalesce(as.numeric(.x), 0))) |>
  arrange(cell_id, ym) |>
  group_by(cell_id) |>
  mutate(at_risk = n0 - lag(cumsum(exits), default = 0)) |>
  ungroup() |>
  mutate(period = factor(period_of(ym), levels = period_levels),
         heir_period = factor(heir_period_of(ym), levels = heir_period_levels),
         bin = factor(bin, levels = bin_levels))

# |- accounting checks ----
stopifnot(all(cells$at_risk >= 0),
          sum(cells$at_risk[cells$ym == min(months)]) == n_par,
          sum(cells$exits) == dbGetQuery(con, "SELECT COUNT(*) AS n FROM ex")$n,
          all(cells$affid == cells$affid_sold + cells$affid_kept),
          all(cells$market == cells$mk_sold + cells$mk_kept))
cells <- filter(cells, at_risk > 0)
arrow::write_parquet(cells, path(data_dir, "fw_wedge_cells.parquet"))
message("Cells: ", n_distinct(cells$cell_id), "; rows: ", fmt_n(nrow(cells)),
        "; lifetime deeds (no spouse document) at risk: ", fmt_n(sum(cells$life_ns)),
        "; affidavits at risk: ", fmt_n(sum(cells$affid)))

# |- raw rates: deeds per 1,000 parcel-years at risk, by wedge bin and period ----
raw_rates <- cells |>
  group_by(bin, period) |>
  summarise(across(all_of(names(outcomes)), ~ 12000 * sum(.x) / sum(at_risk)),
            parcel_months = sum(at_risk), wk = weighted.mean(wk, at_risk),
            wk21 = weighted.mean(wk21, at_risk), .groups = "drop")
write_csv_strict(raw_rates, path(tables_out_dir, "fw_raw_rates.csv"))
print(as.data.frame(raw_rates |> select(bin, period, life_ns, life_sur, spouse, affid, sameown, market) |>
                      mutate(across(where(is.numeric), ~ round(.x, 2)))))

# |- raw shares sold within 24 months, by wedge bin and step 2 period ----
heir_raw <- cells |>
  filter(ym <= heir_end_ym) |>
  group_by(bin, heir_period) |>
  summarise(affid = sum(affid), affid_share_sold = sum(affid_sold) / sum(affid),
            life = sum(life_ns), life_share_sold = sum(life_sold) / sum(life_ns),
            market = sum(market), market_share_sold = sum(mk_sold) / sum(market), .groups = "drop")
write_csv_strict(heir_raw, path(tables_out_dir, "fw_heir_raw.csv"))
print(as.data.frame(heir_raw |> mutate(across(ends_with("share_sold"), ~ round(100 * .x, 1)))))

#============================================#
# 3. Step 1: wedge x period response of deed rates ----
#============================================#

# Poisson rate of each deed type with the parcels at risk as exposure. Cell effects absorb
# the level of each county x wedge bin x occupancy x tenure group; county x month and
# tenure x month effects absorb local housing-market and refinancing cycles that differ
# between long-held homes and recent purchases. The coefficients are the change in the
# rate of a wedge bin relative to the lowest bin, in each period relative to 2018-2019.
fe_rhs <- "cell_id + fips5^ym + tenure^ym"

fit_bins <- function(y, data, pvar = "period") {
  fepois(as.formula(glue("{y} ~ i({pvar}, bin, ref = '{ref_period}', ref2 = '{ref_bin}') | {fe_rhs}")),
         data = data, offset = ~ log(at_risk), cluster = ~fips5, notes = FALSE)
}
fit_linear <- function(y, data, pvar = "period") {
  fepois(as.formula(glue("{y} ~ i({pvar}, wk, ref = '{ref_period}') | {fe_rhs}")),
         data = data, offset = ~ log(at_risk), cluster = ~fips5, notes = FALSE)
}
tidy_fit <- function(m, y, sample) {
  ct <- as.data.frame(coeftable(m))
  tibble(outcome = y, sample = sample, term = rownames(ct), estimate = ct[, 1], se = ct[, 2],
         nobs = nobs(m))
}
split_terms <- function(df, pvar) {
  df |> mutate(period = sub(glue("^{pvar}::([^:]+):.*$"), "\\1", term),
               bin = if_else(grepl(":bin::", term), sub("^.*:bin::(.+)$", "\\1", term), NA_character_))
}

samples <- list(
  all       = cells,
  long_held = filter(cells, tenure == "long_held"),
  hox       = filter(cells, hox == 1),
  nonhox    = filter(cells, hox == 0),
  long_hox    = filter(cells, tenure == "long_held", hox == 1),
  long_nonhox = filter(cells, tenure == "long_held", hox == 0)
)

grid <- expand_grid(y = names(outcomes), s = names(samples))
bins_out <- pmap_dfr(grid, function(y, s) tidy_fit(fit_bins(y, samples[[s]]), y, s)) |>
  split_terms("period")
lin_out  <- pmap_dfr(grid, function(y, s) tidy_fit(fit_linear(y, samples[[s]]), y, s)) |>
  split_terms("period")
stopifnot(all(bins_out$period %in% period_levels), all(bins_out$bin %in% bin_levels),
          all(lin_out$period %in% period_levels))
write_csv_strict(bins_out, path(tables_out_dir, "fw_gradient_bins.csv"))
write_csv_strict(lin_out, path(tables_out_dir, "fw_gradient_linear.csv"))

# |- lifetime deeds against placebo deeds in the same cells ----
# Stack a family outcome and a placebo outcome. Each keeps its own cell, county x month and
# tenure x month effects; wedge bin x period effects are common to both. The coefficients are
# the extra change of the family outcome over the placebo, by wedge bin and period.
fit_stack <- function(y1, y0, data) {
  long <- bind_rows(
    data |> transmute(cell_id, fips5, tenure, ym, bin, period, at_risk, n = .data[[y1]], fam = 1L),
    data |> transmute(cell_id, fips5, tenure, ym, bin, period, at_risk, n = .data[[y0]], fam = 0L)
  ) |>
    mutate(pb = interaction(period, bin, sep = "__", drop = TRUE))
  # the baseline period and the lowest wedge bin are the reference levels
  ref_levels <- levels(long$pb)[startsWith(levels(long$pb), paste0(ref_period, "__")) |
                                  endsWith(levels(long$pb), paste0("__", ref_bin))]
  ref_txt <- paste0("c('", paste(ref_levels, collapse = "', '"), "')")
  fepois(as.formula(glue("n ~ i(pb, fam, ref = {ref_txt}) | ",
                         "cell_id^fam + fips5^ym^fam + tenure^ym^fam + pb")),
         data = long, offset = ~ log(at_risk), cluster = ~fips5, notes = FALSE)
}
stack_grid <- expand_grid(y1 = c("life_ns", "life_sur", "affid"), y0 = c("sameown", "spouse"),
                          s = c("all", "nonhox", "hox", "long_held"))
triple_out <- pmap_dfr(stack_grid, function(y1, y0, s) {
  tidy_fit(fit_stack(y1, y0, samples[[s]]), y1, s) |> mutate(placebo = y0)
}) |>
  mutate(period = sub("^pb::(.+)__(w.+):fam$", "\\1", term),
         bin = sub("^pb::(.+)__(w.+):fam$", "\\2", term))
stopifnot(all(triple_out$period %in% period_levels), all(triple_out$bin %in% bin_levels))
write_csv_strict(triple_out, path(tables_out_dir, "fw_triple_bins.csv"))

#============================================#
# 4. Step 2: homes sold or kept within 24 months of the deed ----
#============================================#

# The rate of at-death transfers splits into homes sold within 24 months and homes kept.
# If heirs who lose the old assessment sell more, the sold rate rises with the wedge after
# the reform and the kept rate falls, with the total unchanged. Market purchases are the
# placebo: the wedge of the seller should not move the buyer's resale.
heir_samples <- lapply(samples[c("all", "long_held", "hox", "nonhox")],
                       function(d) filter(d, ym <= heir_end_ym))
heir_grid <- expand_grid(y = names(heir_outcomes), s = names(heir_samples))
heir_bins <- pmap_dfr(heir_grid, function(y, s) {
  tidy_fit(fit_bins(y, heir_samples[[s]], "heir_period"), y, s)
}) |> split_terms("heir_period")
heir_lin <- pmap_dfr(heir_grid, function(y, s) {
  tidy_fit(fit_linear(y, heir_samples[[s]], "heir_period"), y, s)
}) |> split_terms("heir_period")
stopifnot(all(heir_bins$period %in% heir_period_levels), all(heir_lin$period %in% heir_period_levels))
write_csv_strict(heir_bins, path(tables_out_dir, "fw_heir_bins.csv"))
write_csv_strict(heir_lin, path(tables_out_dir, "fw_heir_linear.csv"))

#============================================#
# 5. Print ----
#============================================#

wide <- function(df, cols) {
  df |> mutate(cell = sprintf("%.4f (%.4f)", estimate, se)) |>
    select(all_of(cols), period, cell) |>
    pivot_wider(names_from = period, values_from = cell) |>
    as.data.frame()
}
message("Step 1, slope per $1,000 of annual wedge:")
print(wide(lin_out, c("outcome", "sample")), row.names = FALSE)
message("Step 1, lifetime deeds against placebo deeds, by wedge bin:")
print(wide(filter(triple_out, sample %in% c("all", "nonhox", "hox")),
           c("outcome", "placebo", "sample", "bin")), row.names = FALSE)
message("Step 2, slope per $1,000 of annual wedge:")
print(wide(heir_lin, c("outcome", "sample")), row.names = FALSE)
message("Step 2, by wedge bin:")
print(wide(filter(heir_bins, sample %in% c("all", "nonhox")), c("outcome", "sample", "bin")),
      row.names = FALSE)

message("Finished 29_framework_gradient at ", Sys.time())
