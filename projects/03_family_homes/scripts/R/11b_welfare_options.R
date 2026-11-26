#======================================================================#
# Purpose : Options and numbers for the Gate 3 welfare decisions. Rebuilds the
#           annual misallocation cost as
#               quantity (parcels/yr) x reallocation share x lambda x loss per parcel
#           over a grid of (i) the population whose wedge is used, (ii) who
#           lives in the home, (iii) how much of the post-reform shortfall is
#           counted, (iv) the reallocation share. Also writes number macros and the
#           appendix grid table (Gate 3: central cell kept, transfer figure dropped).
#           Does NOT replace 11_welfare.R and writes only new welfare_v2_* files.
#
#   Populations (CA parcels whose first exit after January 2017 falls in 2018-19;
#   wedge measured at the January 2017 lien date, as in 04c):
#     (i)   class-based rule: first exit is a family-proxy deed (class family_person,
#           family_estate, family_other) that is not a same-person or entity deed
#           (author's 2026-11-12 family measure, as in 04a); death affidavits and
#           spouses included
#     (ii)  main measure: the first deed that is a market sale or a deed of
#           `bygroup_main` (00_groups.R: proxy without death affidavits plus
#           chain-verified trust-to-heir deeds); the exit must be the main deed
#     (iii) (ii) restricted to lifetime transfers (fam_trigger = 'lifetime')
#   Occupancy of the heir (not observed): mail_eq_situs of the exit deed (buyer's
#   mailing address equals the property address) as a proxy, plus both polar cases.
#     non-occupant heir loses  t (p - A0)
#     occupant heir loses      t max(p - A0 - cap, 0)   [cap 1,000,000 in 2021]
#   The occupant loss is the parcel's wedge times the share of its 2021 gap above
#   the cap.
#
# Name    : Saani Rawat
# Created : 11/11/2026
# Log     : 11/11/2026: created for Gate 3.
#           11/12/2026: grid-table text only (approved proofreading): rho and kappa for the
#           reallocation share and the Harberger factor, Section 4 measure, monthly counterfactual.
# Inputs  : data/derived/03_family_homes/ca_parcel_wedge_v2det.parquet (the
#           deterministic 04a build; falls back to ca_parcel_wedge.parquet; override
#           with env FAM_WEDGE_PARQUET)
#           data/derived/03_family_homes/events.parquet (via events_panel_path)
#           _outputs/tables/bygroup_rush_hole.csv, bygroup_cf_monthly.csv,
#           bygroup_deeds_per_parcel.csv, bygroup_ddd.csv,
#           bygroup_suppl_ca_log_changes.csv, wedge_inherited_homes.csv,
#           welfare_effect_inputs.csv; scripts/R/inputs/welfare_cpi.csv
# Outputs : _outputs/tables/welfare_v2_{provenance,populations,units_quantity,
#           grid,number_sources}.csv, welfare_v2_numbers.tex, TA_welfare_grid.tex
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "00_group_helpers.R"))

set.seed(20261111)

#============================================#
# Settings ----
#============================================#

fam_classes <- family_classes
inherited_window <- as.Date(c("2018-01-01", "2019-12-31"))
window_years <- 2                      # 2018 and 2019
value_cap <- 1e6                       # Prop 19 occupant allowance, 2021 (BOE Pub. 801)
lambda <- 0.5                          # Harberger triangle, as in 11_welfare.R
alphas <- c(0.25, 0.5, 1)              # reallocation share of missing parcels
dpp_years <- 2017:2019                 # within-year deeds per parcel, pre-reform

wedge_parquet <- Sys.getenv("FAM_WEDGE_PARQUET", unset = "")
if (!nzchar(wedge_parquet)) {
  det <- path(data_dir, "ca_parcel_wedge_v2det.parquet")
  wedge_parquet <- if (file_exists(det)) as.character(det) else as.character(path(data_dir, "ca_parcel_wedge.parquet"))
}
stopifnot(file_exists(wedge_parquet))

fmt <- function(x, digits = 0) formatC(x, digits = digits, format = "f", big.mark = ",")
read_result <- function(name) read_csv(path(tables_out_dir, name), show_col_types = FALSE)
save_csv <- function(x, name) write_csv_strict(x, path(tables_out_dir, paste0("welfare_v2_", name, ".csv")))
to_clip <- function(x) as.numeric(as.character(x))

cpi <- read_csv(path(r_dir, "inputs", "welfare_cpi.csv"), show_col_types = FALSE) %>%
  group_by(year) %>% summarise(cpi = round(mean(cpi_u), 3), .groups = "drop")
inflator <- cpi$cpi[cpi$year == 2023] / cpi$cpi[cpi$year == 2017]
stopifnot(abs(inflator - 1.243072780678851) < 1e-9)

#============================================#
# Importing parcel wedges (January 2017 baseline) ----
#============================================#

parcels <- arrow::open_dataset(wedge_parquet) %>%
  filter(cohort == "d2017") %>%
  select(clip, wedge, rate, assessed_d0, market_d0, market_2021, assessed_2021,
         over_cap_2021, tenure, hox, end_type, spell_end) %>%
  collect() %>%
  mutate(clip = to_clip(clip),
         gap_2021 = pmax(market_2021 - assessed_2021, 0),
         above_cap_share = if_else(gap_2021 > 0, pmax(gap_2021 - value_cap, 0) / gap_2021, 0)) %>%
  arrange(clip)
stopifnot(!anyDuplicated(parcels$clip), nrow(parcels) > 7e6)
message("NOTE: ", fmt(nrow(parcels)), " parcels with a January 2017 wedge, from ", wedge_parquet)

#============================================#
# Importing CA deeds 2017-2019 (exit deeds) ----
#============================================#

con <- open_corelogic_duckdb(memory_limit = Sys.getenv("FAM_DUCKDB_MEM", "19GB"), threads = 4L)
ev <- glue("read_parquet('{sql_quote_path(events_panel_path)}')")
main_sql <- bygroup_filter(bygroup_main)
deeds <- dbGetQuery(con, glue("
  SELECT clip, sale_raw, class, doc_type, fam_trigger, mail_eq_situs, fam_relation_nm,
         coalesce({main_sql}, FALSE) AS is_main
  FROM {ev}
  WHERE state = 'CA' AND sale_raw BETWEEN 20170101 AND 20191231
    AND (class = 'market_sale' OR class IN ('{paste(fam_classes, collapse = \"','\")}')
         OR coalesce({main_sql}, FALSE))")) %>%
  mutate(clip = to_clip(clip), sale_raw = as.numeric(sale_raw),
         sale_date = parse_event_dates(sale_raw),
         # 2026-11-12 family measure (00_setup.R): same-person and entity deeds are not family deeds
         is_legacy = class %in% fam_classes &
           !(coalesce(fam_relation_nm, "") %in% family_excluded_relations),
         is_market = class == "market_sale") %>%
  filter(!is.na(sale_date), sale_date >= as.Date("2017-01-01")) %>%
  # total order: ties on a parcel and date are broken by the raw date string, class, doc type,
  # trigger and flag, so the kept deed never depends on file order
  arrange(clip, sale_date, desc(is_market), sale_raw, class, doc_type, fam_trigger, mail_eq_situs)
dbDisconnect(con, shutdown = TRUE)
message("NOTE: ", fmt(nrow(deeds)), " CA deeds 2017-2019 considered")

#============================================#
# First exit under each rule ----
#============================================#

# A spell ends at the first deed that is a market sale or a deed of the rule; a market
# sale wins a same-date collision. The parcel enters the population when that deed is a
# rule deed dated in the 2018-19 window (04c's definition of an inherited home).
first_exit <- function(flag_col) {
  deeds %>%
    filter(is_market | .data[[flag_col]]) %>%
    distinct(clip, .keep_all = TRUE) %>%
    filter(!is_market, between(sale_date, inherited_window[1], inherited_window[2])) %>%
    transmute(clip, exit_date = sale_date, exit_class = class, exit_trigger = fam_trigger,
              exit_doc = doc_type, mail_eq_situs)
}
exit_legacy <- first_exit("is_legacy")
exit_main <- first_exit("is_main")

# (i) is taken from the parcel panel itself (04a's exit), and the occupancy flag of its exit deed is
# attached from the deed with the same parcel, date and class.
flag_lookup <- deeds %>%
  filter(is_legacy) %>%
  distinct(clip, sale_date, class, .keep_all = TRUE) %>%
  select(clip, spell_end = sale_date, end_type = class, mail_eq_situs, fam_trigger)
pop_i <- parcels %>%
  filter(end_type %in% fam_classes, between(spell_end, inherited_window[1], inherited_window[2])) %>%
  left_join(flag_lookup, by = c("clip", "spell_end", "end_type")) %>%
  mutate(population = "i_legacy_family_proxy")
chk_i <- inner_join(parcels %>% select(clip), exit_legacy, by = "clip")
message("NOTE: (i) from parcel panel = ", fmt(nrow(pop_i)), "; rebuilt from deeds = ", fmt(nrow(chk_i)))

pop_main <- parcels %>%
  inner_join(exit_main, by = "clip") %>%
  mutate(fam_trigger = exit_trigger)
pop_ii <- pop_main %>% mutate(population = "ii_main_measure")
pop_iii <- pop_main %>% filter(exit_trigger == "lifetime") %>% mutate(population = "iii_main_lifetime")
pops <- bind_rows(pop_i, pop_ii, pop_iii) %>%
  mutate(w = wedge * inflator,                                  # 2023 dollars
         occ_flag = mail_eq_situs,
         loss_nonocc = w,
         loss_occ = w * above_cap_share,
         loss_proxy = if_else(coalesce(occ_flag, 0L) == 1L, loss_occ, loss_nonocc))
stopifnot(all(pops$w >= 0))

#============================================#
# Wedge by population ----
#============================================#

pop_table <- pops %>%
  group_by(population) %>%
  summarise(
    n_parcels = n(), n_per_year = n() / window_years,
    share_positive_wedge = mean(w > 0),
    mean_wedge_2023 = mean(w), median_wedge_2023 = median(w),
    mean_wedge_positive_2023 = mean(w[w > 0]), median_wedge_positive_2023 = median(w[w > 0]),
    mean_wedge_2017 = mean(wedge), median_wedge_2017 = median(wedge),
    share_wedge_above_cap = sum(loss_occ) / sum(w),
    share_parcels_gap_above_cap = mean(above_cap_share > 0),
    share_occupant_proxy = mean(occ_flag == 1L, na.rm = TRUE),
    share_flag_missing = mean(is.na(occ_flag)),
    mean_loss_nonocc_2023 = mean(loss_nonocc), mean_loss_occ_2023 = mean(loss_occ),
    mean_loss_proxy_2023 = mean(loss_proxy),
    .groups = "drop")
print(pop_table, width = Inf)
save_csv(pop_table, "populations")

#============================================#
# Units and quantity ----
#============================================#

dpp_tab <- read_result("bygroup_deeds_per_parcel.csv") %>%
  filter(region == "CA", group == bygroup_main, period %in% paste0("y", dpp_years))
stopifnot(nrow(dpp_tab) == length(dpp_years), !any(dpp_tab$lookback_incomplete))
dpp <- sum(dpp_tab$deeds) / sum(dpp_tab$parcels)
dpp_other <- read_result("bygroup_deeds_per_parcel.csv") %>%
  filter(region == "CA", group == bygroup_main, period %in% c("rush", "post_year", "base_rush_2019")) %>%
  select(period, deeds_per_parcel)

hole <- read_result("bygroup_rush_hole.csv") %>%
  filter(group == bygroup_main, base_window == "2019", end_label == "main_202406")
stopifnot(nrow(hole) == 1)
hole_years <- hole$n_months_hole / 12

# California main-measure deeds that would be missing if they had moved like California spouse deeds
cfm <- read_result("bygroup_cf_monthly.csv") %>% filter(base_window == "2019" | base_window == 2019)
sp <- cfm %>% filter(group == "spouse_ca") %>% transmute(ym, sp_ratio = ca_actual / ca_base)
mn <- cfm %>% filter(group == bygroup_main) %>% select(ym, ca_actual, ca_base, cf)
bs <- inner_join(mn, sp, by = "ym") %>% filter(ym >= bygroup_windows$hole_start, ym <= bygroup_windows$hole_end_main)
stopifnot(nrow(bs) == hole$n_months_hole)
shortfall_chk <- sum(bs$cf - bs$ca_actual)
stopifnot(abs(shortfall_chk - hole$shortfall) < 1)
beyond_spouse_total <- sum(bs$ca_base * bs$sp_ratio - bs$ca_actual)
# cross-check on the 2022-23 log change, from bygroup_suppl_ca_log_changes.csv
lc <- read_result("bygroup_suppl_ca_log_changes.csv")
lc_main <- lc[lc$group == bygroup_main, ]; lc_sp <- lc[lc$group == "spouse_ca", ]
beyond_spouse_2223_annual <- lc_main$mean_2017_19 * (exp(lc_sp$lchg_vs_2017_19) - exp(lc_main$lchg_vs_2017_19))
ddd <- read_result("bygroup_ddd.csv") %>% filter(group == bygroup_main)
ddd_annual <- lc_main$mean_2017_19 * (1 - exp(ddd$ddd))

# Net of market sales (the triple-difference shortfall): the monthly counterfactual is scaled by the
# California market-sale ratio (actual / counterfactual) before the deficit is summed. Rebuilt here
# from the monthly file and checked against `shortfall_adj` of bygroup_rush_hole.csv (17_bygroup_designs.R).
mkt <- cfm %>% filter(group == "market_sale") %>% transmute(ym, mkt_factor = ca_actual / cf)
bn <- inner_join(mn, mkt, by = "ym")
short_net_mkt <- -sum((bn$ca_actual - bn$cf * bn$mkt_factor)[bn$ym >= bygroup_windows$hole_start &
                                                               bn$ym <= bygroup_windows$hole_end_main])
stopifnot(abs(short_net_mkt - hole$shortfall_adj) < 1)

quantity <- tibble(
  bound = c("zero", "beyond_spouse", "net_of_rush", "net_of_market", "counterfactual"),
  label = c("Zero (decline not attributed to the reform)",
            "Net of California spouse deeds (counterfactual basis)",
            "Net of market sales, less the rush (retiming)",
            "Net of market sales: the triple-difference shortfall (central case)",
            "Counterfactual, not netted of market sales (sensitivity)"),
  deeds_total = c(0, beyond_spouse_total, hole$shortfall_adj - hole$rush_excess_adj,
                  hole$shortfall_adj, hole$shortfall)) %>%
  mutate(deeds_per_year = deeds_total / hole_years,
         parcels_per_year = deeds_per_year / dpp)
units_out <- bind_rows(
  quantity %>% mutate(item = "quantity") %>%
    select(item, name = bound, deeds_total, deeds_per_year, parcels_per_year),
  tibble(item = "units",
         name = c("deeds_per_parcel_CA_main_2017_19", paste0("dpp_", dpp_other$period),
                  "rush_excess_deeds", "rush_excess_net_of_market_deeds", "hole_months", "ddd_log", "ddd_implied_deeds_per_year",
                  "beyond_spouse_2022_23_deeds_per_year", "baseline_main_deeds_2017_19_per_year"),
         deeds_per_year = c(dpp, dpp_other$deeds_per_parcel, hole$rush_excess, hole$rush_excess_adj, hole$n_months_hole,
                            ddd$ddd, ddd_annual, beyond_spouse_2223_annual, lc_main$mean_2017_19)))
print(quantity, width = Inf)
save_csv(units_out, "units_quantity")

#============================================#
# Grid: population x occupancy x quantity x reallocation share ----
#============================================================#

occ_cases <- tribble(~occupancy, ~loss_col,
                     "proxy_mail_eq_situs", "mean_loss_proxy_2023",
                     "all_non_occupants", "mean_loss_nonocc_2023",
                     "all_occupants", "mean_loss_occ_2023")
grid <- expand_grid(population = pop_table$population, occ_cases,
                    quantity %>% select(bound, parcels_per_year), alpha = alphas) %>%
  rowwise() %>%
  mutate(loss_per_parcel_year = pop_table[[loss_col]][pop_table$population == population],
         misallocation_cost_musd = parcels_per_year * alpha * lambda * loss_per_parcel_year / 1e6) %>%
  ungroup() %>%
  select(population, occupancy, bound, alpha, parcels_per_year, loss_per_parcel_year,
         misallocation_cost_musd)
save_csv(grid, "grid")

#============================================#
# Central cell, manuscript macros and appendix table ----
#============================================#

# Gate 3 (author): keep the main-measure central cell; the transfer figure and its bridge are
# dropped from every output of this script.
cen_pop <- "ii_main_measure"; cen_occ <- "proxy_mail_eq_situs"
cen_q <- "net_of_market"; cen_alpha <- 0.5
cell <- function(pop = cen_pop, occ = cen_occ, q = cen_q, a = cen_alpha) {
  v <- grid$misallocation_cost_musd[grid$population == pop & grid$occupancy == occ &
                                      grid$bound == q & abs(grid$alpha - a) < 1e-9]
  stopifnot(length(v) == 1L); v
}
central <- cell(); low_end <- cell(q = "beyond_spouse"); high_end <- cell(a = 1)
pm <- pop_table[pop_table$population == cen_pop, ]
qv <- function(b, col) quantity[[col]][quantity$bound == b]
dollar_m <- function(x) formatC(x, digits = 1, format = "f", big.mark = ",")
macros <- tribble(
  ~macro, ~value, ~source, ~definition,
  "WViLossCentral", dollar_m(central), "welfare_v2_grid.csv",
    "main measure, occupancy proxy, shortfall net of market sales (triple difference), reallocation share 0.5; $M a year, 2023 dollars",
  "WViLossLow", dollar_m(low_end), "welfare_v2_grid.csv",
    "main measure, occupancy proxy, beyond-spouse quantity, alpha 0.5",
  "WViLossHigh", dollar_m(high_end), "welfare_v2_grid.csv",
    "main measure, occupancy proxy, shortfall net of market sales, reallocation share 1",
  "WViLossZero", "0", "welfare_v2_units_quantity.csv", "quantity bound 'zero': loss is zero by construction",
  "WViQuantityZero", "0", "welfare_v2_units_quantity.csv", "parcels a year under the zero bound",
  "WViWedgeMean", fmt(pm$mean_wedge_2023), "welfare_v2_populations.csv",
    "mean_wedge_2023, main measure, zeros included, 2023 dollars",
  "WViWedgeMedian", fmt(pm$median_wedge_2023), "welfare_v2_populations.csv",
    "median_wedge_2023, main measure, zeros included",
  "WViWedgeMedianPos", fmt(pm$median_wedge_positive_2023), "welfare_v2_populations.csv",
    "median_wedge_positive_2023, main measure",
  "WViSharePositive", fmt(100 * pm$share_positive_wedge, 1), "welfare_v2_populations.csv",
    "share_positive_wedge (percent)",
  "WViShareAboveCap", fmt(100 * pm$share_wedge_above_cap, 1), "welfare_v2_populations.csv",
    "share of the wedge above the $1M cap (percent)",
  "WViLossPerParcel", fmt(pm$mean_loss_proxy_2023), "welfare_v2_populations.csv",
    "mean_loss_proxy_2023: mean wedge lost by the heir under the occupancy proxy, before lambda and alpha",
  "WViParcels", fmt(pm$n_parcels), "welfare_v2_populations.csv", "n_parcels, main-measure exits 2018-19",
  "WViParcelsPerYear", fmt(pm$n_per_year), "welfare_v2_populations.csv", "n_per_year",
  "WViShortfallDeeds", fmt(qv("net_of_market", "deeds_total")), "welfare_v2_units_quantity.csv",
    "deeds_total, Mar 2021 - Jun 2024, shortfall NET OF MARKET SALES (triple difference; shortfall_adj in bygroup_rush_hole.csv); central case",
  "WViShortfallDeedsYear", fmt(qv("net_of_market", "deeds_per_year")), "welfare_v2_units_quantity.csv",
    "deeds_per_year = WViShortfallDeeds / (WViHoleMonths / 12)",
  "WViShortfallParcelsYear", fmt(qv("net_of_market", "parcels_per_year")), "welfare_v2_units_quantity.csv",
    "parcels_per_year used in the central cell = WViShortfallDeedsYear / WViDeedsPerParcel",
  "WViShortfallDeedsCF", fmt(qv("counterfactual", "deeds_total")), "welfare_v2_units_quantity.csv",
    "deeds_total, same window, counterfactual NOT netted of market sales (shortfall in bygroup_rush_hole.csv); sensitivity only",
  "WViShortfallParcelsYearCF", fmt(qv("counterfactual", "parcels_per_year")), "welfare_v2_units_quantity.csv",
    "parcels_per_year of the un-netted counterfactual shortfall; sensitivity only",
  "WViBeyondSpouseParcelsYear", fmt(qv("beyond_spouse", "parcels_per_year")), "welfare_v2_units_quantity.csv",
    "parcels_per_year, shortfall net of California spouse deeds (counterfactual basis)",
  "WViNetRushParcelsYear", fmt(qv("net_of_rush", "parcels_per_year")), "welfare_v2_units_quantity.csv",
    "parcels_per_year, shortfall net of market sales less the market-netted rush",
  "WViRushDeeds", fmt(hole$rush_excess), "welfare_v2_units_quantity.csv",
    "rush_excess_deeds (counterfactual, not netted of market sales)",
  "WViRushDeedsNetMarket", fmt(hole$rush_excess_adj), "welfare_v2_units_quantity.csv",
    "rush_excess_net_of_market_deeds (rush_excess_adj)",
  "WViHoleMonths", fmt(hole$n_months_hole), "welfare_v2_units_quantity.csv", "hole_months",
  "WViDeedsPerParcel", fmt(dpp, 3), "welfare_v2_units_quantity.csv",
    "CA main-measure deeds per parcel within year, 2017-2019 pooled",
  "WViOccupancyShare", fmt(100 * pm$share_occupant_proxy, 1), "welfare_v2_populations.csv",
    "share_occupant_proxy: exits whose buyer mailing address equals the property (percent)",
  "WViAlpha", "0.5", "11b_welfare_options.R", "central reallocation share (parameter)",
  "WViAlphaLow", "0.25", "11b_welfare_options.R", "lowest reallocation share tabulated",
  "WViAlphaHigh", "1", "11b_welfare_options.R", "highest reallocation share tabulated",
  "WViHarberger", "0.5", "11b_welfare_options.R", "lambda: average loss is lambda times the wedge",
  "WViValueCap", fmt(value_cap), "11b_welfare_options.R", "Prop 19 occupant allowance, 2021 (dollars)")
num <- function(m) as.numeric(gsub(",", "", macros$value[macros$macro == m]))
arith <- function(loss_parcels_macro, alpha_macro, value_macro) {
  v <- num(loss_parcels_macro) * num(alpha_macro) * num("WViHarberger") * num("WViLossPerParcel") / 1e6
  sprintf("%s x %s x %s x %s / 1e6 = %s x %s x %s x %s / 1e6 = %.2f, shown as %s",
          loss_parcels_macro, alpha_macro, "WViHarberger", "WViLossPerParcel",
          macros$value[macros$macro == loss_parcels_macro], macros$value[macros$macro == alpha_macro],
          macros$value[macros$macro == "WViHarberger"], macros$value[macros$macro == "WViLossPerParcel"],
          v, macros$value[macros$macro == value_macro])
}
macros$comment <- ""
macros$comment[macros$macro == "WViLossCentral"] <-
  paste0("WViShortfallParcelsYear = WViShortfallDeeds / (WViHoleMonths / 12) / WViDeedsPerParcel = ",
         macros$value[macros$macro == "WViShortfallDeeds"], " / (", macros$value[macros$macro == "WViHoleMonths"],
         " / 12) / ", macros$value[macros$macro == "WViDeedsPerParcel"], "; then ",
         arith("WViShortfallParcelsYear", "WViAlpha", "WViLossCentral"))
macros$comment[macros$macro == "WViLossLow"] <-
  arith("WViBeyondSpouseParcelsYear", "WViAlpha", "WViLossLow")
macros$comment[macros$macro == "WViLossHigh"] <-
  arith("WViShortfallParcelsYear", "WViAlphaHigh", "WViLossHigh")
macros$comment[macros$macro == "WViShortfallDeedsCF"] <-
  arith("WViShortfallParcelsYearCF", "WViAlpha", "WViLossCentral") %>% sub(", shown as .*", " (sensitivity row, not the central case)", .)
writeLines(c("% Generated by 11b_welfare_options.R. Do not edit.",
             paste0("\\newcommand{\\", macros$macro, "}{", macros$value, "}")),
           path(tables_out_dir, "welfare_v2_numbers.tex"))
save_csv(macros, "number_sources")

# Appendix table: main-measure population only; rows occupancy case x quantity bound
occ_lab <- c(proxy_mail_eq_situs = "Mailing-address proxy", all_non_occupants = "All heirs non-occupants",
             all_occupants = "All heirs occupants")
q_lab <- c(beyond_spouse = "Net of California spouse deeds (counterfactual basis)",
           net_of_rush = "Net of market sales, less the rush",
           net_of_market = "Net of market sales (triple difference; central)",
           counterfactual = "Counterfactual, not netted (sensitivity)")
g_main <- grid %>% filter(population == cen_pop, parcels_per_year > 0)
tab_rows <- character(0)
for (o in names(occ_lab)) {
  for (qb in names(q_lab)) {
    d <- g_main %>% filter(occupancy == o, bound == qb) %>% arrange(alpha)
    stopifnot(nrow(d) == length(alphas))
    tab_rows <- c(tab_rows, sprintf("%s & %s & %s & %s & %s & %s \\\\",
      if (qb == names(q_lab)[1]) occ_lab[[o]] else "", q_lab[[qb]],
      fmt(d$parcels_per_year[1]), dollar_m(d$misallocation_cost_musd[1]),
      dollar_m(d$misallocation_cost_musd[2]), dollar_m(d$misallocation_cost_musd[3])))
  }
  if (o != tail(names(occ_lab), 1)) tab_rows <- c(tab_rows, "\\addlinespace")
}
ta_notes <- paste0(
  "\\begin{minipage}{0.96\\textwidth}\\vspace{3pt}{\\scriptsize\\textit{Notes:} ",
  "Each cell is parcels a year $\\times\\ \\rho\\ \\times\\ \\kappa\\ \\times$ the mean annual wedge the heir loses, ",
  "with $\\kappa=1/2$. The wedge is that of the ", fmt(pm$n_parcels),
  " California parcels whose first exit in 2018--2019 was a deed in the Section~\\ref{sec:prop19} measure, zeros included (mean \\$",
  fmt(pm$mean_wedge_2023), " in 2023 dollars), converted from deeds to parcels at ", fmt(dpp, 3),
  " deeds per parcel. A non-occupant heir loses the whole wedge; an occupant heir loses only the part of ",
  "the 2021 gap above the \\$1 million allowance (", fmt(100 * pm$share_wedge_above_cap, 1),
  "\\% of the wedge). The proxy treats the heir as an occupant when the buyer's mailing address matches the property (",
  fmt(100 * pm$share_occupant_proxy, 1), "\\% of exits). Missing parcels: the deficit against the ",
  "monthly 2019-scaled counterfactual, March 2021 to June 2024, counted four ways in deeds: net of California spouse deeds (",
  fmt(qv("beyond_spouse", "deeds_total")), "); net of California market sales, which is the triple-difference shortfall and the central case (",
  fmt(qv("net_of_market", "deeds_total")), "); that count less the market-netted rush of ", fmt(hole$rush_excess_adj), " deeds (",
  fmt(qv("net_of_rush", "deeds_total")), "); and, as a sensitivity only, the counterfactual shortfall not netted of market sales (",
  fmt(qv("counterfactual", "deeds_total")), "). Deeds are converted to parcels a year by dividing by 40/12 years and by the deeds per parcel. A zero count of missing parcels gives a zero cost. ",
  "Rows are scenarios, not confidence intervals; $\\rho$ is not identified.}\\end{minipage}\\end{table}")
ta <- c("\\begin{table}[!ht]\\centering",
  "\\caption{Annual misallocation cost by occupancy, quantity and reallocation share (\\$ millions, 2023 dollars)}\\label{tab:welfare_grid}",
  "{\\small\\setlength{\\tabcolsep}{5pt}",
  "\\begin{tabular}{lp{5.6cm}rrrr}\\toprule",
  " & & Parcels & \\multicolumn{3}{c}{Reallocation share $\\rho$} \\\\",
  "\\cmidrule(lr){4-6}",
  "Heir occupancy & Missing parcels counted & a year & 0.25 & 0.50 & 1.00 \\\\\\midrule",
  tab_rows, "\\bottomrule\\end{tabular}}", ta_notes)
writeLines(ta, path(tables_out_dir, "TA_welfare_grid.tex"))
message("NOTE: central cell $", dollar_m(central), "M; range ", dollar_m(low_end), " to ", dollar_m(high_end))

#============================================#
# Provenance ----
#============================================#

save_csv(tibble(
  item = c("bygroup_main", "wedge_parquet", "wedge_parquet_bytes", "events_panel", "inflator_2017_2023",
           "deeds_per_parcel_used", "value_cap", "lambda", "n_pop_i_rebuilt_from_deeds",
           "n_pop_i_from_parcel_panel", "run_time"),
  value = c(bygroup_main, wedge_parquet, as.character(file_info(wedge_parquet)$size), as.character(events_panel_path),
            as.character(inflator), as.character(dpp), as.character(value_cap), as.character(lambda),
            as.character(nrow(chk_i)), as.character(nrow(pop_i)), as.character(Sys.time()))),
  "provenance")
capture.output(sessionInfo(), file = path(logs_dir, "11b_welfare_options_sessionInfo.txt"))
message("11b done: ", nrow(grid), " grid cells; central cell = $", dollar_m(central), "M")
