#======================================================================#
# Purpose : Counterfactuals from the framework estimates (29).
#           (1) No reform: lifetime family deeds that the wedge response
#               added before the deadline and added or removed after it,
#               by wedge bin. Three comparisons (lowest wedge bin only;
#               same-owner retitles as placebo; spouse deeds as placebo)
#               and two reference periods (2018-2019; January-May 2020,
#               the months just before the ballot placement).
#           (2) Where the rush came from: the shortfall of homes passing
#               at death after the deadline, set against the deeds added
#               in the rush.
#           (3) Reform without notice: the wedge carried by the deeds of
#               the rush (the graded part of (1), and the level rush of
#               1-15 February 2021), and the tax the reform collects each
#               year from homes passing at death and kept by the heir.
#           (4) The heir: homes sold within 24 months of a death that the
#               wedge response adds after the reform.
#           The share of at-death transfers reassessed before and after
#           the reform comes from the Los Angeles roll match (08).
# Name    : Saani Rawat
# Created : 11/19/2026
# Log     : 11/19/2026: first version.
# Inputs  : data/derived/03_family_homes/fw_wedge_cells.parquet (29)
#           data/derived/03_family_homes/ca_deed_roll.parquet   (08)
#           data/derived/03_family_homes/events.parquet
#           _outputs/tables/fw_gradient_bins.csv, fw_triple_bins.csv, fw_heir_bins.csv (29)
#           _outputs/tables/fw_deadline_notch.csv (29)
# Outputs : _outputs/tables/fw_cf_reassessed_la.csv    share reassessed, before and after
#           _outputs/tables/fw_cf_lifetime.csv         lifetime deeds added or removed, by bin
#           _outputs/tables/fw_cf_lifetime_summary.csv the same, totals
#           _outputs/tables/fw_cf_displacement.csv     rush against the at-death shortfall
#           _outputs/tables/fw_cf_deadline_rush.csv    deeds signed 1-15 February 2021 above the usual
#           _outputs/tables/fw_cf_revenue.csv          wedge carried by the rush and tax collected
#           _outputs/tables/fw_cf_heir.csv             homes sold by heirs, by bin
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

#============================================#
# Settings ----
#============================================#

# Share of heirs of an owner-occupied home who occupy it themselves. Legislative Analyst's
# Office (2017, "The Property Tax Inheritance Exclusion"): in Los Angeles County about 70
# percent of inherited homes had the homeowner's exemption before the transfer and about 40
# percent after. An assumption, not an estimate.
heir_occupancy <- 0.40 / 0.70
# share of the wedge a family loses under the new rules: all of it when the home was not
# the owner's residence; the non-occupant heirs' share when it was (the $1 million
# allowance is ignored, which understates the loss on the largest wedges)
exposure <- c(nonhox = 1, hox = 1 - heir_occupancy)
high_bins <- c("w5_10k", "w10k_plus")      # wedge of $5,000 a year or more
deadline_raw <- 20210216L                  # first day of the new parent-child rules
la_fips <- "06037"
bin_levels <- c("w0_0.5k", "w0.5_1.5k", "w1.5_3k", "w3_5k", "w5_10k", "w10k_plus")

cells <- arrow::read_parquet(path(data_dir, "fw_wedge_cells.parquet")) |>
  mutate(occ = if_else(hox == 1, "hox", "nonhox"), bin = as.character(bin),
         period = as.character(period), heir_period = as.character(heir_period))
bins_coef   <- read_csv(path(tables_out_dir, "fw_gradient_bins.csv"), show_col_types = FALSE)
triple_coef <- read_csv(path(tables_out_dir, "fw_triple_bins.csv"), show_col_types = FALSE)
heir_coef   <- read_csv(path(tables_out_dir, "fw_heir_bins.csv"), show_col_types = FALSE)

#============================================#
# 1. Share of at-death transfers reassessed (Los Angeles rolls) ----
#============================================#

# A home sold on the market within two years is reassessed by the sale, so those homes are
# set aside. The reform's bite is the rise in the reassessed share among the rest.
con <- open_corelogic_duckdb(memory_limit = "12GB")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
gp <- glue("read_parquet('{sql_quote_path(path(data_dir, 'ca_deed_roll.parquet'))}')")
ev <- glue("read_parquet('{sql_quote_path(events_panel_path)}')")
dbExecute(con, glue("CREATE TEMP TABLE mk AS SELECT clip, sale_raw AS m_raw, sale_year AS m_year
                     FROM {ev} WHERE state = 'CA' AND fips5 = '{la_fips}' AND deed_cat = 'market_sale'"))
reassessed_la <- dbGetQuery(con, glue("
  SELECT CASE WHEN g.route = 'family_lifetime' AND g.spouse_doc = 1 THEN 'lifetime, spouse document'
              WHEN g.route = 'family_lifetime' THEN 'lifetime, other' ELSE g.route END AS route,
         CASE WHEN g.sale_raw < {deadline_raw} THEN 'before' ELSE 'after' END AS era,
         COUNT(*) AS deeds, AVG(g.reassessed_k2) AS reassessed
  FROM {gp} g ASOF LEFT JOIN mk m ON m.clip = g.clip AND m.m_raw > g.sale_raw
  WHERE g.fips5 = '{la_fips}' AND g.market_value > 0 AND g.route <> 'market_sale'
    AND NOT (m.m_year IS NOT NULL AND m.m_year <= g.sale_year + 2)
  GROUP BY ALL ORDER BY 1, 2 DESC")) |> as_tibble()
write_csv_strict(reassessed_la, path(tables_out_dir, "fw_cf_reassessed_la.csv"))
print(as.data.frame(reassessed_la |> mutate(reassessed = round(reassessed, 3))))
share_la <- function(r, e) reassessed_la$reassessed[reassessed_la$route == r & reassessed_la$era == e]
bite <- share_la("family_at_death", "after") - share_la("family_at_death", "before")
stopifnot(length(bite) == 1L, bite > 0, bite < 1)
message("Rise in the reassessed share of at-death transfers (Los Angeles): ",
        sprintf("%.1f", 100 * bite), " points")

#============================================#
# 2. Lifetime deeds the wedge response added or removed ----
#============================================#

# For wedge bin b in period p, N deeds were signed. With coefficient c (the change in the
# rate relative to the comparison and the reference period), the deeds without the wedge
# response are N / exp(c), so the response accounts for N (1 - exp(-c)) deeds.
coef_base <- bind_rows(
  bins_coef |> filter(outcome == "life_ns", sample %in% c("all", names(exposure))) |>
    transmute(comparison = "lowest wedge bin", occ = sample, bin, period, coef = estimate, se),
  triple_coef |> filter(outcome == "life_ns", sample %in% c("all", names(exposure))) |>
    transmute(comparison = paste("placebo:", placebo), occ = sample, bin, period, coef = estimate, se)
)
# the same coefficients measured from January-May 2020 (standard errors ignore the covariance
# between the two periods' coefficients, so they are too large or too small by an unknown amount)
coef_early <- coef_base |>
  inner_join(coef_base |> filter(period == "early2020") |>
               select(comparison, occ, bin, coef0 = coef, se0 = se),
             by = c("comparison", "occ", "bin")) |>
  filter(period != "early2020") |>
  mutate(coef = coef - coef0, se = sqrt(se^2 + se0^2), reference = "Jan-May 2020") |>
  select(-coef0, -se0)
coefs <- bind_rows(mutate(coef_base, reference = "2018-2019"), coef_early)

months_in <- cells |> distinct(ym, period) |> count(period, name = "months")
count_by <- function(d, occ_label) {
  d |> group_by(bin, period) |>
    summarise(deeds = sum(life_ns), wk21 = weighted.mean(wk21, pmax(life_ns, 1e-9)),
              parcel_months = sum(at_risk), .groups = "drop") |>
    mutate(occ = occ_label)
}
life_counts <- bind_rows(count_by(cells, "all"),
                         count_by(filter(cells, occ == "hox"), "hox"),
                         count_by(filter(cells, occ == "nonhox"), "nonhox"))
cf_life <- coefs |>
  inner_join(life_counts, by = c("occ", "bin", "period")) |>
  left_join(months_in, by = "period") |>
  mutate(added = deeds * (1 - exp(-coef)),
         added_se = deeds * exp(-coef) * se,
         # annual property tax the added deeds carry, at 2021 values, for the exposed share
         share_exposed = if_else(occ == "all", NA_real_, exposure[occ]),
         tax_locked = added * 1000 * wk21 * share_exposed,
         tax_locked_se = added_se * 1000 * wk21 * share_exposed)
write_csv_strict(cf_life, path(tables_out_dir, "fw_cf_lifetime.csv"))

life_summary <- cf_life |>
  filter(period %in% c("antic", "post2021", "post2223")) |>
  mutate(wedge = if_else(bin %in% high_bins, "$5,000 or more", "under $5,000")) |>
  group_by(reference, comparison, period, wedge, group = if_else(occ == "all", "all", "by occupancy")) |>
  summarise(deeds = sum(deeds), added = sum(added), added_se = sqrt(sum(added_se^2)),
            tax_locked_m = sum(tax_locked) / 1e6, tax_locked_se_m = sqrt(sum(tax_locked_se^2)) / 1e6,
            months = first(months), .groups = "drop") |>
  mutate(added_per_year = 12 * added / months, added_pct = 100 * added / (deeds - added))
write_csv_strict(life_summary, path(tables_out_dir, "fw_cf_lifetime_summary.csv"))
message("Lifetime deeds the wedge response accounts for (all parcels in the risk set):")
print(as.data.frame(life_summary |> filter(group == "all") |>
                      select(reference, comparison, period, wedge, deeds, added, added_se, added_pct) |>
                      mutate(across(where(is.numeric), ~ round(.x, 1)))))
message("Annual tax locked in, by occupancy group summed ($ million):")
print(as.data.frame(life_summary |> filter(group == "by occupancy", period == "antic") |>
                      select(reference, comparison, wedge, added, tax_locked_m, tax_locked_se_m) |>
                      mutate(across(where(is.numeric), ~ round(.x, 1)))))

#============================================#
# 3. Where the rush came from: the at-death shortfall ----
#============================================#

# If the rush turned transfers that would have happened at death into lifetime deeds, homes
# passing at death (and kept by the heir) fall afterwards on the same parcels. Shortfall =
# N (1 - exp(-c)) with c the wedge bin x period coefficient of the at-death rate.
heir_counts <- cells |>
  group_by(bin, period = heir_period) |>
  summarise(affid_sold = sum(affid_sold), affid_kept = sum(affid_kept), .groups = "drop") |>
  pivot_longer(c(affid_sold, affid_kept), names_to = "outcome", values_to = "homes")
heir_added <- heir_coef |>
  filter(sample == "all", outcome %in% c("affid_sold", "affid_kept")) |>
  select(outcome, bin, period, coef = estimate, se) |>
  inner_join(heir_counts, by = c("outcome", "bin", "period")) |>
  mutate(added = homes * (1 - exp(-coef)), added_se = homes * exp(-coef) * se)
affid_all <- bins_coef |>
  filter(sample == "all", outcome == "affid", period %in% c("post2021", "post2223")) |>
  select(bin, period, coef = estimate, se) |>
  inner_join(cells |> group_by(bin, period) |> summarise(homes = sum(affid), .groups = "drop"),
             by = c("bin", "period")) |>
  mutate(added = homes * (1 - exp(-coef)), added_se = homes * exp(-coef) * se)

rush_by_bin <- cf_life |>
  filter(occ == "all", period == "antic") |>
  mutate(col = paste0("rush_", if_else(reference == "2018-2019", "base_", "early_"),
                      c("lowest wedge bin" = "plain", "placebo: sameown" = "sameown",
                        "placebo: spouse" = "spouse")[comparison])) |>
  select(bin, col, added) |>
  pivot_wider(names_from = col, values_from = added)
displacement <- rush_by_bin |>
  left_join(heir_added |> filter(outcome == "affid_kept", period %in% c("buffer", "post")) |>
              group_by(bin) |> summarise(kept_at_death_mar2021_jun2022 = sum(added)), by = "bin") |>
  left_join(heir_added |> filter(outcome == "affid_sold", period %in% c("buffer", "post")) |>
              group_by(bin) |> summarise(sold_at_death_mar2021_jun2022 = sum(added)), by = "bin") |>
  left_join(affid_all |> group_by(bin) |> summarise(at_death_mar2021_dec2023 = sum(added)), by = "bin") |>
  mutate(bin = factor(bin, levels = bin_levels)) |> arrange(bin)
write_csv_strict(displacement, path(tables_out_dir, "fw_cf_displacement.csv"))
message("Deeds added in the rush against the change in homes passing at death afterwards:")
print(as.data.frame(displacement |> mutate(across(where(is.numeric), ~ round(.x)))))

#============================================#
# 4. Reform without notice: tax locked in against tax collected ----
#============================================#

# Each year's homes passing at death carry a wedge. A home the heir sells is reassessed
# under either rule, so the reform's new tax comes from homes the heir keeps: their wedge
# times the rise in the share reassessed (Los Angeles, applied to the state; the assessor's
# backlog, spouses and deaths before the deadline are all in the after period, and a share
# of homes is not a share of wedge when the $1 million allowance binds). Full repeal would
# reach every kept home not already reassessed, an upper bound that still includes spouses.
# These are annual flows per yearly cohort of deaths. The rush is different in kind: a
# one-time cohort whose homes would otherwise have been reassessed at the parent's death,
# so its wedge is reported on its own and not as a ratio to the annual flow.
kept_share <- cells |>
  filter(heir_period == "base") |>
  group_by(bin) |> summarise(kept = sum(affid_kept) / sum(affid), .groups = "drop")
death_flow <- cells |>
  filter(period == "post2223") |>
  group_by(bin) |>
  summarise(affid_per_year = sum(affid) / 2, wk21 = weighted.mean(wk21, pmax(affid, 1e-9)),
            .groups = "drop") |>
  left_join(kept_share, by = "bin") |>
  mutate(wedge_per_year_m = affid_per_year * 1000 * wk21 / 1e6,
         wedge_kept_m = wedge_per_year_m * kept)
wedge_at_death_m <- sum(death_flow$wedge_per_year_m)
wedge_kept_m <- sum(death_flow$wedge_kept_m)

# |- the deadline rush in levels: deeds signed 1-15 February 2021 above the same days of other years ----
# Expected first-half deeds = second-half deeds of 2021 x the usual first-to-second-half
# ratio (2018-2020 and 2022). The second half of February 2021 may itself be low after the
# rush, which overstates the excess; the 2019-2020 first-half average is the other benchmark.
notch <- read_csv(path(tables_out_dir, "fw_deadline_notch.csv"), show_col_types = FALSE)
notch_rush <- notch |>
  group_by(bin) |>
  summarise(first_2021 = life_ns_first[year == 2021], second_2021 = life_ns_second[year == 2021],
            usual_ratio = sum(life_ns_first[year != 2021]) / sum(life_ns_second[year != 2021]),
            first_2019_2020 = mean(life_ns_first[year %in% c(2019, 2020)]),
            wk21 = wk21_life[year == 2021], .groups = "drop") |>
  mutate(excess_vs_second_half = first_2021 - second_2021 * usual_ratio,
         excess_vs_2019_2020 = first_2021 - first_2019_2020,
         times_usual = first_2021 / first_2019_2020,
         wedge_m = excess_vs_2019_2020 * 1000 * wk21 / 1e6,
         bin = factor(bin, levels = bin_levels)) |>
  arrange(bin)
write_csv_strict(notch_rush, path(tables_out_dir, "fw_cf_deadline_rush.csv"))
message("Lifetime deeds signed 1-15 February 2021 against the usual, by wedge bin (parcels at risk):")
print(as.data.frame(notch_rush |> mutate(across(where(is.numeric), ~ round(.x, 2)))))

graded <- life_summary |>
  filter(group == "by occupancy", period == "antic", wedge == "$5,000 or more", reference == "2018-2019")
revenue <- tibble(
  item = c("Homes passing at death a year (risk set, 2022-2023)",
           "Annual wedge those homes carry ($ million)",
           "Annual wedge of the homes heirs keep ($ million)",
           "New tax a year from the reform, per yearly cohort: estimate ($ million)",
           "New tax a year under full repeal, per yearly cohort: upper bound ($ million)",
           "Extra lifetime deeds, 1-15 February 2021, above 2019-2020 (all wedge bins)",
           "Annual wedge those deeds carry ($ million)",
           paste0("Graded rush, wedge of $5,000 or more, June 2020-February 2021: deeds (",
                  graded$comparison, ")"),
           paste0("Annual wedge of the graded rush for the exposed share ($ million; ",
                  graded$comparison, ")")),
  value = c(sum(death_flow$affid_per_year), wedge_at_death_m, wedge_kept_m, wedge_kept_m * bite,
            wedge_kept_m * (1 - share_la("family_at_death", "before")),
            sum(notch_rush$excess_vs_2019_2020), sum(notch_rush$wedge_m),
            graded$added, graded$tax_locked_m)
)
write_csv_strict(revenue, path(tables_out_dir, "fw_cf_revenue.csv"))
print(as.data.frame(revenue |> mutate(value = round(value, 1))))

#============================================#
# 5. The heir: homes sold within 24 months of a death ----
#============================================#

cf_heir <- heir_added |>
  filter(period == "post") |>
  # the post period is September 2021 to June 2022: ten months
  mutate(added_per_year = added * 12 / 10, bin = factor(bin, levels = bin_levels)) |>
  arrange(outcome, bin)
write_csv_strict(cf_heir, path(tables_out_dir, "fw_cf_heir.csv"))
print(as.data.frame(cf_heir |> select(outcome, bin, homes, coef, se, added, added_se, added_per_year) |>
                      mutate(across(where(is.numeric), ~ round(.x, 3)))))

message("Finished 30_framework_counterfactuals at ", Sys.time())
