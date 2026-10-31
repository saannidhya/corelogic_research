#======================================================================#
# Purpose : Manuscript tables and number macros for the parcel tax wedge
#           built by 04a. Summarizes each term of w = t * max(p - A, 0) for
#           (i) all California SFR/condo parcels at the January 2019 lien
#           date and (ii) inherited homes, the parcels whose first ownership
#           exit after January 2017 was a family transfer in 2018-2019
#           (before the reform reached the ballot). The inherited-home
#           wedge feeds the welfare calibration in 11_welfare.R.
# Name    : Saani Rawat
# Created : 10/22/2026
# Log     : 10/22/2026: created (component table, AVM fit and tenure
#           appendix tables, wedge macros, inherited-home wedge CSV).
# Inputs  : data/derived/03_family_homes/ca_parcel_wedge.parquet (04a)
#           _outputs/tables/wedge_avm_holdout_fit.csv, wedge_linkage.csv,
#           wedge_audit_baseline_summary.csv, wedge_avm_vs_repeat_sales.csv,
#           wedge_lao_benchmark_la_longheld.csv (04a)
#           scripts/R/inputs/welfare_cpi.csv
# Outputs : manuscript/tables/T9_wedge_components.tex
#           manuscript/tables/TA_wedge_avm_fit.tex
#           manuscript/tables/TA_wedge_tenure.tex
#           manuscript/tables/wedge_numbers.tex
#           _outputs/tables/wedge_inherited_homes.csv
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

# Settings ----
fam_classes <- c("family_person", "family_other", "family_estate")
population_cohort <- "d2019"   # matches the distribution figure (04b)
inherited_cohort <- "d2017"    # baseline before the 2018-2019 transfers
inherited_window <- as.Date(c("2018-01-01", "2019-12-31"))
wedge_columns <- c("cohort", "rate", "assessed_d0", "market_d0", "wedge", "tenure", "hox",
                   "over_cap_2021", "base_source", "avm_model", "end_type", "spell_end")

fmt <- function(x, digits = 0) formatC(x, digits = digits, format = "f", big.mark = ",")
read_result <- function(name) read_csv(path(tables_out_dir, name), show_col_types = FALSE)
macro <- function(name, value) paste0("\\newcommand{\\", name, "}{", value, "}")

#============================================#
# Importing the parcel wedge panel ----
#============================================#

wedge_path <- path(data_dir, "ca_parcel_wedge.parquet")
stopifnot(file_exists(wedge_path))
parcels <- arrow::open_dataset(wedge_path) %>%
  filter(cohort %in% c(population_cohort, inherited_cohort)) %>%
  select(all_of(wedge_columns)) %>%
  collect() %>%
  mutate(gap = pmax(market_d0 - assessed_d0, 0))

population <- parcels %>% filter(cohort == population_cohort)
inherited <- parcels %>%
  filter(cohort == inherited_cohort, end_type %in% fam_classes,
         between(spell_end, inherited_window[1], inherited_window[2]))
rm(parcels); gc(verbose = FALSE)
stopifnot(nrow(population) > 7e6, nrow(inherited) > 2e5)
message("NOTE: ", fmt(nrow(population)), " parcels (2019); ",
        fmt(nrow(inherited)), " inherited homes (2018-2019 family transfers)")

# CPI-U annual averages convert the 2017 baseline to 2023 dollars (as 11_welfare)
cpi_annual <- read_csv(path(r_dir, "inputs", "welfare_cpi.csv"), show_col_types = FALSE) %>%
  group_by(year) %>%
  summarise(cpi = round(mean(cpi_u), 3), .groups = "drop")
inflator_2017_2023 <- cpi_annual$cpi[cpi_annual$year == 2023] / cpi_annual$cpi[cpi_annual$year == 2017]

#============================================#
# Summarizing each term of the wedge ----
#============================================#

summarise_terms <- function(df) {
  df %>%
    summarise(
      n = n(),
      rate_median = median(rate), rate_mean = mean(rate),
      assessed_median = median(assessed_d0), assessed_mean = mean(assessed_d0),
      market_median = median(market_d0), market_mean = mean(market_d0),
      gap_median = median(gap), gap_mean = mean(gap),
      wedge_median = median(wedge), wedge_mean = mean(wedge),
      wedge_p90 = unname(quantile(wedge, 0.9)),
      share_zero = mean(wedge == 0),
      share_long_held = mean(tenure == "long_held"),
      share_hox = mean(hox == 1),
      share_over_cap = mean(over_cap_2021 == 1)
    )
}

term_summary <- bind_rows(
  population %>% summarise_terms() %>% mutate(sample = "all_2019"),
  population %>% filter(wedge > 0) %>% summarise_terms() %>% mutate(sample = "positive_2019"),
  inherited %>% summarise_terms() %>% mutate(sample = "inherited_2017")
)
print(term_summary, width = Inf)

# |- Inherited-home wedge for the welfare calibration ----
inherited_terms <- term_summary %>% filter(sample == "inherited_2017")
inherited_out <- tibble(
  n_inherited = inherited_terms$n,
  n_inherited_per_year = inherited_terms$n / 2,
  median_wedge_2017usd = inherited_terms$wedge_median,
  mean_wedge_2017usd = inherited_terms$wedge_mean,
  p90_wedge_2017usd = inherited_terms$wedge_p90,
  inflator_2017_2023 = inflator_2017_2023,
  median_wedge_2023usd = inherited_terms$wedge_median * inflator_2017_2023,
  mean_wedge_2023usd = inherited_terms$wedge_mean * inflator_2017_2023,
  annual_wedge_flow_2023usd = sum(inherited$wedge) / 2 * inflator_2017_2023
)
write_csv_strict(inherited_out, path(tables_out_dir, "wedge_inherited_homes.csv"))

#============================================#
# Writing the component table ----
#============================================#

cell <- function(x, scale = 1, digits = 0) fmt(x / scale, digits)
term_row <- function(label, stat, scale = 1, digits = 0) {
  vals <- unlist(lapply(c("all_2019", "positive_2019", "inherited_2017"), function(s) {
    r <- term_summary %>% filter(sample == s)
    c(cell(r[[paste0(stat, "_median")]], scale, digits), cell(r[[paste0(stat, "_mean")]], scale, digits))
  }))
  paste0(label, " & ", paste(vals, collapse = " & "), " \\\\")
}
share_row <- function(label, stat) {
  vals <- map_chr(c("all_2019", "positive_2019", "inherited_2017"),
                  ~ fmt(100 * term_summary[[stat]][term_summary$sample == .x], 1))
  paste0(label, " & \\multicolumn{2}{c}{", vals[1], "} & \\multicolumn{2}{c}{", vals[2],
         "} & \\multicolumn{2}{c}{", vals[3], "} \\\\")
}

t9 <- c(
  "\\begin{table}[!ht]\\centering",
  "\\caption{Components of the estimated annual tax wedge}\\label{tab:wedge_components}",
  "\\begin{adjustbox}{max width=\\textwidth}{\\small\\setlength{\\tabcolsep}{5pt}",
  "\\begin{tabular}{lrrrrrr}\\toprule",
  " & \\multicolumn{2}{c}{All parcels} & \\multicolumn{2}{c}{Positive wedge} & \\multicolumn{2}{c}{Inherited homes} \\\\",
  " & \\multicolumn{2}{c}{(January 2019)} & \\multicolumn{2}{c}{(January 2019)} & \\multicolumn{2}{c}{(2018--2019 transfers)} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}",
  " & Median & Mean & Median & Mean & Median & Mean \\\\\\midrule",
  term_row("Effective tax rate $\\hat t$ (\\%)", "rate", 0.01, 2),
  term_row("Assessment base $\\hat A$ (\\$000)", "assessed", 1000),
  term_row("Market value $\\hat p$ (\\$000)", "market", 1000),
  term_row("Valuation gap $\\max(\\hat p-\\hat A,0)$ (\\$000)", "gap", 1000, 1),
  term_row("Annual wedge $\\hat w$ (\\$)", "wedge"),
  "\\midrule",
  share_row("Zero wedge (\\%)", "share_zero"),
  share_row("No market sale since 2007 (\\%)", "share_long_held"),
  share_row("Historical homeowner exemption (\\%)", "share_hox"),
  share_row("Gap above the \\$1 million allowance in 2021 (\\%)", "share_over_cap"),
  paste0("Parcels & \\multicolumn{2}{c}{", fmt(term_summary$n[1]), "} & \\multicolumn{2}{c}{",
         fmt(term_summary$n[2]), "} & \\multicolumn{2}{c}{", fmt(term_summary$n[3]), "} \\\\"),
  "\\bottomrule\\end{tabular}}\\end{adjustbox}",
  paste0("\\begin{minipage}{0.96\\textwidth}\\vspace{3pt}{\\scriptsize\\textit{Notes:} ",
         "California single-family and condominium parcels on the 2008--2009 roll. ",
         "$\\hat w=\\hat t\\max(\\hat p-\\hat A,0)$ in nominal dollars of the baseline year. ",
         "All-parcel and positive-wedge columns use the January 2019 lien date. Inherited homes ",
         "are parcels whose first ownership change after January 2017 was a family transfer ",
         "recorded in 2018--2019; their terms are measured at the January 2017 lien date, before ",
         "Proposition~19 reached the ballot. Medians are taken term by term, so the median wedge ",
         "is not the product of the median rate and median gap. Allowance share: 2021 gap ",
         "between projected market value and factored base above \\$1 million.}\\end{minipage}",
         "\\end{table}")
)
writeLines(t9, path(tables_dir, "T9_wedge_components.tex"))
message("Wrote T9_wedge_components")

#============================================#
# Writing the appendix tables ----
#============================================#

# |- AVM holdout fit and coverage ----
avm_fit <- read_result("wedge_avm_holdout_fit.csv")
avm_coverage <- count(population, avm_model) %>% mutate(share = n / sum(n))
avm_rows <- avm_fit %>%
  filter(group == "all" | group == "avm_model") %>%
  mutate(level = if_else(group == "all", "all", level),
         label = recode(level, all = "All holdout sales",
                        full = "Full hedonic model",
                        nosqft = "Without square footage",
                        county = "County $\\times$ type fallback")) %>%
  left_join(avm_coverage %>% select(level = avm_model, share), by = "level") %>%
  mutate(share = if_else(level == "all", 1, share),
         row = sprintf("%s & %s & %s & %.3f & %.1f & %.1f & %.1f & %.1f \\\\",
                       label, if_else(is.na(n_train), "", fmt(n_train)), fmt(n),
                       r2_log, 100 * median_ape, 100 * share_within_10pct,
                       100 * share_within_20pct, 100 * share))
avm_rows <- avm_rows[match(c("all", "full", "nosqft", "county"), avm_rows$level), ]

ta_avm <- c(
  "\\begin{table}[!ht]\\centering",
  "\\caption{Hedonic market-value model: holdout fit and coverage}\\label{tab:wedge_avm}",
  "\\begin{adjustbox}{max width=\\textwidth}{\\small\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lrrrrrrr}\\toprule",
  " & Training & Holdout & $R^2$ & Median abs. & Within & Within & Parcels \\\\",
  "Model & sales & sales & (log price) & error (\\%) & 10\\% & 20\\% & valued (\\%) \\\\\\midrule",
  avm_rows$row,
  "\\bottomrule\\end{tabular}}\\end{adjustbox}",
  paste0("\\begin{minipage}{0.96\\textwidth}\\vspace{3pt}{\\scriptsize\\textit{Notes:} ",
         "Arm's-length, non-distressed California sales of single-family and condominium ",
         "parcels, 2016--2017, priced \\$30,000--\\$20 million. A random 20 percent of sales is ",
         "held out. Each parcel is valued with the richest model that yields a prediction. ",
         "Errors compare predicted with recorded prices in the sale month; the last column is ",
         "the share of the January 2019 parcel universe valued by each model.}\\end{minipage}",
         "\\end{table}")
)
writeLines(ta_avm, path(tables_dir, "TA_wedge_avm_fit.tex"))

# |- Wedge by ownership history and homeowner exemption ----
tenure_labels <- c(long_held = "No market sale since 2007",
                   bought_2007_2012 = "Last market sale 2007--2012",
                   bought_2013_plus = "Last market sale 2013--2018")
tenure_tab <- population %>%
  group_by(tenure, hox) %>%
  summarise(n = n(), share_zero = mean(wedge == 0),
            median_pos = median(wedge[wedge > 0]),
            p25_pos = unname(quantile(wedge[wedge > 0], 0.25)),
            p75_pos = unname(quantile(wedge[wedge > 0], 0.75)),
            ratio = median(assessed_d0 / market_d0),
            over_cap = mean(over_cap_2021 == 1), .groups = "drop") %>%
  mutate(tenure = factor(tenure, levels = names(tenure_labels))) %>%
  arrange(tenure, desc(hox)) %>%
  mutate(row = sprintf("%s & %s & %s & %.1f & %s & %s--%s & %.2f & %.1f \\\\",
                       tenure_labels[as.character(tenure)], if_else(hox == 1, "Yes", "No"),
                       fmt(n), 100 * share_zero, fmt(median_pos), fmt(p25_pos), fmt(p75_pos),
                       ratio, 100 * over_cap))
ta_tenure <- c(
  "\\begin{table}[!ht]\\centering",
  "\\caption{Tax wedge by ownership history and homeowner exemption, January 2019}\\label{tab:wedge_tenure}",
  "\\begin{adjustbox}{max width=\\textwidth}{\\small\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{llrrrrrr}\\toprule",
  " & Homeowner & & Zero & \\multicolumn{2}{c}{Positive wedge (\\$)} & Median & Over \\$1M \\\\",
  "Ownership history & exemption & Parcels & wedge (\\%) & Median & IQR & $\\hat A/\\hat p$ & allowance (\\%) \\\\\\midrule",
  tenure_tab$row,
  "\\bottomrule\\end{tabular}}\\end{adjustbox}",
  paste0("\\begin{minipage}{0.96\\textwidth}\\vspace{3pt}{\\scriptsize\\textit{Notes:} ",
         "Ownership history uses market sales observed from 2007; homeowner exemption is the ",
         "2008--2009 roll indicator, a proxy for owner occupancy rather than verified heir ",
         "eligibility. Nominal 2019 dollars. The last column is the share whose projected 2021 ",
         "gap exceeds the \\$1 million Proposition~19 allowance for occupant heirs.}\\end{minipage}",
         "\\end{table}")
)
writeLines(ta_tenure, path(tables_dir, "TA_wedge_tenure.tex"))

#============================================#
# Writing number macros ----
#============================================#

audit <- read_result("wedge_audit_baseline_summary.csv") %>% filter(cohort == population_cohort)
rs <- read_result("wedge_avm_vs_repeat_sales.csv") %>% filter(cohort == population_cohort)
lao <- read_result("wedge_lao_benchmark_la_longheld.csv")
linkage <- read_result("wedge_linkage.csv")
avm_all <- avm_fit %>% filter(group == "all")
pos <- term_summary %>% filter(sample == "positive_2019")
all19 <- term_summary %>% filter(sample == "all_2019")
lh_hox <- tenure_tab %>% filter(tenure == "long_held", hox == 1)
recent_hox <- tenure_tab %>% filter(tenure == "bought_2013_plus", hox == 1)
share_model <- function(m) 100 * avm_coverage$share[avm_coverage$avm_model == m]

writeLines(c(
  "% Generated by 04c_wedge_tables.R; do not edit by hand.",
  macro("WedgeParcels", fmt(all19$n)),
  macro("WedgePositiveParcels", fmt(pos$n)),
  macro("WedgeShareZero", fmt(100 * all19$share_zero, 1)),
  macro("WedgeMedianPos", fmt(pos$wedge_median)),
  macro("WedgeMeanPos", fmt(pos$wedge_mean)),
  macro("WedgeMedianAll", fmt(all19$wedge_median)),
  macro("WedgeMeanAll", fmt(all19$wedge_mean)),
  macro("WedgeMedianRate", fmt(100 * all19$rate_median, 2)),
  macro("WedgeMedianGapPos", fmt(pos$gap_median)),
  macro("WedgeRateLowerShare", fmt(100 * audit$share_rate_lower_bound, 1)),
  macro("WedgeRateUpperShare", fmt(100 * audit$share_rate_upper_bound, 1)),
  macro("WedgeShareRollBase", fmt(100 * audit$share_roll_base, 1)),
  macro("WedgeShareSaleReset", fmt(100 * audit$share_sale_reset, 1)),
  macro("WedgeAVMHoldout", fmt(avm_all$n)),
  macro("WedgeAVMRsq", fmt(avm_all$r2_log, 2)),
  macro("WedgeAVMMedianAPE", fmt(100 * avm_all$median_ape, 1)),
  macro("WedgeAVMWithinTwenty", fmt(100 * avm_all$share_within_20pct, 0)),
  macro("WedgeAVMShareFull", fmt(share_model("full"), 1)),
  macro("WedgeRSParcels", fmt(rs$n)),
  macro("WedgeRSCorValue", fmt(rs$cor_log_value, 2)),
  macro("WedgeRSCorWedge", fmt(rs$cor_wedge, 2)),
  macro("WedgeLAOMedian", fmt(lao$median_wedge)),
  macro("WedgeLAOMean", fmt(lao$mean_wedge)),
  macro("WedgeLAOParcels", fmt(lao$n)),
  macro("WedgeFamLinkage", fmt(100 * linkage$share_fam_events_2017_2024_in_universe, 1)),
  macro("WedgeLongHeldHOXMedian", fmt(lh_hox$median_pos)),
  macro("WedgeLongHeldHOXOverCap", fmt(100 * lh_hox$over_cap, 1)),
  macro("WedgeRecentHOXMedian", fmt(recent_hox$median_pos)),
  macro("WedgeRecentHOXZero", fmt(100 * recent_hox$share_zero, 1)),
  macro("WedgeInheritedN", fmt(inherited_out$n_inherited)),
  macro("WedgeInheritedPerYear", fmt(inherited_out$n_inherited_per_year)),
  macro("WedgeInheritedMedianNominal", fmt(inherited_out$median_wedge_2017usd)),
  macro("WedgeInheritedMeanNominal", fmt(inherited_out$mean_wedge_2017usd)),
  macro("WedgeInheritedMedian", fmt(inherited_out$median_wedge_2023usd)),
  macro("WedgeInheritedMean", fmt(inherited_out$mean_wedge_2023usd)),
  macro("WedgeInheritedFlowM", fmt(inherited_out$annual_wedge_flow_2023usd / 1e6)),
  macro("WedgeInheritedShareZero", fmt(100 * inherited_terms$share_zero, 1)),
  macro("WedgeInheritedShareLongHeld", fmt(100 * inherited_terms$share_long_held, 1))
), path(tables_dir, "wedge_numbers.tex"))
message("Finished 04c_wedge_tables at ", Sys.time())
