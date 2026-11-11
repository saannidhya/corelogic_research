# ============================================================
# Purpose : Export verified descriptive and policy numbers for manuscript prose
# Name    : Saani Rawat
# Created : 11/04/2026
# Log     : 11/04/2026: link revised prose to the wider-definition refresh.
# Inputs  : Saved aggregate CSVs from 02-04, 07, 09, 09b, 13, and 14
# Outputs : manuscript/tables/refresh_numbers.tex; refresh_number_sources.csv
# ============================================================
# 11/04/2026: removed state-placebo inference and synchronized current outputs.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

read_result <- function(name) {
  read_csv(path(tables_out_dir, paste0(name, ".csv")), show_col_types = FALSE)
}
number_rows <- list()
add_number <- function(name, value, source, digits = 1L) {
  stopifnot(length(value) == 1L, is.finite(value))
  if (name %in% names(number_rows)) stop("Duplicate manuscript macro: ", name)
  number_rows[[name]] <<- tibble(
    macro = name, value = as.numeric(value), source = source,
    formatted = formatC(value, format = "f", digits = digits, big.mark = ",")
  )
}

# ---- Policy response and comparison fit ----
bunch <- read_result("prop19_bunching_summary")
add_number("PolicyAnticPercent", bunch$excess_pct, "prop19_bunching_summary")
add_number("PolicyAnticDeeds", bunch$excess_antic_window, "prop19_bunching_summary", 0)
add_number("PolicyPostMissing", bunch$missing_post_window, "prop19_bunching_summary", 0)
add_number("PolicyRetimingPercent", 100 * bunch$excess_antic_window /
             bunch$missing_post_window, "prop19_bunching_summary")
cf <- read_result("prop19_ca_counterfactual_monthly")
pre <- cf %>% filter(ym <= prop19_pre_end_ym)
early <- cf %>% filter(between(ym, 202001L, 202005L))
add_number("PolicyCFMAE", 100 * mean(abs(pre$excess / pre$ca_cf)),
           "prop19_ca_counterfactual_monthly")
add_number("PolicyCFRMSPE", 100 * sqrt(mean((pre$excess / pre$ca_cf)^2)),
           "prop19_ca_counterfactual_monthly")
add_number("PolicyEarlyExcess", 100 * sum(early$excess) / sum(early$ca_cf),
           "prop19_ca_counterfactual_monthly")
did <- read_result("prop19_did_volume")
ddd <- read_result("prop19_ddd_volume")
stopifnot(nrow(ddd) == 1L)
add_number("PolicyFamilyLog", did$coef[did$model == "fam"], "prop19_did_volume", 3)
add_number("PolicyMarketLog", did$coef[did$model == "market_placebo"], "prop19_did_volume", 3)
add_number("PolicyNetLog", ddd$estimate, "prop19_ddd_volume", 3)
scm <- read_result("prop19_scm_summary")
retiming <- read_result("prop19_retiming_summary") %>%
  mutate(scm_gap_2022_2023_pct = scm$gap_ss_pct)
write_csv_strict(retiming, path(tables_out_dir, "prop19_retiming_summary.csv"))
add_number("PolicySCMDecline", -scm$gap_ss_pct, "prop19_scm_summary")
add_number("PolicySCMFit", 100 * scm$ca_pre_rmspe, "prop19_scm_summary")
scm_pc <- read_result("prop19_scm_pc_summary")
add_number("PolicySCMPCDecline", -scm_pc$gap_ss_pct, "prop19_scm_pc_summary")
add_number("PolicySCMPCFit", 100 * scm_pc$ca_pre_rmspe, "prop19_scm_pc_summary")
es <- read_result("prop19_event_study_monthly")
add_number("PolicyESEarly", mean(es$estimate[es$ym <= 202005]),
           "prop19_event_study_monthly", 3)
add_number("PolicyESDeadline", es$estimate[es$ym == 202102],
           "prop19_event_study_monthly", 3)
add_number("PolicyESPost", mean(es$estimate[es$ym >= 202201]),
           "prop19_event_study_monthly", 3)
selection <- readRDS(path(out_dir, "prop19_ddd_hazard.rds"))
add_number("SelectionFamilyPP", 100 * fixest::coeftable(selection$fam)["ca:post", "Estimate"],
           "prop19_ddd_hazard.rds")
add_number("SelectionMarketPP", 100 * fixest::coeftable(selection$market_placebo)["ca:post", "Estimate"],
           "prop19_ddd_hazard.rds")
absentee <- read_result("prop19_absentee_did")
add_number("SelectionAbsenteePP", 100 * absentee$coef, "prop19_absentee_did")
trust <- read_result("trust_sub_did")
for (outcome in c("n_fam_plus_out", "n_fam_plus_trust")) {
  row <- trust %>% filter(.data$outcome == .env$outcome)
  prefix <- if (outcome == "n_fam_plus_out") "TrustDistribution" else "TrustAll"
  add_number(paste0(prefix, "Log"), row$ddd, "trust_sub_did", 3)
}

# ---- Property profiles, with their explicitly restricted denominators ----
age <- read_result("fact_age_profile")
for (bin in c("10-29", "30-49", "50-74")) {
  row <- age %>% filter(age_bin == bin)
  age_name <- c(`10-29` = "Young", `30-49` = "Middle", `50-74` = "Older")[[bin]]
  add_number(paste0("AgeShare", age_name),
             100 * row$fam / (row$fam + row$market), "fact_age_profile")
}
senior <- read_result("fact_senior")
for (flag in 0:1) {
  row <- senior %>% filter(senior_exempt == flag)
  add_number(paste0("SeniorShare", if (flag == 0) "No" else "Yes"),
             100 * row$fam / (row$fam + row$market), "fact_senior")
}
types <- read_result("property_type_national")
type_keys <- c(`Single-family` = "SF", Duplex = "Duplex", Condominium = "Condo",
               `Townhouse/rowhouse` = "Townhouse")
for (type in names(type_keys)) {
  row <- types %>% filter(property_type == type)
  add_number(paste0("TypeShare", type_keys[[type]]),
             100 * row$family_share_family_market, "property_type_national")
}
resale <- read_result("property_type_resale_standardized")
for (type in names(type_keys)) {
  row <- resale %>% filter(property_type == type)
  add_number(paste0("TypeResaleFamily", type_keys[[type]]),
             100 * row$family_sold_60m, "property_type_resale_standardized")
  add_number(paste0("TypeResaleMarket", type_keys[[type]]),
             100 * row$market_sold_60m, "property_type_resale_standardized")
}
deadline <- read_result("property_type_ca_deadline")
for (type in c("Single-family", "Duplex", "Condominium")) {
  row <- deadline %>% filter(property_type == type)
  add_number(paste0("TypeAntic", type_keys[[type]]), row$anticipation_pct,
             "property_type_ca_deadline")
}
haz <- read_result("hazard_sold_within")
for (group in c("family_all", "family_person", "family_other", "market_sale")) {
  prefix <- c(family_all = "Main", family_person = "Surname",
              family_other = "OtherFamily", market_sale = "Market")[[group]]
  row <- haz %>% filter(class == group)
  add_number(paste0(prefix, "CohortMillion"), row$n / 1e6, "hazard_sold_within")
  add_number(paste0(prefix, "SoldOne"), 100 * row$sold_12m, "hazard_sold_within")
  if (group != "market_sale") {
    add_number(paste0(prefix, "SoldTwo"), 100 * row$sold_24m, "hazard_sold_within")
  }
}

# ---- Exporting macros and their numerical provenance ----
numbers <- bind_rows(number_rows)
stopifnot(!anyDuplicated(numbers$macro))
writeLines(paste0("\\newcommand{\\", numbers$macro, "}{", numbers$formatted, "}"),
           path(tables_dir, "refresh_numbers.tex"))
write_csv_strict(numbers, path(tables_out_dir, "refresh_number_sources.csv"))
message("Exported ", nrow(numbers), " verified manuscript numbers")
