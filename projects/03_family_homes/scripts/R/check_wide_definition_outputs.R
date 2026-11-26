# ============================================================
# Purpose : Reconcile the wider family proxy across refreshed outputs
# Name    : Saani Rawat
# Created : 11/04/2026
# Log     : 11/04/2026: verify counts, policy inputs, and welfare accounting.
# Inputs  : Saved aggregate CSVs; no CoreLogic microdata reads
# Outputs : Console PASS lines; stops on a reconciliation failure
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
read_result <- function(name) {
  read_csv(path(tables_out_dir, paste0(name, ".csv")), show_col_types = FALSE)
}
check <- function(ok, label) {
  if (!isTRUE(ok)) stop("FAIL: ", label)
  message("PASS: ", label)
}

headline <- read_result("fact_headline_ratios")
national <- read_result("fact_national_by_year_class")
family_by_year <- national %>% filter(class %in% family_classes) %>%
  group_by(sale_year) %>% summarise(family = sum(n), .groups = "drop")
counts <- headline %>% left_join(family_by_year, by = "sale_year")
check(all(counts$fam_broad == counts$family & counts$fam_wide == counts$family),
      "main family counts equal the sum of all three family classes")
check(all(abs(counts$share_family_all - counts$family / counts$n_all) < 1e-12),
      "headline shares use all deed events")
state_counts <- read_result("fact_state_class_2007_2023")
check(all(state_counts$fam_broad == state_counts$family_person +
            state_counts$family_estate + state_counts$family_other),
      "every state uses the same three-class numerator")
check(sum(state_counts$n_all) == sum(headline$n_all) &&
        sum(state_counts$fam_broad) == sum(headline$fam_broad),
      "national and state count totals reconcile including residual geographic codes")

haz <- read_result("hazard_sold_within")
components <- haz %>% filter(class %in% family_classes)
pooled <- haz %>% filter(class == "family_all")
check(nrow(pooled) == 1L && pooled$n == sum(components$n),
      "pooled resale cohort equals the three component cohorts")
for (horizon in c("sold_12m", "sold_24m", "sold_36m", "sold_60m")) {
  check(abs(pooled[[horizon]] - weighted.mean(components[[horizon]], components$n)) < 1e-12,
        paste("pooled resale outcome reconciles:", horizon))
}
km <- read_result("hazard_km_curves") %>%
  filter(class == "family_all") %>% arrange(t)
check(nrow(km) == 120L && all(diff(km$surv) <= 1e-12),
      "pooled family survival is defined and nonincreasing through ten years")

did <- read_result("prop19_did_volume")
ddd <- read_result("prop19_ddd_volume")
beta_net <- did$coef[did$model == "fam"] - did$coef[did$model == "market_placebo"]
check(abs(beta_net - ddd$estimate) < 1e-10,
      "stacked triple difference equals family minus market difference-in-differences")
ca <- read_result("fact_california_by_year_class") %>%
  filter(class %in% family_classes, sale_year %in% 2018:2019) %>%
  group_by(sale_year) %>% summarise(n = sum(n), .groups = "drop")
welfare <- read_result("welfare_effect_inputs") %>%
  filter(specification == "net_DiD_baseline_scaled")
check(nrow(ca) == 2L && abs(welfare$baseline - mean(ca$n)) < 1e-8,
      "welfare reference flow uses the wider California baseline")
check(abs(welfare$annual_deeds - mean(ca$n) * (1 - exp(beta_net))) < 1e-7,
      "welfare missing-deed flow matches the refreshed policy response")
retiming <- read_result("prop19_retiming_summary")
scm <- read_result("prop19_scm_summary")
check(abs(retiming$scm_gap_2022_2023_pct - scm$gap_ss_pct) < 1e-10,
      "retiming summary uses the refreshed synthetic-control estimate")
inherited <- read_result("wedge_inherited_homes")
check(abs(inherited$n_inherited / 2 - inherited$n_inherited_per_year) < 1e-8 &&
        abs(inherited$mean_wedge_2023usd * inherited$n_inherited_per_year -
              inherited$annual_wedge_flow_2023usd) < 1,
      "inherited-proxy parcel count and tax-saving flow reconcile")
sources <- read_result("refresh_number_sources")
check(!anyDuplicated(sources$macro) && all(is.finite(sources$value)),
      "manuscript numerical macros are unique and finite")
message("All wider-definition output reconciliations passed.")
