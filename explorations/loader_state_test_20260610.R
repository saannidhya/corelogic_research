# Test: can the reduced column set load for every state (year 2021)?
suppressPackageStartupMessages({source(here::here("shared_utils/R/corelogic_loader.R"))})
source(here::here("shared_utils/R/filters.R"))

cols <- c(
  "clip", "fips_code", "sale_amount", "sale_derived_date",
  "deed_situs_zip_code_static", "primary_category_code",
  "residential_indicator", "property_indicator_code_static",
  "interfamily_related_indicator", "foreclosure_reo_indicator",
  "cash_purchase_indicator", "investor_purchase_indicator",
  "new_construction_indicator", "resale_indicator",
  "buyer_1_full_name", "seller_1_full_name"
)
states <- setdiff(VALID_STATE_CODES,
                  c("AS","FM","MH","MP","PW","AA","AE","AP","GU","VI","PR"))
for (st in states) {
  r <- tryCatch({
    d <- load_corelogic_ot(states = st, years = 2021L, columns = cols)
    paste0("OK n=", nrow(d))
  }, error = function(e) paste0("FAIL: ", substr(conditionMessage(e), 1, 80)))
  cat(sprintf("%-3s %s\n", st, r))
}
