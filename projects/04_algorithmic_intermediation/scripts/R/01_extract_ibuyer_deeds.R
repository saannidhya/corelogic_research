#' 01_extract_ibuyer_deeds.R — National scan for iBuyer transactions
#'
#' Scans CoreLogic Owner Transfer deeds (all states, 2013–2024) for purchases
#' and sales by the four major iBuyers (Opendoor, Zillow Offers, Offerpad,
#' RedfinNow), identified by curated entity-name patterns on buyer/seller
#' names. Writes:
#'   - data_dir/ibuyer_deeds.parquet      (every deed touching an iBuyer)
#'   - out_dir/ibuyer_state_summary.rds   (counts by state x firm)
#'   - out_dir/ibuyer_cbsa_entry.rds      (per-FIPS first-activity quarters)
#'
#' Run time: ~30–60 min (full national scan). Idempotent.

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))
set.seed(20260610)

SCAN_YEARS  <- 2013:2024
SCAN_STATES <- setdiff(
  VALID_STATE_CODES,
  c("AS", "FM", "MH", "MP", "PW", "AA", "AE", "AP", "GU", "VI", "PR")
)

# NOTE: buyer_2/seller_2 name columns are excluded — they have bool-typed
# all-NA partitions that break arrow schema unification (see
# explorations/schema_audit_20260610.R). The iBuyer entity is recorded as
# the first-listed party on its deeds (verified on AZ 2019–2021).
scan_cols <- c(
  "clip", "fips_code", "sale_amount", "sale_derived_date",
  "deed_situs_zip_code_static", "primary_category_code",
  "residential_indicator", "property_indicator_code_static",
  "buyer_1_full_name", "seller_1_full_name"
)

any_pattern <- paste(unname(IBUYER_PATTERNS), collapse = "|")

scan_state <- function(st) {
  hits <- vector("list", length(SCAN_YEARS))
  for (i in seq_along(SCAN_YEARS)) {
    yr <- SCAN_YEARS[i]
    d <- tryCatch(
      load_corelogic_ot(states = st, years = yr, columns = scan_cols),
      error = function(e) NULL
    )
    if (is.null(d) || nrow(d) == 0) next
    keep <- d |>
      filter(
        grepl(any_pattern, buyer_1_full_name,  ignore.case = TRUE) |
        grepl(any_pattern, seller_1_full_name, ignore.case = TRUE)
      )
    hits[[i]] <- keep
    rm(d); gc(verbose = FALSE)
  }
  bind_rows(hits)
}

message("Scanning ", length(SCAN_STATES), " states, years ",
        min(SCAN_YEARS), "-", max(SCAN_YEARS))

all_hits <- vector("list", length(SCAN_STATES))
names(all_hits) <- SCAN_STATES
t0 <- Sys.time()
for (st in SCAN_STATES) {
  res <- scan_state(st)
  all_hits[[st]] <- res
  message(sprintf("  %s: %6d iBuyer-linked deeds  [%.1f min elapsed]",
                  st, ifelse(is.null(res), 0L, nrow(res)),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
}

ibuyer <- bind_rows(all_hits)
ibuyer <- ibuyer |>
  bind_cols(parse_yyyymmdd_quarter(as.numeric(ibuyer$sale_derived_date))) |>
  mutate(
    buyer_firm  = classify_ibuyer(buyer_1_full_name),
    seller_firm = classify_ibuyer(seller_1_full_name),
    zip5 = zip5_of(suppressWarnings(as.numeric(deed_situs_zip_code_static)))
  ) |>
  filter(!is.na(buyer_firm) | !is.na(seller_firm))

write_parquet(ibuyer, path(data_dir, "ibuyer_deeds.parquet"))

state_summary <- ibuyer |>
  mutate(firm = coalesce(buyer_firm, seller_firm)) |>
  count(state, firm, name = "n_deeds") |>
  pivot_wider(names_from = firm, values_from = n_deeds, values_fill = 0L) |>
  mutate(total = rowSums(across(where(is.numeric))))

saveRDS(state_summary, path(out_dir, "ibuyer_state_summary.rds"))

# Purchases only (the algorithm taking inventory), by county-quarter
fips_qtr <- ibuyer |>
  filter(!is.na(buyer_firm), !is.na(yq)) |>
  count(fips_code, buyer_firm, yq, name = "n_buys")

saveRDS(fips_qtr, path(out_dir, "ibuyer_fips_qtr.rds"))

message("Total iBuyer-linked deeds: ", nrow(ibuyer))
message("  purchases: ", sum(!is.na(ibuyer$buyer_firm)),
        " | dispositions: ", sum(!is.na(ibuyer$seller_firm)))
print(state_summary |> arrange(desc(total)) |> head(25), n = 25)
