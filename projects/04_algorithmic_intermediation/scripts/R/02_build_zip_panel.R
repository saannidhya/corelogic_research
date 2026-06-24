#' 02_build_zip_panel.R — ZIP5 x quarter outcome panel + deed-level cache
#'
#' For every state with material iBuyer activity (>= MIN_STATE_DEEDS deeds in
#' the 01 scan), loads market-sale deeds 2010–2024, applies the project's
#' arm's-length residential filter, flags iBuyer counterparties, and builds:
#'   - data_dir/deeds_cache/state=XX/part-0.parquet  (deed-level, filtered)
#'   - data_dir/zip_quarter_panel.parquet            (ZIP5 x quarter aggregates)
#'
#' Market-sale filter (project conventions, validated in project 03 probes):
#'   primary_category_code == "A", residential_indicator == "Y",
#'   property_indicator_code_static in {10 SFR, 11 condo},
#'   price in [$10k, $10M], interfamily == 0, foreclosure/REO == 0.
#'
#' Run time: ~1–2 h. Idempotent (states already cached are skipped).

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))
set.seed(20260610)

MIN_STATE_DEEDS <- 500L
PANEL_YEARS <- PANEL_START_YEAR:2024L

state_summary <- readRDS(path(out_dir, "ibuyer_state_summary.rds"))
panel_states <- state_summary |>
  filter(total >= MIN_STATE_DEEDS) |>
  pull(state) |>
  sort()
message("Panel states (", length(panel_states), "): ",
        paste(panel_states, collapse = " "))
saveRDS(panel_states, path(out_dir, "panel_states.rds"))

# buyer_2/seller_2/corporate-indicator columns excluded: bool-typed all-NA
# partitions break arrow schema unification (explorations/schema_audit_20260610.R).
# Institutional buyers proxied via CORP_NAME_PATTERN on buyer_1_full_name.
panel_cols <- c(
  "clip", "fips_code", "sale_amount", "sale_derived_date",
  "deed_situs_zip_code_static", "primary_category_code",
  "residential_indicator", "property_indicator_code_static",
  "interfamily_related_indicator", "foreclosure_reo_indicator",
  "cash_purchase_indicator", "investor_purchase_indicator",
  "new_construction_indicator", "resale_indicator",
  "buyer_1_full_name", "seller_1_full_name"
)

cache_dir <- path(data_dir, "deeds_cache")
dir_create(cache_dir)

#' Filter one state-year slice to market sales and flag iBuyer counterparties.
clean_slice <- function(d) {
  d <- d |>
    mutate(
      # property indicator is double in some partitions, string in others
      pic = suppressWarnings(as.numeric(property_indicator_code_static))
    ) |>
    filter(
      primary_category_code == "A",
      residential_indicator == "Y",
      pic %in% c(10, 11),
      !is.na(sale_amount),
      sale_amount >= PRICE_MIN, sale_amount <= PRICE_MAX,
      is.na(interfamily_related_indicator) | interfamily_related_indicator != 1,
      is.na(foreclosure_reo_indicator) | foreclosure_reo_indicator != 1
    )
  if (nrow(d) == 0) return(NULL)
  pq <- parse_yyyymmdd_quarter(as.numeric(d$sale_derived_date))
  d |>
    bind_cols(pq) |>
    filter(!is.na(yq), sale_year >= PANEL_START_YEAR, yq <= PANEL_END_YQ) |>
    mutate(
      zip5        = zip5_of(suppressWarnings(as.numeric(deed_situs_zip_code_static))),
      buyer_firm  = classify_ibuyer(buyer_1_full_name),
      seller_firm = classify_ibuyer(seller_1_full_name),
      condo       = as.integer(pic == 11),
      corp_buyer  = as.integer(!is.na(buyer_1_full_name) &
                                 grepl(CORP_NAME_PATTERN, buyer_1_full_name)),
      lprice      = log(sale_amount)
    ) |>
    filter(!is.na(zip5)) |>
    select(clip, fips_code, zip5, sale_amount, lprice, sale_year, sale_qtr, yq,
           buyer_firm, seller_firm, condo, corp_buyer,
           cash_purchase_indicator, investor_purchase_indicator,
           new_construction_indicator, resale_indicator)
}

for (st in panel_states) {
  st_file <- path(cache_dir, paste0("state=", st), "part-0.parquet")
  if (file_exists(st_file)) {
    message(st, ": cached, skipping")
    next
  }
  t0 <- Sys.time()
  slices <- vector("list", length(PANEL_YEARS))
  for (i in seq_along(PANEL_YEARS)) {
    yr <- PANEL_YEARS[i]
    d <- tryCatch(
      load_corelogic_ot(states = st, years = yr, columns = panel_cols),
      error = function(e) NULL
    )
    if (is.null(d) || nrow(d) == 0) next
    slices[[i]] <- clean_slice(d)
    rm(d); gc(verbose = FALSE)
  }
  deeds <- bind_rows(slices)
  dir_create(path(cache_dir, paste0("state=", st)))
  write_parquet(deeds, st_file)
  message(sprintf("%s: %7d market sales cached  [%.1f min]",
                  st, nrow(deeds),
                  as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  rm(deeds, slices); gc(verbose = FALSE)
}

# ---- ZIP5 x quarter aggregates ----
agg_list <- vector("list", length(panel_states))
names(agg_list) <- panel_states
for (st in panel_states) {
  deeds <- read_parquet(path(cache_dir, paste0("state=", st), "part-0.parquet"))
  agg_list[[st]] <- deeds |>
    group_by(zip5, sale_year, sale_qtr, yq) |>
    summarise(
      state            = st,
      fips             = first(fips_code),
      n_sales          = n(),
      n_ib_buy         = sum(!is.na(buyer_firm)),
      n_ib_sell        = sum(!is.na(seller_firm)),
      n_zillow_buy     = sum(buyer_firm == "zillow",   na.rm = TRUE),
      n_opendoor_buy   = sum(buyer_firm == "opendoor", na.rm = TRUE),
      n_offerpad_buy   = sum(buyer_firm == "offerpad", na.rm = TRUE),
      n_redfin_buy     = sum(buyer_firm == "redfin",   na.rm = TRUE),
      n_zillow_sell    = sum(seller_firm == "zillow",  na.rm = TRUE),
      med_price        = median(sale_amount, na.rm = TRUE),
      mean_lprice      = mean(lprice, na.rm = TRUE),
      cash_share       = mean(cash_purchase_indicator == 1, na.rm = TRUE),
      investor_share   = mean(investor_purchase_indicator == 1, na.rm = TRUE),
      newconstr_share  = mean(new_construction_indicator == 1, na.rm = TRUE),
      corp_buyer_share = mean(corp_buyer, na.rm = TRUE),
      .groups = "drop"
    )
  rm(deeds); gc(verbose = FALSE)
  message(st, ": aggregated")
}

panel <- bind_rows(agg_list)
write_parquet(panel, path(data_dir, "zip_quarter_panel.parquet"))

message("Panel: ", nrow(panel), " ZIP-quarter cells, ",
        n_distinct(panel$zip5), " ZIPs, states: ", length(panel_states))
saveRDS(
  list(
    n_cells = nrow(panel), n_zips = n_distinct(panel$zip5),
    n_states = length(panel_states),
    n_sales_total = sum(panel$n_sales),
    n_ib_buy_total = sum(panel$n_ib_buy),
    built_at = Sys.time()
  ),
  path(out_dir, "panel_build_meta.rds")
)
