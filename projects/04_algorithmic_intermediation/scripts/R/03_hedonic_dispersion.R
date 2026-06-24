#' 03_hedonic_dispersion.R — Price-discovery outcomes + ZIP covariates
#'
#' For each panel state:
#'   1. Join the deed cache (script 02) to Property Characteristics hedonics
#'      by clip.
#'   2. Estimate a yearly hedonic: lprice ~ lsqft + beds + baths + age +
#'      age^2 + condo | zip5, on non-iBuyer market sales (the household-to-
#'      household segment whose price discovery we measure).
#'   3. Compute within-ZIP5-x-quarter dispersion of demeaned hedonic
#'      residuals (SD + IQR-based robust SD + left-tail share), and the mean
#'      residual of iBuyer purchases/dispositions (machine's buy discount).
#'   4. Build ZIP-level "machine readability" covariates from the housing
#'      stock (homogeneity of size, vintage, SFR share) + modal CBSA.
#'
#' Outputs:
#'   - data_dir/zip_quarter_dispersion.parquet
#'   - data_dir/zip_covariates.parquet
#'   - out_dir/hedonic_fit_stats.rds (R2, N per state-year)
#'
#' Run time: ~1–2 h. Idempotent per state via interim RDS.

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))
set.seed(20260610)

panel_states <- readRDS(path(out_dir, "panel_states.rds"))
cache_dir    <- path(data_dir, "deeds_cache")
interim_dir  <- path(data_dir, "hedonic_interim")
dir_create(interim_dir)

prop_cols <- c(
  "clip", "situs_core_based_statistical_area_cbsa",
  "universal_building_square_feet",
  "total_number_of_bedrooms_all_buildings",
  "total_number_of_bathrooms_all_buildings",
  "year_built", "property_indicator_code", "situs_zip_code"
)

MIN_CELL <- 5L          # min non-iBuyer sales per ZIP-quarter for dispersion
LEFT_TAIL <- -0.25      # fire-sale proxy: residual < -25 log points

process_state <- function(st) {
  f_disp <- path(interim_dir, paste0("disp_", st, ".rds"))
  f_cov  <- path(interim_dir, paste0("cov_", st, ".rds"))
  f_fit  <- path(interim_dir, paste0("fit_", st, ".rds"))
  if (file_exists(f_disp) && file_exists(f_cov)) {
    message(st, ": interim cached, skipping")
    return(invisible(NULL))
  }

  deeds <- read_parquet(path(cache_dir, paste0("state=", st), "part-0.parquet"))
  # Standard unified-schema load; fall back to the partition's native schema
  # when cross-partition type drift breaks the unified scan (e.g., IN stores
  # situs_zip_code as string with junk the unified double type cannot parse).
  prop <- tryCatch(
    load_corelogic_prop(states = st, columns = prop_cols),
    error = function(e) {
      message(st, ": unified prop scan failed (", substr(conditionMessage(e), 1, 60),
              ") - falling back to per-partition read")
      load_corelogic_prop(states = st, columns = prop_cols, per_partition = TRUE)
    }
  ) |>
    distinct(clip, .keep_all = TRUE)

  # ---- ZIP covariates from the stock (computed before any sample cuts) ----
  stock <- prop |>
    mutate(
      zip5  = zip5_of(suppressWarnings(as.numeric(situs_zip_code))),
      # property indicator is double in some partitions, string in others
      pic   = suppressWarnings(as.numeric(property_indicator_code)),
      sfr   = as.integer(pic == 10),
      resi  = as.integer(pic %in% c(10, 11)),
      lsqft = ifelse(!is.na(universal_building_square_feet) &
                       universal_building_square_feet > 100,
                     log(universal_building_square_feet), NA_real_)
    ) |>
    filter(!is.na(zip5), resi == 1)
  zip_cov <- stock |>
    group_by(zip5) |>
    summarise(
      state          = st,
      cbsa           = {
        x <- situs_core_based_statistical_area_cbsa
        x <- x[!is.na(x)]
        if (length(x) == 0) NA_real_ else as.numeric(names(sort(table(x), decreasing = TRUE))[1])
      },
      stock_resi     = n(),
      sfr_share      = mean(sfr, na.rm = TRUE),
      sd_lsqft_stock = sd(lsqft, na.rm = TRUE),
      share_post1990 = mean(year_built >= 1990, na.rm = TRUE),
      med_yearbuilt  = median(year_built, na.rm = TRUE),
      .groups = "drop"
    )
  saveRDS(zip_cov, f_cov)
  rm(stock); gc(verbose = FALSE)

  # ---- hedonic estimation sample ----
  hed <- deeds |>
    left_join(
      prop |> select(clip, universal_building_square_feet,
                     total_number_of_bedrooms_all_buildings,
                     total_number_of_bathrooms_all_buildings, year_built),
      by = "clip"
    ) |>
    mutate(
      lsqft = ifelse(!is.na(universal_building_square_feet) &
                       universal_building_square_feet > 100 &
                       universal_building_square_feet < 20000,
                     log(universal_building_square_feet), NA_real_),
      beds  = ifelse(suppressWarnings(as.numeric(total_number_of_bedrooms_all_buildings)) %in% 0:15,
                     suppressWarnings(as.numeric(total_number_of_bedrooms_all_buildings)), NA_real_),
      baths = {
        b <- suppressWarnings(as.numeric(total_number_of_bathrooms_all_buildings))
        ifelse(!is.na(b) & b >= 0 & b <= 15, b, NA_real_)
      },
      # Missing-dummy treatment: some states (AZ, MD) do not report bedrooms
      # at all; the hedonic must not drop whole states over one regressor.
      beds_x  = coalesce(beds, 0),  beds_na  = as.integer(is.na(beds)),
      baths_x = coalesce(baths, 0), baths_na = as.integer(is.na(baths)),
      age   = ifelse(!is.na(year_built) & year_built >= 1800 &
                       year_built <= sale_year,
                     sale_year - year_built, NA_real_),
      age2  = age^2,
      ib_deed = !is.na(buyer_firm) | !is.na(seller_firm)
    ) |>
    filter(!is.na(lsqft), !is.na(age))
  rm(prop, deeds); gc(verbose = FALSE)

  # Yearly hedonics on household-to-household (non-iBuyer) trades
  years <- sort(unique(hed$sale_year))
  res_list <- vector("list", length(years))
  fit_list <- vector("list", length(years))
  for (i in seq_along(years)) {
    yr <- years[i]
    dy <- hed |> filter(sale_year == yr)
    est_sample <- dy |> filter(!ib_deed)
    if (nrow(est_sample) < 1000) next
    m <- tryCatch(
      feols(lprice ~ lsqft + beds_x + beds_na + baths_x + baths_na +
              age + age2 + condo | zip5,
            data = est_sample, lean = FALSE, notes = FALSE),
      error = function(e) NULL
    )
    if (is.null(m)) next
    fit_list[[i]] <- tibble(state = st, year = yr,
                            n = nobs(m), r2 = r2(m, type = "r2")[["r2"]])
    # Residuals for ALL deeds that year (incl. iBuyer), using non-iBuyer betas
    pred <- as.numeric(predict(m, newdata = dy))
    dy$resid <- dy$lprice - pred
    res_list[[i]] <- dy |>
      filter(!is.na(resid)) |>
      select(zip5, yq, sale_year, sale_qtr, resid, ib_deed,
             buyer_firm, seller_firm)
    rm(dy, est_sample, m); gc(verbose = FALSE)
  }
  res <- bind_rows(res_list)
  saveRDS(bind_rows(fit_list), f_fit)

  # ---- within ZIP-quarter dispersion among non-iBuyer trades ----
  # left_tail: referee B Major 5 — report BOTH the demeaned-residual tail
  # (a dispersion tail) and the raw-residual tail (the below-predicted-price
  # tail the text describes as the fire-sale margin).
  disp <- res |>
    filter(!ib_deed) |>
    group_by(zip5, yq) |>
    mutate(dev = resid - mean(resid)) |>
    summarise(
      state         = st,
      n_hedonic     = n(),
      disp_sd       = ifelse(n() >= MIN_CELL, sd(dev), NA_real_),
      disp_iqr      = ifelse(n() >= MIN_CELL, IQR(dev) / 1.349, NA_real_),
      left_tail     = ifelse(n() >= MIN_CELL, mean(dev < LEFT_TAIL), NA_real_),
      left_tail_raw = ifelse(n() >= MIN_CELL, mean(resid < LEFT_TAIL), NA_real_),
      .groups = "drop"
    )

  # iBuyer purchase/disposition residuals (descriptive: machine discount).
  # sd_resid added per referee A Major 1: firm-level forecast-error dispersion
  # (sigma_a) to discipline the price-discovery calibration. Computed within
  # ZIP x year then aggregated, so it nets out cross-segment level differences.
  ib_resid <- res |>
    filter(ib_deed) |>
    mutate(
      side = case_when(
        !is.na(buyer_firm)  ~ "buy",
        !is.na(seller_firm) ~ "sell",
        TRUE ~ NA_character_
      ),
      firm = coalesce(buyer_firm, seller_firm)
    ) |>
    filter(!is.na(side)) |>
    group_by(firm, side, sale_year) |>
    summarise(state = st, n = n(), mean_resid = mean(resid),
              med_resid = median(resid), sd_resid = sd(resid),
              .groups = "drop")

  # Firm-level within-ZIP-year residual SD (the cleaner sigma_a proxy:
  # de-meaned within zip5 x year so it is not inflated by segment levels).
  ib_resid_demeaned <- res |>
    filter(ib_deed, !is.na(buyer_firm)) |>
    group_by(zip5, sale_year) |>
    mutate(dev = resid - mean(resid), ncell = n()) |>
    ungroup() |>
    filter(ncell >= 3L) |>
    group_by(buyer_firm) |>
    summarise(state = st, n = n(), sd_within = sd(dev), .groups = "drop")

  saveRDS(list(disp = disp, ib_resid = ib_resid,
               ib_resid_demeaned = ib_resid_demeaned), f_disp)
  message(st, ": dispersion done (", nrow(disp), " cells)")
  rm(hed, res, disp); gc(verbose = FALSE)
  invisible(NULL)
}

failed <- character(0)
for (st in panel_states) {
  ok <- tryCatch({ process_state(st); TRUE },
                 error = function(e) {
                   message(st, " FAILED: ", conditionMessage(e))
                   FALSE
                 })
  if (!ok) failed <- c(failed, st)
}
if (length(failed) > 0) {
  message("States failed in hedonic stage: ", paste(failed, collapse = " "))
  saveRDS(failed, path(out_dir, "hedonic_failed_states.rds"))
  panel_states <- setdiff(panel_states, failed)
}

# ---- combine interim files ----
disp_all <- map(panel_states,
                \(st) readRDS(path(interim_dir, paste0("disp_", st, ".rds")))$disp) |>
  bind_rows()
ib_resid_all <- map(panel_states,
                    \(st) readRDS(path(interim_dir, paste0("disp_", st, ".rds")))$ib_resid) |>
  bind_rows()
ib_resid_dm_all <- map(panel_states, \(st) {
  x <- readRDS(path(interim_dir, paste0("disp_", st, ".rds")))$ib_resid_demeaned
  if (is.null(x)) NULL else x
}) |> bind_rows()
cov_all <- map(panel_states,
               \(st) readRDS(path(interim_dir, paste0("cov_", st, ".rds")))) |>
  bind_rows()
fit_all <- map(panel_states, \(st) {
  f <- path(interim_dir, paste0("fit_", st, ".rds"))
  if (file_exists(f)) readRDS(f) else NULL
}) |> bind_rows()

write_parquet(disp_all, path(data_dir, "zip_quarter_dispersion.parquet"))
write_parquet(cov_all,  path(data_dir, "zip_covariates.parquet"))
saveRDS(ib_resid_all, path(out_dir, "ibuyer_residuals_by_year.rds"))
saveRDS(fit_all,      path(out_dir, "hedonic_fit_stats.rds"))

# Firm-level sigma_a (within-ZIP-year residual SD), pooled across states by
# n-weighting the within-state SDs (referee A Major 1 calibration discipline).
sigma_a_firm <- ib_resid_dm_all |>
  filter(!is.na(sd_within), n > 0) |>
  group_by(buyer_firm) |>
  summarise(sigma_a = sqrt(weighted.mean(sd_within^2, w = n)),
            n_tot = sum(n),
            .groups = "drop")
saveRDS(sigma_a_firm, path(out_dir, "sigma_a_by_firm.rds"))
message("Firm-level sigma_a (within-ZIP-year residual SD):")
print(sigma_a_firm)

message("Dispersion cells: ", nrow(disp_all),
        " | mean hedonic R2: ", round(mean(fit_all$r2, na.rm = TRUE), 3))
