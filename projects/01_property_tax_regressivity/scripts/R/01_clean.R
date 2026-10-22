#' 01: Build the panel for Berry-replication.
#'
#' Joins CoreLogic OT (transactions) and Prop (characteristics) on `clip`,
#' restricts to sales 2007-2010 (window where assessment vintage is
#' contemporaneous with sale — see ADR-004 below), applies arms-length
#' filter, computes assessment ratio and effective tax rate.
#'
#' Output: data/derived/01_property_tax_regressivity/national_panel_2007_2010.parquet
#'
#' Data limitation (informs ADR-004, to be written):
#'   - UC's CoreLogic prop snapshot is overwhelmingly tax_year 2008-2009 vintage
#'   - For sales in 2007-2010, the recorded assessment is approximately
#'     contemporary with the sale (within ~1-2 years)
#'   - For sales after 2010, assessment is stale by years; Berry-style ratio
#'     analysis would conflate true regressivity with appreciation drift
#'   - Future paper version will need a more current prop extract or
#'     historical-panel licensing to extend to 2011-2024

source(here::here("projects/01_property_tax_regressivity/scripts/R/00_setup.R"))

# ---- config ----
USE_SAMPLE   <- FALSE  # smoke run; flip to TRUE for sample
SALE_YEARS   <- 2007:2010
ARM_MIN_PRICE <- 5000  # nominal-sale filter from filter_arms_length() default

# ---- read OT + Prop ----
if (USE_SAMPLE) {
  log_msg("SAMPLE MODE: reading 10K samples")
  ot   <- load_corelogic_ot(sample = TRUE)
  prop <- load_corelogic_prop(sample = TRUE)
} else {
  log_msg("FULL MODE: reading national OT for ", min(SALE_YEARS), "-", max(SALE_YEARS))
  # Select only the columns we need. Reading the full 103-column schema
  # nationally trips on type-coercion errors (some columns inferred as int
  # in one state's parquet but contain "Y"/"N" flags in another). The
  # loader's `columns` argument cuts the read width to a clean subset.
  ot <- load_corelogic_ot(
    years = SALE_YEARS,
    columns = c(
      "clip",
      "fips_code",
      "sale_amount",
      "sale_derived_date",
      "interfamily_related_indicator",
      "foreclosure_reo_indicator",
      "residential_indicator",
      "sale_type_code",
      "primary_category_code"
    )
  )
  log_msg("OT rows: ", format(nrow(ot), big.mark = ","))
  log_msg("Reading national Prop (state-by-state to dodge arrow's cross-state schema unification)")
  # KNOWN ISSUE: arrow::open_dataset() infers a unified schema across ALL
  # partitions FIRST, then applies column selection. A type mismatch in ANY
  # state's parquet (e.g., 'RDH' in a column inferred as double) blocks the
  # read even if we don't ask for that column. Workaround: load state-by-state
  # via direct read_parquet on each state's part.parquet, select the columns
  # we want, then bind. This is slower per-state but bypasses the unification.
  # TODO: fix in shared_utils/R/corelogic_loader.R (define explicit schema or
  # switch to duckdb backend).

  PROP_KEEP_COLS <- c("clip", "fips_code", "assessed_total_value",
                      "total_tax_amount", "tax_year")
  prop_root <- here::here("data/corelogic_extracts/by_state/prop")
  state_dirs <- fs::dir_ls(prop_root, type = "directory")
  log_msg("Reading ", length(state_dirs), " state prop partitions...")

  prop_list <- vector("list", length(state_dirs))
  for (i in seq_along(state_dirs)) {
    sd <- state_dirs[i]
    state_code <- sub("^state=", "", fs::path_file(sd))
    part_file <- fs::path(sd, "part.parquet")
    if (!fs::file_exists(part_file)) next
    tryCatch({
      df <- arrow::read_parquet(part_file)
      df <- df[, intersect(PROP_KEEP_COLS, names(df)), drop = FALSE]
      # Coerce types defensively
      if ("assessed_total_value" %in% names(df))
        df$assessed_total_value <- suppressWarnings(as.numeric(df$assessed_total_value))
      if ("total_tax_amount" %in% names(df))
        df$total_tax_amount <- suppressWarnings(as.numeric(df$total_tax_amount))
      if ("tax_year" %in% names(df))
        df$tax_year <- suppressWarnings(as.integer(df$tax_year))
      df$state <- state_code
      prop_list[[i]] <- df
    }, error = function(e) {
      log_msg("  ERROR reading ", state_code, ": ", conditionMessage(e))
    })
    if (i %% 10 == 0) log_msg("  ", i, "/", length(state_dirs), " states read")
  }
  prop <- do.call(rbind, prop_list)
  log_msg("Pre-dedup Prop rows: ", format(nrow(prop), big.mark = ","))
  # DEDUPE: clip is not unique in Prop — multiple tax_year vintages per
  # parcel. For each clip, keep the row with the largest non-NA
  # assessed_total_value AND highest tax_year (latest assessment available).
  # This is approximate; future Phase will do year-aware OT->Prop matching
  # where each sale joins the assessment whose tax_year is closest to the
  # sale_year.
  prop <- prop |>
    filter(!is.na(clip), !is.na(assessed_total_value), assessed_total_value > 0) |>
    arrange(clip, desc(tax_year)) |>
    group_by(clip) |>
    slice(1) |>
    ungroup()
  log_msg("Post-dedup Prop rows (one per clip): ", format(nrow(prop), big.mark = ","))
  log_msg("Prop rows: ", format(nrow(prop), big.mark = ","))
}

# ---- arms-length filter on OT (sale-side cleaning) ----
# Before 2026-10-22, filter_arms_length() compared the 0/1 interfamily and REO
# flags to "Y", so neither was dropped; panels built before then include them.
log_msg("Pre-filter OT rows: ", format(nrow(ot), big.mark = ","),
        " (interfamily: ", sum(is_flag_set(ot$interfamily_related_indicator)),
        ", REO: ", sum(is_flag_set(ot$foreclosure_reo_indicator)), ")")
ot_clean <- filter_arms_length(ot, min_price = ARM_MIN_PRICE)
log_msg("Post-arms-length OT rows: ", format(nrow(ot_clean), big.mark = ","))
stopifnot(!any(is_flag_set(ot_clean$interfamily_related_indicator)),
          !any(is_flag_set(ot_clean$foreclosure_reo_indicator)))

# Drop sales without a usable date; restrict to SALE_YEARS in full mode
if ("sale_derived_date" %in% names(ot_clean)) {
  ot_clean <- ot_clean |>
    filter(!is.na(sale_derived_date)) |>
    mutate(
      sale_year = as.integer(substr(as.character(sale_derived_date), 1, 4))
    )
  if (!USE_SAMPLE) {
    ot_clean <- ot_clean |> filter(sale_year %in% SALE_YEARS)
    log_msg("Post-sale-year-window OT rows: ", format(nrow(ot_clean), big.mark = ","))
  }
}

# ---- join on clip ----
log_msg("Joining OT to Prop on clip")
panel <- ot_clean |>
  inner_join(prop, by = "clip", suffix = c("_ot", "_prop"))
log_msg("Joined panel rows: ", format(nrow(panel), big.mark = ","))

# ---- compute dependent variables ----
panel <- panel |>
  mutate(
    # Berry's primary outcome: assessment ratio = assessed_value / sale_price
    assessment_ratio = assessed_total_value / sale_amount,
    # Effective tax rate: tax_bill / sale_price (Berry's preferred outcome)
    effective_tax_rate = total_tax_amount / sale_amount
  )

# ---- sanity / filter ----
# Drop properties without an assessed value or where ratio is implausible
# Berry's footnote: drop ratios outside [0.01, 5.0] to remove data errors
log_msg("Pre-ratio-filter rows: ", format(nrow(panel), big.mark = ","))
panel <- panel |>
  filter(
    !is.na(assessed_total_value),
    !is.na(sale_amount),
    assessed_total_value > 0,
    assessment_ratio >= 0.01,
    assessment_ratio <= 5.0
  )
log_msg("Post-ratio-filter rows: ", format(nrow(panel), big.mark = ","))

# TODO: refine residential land_use codes per Berry footnote 12
# For now keep all land uses and document in the replication report

# ---- write derived panel ----
out_path <- if (USE_SAMPLE) {
  path(data_dir, "sample_panel_for_smoke.parquet")
} else {
  panel_path
}
log_msg("Writing panel to: ", out_path)
write_parquet(panel, out_path)
saveRDS(panel, path(out_dir, sub("\\.parquet$", ".rds", path_file(out_path))))

log_msg("DONE — rows in final panel: ", format(nrow(panel), big.mark = ","))
log_msg("Columns: ", paste(head(names(panel), 8), collapse = ", "), " ...")
