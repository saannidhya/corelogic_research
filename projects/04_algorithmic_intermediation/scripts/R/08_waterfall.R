#' 08_waterfall.R — Sample filter waterfall for Appendix Table (tab:waterfall)
#'
#' Counts deed records surviving each filter stage, accumulated over the 22
#' panel states, 2010–2024. Writes:
#'   out_dir/filter_waterfall.rds
#'   tables_dir/TA1_waterfall.tex  (\label{tab:waterfall})

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))

panel_states <- readRDS(path(out_dir, "panel_states.rds"))
YEARS <- PANEL_START_YEAR:2024L

wf_cols <- c(
  "sale_amount", "sale_derived_date", "deed_situs_zip_code_static",
  "primary_category_code", "residential_indicator",
  "property_indicator_code_static", "interfamily_related_indicator",
  "foreclosure_reo_indicator"
)

stage_names <- c(
  "All deed records, 2010Q1--2024Q2",
  "Arm's-length market sale (category A)",
  "Residential indicator",
  "Single-family or condominium",
  "Price \\$10{,}000--\\$10 million",
  "Excl.\\ inter-family transfers",
  "Excl.\\ foreclosure/REO",
  "Valid 5-digit ZIP (final sample)"
)

counts <- numeric(length(stage_names))

for (st in panel_states) {
  for (yr in YEARS) {
    d <- tryCatch(
      load_corelogic_ot(states = st, years = yr, columns = wf_cols),
      error = function(e) NULL
    )
    if (is.null(d) || nrow(d) == 0) next
    pq <- parse_yyyymmdd_quarter(as.numeric(d$sale_derived_date))
    d <- d |>
      bind_cols(pq) |>
      filter(!is.na(yq), sale_year >= PANEL_START_YEAR, yq <= PANEL_END_YQ) |>
      mutate(pic = suppressWarnings(as.numeric(property_indicator_code_static)))
    s <- logical(nrow(d))
    counts[1] <- counts[1] + nrow(d)
    k <- d$primary_category_code == "A" & !is.na(d$primary_category_code)
    counts[2] <- counts[2] + sum(k, na.rm = TRUE)
    k <- k & !is.na(d$residential_indicator) & d$residential_indicator == "Y"
    counts[3] <- counts[3] + sum(k, na.rm = TRUE)
    k <- k & !is.na(d$pic) & d$pic %in% c(10, 11)
    counts[4] <- counts[4] + sum(k, na.rm = TRUE)
    k <- k & !is.na(d$sale_amount) & d$sale_amount >= PRICE_MIN &
      d$sale_amount <= PRICE_MAX
    counts[5] <- counts[5] + sum(k, na.rm = TRUE)
    k <- k & (is.na(d$interfamily_related_indicator) |
                d$interfamily_related_indicator != 1)
    counts[6] <- counts[6] + sum(k, na.rm = TRUE)
    k <- k & (is.na(d$foreclosure_reo_indicator) |
                d$foreclosure_reo_indicator != 1)
    counts[7] <- counts[7] + sum(k, na.rm = TRUE)
    zip5 <- zip5_of(suppressWarnings(as.numeric(d$deed_situs_zip_code_static)))
    k <- k & !is.na(zip5)
    counts[8] <- counts[8] + sum(k, na.rm = TRUE)
    rm(d); gc(verbose = FALSE)
  }
  message(st, " done [cum total: ", format(counts[1], big.mark = ","), "]")
}

wf <- tibble(stage = stage_names, n = counts)
saveRDS(wf, path(out_dir, "filter_waterfall.rds"))

tex <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Sample construction: filter waterfall (22 states, 2010Q1--2024Q2)}",
  "\\label{tab:waterfall}",
  "\\begin{tabular}{lr}",
  "\\toprule",
  "Filter stage & Records\\\\",
  "\\midrule",
  paste0(wf$stage, " & ", format(wf$n, big.mark = ","), "\\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(tex, path(tables_dir, "TA1_waterfall.tex"))
message("08_waterfall.R complete.")
print(wf)
