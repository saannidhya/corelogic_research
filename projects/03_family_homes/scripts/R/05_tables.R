# ============================================================
# 05: Manuscript tables (LaTeX)
# Author: Saani Rawat
# Purpose: Build paper tables from _outputs/ artifacts only.
# Inputs:  _outputs/tables/*.csv, _outputs/*.rds
# Outputs: manuscript/tables/T1_taxonomy.tex
#          manuscript/tables/T2_volumes.tex
#          manuscript/tables/T3_hazard.tex
#          manuscript/tables/T4_prop19.tex
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20260609)

fmt_n <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, d = 1) sprintf(paste0("%.", d, "f"), 100 * x)
fmt_usd <- function(x) paste0("\\$", format(round(x / 1000), big.mark = ",", trim = TRUE), "k")

class_labels <- c(
  market_sale        = "Market sale (arm's length)",
  family_person      = "Family transfer: same surname",
  family_other       = "Family transfer: other interfamily",
  family_estate      = "Family transfer: estate/executor",
  family_retitle     = "Same-person retitle (co-owner change)",
  family_trust       = "Trust self-transfer",
  other_nonarms      = "Other non-arm's-length",
  estate_noninterfam = "Estate deed (non-interfamily)"
)

# ---- T1: taxonomy + validation moments ------------------------------------
val <- read_csv(path(tables_out_dir, "fact_validation_moments.csv"),
                show_col_types = FALSE) |>
  mutate(lab = class_labels[class]) |>
  arrange(desc(n))

t1_rows <- val |>
  mutate(row = glue("{lab} & {fmt_n(n)} & {fmt_pct(zero_price)} & ",
                    "{if_else(is.na(med_pos_price), '---', fmt_usd(med_pos_price))} & ",
                    "{fmt_pct(quitclaim_share)} & ",
                    "{if_else(absentee_obs > 0.5, fmt_pct(absentee_share_raw / absentee_obs), '---')} \\\\")) |>
  pull(row)

t1 <- c(
  "\\begin{table}[!t]\\centering",
  "\\caption{Residential deed events by class, 2007--2023}",
  "\\label{tab:taxonomy}",
  "{\\footnotesize\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  " & N & Zero/no & Median & Quitclaim & Absentee \\\\",
  "Event class & (2007--2023) & price (\\%) & price$^{a}$ & share (\\%) & recipient (\\%)$^{b}$ \\\\",
  "\\midrule",
  t1_rows,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Authors' classification of CoreLogic Owner Transfer records, residential parcels, deduplicated to one event per parcel-date. $^{a}$Median of strictly positive recorded prices. $^{b}$Share of events whose buyer mailing ZIP differs from the property ZIP, among events where both are observed.}\\end{minipage}",
  "\\end{table}"
)
writeLines(t1, path(tables_dir, "T1_taxonomy.tex"))
message("Wrote T1")

# ---- T2: volumes by year ---------------------------------------------------
headline <- read_csv(path(tables_out_dir, "fact_headline_ratios.csv"),
                     show_col_types = FALSE)
t2_years <- c(2007, 2010, 2013, 2016, 2019, 2021, 2022, 2023)
t2_rows <- headline |>
  filter(sale_year %in% t2_years) |>
  mutate(row = glue("{sale_year} & {fmt_n(market)} & {fmt_n(fam_conservative)} & ",
                    "{fmt_n(fam_broad)} & {fmt_n(trust)} & ",
                    "{sprintf('%.2f', ratio_broad)} \\\\")) |>
  pull(row)

t2 <- c(
  "\\begin{table}[!t]\\centering",
  "\\caption{The parallel housing market: annual volumes}",
  "\\label{tab:volumes}",
  "{\\small",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  " & Market & Family & Family & Trust & Family:market \\\\",
  "Year & sales & (same surname) & (broad) & self-transfers & ratio (broad) \\\\",
  "\\midrule",
  t2_rows,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.9\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Residential deed events. ``Family (broad)'' = same-surname + other-interfamily + estate/executor classes; excludes trust self-transfers. Ratio = family (broad) / market sales.}\\end{minipage}",
  "\\end{table}"
)
writeLines(t2, path(tables_dir, "T2_volumes.tex"))
message("Wrote T2")

# ---- T3: post-transfer outcomes -------------------------------------------
haz <- read_csv(path(tables_out_dir, "hazard_sold_within.csv"),
                show_col_types = FALSE) |>
  filter(class %in% c("family_person", "family_other", "family_estate",
                      "family_trust", "market_sale")) |>
  mutate(lab = class_labels[class]) |>
  arrange(match(class, c("market_sale", "family_person", "family_other",
                         "family_estate", "family_trust")))

t3_rows <- haz |>
  mutate(row = glue("{lab} & {fmt_n(n)} & {fmt_pct(sold_12m)} & {fmt_pct(sold_24m)} & ",
                    "{fmt_pct(sold_36m)} & {fmt_pct(sold_60m)} \\\\")) |>
  pull(row)

t3 <- c(
  "\\begin{table}[!t]\\centering",
  "\\caption{Time to next open-market sale, 2008--2018 event cohorts}",
  "\\label{tab:hazard}",
  "{\\small",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  " & & \\multicolumn{4}{c}{Sold on open market within (\\%)} \\\\",
  "\\cmidrule(lr){3-6}",
  "Event class & N & 12m & 24m & 36m & 60m \\\\",
  "\\midrule",
  t3_rows,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.9\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Share of events followed by an arm's-length market sale of the same parcel within the stated horizon. All cohorts observed at least 66 months before the June 2024 censoring date.}\\end{minipage}",
  "\\end{table}"
)
writeLines(t3, path(tables_dir, "T3_hazard.tex"))
message("Wrote T3")

# ---- T4: Prop 19 regressions ----------------------------------------------
vol <- readRDS(path(out_dir, "prop19_did_volume.rds"))
haz19 <- readRDS(path(out_dir, "prop19_ddd_hazard.rds"))
abs19 <- readRDS(path(out_dir, "prop19_absentee_did.rds"))

etable(
  vol$fam, vol$market_placebo, haz19$fam, haz19$market_placebo, abs19,
  dict = c("ca:post" = "CA $\\times$ Post",
           "state" = "State", "sale_year" = "Year", "ym" = "Cohort month"),
  depvar = FALSE,
  headers = list(
    c("Log volume", "Log volume", "Sold within 24 months", "Sold within 24 months",
      "Absentee recipient"),
    c("Family transfers", "Market sales", "Family transfers", "Market purchases",
      "Family transfers")
  ),
  signif.code = NA,
  fitstat = ~ n + r2,
  fontsize = "scriptsize",
  tex = TRUE,
  file = path(tables_dir, "T4_prop19.tex"),
  replace = TRUE,
  title = "Proposition 19: volume, selection, and composition",
  label = "tab:prop19",
  notes = "Columns (1)-(2): state-year counts, 2017-2019 versus 2022-2023. Columns (3)-(5): transfer-cohort cells, January 2017-December 2018 versus July 2021-June 2022, weighted by cell size. Standard errors are clustered by state and shown for scale; inference with one treated state uses state placebos."
)
message("Wrote T4")

message("Finished 05_tables at ", Sys.time())
