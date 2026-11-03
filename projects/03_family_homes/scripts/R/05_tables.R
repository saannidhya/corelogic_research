# ============================================================
# 05: Manuscript tables (LaTeX)
# Author: Saani Rawat
# Purpose: Build paper tables from _outputs/ artifacts only.
# Inputs:  _outputs/tables/*.csv, _outputs/*.rds
# Outputs: manuscript/tables/T1_taxonomy.tex
#          manuscript/tables/T2_volumes.tex
#          manuscript/tables/T3_hazard.tex
#          manuscript/tables/T4_prop19.tex
#          manuscript/tables/TA_raw22.tex (appendix tab:raw22)
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
  family_other       = "Interfamily, no surname match",
  family_estate      = "Family transfer: estate/executor",
  family_retitle     = "Same-person retitle (co-owner change)",
  family_trust       = "Interfamily trust deed",
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
  "\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Authors' classification of CoreLogic Owner Transfer records, residential parcels, deduplicated to one event per parcel-date. Market sales are arm's-length (category A), non-interfamily deeds with no price floor, so they include deeds recorded without a price (common in non-disclosure states). The main family definition is same surname plus estate/executor; interfamily deeds without a surname match enter only the wide definition in Table~\\ref{tab:volumes}. Interfamily trust deeds are mostly self-transfers or trust deeds with no surname link; Table~\\ref{tab:volumes} separates the trust-mediated family transfers. $^{a}$Median of strictly positive recorded prices. $^{b}$Share of events whose buyer mailing ZIP differs from the property ZIP, among events where both are observed.}\\end{minipage}",
  "\\end{table}"
)
writeLines(t1, path(tables_dir, "T1_taxonomy.tex"))
message("Wrote T1")

# ---- T2: volumes by year ---------------------------------------------------
headline <- read_csv(path(tables_out_dir, "fact_headline_ratios.csv"),
                     show_col_types = FALSE)
t2_rows <- headline |>
  arrange(sale_year) |>
  mutate(row = glue("{sale_year} & {fmt_n(market)} & {fmt_n(fam_conservative)} & ",
                    "{fmt_n(fam_broad)} & {fmt_n(fam_wide)} & ",
                    "{fmt_n(trust_clear)}--{fmt_n(trust_clear + trust_ambig)} & ",
                    "{fmt_n(trust_self)} & ",
                    "{sprintf('%.2f', ratio_broad)} \\\\")) |>
  pull(row)

t2 <- c(
  "\\begin{table}[!t]\\centering",
  "\\caption{The parallel housing market: annual volumes under alternative family definitions}",
  "\\label{tab:volumes}",
  "{\\scriptsize\\setlength{\\tabcolsep}{3pt}",
  "\\begin{tabular}{lrrrrrrr}",
  "\\toprule",
  " & Market & Same & Same surname & Same surname & Family trust & Trust & Family:market \\\\",
  "Year & sales & surname & + estate (main) & + estate + other$^{a}$ & transfers$^{b}$ & self or unlinked$^{c}$ & ratio (main) \\\\",
  "\\midrule",
  t2_rows,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Residential deed events. The main family definition (``same surname + estate'') counts interfamily-flagged deeds with a buyer--seller surname match plus estate/executor deeds, and is used in all later analysis. $^{a}$Adds interfamily-flagged deeds without a surname match. $^{b}$Trust-mediated family transfers, lower--upper bound: lower = trust deeds whose trust shares a surname with the counterparty but names a different individual; upper adds trust-to-person deeds whose trust name carries no given name. $^{c}$Interfamily-flagged trust deeds classified as a person moving property to or from the person's own trust, plus those with no surname link; excluded from every family column. Ratio = main family definition / market sales.}\\end{minipage}",
  "\\end{table}"
)
writeLines(t2, path(tables_dir, "T2_volumes.tex"))
message("Wrote T2")

# ---- T3: post-transfer outcomes -------------------------------------------
haz <- read_csv(path(tables_out_dir, "hazard_sold_within.csv"),
                show_col_types = FALSE) |>
  filter(class %in% c("family_person", "family_estate",
                      "family_trust", "market_sale")) |>
  mutate(lab = class_labels[class]) |>
  arrange(match(class, c("market_sale", "family_person",
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

# ---- TA_raw22: raw sold-within-24-months cohort means (appendix) ----------
# Generated from 04's cell means (was hand-typed in appendix.tex).
raw_fam <- read_csv(path(tables_out_dir, "prop19_raw_2x2_fam.csv"), show_col_types = FALSE) |>
  mutate(group = "Family transfer")
raw_mkt <- read_csv(path(tables_out_dir, "prop19_raw_2x2_market.csv"), show_col_types = FALSE) |>
  mutate(group = "Market purchase")
raw22 <- bind_rows(raw_fam, raw_mkt)
stopifnot(nrow(raw22) == 8L, all(raw22$n > 0))
perm_ddd <- read_csv(path(tables_out_dir, "ref_perm_ddd.csv"), show_col_types = FALSE)

raw22_wide <- raw22 |>
  mutate(period = if_else(post == 1, "post", "pre")) |>
  pivot_wider(id_cols = c(group, ca), names_from = period, values_from = c(sold24, n)) |>
  mutate(change = sold24_post - sold24_pre,
         region = if_else(ca == 1, "California", "Rest of US")) |>
  arrange(group, desc(ca))
did_of <- function(g) {
  d <- filter(raw22_wide, group == g)
  d$change[d$ca == 1] - d$change[d$ca == 0]
}
did_fam <- did_of("Family transfer")
did_mkt <- did_of("Market purchase")
fmt_pp <- function(x) sprintf("$%s%.1f$pp", if_else(x < 0, "-", "+"), abs(100 * x))
fmt_n_tex <- function(x) gsub(",", "{,}", fmt_n(x), fixed = TRUE)

raw22_rows <- raw22_wide |>
  mutate(row = glue("{group} & {region} & {fmt_pct(sold24_pre)}\\% \\; (n={fmt_n_tex(n_pre)}) & ",
                    "{fmt_pct(sold24_post)}\\% \\; (n={fmt_n_tex(n_post)}) & {fmt_pp(change)} \\\\")) |>
  pull(row)

ta_raw22 <- c(
  "\\begin{table}[!ht]\\centering",
  "\\caption{Sold on open market within 24 months: raw cohort means}",
  "\\label{tab:raw22}",
  "{\\small",
  "\\begin{tabular}{llrrr}",
  "\\toprule",
  "Cohort group & Region & Pre (2017m1--2018m12) & Post (2021m7--2022m6) & Change \\\\",
  "\\midrule",
  raw22_rows,
  "\\bottomrule",
  "\\end{tabular}}",
  paste0("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} ",
         "Non-corporate recipients only. Family transfer = same-surname interfamily and ",
         "estate/executor deeds (same-person retitles and trust deeds excluded); market ",
         "purchases carry no price floor. Raw difference-in-differences: family ",
         fmt_pp(did_fam), ", market ", fmt_pp(did_mkt), "; raw triple difference ",
         fmt_pp(did_fam - did_mkt), ". The cell-level regression triple difference is ",
         fmt_pp(perm_ddd$ca_coef), " (state-placebo $p = ", sprintf("%.2f", perm_ddd$perm_p),
         "$).}\\end{minipage}"),
  "\\end{table}"
)
writeLines(ta_raw22, path(tables_dir, "TA_raw22.tex"))
message("Wrote TA_raw22")

# ---- T4: Prop 19 regressions ----------------------------------------------
vol <- readRDS(path(out_dir, "prop19_did_volume.rds"))
ddd_vol <- readRDS(path(out_dir, "prop19_ddd_volume.rds"))   # headline DDD (07)
haz19 <- readRDS(path(out_dir, "prop19_ddd_hazard.rds"))
abs19 <- readRDS(path(out_dir, "prop19_absentee_did.rds"))

# State-placebo rank p-value for the headline triple difference
ddd_perm_p <- sprintf("%.3f", ddd_vol$summary$perm_p)

etable(
  vol$fam, vol$market_placebo, ddd_vol$model, haz19$fam, haz19$market_placebo, abs19,
  dict = c("ca:post" = "CA $\\times$ Post",
           "ca:post:fam" = "CA $\\times$ Post $\\times$ Family",
           "state" = "State", "sale_year" = "Year", "ym" = "Cohort month",
           "grp" = "deed group"),
  order = c("Family", "Post"),
  depvar = FALSE,
  headers = list(
    c("Log volume", "Log volume", "Log volume", "Sold within 24 months",
      "Sold within 24 months", "Absentee recipient"),
    c("Family transfers", "Market sales", "Family vs. market", "Family transfers",
      "Market purchases", "Family transfers")
  ),
  extralines = list("State-placebo $p$ (DDD)" = c("", "", ddd_perm_p, "", "", "")),
  signif.code = NA,
  fitstat = ~ n + r2,
  fontsize = "scriptsize",
  adjustbox = "max width = \\textwidth",   # six columns exceed the text width
  tex = TRUE,
  file = path(tables_dir, "T4_prop19.tex"),
  replace = TRUE,
  title = "Proposition 19: volume, selection, and composition",
  label = "tab:prop19"
)

# Notes go below the adjustbox so they wrap at text width instead of
# widening the box and shrinking the table.
t4_notes <- paste0("Columns (1)-(3): state-year counts, 2017-2019 versus 2022-2023. ",
                   "Column (3) stacks family and market counts with state and year effects ",
                   "that vary by deed group; its CA $\\times$ Post $\\times$ Family ",
                   "coefficient is the triple difference, equal to column (1) minus column (2). ",
                   "Columns (4)-(6): transfer-cohort cells, January 2017-December 2018 ",
                   "versus July 2021-June 2022, weighted by cell size. Standard errors are ",
                   "clustered by state and shown for scale; inference with one treated ",
                   "state uses state placebos.")
t4_path <- path(tables_dir, "T4_prop19.tex")
t4_lines <- readLines(t4_path)
end_table <- max(grep("\\end{table}", t4_lines, fixed = TRUE))
t4_lines <- append(t4_lines,
                   c("   \\par\\vspace{2pt}\\begin{minipage}{0.95\\textwidth}\\scriptsize",
                     paste0("   \\textit{Notes:} ", t4_notes),
                     "   \\end{minipage}"),
                   after = end_table - 1)
writeLines(t4_lines, t4_path)
message("Wrote T4")

message("Finished 05_tables at ", Sys.time())
