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

# 11/04/2026: restored other interfamily to the main family proxy.
# 11/04/2026: removed state-placebo inference and synchronized current outputs.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20260609)

fmt_n <- function(x) format(round(x), big.mark = ",", scientific = FALSE, trim = TRUE)
fmt_pct <- function(x, d = 1) sprintf(paste0("%.", d, "f"), 100 * x)
fmt_usd <- function(x) paste0("\\$", format(round(x / 1000), big.mark = ",", trim = TRUE), "k")

class_labels <- c(
  family_all         = "Family transfers (main definition)",
  market_sale        = "Market sale (arm's length)",
  family_person      = "Family transfer: same surname",
  family_other       = "Interfamily, no surname match",
  family_estate      = "Family transfer: estate/executor",
  family_via_trust   = "Family transfer: through a trust, or affidavit with a trust party",
  family_retitle     = "Same-person retitle (co-owner change)",
  family_samep_entity = "Same-person or entity deed (not in main family)",
  family_trust       = "Interfamily trust deed",
  other_nonarms      = "Other non-arm's-length",
  estate_noninterfam = "Estate deed (non-interfamily)"
)

# Tables 1 and 2 (taxonomy and annual volumes) are written by 01b_taxonomy_tables.R.
headline <- read_csv(path(tables_out_dir, "fact_headline_ratios.csv"),
                     show_col_types = FALSE)

# Generated descriptive macros keep the manuscript tied to this run.
state_counts <- read_csv(path(tables_out_dir, "fact_state_class_2007_2023.csv"),
                         show_col_types = FALSE) %>%
  filter(state %in% c(state.abb, "DC"))
national_share <- sum(state_counts$fam_broad) / sum(state_counts$n_all)
ca_counts <- state_counts %>% filter(state == "CA")
ut_counts <- state_counts %>% filter(state == "UT")
descriptive_numbers <- c(
  DeedFamilyMillion = sprintf("%.1f", sum(headline$fam_broad) / 1e6),
  DeedNationalShare = sprintf("%.1f", 100 * national_share),
  DeedNarrowShare = sprintf("%.1f", 100 * sum(headline$fam_narrow) / sum(headline$n_all)),
  DeedShareMin = sprintf("%.1f", 100 * min(headline$share_family_all)),
  DeedShareMax = sprintf("%.1f", 100 * max(headline$share_family_all)),
  DeedCAShare = sprintf("%.1f", 100 * ca_counts$fam_broad / ca_counts$n_all),
  DeedUTShare = sprintf("%.1f", 100 * ut_counts$fam_broad / ut_counts$n_all),
  DeedCARelative = sprintf("%.1f", (ca_counts$fam_broad / ca_counts$n_all) / national_share),
  # upper bound including same-person and entity deeds (earlier class-based proxy)
  DeedInclNationalShare = sprintf("%.1f", 100 * sum(state_counts$fam_incl_samep) / sum(state_counts$n_all)),
  DeedInclCAShare = sprintf("%.1f", 100 * ca_counts$fam_incl_samep / ca_counts$n_all)
)
writeLines(paste0("\\newcommand{\\", names(descriptive_numbers), "}{",
                  descriptive_numbers, "}"), path(tables_dir, "descriptive_numbers.tex"))


# ---- T3: post-transfer outcomes -------------------------------------------
haz <- read_csv(path(tables_out_dir, "hazard_sold_within.csv"),
                show_col_types = FALSE) |>
  filter(class %in% c("family_all", "family_person", "family_estate", "family_other",
                      "family_via_trust", "family_trust", "market_sale")) |>
  mutate(lab = class_labels[class]) |>
  arrange(match(class, c("market_sale", "family_all", "family_person",
                         "family_other", "family_estate", "family_via_trust", "family_trust")))

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
  "\\begin{minipage}{0.9\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Share of events followed by an arm's-length market sale of the same parcel within the stated horizon. The main family row pools the same-surname, estate/executor, and other interfamily component rows; these displayed rows overlap and must not be added. All cohorts observed at least 66 months before the June 2024 censoring date.}\\end{minipage}",
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
         "Non-corporate recipients only. Family transfer = same-surname and other interfamily plus ",
         "estate/executor deeds (same-person retitles and trust deeds excluded); market ",
         "purchases carry no price floor. Raw difference-in-differences: family ",
         fmt_pp(did_fam), ", market ", fmt_pp(did_mkt), "; raw triple difference ",
         fmt_pp(did_fam - did_mkt), ". The cell-level regression triple difference is ",
         fmt_pp(unname(coef(readRDS(path(out_dir, "prop19_ddd_hazard.rds"))$ddd)["ca:post:fam"])),
         ".}\\end{minipage}"),
  "\\end{table}"
)
writeLines(ta_raw22, path(tables_dir, "TA_raw22.tex"))
message("Wrote TA_raw22")

# ---- T4: Prop 19 regressions ----------------------------------------------
vol <- readRDS(path(out_dir, "prop19_did_volume.rds"))
ddd_vol <- readRDS(path(out_dir, "prop19_ddd_volume.rds"))   # headline DDD (07)

# Log volume only. The resale (sold within 24 months) and absentee-recipient columns were
# removed on 11/18/2026: the post-reform cohort of family transfers is selected by the
# reform, and the mailing ZIP at the deed misclassifies heirs who have a year to move in.
etable(
  vol$fam, vol$market_placebo, ddd_vol$model,
  dict = c("ca:post" = "CA $\\times$ Post",
           "ca:post:fam" = "CA $\\times$ Post $\\times$ Family",
           "state" = "State", "sale_year" = "Year", "grp" = "deed group"),
  order = c("Family", "Post"),
  depvar = FALSE,
  headers = list(
    c("Family transfers", "Market sales", "Family vs. market")
  ),
  signif.code = NA,
  fitstat = ~ n + r2,
  fontsize = "scriptsize",
  tex = TRUE,
  file = path(tables_dir, "T4_prop19.tex"),
  replace = TRUE,
  title = "Proposition 19: the triple difference in deed volume",
  label = "tab:prop19"
)

# Notes go below the table so they wrap at text width.
t4_notes <- paste0("State-year counts, ", prop19_pre_label, " versus ", prop19_post_label, ". ",
                   "Column (3) stacks family and market counts with state and year effects ",
                   "that vary by deed group; its CA $\\times$ Post $\\times$ Family ",
                   "coefficient is the triple difference, equal to column (1) minus column (2). ",
                   "Standard errors are ",
                   "clustered by state and shown for scale; their reliability is limited ",
                   "with one treated state.")
# Panel B: the triple difference of column (3) by route of family transfer (07, part f)
ddd_route <- read_csv(path(tables_out_dir, "prop19_ddd_by_route.csv"), show_col_types = FALSE)
coef_se <- function(b, se) paste0(sub("-", "$-$", sprintf("%.3f", b), fixed = TRUE),
                                  " (", sprintf("%.3f", se), ")")
panel_b_rows <- ddd_route |>
  mutate(row = glue("      {label} & {fmt_n(ca_per_year_pre)} & {fmt_n(ca_per_year_post)} & ",
                    "{coef_se(did, did_se)} & {coef_se(ddd, ddd_se)} & {n_states} \\\\")) |>
  pull(row)
panel_b <- c(
  "   \\par\\vspace{8pt}",
  "   {\\scriptsize\\begin{tabular}{lrrccc}",
  "      \\multicolumn{6}{l}{\\emph{Panel B. Log volume by route of family transfer}} \\\\",
  "      \\midrule",
  "       & \\multicolumn{2}{c}{California deeds per year} & CA $\\times$ Post & CA $\\times$ Post $\\times$ Family & \\\\",
  "      \\cmidrule(lr){2-3}",
  paste0("      Route & ", prop19_pre_label, " & ", prop19_post_label,
         " & (family only) & (net of market sales) & States \\\\"),
  "      \\midrule",
  panel_b_rows,
  "      \\midrule \\midrule",
  "   \\end{tabular}}")
t4_notes <- paste0("Panel A. ", t4_notes,
                   " Panel B repeats columns (1) and (3) for each route of the family measure ",
                   "(Table~\\ref{tab:taxonomy}); its first row is the family measure of Panel A. ",
                   "A route enters with the states that record it in every year compared. ",
                   "Affidavits of death include those with a trust party.")
t4_path <- path(tables_dir, "T4_prop19.tex")
t4_lines <- readLines(t4_path)
end_table <- max(grep("\\end{table}", t4_lines, fixed = TRUE))
t4_lines <- append(t4_lines,
                   c(panel_b,
                     "   \\par\\vspace{2pt}\\begin{minipage}{0.95\\textwidth}\\scriptsize",
                     paste0("   \\textit{Notes:} ", t4_notes),
                     "   \\end{minipage}"),
                   after = end_table - 1)
# label the etable block as Panel A
first_rule <- min(grep("\\midrule \\midrule", t4_lines, fixed = TRUE))
t4_lines <- append(t4_lines, "         \\multicolumn{4}{l}{\\emph{Panel A. Log volume}} \\\\",
                   after = first_rule)
writeLines(sub("[[:blank:]]+$", "", t4_lines), t4_path)
message("Wrote T4")

message("Finished 05_tables at ", Sys.time())
