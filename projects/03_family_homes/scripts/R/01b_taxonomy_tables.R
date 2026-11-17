#======================================================================#
# Purpose : Count the deed categories of the events panel and write the two
#           definition tables of the paper: Table 1 (the taxonomy of deed
#           events, United States and California) and Table 2 (annual
#           volumes of family transfers by route).
# Name    : Saani Rawat
# Created : 11/16/2026
# Log     : 11/16/2026: first version, on the locked taxonomy (deed_cat,
#                       deed_subcat, fam_route) of 01_build_panel.R.
# Inputs  : data/derived/03_family_homes/events[_dev].parquet
# Outputs : _outputs/tables/deed_cells_year.csv        counts by cell and year
#           _outputs/tables/deed_categories.csv        the breakdown, pooled
#           _outputs/taxonomy/T1_taxonomy.tex          Table 1 (staged)
#           _outputs/taxonomy/T2_volumes.tex           Table 2 (staged)
# Settings: FAM_DEV=1  read the dev panel (VT, RI, CA); outputs get a _dev suffix
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

#============================================#
# Settings ----
#============================================#

dev_mode   <- Sys.getenv("FAM_DEV", "0") == "1"
sfx        <- ifelse(dev_mode, "_dev", "")
panel_path <- if (dev_mode) path(data_dir, "events_dev.parquet") else events_panel_path
tax_dir    <- path(out_dir, "taxonomy")
dir_create(tax_dir)

first_year   <- window_start_year        # 2007
last_year    <- full_year_end            # 2023
recent_years <- 2017:2023                # chain look-back complete; window of Table 1

#============================================#
# Counting deed events by cell ----
#============================================#

con <- open_corelogic_duckdb()
ev  <- glue("read_parquet('{sql_quote_path(panel_path)}')")

# one row per year, region and taxonomy cell. spouse_doc marks the interspousal document
# types, which only California (IT, IC) and Arizona (DS) record; exact_retitle marks the
# same-owner deeds caught by the exact name test, which sit outside the upper bound.
cells <- dbGetQuery(con, glue("
  SELECT sale_year,
         CASE WHEN state = 'CA' THEN 'CA' ELSE 'rest' END AS region,
         deed_cat, deed_subcat, coalesce(fam_route, '') AS fam_route,
         (coalesce(doc_type, '') IN ('IT','IC','DS'))::INT AS spouse_doc,
         (class = 'family_retitle')::INT AS exact_retitle,
         COUNT(*) AS n
  FROM {ev}
  WHERE sale_year BETWEEN {first_year} AND {last_year}
  GROUP BY ALL ORDER BY ALL")) %>%
  as_tibble() %>%
  mutate(n = as.numeric(n))
dbDisconnect(con, shutdown = TRUE)

assert_rows(cells, 100, "deed cells by year")
stopifnot(!anyNA(cells$deed_cat), !anyNA(cells$deed_subcat))
write_csv_strict(cells, path(tables_out_dir, paste0("deed_cells_year", sfx, ".csv")))
message("NOTE: ", format(sum(cells$n), big.mark = ","), " deed events, ",
        first_year, "-", last_year, ".")

#============================================#
# Rows of the taxonomy ----
#============================================#

# each table row is a label and a filter on the cells
tax_rows <- tribble(
  ~panel, ~label,                                                        ~keep,
  "A", "Direct deed, lifetime",                                          quote(fam_route == "lifetime"),
  "A", "Direct deed, at death: affidavit of death",                      quote(fam_route == "death_affidavit" & deed_subcat != "affidavit_trust_party"),
  "A", "At death: affidavit of death with a trust party",                quote(deed_subcat == "affidavit_trust_party"),
  "A", "Direct deed, at death: probate or estate deed",                  quote(fam_route == "probate"),
  "A", "Direct deed, other document",                                    quote(fam_route == "unknown"),
  "A", "Through a trust: chain-verified heir",                           quote(deed_subcat == "trust_heir_chain"),
  "A", "Through a trust: trust names one person, deeds to another",      quote(deed_subcat == "trust_named_other"),
  "A", "Through a trust: trust to a trust naming another person",        quote(deed_subcat == "trust_to_trust_other"),
  "A", "Total, family transfers",                                        quote(deed_cat == "family_transfer"),
  "M", "\\quad Memo, direct deeds: same surname",                        quote(deed_cat == "family_transfer" & deed_subcat == "same_surname"),
  "M", "\\quad Memo, direct deeds: different surname",                   quote(deed_cat == "family_transfer" & deed_subcat %in% c("cross_surname", "unknown")),
  "M", "\\quad Memo: spouse deeds by document type$^{a}$",               quote(deed_cat == "family_transfer" & spouse_doc == 1),
  "B", "Same owners on both sides",                                      quote(deed_subcat == "same_owners"),
  "B", "Same owners, and a new person joins title",                      quote(deed_subcat == "new_person_joins"),
  "B", "Entity on either side",                                          quote(deed_cat == "entity"),
  "C", "Into the owner's own trust",                                     quote(deed_subcat == "into_own_trust"),
  "C", "Trust back to its settlor",                                      quote(deed_subcat == "trust_to_settlor"),
  "C", "Trust to a trust with the same names",                           quote(deed_subcat == "trust_to_trust_same"),
  "C", "Surname-only trust to a same-surname person$^{b}$",              quote(deed_subcat == "trust_surname_only"),
  "C", "Trust to a person with another surname, no price$^{b}$",         quote(deed_subcat == "trust_to_unlinked_unpriced"),
  "C", "Other trust deeds$^{c}$",                                        quote(deed_subcat %in% c("into_trust_for_others", "into_trust_unlinked",
                                                                                                  "trust_to_trust_unclear", "trust_to_unlinked_priced")),
  "D", "Market sales",                                                   quote(deed_cat == "market_sale"),
  "D", "Other deeds, not arm's-length",                                  quote(deed_cat == "other"),
  "T", "All deed events",                                                quote(TRUE)
)

# every non-memo, non-total row is counted once: the rows partition the panel
part <- tax_rows %>% filter(!panel %in% c("M", "T"), label != "Total, family transfers")
hits <- sapply(part$keep, function(k) eval(k, cells))
stopifnot(all(rowSums(hits) == 1))

#' Sum of deed events in the cells a row keeps, by region, over a set of years.
count_row <- function(keep, years) {
  d <- cells %>% filter(sale_year %in% years, eval(keep, .))
  tibble(us = sum(d$n), ca = sum(d$n[d$region == "CA"]))
}

#============================================#
# Breakdown: every category, pooled ----
#============================================#

breakdown <- cells %>%
  group_by(deed_cat, deed_subcat, fam_route) %>%
  summarise(us_2007_2023      = sum(n),
            ca_2007_2023      = sum(n[region == "CA"]),
            us_per_year_recent = sum(n[sale_year %in% recent_years]) / length(recent_years),
            ca_per_year_recent = sum(n[sale_year %in% recent_years & region == "CA"]) / length(recent_years),
            .groups = "drop") %>%
  mutate(us_per_100 = 100 * us_2007_2023 / sum(us_2007_2023),
         ca_per_100 = 100 * ca_2007_2023 / sum(ca_2007_2023)) %>%
  arrange(deed_cat, desc(us_2007_2023))
write_csv_strict(breakdown, path(tables_out_dir, paste0("deed_categories", sfx, ".csv")))

#============================================#
# Table 1: the taxonomy of deed events ----
#============================================#

num  <- function(x) format(round(x), big.mark = ",", trim = TRUE)
per  <- function(x, total) formatC(100 * x / total, format = "f", digits = 1)
n_yr <- length(recent_years)

t1 <- tax_rows %>%
  mutate(cnt = map(keep, count_row, years = recent_years)) %>%
  unnest(cnt)
tot <- t1 %>% filter(panel == "T")
t1 <- t1 %>%
  mutate(line = glue("{ifelse(panel %in% c('M', 'T') | label == 'Total, family transfers', '', '\\\\quad ')}",
                     "{label} & {num(us / n_yr)} & {per(us, tot$us)} & ",
                     "{num(ca / n_yr)} & {per(ca, tot$ca)} \\\\"))

panel_head <- c(
  A = "\\multicolumn{5}{l}{\\emph{A. Family transfers: the measure}} \\\\",
  B = "\\addlinespace\\multicolumn{5}{l}{\\emph{B. Flagged, no trust, the seller stays on title or an entity is a party}} \\\\",
  C = "\\addlinespace\\multicolumn{5}{l}{\\emph{C. Flagged trust deeds that are not family transfers}} \\\\",
  D = "\\addlinespace\\multicolumn{5}{l}{\\emph{D. Not flagged}} \\\\")
body <- c(panel_head["A"], t1$line[t1$panel == "A"], t1$line[t1$panel == "M"],
          panel_head["B"], t1$line[t1$panel == "B"],
          panel_head["C"], t1$line[t1$panel == "C"],
          panel_head["D"], t1$line[t1$panel == "D"],
          "\\midrule", t1$line[t1$panel == "T"])

t1_notes <- paste0(
  "\\textit{Notes:} Residential deed events, one per parcel and signing date, average of ",
  min(recent_years), "--", max(recent_years), ". Every event is in exactly one row of Panels A to D; ",
  "memo rows repeat deeds already counted. ``Flagged'' means the vendor's interfamily indicator is set; ",
  "the indicator does not verify kinship. A family transfer is a recorded deed that moves a home, with no sale, ",
  "from one person, or that person's trust, to a different person, during life or at death. ",
  "Direct deeds have no trust and no entity on either side, and the names show two distinct people. ",
  "A deed through a trust takes title out of a trust and gives it to someone other than the trust's settlor: ",
  "an earlier deed moved the home from a person into the trust and the trust deeds it to a different person with the same surname ",
  "(chain-verified, ten-year look-back); or the trust names one person and deeds to a different person with that surname; ",
  "or two trusts share a name and each names a person the other does not. Lifetime gifts and distributions after a death are not separated for trust deeds. ",
  "An affidavit of death counts whether or not a trust is a party, so a home whose trust later deeds it to an heir can be counted twice. ",
  "$^{a}$Identified only in California and Arizona; spouse deeds elsewhere sit in the other rows. ",
  "$^{b}$The names cannot say whether the recipient is an heir or the settlor, or whether the parties are related; these two rows are the upper bound of the trust route. ",
  "$^{c}$Into a trust for others, into a trust with no surname link, trust to trust with unclear names, and priced or arm's-length deeds from a trust to a person with another surname.")

t1_tex <- c(
  "\\begin{table}[!t]\\centering",
  glue("\\caption{{The taxonomy of deed events: average annual deeds, {min(recent_years)}--{max(recent_years)}}}"),
  "\\label{tab:taxonomy}",
  "{\\footnotesize\\setlength{\\tabcolsep}{4pt}",
  "\\begin{adjustbox}{max width=\\textwidth}\\begin{tabular}{lrrrr}",
  "\\toprule",
  " & \\multicolumn{2}{c}{United States} & \\multicolumn{2}{c}{California} \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Category & Deeds per year & Per 100 deeds & Deeds per year & Per 100 deeds \\\\",
  "\\midrule", body,
  "\\bottomrule\\end{tabular}\\end{adjustbox}}",
  glue("\\begin{{minipage}}{{0.97\\textwidth}}\\vspace{{2pt}}{{\\scriptsize {t1_notes}}}\\end{{minipage}}"),
  "\\end{table}")
writeLines(t1_tex, path(tax_dir, paste0("T1_taxonomy", sfx, ".tex")))

#============================================#
# Table 2: annual volumes ----
#============================================#

# upper bound: the measure, plus flagged same-owner (not the exact retitles) and entity
# deeds, plus the two trust cells the names cannot settle
in_upper <- quote(deed_cat == "family_transfer" | deed_cat == "entity" |
                    (deed_cat == "same_owner" & exact_retitle == 0) |
                    deed_subcat %in% family_trust_upper_subcats)

t2 <- cells %>%
  group_by(sale_year) %>%
  summarise(all_deeds  = sum(n),
            market     = sum(n[deed_cat == "market_sale"]),
            family     = sum(n[deed_cat == "family_transfer"]),
            lifetime   = sum(n[fam_route %in% c("lifetime", "unknown")]),
            affidavit  = sum(n[fam_route == "death_affidavit"]),
            probate    = sum(n[fam_route == "probate"]),
            trust      = sum(n[fam_route == "trust"]),
            fam_us     = 100 * family / all_deeds,
            fam_ca     = 100 * sum(n[deed_cat == "family_transfer" & region == "CA"]) / sum(n[region == "CA"]),
            upper_us   = 100 * sum(n[eval(in_upper)]) / all_deeds,
            af_trust   = sum(n[deed_subcat == "affidavit_trust_party"]),
            .groups = "drop")
stopifnot(all(abs(t2$family - (t2$lifetime + t2$affidavit + t2$probate + t2$trust)) < 0.5))
write_csv_strict(t2, path(tables_out_dir, paste0("deed_volumes_year", sfx, ".csv")))

dec <- function(x) formatC(x, format = "f", digits = 1)
t2_lines <- t2 %>%
  mutate(yr = ifelse(sale_year == 2018, "2018$^{c}$", as.character(sale_year)),
         line = glue("{yr} & {num(all_deeds)} & {num(market)} & {num(family)} & {num(lifetime)} & ",
                     "{num(affidavit)} & {num(probate)} & {num(trust)} & {dec(fam_us)} & {dec(fam_ca)} & ",
                     "{dec(upper_us)} & {num(af_trust)} \\\\")) %>%
  pull(line)

pooled <- cells %>%
  summarise(us = 100 * sum(n[deed_cat == "family_transfer"]) / sum(n),
            ca = 100 * sum(n[deed_cat == "family_transfer" & region == "CA"]) / sum(n[region == "CA"]),
            upper = 100 * sum(n[eval(in_upper)]) / sum(n))

t2_notes <- paste0(
  "\\textit{Notes:} Residential deed events, one per parcel and signing date, by calendar year of signing. ",
  "Family transfers are the measure of Table~\\ref{tab:taxonomy}; the four route columns sum to it ",
  "(``lifetime'' includes the few direct deeds whose document type gives no route). ",
  "$^{a}$The measure plus flagged deeds with the same owner or an entity on both sides and the two trust rows the names cannot settle. ",
  "$^{b}$Chain-verified deeds need a ten-year look-back, complete only from 2017, so the trust column understates the route before 2017. ",
  "$^{c}$From 2018 the vendor records a trust party on many affidavits of death, mostly in California. ",
  "The affidavit column counts every affidavit, and the last column shows how many of them carry a trust party.")

t2_tex <- c(
  "\\begin{table}[!t]\\centering",
  "\\caption{Annual deed volumes and family transfers by route}",
  "\\label{tab:volumes}",
  "{\\scriptsize\\setlength{\\tabcolsep}{2.5pt}",
  "\\begin{adjustbox}{max width=\\textwidth}\\begin{tabular}{lrrrrrrrrrrr}\\toprule",
  " & & & & \\multicolumn{4}{c}{Family transfers by route} & \\multicolumn{3}{c}{Per 100 deeds} & Of affidavits: \\\\",
  "\\cmidrule(lr){5-8}\\cmidrule(lr){9-11}",
  "Year & All deeds & Market sales & Family & Lifetime & Affidavit & Probate & Trust$^{b}$ & US & CA & Upper, US$^{a}$ & trust party \\\\",
  "\\midrule", t2_lines, "\\midrule",
  glue("{first_year}--{last_year} & {num(sum(t2$all_deeds))} & {num(sum(t2$market))} & {num(sum(t2$family))} & ",
       "{num(sum(t2$lifetime))} & {num(sum(t2$affidavit))} & {num(sum(t2$probate))} & {num(sum(t2$trust))} & ",
       "{dec(pooled$us)} & {dec(pooled$ca)} & {dec(pooled$upper)} & {num(sum(t2$af_trust))} \\\\"),
  "\\bottomrule\\end{tabular}\\end{adjustbox}}",
  glue("\\begin{{minipage}}{{0.98\\textwidth}}\\vspace{{2pt}}{{\\scriptsize {t2_notes}}}\\end{{minipage}}"),
  "\\end{table}")
writeLines(t2_tex, path(tax_dir, paste0("T2_volumes", sfx, ".tex")))

#============================================#
# Appendix table: every cell of the taxonomy ----
#============================================#

# one row per cell, with the rule that puts a deed there. Direct family transfers are split
# by route and by whether the two parties share a surname.
same  <- quote(deed_subcat == "same_surname")
other <- quote(deed_subcat %in% c("cross_surname", "unknown"))
fam_cell <- function(route, rel) bquote(fam_route == .(route) & .(rel))

detail_rows <- tribble(
  ~panel, ~label, ~rule, ~keep,
  "A", "Lifetime deed, same surname",            "Grant, quitclaim or similar deed; two distinct people who share a surname",       fam_cell("lifetime", same),
  "A", "Lifetime deed, different surname",       "The same, with no surname match",                                                  fam_cell("lifetime", other),
  "A", "Affidavit of death, same surname",       "Affidavit of death; the survivor shares the decedent's surname",                   fam_cell("death_affidavit", same),
  "A", "Affidavit of death, different surname",  "The same, with no surname match",                                                  fam_cell("death_affidavit", other),
  "A", "Affidavit of death, trust party",        "Affidavit of death with a trust as a party",                                       quote(deed_subcat == "affidavit_trust_party"),
  "A", "Probate or estate deed, same surname",   "Executor's, administrator's or personal representative's deed, deed of distribution, or an estate keyword", fam_cell("probate", same),
  "A", "Probate or estate deed, different surname", "The same, with no surname match",                                               fam_cell("probate", other),
  "A", "Other document, same surname",           "Correction, re-recording or unclassified document type",                           fam_cell("unknown", same),
  "A", "Other document, different surname",      "The same, with no surname match",                                                  fam_cell("unknown", other),
  "A", "Trust to heir, chain-verified",          "A person deeded the home into a trust; the trust deeds it to a different person with that surname (ten-year look-back)", quote(deed_subcat == "trust_heir_chain"),
  "A", "Trust names one person, deeds to another", "The trust names one person and deeds to a different person with that surname",   quote(deed_subcat == "trust_named_other"),
  "A", "Trust to a trust naming another person", "Two trusts share a name and each names a person the other does not",               quote(deed_subcat == "trust_to_trust_other"),
  "B", "Same owners",                            "The buyers are the sellers: an exact retitle or a name variant",                   quote(deed_subcat == "same_owners"),
  "B", "Same owners, a new person joins",        "A seller stays on title and a buyer is nobody on the seller side",                 quote(deed_subcat == "new_person_joins"),
  "B", "Entity",                                 "A company, lender, agency or placeholder name on either side",                     quote(deed_cat == "entity"),
  "C", "Into the owner's own trust",             "A person deeds to a trust that carries the person's name",                         quote(deed_subcat == "into_own_trust"),
  "C", "Trust back to its settlor",              "A trust deeds to the person it names, or to the person who funded it",             quote(deed_subcat == "trust_to_settlor"),
  "C", "Trust to a trust, same names",           "Two trusts that name the same people",                                             quote(deed_subcat == "trust_to_trust_same"),
  "C", "Surname-only trust to a person$^{a}$",   "A trust that names no individual deeds to a person with its surname; no chain",    quote(deed_subcat == "trust_surname_only"),
  "C", "Trust to another surname, no price$^{a}$", "A trust deeds to a person with another surname; no price, not arm's-length",     quote(deed_subcat == "trust_to_unlinked_unpriced"),
  "C", "Trust to another surname, priced",       "The same, with a price or an arm's-length code",                                   quote(deed_subcat == "trust_to_unlinked_priced"),
  "C", "Into a trust for others",                "A person deeds to a trust that names someone else, or a children's or irrevocable trust", quote(deed_subcat == "into_trust_for_others"),
  "C", "Into a trust, no surname link",          "A person deeds to a trust with another name",                                      quote(deed_subcat == "into_trust_unlinked"),
  "C", "Trust to a trust, unclear",              "Two trusts with no shared name, or names that do not settle it",                   quote(deed_subcat == "trust_to_trust_unclear"),
  "D", "Market sale",                            "Arm's-length category, not flagged; no price floor",                               quote(deed_cat == "market_sale"),
  "D", "Other deed, not arm's-length",           "Not flagged, not arm's-length",                                                    quote(deed_subcat == "other_nonarms"),
  "D", "Estate deed, not flagged",               "An estate keyword in a name, not flagged, not arm's-length",                       quote(deed_subcat == "estate_noninterfam")
)

# the rows partition the panel: every cell is counted exactly once
hits_d <- sapply(detail_rows$keep, function(k) eval(k, cells))
stopifnot(all(rowSums(hits_d) == 1))

all_years <- first_year:last_year
td <- detail_rows %>%
  mutate(cnt = map(keep, count_row, years = all_years)) %>%
  unnest(cnt)
tot_d <- count_row(quote(TRUE), all_years)
stopifnot(abs(sum(td$us) - tot_d$us) < 0.5, abs(sum(td$ca) - tot_d$ca) < 0.5)
write_csv_strict(td %>% transmute(panel, label, rule, us_2007_2023 = us, ca_2007_2023 = ca,
                                  us_per_100 = 100 * us / tot_d$us, ca_per_100 = 100 * ca / tot_d$ca),
                 path(tables_out_dir, paste0("deed_taxonomy_detail", sfx, ".csv")))

per2 <- function(x, total) formatC(100 * x / total, format = "f", digits = 2)
td <- td %>%
  mutate(line = glue("\\hspace{{1em}}{label} & {rule} & {num(us)} & {per2(us, tot_d$us)} & ",
                     "{num(ca)} & {per2(ca, tot_d$ca)} \\\\"))
sub_line <- function(p, text) {
  d <- td %>% filter(panel == p)
  glue("\\addlinespace\\multicolumn{{2}}{{l}}{{\\emph{{{text}}}}} & {num(sum(d$us))} & {per2(sum(d$us), tot_d$us)} & ",
       "{num(sum(d$ca))} & {per2(sum(d$ca), tot_d$ca)} \\\\")
}
detail_body <- c(
  sub_line("A", "A. Family transfers: the measure"),                    td$line[td$panel == "A"],
  sub_line("B", "B. Flagged, no trust, not a transfer"),                td$line[td$panel == "B"],
  sub_line("C", "C. Flagged trust deeds that are not family transfers"), td$line[td$panel == "C"],
  sub_line("D", "D. Not flagged"),                                      td$line[td$panel == "D"],
  "\\midrule",
  glue("\\multicolumn{{2}}{{l}}{{All deed events}} & {num(tot_d$us)} & 100.00 & {num(tot_d$ca)} & 100.00 \\\\"))

detail_notes <- paste0(
  "\\textit{Notes:} Residential deed events, one per parcel and signing date, pooled over ",
  first_year, "--", last_year, ". Every event is in exactly one row; the panel rows give the sum of their cells. ",
  "``Flagged'' means the vendor's interfamily indicator is set. Rules are applied in the order shown within a panel, ",
  "and a chain-verified heir stays an heir whatever the name rule says. ",
  "Chain-verified deeds need a ten-year look-back, complete only from 2017, so their pooled count understates the cell. ",
  "From 2018 the vendor records a trust party on many affidavits of death, which moves them from the two surname rows to the trust-party row. ",
  "$^{a}$The two cells added to the measure in the upper bound of the trust route.")

detail_tex <- c(
  "\\begin{table}[!ht]\\centering",
  glue("\\caption{{Every cell of the deed taxonomy, {first_year}--{last_year}}}"),
  "\\label{tab:taxonomy_detail}",
  "{\\scriptsize\\setlength{\\tabcolsep}{3pt}",
  "\\begin{adjustbox}{max width=\\textwidth}\\begin{tabular}{>{\\raggedright\\arraybackslash}p{5.6cm}>{\\raggedright\\arraybackslash}p{6.8cm}rrrr}",
  "\\toprule",
  " & & \\multicolumn{2}{c}{United States} & \\multicolumn{2}{c}{California} \\\\",
  "\\cmidrule(lr){3-4}\\cmidrule(lr){5-6}",
  "Cell & Rule & Deeds & Per 100 & Deeds & Per 100 \\\\",
  "\\midrule", detail_body,
  "\\bottomrule\\end{tabular}\\end{adjustbox}}",
  glue("\\begin{{minipage}}{{0.98\\textwidth}}\\vspace{{2pt}}{{\\scriptsize {detail_notes}}}\\end{{minipage}}"),
  "\\end{table}")
writeLines(detail_tex, path(tax_dir, paste0("TA_taxonomy_detail", sfx, ".tex")))

message("NOTE: family transfers ", first_year, "-", last_year, ": ",
        format(sum(t2$family), big.mark = ","), " (", dec(pooled$us), " per 100 deed events; California ",
        dec(pooled$ca), "; upper bound ", dec(pooled$upper), ").")
message("Wrote: ", path(tax_dir, paste0("T1_taxonomy", sfx, ".tex")), " and T2_volumes", sfx, ".tex")
