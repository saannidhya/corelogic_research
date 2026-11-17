# ============================================================
# 21: Sections 2-3 exhibits (Phase 7): taxonomy Tables 1-2, Fact 2 decomposition,
#     state correlates, Fact 4 reweighted resale row, number macros s23_numbers.tex
# Author: Saani Rawat
# Purpose: Adopt the panel-v2 taxonomy (relation, trigger, vehicle) in the national
#          facts. Stage 1 pulls counts from events.parquet (cached to csv); stage 2
#          builds the tables, the decomposition, the correlates and the macros.
#          New script (not an edit of 02_facts.R) so the heavy property join in 02 is
#          not rerun; 02, 03 and 05 outputs are left untouched.
# Inputs:  data/derived/03_family_homes/events.parquet; data/external/fhfa_state_hpi.parquet,
#          data/external/state_pop65_acs.parquet; _outputs/tables/validation_*_v2.csv,
#          bygroup_state_month_counts.csv, fact_validation_moments.csv
# Outputs: _outputs/section23/{tables,figures}/..., s23_numbers.tex, s23_number_sources.csv
# Run:     Rscript projects/03_family_homes/scripts/R/21_s23_exhibits.R   (S23_PULL=1 re-pulls)
# Log:     11/11/2026: created (Phase 7).
#          11/12/2026: main-measure class counts (SIIIClass*Main*) and the state ranked second
#          (SIIIRunnerUpState, SIIIRunnerUpShare) for Sections 2 and 3. Table text (approved
#          proofreading): "main family measure" for the national measure, "deed events", T3 caption.
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
suppressPackageStartupMessages(library(arrow))

s23_dir <- path(out_dir, "section23")
s23_tab <- path(s23_dir, "tables"); dir_create(s23_tab)
T <- function(f) path(tables_out_dir, f)

# ---- stage 1: counts from the panel (about 30 percent of RAM) ----------------
cells_file  <- path(s23_dir, "s23_cells_state_year.csv")
totals_file <- path(s23_dir, "s23_totals_state_year.csv")
spell_file  <- path(s23_dir, "s23_resale_cells.csv")
class_file  <- path(s23_dir, "s23_class_counts.csv")
if (identical(Sys.getenv("S23_PULL", "0"), "1") || !file_exists(cells_file) ||
    !file_exists(totals_file) || !file_exists(spell_file) || !file_exists(class_file)) {
  con <- open_corelogic_duckdb(memory_limit = "18GB", threads = 6L)
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  ev <- events_main_from(events_panel_path)   # family measure of 2026-11-12 (00_setup.R)
  cells <- dbGetQuery(con, glue("
    SELECT state, sale_year, class,
      coalesce(fam_relation, 'NA') AS fam_relation, coalesce(fam_relation_nm, 'NA') AS fam_relation_nm,
      coalesce(fam_trigger, 'NA') AS fam_trigger, coalesce(fam_vehicle, 'NA') AS fam_vehicle,
      CASE WHEN trust_chain = 'heir' AND trust_chain_gap_days <= 3652 THEN 1 ELSE 0 END AS heir10,
      COUNT(*) AS n
    FROM {ev}
    WHERE sale_year BETWEEN 2007 AND 2023 AND class LIKE 'family_%'
    GROUP BY ALL"))
  totals <- dbGetQuery(con, glue("
    SELECT state, sale_year, COUNT(*) AS n_all,
      SUM(CASE WHEN class = 'market_sale' THEN 1 ELSE 0 END) AS n_market
    FROM {ev} WHERE sale_year BETWEEN 2007 AND 2023 GROUP BY ALL"))
  write_csv_strict(cells, cells_file); write_csv_strict(totals, totals_file)
  # raw panel classes (the classes of Section 2, before the 2026-11-12 relabel): events and zero or
  # missing prices, 2007-2023, and all events of the extract (2024 partial year included)
  classes <- dbGetQuery(con, glue("
    SELECT class, COUNT(*) AS n_all_years,
      SUM(CASE WHEN sale_year BETWEEN 2007 AND 2023 THEN 1 ELSE 0 END) AS n_0723,
      SUM(CASE WHEN sale_year BETWEEN 2007 AND 2023 AND (amt IS NULL OR amt = 0) THEN 1 ELSE 0 END) AS n_zero_0723
    FROM read_parquet('{sql_quote_path(events_panel_path)}') GROUP BY class"))
  write_csv_strict(classes, class_file)

  # Fact 4 (state-and-cohort reweighting): next market sale within 12/24/36/60 months
  dbExecute(con, glue("
    CREATE TEMP TABLE c AS
    SELECT clip, state, sale_year, class,
      make_date(sale_raw // 10000, GREATEST(1, (sale_raw // 100) % 100), GREATEST(1, sale_raw % 100)) AS d
    FROM {ev}
    WHERE sale_year BETWEEN 2008 AND 2018
      AND class IN ('family_person','family_estate','family_other','market_sale')"))
  dbExecute(con, glue("
    CREATE TEMP TABLE m AS
    SELECT clip, make_date(sale_raw // 10000, GREATEST(1, (sale_raw // 100) % 100), GREATEST(1, sale_raw % 100)) AS md
    FROM {ev} WHERE class = 'market_sale'"))
  spell <- dbGetQuery(con, "
    WITH s AS (
      SELECT c.state, c.sale_year, (c.class = 'market_sale') AS is_market, c.d, MIN(m.md) AS nd
      FROM c LEFT JOIN m ON m.clip = c.clip AND m.md > c.d GROUP BY c.clip, c.state, c.sale_year, c.class, c.d)
    SELECT state, sale_year, is_market, COUNT(*) AS n,
      SUM(CASE WHEN nd <= d + INTERVAL 12 MONTH THEN 1 ELSE 0 END) AS s12,
      SUM(CASE WHEN nd <= d + INTERVAL 24 MONTH THEN 1 ELSE 0 END) AS s24,
      SUM(CASE WHEN nd <= d + INTERVAL 36 MONTH THEN 1 ELSE 0 END) AS s36,
      SUM(CASE WHEN nd <= d + INTERVAL 60 MONTH THEN 1 ELSE 0 END) AS s60
    FROM s GROUP BY ALL")
  write_csv_strict(spell, spell_file)
}
cells  <- read_csv(cells_file, show_col_types = FALSE) %>%
  mutate(across(c(fam_relation, fam_relation_nm, fam_trigger, fam_vehicle), ~ ifelse(is.na(.x), "NA", .x)))
totals <- read_csv(totals_file, show_col_types = FALSE)
spell  <- read_csv(spell_file, show_col_types = FALSE)
classes <- read_csv(class_file, show_col_types = FALSE)
stopifnot(nrow(cells) > 1000, nrow(totals) > 500, nrow(spell) > 500, nrow(classes) >= 8L)
message("cells: ", nrow(cells), "; totals: ", nrow(totals), "; spell cells: ", nrow(spell))

# ---- stage 2: definitions -------------------------------------------------------
yrs_recent <- 2017L:2023L
# Family measure of 2026-11-12 (00_setup.R): the panel was read through events_main_from(), so
# `class %in% family_classes` is the new main measure (same-person and entity deeds removed,
# spouse deeds kept); the removed deeds carry the label family_samep_label and are the upper
# bound "including same-person and entity deeds" when added back.
main_cls   <- family_classes
cells <- cells %>% mutate(is_ca = state == "CA", in_main = class %in% main_cls,
                          in_old = in_main | class == family_samep_label)

numbers <- list()
add_number <- function(name, value, source, digits = 1L) {
  stopifnot(length(value) == 1L, is.finite(value), grepl("^[A-Za-z]+$", name), !(name %in% names(numbers)))
  numbers[[name]] <<- tibble(macro = name, value = as.numeric(value), source = source,
    formatted = formatC(value, format = "f", digits = digits, big.mark = ","))
}
fmt0 <- function(x) formatC(round(x), format = "f", digits = 0, big.mark = ",")
pct1 <- function(x) formatC(100 * x, format = "f", digits = 1)

# ---- read precision (validation_read_precision_v2.csv, validation_chain_read_precision_v2.csv) ----
rp  <- read_csv(T("validation_read_precision_v2.csv"), show_col_types = FALSE)
rpc <- read_csv(T("validation_chain_read_precision_v2.csv"), show_col_types = FALSE)
rp_get <- function(cell, d = rp) 100 * d$share_correct[d$cell == cell]
prec <- c(spouse = rp_get("spouse_CA_IT"), parent_child = rp_get("parent_child_direct"),
          owner_retained = rp_get("owner_retained_nm_fuzzy"), cross_surname = rp_get("cross_surname_after_rule"),
          cross_before = rp_get("cross_surname_before_rule"), moved_same = rp_get("cross_surname_moved_to_owner_retained"),
          af = rp_get("death_affidavit_trust"), to_other = rp_get("trust_to_other_person"),
          unlinked = rp_get("trust_unlinked"), for_others = rp_get("into_trust_for_others"),
          settlor_a = rp_get("trust_to_self_2019"), settlor_b = rp_get("trust_to_self_JF2021"),
          heir = rp_get("chain_heir_cap10", rpc))
pfmt <- function(x) paste0(formatC(x, format = "f", digits = 0), "\\%")
S_rp <- "validation_read_precision_v2.csv; validation_chain_read_precision_v2.csv"
add_number("SIIIReadCrossSurname", prec[["cross_surname"]], S_rp, 0)
add_number("SIIIReadCrossSurnameN", rp$n_read[rp$cell == "cross_surname_after_rule"], S_rp, 0)
add_number("SIIIReadCrossSurnameCorrect", rp$n_correct[rp$cell == "cross_surname_after_rule"], S_rp, 0)
add_number("SIIIReadCrossBefore", prec[["cross_before"]], S_rp, 0)
add_number("SIIIReadMovedSamePerson", prec[["moved_same"]], S_rp, 0)
add_number("SIIIReadMovedSamePersonN", rp$n_read[rp$cell == "cross_surname_moved_to_owner_retained"], S_rp, 0)
add_number("SIIIReadIntoTrustOthers", prec[["for_others"]], S_rp, 0)
add_number("SIIIReadHeir", prec[["heir"]], S_rp, 0)
add_number("SIIIReadSpouse", prec[["spouse"]], S_rp, 0)
add_number("SIIIReadParentChild", prec[["parent_child"]], S_rp, 0)
add_number("SIIIReadOwnerRetained", prec[["owner_retained"]], S_rp, 0)
add_number("SIIIReadToOther", prec[["to_other"]], S_rp, 0)

# ---- Table 1 (taxonomy), 2017-2023 average annual deeds ------------------------------
rec <- cells %>% filter(sale_year %in% yrs_recent)
main_rec <- rec %>% filter(in_main)
tot <- function(d) c(nat = sum(d$n) / 7, ca = sum(d$n[d$is_ca]) / 7)
tabrow <- function(label, d, base, read = "--", shares = TRUE) {
  v <- tot(d)
  sh <- function(x, b) if (shares) pct1(x / b) else "--"
  sprintf("\\quad %s & %s & %s & %s & %s & %s \\\\", label, fmt0(v[["nat"]]), sh(v[["nat"]], base[["nat"]]),
          fmt0(v[["ca"]]), sh(v[["ca"]], base[["ca"]]), read)
}
base_main <- tot(main_rec)
rel_rows <- list(
  list("Spouse (document type; California and Arizona only)", main_rec %>% filter(fam_relation == "spouse"), pfmt(prec[["spouse"]])),
  list("Parent-child candidate", main_rec %>% filter(fam_relation == "parent_child_candidate"), pfmt(prec[["parent_child"]])),
  list("Cross-surname", main_rec %>% filter(fam_relation == "cross_surname"), pfmt(prec[["cross_surname"]])),
  list("Unknown", main_rec %>% filter(fam_relation == "unknown"), "--"))
trg_rows <- list(
  list("Death (death document or estate keyword)", main_rec %>% filter(fam_trigger == "death"), "--"),
  list("Lifetime", main_rec %>% filter(fam_trigger == "lifetime"), "--"),
  list("Unknown", main_rec %>% filter(fam_trigger == "unknown"), "--"))
trust_rec <- rec %>% filter(class == "family_trust")
base_tr <- tot(trust_rec)
veh <- function(v) trust_rec %>% filter(fam_vehicle == v)
settlor_read <- sprintf("%s--%s", formatC(min(prec[["settlor_a"]], prec[["settlor_b"]]), format = "f", digits = 0),
                        pfmt(max(prec[["settlor_a"]], prec[["settlor_b"]])))
veh_rows <- list(
  list("Into own trust", veh("into_own_trust"), "--"),
  list("Trust to a different person (name rule)", veh("trust_to_other_person"), pfmt(prec[["to_other"]])),
  list("\\quad of which chain-verified heir$^{b}$", trust_rec %>% filter(heir10 == 1), pfmt(prec[["heir"]])),
  list("Into a trust for others", veh("into_trust_for_others"), pfmt(prec[["for_others"]])),
  list("Trust to settlor", veh("trust_to_self"), settlor_read),
  list("Trust to trust", veh("trust_to_trust"), "--"),
  list("Trust, no surname link", veh("trust_unlinked"), pfmt(prec[["unlinked"]])),
  list("Affidavit of death with a trust party$^{c}$", veh("death_affidavit_trust"), pfmt(prec[["af"]])))
ret <- tot(rec %>% filter(class == "family_retitle"))
body1 <- c(
  "\\multicolumn{6}{l}{\\emph{A. Relation: main family measure}} \\\\",
  vapply(rel_rows, function(r) tabrow(r[[1]], r[[2]], base_main, r[[3]]), ""),
  sprintf("Total, main family measure & %s & 100.0 & %s & 100.0 & \\\\", fmt0(base_main[["nat"]]), fmt0(base_main[["ca"]])),
  tabrow("Memo: same-person or entity deeds, outside the main family measure$^{d}$",
         cells %>% filter(sale_year %in% yrs_recent, class == family_samep_label),
         base_main, pfmt(prec[["owner_retained"]]), shares = FALSE),
  "\\addlinespace\\multicolumn{6}{l}{\\emph{B. Trigger: main family measure}} \\\\",
  vapply(trg_rows, function(r) tabrow(r[[1]], r[[2]], base_main, r[[3]]), ""),
  "\\addlinespace\\multicolumn{6}{l}{\\emph{C. Vehicle: interfamily trust deeds, outside the main family measure}} \\\\",
  vapply(veh_rows, function(r) tabrow(r[[1]], r[[2]], base_tr, r[[3]]), ""),
  sprintf("Total, trust deeds & %s & 100.0 & %s & 100.0 & \\\\", fmt0(base_tr[["nat"]]), fmt0(base_tr[["ca"]])),
  "\\addlinespace\\multicolumn{6}{l}{\\emph{D. Same-person retitles, outside the main family measure}} \\\\",
  sprintf("\\quad Same-person retitle (co-owner change) & %s & -- & %s & -- & -- \\\\", fmt0(ret[["nat"]]), fmt0(ret[["ca"]])))
write_lines(c(
  "\\begin{table}[!t]\\centering",
  "\\caption{The taxonomy of family-type deeds: relation, trigger and vehicle, average annual deeds 2017--2023}",
  "\\label{tab:taxonomy}",
  "{\\footnotesize\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lrrrrr}", "\\toprule",
  " & \\multicolumn{2}{c}{United States} & \\multicolumn{2}{c}{California} & Read \\\\",
  "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
  "Cell & Deeds per year & Share (\\%) & Deeds per year & Share (\\%) & correct$^{a}$ \\\\", "\\midrule",
  body1, "\\bottomrule\\end{tabular}}",
  paste0("\\begin{minipage}{0.97\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Residential deed events, ",
         "deduplicated to one per parcel-date, average of 2017--2023. The main family measure is the same-surname, ",
         "estate/executor and other interfamily deeds of Section~\\ref{sec:data}, without the deeds in which the same person or an entity is on both sides; ",
         "shares in Panels A and B are of that measure, shares in Panel C of interfamily trust deeds. Relation, trigger and vehicle follow document types and name rules; ",
         "the vendor flag does not verify kinship. Spouse deeds are identified only by document type in California and Arizona, ",
         "so spouse deeds elsewhere sit in the other cells. ",
         "$^{a}$Share of a seeded read of about 100 deeds per cell (50 for trust to settlor) that the names settle as correct, scored against each cell's own label ",
         "(Appendix Table~\\ref{tab:readprec} gives the criterion). Spouse: two natural persons with a shared surname. Parent-child candidate: two different natural persons sharing a surname. ",
         "Cross-surname: two natural persons with genuinely different surnames, read on the deeds still labelled cross-surname after the same-person rule (\\SIIIReadCrossSurnameCorrect\\ of \\SIIIReadCrossSurnameN). ",
         "Same-person or entity: the seller is still on title in the buyer names. Trust cells: the trust route named in the row (chain-verified: the chain holds and the buyer is a different natural person with the settlor's surname). ",
         "The into-a-trust-for-others cell reads weakly and carries no headline. ",
         "$^{b}$An earlier deed moved the home from a person into a trust with no market sale in between, and the trust ",
         "later deeded it to a different person with the same surname; ten-year look-back, complete from 2017. ",
         "$^{c}$A change in how the vendor codes affidavits of death with a trust party in 2018 moves these deeds into the trust class ",
         "(Table~\\ref{tab:volumes}). $^{d}$In the earlier proxy and the upper bound, outside the main family measure; shares are of the main family measure's total and are not shown.}\\end{minipage}"),
  "\\end{table}"), path(s23_tab, "T1_taxonomy_v2.tex"))

# ---- Table 2 (annual volumes) -------------------------------------------------------
tb  <- read_csv(T("validation_trust_bounds_vs_table2_v2.csv"), show_col_types = FALSE)
yr <- cells %>% group_by(sale_year) %>% summarize(
  fam_main = sum(n[in_main]), fam_old = sum(n[in_old]), fam_old_ca = sum(n[in_old & is_ca]),
  fam_main_ca = sum(n[in_main & is_ca]),
  heir = sum(n[heir10 == 1]), af = sum(n[fam_vehicle == "death_affidavit_trust"]),
  af_ca = sum(n[fam_vehicle == "death_affidavit_trust" & is_ca]), .groups = "drop")
ty <- totals %>% group_by(sale_year) %>% summarize(n_all = sum(n_all), n_market = sum(n_market), .groups = "drop")
ty_ca <- totals %>% filter(state == "CA") %>% select(sale_year, ca_all = n_all)
t2 <- yr %>% left_join(ty, by = "sale_year") %>% left_join(ty_ca, by = "sale_year") %>%
  left_join(tb %>% select(sale_year, lower_old_trust_rel, upper_old_trust_rel), by = "sale_year") %>%
  mutate(per100 = 100 * fam_main / n_all, per100_ca = 100 * fam_main_ca / ca_all,
         per100_old = 100 * fam_old / n_all)
# pooled 2007-2023 checks against the pipeline's own headline macros (descriptive_numbers.tex,
# written by 02_facts/05_tables through family_classes and the same predicate)
dn <- read_lines(here::here("projects/03_family_homes/manuscript/tables/descriptive_numbers.tex"))
dn_get <- function(nm) as.numeric(sub(".*\\{([0-9.]+)\\}.*", "\\1", dn[grepl(paste0("\\\\", nm, "\\}"), dn)]))
pool_nat  <- 100 * sum(t2$fam_main) / sum(t2$n_all);  pool_old <- 100 * sum(t2$fam_old) / sum(t2$n_all)
pool_ca   <- 100 * sum(t2$fam_main_ca) / sum(t2$ca_all); pool_ca_old <- 100 * sum(t2$fam_old_ca) / sum(t2$ca_all)
stopifnot(abs(round(pool_nat, 1) - dn_get("DeedNationalShare")) < 0.051,
          abs(round(pool_old, 1) - dn_get("DeedInclNationalShare")) < 0.051,
          abs(round(pool_ca, 1) - dn_get("DeedCAShare")) < 0.051,
          abs(round(pool_ca_old, 1) - dn_get("DeedInclCAShare")) < 0.051)
message(sprintf("Pooled 2007-2023 per 100 deeds: main %.2f (pipeline %.1f), incl. same-person and entity %.2f (pipeline %.1f)",
                pool_nat, dn_get("DeedNationalShare"), pool_old, dn_get("DeedInclNationalShare")))
add_number("SIIIPoolMainNat", pool_nat, "s23 cells and totals", 1); add_number("SIIIPoolOldNat", pool_old, "s23 cells and totals", 1)
add_number("SIIIPoolMainCA", pool_ca, "s23 cells and totals", 1); add_number("SIIIPoolOldCA", pool_ca_old, "s23 cells and totals", 1)
t2_lines <- vapply(seq_len(nrow(t2)), function(i) {
  r <- t2[i, ]
  heir <- if (r$sale_year >= 2017) fmt0(r$heir) else "--"
  sprintf("%d%s & %s & %s & %s & %s & %.1f & %.1f & %.1f & %s--%s & %s & %s \\\\", r$sale_year,
          if (r$sale_year == 2018) "$^{e}$" else "", fmt0(r$n_all), fmt0(r$n_market), fmt0(r$fam_main), fmt0(r$fam_old),
          r$per100, r$per100_ca, r$per100_old, fmt0(r$lower_old_trust_rel), fmt0(r$upper_old_trust_rel), heir, fmt0(r$af))
}, "")
write_lines(c(
  "\\begin{table}[!t]\\centering",
  "\\caption{Annual deed volumes and the family share of all deed events}",
  "\\label{tab:volumes}",
  "{\\scriptsize\\setlength{\\tabcolsep}{2.5pt}",
  "\\begin{tabular}{lrrrrrrrrrr}\\toprule",
  " & All & Market & Family & Upper bound$^{a}$ & \\multicolumn{3}{c}{Per 100 deeds} & Trust-name & Chain-verified & Affidavit, \\\\",
  "Year & deeds & sales & (main) & (incl. same person) & Main, US & Main, CA & Upper, US & bounds$^{b}$ & trust to heir$^{c}$ & trust party$^{d}$ \\\\",
  "\\midrule", t2_lines, "\\bottomrule\\end{tabular}}",
  paste0("\\begin{minipage}{0.98\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Residential deed events, pooled within each calendar year. ",
         "The main family measure is the same-surname, estate/executor and other interfamily deeds without the deeds in which the same person or an entity is on both sides; ",
         "it includes spouse deeds, which are identified only in California and Arizona (Table~\\ref{tab:taxonomy}). ",
         "The vendor relationship flag does not verify kinship or inheritance. Per 100 deeds divides the family count by all deed events, ",
         "including market sales, trusts, retitles and other conveyances. ",
         "$^{a}$The upper bound adds back the same-person and entity deeds, the earlier proxy. ",
         "$^{b}$Trust-mediated family transfers under the name rule: the lower count and the upper count, which adds ambiguous trust-to-person matches. ",
         "$^{c}$Chain-verified trust-to-heir deeds, a subset of trust-to-person deeds, with a ten-year look-back that is complete only from 2017. ",
         "$^{d}$Affidavits of death with a trust party, in the trust class. $^{e}$From 2018 a vendor coding change records a trust party on these deeds, ",
         "almost all in California, and moves them out of the main family measure; the California series, and any series crossing 2018, are not comparable across the break. ",
         "These counts are operational classifications, not verified relationships. Market sales carry no price floor.}\\end{minipage}"),
  "\\end{table}"), path(s23_tab, "T2_volumes_v2.tex"))


# ---- headline macros (national and California, main measure) -----------------------------
S_c <- "events.parquet via s23_cells_state_year.csv"
rel_share <- function(d, rel) sum(d$n[d$fam_relation == rel]) / sum(d$n)
nat_m <- main_rec; ca_m <- main_rec %>% filter(is_ca)
for (r in list(c("ParentChild", "parent_child_candidate"), c("CrossSurname", "cross_surname"), c("Spouse", "spouse"))) {
  add_number(paste0("SIII", r[1], "ShareNat"), 100 * rel_share(nat_m, r[2]), S_c, 1)
  add_number(paste0("SIII", r[1], "ShareCA"), 100 * rel_share(ca_m, r[2]), S_c, 1)
}
# the deeds the 2026-11-12 rule moved out of the old proxy (same person or entity)
sp_rec <- rec %>% filter(class == family_samep_label); old_rec <- rec %>% filter(in_old)
add_number("SIIISamePEntityAvgNat", sum(sp_rec$n) / 7, S_c, 0)
add_number("SIIISamePEntityAvgCA", sum(sp_rec$n[sp_rec$is_ca]) / 7, S_c, 0)
add_number("SIIISamePEntityShareOfOldNat", 100 * sum(sp_rec$n) / sum(old_rec$n), S_c, 0)
add_number("SIIISamePEntityShareOfOldCA", 100 * sum(sp_rec$n[sp_rec$is_ca]) / sum(old_rec$n[old_rec$is_ca]), S_c, 0)
add_number("SIIIOldAvgNat", sum(old_rec$n) / 7, S_c, 0)
add_number("SIIIDeathShareNat", 100 * sum(nat_m$n[nat_m$fam_trigger == "death"]) / sum(nat_m$n), S_c, 1)
add_number("SIIILifetimeShareNat", 100 * sum(nat_m$n[nat_m$fam_trigger == "lifetime"]) / sum(nat_m$n), S_c, 1)
add_number("SIIIMainAvgNat", sum(nat_m$n) / 7, S_c, 0)
add_number("SIIIMainAvgCA", sum(ca_m$n) / 7, S_c, 0)
add_number("SIIIHeirAvgNat", sum(rec$n[rec$heir10 == 1]) / 7, S_c, 0)
add_number("SIIITrustToOtherAvgNat", sum(rec$n[rec$fam_vehicle == "trust_to_other_person"]) / 7, S_c, 0)
add_number("SIIIAfTrustCATwentySeventeen", sum(cells$n[cells$fam_vehicle == "death_affidavit_trust" & cells$is_ca & cells$sale_year == 2017L]), S_c, 0)
add_number("SIIIAfTrustCATwentyEighteen", sum(cells$n[cells$fam_vehicle == "death_affidavit_trust" & cells$is_ca & cells$sale_year == 2018L]), S_c, 0)
add_number("SIIIAfTrustNatTwentySeventeen", sum(cells$n[cells$fam_vehicle == "death_affidavit_trust" & cells$sale_year == 2017L]), S_c, 0)
add_number("SIIIAfTrustNatTwentyEighteen", sum(cells$n[cells$fam_vehicle == "death_affidavit_trust" & cells$sale_year == 2018L]), S_c, 0)

# ---- Fact 2: decomposition of California's lead, 2007-2023 pooled -------------------------
# Comparable cells use the relation WITHOUT the document-type spouse line (fam_relation_nm),
# because spouse deeds are identified only in California and Arizona.
all_ca   <- sum(totals$n_all[totals$state == "CA"]);  all_rest <- sum(totals$n_all[totals$state != "CA"])
mkt_ca   <- sum(totals$n_market[totals$state == "CA"]); mkt_rest <- sum(totals$n_market[totals$state != "CA"])
mm <- cells %>% filter(in_main)
per100 <- function(d, den_ca, den_rest) c(ca = 100 * sum(d$n[d$is_ca]) / den_ca, rest = 100 * sum(d$n[!d$is_ca]) / den_rest)
dec_row <- function(label, d) {
  v <- per100(d, all_ca, all_rest)
  tibble(label = label, ca = v[["ca"]], rest = v[["rest"]], gap = v[["ca"]] - v[["rest"]])
}
dec_rel <- bind_rows(
  dec_row("Parent-child candidate", mm %>% filter(fam_relation_nm == "parent_child_candidate")),
  dec_row("Cross-surname", mm %>% filter(fam_relation_nm == "cross_surname")),
  dec_row("Unknown", mm %>% filter(!(fam_relation_nm %in% c("parent_child_candidate", "cross_surname")))))
dec_tot <- dec_row("Total, main family measure", mm)
dec_samep <- dec_row("Memo: same-person or entity deeds, outside the main family measure", cells %>% filter(class == family_samep_label))
dec_old   <- dec_row("Memo: upper bound, main family measure plus same-person and entity deeds", cells %>% filter(in_old))
stopifnot(abs(sum(dec_rel$gap) - dec_tot$gap) < 1e-9)
dec_trg <- bind_rows(
  dec_row("Death", mm %>% filter(fam_trigger == "death")),
  dec_row("Lifetime", mm %>% filter(fam_trigger == "lifetime")),
  dec_row("Unknown", mm %>% filter(fam_trigger == "unknown")))
stopifnot(abs(sum(dec_trg$gap) - dec_tot$gap) < 1e-9)
dec_spouse <- dec_row("Memo: spouse deeds by document type (California and Arizona)",
                      mm %>% filter(fam_relation == "spouse"))
dec_af <- dec_row("Memo: affidavits of death in the trust class", cells %>% filter(fam_vehicle == "death_affidavit_trust"))
# the headline must agree with the manuscript's Fact 2 macros
chk <- read_lines(here::here("projects/03_family_homes/manuscript/tables/descriptive_numbers.tex"))
old_ca <- as.numeric(sub(".*\\{([0-9.]+)\\}.*", "\\1", chk[grepl("\\\\DeedCAShare\\}", chk)]))
stopifnot(length(old_ca) == 1L, abs(round(dec_tot$ca, 1) - old_ca) < 0.051)

dec_line <- function(r, bold = FALSE, memo = FALSE) {
  sh <- if (memo) "--" else pct1(r$gap / dec_tot$gap)
  sprintf("%s%s & %.1f & %.1f & %.1f & %s \\\\", if (bold) "" else "\\quad ", r$label, r$ca, r$rest, r$gap, sh)
}
write_lines(c(
  "\\begin{table}[!t]\\centering",
  "\\caption{What makes California's family share high: the gap by taxonomy cell, 2007--2023}",
  "\\label{tab:decomp}",
  "{\\small\\setlength{\\tabcolsep}{5pt}",
  "\\begin{tabular}{lrrrr}\\toprule",
  " & California & Other states & Gap & Share of gap \\\\",
  " & \\multicolumn{3}{c}{Deeds per 100 deed events} & (\\%) \\\\", "\\midrule",
  "\\multicolumn{5}{l}{\\emph{A. By relation (without the document-type spouse line)}} \\\\",
  vapply(seq_len(nrow(dec_rel)), function(i) dec_line(dec_rel[i, ]), ""),
  "\\multicolumn{5}{l}{\\emph{B. By trigger}} \\\\",
  vapply(seq_len(nrow(dec_trg)), function(i) dec_line(dec_trg[i, ]), ""),
  "\\midrule", dec_line(dec_tot, bold = TRUE),
  "\\addlinespace",
  dec_line(dec_samep, bold = TRUE, memo = TRUE),
  dec_line(dec_old, bold = TRUE, memo = TRUE),
  dec_line(dec_spouse, bold = TRUE, memo = TRUE),
  dec_line(dec_af, bold = TRUE, memo = TRUE),
  "\\bottomrule\\end{tabular}}",
  paste0("\\begin{minipage}{0.92\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} Main family measure (same-surname, ",
         "estate/executor and other interfamily deeds, without the deeds in which the same person or an entity is on both sides) ",
         "per 100 residential deed events, pooled over 2007--2023; other states are ",
         "the other forty-nine states and the District of Columbia. Panel A uses the relation rule without the spouse line, which can be ",
         "applied in every state; Panel B uses the trigger. Each panel sums to the total. The memo lines overlap the cells above: spouse deeds ",
         "are identified by document type in California and Arizona only, and affidavits of death with a trust party are in the trust class, ",
         "outside the main family measure; they are shown for scale. Counts are deed events, not homes.}\\end{minipage}"),
  "\\end{table}"), path(s23_tab, "T_s23_decomp.tex"))

S_d <- "events.parquet via s23_cells_state_year.csv, s23_totals_state_year.csv"
add_number("SIIICAPerHundred", dec_tot$ca, S_d, 1); add_number("SIIIRestPerHundred", dec_tot$rest, S_d, 1)
add_number("SIIIGapPerHundred", dec_tot$gap, S_d, 1)
for (r in list(c("ParentChild", 1), c("CrossSurname", 2), c("Unknown", 3))) {
  add_number(paste0("SIIIGapShare", r[1]), 100 * dec_rel$gap[as.integer(r[2])] / dec_tot$gap, S_d, 0)
}
add_number("SIIIGapSamePEntity", dec_samep$gap, S_d, 1)
add_number("SIIIGapOld", dec_old$gap, S_d, 1)
add_number("SIIIOldCAPerHundred", dec_old$ca, S_d, 1); add_number("SIIIOldRestPerHundred", dec_old$rest, S_d, 1)
add_number("SIIIGapShareLifetime", 100 * dec_trg$gap[2] / dec_tot$gap, S_d, 0)
add_number("SIIIGapShareDeath", 100 * dec_trg$gap[1] / dec_tot$gap, S_d, 0)
add_number("SIIISpousePerHundredCA", dec_spouse$ca, S_d, 1)
add_number("SIIISpouseGapShare", 100 * dec_spouse$gap / dec_tot$gap, S_d, 0)
add_number("SIIIFamPerMarketCA", 100 * sum(mm$n[mm$is_ca]) / mkt_ca, S_d, 0)
add_number("SIIIFamPerMarketRest", 100 * sum(mm$n[!mm$is_ca]) / mkt_rest, S_d, 0)
nm_ca  <- mm %>% filter(is_ca, fam_relation != "spouse")
add_number("SIIICAPerHundredNoSpouse", 100 * sum(nm_ca$n) / all_ca, S_d, 1)

# ---- Fact 2: state correlates from data already on disk (FHFA HPI, ACS 65+) -----------------
hpi <- read_parquet(here::here("data/external/fhfa_state_hpi.parquet")) %>% filter(year %in% c(2012L, 2023L)) %>%
  select(state = state_abbr, year, hpi) %>% pivot_wider(names_from = year, values_from = hpi, names_prefix = "y") %>%
  mutate(hpi_growth = log(y2023 / y2012))
p65 <- read_parquet(here::here("data/external/state_pop65_acs.parquet")) %>% filter(sale_year %in% 2018:2023) %>%
  group_by(state) %>% summarize(pop65 = mean(pop65), .groups = "drop")
st <- cells %>% filter(in_main, sale_year %in% 2018:2023, fam_relation != "spouse") %>% group_by(state) %>%
  summarize(fam = sum(n), .groups = "drop") %>%
  left_join(totals %>% filter(sale_year %in% 2018:2023) %>% group_by(state) %>%
              summarize(n_all = sum(n_all), .groups = "drop"), by = "state") %>%
  mutate(share = 100 * fam / n_all) %>% left_join(p65, by = "state") %>% left_join(hpi, by = "state") %>%
  mutate(old_per_deed = pop65 / (n_all / 6) * 100)     # residents 65+ per 100 annual deed events
stopifnot(sum(complete.cases(st[c("share", "old_per_deed", "hpi_growth")])) >= 49L)
st <- st %>% filter(complete.cases(share, old_per_deed, hpi_growth))
fit <- lm(share ~ old_per_deed + hpi_growth, data = st %>% filter(state != "CA"))
ca_row <- st %>% filter(state == "CA")
ca_pred <- predict(fit, newdata = ca_row)
write_csv_strict(st %>% mutate(fitted_noCA = predict(fit, newdata = st)), path(s23_dir, "s23_state_correlates.csv"))
S_s <- "fhfa_state_hpi.parquet; state_pop65_acs.parquet; s23 cells"
add_number("SIIIStateN", nrow(st), S_s, 0)
add_number("SIIICorrOld", cor(st$share, st$old_per_deed), S_s, 2)
add_number("SIIICorrHPI", cor(st$share, st$hpi_growth), S_s, 2)
add_number("SIIIStateRsq", summary(fit)$r.squared, S_s, 2)
add_number("SIIICAShareNoSpouse", ca_row$share, S_s, 1)
add_number("SIIICAPredShare", ca_pred, S_s, 1)
add_number("SIIIStateMeanShare", mean(st$share[st$state != "CA"]), S_s, 1)

# ---- Fact 4: market purchases reweighted to the family state-and-cohort mix ---------------
fam_w <- spell %>% filter(!is_market) %>% group_by(state, sale_year) %>% summarize(nf = sum(n), .groups = "drop")
mk <- spell %>% filter(is_market) %>% select(state, sale_year, m_n = n, m12 = s12, m24 = s24, m36 = s36, m60 = s60)
rw <- fam_w %>% inner_join(mk, by = c("state", "sale_year")) %>%
  summarize(across(c(m12, m24, m36, m60), ~ sum(nf * .x / m_n) / sum(nf)), covered = sum(nf))
fam_all <- spell %>% filter(!is_market) %>% summarize(n = sum(n), across(c(s12, s24, s36, s60), ~ sum(.x) / sum(n)))
stopifnot(rw$covered / fam_all$n > 0.999)
hz <- read_csv(T("hazard_sold_within.csv"), show_col_types = FALSE)
fam_old <- hz %>% filter(class == "family_all")
# the family row must reproduce Table 3 (same deeds, same definition)
stopifnot(abs(fam_old$sold_24m - fam_all$s24) < 0.002, abs(fam_old$sold_60m - fam_all$s60) < 0.002)
S_h <- "events.parquet via s23_resale_cells.csv"
add_number("SIIIMktRewSoldOne", 100 * rw$m12, S_h, 1); add_number("SIIIMktRewSoldTwo", 100 * rw$m24, S_h, 1)
add_number("SIIIMktRewSoldThree", 100 * rw$m36, S_h, 1); add_number("SIIIMktRewSoldFive", 100 * rw$m60, S_h, 1)
add_number("SIIIFamSoldTwoCheck", 100 * fam_all$s24, S_h, 1)
km <- read_csv(T("hazard_km_curves.csv"), show_col_types = FALSE) %>% filter(t == 120)
km_mkt <- 100 * km$surv[km$class == "market_sale"]; km_fam <- 100 * km$surv[km$class == "family_all"]
stopifnot(length(km_mkt) == 1L, length(km_fam) == 1L, round(km_fam) == 59)   # Table 3 / hazard_numbers.tex: 59 percent
add_number("SIIIMarketUnsoldTen", km_mkt, "hazard_km_curves.csv", 0)
t3 <- read_lines(here::here("projects/03_family_homes/manuscript/tables/T3_hazard.tex"))
i_mkt <- grep("^Market sale \\(arm's length\\)", t3)
stopifnot(length(i_mkt) == 1L)
new_row <- sprintf("Market purchases, reweighted to the family state and cohort mix & -- & %.1f & %.1f & %.1f & %.1f \\\\",
                   100 * rw$m12, 100 * rw$m24, 100 * rw$m36, 100 * rw$m60)
t3 <- append(t3, new_row, after = i_mkt)
t3 <- sub("Time to next open-market sale", "Time to next market sale", t3, fixed = TRUE)  # 11/12: approved proofreading TB2
t3 <- sub("(main definition)", "(main family measure)", t3, fixed = TRUE)                # 11/12: approved proofreading CO-1
t3 <- sub("\\\\textit\\{Notes:\\} Share of events",
          "\\\\textit{Notes:} The reweighted row gives each market purchase the weight of the family deeds in its state and cohort year. Share of events", t3)
write_lines(t3, path(s23_tab, "T3_hazard_v2.tex"))

# ---- appendix: read precision with the criterion each cell was scored against -----------------
crit <- c(
  spouse_CA_IT = "Spouse deed, California (document type)|Two natural persons with a shared surname",
  parent_child_direct = "Parent-child candidate, direct|Two different natural persons sharing a surname",
  owner_retained_nm_fuzzy = "Same person (name rule), in the earlier proxy|The seller is still on title in the buyer names",
  cross_surname_before_rule = "Cross-surname, before the same-person rule|Two natural persons with genuinely different surnames (earlier label)",
  cross_surname_after_rule = "Cross-surname, after the rule (main family measure)|Two natural persons with genuinely different surnames; the same reads, deeds still cross-surname",
  cross_surname_moved_to_owner_retained = "Deeds the rule moved to same person|Same person: the two names are one individual or one household retitling",
  cross_surname_stays = "Deeds that stay cross-surname (prototype read)|A transfer between distinct people; not the same test as the label",
  death_affidavit_trust = "Affidavit of death with a trust party|Document type AF with a trust as the buyer",
  trust_to_other_person = "Trust to a different person (name rule)|Trust to a surname-linked natural person",
  trust_unlinked = "Trust, no surname link|No family link between the parties",
  into_trust_for_others = "Into a trust for others|Trust named for a different individual or for children",
  into_trust_keyword = "Into a trust, keyword-promoted deeds|As above, promoted by a trust-name keyword",
  trust_to_self_2019 = "Trust to settlor, 2019|The buyer is the trust's named settlor",
  trust_to_self_JF2021 = "Trust to settlor, January--February 2021|The buyer is the trust's named settlor",
  chain_heir_cap10 = "Chain-verified trust to heir (10-year look-back)|The chain holds and the buyer is a different natural person with the earlier owner's surname",
  name_rule_t2o_not_heir = "Name-rule trust to other person, not chain-verified|The name rule holds on the names")
rp_all <- bind_rows(rp, rpc %>% filter(!(cell %in% rp$cell))) %>% filter(cell %in% names(crit))
rp_all <- rp_all[match(names(crit), rp_all$cell), ] %>% filter(!is.na(cell))
rp_rows <- vapply(seq_len(nrow(rp_all)), function(i) {
  lab <- strsplit(crit[[rp_all$cell[i]]], "|", fixed = TRUE)[[1]]
  sprintf("%s & %s & %d & %d & %.0f \\\\", lab[1], lab[2], rp_all$n_read[i], rp_all$n_uncertain[i], 100 * rp_all$share_correct[i])
}, "")
write_lines(c(
  "\\begin{table}[!ht]\\centering",
  "\\caption{Read precision of the taxonomy cells, and what each read scored}", "\\label{tab:readprec}",
  "{\\footnotesize\\setlength{\\tabcolsep}{3pt}\\begin{tabular}{p{4.3cm}p{6.4cm}rrr}\\toprule",
  "Cell & Criterion scored & Read & Uncertain & Correct (\\%) \\\\", "\\midrule", rp_rows, "\\bottomrule\\end{tabular}}",
  paste0("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} A seeded sample of deeds was read by hand ",
         "from the recorded party names and document types. Correct is the share of reads the names settle as meeting the criterion, ",
         "excluding uncertain reads. The cross-surname cell of the main family measure scores the label itself on the deeds that remain cross-surname after the same-person rule; ",
         "the deeds the rule moved to same person were read separately and are the same person in every read. ",
         "Samples are stored outside the repository and no names enter the paper.}\\end{minipage}"),
  "\\end{table}"), path(s23_tab, "TA_s23_readprec.tex"))
old1 <- read_lines(here::here("projects/03_family_homes/manuscript/tables/T1_taxonomy.tex"))
old1 <- sub("\\\\label\\{tab:taxonomy\\}", "\\\\label{tab:classmoments}", old1)
old1 <- sub("\\\\caption\\{Residential deed events by class, 2007--2023\\}",
            "\\\\caption{Residential deed events by class: observable moments, 2007--2023}", old1)
write_lines(old1, path(s23_tab, "TA_s23_classmoments.tex"))

# ---- scale wide tables to the text width (adjustbox is loaded in paper.tex) ----------------
for (f in c("T1_taxonomy_v2.tex", "T2_volumes_v2.tex", "T3_hazard_v2.tex", "T_s23_decomp.tex",
            "TA_s23_classmoments.tex")) {
  x <- read_lines(path(s23_tab, f))
  x <- sub("\\begin{tabular}", "\\begin{adjustbox}{max width=\\textwidth}\\begin{tabular}", x, fixed = TRUE)
  x <- sub("\\end{tabular}", "\\end{tabular}\\end{adjustbox}", x, fixed = TRUE)
  write_lines(x, path(s23_tab, f))
}

# ---- Section 2 class counts (macros for the class list), millions ---------------------------
cl <- function(k, col = "n_0723") classes[[col]][classes$class == k]
S_cl <- "s23_class_counts.csv (events.parquet, raw class)"
add_number("SIIIEventsAllM", sum(classes$n_all_years) / 1e6, S_cl, 1)
add_number("SIIIEventsSampleM", sum(classes$n_0723) / 1e6, S_cl, 1)
add_number("SIIIClassMarketM", cl("market_sale") / 1e6, S_cl, 1)
add_number("SIIIMarketZeroPrice", 100 * cl("market_sale", "n_zero_0723") / cl("market_sale"), S_cl, 1)
add_number("SIIIClassRetitleM", cl("family_retitle") / 1e6, S_cl, 1)
add_number("SIIIClassPersonM", cl("family_person") / 1e6, S_cl, 1)
add_number("SIIIClassEstate", cl("family_estate"), S_cl, 0)
add_number("SIIIClassOtherM", cl("family_other") / 1e6, S_cl, 1)
add_number("SIIIClassTrustM", cl("family_trust") / 1e6, S_cl, 1)
add_number("SIIIClassNonarmsM", cl("other_nonarms") / 1e6, S_cl, 1)
add_number("SIIITrustLowerM", sum(tb$lower_old_trust_rel[tb$sale_year %in% 2007:2023]) / 1e6, "validation_trust_bounds_vs_table2_v2.csv", 2)
add_number("SIIITrustUpperM", sum(tb$upper_old_trust_rel[tb$sale_year %in% 2007:2023]) / 1e6, "validation_trust_bounds_vs_table2_v2.csv", 2)

# 11/12/2026: the family classes of the class list after the same-person and entity removal of
# Section 2.3 (the main measure). `cells` was read through events_main_from(), so the removed
# deeds already carry family_samep_label and these classes hold only main-measure deeds.
S_cm <- "events.parquet via s23_cells_state_year.csv (main measure, 2007-2023)"
cl_main <- function(k) sum(cells$n[cells$class == k])
add_number("SIIIClassPersonMainM", cl_main("family_person") / 1e6, S_cm, 2)
add_number("SIIIClassOtherMainM", cl_main("family_other") / 1e6, S_cm, 2)
add_number("SIIIClassEstateMain", cl_main("family_estate"), S_cm, 0)
# the three classes must add up to the pipeline's headline count (descriptive_numbers.tex)
stopifnot(abs(round(sum(vapply(main_cls, cl_main, 0)) / 1e6, 1) - dn_get("DeedFamilyMillion")) < 0.051)

# ---- Fact 2: the state ranked second (fifty states and DC, main measure, 2007-2023) ----------
# 11/12/2026: replaces the Utah comparison, a leftover of the earlier proxy.
st_rank <- cells %>% filter(in_main, state %in% c(state.abb, "DC")) %>% group_by(state) %>%
  summarize(fam = sum(n), .groups = "drop") %>%
  inner_join(totals %>% group_by(state) %>% summarize(n_all = sum(n_all), .groups = "drop"), by = "state") %>%
  mutate(share = 100 * fam / n_all) %>% arrange(desc(share))
stopifnot(nrow(st_rank) == 51L, st_rank$state[1] == "CA")
# the ranking must agree with the file behind Figure 2 (fact_state_class_2007_2023.csv)
fs_rank <- read_csv(T("fact_state_class_2007_2023.csv"), show_col_types = FALSE) %>%
  filter(state %in% c(state.abb, "DC")) %>% mutate(share = 100 * fam_broad / n_all) %>% arrange(desc(share))
stopifnot(identical(fs_rank$state[1:2], st_rank$state[1:2]), abs(fs_rank$share[2] - st_rank$share[2]) < 1e-6)
runner_up <- st_rank$state[2]
runner_up_name <- if (runner_up == "DC") "the District of Columbia" else state.name[match(runner_up, state.abb)]
add_number("SIIIRunnerUpShare", st_rank$share[2], paste0(S_c, "; state ranked second: ", runner_up), 1)
message("State ranked second: ", runner_up_name, sprintf(" (%.2f per 100)", st_rank$share[2]))

# ---- share of the earlier proxy matched only through a single initial (c3b) --------------------
# 22_same_person_audit.R: c3b deeds over all same-person and entity deeds in the audited states
# (CA, OH, TX, 2017-19), times the owner-retained share of the earlier proxy, 2017-19, all states
# (same_person_panel_split.csv). Extrapolates the three-state initial-only share to the nation.
spo <- read_csv(T("same_person_owner_retained_split.csv"), show_col_types = FALSE)
psp <- read_csv(T("same_person_panel_split.csv"), show_col_types = FALSE) %>%
  filter(sale_year %in% 2017:2019, class %in% c("family_person", "family_other", "family_estate"))
c3b_share <- sum(spo$n[spo$mech == "c3b_initial_only_match"]) / sum(spo$n)
or_share  <- sum(psp$n[psp$fam_relation_nm == "owner_retained"]) / sum(psp$n)
add_number("SIIIInitialOnlyShareOfOld", 100 * c3b_share * or_share, "same_person_owner_retained_split.csv; same_person_panel_split.csv", 1)

# raw California affidavit counts (validation file, residential): total growth 2017 to 2018
afr <- read_csv(T("validation_ca_af_raw_by_year_v2.csv"), show_col_types = FALSE) %>% filter(residential == "Y")
add_number("SIIIAfRawTotalGrowth", 100 * (afr$n_af[afr$sale_year == 2018] / afr$n_af[afr$sale_year == 2017] - 1),
           "validation_ca_af_raw_by_year_v2.csv", 0)

# ---- export macros ------------------------------------------------------------------------
numbers_df <- bind_rows(bind_rows(numbers),
  # text macro (no numeric value): the name of the state ranked second
  tibble(macro = "SIIIRunnerUpState", value = NA_real_, source = paste0(S_c, "; state ranked second"),
         formatted = runner_up_name))
stopifnot(!anyDuplicated(numbers_df$macro))
write_lines(paste0("\\newcommand{\\", numbers_df$macro, "}{", numbers_df$formatted, "}"), path(s23_dir, "s23_numbers.tex"))
write_csv_strict(numbers_df, path(s23_dir, "s23_number_sources.csv"))
message("Exported ", nrow(numbers_df), " macros to ", path(s23_dir, "s23_numbers.tex"))
