# ============================================================
# 20: Section 4 (rewrite) exhibits, built from by-group outputs only
# Author: Saani Rawat
# Purpose: Every table, figure and number macro the rewritten Section 4 uses, written to a
#          STAGING folder (nothing goes into manuscript/). Main measure first in every table.
#          1 F_s4_daily_rush      daily deeds, 4 Jan-15 Apr 2021, scaled by own January mean
#          2 T_s4_rush            rush ratios and deadline ratios by group
#          3 F_s4_event_study     monthly event study, 95% bands
#          4 T_s4_prop19          revised Table 4 (DiD/DDD, rush-cohort resale)
#            TA_s4_occupancy      appendix: occupant share (replaces old Table 4 column 6)
#          5 TA_s4_ca_classes     California deed classes (appendix)
#          6 TA_s4_rush_hole      cumulative rush and shortfall (appendix)
#          7 s4_numbers.tex       number macros, prefix SIV (06d uses Policy/Selection/Trust/...)
# Inputs:  _outputs/tables/bygroup_*.csv, bygroup_suppl_*.csv (16, 17, 17b, 18, 19)
#          bygroup_resale_dd.csv and bygroup_occupancy_dd.csv must be regenerated after
#          2026-11-11 16:00 and contain the main measure (bygroup_main); otherwise the script stops.
#          bygroup_deeds_per_parcel.csv rows flagged lookback_incomplete are never used.
#          S4_DEV=1 skips those two requirements (development only; panels B and C are omitted).
# Outputs: _outputs/section4_v2/{tables,figures}/..., _outputs/section4_v2/s4_numbers.tex,
#          _outputs/section4_v2/s4_number_sources.csv
# Log:     11/11/2026: created (Phase 5, stage A). No prose.
#          11/12/2026: SIVOldColSix* read the current prop19_absentee_did.csv, not the v1 snapshot.
#          11/12/2026: label "Section 4 measure" for main_distinct (approved proofreading CO-1).
#          11/12/2026: "Trust-inclusive measure" for main_distinct_hi; "other 49 states" in notes (JUE review).
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(here::here("projects/03_family_homes/scripts/R/00_groups.R"))

set.seed(20261111)

s4_dev <- identical(Sys.getenv("S4_DEV", "0"), "1")
T <- function(f) path(tables_out_dir, f)
s4_dir <- path(out_dir, "section4_v2")
s4_tab <- path(s4_dir, "tables"); s4_fig <- path(s4_dir, "figures")
for (d in c(s4_dir, s4_tab, s4_fig)) dir_create(d)
main_g <- bygroup_main                     # "main_distinct" (distinct people; author's decision 11/12)
stopifnot(main_g == "main_distinct")

# ---- input guards: WP-B files regenerated after 2026-11-11 16:00, with the main measure ----
need_regenerated <- function(file, label) {
  if (!file_exists(file)) return(paste0(label, " is missing: ", file))
  if (file.mtime(file) < as.POSIXct("2026-11-11 16:00:00")) {
    return(paste0(label, " predates 2026-11-11 16:00 (", format(file.mtime(file)), "): not regenerated"))
  }
  d <- read_csv(file, show_col_types = FALSE, n_max = 5000)
  if (!"group" %in% names(d) || !main_g %in% d$group) {
    return(paste0(label, " has no rows for ", main_g))
  }
  NULL
}
problems <- c(need_regenerated(T("bygroup_resale_dd.csv"), "bygroup_resale_dd.csv"),
              need_regenerated(T("bygroup_occupancy_dd.csv"), "bygroup_occupancy_dd.csv"))
if (length(problems) && !s4_dev) {
  stop("Section 4 exhibits need regenerated WP-B inputs:\n  ", paste(problems, collapse = "\n  "))
}
if (length(problems)) message("S4_DEV: skipping resale/occupancy panels. ", paste(problems, collapse = " | "))
have_wpb <- !length(problems)

# ---- labels (words, as in the Gate 1 memo) ----
lab <- c(
  main_distinct = "Section 4 measure", main_distinct_hi = "Trust-inclusive measure",
  main_noaf_trust_heir = "Including same-person and entity deeds",
  main_noaf = "Proxy without death affidavits", trust_heir_chain = "Chain-verified trust-to-heir deeds",
  trust_heir_nochain = "Trust to other person, no chain", lifetime_pc = "Parent-child lifetime deeds",
  spouse_ca = "Spouse deeds (California only)", owner_retained = "Same-person retitles (mixed)",
  trust_to_self = "Trust-to-settlor deeds", death_affidavit_all = "All affidavits of death",
  main_proxy = "Old main proxy", market_sale = "Market sales")

# ---- formatting helpers ----
num <- function(x, d = 2) ifelse(is.na(x), "--",
  gsub("-", "$-$", formatC(x, format = "f", digits = d, big.mark = ",")))
pct <- function(x, d = 1) ifelse(is.na(x), "--", paste0(num(100 * x, d), "\\%"))
row_tex <- function(cells) paste0(paste(cells, collapse = " & "), " \\\\")
panel_tex <- function(txt, ncol) paste0("\\multicolumn{", ncol, "}{l}{\\emph{", txt, "}} \\\\")
write_table <- function(file, caption, label, colspec, header, body, notes) {
  writeLines(c("\\begin{table}[!ht]\\centering", paste0("\\caption{", caption, "}"),
               paste0("\\label{", label, "}"), "{\\small", "\\begin{adjustbox}{max width = \\textwidth}",
               paste0("\\begin{tabular}{", colspec, "}"), "\\toprule", header, "\\midrule", body,
               "\\bottomrule", "\\end{tabular}", "\\end{adjustbox}}",
               paste0("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} ",
                      notes, "}\\end{minipage}"), "\\end{table}"), file)
  message("Wrote table: ", file)
}
save_fig <- function(p, name, width = 9, height = 5) {
  ggsave(file.path(s4_fig, paste0(name, ".png")), p, width = width, height = height,
         bg = "transparent", dpi = 300)
  message("Saved figure: ", name)
}

# ---- read by-group outputs ----
rush <- read_csv(T("bygroup_rush_table.csv"), show_col_types = FALSE) %>% filter(base_window == "2019")
disc <- read_csv(T("bygroup_suppl_deadline_discontinuity.csv"), show_col_types = FALSE)
ddd <- read_csv(T("bygroup_ddd.csv"), show_col_types = FALSE)
es <- read_csv(T("bygroup_event_study_monthly.csv"), show_col_types = FALSE)
hole <- read_csv(T("bygroup_rush_hole.csv"), show_col_types = FALSE) %>% filter(base_window == "2019")
lchg <- read_csv(T("bygroup_suppl_ca_log_changes.csv"), show_col_types = FALSE) %>%
  mutate(group = sub(" (retitle, mixed)", "", group, fixed = TRUE))
cls <- read_csv(T("bygroup_suppl_ca_class_averages.csv"), show_col_types = FALSE)
daily <- read_csv(T("bygroup_daily_20210101_20210415.csv"), show_col_types = FALSE)
rec_daily <- read_csv(T("bygroup_suppl_daily_10_19feb2021.csv"), show_col_types = FALSE)
parcels <- read_csv(T("bygroup_deeds_per_parcel.csv"), show_col_types = FALSE)
stopifnot("lookback_incomplete" %in% names(parcels), main_g %in% parcels$group)
parcels <- parcels %>% filter(!lookback_incomplete)   # chain groups: 2017 onward only
stopifnot(main_g %in% ddd$group, main_g %in% es$group, main_g %in% rush$group)

# ---- number macros (prefix SIV) ----
number_rows <- list()
add_number <- function(name, value, source, digits = 2L) {
  stopifnot(length(value) == 1L, is.finite(value), grepl("^[A-Za-z]+$", name))
  if (name %in% names(number_rows)) stop("Duplicate macro: ", name)
  number_rows[[name]] <<- tibble(macro = name, value = as.numeric(value), source = source,
    formatted = formatC(value, format = "f", digits = digits, big.mark = ","))
}
rv <- function(g, w, col = "relative_ratio") rush[[col]][rush$group == g & rush$window == w]
dl <- function(g, st, yr, basis) disc$ratio[disc$group == g & disc$state_class == st &
                                              disc$year == yr & disc$basis == basis]
b_sign16 <- "signing 17-28 over 1-16, all days (sensitivity)"
b_sign15 <- "signing 16-28 over 1-15, all days (primary)"
b_rec    <- "recording 17-28 over 1-16, business days"

# ============================================================
# 1. Figure: daily deeds by signing date, scaled by own January 2021 mean
# ============================================================
fig_g <- c("trust_heir_chain", "lifetime_pc", "spouse_ca")
dd1 <- daily %>% filter(state_class == "CA", group %in% fig_g) %>%
  mutate(date = as.Date(date)) %>% group_by(group) %>%
  mutate(jan_mean = mean(n[date >= as.Date("2021-01-01") & date <= as.Date("2021-01-31")]),
         scaled = n / jan_mean) %>% ungroup() %>%
  filter(date >= as.Date("2021-01-04"), date <= as.Date("2021-04-15"))
stopifnot(nrow(dd1) == length(fig_g) * 102L, all(is.finite(dd1$scaled)))
ma7 <- function(x) as.numeric(stats::filter(x, rep(1 / 7, 7), sides = 2))
dd1 <- dd1 %>% group_by(group) %>% arrange(date) %>% mutate(ma = ma7(scaled)) %>% ungroup() %>%
  mutate(label = factor(lab[group], levels = lab[fig_g]))
p1 <- ggplot(dd1, aes(date, scaled, color = label)) +
  geom_hline(yintercept = 1, color = "grey60", linewidth = 0.3) +
  geom_line(linewidth = 0.25, alpha = 0.45) +
  geom_line(aes(y = ma), linewidth = 0.9, na.rm = TRUE) +
  geom_vline(xintercept = as.Date("2021-02-16") + 0.5, linetype = "dashed", color = "grey30") +
  annotate("text", x = as.Date("2021-02-16") + 2, y = Inf, vjust = 1.5, hjust = 0, size = 3,
           label = "After 16 Feb: new rules", color = "grey25") +
  scale_color_manual(values = palette_paper()[seq_along(fig_g)]) +
  scale_x_date(date_breaks = "2 weeks", date_labels = "%d %b") +
  labs(x = "Signing date, 2021", y = "Deeds per day / January 2021 mean", color = NULL) +
  theme(legend.position = "bottom")
save_fig(p1, "F_s4_daily_rush", 9, 5)

# ============================================================
# 2. Table: the rush by group
# ============================================================
rush_rows <- c(main_g, "main_distinct_hi", "main_noaf", "trust_heir_chain", "lifetime_pc")
comp_rows <- c("spouse_ca", "owner_retained", "trust_to_self", "death_affidavit_all")
t2_line <- function(g) {
  sp <- g == "spouse_ca"
  w <- function(win) if (sp) rv(g, win, "raw_ratio") else rv(g, win)
  row_tex(c(lab[g], num(w("jan_feb_2021")), num(w("mar_apr_2021")), num(w("nine_month")),
            num(dl(g, "CA", 2021, b_sign16)), num(dl(g, "CA", 2019, b_sign16)),
            if (sp) "--" else num(dl(g, "other", 2021, b_sign16))))
}
write_table(path(s4_tab, "T_s4_rush.tex"), "The deadline rush by deed group", "tab:s4rush", "lrrrrrr",
  c(" & \\multicolumn{3}{c}{California over counterfactual} & \\multicolumn{3}{c}{Deadline ratio} \\\\",
    "\\cmidrule(lr){2-4}\\cmidrule(lr){5-7}",
    "Group & Jan--Feb 2021 & Mar--Apr 2021 & Jun 2020--Feb 2021 & CA 2021 & CA 2019 & Other states 2021 \\\\"),
  c(panel_tex("Deeds a family can time", 7), vapply(rush_rows, t2_line, ""),
    "\\addlinespace", panel_tex("Comparison series", 7), vapply(comp_rows, t2_line, "")),
  paste0("Ratios of California deeds to a counterfactual built from the other 49 states and the District of Columbia and calendar 2019 (same calendar months). ",
         "The deadline ratio is the mean deeds per signing day for 17--28 February over 1--16 February (all calendar days); a ratio below one means fewer deeds per day after the deadline. ",
         "Spouse deeds are identified by document type in California only, so the first three columns show raw ratios to 2019 and there is no other-state ratio. ",
         "Same-person retitles mix refinance retitles with a parent adding a child to title. ",
         "Sources: bygroup\\_rush\\_table.csv, bygroup\\_suppl\\_deadline\\_discontinuity.csv."))

# ============================================================
# 3. Figure: monthly event study with 95% bands
# ============================================================
es_g <- c(main_g, "lifetime_pc", "trust_heir_chain", "owner_retained")
# Estimated months start in 2020 (the 2018-19 months are the reference); the saved event study
# begins in 2018, so the plot starts at 2018 with the reference period at zero.
es_plot <- es %>% filter(group %in% es_g) %>% mutate(date = as.Date(date)) %>%
  mutate(label = factor(lab[group], levels = lab[es_g]))
ref_rows <- tibble(label = factor(lab[es_g], levels = lab[es_g]), date = as.Date("2018-01-01"),
                   estimate = 0)
p3 <- ggplot(es_plot, aes(date, estimate)) +
  geom_hline(yintercept = 0, color = "grey55", linewidth = 0.3) +
  annotate("rect", xmin = as.Date("2018-01-01"), xmax = as.Date("2019-12-31"), ymin = -Inf,
           ymax = Inf, fill = "grey92") +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = palette_paper()[2], alpha = 0.2) +
  geom_line(color = palette_paper()[2], linewidth = 0.6) +
  geom_point(color = palette_paper()[2], size = 0.8) +
  geom_vline(xintercept = prop19_event_dates[["effective"]], linetype = "dashed", color = "grey30") +
  geom_vline(xintercept = prop19_event_dates[["ballot"]], linetype = "dotted", color = "grey30") +
  facet_wrap(~label, ncol = 2, scales = "free_y") +
  scale_x_date(limits = as.Date(c("2018-01-01", "2023-12-31")), date_breaks = "1 year", date_labels = "%Y") +
  labs(x = NULL, y = "California relative to other states (log points)")
save_fig(p3, "F_s4_event_study", 9, 6)

# ============================================================
# 4. Revised Table 4
# ============================================================
t4_groups <- c(main_g, "trust_heir_chain", "lifetime_pc", "owner_retained", "trust_to_self")
a_line <- function(g) {
  r <- ddd[ddd$group == g, ]
  row_tex(c(lab[g], num(r$did, 3), paste0("(", num(r$did_se, 3), ")"), num(r$ddd, 3),
            paste0("(", num(r$ddd_se, 3), ")"), num(r$nobs, 0), num(r$n_states, 0)))
}
body4 <- c(panel_tex("A. Log deeds: California against other states, 2022--23 against 2017--19", 7),
           vapply(t4_groups, a_line, ""))
mkt_did <- ddd$did[ddd$group == main_g] - ddd$ddd[ddd$group == main_g]   # market-sale DiD, same sample
notes_extra <- character()
if (have_wpb) {
  rs <- read_csv(T("bygroup_resale_dd.csv"), show_col_types = FALSE) %>%
    filter(contrast == "rush_vs_ref_pooled")
  rs_g <- c(main_g, "trust_heir_chain", "trust_to_self", "market_sale")
  b_line <- function(g) {
    r <- rs[rs$group == g, ]
    stopifnot(nrow(r) == 1L)
    row_tex(c(lab[g], pct(r$ca_treat), pct(r$ca_ref), pct(r$other_treat), pct(r$other_ref),
              num(r$dd_pp, 2), num(r$n_ca_treat, 0)))
  }
  body4 <- c(body4, "\\addlinespace",
    panel_tex("B. Resold within 24 months: rush cohort (Jun 2020--Feb 2021) against Jun--Feb 2018--19 cohorts", 7),
    "Group & CA rush & CA reference & Other rush & Other reference & DiD (pp) & CA rush deeds \\\\",
    vapply(rs_g, b_line, ""))
  sp_rs <- read_csv(T("bygroup_resale_dd.csv"), show_col_types = FALSE) %>%
    filter(group == "spouse_ca", contrast == "rush_vs_ref_pooled")
  stopifnot(nrow(sp_rs) == 1L)
  sp_rs_pp <- 100 * sp_rs$diff_ca
  notes_extra <- paste0(
    " Panel B is a difference in means with no standard errors. Spouse deeds, observed in California only, change by ",
    num(sp_rs_pp, 2), " percentage points in the share resold within 24 months between the same cohorts.")
}
write_table(path(s4_tab, "T_s4_prop19.tex"),
  "Proposition 19: volume and resale by deed group", "tab:s4prop19}\\label{tab:prop19", "lrrrrrr",
  c("Group & DiD & (SE) & Triple difference & (SE) & Observations & States \\\\"), body4,
  paste0("Panel A: log annual deeds by state and year, 2017--2019 against 2022--2023. ",
         "DiD is California $\\times$ post; the triple difference is the same coefficient net of market sales, which equals the DiD minus the market-sale DiD (",
         num(mkt_did, 3), " for the Section~\\ref{sec:prop19} measure). State-clustered standard errors in parentheses; inference is limited with one treated state. ",
         "Observations are state-year cells in the stacked regression (DiD columns use half as many). ",
         "Chain-verified trust-to-heir deeds are used from 2017 on only (10-year look-back). Same-person retitles mix refinance retitles with a parent adding a child to title.",
         paste(notes_extra, collapse = "")))

# ============================================================
# 4b. Appendix table: occupant share (replaces old Table 4 column 6)
# ============================================================
oc <- read_csv(T("bygroup_occupancy_dd.csv"), show_col_types = FALSE)
oc_need <- c("group", "measure", "share_CA_pre_2017_19", "share_CA_post_2022_23",
             "share_other_pre_2017_19", "share_other_post_2022_23", "dd_post", "dd_2021",
             "diff_ca_post", "fill_rate_CA_post_2022_23", "fill_rate_CA_pre_2017_19",
             "fill_rate_other_post_2022_23", "fill_rate_other_pre_2017_19", "table4_col6_saved")
stopifnot(all(oc_need %in% names(oc)))
oc_g <- c(main_g, "main_noaf", "lifetime_pc", "trust_heir_chain", "market_sale")
ocr <- function(g) { r <- oc[oc$group == g, ]; stopifnot(nrow(r) == 1L); r }
mk_dd <- 100 * ocr("market_sale")$dd_post
o_line <- function(g) {
  r <- ocr(g)
  row_tex(c(lab[g], pct(r$share_CA_pre_2017_19), pct(r$share_CA_post_2022_23),
            pct(r$share_other_pre_2017_19), pct(r$share_other_post_2022_23),
            num(100 * r$dd_post, 2), if (g == "market_sale") "--" else num(100 * r$dd_post - mk_dd, 2),
            num(100 * r$dd_2021, 2)))
}
sp_o <- ocr("spouse_ca")
fill_rng <- function(cols) paste0(num(100 * min(unlist(oc[oc$group %in% oc_g, cols])), 1), "--",
                                  num(100 * max(unlist(oc[oc$group %in% oc_g, cols])), 1), "\\%")
write_table(path(s4_tab, "TA_s4_occupancy.tex"),
  "Occupant share of recipients: mailing address equals property address", "tab:s4occupancy",
  "lrrrrrrr",
  c(" & \\multicolumn{2}{c}{California} & \\multicolumn{2}{c}{Other states} & & & \\\\",
    "\\cmidrule(lr){2-3}\\cmidrule(lr){4-5}",
    "Group & 2017--19 & 2022--23 & 2017--19 & 2022--23 & DiD (pp) & Net of market (pp) & 2021 DiD (pp) \\\\"),
  c(vapply(oc_g, o_line, ""), "\\addlinespace",
    row_tex(c(lab["spouse_ca"], pct(sp_o$share_CA_pre_2017_19), pct(sp_o$share_CA_post_2022_23),
              "--", "--", "--", "--", "--"))),
  paste0("Share of non-corporate recipients whose mailing street address equals the property address, among deeds with both addresses recorded. ",
         "DiD is the change in California between 2017--19 and 2022--23 minus the same change in the other 49 states and the District of Columbia; ",
         "net of market subtracts the market-sale DiD (", num(mk_dd, 2), " points); the 2021 DiD uses 2021 in place of 2022--23. ",
         "Spouse deeds exist in California only; their California change is ", num(100 * sp_o$diff_ca_post, 2),
         " points. The address fields are recorded for ", fill_rng(c("fill_rate_CA_pre_2017_19", "fill_rate_CA_post_2022_23")),
         " of California deeds and ", fill_rng(c("fill_rate_other_pre_2017_19", "fill_rate_other_post_2022_23")),
         " of deeds elsewhere. Differences in means with no standard errors. ",
         "A mailing address equal to the property address is an upper bound on owner occupancy (the vendor may default blank mailing addresses). ",
         "Source: bygroup\\_occupancy\\_dd.csv."))

# ============================================================
# 5. Appendix table: California deed classes
# ============================================================
cl <- cls %>% rename(a1719 = `avg 2017-19`, y21 = `2021`, a2223 = `avg 2022-23`) %>%
  filter(!grepl("^VARIANT", cell)) %>% mutate(lchg = log(a2223 / a1719))
sp_l <- cl$lchg[cl$cell == "family direct / spouse"]; mk_l <- cl$lchg[cl$cell == "market_sale"]
cl <- cl %>% mutate(cell_label = case_when(
  grepl("^TOTAL", cell) ~ "Total, all California deeds", cell == "market_sale" ~ "Market sales",
  cell == "other_nonarms" ~ "Other non-arm's-length deeds", cell == "estate_noninterfam" ~ "Estate deeds, not flagged family",
  TRUE ~ gsub("_", " ", sub("^family ", "Family: ", cell))),
  less_spouse = lchg - sp_l, less_market = lchg - mk_l) %>%
  arrange(desc(grepl("^Total", cell_label)), desc(a1719))
mm <- lchg %>% filter(group %in% c(main_g, "trust_heir_chain")) %>%
  transmute(cell_label = paste0(lab[group], " (group)"), a1719 = mean_2017_19, y21 = NA_real_,
            a2223 = mean_2022_23, lchg = lchg_vs_2017_19, less_spouse = minus_spouse_vs_2017_19,
            less_market = minus_market_vs_2017_19)
cl_all <- bind_rows(cl %>% select(cell_label, a1719, y21, a2223, lchg, less_spouse, less_market), mm)
c_line <- function(i) row_tex(c(cl_all$cell_label[i], num(cl_all$a1719[i], 0), num(cl_all$y21[i], 0),
  num(cl_all$a2223[i], 0), num(cl_all$lchg[i], 3), num(cl_all$less_spouse[i], 3), num(cl_all$less_market[i], 3)))
write_table(path(s4_tab, "TA_s4_ca_classes.tex"), "California deeds by class, 2017--2023", "tab:s4classes",
  "lrrrrrr",
  "Class & Avg.\\ 2017--19 & 2021 & Avg.\\ 2022--23 & Log change & Less spouse & Less market \\\\",
  vapply(seq_len(nrow(cl_all)), c_line, ""),
  paste0("Average annual deeds (recorded by signing year). Log change is the change from the 2017--19 average to the 2022--23 average; ",
         "less spouse and less market subtract the same change for direct spouse deeds (", num(sp_l, 3),
         ") and market sales (", num(mk_l, 3), "). Cells partition California deeds. ",
         "Death affidavits with a trust party reflect a 2018 recording change and are not comparable across 2017--19 and 2022--23. ",
         "The group rows overlap the classes. Sources: bygroup\\_suppl\\_ca\\_class\\_averages.csv, bygroup\\_suppl\\_ca\\_log\\_changes.csv."))

# ============================================================
# 6. Appendix table: cumulative rush and shortfall
# ============================================================
h_groups <- c(main_g, "main_distinct_hi", "main_noaf", "trust_heir_chain", "lifetime_pc",
              "owner_retained", "trust_to_self", "death_affidavit_all")
hv <- function(g, e, col) hole[[col]][hole$group == g & hole$end_label == e]
h_line <- function(g) row_tex(c(lab[g], num(hv(g, "main_202406", "rush_excess"), 0),
  num(hv(g, "sens_202312", "shortfall"), 0), num(hv(g, "sens_202405", "shortfall"), 0),
  num(hv(g, "main_202406", "shortfall"), 0), num(hv(g, "main_202406", "harvest_ratio"), 2),
  num(hv(g, "main_202406", "shortfall_adj"), 0)))
write_table(path(s4_tab, "TA_s4_rush_hole.tex"), "Cumulative rush and cumulative shortfall", "tab:s4hole",
  "lrrrrrr",
  c(" & Rush & \\multicolumn{3}{c}{Shortfall through} & Rush / shortfall & Shortfall \\\\",
    "\\cmidrule(lr){3-5}", "Group & Jun 2020--Feb 2021 & Dec 2023 & May 2024 & Jun 2024 & (Jun 2024) & net of market (Jun 2024) \\\\"),
  vapply(h_groups, h_line, ""),
  paste0("Deeds. The rush is the sum of monthly excess over the counterfactual from June 2020 to February 2021; the shortfall is minus the sum of monthly excess from March 2021 to the end month. ",
         "June 2024 is the last usable month (the extract ends in August 2024 and recent months are incomplete); December 2023 and May 2024 are sensitivities. ",
         "Net of market scales the counterfactual by California's market-sale gap. Chain-verified deeds are used from 2017 on. ",
         "Sources: bygroup\\_rush\\_hole.csv."))

# ============================================================
# 7. Number macros
# ============================================================
S_ru <- "bygroup_rush_table.csv"; S_dl <- "bygroup_suppl_deadline_discontinuity.csv"
S_dd <- "bygroup_ddd.csv"; S_es <- "bygroup_event_study_monthly.csv"
S_ho <- "bygroup_rush_hole.csv"; S_lc <- "bygroup_suppl_ca_log_changes.csv"
for (pr in list(c("Main", main_g), c("Heir", "trust_heir_chain"), c("Pc", "lifetime_pc"),
                c("Upper", "main_distinct_hi"))) {
  add_number(paste0("SIV", pr[1], "JanFeb"), rv(pr[2], "jan_feb_2021"), S_ru)
  add_number(paste0("SIV", pr[1], "MarApr"), rv(pr[2], "mar_apr_2021"), S_ru)
  add_number(paste0("SIV", pr[1], "Nine"), rv(pr[2], "nine_month"), S_ru)
  add_number(paste0("SIV", pr[1], "DeadlineSign"), dl(pr[2], "CA", 2021, b_sign16), S_dl)
  add_number(paste0("SIV", pr[1], "DeadlineSignPrimary"), dl(pr[2], "CA", 2021, b_sign15), S_dl)
  add_number(paste0("SIV", pr[1], "DeadlineRec"), dl(pr[2], "CA", 2021, b_rec), S_dl)
  add_number(paste0("SIV", pr[1], "DeadlineSignPrior"), dl(pr[2], "CA", 2019, b_sign16), S_dl)
  add_number(paste0("SIV", pr[1], "DeadlineSignOther"), dl(pr[2], "other", 2021, b_sign16), S_dl)
}
add_number("SIVSpouseJanFeb", rv("spouse_ca", "jan_feb_2021", "raw_ratio"), S_ru)
add_number("SIVSpouseDeadlineSign", dl("spouse_ca", "CA", 2021, b_sign16), S_dl)
add_number("SIVSpouseDeadlineRec", dl("spouse_ca", "CA", 2021, b_rec), S_dl)
add_number("SIVSettlorJanFeb", rv("trust_to_self", "jan_feb_2021"), S_ru)
add_number("SIVSettlorDeadlineSign", dl("trust_to_self", "CA", 2021, b_sign16), S_dl)
add_number("SIVAffidavitDeadlineSign", dl("death_affidavit_all", "CA", 2021, b_sign16), S_dl)
add_number("SIVRetitleDeadlineSign", dl("owner_retained", "CA", 2021, b_sign16), S_dl)
for (pr in list(c("Main", main_g), c("Heir", "trust_heir_chain"), c("Pc", "lifetime_pc"),
                c("Retitle", "owner_retained"), c("Settlor", "trust_to_self"))) {
  r <- ddd[ddd$group == pr[2], ]
  add_number(paste0("SIV", pr[1], "DiD"), r$did, S_dd, 3)
  add_number(paste0("SIV", pr[1], "DDD"), r$ddd, S_dd, 3)
  add_number(paste0("SIV", pr[1], "DDDSE"), r$ddd_se, S_dd, 3)
  add_number(paste0("SIV", pr[1], "DDDPct"), r$pct_effect, S_dd, 1)
  add_number(paste0("SIV", pr[1], "Obs"), r$nobs, S_dd, 0)
}
add_number("SIVMarketDiD", mkt_did, S_dd, 3)
for (pr in list(c("Main", main_g), c("Heir", "trust_heir_chain"), c("Pc", "lifetime_pc"),
                c("Retitle", "owner_retained"))) {
  e <- es %>% filter(group == pr[2])
  add_number(paste0("SIV", pr[1], "ESTwentyTwo"), mean(e$estimate[e$ym >= 202201 & e$ym <= 202212]), S_es, 3)
  add_number(paste0("SIV", pr[1], "ESTwentyThree"), mean(e$estimate[e$ym >= 202301 & e$ym <= 202312]), S_es, 3)
  add_number(paste0("SIV", pr[1], "ESTwentyOne"), mean(e$estimate[e$ym >= 202103 & e$ym <= 202112]), S_es, 3)
}
for (pr in list(c("Main", main_g), c("Heir", "trust_heir_chain"), c("Pc", "lifetime_pc"))) {
  add_number(paste0("SIV", pr[1], "Rush"), hv(pr[2], "main_202406", "rush_excess"), S_ho, 0)
  add_number(paste0("SIV", pr[1], "Shortfall"), hv(pr[2], "main_202406", "shortfall"), S_ho, 0)
  add_number(paste0("SIV", pr[1], "ShortfallDec"), hv(pr[2], "sens_202312", "shortfall"), S_ho, 0)
  add_number(paste0("SIV", pr[1], "RushShortfall"), hv(pr[2], "main_202406", "harvest_ratio"), S_ho, 2)
}
lm <- lchg[lchg$group == main_g, ]
add_number("SIVMainLessSpouse", lm$minus_spouse_vs_2017_19, S_lc, 3)
add_number("SIVMainLessMarket", lm$minus_market_vs_2017_19, S_lc, 3)
add_number("SIVMainLogChange", lm$lchg_vs_2017_19, S_lc, 3)
add_number("SIVSpouseLogChange", lchg$lchg_vs_2017_19[lchg$group == "spouse_ca"], S_lc, 3)
add_number("SIVMarketLogChange", lchg$lchg_vs_2017_19[lchg$group == "market_sale"], S_lc, 3)
# daily signing and recording counts around the deadline
dsign <- function(g, d) daily$n[daily$group == g & daily$state_class == "CA" & daily$date == as.Date(d)]
add_number("SIVHeirSignFridayTwelve", dsign("trust_heir_chain", "2021-02-12"), "bygroup_daily_20210101_20210415.csv", 0)
add_number("SIVHeirSignTuesdaySixteen", dsign("trust_heir_chain", "2021-02-16"), "bygroup_daily_20210101_20210415.csv", 0)
rec_n <- function(g, d) rec_daily$n[rec_daily$kind == "recording" & rec_daily$group == g &
                                       rec_daily$state_class == "CA" & rec_daily$day_raw == d]
S_rd <- "bygroup_suppl_daily_10_19feb2021.csv"
add_number("SIVHeirRecSixteen", rec_n("trust_heir_chain", 20210216L), S_rd, 0)
add_number("SIVMainRecSixteen", rec_n(main_g, 20210216L), S_rd, 0)
rec_mean <- function(g, days) mean(vapply(days, function(d) rec_n(g, d), numeric(1)))
add_number("SIVHeirRecBeforeMean", rec_mean("trust_heir_chain", c(20210210L, 20210211L, 20210212L)), S_rd, 0)
add_number("SIVHeirRecAfterMean", rec_mean("trust_heir_chain", c(20210217L, 20210218L, 20210219L)), S_rd, 0)
add_number("SIVMainRecBeforeMean", rec_mean(main_g, c(20210210L, 20210211L, 20210212L)), S_rd, 0)
add_number("SIVMainRecAfterMean", rec_mean(main_g, c(20210217L, 20210218L, 20210219L)), S_rd, 0)
# spouse-deed share of the main measure in California (footnote, Section 4.3):
# main measure minus main_distinct_xspouse_ca (the main measure without spouse deeds).
mm <- function(g, col) lchg[[col]][lchg$group == g]
for (pr in list(c("Pre", "mean_2017_19"), c("Post", "mean_2022_23"))) {
  sp_in <- mm(main_g, pr[2]) - mm("main_distinct_xspouse_ca", pr[2])
  add_number(paste0("SIVSpouseShareMain", pr[1]), 100 * sp_in / mm(main_g, pr[2]), S_lc, 1)
}
# California affidavits of death with a trust buyer, 2017 and 2018 (vendor coding break)
af <- read_csv(T("validation_ca_af_raw_by_year_v2.csv"), show_col_types = FALSE) %>% filter(residential == "Y")
add_number("SIVAffTrustBuyerTwentySeventeen", af$buyer_trust[af$sale_year == 2017], "validation_ca_af_raw_by_year_v2.csv", 0)
add_number("SIVAffTrustBuyerTwentyEighteen", af$buyer_trust[af$sale_year == 2018], "validation_ca_af_raw_by_year_v2.csv", 0)
# deeds per parcel: national main-proxy count (the 1.1940 reproduction) and California within a year
S_p <- "bygroup_deeds_per_parcel.csv"
pp <- function(g, reg, per) parcels$deeds_per_parcel[parcels$group == g & parcels$region == reg & parcels$period == per]
add_number("SIVDeedsPerParcelNational", pp("main_proxy", "all", "span_2007_2023"), S_p, 4)
add_number("SIVDeedsPerParcelCASpan", pp("main_proxy", "CA", "span_2007_2023"), S_p, 3)
pg <- main_g
message("Deeds-per-parcel within-year macros use group: ", pg)
yrs <- paste0("y", 2017:2023)
within <- parcels$deeds_per_parcel[parcels$group == pg & parcels$region == "CA" & parcels$period %in% yrs]
add_number("SIVDeedsPerParcelYearMin", min(within), paste0(S_p, " (", pg, ")"), 3)
add_number("SIVDeedsPerParcelYearMax", max(within), paste0(S_p, " (", pg, ")"), 3)
add_number("SIVDeedsPerParcelRush", pp(pg, "CA", "rush"), paste0(S_p, " (", pg, ")"), 3)
if (have_wpb) {
  rsa <- read_csv(T("bygroup_resale_dd.csv"), show_col_types = FALSE) %>% filter(contrast == "rush_vs_ref_pooled")
  rj <- read_csv(T("bygroup_resale_dd.csv"), show_col_types = FALSE) %>% filter(contrast == "jan_feb_2021_vs_ref_pooled")
  S_r <- "bygroup_resale_dd.csv"
  for (pr in list(c("Main", main_g), c("Heir", "trust_heir_chain"), c("Settlor", "trust_to_self"),
                  c("Market", "market_sale"))) {
    add_number(paste0("SIV", pr[1], "ResaleDD"), rsa$dd_pp[rsa$group == pr[2]], S_r, 2)
    add_number(paste0("SIV", pr[1], "ResaleJanFebDD"), rj$dd_pp[rj$group == pr[2]], S_r, 2)
  }
  add_number("SIVSpouseResaleChange", sp_rs_pp, S_r, 2)
}
S_o <- "bygroup_occupancy_dd.csv"
add_number("SIVMainOccupDD", 100 * ocr(main_g)$dd_post, S_o, 2)
add_number("SIVMarketOccupDD", 100 * ocr("market_sale")$dd_post, S_o, 2)
add_number("SIVMainOccupNet", 100 * ocr(main_g)$dd_post - mk_dd, S_o, 2)
add_number("SIVPcOccupDD", 100 * ocr("lifetime_pc")$dd_post, S_o, 2)
add_number("SIVPcOccupNet", 100 * ocr("lifetime_pc")$dd_post - mk_dd, S_o, 2)
add_number("SIVHeirOccupDD", 100 * ocr("trust_heir_chain")$dd_post, S_o, 2)
add_number("SIVHeirOccupTwentyOne", 100 * ocr("trust_heir_chain")$dd_2021, S_o, 2)
add_number("SIVSpouseOccupChange", 100 * sp_o$diff_ca_post, S_o, 2)
add_number("SIVOccupFillCA", 100 * ocr(main_g)$fill_rate_CA_post_2022_23, S_o, 1)
# old Table 4 column (6): absentee recipient share, California x post.
# 11/12/2026: read the current output of 04_prop19.R (rerun on the current measure), not the
# pre-switch _v1_snapshot_2026-11-11 copy, which predates events.parquet.
snap_abs <- read_csv(T("prop19_absentee_did.csv"), show_col_types = FALSE)
stopifnot(nrow(snap_abs) == 1L, is.finite(snap_abs$coef), is.finite(snap_abs$se),
          file.mtime(T("prop19_absentee_did.csv")) > file.mtime(events_panel_path))  # not older than the panel
S_snap <- "tables/prop19_absentee_did.csv"
add_number("SIVOldColSixValue", snap_abs$coef, S_snap, 4)
add_number("SIVOldColSixSE", snap_abs$se, S_snap, 4)
add_number("SIVOldColSixSaved", oc$table4_col6_saved[oc$group == "main_proxy"], S_o, 4)
numbers <- bind_rows(number_rows)
stopifnot(!anyDuplicated(numbers$macro))
writeLines(paste0("\\newcommand{\\", numbers$macro, "}{", numbers$formatted, "}"),
           path(s4_dir, "s4_numbers.tex"))
write_csv_strict(numbers, path(s4_dir, "s4_number_sources.csv"))
message("Exported ", nrow(numbers), " macros to ", path(s4_dir, "s4_numbers.tex"))
