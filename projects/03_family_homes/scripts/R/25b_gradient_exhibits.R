#======================================================================#
# Purpose : Section 4 exhibit for the within-California wedge gradient,
#           intensive margin only (how the rush and the deadline break vary
#           with each parcel's tax saving): table T_s4_gradient.tex, figure
#           F_s4_gradient.png and the text macros, built from the saved
#           outputs of 25_wedge_gradient.R. No estimation here.
#           Author's decision (11/13/2026): the gradient enters Section 4 as
#           supporting evidence after the deadline break; the cap, parent's
#           gain, decline, resale and heir-sale results stay out of the paper.
# Name    : Saani Rawat
# Created : 11/13/2026
# Log     : 11/13/2026: created (WP-R2 exhibit).
# Inputs  : _outputs/tables/r2_wedge_gradient_bins.csv, r2_wedge_bin_doses.csv,
#           r2_wedge_dose_slopes.csv, r2_numbers_sources.csv (all from 25)
# Outputs : manuscript/tables/T_s4_gradient.tex
#           manuscript/figures/F_s4_gradient.png
#           manuscript/tables/r2_numbers.tex and _outputs/tables/r2_numbers_sources.csv
#             (adds this script's macros; keeps those of 25 and 26)
#           _outputs/tex_test_r2/T_s4_gradient_test.tex (standalone compile test)
# Run     : after 25_wedge_gradient.R; rerun whenever 25 is rerun.
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

script_name <- "25b_gradient_exhibits.R"

#============================================#
# Settings ----
#============================================#

series <- c(main = bygroup_main, spouse = "spouse_ca", af = "death_affidavit_all")
series_label <- c(main = "Section 4 measure", spouse = "Spouse deeds",
                  af = "Affidavits of death")
stopifnot(bygroup_main == "main_distinct")

f_bins    <- path(tables_out_dir, "r2_wedge_gradient_bins.csv")
f_doses   <- path(tables_out_dir, "r2_wedge_bin_doses.csv")
f_slopes  <- path(tables_out_dir, "r2_wedge_dose_slopes.csv")
f_sources <- path(tables_out_dir, "r2_numbers_sources.csv")
f_macros  <- path(tables_dir, "r2_numbers.tex")
f_table   <- path(tables_dir, "T_s4_gradient.tex")
f_figure  <- path(figures_dir, "F_s4_gradient.png")
test_dir  <- path(out_dir, "tex_test_r2")
dir_create(test_dir)

# slope specifications in 25 (Poisson, one decile step; zero bin has its own indicator)
rush_outcome     <- "rush_jf2021_vs_jf2019"
deadline_outcome <- "deadline_2021_vs_2019"

#============================================#
# Importing the outputs of 25 ----
#============================================#

stopifnot(all(file_exists(c(f_bins, f_doses, f_slopes, f_sources))))
bins <- read_csv(f_bins, show_col_types = FALSE) %>%
  filter(system == "wedge", kind == "ev", group %in% names(series)) %>%
  mutate(wdec = as.integer(substr(bin, 2, 3)))
doses <- read_csv(f_doses, show_col_types = FALSE) %>% filter(lien == "2019-01-01")
totals <- read_csv(f_bins, show_col_types = FALSE) %>%
  filter(system == "all", kind == "ev", group %in% names(series))
slopes <- read_csv(f_slopes, show_col_types = FALSE) %>%
  filter(model == "poisson", dose == "rank",
         (outcome == rush_outcome & term == "pd") | (outcome == deadline_outcome & term == "ad21"),
         group %in% names(series))
stopifnot(nrow(bins) == 33L, nrow(doses) == 11L, nrow(totals) == 3L, nrow(slopes) == 6L,
          all(is.finite(bins$rush_ratio)), all(is.finite(bins$deadline_2021)))

# |- one row per bin, columns per series ----
bin_wide <- bins %>%
  select(wdec, group, n_risk = denom_jf2021, rush_ratio, deadline_2021) %>%
  pivot_wider(names_from = group, values_from = c(rush_ratio, deadline_2021, n_risk)) %>%
  left_join(doses %>% select(wdec, wedge_min, wedge_max, n_parcels), by = "wdec") %>%
  arrange(wdec)
# the at-risk denominator is the same parcel set for every series
stopifnot(all(bin_wide$n_risk_main == bin_wide$n_risk_spouse), all(bin_wide$n_risk_main == bin_wide$n_risk_af))
slope_of <- function(g, out, col = "estimate") slopes[[col]][slopes$group == g & slopes$outcome == out]

#============================================#
# Table: T_s4_gradient.tex ----
#============================================#

num <- function(x, d = 2) ifelse(is.na(x), "--",
  gsub("-", "$-$", formatC(x, format = "f", digits = d, big.mark = ",")))
dollars <- function(x) formatC(round(x), format = "d", big.mark = ",")
row_tex <- function(cells) paste0(paste(cells, collapse = " & "), " \\\\")
panel_tex <- function(txt, ncol) paste0("\\multicolumn{", ncol, "}{l}{\\emph{", txt, "}} \\\\")

# decile wedge range in dollars a year (upper ends are the decile cut points)
cuts <- bin_wide$wedge_max[bin_wide$wdec %in% 1:9]
wedge_range <- c("0", paste0("0--", dollars(cuts[1])),
                 paste0(dollars(cuts[1:8]), "--", dollars(cuts[2:9])),
                 paste0("above ", dollars(cuts[9])))
bin_name <- c("Zero wedge", paste("Decile", 1:10))

bin_rows <- map_chr(seq_len(nrow(bin_wide)), function(i) {
  r <- bin_wide[i, ]
  row_tex(c(bin_name[i], wedge_range[i], dollars(r$n_risk_main),
            num(r$rush_ratio_main), num(r$deadline_2021_main),
            num(r$rush_ratio_spouse), num(r$deadline_2021_spouse),
            num(r$rush_ratio_af), num(r$deadline_2021_af)))
})
tot <- function(g, col) totals[[col]][totals$group == g]
total_row <- row_tex(c("All parcels", "", dollars(tot("main", "denom_jf2021")),
                       num(tot("main", "rush_ratio")), num(tot("main", "deadline_2021")),
                       num(tot("spouse", "rush_ratio")), num(tot("spouse", "deadline_2021")),
                       num(tot("af", "rush_ratio")), num(tot("af", "deadline_2021"))))
slope_cells <- function(col, d = 3, wrap = FALSE) {
  v <- unlist(map(names(series), function(g) c(slope_of(g, rush_outcome, col),
                                               slope_of(g, deadline_outcome, col))))
  out <- if (col == "n_clusters") as.character(v) else num(v, d)
  if (wrap) paste0("(", out, ")") else out
}
slope_rows <- c(row_tex(c("Slope", "", "", slope_cells("estimate"))),
                row_tex(c("", "", "", slope_cells("se", wrap = TRUE))),
                row_tex(c("County clusters", "", "", slope_cells("n_clusters"))))

table_notes <- paste0(
  "California single-family and condominium parcels on the 2008--2009 roll, with the annual tax wedge ",
  "$\\hat w=\\hat t\\max(\\hat p-\\hat A,0)$ of Section~\\ref{subsec:wedge} measured at the January 1, 2019 lien date, ",
  "before Proposition~19 reached the ballot. Zero-wedge parcels form their own bin; deciles split the positive wedges, ",
  "and the second column gives each decile's range in dollars a year. Parcels at risk in 2021 (third column) are those ",
  "with no arm's-length sale between January 1, 2019 and January 1, 2021; in 2019 every parcel is at risk. Rush: parcels with at least one deed of the series signed in ",
  "January--February 2021 per 1,000 parcels at risk, over the same rate for January--February 2019. Deadline ratio: ",
  "deeds per signing day on 17--28 February 2021 over 1--16 February 2021 (all calendar days). ",
  "Slope: Poisson regression of county $\\times$ bin $\\times$ period counts with the log of parcels at risk as offset ",
  "and county-by-period and county-by-bin fixed effects. The rush slope is the coefficient on the January--February ",
  "2021 indicator times the decile number (1--10); the deadline slope is the coefficient on the after-deadline ",
  "indicator times the decile number in February 2021, net of the same coefficient in February 2019. In both the ",
  "zero-wedge bin has its own indicator and does not enter the slope. Log points per decile; standard errors in ",
  "parentheses are clustered by county. Spouse deeds are identified by document type; affidavits of death include ",
  "those with a trust party.")

table_lines <- c(
  "\\begin{table}[!ht]\\centering",
  "\\caption{The deadline rush by the parcel's tax wedge, California}",
  "\\label{tab:s4gradient}",
  "{\\small",
  "\\begin{adjustbox}{max width = \\textwidth}",
  "\\begin{tabular}{llrrrrrrr}",
  "\\toprule",
  paste0(" & Annual & Parcels & \\multicolumn{2}{c}{", series_label[["main"]], "} & \\multicolumn{2}{c}{",
         series_label[["spouse"]], "} & \\multicolumn{2}{c}{", series_label[["af"]], "} \\\\"),
  "\\cmidrule(lr){4-5}\\cmidrule(lr){6-7}\\cmidrule(lr){8-9}",
  "Wedge bin & wedge (\\$) & at risk & Rush & Deadline & Rush & Deadline & Rush & Deadline \\\\",
  "\\midrule",
  bin_rows,
  "\\addlinespace",
  total_row,
  "\\midrule",
  panel_tex("Slope per decile (log points)", 9),
  slope_rows,
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{adjustbox}}",
  paste0("\\begin{minipage}{0.95\\textwidth}\\vspace{2pt}{\\scriptsize \\textit{Notes:} ", table_notes,
         "}\\end{minipage}"),
  "\\end{table}")
writeLines(table_lines, f_table)
message("Wrote table: ", f_table)

#============================================#
# Figure: F_s4_gradient.png ----
#============================================#

fig_df <- bins %>%
  select(wdec, group, rush_ratio, deadline_2021) %>%
  pivot_longer(c(rush_ratio, deadline_2021), names_to = "metric", values_to = "ratio") %>%
  mutate(series = factor(series_label[group], levels = series_label),
         metric = factor(if_else(metric == "rush_ratio",
                                 "Rush: Jan-Feb 2021 over Jan-Feb 2019",
                                 "Deadline ratio: 17-28 over 1-16 Feb 2021"),
                         levels = c("Rush: Jan-Feb 2021 over Jan-Feb 2019",
                                    "Deadline ratio: 17-28 over 1-16 Feb 2021")))
p_gradient <- ggplot(fig_df, aes(wdec, ratio, color = series, shape = series)) +
  geom_hline(yintercept = 1, color = "grey60", linewidth = 0.3) +
  geom_vline(xintercept = 0.5, color = "grey75", linetype = "dotted", linewidth = 0.3) +
  # deciles joined by lines; the zero-wedge bin stands apart
  geom_line(data = fig_df %>% filter(wdec >= 1L), linewidth = 0.7) +
  geom_point(size = 1.8) +
  facet_wrap(~ metric, scales = "free_y") +
  scale_x_continuous(breaks = 0:10, labels = c("Zero", 1:10)) +
  scale_color_manual(values = palette_paper()[c(1, 2, 3)]) +
  labs(x = "Wedge bin at the January 2019 lien date (zero, then deciles of the positive wedge)",
       y = "Ratio", color = NULL, shape = NULL) +
  theme(legend.position = "bottom")
ggsave(f_figure, p_gradient, width = 9, height = 4.5, bg = "transparent", dpi = 300)
message("Saved figure: ", f_figure)

#============================================#
# Text macros (RTWO) ----
#============================================#

# |- macros the text cites that 25 already writes: check they exist and agree with the table ----
existing <- read_csv(f_sources, show_col_types = FALSE, col_types = cols(.default = "c"))
ev <- function(m) { v <- as.numeric(existing$value[existing$macro == m]); stopifnot(length(v) == 1L); v }
bv <- function(w, g, col) bins[[col]][bins$wdec == w & bins$group == g]
required <- tribble(
  ~macro,                         ~value,
  "RTWORushMainZero",             bv(0L, "main", "rush_ratio"),
  "RTWORushMainDTen",             bv(10L, "main", "rush_ratio"),
  "RTWODeadlineMainZero",         bv(0L, "main", "deadline_2021"),
  "RTWODeadlineMainDTen",         bv(10L, "main", "deadline_2021"),
  "RTWORushSlopeMain",            slope_of("main", rush_outcome),
  "RTWORushSlopeMainSE",          slope_of("main", rush_outcome, "se"),
  "RTWORushSlopeSpouse",          slope_of("spouse", rush_outcome),
  "RTWORushSlopeSpouseSE",        slope_of("spouse", rush_outcome, "se"),
  "RTWORushSlopeAffidavit",       slope_of("af", rush_outcome),
  "RTWORushSlopeAffidavitSE",     slope_of("af", rush_outcome, "se"),
  "RTWODeadlineSlopeMain",        slope_of("main", deadline_outcome),
  "RTWODeadlineSlopeMainSE",      slope_of("main", deadline_outcome, "se"),
  "RTWODeadlineSlopeSpouse",      slope_of("spouse", deadline_outcome),
  "RTWODeadlineSlopeAffidavit",   slope_of("af", deadline_outcome),
  "RTWOAtRiskJFTwentyOne",        tot("main", "denom_jf2021")
)
stopifnot(all(required$macro %in% existing$macro))
required_check <- required %>% mutate(saved = map_dbl(macro, ev), gap = abs(saved - value))
stopifnot(all(required_check$gap < 1e-9))

# |- new macros for the text: ranges and thresholds ----
macro_rows <- list()
add_macro <- function(name, value, digits, source_file, note = "") {
  stopifnot(grepl("^RTWO[A-Za-z]+$", name), length(value) == 1L, is.finite(value),
            !name %in% existing$macro[existing$script != script_name])
  macro_rows[[name]] <<- tibble(macro = name, value = value, digits = digits,
                                formatted = formatC(value, format = "f", digits = digits, big.mark = ","),
                                source_file = path_rel(source_file, project_dir),
                                script = script_name, note = note)
}
d17 <- bins %>% filter(group == "main", wdec %in% 1:7)
add_macro("RTWORushMainDOneSevenLow", min(d17$rush_ratio), 2, f_bins, "lowest main rush, deciles 1-7")
add_macro("RTWORushMainDOneSevenHigh", max(d17$rush_ratio), 2, f_bins, "highest main rush, deciles 1-7")
add_macro("RTWODeadlineMainDOneSevenLow", min(d17$deadline_2021), 2, f_bins, "lowest main deadline ratio, deciles 1-7")
add_macro("RTWODeadlineMainDOneSevenHigh", max(d17$deadline_2021), 2, f_bins, "highest main deadline ratio, deciles 1-7")
add_macro("RTWORushMainDEight", bv(8L, "main", "rush_ratio"), 2, f_bins)
add_macro("RTWORushMainDNine", bv(9L, "main", "rush_ratio"), 2, f_bins)
add_macro("RTWODeadlineMainDEight", bv(8L, "main", "deadline_2021"), 2, f_bins)
add_macro("RTWODeadlineMainDNine", bv(9L, "main", "deadline_2021"), 2, f_bins)
for (g in c("spouse", "af")) {
  w <- c(spouse = "Spouse", af = "Affidavit")[[g]]
  r <- bins %>% filter(group == g)
  add_macro(paste0("RTWORush", w, "Low"), min(r$rush_ratio), 2, f_bins, "lowest rush over the 11 bins")
  add_macro(paste0("RTWORush", w, "High"), max(r$rush_ratio), 2, f_bins, "highest rush over the 11 bins")
}
add_macro("RTWORushSlopeMainPct", 100 * (exp(slope_of("main", rush_outcome)) - 1), 1, f_slopes,
          "percent rise in the rush per decile")
add_macro("RTWOWedgeDEightMin", cuts[7], 0, f_doses, "lower edge of decile 8, $ a year")
add_macro("RTWOWedgeDTenMin", cuts[9], 0, f_doses, "lower edge of decile 10, $ a year")
add_macro("RTWOWedgeDTenMean", doses$wedge_mean[doses$wdec == 10L], 0, f_doses, "mean wedge, decile 10")

#' Merge this script's macros into the shared sources CSV and rewrite the .tex file (as 25, 26)
write_r2_macros <- function(rows, script) {
  old <- read_csv(f_sources, show_col_types = FALSE, col_types = cols(.default = "c"))
  new <- rows %>% mutate(across(everything(), as.character))
  all_rows <- bind_rows(old %>% filter(script != !!script), new) %>% arrange(macro)
  stopifnot(!anyDuplicated(all_rows$macro))
  write_csv_strict(all_rows, f_sources)
  writeLines(c("% Generated by 25_wedge_gradient.R, 25b_gradient_exhibits.R and 26_heir_sale_hazard.R (WP-R2); do not edit by hand.",
               "% Sources: scripts/R/_outputs/tables/r2_numbers_sources.csv",
               paste0("\\newcommand{\\", all_rows$macro, "}{", all_rows$formatted, "}")), f_macros)
  message("Wrote: ", f_macros, " (", nrow(all_rows), " macros)")
}
write_r2_macros(bind_rows(macro_rows), script_name)

#============================================#
# Standalone compile test (in _outputs, not manuscript) ----
#============================================#

# relative paths from the test folder (the repository path has spaces)
rel <- function(p) gsub("\\\\", "/", path_rel(p, test_dir))
header_path <- here("Preambles", "header.tex")
test_lines <- c(
  "\\documentclass[12pt, letterpaper]{article}",
  paste0("\\input{", rel(header_path), "}"),
  "\\usepackage{adjustbox}",
  "\\usepackage[margin=1.1in]{geometry}",
  paste0("\\input{", rel(f_macros), "}"),
  "\\begin{document}",
  "\\section*{Compile test: T\\_s4\\_gradient and RTWO macros}",
  paste0("Rush \\RTWORushMainZero\\ (zero wedge), \\RTWORushMainDOneSevenLow--\\RTWORushMainDOneSevenHigh\\ ",
         "(deciles 1--7), \\RTWORushMainDEight, \\RTWORushMainDNine\\ and \\RTWORushMainDTen\\ (deciles 8--10). ",
         "Deadline ratio \\RTWODeadlineMainZero, \\RTWODeadlineMainDOneSevenLow--\\RTWODeadlineMainDOneSevenHigh, ",
         "\\RTWODeadlineMainDTen. Slope $\\RTWORushSlopeMain$ (\\RTWORushSlopeMainSE), about \\RTWORushSlopeMainPct\\ ",
         "percent per decile; placebos $\\RTWORushSlopeSpouse$ (\\RTWORushSlopeSpouseSE) and ",
         "$\\RTWORushSlopeAffidavit$ (\\RTWORushSlopeAffidavitSE). Deadline slope $\\RTWODeadlineSlopeMain$ ",
         "(\\RTWODeadlineSlopeMainSE); placebos $\\RTWODeadlineSlopeSpouse$ and $\\RTWODeadlineSlopeAffidavit$. ",
         "Decile 8 starts at \\$\\RTWOWedgeDEightMin, decile 10 at \\$\\RTWOWedgeDTenMin. ",
         "Placebo rush ranges \\RTWORushSpouseLow--\\RTWORushSpouseHigh\\ and \\RTWORushAffidavitLow--\\RTWORushAffidavitHigh."),
  paste0("\\input{", rel(f_table), "}"),
  "\\begin{figure}[!ht]\\centering",
  paste0("\\includegraphics[width=\\textwidth]{", rel(f_figure), "}"),
  "\\caption{F\\_s4\\_gradient (compile test)}",
  "\\end{figure}",
  "\\end{document}")
writeLines(test_lines, path(test_dir, "T_s4_gradient_test.tex"))
message("Wrote: ", path(test_dir, "T_s4_gradient_test.tex"))
message("Finished 25b_gradient_exhibits at ", Sys.time())
