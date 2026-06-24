#' 06_tables.R — Manuscript tables (LaTeX fragments) + key-numbers digest
#'
#'   T1 summary, T2 entry, T3 exit main (household volume primary),
#'   T4 heterogeneity, T5 exit robustness, T7 identification checks
#'   (vintage split, dose-response, extensive margin, placebo + inference).
#'   (T6 balance written by 05c; TA1 waterfall by 08; TA2 calibration by 09.)

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))

fmt  <- function(x, d = 3) formatC(x, format = "f", digits = d)
star <- function(p) ifelse(is.na(p), "", ifelse(p < .01, "$^{***}$",
                    ifelse(p < .05, "$^{**}$", ifelse(p < .1, "$^{*}$", ""))))
cell <- function(r, d = 3) {
  if (nrow(r) == 0 || is.na(r$estimate[1]))
    return(c("---", ""))
  c(paste0(fmt(r$estimate[1], d), star(r$p.value[1])),
    paste0("(", fmt(r$std.error[1], d), ")"))
}
get_att <- function(tt) {
  r <- tt[grepl("att|ATT", tt$term, ignore.case = TRUE), ]
  if (nrow(r) == 0) r <- tt[1, ]
  r[1, ]
}
zrow  <- function(tt) tt[grepl("zshare", tt$term) & !grepl("pre|ramp|95|hi", tt$term), ][1, ]
odrow <- function(tt) tt[grepl("odshare", tt$term), ][1, ]

key <- list()

# ---------------------------------------------------------------- inputs
panel  <- read_parquet(path(data_dir, "zip_quarter_panel.parquet"))
disp   <- read_parquet(path(data_dir, "zip_quarter_dispersion.parquet"))
zcov   <- read_parquet(path(data_dir, "zip_covariates.parquet"))
ibuyer <- read_parquet(path(data_dir, "ibuyer_deeds.parquet"))
ib_res <- readRDS(path(out_dir, "ibuyer_residuals_by_year.rds"))
fitstats <- readRDS(path(out_dir, "hedonic_fit_stats.rds"))
es     <- readRDS(path(out_dir, "entry_es_models.rds"))
erob   <- readRDS(path(out_dir, "entry_robustness.rds"))
ehet   <- readRDS(path(out_dir, "entry_heterogeneity.rds"))
emeta  <- readRDS(path(out_dir, "entry_panel_meta.rds"))
main   <- readRDS(path(out_dir, "exit_main.rds"))
xmeta  <- readRDS(path(out_dir, "exit_sample_meta.rds"))
xhet   <- readRDS(path(out_dir, "exit_heterogeneity.rds"))
xrob   <- readRDS(path(out_dir, "exit_robustness.rds"))
perm   <- readRDS(path(out_dir, "exit_permutation.rds"))
vintage <- readRDS(path(out_dir, "exit_vintage.rds"))
doseresp <- readRDS(path(out_dir, "exit_doseresponse.rds"))
extmarg <- readRDS(path(out_dir, "exit_extensive_margin.rds"))
placebo <- readRDS(path(out_dir, "exit_placebo_perm.rds"))
wildboot <- readRDS(path(out_dir, "exit_wildboot.rds"))
pretrend <- readRDS(path(out_dir, "exit_pretrend.rds"))
sortst <- readRDS(path(out_dir, "sorting_stats.rds"))

exit_oname <- c(lvol = "Log volume (household)",
                lvol_gross = "Log volume (incl.\\ machine deeds)",
                disp = "Residual SD",
                iqr = "Residual IQR/1.349",
                ltail = "Left-tail share (demeaned)",
                ltail_raw = "Left-tail share (raw)",
                lmedp = "Log median price")

# ---------------------------------------------------------------- T1 summary
ib_buys <- ibuyer |> filter(!is.na(buyer_firm))
zip_cbsa <- panel |> distinct(state, zip5) |>
  inner_join(zcov |> select(state, zip5, cbsa), by = c("state", "zip5"))
key$n_ibuyer_deeds    <- nrow(ibuyer)
key$n_ib_purchases    <- sum(!is.na(ibuyer$buyer_firm))
key$n_ib_dispositions <- sum(!is.na(ibuyer$seller_firm))
key$n_states_panel    <- n_distinct(panel$state)
key$n_zips_panel      <- nrow(distinct(panel, state, zip5))
key$n_cbsas_panel     <- n_distinct(zip_cbsa$cbsa[!is.na(zip_cbsa$cbsa)])
key$n_cells_panel     <- nrow(panel)
key$n_sales_panel     <- sum(panel$n_sales)
key$hedonic_mean_r2   <- mean(fitstats$r2, na.rm = TRUE)
key$hedonic_r2_range  <- range(fitstats$r2, na.rm = TRUE)
key$mean_disp         <- mean(disp$disp_sd, na.rm = TRUE)
key$mean_iqr          <- mean(disp$disp_iqr, na.rm = TRUE)
key$median_price      <- median(panel$med_price, na.rm = TRUE)
key$buys_2021         <- ib_buys |> filter(sale_year == 2021) |> nrow()
key$mean_ibuy_resid   <- ib_res |> filter(side == "buy") |>
  summarise(m = weighted.mean(mean_resid, n)) |> pull(m)
key$mean_isell_resid  <- ib_res |> filter(side == "sell") |>
  summarise(m = weighted.mean(mean_resid, n)) |> pull(m)
key$sorting <- sortst

firm_rows <- ib_buys |> count(buyer_firm) |>
  mutate(firm = c(offerpad = "Offerpad", opendoor = "Opendoor",
                  redfin = "RedfinNow", zillow = "Zillow Offers")[buyer_firm]) |>
  arrange(desc(n))

t1 <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Sample and algorithmic-buyer activity}",
  "\\label{tab:T1}",
  "\\begin{tabular}{lr}",
  "\\toprule",
  "\\multicolumn{2}{l}{\\emph{Panel A: ZIP $\\times$ quarter panel (2010Q1--2024Q2)}}\\\\",
  paste0("States & ", key$n_states_panel, "\\\\"),
  paste0("State-ZIP units & ", format(key$n_zips_panel, big.mark = ","), "\\\\"),
  paste0("CBSAs & ", format(key$n_cbsas_panel, big.mark = ","), "\\\\"),
  paste0("ZIP-quarter cells & ", format(key$n_cells_panel, big.mark = ","), "\\\\"),
  paste0("Arm's-length sales & ", format(key$n_sales_panel, big.mark = ","), "\\\\"),
  paste0("Median sale price (\\$) & ", format(round(key$median_price), big.mark = ","), "\\\\"),
  paste0("Mean within-cell residual SD (log pts) & ", fmt(key$mean_disp, 3), "\\\\"),
  paste0("Mean within-cell residual IQR/1.349 & ", fmt(key$mean_iqr, 3), "\\\\"),
  paste0("Mean hedonic $R^2$ (state-year) & ", fmt(key$hedonic_mean_r2, 2), "\\\\"),
  "\\midrule",
  "\\multicolumn{2}{l}{\\emph{Panel B: iBuyer purchases, 2013--2024 (national scan)}}\\\\",
  paste0(firm_rows$firm, " & ", format(firm_rows$n, big.mark = ","), "\\\\"),
  paste0("Total purchases & ", format(key$n_ib_purchases, big.mark = ","), "\\\\"),
  paste0("Total dispositions & ", format(key$n_ib_dispositions, big.mark = ","), "\\\\"),
  paste0("Mean purchase residual (log pts) & ", fmt(key$mean_ibuy_resid, 3), "\\\\"),
  paste0("Mean disposition residual (log pts) & ", fmt(key$mean_isell_resid, 3), "\\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.85\\linewidth}\\footnotesize\\vspace{2pt}",
  "\\emph{Notes:} Panel A covers the 22 iBuyer-active states: arm's-length",
  "category-A residential (SFR + condo) deeds, \\$10k--\\$10M, non-interfamily,",
  "non-foreclosure/REO. Residual dispersion within ZIP-quarter cells with at",
  "least five household-to-household trades, from state-by-year hedonic",
  "regressions. Panel B: purchases identified from first-listed buyer entity",
  "names (SEC-verified aliases, Appendix Table~\\ref{tab:entities}).",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(t1, path(tables_dir, "T1_summary.tex"))

# ------------------------------------------------------------- T2 entry ATTs
e_on <- c(lvol = "log volume", disp = "residual SD", iqr = "residual IQR/1.349",
          lmedp = "log median price", ltail = "left-tail share")
t2_body <- unlist(map(names(e_on), function(nm) {
  a  <- cell(get_att(es[[nm]]$agg))
  sq <- cell(get_att(erob[[nm]]$state_qtr))
  wt <- cell(get_att(erob[[nm]]$weighted))
  tw <- cell(erob[[nm]]$twfe[erob[[nm]]$twfe$term == "treated_post", ])
  c(paste0(e_on[[nm]], " & ", a[1], " & ", sq[1], " & ", wt[1], " & ", tw[1], "\\\\"),
    paste0(" & ", a[2], " & ", sq[2], " & ", wt[2], " & ", tw[2], "\\\\[2pt]"))
}))
t2 <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{iBuyer entry: average post-entry effects (descriptive)}",
  "\\label{tab:T2}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4)\\\\",
  "Outcome & Sun--Abraham & State$\\times$qtr FE & Stock-weighted & TWFE\\\\",
  "\\midrule",
  t2_body,
  "\\midrule",
  paste0("Pre-trend joint $p$ (vol; SD; IQR) & \\multicolumn{4}{c}{",
         fmt(es$lvol$pretrend_p, 2), "; ", fmt(es$disp$pretrend_p, 2), "; ",
         fmt(es$iqr$pretrend_p, 2), "}\\\\"),
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.95\\linewidth}\\footnotesize\\vspace{2pt}",
  "\\emph{Notes:} Average post-entry effect of CBSA-level iBuyer entry",
  "\\citep{sun2021estimating}, ZIP$\\times$quarter panel 2012Q1--2021Q3, ZIP and",
  "quarter fixed effects, never-entered CBSAs as controls. The entry design is",
  "\\emph{descriptive}: pre-trends reject for all outcomes except the",
  "IQR-based dispersion measure ($p = ", fmt(es$iqr$pretrend_p, 2), "$). SEs",
  "clustered by CBSA. $^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$.",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(t2, path(tables_dir, "T2_entry.tex"))
key$entry_att      <- map(es, \(x) get_att(x$agg))
key$entry_pretrend <- map_dbl(es, \(x) x$pretrend_p)

# ---------------------------------------------------------------- T3 exit DiD
t3_rows <- c("lvol", "lvol_gross", "disp", "iqr", "ltail", "ltail_raw", "lmedp")
t3_body <- unlist(map(t3_rows, function(nm) {
  if (is.null(main[[nm]])) return(NULL)
  tt <- main[[nm]]$tidy
  z  <- cell(zrow(tt), 4); od <- cell(odrow(tt), 4)
  sep <- if (nm == "lvol") "\\\\[1pt]\\midrule" else "\\\\[2pt]"
  c(paste0(exit_oname[[nm]], " & ", z[1], " & ", od[1], " & ",
           format(main[[nm]]$nobs, big.mark = ","), "\\\\"),
    paste0(" & ", z[2], " & ", od[2], " & ", sep))
}))
t3 <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{The Zillow Offers shutdown: difference-in-differences}",
  "\\label{tab:T3}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & Zillow share & Opendoor/Offerpad & \\\\",
  "Outcome & $\\times$ Post & share $\\times$ Post & N\\\\",
  "\\midrule",
  t3_body,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.92\\linewidth}\\footnotesize\\vspace{2pt}",
  "\\emph{Notes:} ZIP$\\times$quarter panel, 2019Q1--2024Q2, ZIPs in CBSAs with",
  "$\\geq 50$ Zillow Offers purchases (",
  paste0(xmeta$n_zip, " ZIPs, ", xmeta$n_cbsa, " CBSAs, ", xmeta$n_zero_ctrl,
         " zero-exposure controls"),
  "). The \\emph{primary} volume outcome is household-only (machine purchases",
  "and resales netted out); the gross row is shown for transparency. Exposure",
  "= firm purchases as a percent of ZIP transactions over 2018Q2--2021Q3,",
  "winsorized at the 99th percentile. Post $=$ from 2021Q4 (announced Nov 2,",
  "2021). ZIP and CBSA$\\times$quarter fixed effects; SEs clustered by CBSA (27",
  "clusters). $^{*}p<0.1$, $^{**}p<0.05$, $^{***}p<0.01$.",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(t3, path(tables_dir, "T3_exit.tex"))
key$exit_main <- map(main, \(x) x$tidy)
key$exit_nobs <- map(main, \(x) x$nobs)
key$exit_meta <- xmeta

# ------------------------------------------------------ T4 heterogeneity
xt <- xhet$disp[grepl("zshare:post:factor\\(read_terc\\)", xhet$disp$term), ]
xt <- xt[order(xt$term), ]
et <- ehet$disp$terc[grepl("read_terc::[23]:post", ehet$disp$terc$term), ]
et <- et[order(et$term), ]
terc_label <- c("Least machine-readable (T1)", "Middle (T2)",
                "Most machine-readable (T3)")
t4_body <- unlist(map(1:3, function(i) {
  xc <- cell(xt[i, ], 4)
  ec <- if (i == 1) c("(ref.)", "") else cell(et[i - 1, ], 4)
  c(paste0(terc_label[i], " & ", xc[1], " & ", ec[1], "\\\\"),
    paste0(" & ", xc[2], " & ", ec[2], "\\\\[2pt]"))
}))
t4 <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Heterogeneity by machine readability (dispersion outcome)}",
  "\\label{tab:T4}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "ZIP readability tercile & Exit: Zillow share $\\times$ Post & Entry: Post\\\\",
  "\\midrule",
  t4_body,
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.9\\linewidth}\\footnotesize\\vspace{2pt}",
  "\\emph{Notes:} Readability terciles (on distinct ZIPs) from the (negative)",
  "SD of log square footage of the ZIP residential stock; T3 $=$ most",
  "homogeneous. Outcome: within-cell residual SD. Entry column reports effects",
  "relative to T1 (the level effect is absorbed by CBSA$\\times$quarter FE).",
  "All point estimates insignificant; reported for completeness. SEs clustered",
  "by CBSA.",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(t4, path(tables_dir, "T4_heterogeneity.tex"))
key$exit_het_disp  <- xt
key$entry_het_disp <- et

# ------------------------------------------------------- T5 exit robustness
mainz_t <- function(nm) {
  tt <- main[[nm]]$tidy; zrow(tt)$statistic
}
perm_p <- function(nm) {
  if (!nm %in% colnames(perm)) return(NA_real_)
  tp <- perm[, nm]; tp <- tp[is.finite(tp)]
  (1 + sum(abs(tp) >= abs(mainz_t(nm)))) / (1 + length(tp))
}
grabz <- function(tt) cell(tt[grepl("^z(share|hi)(95)?:post$", tt$term), ][1, ], 4)
rob_rows <- c("lvol", "disp")
t5_body <- unlist(map(rob_rows, function(nm) {
  a <- grabz(xrob[[nm]]$drop_liq); b <- grabz(xrob[[nm]]$binary)
  d <- grabz(xrob[[nm]]$zip_clust); e <- grabz(xrob[[nm]]$weighted)
  g <- grabz(xrob[[nm]]$winsor95); h <- grabz(xrob[[nm]]$no_phoenix)
  pp <- perm_p(nm)
  wb <- if (!is.null(wildboot[[nm]])) fmt(wildboot[[nm]]$p, 3) else "--"
  c(paste0(exit_oname[[nm]], " & ", a[1], " & ", b[1], " & ", d[1], " & ",
           e[1], " & ", g[1], " & ", h[1], " & ", fmt(pp, 3), " & ", wb, "\\\\"),
    paste0(" & ", a[2], " & ", b[2], " & ", d[2], " & ", e[2], " & ", g[2],
           " & ", h[2], " & & \\\\[2pt]"))
}))
t5 <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Exit design: robustness and inference}",
  "\\label{tab:T5}",
  "\\resizebox{\\textwidth}{!}{%",
  "\\begin{tabular}{lcccccccc}",
  "\\toprule",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8)\\\\",
  "Outcome & Drop & Top- & ZIP & Weight & Wins.\\ & Excl.\\ & Perm.\\ & Wild\\\\",
  " & liq. & quart. & clust. & & 95th & Phx & $p$ & $p$\\\\",
  "\\midrule",
  t5_body,
  "\\bottomrule",
  "\\end{tabular}}",
  "\\begin{minipage}{0.97\\linewidth}\\footnotesize\\vspace{2pt}",
  "\\emph{Notes:} Zillow-share$\\times$Post coefficient under variants of",
  "Table~\\ref{tab:T3} (primary household-volume and SD-dispersion outcomes).",
  "(1) drops 2021Q4--2022Q2; (2) top-quartile exposure indicator; (3) ZIP",
  "clustering; (4) transaction-weighted; (5) 95th-pct winsorization; (6)",
  "excluding Phoenix; (7) permutation $p$ from 999 within-CBSA reshuffles,",
  "$(1+\\#\\{|t|\\geq|t_{obs}|\\})/(B+1)$; (8) wild cluster bootstrap (Webb",
  "weights, 999 reps, null imposed). The Opendoor/Offerpad placebo loading is",
  "near zero in every variant (Appendix).",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(t5, path(tables_dir, "T5_robustness.tex"))
key$exit_rob <- xrob
key$perm_p   <- setNames(map_dbl(c("lvol","disp","ltail"), perm_p),
                         c("lvol","disp","ltail"))
key$wildboot <- wildboot

# ------------------------------------------------------- T7 identification
# vintage split (hh volume), dose-response terciles (hh volume), extensive margin
vz_pre  <- cell(vintage$lvol[grepl("zshare_pre", vintage$lvol$term), ][1, ], 4)
vz_ramp <- cell(vintage$lvol[grepl("zshare_ramp", vintage$lvol$term), ][1, ], 4)
dose_l <- doseresp$lvol[grepl("zterc::[123]:post", doseresp$lvol$term), ]
dose_l <- dose_l[order(dose_l$term), ]
em_z <- cell(extmarg[grepl("zshare", extmarg$term) & !grepl("od", extmarg$term), ][1, ], 4)
t7 <- c(
  "\\begin{table}[!htbp]\\centering",
  "\\caption{Identification checks (household volume)}",
  "\\label{tab:T7}",
  "\\begin{tabular}{lcc}",
  "\\toprule",
  "& Coefficient & (s.e.)\\\\",
  "\\midrule",
  "\\multicolumn{3}{l}{\\emph{Panel A: vintage split of exposure}}\\\\",
  paste0("Pre-Ketchup exposure (2018Q2--2020Q4) $\\times$ Post & ", vz_pre[1],
         " & ", vz_pre[2], "\\\\"),
  paste0("Ramp exposure (2021Q1--Q3) $\\times$ Post & ", vz_ramp[1], " & ",
         vz_ramp[2], "\\\\"),
  "\\midrule",
  "\\multicolumn{3}{l}{\\emph{Panel B: dose-response (exposure terciles vs.\\ zero)}}\\\\",
  paste0("Tercile 1 $\\times$ Post & ", cell(dose_l[1, ], 4)[1], " & ",
         cell(dose_l[1, ], 4)[2], "\\\\"),
  paste0("Tercile 2 $\\times$ Post & ", cell(dose_l[2, ], 4)[1], " & ",
         cell(dose_l[2, ], 4)[2], "\\\\"),
  paste0("Tercile 3 $\\times$ Post & ", cell(dose_l[3, ], 4)[1], " & ",
         cell(dose_l[3, ], 4)[2], "\\\\"),
  "\\midrule",
  "\\multicolumn{3}{l}{\\emph{Panel C: extensive margin (cell has $\\geq 5$ trades)}}\\\\",
  paste0("Zillow share $\\times$ Post & ", em_z[1], " & ", em_z[2], "\\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begin{minipage}{0.9\\linewidth}\\footnotesize\\vspace{2pt}",
  "\\emph{Notes:} All specifications carry ZIP and CBSA$\\times$quarter fixed",
  "effects, the Opendoor/Offerpad control loading, and CBSA-clustered SEs.",
  "Panel A splits Zillow exposure by purchase vintage. Panel B replaces the",
  "continuous dose with exposure-tercile indicators (distinct ZIPs; zero-",
  "exposure ZIPs are the omitted group). Panel C regresses an indicator for",
  "the dispersion cell clearing the five-trade minimum on the treatment, to",
  "test treatment-correlated sample selection.",
  "\\end{minipage}",
  "\\end{table}"
)
writeLines(t7, path(tables_dir, "T7_identification.tex"))
key$vintage <- vintage; key$doseresp <- doseresp
key$extmarg <- extmarg; key$placebo <- placebo

# ------------------------------------------------------------ digest
saveRDS(key, path(out_dir, "key_numbers.rds"))

zx <- function(nm) {
  tt <- key$exit_main[[nm]]; if (is.null(tt)) return(paste0("- ", nm, ": NA"))
  z <- zrow(tt); o <- odrow(tt)
  paste0("- ", nm, ": z=", fmt(z$estimate, 5), " (", fmt(z$std.error, 5),
         ", p ", fmt(z$p.value, 4), "); od=", fmt(o$estimate, 5),
         " (", fmt(o$std.error, 5), ")")
}
plac_line <- map_chr(c("lvol","disp","ltail"), function(nm) {
  if (is.null(placebo$perm) || !nm %in% colnames(placebo$perm))
    return(paste0("- ", nm, ": NA"))
  tp <- placebo$perm[, nm]; tp <- tp[is.finite(tp)]
  obs <- placebo$obs[[nm]]
  paste0("- placebo ", nm, ": obs t=", fmt(obs, 3), ", perm p=",
         fmt((1 + sum(abs(tp) >= abs(obs))) / (1 + length(tp)), 3))
})
digest <- c(
  "# Key numbers (06_tables.R, R&R round)", "",
  "## Sample",
  paste0("- deeds ", key$n_ibuyer_deeds, " (buys ", key$n_ib_purchases,
         ", disp ", key$n_ib_dispositions, ")"),
  paste0("- states ", key$n_states_panel, "; state-ZIPs ", key$n_zips_panel,
         "; CBSAs ", key$n_cbsas_panel, "; cells ", key$n_cells_panel,
         "; sales ", key$n_sales_panel),
  paste0("- median price ", round(key$median_price), "; mean SD ",
         fmt(key$mean_disp, 4), "; mean IQR ", fmt(key$mean_iqr, 4),
         "; hedonic R2 ", fmt(key$hedonic_mean_r2, 3)),
  paste0("- iBuyer resid buy ", fmt(key$mean_ibuy_resid, 4), "; sell ",
         fmt(key$mean_isell_resid, 4), "; 2021 buys ", key$buys_2021),
  paste0("- sorting slope ", fmt(key$sorting$slope, 3), "; n ",
         key$sorting$n_zips),
  "",
  "## Exit DiD (zshare:post / odshare:post)",
  map_chr(t3_rows, zx),
  paste0("- N household-vol ", key$exit_nobs$lvol, "; disp ", key$exit_nobs$disp),
  "",
  "## Exit sample",
  paste0("- ", xmeta$n_zip, " ZIPs, ", xmeta$n_cbsa, " CBSAs, ",
         xmeta$n_zero_ctrl, " zero-exposure; zshare mean ",
         fmt(xmeta$zshare_mean, 3), "; pre ", fmt(xmeta$zshare_pre_mean, 3),
         "; ramp ", fmt(xmeta$zshare_ramp_mean, 3), "; odshare ",
         fmt(xmeta$odshare_mean, 3)),
  "",
  "## Vintage split (hh volume)",
  paste0("- pre: ", fmt(vintage$lvol[grepl("zshare_pre", vintage$lvol$term), ]$estimate[1], 5),
         " (", fmt(vintage$lvol[grepl("zshare_pre", vintage$lvol$term), ]$std.error[1], 5), ")"),
  paste0("- ramp: ", fmt(vintage$lvol[grepl("zshare_ramp", vintage$lvol$term), ]$estimate[1], 5),
         " (", fmt(vintage$lvol[grepl("zshare_ramp", vintage$lvol$term), ]$std.error[1], 5), ")"),
  "",
  "## Dose-response (hh volume, terciles vs zero)",
  map_chr(1:3, \(i) paste0("- T", i, ": ", fmt(dose_l$estimate[i], 5), " (",
                           fmt(dose_l$std.error[i], 5), ", p ",
                           fmt(dose_l$p.value[i], 3), ")")),
  "",
  "## Extensive margin (P(cell>=5) on zshare:post)",
  paste0("- ", fmt(extmarg[grepl('zshare', extmarg$term) & !grepl('od', extmarg$term), ]$estimate[1], 5),
         " (", fmt(extmarg[grepl('zshare', extmarg$term) & !grepl('od', extmarg$term), ]$std.error[1], 5),
         ", p ", fmt(extmarg[grepl('zshare', extmarg$term) & !grepl('od', extmarg$term), ]$p.value[1], 3), ")"),
  "",
  "## Inference",
  paste0("- perm p (999): lvol ", fmt(perm_p("lvol"), 3), "; disp ",
         fmt(perm_p("disp"), 3), "; ltail ", fmt(perm_p("ltail"), 3)),
  paste0("- wild p (999): lvol ", fmt(wildboot$lvol$p, 3), "; disp ",
         fmt(wildboot$disp$p, 3)),
  plac_line,
  "",
  "## Entry (descriptive)",
  map_chr(names(e_on), \(nm) {
    a <- get_att(es[[nm]]$agg)
    paste0("- ", nm, ": ", fmt(a$estimate, 4), " (", fmt(a$std.error, 4),
           ", p ", fmt(a$p.value, 3), "); pretrend p ",
           fmt(es[[nm]]$pretrend_p, 3))
  })
)
writeLines(digest, path(project_dir, "quality_reports", "key_numbers.md"))
message("06_tables.R complete - T1-T5,T7 + key_numbers.md.")
