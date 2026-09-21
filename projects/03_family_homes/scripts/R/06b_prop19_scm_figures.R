# ============================================================
# 06b: New Prop 19 cross-state figures from the synthetic control (09)
# Author: Saani Rawat
# Purpose: Replace the weak/redundant F5 (selection cohorts) and F6 (bare gap
#          bars) with two exhibits built on the donor-weighted synthetic control:
#   F5_scm_california     — California vs synthetic California (levels), gap shaded
#   F6_scm_gap_envelope   — the "money figure": CA's gap vs the 50-state placebo
#                           envelope, so the reader SEES CA escape the null.
#          Continuous monthly series (P2: nothing truncated). The selection
#          cohort figure is demoted to the appendix elsewhere.
# Inputs:  _outputs/tables/prop19_scm_series.csv, prop19_scm_placebos.csv,
#          prop19_scm_inference.csv
# Outputs: manuscript/figures/F5_scm_california.{pdf,png}
#          manuscript/figures/F6_scm_gap_envelope.{pdf,png}
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

save_fig <- function(p, name, width = 9, height = 5) {
  ggsave(path(figures_dir, paste0(name, ".pdf")), p,
         width = width, height = height, bg = "transparent")
  ggsave(path(figures_dir, paste0(name, ".png")), p,
         width = width, height = height, bg = "transparent", dpi = 300)
  message("Saved figure: ", name)
}

mk_date <- function(df) df |>
  mutate(date = as.Date(sprintf("%d-%02d-01", sale_year, sale_month)))

series <- read_csv(path(tables_out_dir, "prop19_scm_series.csv"),
                   show_col_types = FALSE) |> mk_date()
plc <- read_csv(path(tables_out_dir, "prop19_scm_placebos.csv"),
                show_col_types = FALSE) |> mk_date()
inf <- read_csv(path(tables_out_dir, "prop19_scm_inference.csv"),
                show_col_types = FALSE)

vl <- as.Date(c("2020-11-03", "2021-02-16"))

# ---- F5: California vs synthetic California (levels, thousands/month) ------
f5 <- ggplot(series, aes(date)) +
  geom_ribbon(aes(ymin = pmin(ca_n, synth_n) / 1000,
                  ymax = pmax(ca_n, synth_n) / 1000),
              fill = "grey80", alpha = 0.45) +
  geom_line(aes(y = ca_n / 1000, color = "California (actual)"), linewidth = 0.9) +
  geom_line(aes(y = synth_n / 1000, color = "Synthetic California"),
            linewidth = 0.9, linetype = "22") +
  geom_vline(xintercept = vl, linetype = "dotted", color = "grey30") +
  annotate("text", x = vl[2] + 20, y = max(series$ca_n / 1000) * 0.98,
           label = "Prop 19 effective", hjust = 0, size = 3, color = "grey30") +
  scale_color_manual(values = palette_paper()[c(2, 1)]) +
  labs(x = NULL, y = "CA family transfers (thousands/month)", color = NULL,
       caption = sprintf(paste0("Synthetic California = donor-weighted control (ADH). Pre-period fit RMSPE %.3f. ",
                                "2022-2023 gap %.0f%% below counterfactual; MSPE-ratio rank %d/51 (Fisher p = %.3f)."),
                         inf$ca_pre_rmspe, inf$gap_ss_pct, inf$scm_mspe_rank, inf$scm_mspe_p))
save_fig(f5, "F5_scm_california")

# ---- F6: money figure — CA gap vs the 50-state placebo envelope -----------
# Envelope restricted to donors that fit COMPARABLY WELL to California in the
# pre-period (pre-RMSPE <= 2x CA's, the standard Abadie placebo cutoff), so the
# reference distribution is the legitimate well-fit comparison set rather than
# volatile poorly-fit states that mechanically inflate the band.
prefit <- plc |> filter(ym <= 202010) |>
  group_by(unit, is_placebo) |>
  summarise(pre_rmspe = sqrt(mean(gap^2)), .groups = "drop")
keep_units <- prefit |> filter(pre_rmspe <= 2 * inf$ca_pre_rmspe) |> pull(unit)
message("F6 envelope donors (pre-RMSPE <= 2x CA): ", length(keep_units) - 1)

donors <- plc |> filter(is_placebo == 1, unit %in% keep_units)
ca_gap <- plc |> filter(is_placebo == 0)
env <- donors |> group_by(date) |>
  summarise(lo = quantile(gap_rel, 0.025), hi = quantile(gap_rel, 0.975),
            .groups = "drop")

f6 <- ggplot() +
  geom_hline(yintercept = 0, color = "grey50", linewidth = 0.3) +
  geom_line(data = donors, aes(date, gap_rel, group = unit),
            color = "grey80", linewidth = 0.25, alpha = 0.5) +
  geom_ribbon(data = env, aes(date, ymin = lo, ymax = hi),
              fill = "grey65", alpha = 0.35, inherit.aes = FALSE) +
  geom_line(data = ca_gap, aes(date, gap_rel), color = palette_paper()[2],
            linewidth = 1.1) +
  geom_vline(xintercept = vl, linetype = "dotted", color = "grey30") +
  annotate("text", x = vl[1] - 20, y = 0.55, label = "passed", hjust = 1,
           size = 3, color = "grey30") +
  scale_y_continuous(labels = percent_format()) +
  coord_cartesian(ylim = c(-0.6, 0.7)) +
  labs(x = NULL,
       y = "California minus synthetic (relative gap)",
       caption = sprintf(paste0("Red: California. Grey: each donor state run through the identical synthetic-control machinery (pre-fit-filtered). ",
                                "Shaded: 2.5-97.5%% placebo band. Conley-Taber p = %.3f."),
                         inf$ct_p))
save_fig(f6, "F6_scm_gap_envelope")

# ---- F7: inference made visible — MSPE-ratio permutation distribution ------
# Each state run as if treated; post/pre MSPE ratio = how extreme its post
# deviation is relative to its own pre-period fit. CA in the extreme tail IS
# the p-value (rank / 51). Reproduces grab_significance() from the gap series.
sigdat <- plc |>
  group_by(unit, is_placebo) |>
  summarise(pre_mspe  = mean(gap[ym <= 202010]^2),
            post_mspe = mean(gap[ym >= 202011]^2), .groups = "drop") |>
  mutate(mspe_ratio = post_mspe / pre_mspe, is_ca = unit == "CA")
ca_ratio <- sigdat$mspe_ratio[sigdat$is_ca]
ca_rank  <- sum(sigdat$mspe_ratio >= ca_ratio)
ca_p     <- ca_rank / nrow(sigdat)
message(sprintf("F7 reconstructed MSPE-ratio: CA rank %d/%d (p=%.3f)",
                ca_rank, nrow(sigdat), ca_p))

f7 <- ggplot(sigdat, aes(mspe_ratio)) +
  geom_dotplot(aes(fill = is_ca, color = is_ca), method = "histodot",
               binwidth = 0.07, dotsize = 0.9, stackratio = 1.05) +
  geom_vline(xintercept = ca_ratio, linetype = "dotted", color = palette_paper()[2]) +
  annotate("text", x = ca_ratio, y = 0.95,
           label = sprintf("California: rank %d/%d, p = %.3f", ca_rank, nrow(sigdat), ca_p),
           hjust = 1.03, size = 3.2, color = palette_paper()[2]) +
  scale_x_log10() +
  scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = palette_paper()[2]), guide = "none") +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = palette_paper()[2]), guide = "none") +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(x = "Post/pre MSPE ratio (log scale) — one point per state",
       caption = "Each state assigned the treatment in turn (synthetic-control in-space placebo). California sits in the extreme right tail.")
save_fig(f7, "F7_scm_mspe_ratio", width = 8, height = 4.2)

# ---- Monthly event study (new §6.2 headline; estimate + 95% CI, phase-shaded)
# Formal single-treated-unit inference is the SCM in-space placebo (F7); here we
# show the clustered point estimates + CIs and the three policy phases.
es <- read_csv(path(tables_out_dir, "prop19_event_study_monthly.csv"),
               show_col_types = FALSE) |> mutate(date = as.Date(date))
ballot_date <- as.Date("2020-06-26")   # ACA 11 placed Prop 19 on the ballot
pass_date   <- as.Date("2020-11-03")   # voters pass Prop 19
eff_date    <- as.Date("2021-02-16")   # parent-child provisions take effect
xend        <- max(es$date) + 20

phase_rects <- tibble(
  phase = factor(c("Anticipation", "Rush", "Post-reform"),
                 levels = c("Anticipation", "Rush", "Post-reform")),
  xmin = c(ballot_date, pass_date, eff_date),
  xmax = c(pass_date, eff_date, xend))

fev <- ggplot(es, aes(date, estimate)) +
  geom_rect(data = phase_rects, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.6) +
  scale_fill_manual(values = c(Anticipation = "#e8eef5", Rush = "#fbeccb",
                               `Post-reform` = "#f4ece9"), guide = "none") +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  geom_vline(xintercept = c(pass_date, eff_date), linetype = "dashed",
             color = "grey35", linewidth = 0.45) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  color = palette_paper()[2], linewidth = 0.45, size = 0.22) +
  # phase labels along the top
  annotate("text", x = ballot_date + (pass_date - ballot_date) / 2, y = 0.46,
           label = "Anticipation", size = 2.9, color = "grey40", fontface = "italic") +
  annotate("text", x = pass_date + (eff_date - pass_date) / 2, y = 0.46,
           label = "Rush", size = 3.0, color = "#9a6700", fontface = "bold.italic") +
  annotate("text", x = as.Date("2022-05-01"), y = 0.46,
           label = "Post-reform decline", size = 2.9, color = "grey40", fontface = "italic") +
  # event-date callouts near the dashed lines
  annotate("text", x = pass_date - 14, y = -0.66, label = "Pass date\nNov 2020",
           hjust = 1, vjust = 0, size = 2.6, color = "grey25", lineheight = 0.9) +
  annotate("text", x = eff_date + 14, y = -0.66, label = "Effective date\nFeb 2021",
           hjust = 0, vjust = 0, size = 2.6, color = "grey25", lineheight = 0.9) +
  scale_y_continuous(labels = scales::label_number(style_positive = "plus")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  coord_cartesian(ylim = c(-0.74, 0.52)) +
  labs(x = NULL, y = "Family transfers vs 2018-2019 baseline (log points)") +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, size = 8))
save_fig(fev, "F_event_study_monthly", width = 9, height = 4.8)

message("Finished 06b_prop19_scm_figures at ", Sys.time())
