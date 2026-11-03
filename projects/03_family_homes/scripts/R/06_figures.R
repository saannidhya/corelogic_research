# ============================================================
# 06: Manuscript figures
# Author: Saani Rawat
# Purpose: Figures for paper.tex, built ONLY from _outputs/ artifacts
#          produced by 02-04, 09, and 09b (no direct CoreLogic access here).
# Inputs:  _outputs/tables/*.csv, _outputs/*.rds
# Outputs: manuscript/figures/F1_volumes.png
#          manuscript/figures/F1_size.png, F1_size_decomposed.png
#          manuscript/figures/F1_annual_ratio.png
#          manuscript/figures/F2_state_ratio.png
#          manuscript/figures/F2b_state_ratio_2007_2023.png
#          manuscript/figures/F2c_state_ratio_decomposed_2007_2023.png
#          manuscript/figures/F3_km_curves.png
#          manuscript/figures/F4_prop19_bunching.png
#          manuscript/figures/F5_sold24_cohorts.png
#          manuscript/figures/F5_scm_california.png
#          manuscript/figures/F6_prop19_quarterly_gap.png
#          manuscript/figures/F7_prop19_placebo_ranks.png
#          manuscript/figures/F7_scm_mspe_ratio.png
#          manuscript/figures/F_event_study_monthly.png
# Inputs:  _outputs/tables/prop19_scm_series.csv,
#          prop19_scm_placebos.csv, prop19_scm_inference.csv,
#          prop19_event_study_monthly.csv
# Log:     10/22/2026: export figures as PNG only.
#          10/07/2026: added full-period state ratios and family decomposition.
#          10/22/2026: use the shared setup path for state-ratio figure output.
#          10/22/2026: F4/F6 event lines relabelled (ballot placement Jun 24,
#          passage Nov 3, effective Feb 16) via prop19_event_layers().
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

save_fig <- function(p, name, width = 9, height = 5, output_dir = figures_dir) {
  ggsave(file.path(output_dir, paste0(name, ".png")), p,
         width = width, height = height, bg = "transparent", dpi = 300)
  message("Saved figure: ", name)
}

# ---- F1: national size and persistence -----------------------------------
headline <- read_csv(path(tables_out_dir, "fact_headline_ratios.csv"),
                     show_col_types = FALSE) |>
  filter(sale_year >= window_start_year, sale_year <= full_year_end) |>
  arrange(sale_year)

# Pool event counts before dividing; do not average annual ratios.
stopifnot(identical(as.integer(headline$sale_year),
                    seq.int(window_start_year, full_year_end)),
          all(is.finite(headline$market) & headline$market > 0),
          all(is.finite(headline$fam_broad) & headline$fam_broad >= 0),
          all(is.finite(headline$fam_conservative) &
                headline$fam_conservative >= 0 &
                headline$fam_conservative <= headline$fam_broad))

f1_totals <- headline |>
  summarise(market = sum(market), family = sum(fam_broad),
            same_surname = sum(fam_conservative)) |>
  mutate(family_per_100 = 100 * family / market,
         same_surname_per_100 = 100 * same_surname / market,
         other_family_per_100 = family_per_100 - same_surname_per_100)

family_per_100 <- f1_totals$family_per_100
family_label <- number(family_per_100, accuracy = 1)
f1_period <- paste0(min(headline$sale_year), "\u2013", max(headline$sale_year))
f1_count_label <- paste0(number(f1_totals$family / 1e6, accuracy = 0.1),
                         " million recorded family-transfer deeds, ", f1_period)
family_color <- palette_paper()[1]
market_color <- "#9E9E9E"

f1_bar_data <- tibble(
  channel = c("Market sales", "Family-transfer deeds"),
  y = c(2, 1), per_100 = c(100, family_per_100),
  label = c("100", family_label)
)

f1_size <- ggplot(f1_bar_data) +
  geom_rect(aes(xmin = 0, xmax = per_100, ymin = y - 0.23,
                ymax = y + 0.23, fill = channel)) +
  geom_text(aes(x = per_100 + 3, y = y, label = label),
            hjust = 0, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Market sales" = market_color,
                               "Family-transfer deeds" = family_color)) +
  scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0.45, 2.55), breaks = c(1, 2),
                     labels = c("Family-transfer\ndeeds", "Market sales")) +
  labs(title = paste0("For every 100 market sales,\n", family_label,
                       " family-transfer deeds are recorded"),
       subtitle = paste0("Pooled residential deed events, ", f1_period),
       x = "Deeds per 100 market sales", y = NULL,
       caption = f1_count_label) +
  theme(legend.position = "none", panel.border = element_blank(),
        panel.grid.major.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = margin(12, 18, 12, 12))
save_fig(f1_size, "F1_size", width = 9, height = 5)

# Main family definition plus the interfamily deeds without a surname match,
# which only the wide definition counts (estate deeds are too few to show).
wide_per_100 <- 100 * sum(headline$fam_wide) / f1_totals$market
f1_component_data <- tibble(
  xmin = c(0, 0, family_per_100),
  xmax = c(100, family_per_100, wide_per_100),
  y = c(2, 1, 1),
  component = c("Market sales", "Main definition (same surname + estate)",
                "Other interfamily (wide definition only)")
)
f1_component_labels <- f1_component_data |>
  filter(y == 1) |>
  mutate(x = (xmin + xmax) / 2, label = number(xmax - xmin, accuracy = 1))

f1_size_decomposed <- ggplot(f1_component_data) +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = y - 0.23, ymax = y + 0.23,
                fill = component)) +
  geom_text(data = f1_component_labels, aes(x = x, y = y, label = label),
            color = "white", size = 4.5, fontface = "bold") +
  annotate("text", x = 103, y = 2, label = "100", hjust = 0, size = 5, fontface = "bold") +
  scale_fill_manual(values = c("Market sales" = market_color,
                               "Main definition (same surname + estate)" = family_color,
                               "Other interfamily (wide definition only)" = "#A9B8CC"),
                    breaks = c("Main definition (same surname + estate)",
                               "Other interfamily (wide definition only)")) +
  scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0.45, 2.55), breaks = c(1, 2),
                     labels = c("Family-transfer
deeds", "Market sales")) +
  labs(subtitle = paste0("Pooled residential deed events, ", f1_period),
       x = "Deeds per 100 market sales", y = NULL, fill = NULL) +
  theme(legend.position = "bottom", panel.border = element_blank(),
        panel.grid.major.y = element_blank(), axis.ticks.y = element_blank(),
        plot.margin = margin(12, 18, 12, 12))
save_fig(f1_size_decomposed, "F1_size_decomposed", width = 9, height = 5)

f1_ratio_data <- headline |>
  mutate(family_per_100 = 100 * fam_broad / market)
f1_annual_ratio <- ggplot(f1_ratio_data, aes(sale_year, family_per_100)) +
  geom_hline(yintercept = family_per_100, linetype = "dashed",
             linewidth = 0.6, color = "grey45") +
  geom_line(linewidth = 1, color = family_color) +
  geom_point(size = 2, color = family_color) +
  annotate("text", x = 2016, y = 36,
           label = paste0("Pooled ratio: ", family_label, " per 100"),
           size = 3.6, color = "grey35") +
  scale_x_continuous(breaks = seq(window_start_year, full_year_end, 2)) +
  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 10),
                     expand = expansion(mult = 0)) +
  labs(title = "The family channel remains substantial throughout the sample",
       subtitle = paste0("Annual residential deed events, ", f1_period),
       x = NULL, y = "Family deeds per 100 market sales",
       caption = "Dashed line: ratio of pooled family deeds to pooled market sales.") +
  theme(plot.margin = margin(12, 18, 12, 12))
save_fig(f1_annual_ratio, "F1_annual_ratio", width = 9, height = 5)

# Assemble the paper panels with base grid, avoiding new package dependencies.
f1_size_paper <- f1_size +
  labs(title = "A. Size of the family channel")
f1_ratio_paper <- f1_annual_ratio +
  labs(title = "B. Across years")
f1 <- grid::grobTree(
  grid::editGrob(ggplotGrob(f1_size_paper),
                 name = "f1_size_panel",
                 vp = grid::viewport(x = 0.25, width = 0.5)),
  grid::editGrob(ggplotGrob(f1_ratio_paper),
                 name = "f1_ratio_panel",
                 vp = grid::viewport(x = 0.75, width = 0.5))
)
save_fig(f1, "F1_volumes", width = 11, height = 4.8)
message("Fact 1 pooled family deeds per 100 market sales: ",
        round(family_per_100, 4))

# ---- F2: state family:market ratio ---------------------------------------
st <- read_csv(path(tables_out_dir, "fact_state_class_2017_2023.csv"),
               show_col_types = FALSE) |>
  filter(!state %in% c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")) |>
  mutate(ratio = fam_broad / market) |>
  arrange(desc(ratio))

f2 <- ggplot(st, aes(x = reorder(state, ratio), y = ratio)) +
  geom_col(fill = palette_paper()[1], width = 0.75) +
  coord_flip() +
  labs(x = NULL, y = "Family transfers per market sale, 2017–2023") +
  theme(axis.text.y = element_text(size = 6.5))
save_fig(f2, "F2_state_ratio", width = 7, height = 8)

# ---- F2b: state family:market ratio, pooled 2007-2023 ----
st_full <- read_csv(path(tables_out_dir, "fact_state_class_2007_2023.csv"),
                    show_col_types = FALSE) |>
  filter(state %in% c(state.abb, "DC")) |>
  mutate(ratio = fam_broad / market) |>
  arrange(desc(ratio))

stopifnot(nrow(st_full) == 51L, !anyDuplicated(st_full$state),
          all(is.finite(st_full$market) & st_full$market > 0),
          all(is.finite(st_full$ratio) & st_full$ratio >= 0),
          all(st_full$fam_broad ==
                st_full$family_person + st_full$family_estate))

# Preserve the total-ratio ranking in the decomposed chart as well.
st_full <- st_full |>
  mutate(state = factor(state, levels = rev(state)))

f2b <- ggplot(st_full, aes(x = state, y = ratio)) +
  geom_col(fill = palette_paper()[1], width = 0.75) +
  coord_flip() +
  labs(x = NULL, y = "Family transfers per market sale, 2007\u20132023") +
  theme(axis.text.y = element_text(size = 6.5))
save_fig(f2b, "F2b_state_ratio_2007_2023", width = 7, height = 8)

# ---- F2c: main family ratio plus the deeds only the wide definition adds ----
# Estate deeds are too few to show separately; they stay in the main segment.
st_family_components <- st_full |>
  transmute(state,
            main = family_person + family_estate,
            other = family_other,
            market) |>
  pivot_longer(c(main, other), names_to = "component", values_to = "n") |>
  mutate(component = factor(component, levels = c("main", "other")),
         ratio = n / market)

f2c <- ggplot(st_family_components, aes(x = state, y = ratio, fill = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) +
  coord_flip() +
  scale_fill_manual(values = c(main = palette_paper()[1], other = "#A9B8CC"),
                    labels = c(main = "Main definition (same surname + estate)",
                               other = "Other interfamily (wide definition only)")) +
  guides(fill = guide_legend(ncol = 1)) +
  labs(x = NULL, y = "Family transfers per market sale, 2007–2023", fill = NULL) +
  theme(axis.text.y = element_text(size = 6.5))
save_fig(f2c, "F2c_state_ratio_decomposed_2007_2023", width = 7, height = 8.8)

# ---- F2d: state trust transfers as a proportion of market sales ----------
f2d <- st_full |>
  mutate(trust_ratio = trust / market) |>
  mutate(state = reorder(state, trust_ratio)) |>
  ggplot(aes(x = state, y = trust_ratio)) +
  geom_col(fill = palette_paper()[2], width = 0.75) +
  coord_flip() +
  scale_y_continuous(labels = percent_format()) +
  labs(x = NULL, y = "Interfamily trust deeds per market sale, 2007–2023
(mostly self-transfers into or out of the owner's own trust)") +
  theme(axis.text.y = element_text(size = 6.5))
save_fig(f2d, "F2d_state_trust_ratio_2007_2023", width = 7, height = 8)



# ---- F3: KM survival curves ----------------------------------------------
km <- readRDS(path(out_dir, "hazard_km_curves.rds"))
label_map <- c(
  family_person = "Family transfer (same surname)",
  family_trust  = "Trust self-transfer",
  market_sale   = "Market purchase"
)
f3_dat <- km |>
  filter(class %in% names(label_map)) |>  # family_estate excluded: n = 915
  mutate(lab = label_map[class])

f3 <- ggplot(f3_dat, aes(t / 12, surv, color = lab)) +
  geom_line(linewidth = 0.9) +
  scale_color_paper() +
  scale_y_continuous(limits = c(0.4, 1), labels = percent_format()) +
  scale_x_continuous(breaks = 0:10) +
  guides(color = guide_legend(nrow = 2)) +
  labs(x = "Years since transfer event (2008–2018 cohorts)",
       y = "Share not yet sold on the open market", color = NULL)
save_fig(f3, "F3_km_curves")

# ---- F4: Prop 19 bunching ------------------------------------------------
cf <- read_csv(path(tables_out_dir, "prop19_ca_counterfactual_monthly.csv"),
               show_col_types = FALSE) |>
  mutate(date = as.Date(paste0(sale_year, "-", sale_month, "-01")),
         gap = (ca_actual - ca_cf) / ca_cf)

# prop 19 important dates:
# Passed Legislature: June 24–26, 2020 (Approved by the Assembly and Senate)
# Chaptered by Secretary of State: July 1, 2020 (Officially placed on the ballot)
# Voted on by Public: November 3, 2020 (Approved by voters)
# Effective Date: February 16, 2021 (for intergenerational transfers) and April 1, 2021

f4 <- ggplot(cf, aes(date)) +
  geom_line(aes(y = ca_actual / 1000, color = "California, actual"), linewidth = 0.9) +
  geom_line(aes(y = ca_cf / 1000, color = "Counterfactual (2019 CA x donor growth)"),
            linewidth = 0.9, linetype = "22") +
  # ballot placement, voter passage, and parent-child effective date
  prop19_event_layers(label_y = max(cf$ca_actual) / 1000 * 0.99,
                      label_step = 0.7 * (max(cf$ca_actual) - min(cf$ca_actual)) / 1000) +
  scale_color_manual(values = palette_paper()[c(2, 1)]) +
  labs(x = NULL, y = "CA family transfers (thousands/month)", color = NULL)
save_fig(f4, "F4_prop19_bunching")

# ---- F5: sold-within-24m by cohort ---------------------------------------
cells <- read_csv(path(tables_out_dir, "prop19_cohort_cells.csv"),
                  show_col_types = FALSE)
f5_dat <- cells |>
  filter(grp == "fam") |>
  mutate(group = if_else(ca == 1, "California", "Rest of US"),
         date = as.Date(paste0(ym %/% 100, "-", ym %% 100, "-01"))) |>
  group_by(group, date, post) |>
  summarise(sold24 = weighted.mean(sold24, n), .groups = "drop")

f5 <- ggplot(f5_dat, aes(date, sold24, color = group)) +
  geom_point(size = 1.4, alpha = 0.85) +
  geom_line(aes(group = interaction(group, post)), linewidth = 0.5, alpha = 0.6) +
  scale_color_manual(values = palette_paper()[c(2, 1)]) +
  scale_y_continuous(labels = percent_format()) +
  labs(x = "Family-transfer cohort month",
       y = "Sold on open market within 24 months", color = NULL)
save_fig(f5, "F5_sold24_cohorts")

# ---- F6: quarterly Prop 19 counterfactual gaps ----------------------------
gap_q <- cf |>
  mutate(
    quarter = (sale_month - 1L) %/% 3L + 1L,
    quarter_date = as.Date(paste0(sale_year, "-", (quarter - 1L) * 3L + 1L, "-01"))
  ) |>
  group_by(quarter_date) |>
  summarise(
    ca_actual = sum(ca_actual),
    ca_cf = sum(ca_cf),
    gap = (ca_actual - ca_cf) / ca_cf,
    .groups = "drop"
  ) |>
  mutate(direction = if_else(gap >= 0, "Actual above counterfactual",
                             "Actual below counterfactual"))

f6 <- ggplot(gap_q, aes(quarter_date, gap, fill = direction)) +
  geom_hline(yintercept = 0, color = "grey35", linewidth = 0.3) +
  geom_col(width = 70, alpha = 0.88) +
  prop19_event_layers(label_y = 0, show_labels = FALSE) +
  scale_fill_manual(values = c("Actual above counterfactual" = palette_paper()[2],
                               "Actual below counterfactual" = palette_paper()[1])) +
  scale_y_continuous(labels = percent_format()) +
  guides(fill = guide_legend(nrow = 1)) +
  labs(x = NULL, y = "Actual minus counterfactual", fill = NULL)
save_fig(f6, "F6_prop19_quarterly_gap")

# ---- F7: placebo ranks, raw and pre-fit-adjusted --------------------------
prefit <- read_csv(path(tables_out_dir, "prop19_cf_placebo_summary.csv"),
                   show_col_types = FALSE) |>
  filter(base_window == "2019")

placebo_rank_dat <- bind_rows(
  prefit |> transmute(state = treated_state, window = "Anticipation",
                      statistic = "Raw gap", value = antic_gap),
  prefit |> transmute(state = treated_state, window = "Anticipation",
                      statistic = "Gap / pre-fit RMSPE", value = antic_ratio),
  prefit |> transmute(state = treated_state, window = "February 2021",
                      statistic = "Raw gap", value = feb_gap),
  prefit |> transmute(state = treated_state, window = "February 2021",
                      statistic = "Gap / pre-fit RMSPE", value = feb_ratio),
  prefit |> transmute(state = treated_state, window = "2022-2023 decline",
                      statistic = "Raw gap", value = -post2223_gap),
  prefit |> transmute(state = treated_state, window = "2022-2023 decline",
                      statistic = "Gap / pre-fit RMSPE", value = post2223_ratio)
) |>
  group_by(window, statistic) |>
  mutate(rank = min_rank(desc(value)),
         is_ca = state == "CA") |>
  ungroup()

f7 <- ggplot(placebo_rank_dat, aes(rank, value, color = is_ca)) +
  geom_point(alpha = 0.9, size = 1.8) +
  facet_grid(window ~ statistic, scales = "free_y") +
  scale_color_manual(values = c("FALSE" = "grey70", "TRUE" = palette_paper()[2]),
                     guide = "none") +
  scale_x_continuous(breaks = c(1, 10, 20, 30, 40, 51)) +
  labs(x = "Placebo rank (1 = largest policy-direction deviation)",
       y = "Statistic")
save_fig(f7, "F7_prop19_placebo_ranks", width = 8, height = 7)

# ---- F5: synthetic California levels -------------------------------------
scm_series <- read_csv(path(tables_out_dir, "prop19_scm_series.csv"),
                       show_col_types = FALSE) |>
  mutate(date = as.Date(sprintf("%d-%02d-01", sale_year, sale_month)))
scm_placebos <- read_csv(path(tables_out_dir, "prop19_scm_placebos.csv"),
                         show_col_types = FALSE) |>
  mutate(date = as.Date(sprintf("%d-%02d-01", sale_year, sale_month)))
scm_inference <- read_csv(path(tables_out_dir, "prop19_scm_inference.csv"),
                          show_col_types = FALSE)

scm_label_top <- max(scm_series$ca_n) / 1000 * 0.99
scm_label_drop <- 0.7 * (max(scm_series$ca_n) - min(scm_series$ca_n)) / 1000

scm_f5 <- ggplot(scm_series, aes(date)) +
  geom_ribbon(aes(ymin = pmin(ca_n, synth_n) / 1000,
                  ymax = pmax(ca_n, synth_n) / 1000),
              fill = "grey80", alpha = 0.45) +
  geom_line(aes(y = ca_n / 1000, color = "California (actual)"), linewidth = 0.9) +
  geom_line(aes(y = synth_n / 1000, color = "Synthetic California"),
            linewidth = 0.9, linetype = "22") +
  prop19_event_layers(label_y = scm_label_top, label_step = scm_label_drop) +
  scale_color_manual(values = palette_paper()[c(2, 1)]) +
  labs(x = NULL, y = "CA family transfers (thousands/month)", color = NULL,
       # wrapped: the unwrapped caption ran past the panel edge; abs() avoids
       # printing "gap -29% below"
       caption = str_wrap(sprintf(paste0("Synthetic California = donor-weighted control (ADH). ",
                                         "Pre-period fit RMSPE %.3f. 2022-2023 level %.0f%% ",
                                         "%s the counterfactual; MSPE-ratio rank %d/51 ",
                                         "(Fisher p = %.3f)."),
                                  scm_inference$ca_pre_rmspe, abs(scm_inference$gap_ss_pct),
                                  if_else(scm_inference$gap_ss_pct < 0, "below", "above"),
                                  scm_inference$scm_mspe_rank, scm_inference$scm_mspe_p),
                          width = 110))
save_fig(scm_f5, "F5_scm_california")

# ---- F7: synthetic-control MSPE-ratio placebo distribution ----------------
scm_sigdat <- scm_placebos |>
  group_by(unit, is_placebo) |>
  summarise(pre_mspe = mean(gap[ym <= prop19_antic_start_ym]^2),
            post_mspe = mean(gap[ym > prop19_antic_start_ym]^2), .groups = "drop") |>
  mutate(mspe_ratio = post_mspe / pre_mspe, is_ca = unit == "CA")
scm_ca_ratio <- scm_sigdat$mspe_ratio[scm_sigdat$is_ca]
scm_ca_rank <- sum(scm_sigdat$mspe_ratio >= scm_ca_ratio)
scm_ca_p <- scm_ca_rank / nrow(scm_sigdat)
message(sprintf("F7 reconstructed MSPE-ratio: CA rank %d/%d (p=%.3f)",
                scm_ca_rank, nrow(scm_sigdat), scm_ca_p))

# Stack dots by hand within log bins: geom_dotplot stacked CA and non-CA
# separately, so a state sharing California's bin was hidden behind it.
scm_dots <- scm_sigdat |>
  mutate(bin = floor(log10(mspe_ratio) / 0.07)) |>
  arrange(bin, is_ca) |>
  group_by(bin) |>
  mutate(stack = row_number(), x = mspe_ratio) |>
  ungroup()
scm_f7 <- ggplot(scm_dots, aes(x, stack)) +
  geom_point(aes(fill = is_ca, color = is_ca), shape = 21, size = 3.2) +
  geom_vline(xintercept = scm_ca_ratio, linetype = "dotted", color = palette_paper()[2]) +
  annotate("text", x = scm_ca_ratio, y = max(scm_dots$stack) + 0.6,
           label = sprintf("California: rank %d/%d, p = %.3f",
                           scm_ca_rank, nrow(scm_sigdat), scm_ca_p),
           hjust = 1.03, size = 3.2, color = palette_paper()[2]) +
  scale_x_log10() +
  scale_fill_manual(values = c("FALSE" = "grey75", "TRUE" = palette_paper()[2]),
                    guide = "none") +
  scale_color_manual(values = c("FALSE" = "grey60", "TRUE" = palette_paper()[2]),
                     guide = "none") +
  scale_y_continuous(NULL, breaks = NULL, expand = expansion(add = c(0.6, 1))) +
  labs(x = "Post/pre MSPE ratio (log scale) — one point per state",
       caption = sprintf(paste0("Each state assigned the treatment in turn (synthetic-control ",
                                "in-space placebo). California ranks %d of %d."),
                         scm_ca_rank, nrow(scm_sigdat)))
save_fig(scm_f7, "F7_scm_mspe_ratio", width = 8, height = 4.2)

# ---- Monthly Prop. 19 event study ----------------------------------------
scm_event_study <- read_csv(path(tables_out_dir, "prop19_event_study_monthly.csv"),
                            show_col_types = FALSE) |>
  mutate(date = as.Date(date))
antic_start_date <- prop19_event_dates[["ballot"]]
eff_date <- prop19_event_dates[["effective"]]
xend <- max(scm_event_study$date) + 20

phase_rects <- tibble(
  phase = factor(c("Anticipation & Rush", "Post-reform"),
                 levels = c("Anticipation & Rush", "Post-reform")),
  xmin = c(antic_start_date, eff_date),
  xmax = c(eff_date, xend))

scm_event_plot <- ggplot(scm_event_study, aes(date, estimate)) +
  geom_rect(data = phase_rects, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.6) +
  scale_fill_manual(values = c(`Anticipation & Rush` = "#fbeccb",
                                `Post-reform` = "#f4ece9"), guide = "none") +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  prop19_event_layers(label_y = -0.50, label_step = 0.12) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  color = palette_paper()[2], linewidth = 0.45, size = 0.22) +
  annotate("text", x = antic_start_date + (eff_date - antic_start_date) / 2, y = 0.46,
           label = "Anticipation & Rush", size = 3.0, color = "#9a6700",
           fontface = "bold.italic") +
  annotate("text", x = as.Date("2022-05-01"), y = 0.46,
           label = "Post-reform decline", size = 2.9, color = "grey40",
           fontface = "italic") +
  scale_y_continuous(labels = scales::label_number(style_positive = "plus")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  coord_cartesian(ylim = c(-0.74, 0.52)) +
  labs(x = NULL, y = "Family transfers in California (log points)") +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, size = 8))
save_fig(scm_event_plot, "F_event_study_monthly", width = 9, height = 4.8)

message("Finished 06_figures at ", Sys.time())
