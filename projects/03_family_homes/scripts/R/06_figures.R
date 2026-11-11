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
#          manuscript/figures/F_event_study_monthly.png
# Inputs:  _outputs/tables/prop19_scm_series.csv,
#          prop19_scm_summary.csv,
#          prop19_event_study_monthly.csv
# Log:     10/22/2026: export figures as PNG only.
#          10/07/2026: added full-period state ratios and family decomposition.
#          10/22/2026: use the shared setup path for state-ratio figure output.
#          10/22/2026: F4/F6 event lines relabelled (ballot placement Jun 24,
#          passage Nov 3, effective Feb 16) via prop19_event_layers().
# ============================================================

# 11/04/2026: restored other interfamily to the main family proxy.
# 11/04/2026: added a 100-deed reference bar and uniform, rounded state labels.
# 11/04/2026: removed state-placebo inference and synchronized current outputs.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20260609)

save_fig <- function(p, name, width = 9, height = 5, output_dir = figures_dir) {
  ggsave(file.path(output_dir, paste0(name, ".png")), p,
         width = width, height = height, bg = "transparent", dpi = 300)
  message("Saved figure: ", name)
}

# Descriptive figure block inserted into 06_figures.R (11/04/2026).
# Main definition: same surname + estate/executor + other interfamily.
# All denominators count deed events, not distinct homes.

# ---- F1: national size and annual path ----------------------------------
headline <- read_csv(path(tables_out_dir, "fact_headline_ratios.csv"),
                     show_col_types = FALSE) %>%
  filter(between(sale_year, window_start_year, full_year_end)) %>%
  arrange(sale_year)
stopifnot(identical(as.integer(headline$sale_year),
                    seq.int(window_start_year, full_year_end)),
          all(headline$n_all > 0),
          all(headline$fam_narrow <= headline$fam_broad),
          all(headline$fam_broad <= headline$n_all))

f1_totals <- headline %>%
  summarise(all_deeds = sum(n_all), family = sum(fam_broad),
            narrow = sum(fam_narrow)) %>%
  mutate(family_per_100 = 100 * family / all_deeds,
         narrow_per_100 = 100 * narrow / all_deeds)
family_per_100 <- f1_totals$family_per_100
narrow_per_100 <- f1_totals$narrow_per_100
f1_period <- paste0(window_start_year, "\u2013", full_year_end)
family_color <- palette_paper()[1]

f1_components <- tibble(
  component = c("Same surname + estate", "Other interfamily"),
  xmin = c(0, narrow_per_100),
  xmax = c(narrow_per_100, family_per_100)
) %>% mutate(label = number(xmax - xmin, accuracy = 0.1),
             midpoint = (xmin + xmax) / 2)

f1_size <- ggplot(f1_components) +
  annotate("rect", xmin = 0, xmax = 100, ymin = 1.77, ymax = 2.23,
           fill = "#9E9E9E") +
  geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0.77, ymax = 1.23,
                fill = component)) +
  annotate("text", x = 103, y = 2, label = "100",
           hjust = 0, size = 4.3, fontface = "bold") +
  annotate("text", x = family_per_100 + 2, y = 1,
           label = number(family_per_100, accuracy = 0.1),
           hjust = 0, size = 4.3, fontface = "bold") +
  scale_fill_manual(
    values = c(`Same surname + estate` = family_color,
               `Other interfamily` = "#6986A7"),
    breaks = c("Same surname + estate", "Other interfamily"),
    labels = c(paste0("Same surname + estate (", number(narrow_per_100, accuracy = 0.1), ")"),
               paste0("Other interfamily (", number(family_per_100 - narrow_per_100, accuracy = 0.1), ")"))
  ) +
  scale_x_continuous(limits = c(0, 112), breaks = seq(0, 100, 25),
                     expand = expansion(mult = 0)) +
  scale_y_continuous(limits = c(0.45, 2.55), breaks = c(1, 2),
                     labels = c("Family-transfer\ndeeds", "All deed\ntransfers")) +
  labs(title = "A. Size of the family channel",
       subtitle = paste0("Pooled residential deed events, ", f1_period),
       x = "Deeds per 100 total deed transfers", y = NULL, fill = NULL,
       caption = paste0(number(f1_totals$family / 1e6, accuracy = 0.1),
                         " million family-transfer deeds, ", f1_period)) +
  guides(fill = guide_legend(ncol = 1)) +
  theme(panel.grid.major.y = element_blank(),
        panel.border = element_blank(), legend.position = "bottom",
        plot.margin = margin(10, 14, 10, 10))
save_fig(f1_size, "F1_size", width = 7, height = 4.5)
save_fig(f1_size, "F1_size_decomposed", width = 7, height = 4.5)

f1_annual_ratio <- ggplot(headline, aes(sale_year, 100 * share_family_all)) +
  geom_hline(yintercept = family_per_100, linetype = "dashed",
             linewidth = 0.5, color = "grey45") +
  geom_line(linewidth = 0.9, color = family_color) +
  geom_point(size = 1.8, color = family_color) +
  scale_x_continuous(breaks = seq(window_start_year, full_year_end, 4)) +
  scale_y_continuous(limits = c(0, 20), breaks = seq(0, 20, 5),
                     expand = expansion(mult = 0)) +
  labs(title = "B. Across years", subtitle = "Main definition, including other interfamily",
       x = NULL, y = "Family transfers per 100 deed transfers",
       caption = paste0("Dashed line: pooled share (",
                         number(family_per_100, accuracy = 0.1), " per 100).")) +
  theme(plot.margin = margin(10, 14, 10, 10))
save_fig(f1_annual_ratio, "F1_annual_ratio", width = 7, height = 4.5)

f1 <- grid::grobTree(
  grid::editGrob(ggplotGrob(f1_size), name = "f1_size_panel",
                 vp = grid::viewport(x = 0.25, width = 0.5)),
  grid::editGrob(ggplotGrob(f1_annual_ratio), name = "f1_ratio_panel",
                 vp = grid::viewport(x = 0.75, width = 0.5))
)
save_fig(f1, "F1_volumes", width = 11, height = 4.8)

# ---- F2: state family share of all deed events --------------------------
st_full <- read_csv(path(tables_out_dir, "fact_state_class_2007_2023.csv"),
                    show_col_types = FALSE) %>%
  filter(state %in% c(state.abb, "DC")) %>%
  mutate(ratio = fam_broad / n_all) %>%
  arrange(desc(ratio))
stopifnot(nrow(st_full) == 51L, !anyDuplicated(st_full$state),
          all(st_full$fam_broad == st_full$family_person +
                st_full$family_estate + st_full$family_other),
          all(st_full$ratio >= 0 & st_full$ratio <= 1))
national_share <- sum(st_full$fam_broad) / sum(st_full$n_all)
st_full <- st_full %>% mutate(state = factor(state, levels = rev(state)))

f2b <- ggplot(st_full, aes(x = state, y = 100 * ratio)) +
  geom_hline(yintercept = 100 * national_share, linetype = "dashed",
             color = "grey45", linewidth = 0.45) +
  geom_col(fill = family_color, width = 0.75) +
  geom_label(aes(label = number(100 * ratio, accuracy = 1)),
             hjust = -0.15, size = 2.25, fill = "white", border.colour = NA,
             label.padding = unit(0.05, "lines")) +
  scale_y_continuous(limits = c(0, 25), expand = expansion(mult = 0)) +
  coord_flip() +
  labs(x = NULL, y = "Family transfers per 100 deed transfers, 2007\u20132023",
       caption = paste0("Main definition includes other interfamily. Dashed line: U.S. pooled share (",
                         number(100 * national_share, accuracy = 0.1), ").")) +
  theme(axis.text.y = element_text(size = 7.5))
save_fig(f2b, "F2b_state_ratio_2007_2023", width = 7, height = 8.5)

st <- read_csv(path(tables_out_dir, "fact_state_class_2017_2023.csv"),
               show_col_types = FALSE) %>% filter(state %in% c(state.abb, "DC"))
f2 <- ggplot(st, aes(reorder(state, fam_broad / n_all), 100 * fam_broad / n_all)) +
  geom_col(fill = family_color, width = 0.75) +
  geom_label(aes(label = number(100 * fam_broad / n_all, accuracy = 1)),
             hjust = -0.15, size = 2.25, fill = "white", border.colour = NA,
             label.padding = unit(0.05, "lines")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.1))) + coord_flip() +
  labs(x = NULL, y = "Family transfers per 100 deed transfers, 2017\u20132023") +
  theme(axis.text.y = element_text(size = 6.5))
save_fig(f2, "F2_state_ratio", width = 7, height = 8)

st_components <- st_full %>%
  transmute(state, narrow = family_person + family_estate,
            other = family_other, n_all) %>%
  pivot_longer(c(narrow, other), names_to = "component", values_to = "n") %>%
  mutate(component = factor(component, levels = c("narrow", "other")))
f2c <- ggplot(st_components, aes(state, 100 * n / n_all, fill = component)) +
  geom_col(width = 0.75, position = position_stack(reverse = TRUE)) + coord_flip() +
  scale_fill_manual(values = c(narrow = family_color, other = "#6986A7"),
                    labels = c(narrow = "Same surname + estate", other = "Other interfamily")) +
  labs(x = NULL, y = "Family transfers per 100 deed transfers, 2007\u20132023", fill = NULL) +
  theme(axis.text.y = element_text(size = 6.5))
save_fig(f2c, "F2c_state_ratio_decomposed_2007_2023", width = 7, height = 8.8)

f2d <- st_full %>%
  ggplot(aes(reorder(state, trust / n_all), 100 * trust / n_all)) +
  geom_col(fill = palette_paper()[2], width = 0.75) + coord_flip() +
  labs(x = NULL, y = "Interfamily trust deeds per 100 deed transfers, 2007\u20132023") +
  theme(axis.text.y = element_text(size = 6.5))
save_fig(f2d, "F2d_state_trust_ratio_2007_2023", width = 7, height = 8)


# ---- F3: KM survival curves ----------------------------------------------
km <- readRDS(path(out_dir, "hazard_km_curves.rds"))
label_map <- c(
  family_all = "Family transfers (main definition)",
  family_person = "Family transfer (same surname)",
  family_trust  = "Interfamily trust deeds",
  market_sale   = "Market purchase"
)
f3_dat <- km |>
  filter(class %in% names(label_map)) |>
  mutate(lab = factor(label_map[class], levels = unname(label_map[
    c("family_all", "family_person", "market_sale", "family_trust")
  ])))

f3 <- ggplot(f3_dat, aes(t / 12, surv, color = lab)) +
  geom_line(linewidth = 0.9) +
  scale_color_manual(values = c(
    `Family transfers (main definition)` = palette_paper()[1],
    `Family transfer (same surname)` = "#6986A7",
    `Market purchase` = palette_paper()[2],
    `Interfamily trust deeds` = palette_paper()[3]
  )) +
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

# ---- F5: synthetic California levels -------------------------------------
scm_series <- read_csv(path(tables_out_dir, "prop19_scm_series.csv"),
                       show_col_types = FALSE) |>
  mutate(date = as.Date(sprintf("%d-%02d-01", sale_year, sale_month)))
scm_summary <- read_csv(path(tables_out_dir, "prop19_scm_summary.csv"),
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
       caption = str_wrap(sprintf(paste0("Synthetic California = donor-weighted control. ",
         "Pre-period RMSPE %.3f. 2022-2023 level %.1f%% %s the counterfactual."),
         scm_summary$ca_pre_rmspe, abs(scm_summary$gap_ss_pct),
         if_else(scm_summary$gap_ss_pct < 0, "below", "above")), width = 110))
save_fig(scm_f5, "F5_scm_california")

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
