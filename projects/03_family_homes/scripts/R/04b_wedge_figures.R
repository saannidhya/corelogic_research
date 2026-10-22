#======================================================================#
# Purpose : Main and appendix figures for the 2019 parcel tax wedge
# Name    : Saani Rawat
# Created : 10/07/2026
# Log     : 10/07/2026: positive-wedge reference lines and appendix diagnostics.
# Inputs  : data/derived/03_family_homes/ca_parcel_wedge.parquet
# Outputs : manuscript/figures/wedge_*.{png,pdf}; _outputs/tables/wedge_figure_*.csv
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

build_wedge_figures <- function(parcel_panel = NULL) {
  # Settings ----
  figure_cohort <- "d2019"
  display_max <- 50000
  diagnostic_sample_size <- 50000L
  set.seed(20261007)
  figure_columns <- c("cohort", "wedge", "tenure", "hox", "assessed_d0",
                      "market_d0", "market_rs_d0", "wedge_rs", "assess_ratio")

  # Importing the selected baseline ----
  if (is.null(parcel_panel)) {
    wedge_data <- arrow::open_dataset(path(data_dir, "ca_parcel_wedge.parquet")) %>%
      filter(cohort == figure_cohort) %>%
      select(all_of(figure_columns)) %>%
      collect()
  } else {
    wedge_data <- parcel_panel %>%
      filter(cohort == figure_cohort) %>%
      select(all_of(figure_columns))
  }
  wedge_data <- wedge_data %>% filter(is.finite(wedge), wedge >= 0)
  stopifnot(nrow(wedge_data) > 0, any(wedge_data$wedge > 0))

  dollar_labels <- scales::label_dollar(scale = 1 / 1000, suffix = "k", accuracy = 1)
  figure_theme <- theme_paper(base_size = 12) +
    theme(panel.border = element_blank(), panel.grid.minor = element_blank(),
          plot.title.position = "plot", plot.caption.position = "plot",
          plot.margin = margin(12, 18, 12, 12), legend.position = "bottom")
  save_wedge_figure <- function(plot, name, width = 9, height = 5.6) {
    for (extension in c("png", "pdf")) {
      ggsave(path(figures_dir, paste0(name, ".", extension)), plot,
             width = width, height = height, dpi = 300, bg = "white")
    }
    message("Saved: ", name)
  }

  # Main distribution: all parcels, positive-wedge reference statistics ----
  wedge_summary <- wedge_data %>%
    summarise(cohort = figure_cohort, n = n(), n_positive = sum(wedge > 0),
              share_zero = mean(wedge == 0),
              mean_positive = mean(wedge[wedge > 0]),
              median_positive = median(wedge[wedge > 0]),
              share_above_display = mean(wedge > display_max))
  write_csv_strict(wedge_summary, path(tables_out_dir, "wedge_figure_summary_2019.csv"))
  print(wedge_summary, width = Inf)

  # Aggregate once; both exports use the same full-population bin counts.
  # These are equal-width bins in asinh(wedge / 500), matching the axis transform.
  histogram <- hist(asinh(wedge_data$wedge[wedge_data$wedge <= display_max] / 500),
                    breaks = seq(0, asinh(display_max / 500), length.out = 61),
                    plot = FALSE, include.lowest = TRUE)
  wedge_histogram <- tibble(xmin = 500 * sinh(head(histogram$breaks, -1)),
                            xmax = 500 * sinh(tail(histogram$breaks, -1)),
                            n = histogram$counts)
  stopifnot(sum(wedge_histogram$n) == sum(wedge_data$wedge <= display_max))
  reference_lines <- tibble(
    statistic = c("Median (positive wedges)", "Mean (positive wedges)"),
    value = c(wedge_summary$median_positive, wedge_summary$mean_positive)
  ) %>%
    mutate(label = paste0(statistic, ": ", scales::dollar(value, accuracy = 1)))
  reference_colors <- setNames(c("#1565A0", "#C45D28"), reference_lines$label)

  wedge_plot <- ggplot(wedge_histogram) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = 0, ymax = n),
              fill = "#8CAFC5", color = "white", linewidth = 0.25) +
    geom_vline(data = reference_lines, aes(xintercept = value, color = label,
                                         linetype = label), linewidth = 0.9) +
    scale_color_manual(values = reference_colors) +
    scale_linetype_manual(values = setNames(c("solid", "dashed"), reference_lines$label)) +
    scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 250),
                       breaks = c(0, 500, 1000, 3000, 10000, 30000, 50000),
                       labels = scales::label_dollar(scale = 0.001, suffix = "k", accuracy = 0.1),
                       expand = expansion(mult = c(0, 0.01))) +
    scale_y_continuous(labels = scales::label_number(scale = 0.001, big.mark = ","),
                       expand = expansion(mult = c(0, 0.05))) +
    labs(title = "Annual property-tax exposure in 2019",
         subtitle = paste0(scales::comma(wedge_summary$n), " California parcels; ",
                           scales::percent(wedge_summary$share_zero, accuracy = 0.1),
                           " have a zero wedge"),
         x = "Estimated annual tax wedge (2019 dollars; pseudo-log scale)",
         y = "Parcels (thousands)", color = NULL, linetype = NULL,
         caption = paste0("Mean and median exclude zero wedges. Equal-width bins on the transformed scale.\n",
                          "Display ends at $50,000; ",
                          scales::percent(wedge_summary$share_above_display, accuracy = 0.01),
                          " lie above it. Reference statistics use the full positive-wedge population.")) +
    figure_theme + theme(panel.grid.major.x = element_blank())
  save_wedge_figure(wedge_plot, "wedge_distribution")

  # Appendix: tenure and historical homestead status ----
  tenure_labels <- c(long_held = "No observed market sale since 2007",
                     bought_2007_2012 = "Last market sale: 2007–2012",
                     bought_2013_plus = "Last market sale: 2013–2018")
  group_summary <- wedge_data %>%
    group_by(tenure, hox) %>%
    summarise(n = n(), n_positive = sum(wedge > 0), share_zero = mean(wedge == 0),
              median_positive = median(wedge[wedge > 0]),
              p25_positive = quantile(wedge[wedge > 0], 0.25),
              p75_positive = quantile(wedge[wedge > 0], 0.75), .groups = "drop") %>%
    mutate(tenure_label = factor(tenure_labels[tenure], levels = rev(tenure_labels)),
           homestead = if_else(hox == 1, "Historical homestead exemption", "No historical homestead exemption"))
  write_csv_strict(group_summary, path(tables_out_dir, "wedge_figure_tenure_hox_2019.csv"))
  tenure_plot <- ggplot(group_summary, aes(median_positive, tenure_label, color = homestead)) +
    geom_errorbar(aes(xmin = p25_positive, xmax = p75_positive),
                  orientation = "y", width = 0.15, position = position_dodge(width = 0.5)) +
    geom_point(size = 3, position = position_dodge(width = 0.5)) +
    scale_color_manual(values = c("#1565A0", "#C45D28")) +
    scale_x_continuous(labels = dollar_labels) +
    labs(title = "Tax exposure differs across ownership histories", subtitle = "2019 baseline; positive wedges only",
         x = "Annual tax wedge (2019 dollars)", y = NULL, color = NULL,
         caption = "Points: medians. Bars: interquartile ranges, not confidence intervals.\nHomestead status comes from the historical property roll; sale history is observed from 2007.") + figure_theme
  save_wedge_figure(tenure_plot, "wedge_tenure_homestead_2019", width = 10)

  zero_plot <- ggplot(group_summary, aes(share_zero, tenure_label, fill = homestead)) +
    geom_col(position = position_dodge(width = 0.7), width = 0.6) +
    scale_fill_manual(values = c("#1565A0", "#C45D28")) +
    scale_x_continuous(labels = scales::label_percent(accuracy = 1), expand = expansion(mult = c(0, 0.05))) +
    labs(title = "How often is the estimated tax wedge zero?", subtitle = "2019 baseline; all retained parcels",
         x = "Share of parcels with a zero wedge", y = NULL, fill = NULL,
         caption = "Zero wedge means predicted market value does not exceed the reconstructed assessment base.") + figure_theme
  save_wedge_figure(zero_plot, "wedge_zero_share_2019", width = 10)

  # Appendix: assessment gaps and alternative valuation agreement ----
  # Samples serve only the dense scatter displays; population summaries above are exact.
  diagnostic_data <- wedge_data %>% slice_sample(n = min(nrow(wedge_data), diagnostic_sample_size))
  assessment_plot <- ggplot(diagnostic_data, aes(market_d0, assessed_d0)) +
    geom_point(alpha = 0.08, size = 0.5, color = "#1565A0") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#C45D28") +
    scale_x_log10(breaks = c(1e4, 1e5, 1e6, 1e7), labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
    scale_y_log10(breaks = c(1e4, 1e5, 1e6, 1e7), labels = scales::label_dollar(scale_cut = scales::cut_short_scale())) +
    coord_equal(xlim = c(1e4, 1e7), ylim = c(1e4, 1e7)) +
    labs(title = "Reconstructed assessments versus market values", subtitle = "2019 baseline",
         x = "Predicted market value (log scale)", y = "Reconstructed assessment base (log scale)",
         caption = paste0("Random display sample: ", scales::comma(nrow(diagnostic_data)),
                          " parcels. Dashed line: equal values.\nPoints below the line have a positive wedge. Display window: $10k–$10m on each axis.")) + figure_theme
  save_wedge_figure(assessment_plot, "wedge_assessment_market_2019", height = 7.2)

  comparable_data <- wedge_data %>% filter(is.finite(wedge_rs), is.finite(market_rs_d0), market_rs_d0 > 0)
  comparison_summary <- comparable_data %>%
    summarise(cohort = figure_cohort, n = n(), cor_log_value = cor(log(market_d0), log(market_rs_d0)),
              cor_wedge = cor(wedge, wedge_rs))
  write_csv_strict(comparison_summary, path(tables_out_dir, "wedge_figure_valuation_comparison_2019.csv"))
  comparable_sample <- comparable_data %>% slice_sample(n = min(nrow(comparable_data), diagnostic_sample_size))
  comparison_plot <- ggplot(comparable_sample, aes(wedge_rs, wedge)) +
    geom_point(alpha = 0.08, size = 0.5, color = "#1565A0") +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#C45D28") +
    scale_x_continuous(trans = scales::pseudo_log_trans(sigma = 250),
                       breaks = c(0, 1000, 3000, 10000, 30000, 50000), labels = dollar_labels) +
    scale_y_continuous(trans = scales::pseudo_log_trans(sigma = 250),
                       breaks = c(0, 1000, 3000, 10000, 30000, 50000), labels = dollar_labels) +
    coord_equal(xlim = c(0, display_max), ylim = c(0, display_max)) +
    labs(title = "Sensitivity to the market-value estimator",
         subtitle = paste0("2019 baseline; full eligible-sample wedge correlation = ", round(comparison_summary$cor_wedge, 2)),
         x = "Repeat-sales-based annual wedge (2019 dollars)", y = "Hedonic-based annual wedge (2019 dollars)",
         caption = paste0("Random display sample: ", scales::comma(nrow(comparable_sample)), " of ",
                          scales::comma(comparison_summary$n), " eligible parcels. Dashed line: agreement.\n",
                          "Qualifying market-sale resets only. Display window: $0–$50k on each axis (pseudo-log scale).")) + figure_theme
  save_wedge_figure(comparison_plot, "wedge_valuation_comparison_2019", height = 7.2)
  invisible(list(summary = wedge_summary, distribution = wedge_plot))
}

# Run standalone to redraw from the derived panel; the builder calls the function directly.
if (sys.nframe() == 0L) build_wedge_figures()
