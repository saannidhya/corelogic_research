# ============================================================
# Purpose : Figure 2 alternative: old-definition family deeds / all deeds
# Name    : Saani Rawat
# Created : 11/04/2026
# Log     : 11/04/2026: added separate all-deed-denominator state ranking.
# Inputs  : _outputs/tables/fact_state_class_2007_2023.csv
# Outputs : _outputs/figures/F2_old_family_per_all_deeds_2007_2023.png
#           _outputs/tables/F2_old_family_per_all_deeds_2007_2023.csv
# ============================================================

# 11/04/2026: restored other interfamily to the main family proxy.
# 11/04/2026: use uniform blue bars and whole-number state labels.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

# ============================================================
# Importing pooled state counts ----
# ============================================================

state_counts <- read_csv(
  path(tables_out_dir, "fact_state_class_2007_2023.csv"),
  show_col_types = FALSE
) %>%
  filter(state %in% c(state.abb, "DC")) %>%
  mutate(
    family_old = family_person + family_estate + family_other,
    family_per_100_deeds = 100 * family_old / n_all,
    family_per_100_market_sales = 100 * family_old / market,
    # upper bound: the earlier class-based proxy, including same-person and entity deeds
    family_incl_samep_per_100_deeds = 100 * fam_incl_samep / n_all,
    family_incl_samep_per_100_market_sales = 100 * fam_incl_samep / market
  ) %>%
  arrange(desc(family_per_100_deeds)) %>%
  mutate(rank = row_number())

stopifnot(
  nrow(state_counts) == 51L,
  !anyDuplicated(state_counts$state),
  all(is.finite(state_counts$n_all) & state_counts$n_all > 0),
  all(state_counts$family_old >= 0),
  all(state_counts$family_old + state_counts$market + state_counts$trust <=
        state_counts$n_all),
  all(state_counts$fam_broad ==
        state_counts$family_person + state_counts$family_estate + state_counts$family_other)
)

# Pool counts before dividing; each residential parcel-date event counts once.
national <- state_counts %>%
  summarise(family_old = sum(family_old), n_all = sum(n_all)) %>%
  mutate(family_per_100_deeds = 100 * family_old / n_all)
california <- state_counts %>% filter(state == "CA")

write_csv_strict(
  state_counts,
  path(tables_out_dir, "F2_old_family_per_all_deeds_2007_2023.csv")
)

# ============================================================
# Plotting the state ranking ----
# ============================================================

plot_counts <- state_counts %>%
  mutate(
    state = factor(state, levels = rev(state)),
    label = number(family_per_100_deeds, accuracy = 1)
  )

family_per_deed_plot <- ggplot(
  plot_counts, aes(x = family_per_100_deeds, y = state)
) +
  geom_vline(
    xintercept = national$family_per_100_deeds,
    linetype = "dashed", linewidth = 0.5, color = "grey45"
  ) +
  geom_col(fill = palette_paper()[1], width = 0.75) +
  geom_text(aes(label = label), hjust = -0.25, size = 2.6) +
  scale_x_continuous(
    limits = c(0, ceiling(max(plot_counts$family_per_100_deeds) / 5) * 5 + 2),
    breaks = seq(0, 100, 5), expand = expansion(mult = 0)
  ) +
  labs(
    title = "Family transfers as a share of all deed transfers",
    subtitle = paste0(
      "Pooled residential deed events, 2007\u20132023\n",
      "California: ", number(california$family_per_100_deeds, accuracy = 1),
      " per 100 deeds | U.S.: ",
      number(national$family_per_100_deeds, accuracy = 0.1),
      " (dashed line)"
    ),
    x = "Family transfers per 100 deed transfers", y = NULL,
    caption = paste0(
      "Family measure (2026-11-12): same surname + estate/executor + other interfamily, excluding same-person and entity deeds.\n",
      "Denominator: all residential deed events in the sample, including market sales,\n",
      "family deeds, trusts, retitles, and other deeds. 50 states + DC; events, not unique homes."
    )
  ) +
  theme(
    axis.text.y = element_text(size = 7.5),
    panel.grid.major.y = element_blank(),
    plot.margin = margin(12, 18, 12, 12),
    plot.title = element_text(size = 12)
  )

figure_path <- path(
  figures_out_dir, "F2_old_family_per_all_deeds_2007_2023.png"
)
ggsave(
  figure_path, family_per_deed_plot,
  width = 8, height = 10, dpi = 300, bg = "white"
)

message("Saved: ", figure_path)
message("California: ", california$family_old, " / ", california$n_all,
        " = ", round(california$family_per_100_deeds, 4), " per 100 deeds")
message("U.S.: ", national$family_old, " / ", national$n_all,
        " = ", round(national$family_per_100_deeds, 4), " per 100 deeds")
