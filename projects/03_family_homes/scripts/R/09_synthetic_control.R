# ============================================================
# 09: Appendix synthetic-control comparison
# Author: Saani Rawat
# Purpose: Fit donor-weighted synthetic California using each state's
#          family counts normalized by its 2018-2019 monthly mean.
#          Match January 2018-May 2020 and summarize the 2022-2023 gap.
#          This is a comparison of trajectories, without state-placebo inference.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: _outputs/tables/prop19_scm_series.csv      (CA vs synthetic)
#          _outputs/tables/prop19_scm_summary.csv   (pre-fit RMSPE and post-period gap)
#          _outputs/tables/prop19_scm_weights.csv     (donor weights)
#          _outputs/prop19_scm.rds
#          manuscript/figures/F_scm_california.png
# Log:     10/05/2026: moved event aggregation to in-memory R and applied
#          Saani coding style; preserved SCM, inference, and existing outputs.
#          10/22/2026: anticipation window constants from 00_setup.R (June 2020
#          start); donor pre-fit filter aligned to May 2020; event labels fixed.
# ============================================================

# 11/04/2026: moved SCM to appendix; removed all state-placebo inference.
# 11/04/2026: restored other interfamily to the main family proxy.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(tidysynth)
})

set.seed(20260625)

log_file <- path(logs_dir, "09_synthetic_control.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 09_synthetic_control at ", Sys.time())

# ============================================================
# Importing event data ----
# ============================================================

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))

# Broad family transfers exclude trust self-transfers.
fam_classes <- family_classes
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# Load all events into R; subsequent calculations use in-memory data frames.
ev_data <- read_events_df(events_path, columns = c(
  "state", "ym", "sale_year", "sale_month", "class"
))
message("Loaded ", format(nrow(ev_data), big.mark = ","), " events into R memory")

# ============================================================
# Building the state-month panel ----
# ============================================================

# |- Family-transfer counts, 2018-2023 ----
# Keep months with other deed classes even when their family count is zero.
ev_window <- ev_data %>%
  filter(between(sale_year, 2018L, 2023L))

ms <- ev_window %>%
  group_by(state, ym, sale_year, sale_month) %>%
  summarize(n_fam = sum(class %in% fam_classes), .groups = "drop") %>%
  filter(!state %in% territories)
assert_rows(ms, 2000, "scm_state_month")
stopifnot("CA" %in% ms$state, n_distinct(ms$state) == 51)

# |- Completing the panel and indexing months ----
# Fill missing state-month counts with zero.
tmap <- ms %>%
  distinct(ym, sale_year, sale_month) %>%
  arrange(ym) %>%
  mutate(t = row_number())
panel <- ms %>%
  left_join(tmap, by = c("ym", "sale_year", "sale_month")) %>%
  complete(state, nesting(ym, sale_year, sale_month, t), fill = list(n_fam = 0)) %>%
  arrange(state, t)

# |- Normalizing each state's family-transfer series ----
# Divide monthly transfers by that state's own 2018-2019 monthly mean.
base_mean <- panel %>%
  filter(sale_year %in% 2018:2019) %>%
  group_by(state) %>%
  summarise(base = mean(n_fam), .groups = "drop")
stopifnot(all(base_mean$base > 0))
panel <- panel %>%
  left_join(base_mean, by = "state") %>%
  mutate(rel_fam = n_fam / base)
base_ca <- base_mean$base[base_mean$state == "CA"]

t_treat <- tmap$t[tmap$ym == prop19_antic_start_ym]   # June 2020 ballot placement
pre_t   <- tmap$t[tmap$ym <= prop19_pre_end_ym]       # January 2018 - May 2020

# ============================================================
# Estimating synthetic California ----
# ============================================================
scm <- panel %>%
  synthetic_control(outcome = rel_fam, unit = state, time = t,
                    i_unit = "CA", i_time = t_treat,
                    generate_placebos = FALSE) %>%
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2018],
                     rel_2018 = mean(rel_fam, na.rm = TRUE)) %>%
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2019],
                     rel_2019 = mean(rel_fam, na.rm = TRUE)) %>%
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2020 & tmap$sale_month <= 5],
                     rel_2020 = mean(rel_fam, na.rm = TRUE)) %>%
  generate_weights(optimization_window = pre_t) %>%
  generate_control()

# CA vs synthetic series (rel units; level implied by applying CA's own base).
sc_series <- scm %>%
  grab_synthetic_control(placebo = FALSE) %>%
  rename(t = time_unit, ca_rel = real_y, synth_rel = synth_y) %>%
  left_join(tmap, by = "t") %>%
  mutate(ca_n = ca_rel * base_ca, synth_n = synth_rel * base_ca,
         gap = ca_rel - synth_rel,
         gap_rel = (ca_rel - synth_rel) / synth_rel)
write_csv_strict(sc_series, path(tables_out_dir, "prop19_scm_series.csv"))

pre_rmspe_ca <- sqrt(mean(sc_series$gap[sc_series$ym <= prop19_pre_end_ym]^2))
gap_ss_ca    <- mean(sc_series$gap[sc_series$ym >= 202201])           # rel units
gap_ss_pct   <- 100 * mean(sc_series$gap_rel[sc_series$ym >= 202201])  # % of synth
message(sprintf("CA pre-fit RMSPE (rel): %.4f | 2022-23 mean gap: %.4f (%.1f%% of counterfactual)",
                pre_rmspe_ca, gap_ss_ca, gap_ss_pct))

# ============================================================
# Plotting California and its synthetic control ----
# ============================================================
sc_plot <- sc_series %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", sale_year, sale_month)))

# Shading starts at ballot placement, the first anticipation month.
antic_start_date <- prop19_event_dates[["ballot"]]
eff_date <- prop19_event_dates[["effective"]]
xend <- max(sc_plot$date) + 20
label_top <- max(sc_plot$ca_n) / 1000 * 0.99
label_drop <- 0.7 * (max(sc_plot$ca_n) - min(sc_plot$ca_n)) / 1000

phase_rects <- tibble(
  phase = factor(c("Anticipation & Rush", "Post-reform"),
                 levels = c("Anticipation & Rush", "Post-reform")),
  xmin = c(antic_start_date, eff_date),
  xmax = c(eff_date, xend)
)

f_scm <- ggplot(sc_plot, aes(date)) +
  geom_rect(data = phase_rects, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf,
                fill = phase), alpha = 0.18) +
  geom_ribbon(aes(ymin = pmin(ca_n, synth_n) / 1000,
                  ymax = pmax(ca_n, synth_n) / 1000),
              fill = "grey80", alpha = 0.45) +
  geom_line(aes(y = ca_n / 1000, color = "California (actual)"),
            linewidth = 0.9) +
  geom_line(aes(y = synth_n / 1000, color = "Synthetic California"),
            linewidth = 0.9, linetype = "22") +
  prop19_event_layers(label_y = label_top, label_step = label_drop) +
  scale_color_manual(values = palette_paper()[c(2, 1)]) +
  scale_fill_manual(values = c(`Anticipation & Rush` = "#fbeccb",
                               `Post-reform` = "#f4ece9"), guide = "none") +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  labs(
    x = NULL,
    y = "Family transfers (thousands/month)",
    color = NULL,
    caption = sprintf(
      paste0("Synthetic California is donor-weighted SCM. Pre-period RMSPE %.3f; ",
             "2022-2023 gap %.1f%% relative to the synthetic control."),
      pre_rmspe_ca, gap_ss_pct
    )
  ) +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, size = 8))

ggsave(path(figures_dir, "F_scm_california.png"), f_scm,
       width = 9, height = 4.8, bg = "transparent", dpi = 300)
message("Saved SCM figure: F_scm_california")

# |- Exporting donor weights ----
wts <- scm %>%
  grab_unit_weights() %>%
  arrange(desc(weight))
write_csv_strict(wts, path(tables_out_dir, "prop19_scm_weights.csv"))
message("Top donor weights: ",
        paste(sprintf("%s=%.2f", head(wts$unit, 6), head(wts$weight, 6)), collapse = ", "))

# ============================================================
# Exporting fit and gap summaries ----
# ============================================================
summary_out <- tibble(
  ca_pre_rmspe = pre_rmspe_ca,
  gap_ss_rel = gap_ss_ca,
  gap_ss_pct = gap_ss_pct
)
print(summary_out)
write_csv_strict(summary_out, path(tables_out_dir, "prop19_scm_summary.csv"))
saveRDS(list(scm = scm, series = sc_series, weights = wts,
             summary = summary_out, base_ca = base_ca),
        path(out_dir, "prop19_scm.rds"))
message("Finished 09_synthetic_control at ", Sys.time())
