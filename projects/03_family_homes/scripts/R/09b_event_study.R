# ============================================================
# 09b: RQ3 — Monthly event study of CA family transfers (Prop 19)
# Author: Saani Rawat
# Purpose: The annual event study hides the reform-year effect because the
#          pre-deadline rush (June 2020-Feb 2021) and post-deadline collapse
#          offset starting 2021. A monthly event study relative to the pooled
#          2018-2019 baseline separates them: a sharp
#          positive spike in the four anticipation months, then a persistent
#          negative level through 2022-2023.
#
#          Spec: log(family transfers) ~ i(ym, CA, ref = 2018-2019) | state + ym,
#          with log1p used if any state-month count is zero.
#          California treated, the other 49 states + DC as controls. Clustered
#          (state) CIs for the plot; because California is a single treated
#          unit, we also compute a placebo band (each donor assigned treatment
#          in turn) as the honest single-unit inference.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: _outputs/tables/prop19_event_study_monthly.csv
#          _outputs/prop19_event_study.rds
# Log:     10/04/2026: moved monthly aggregation into R memory.
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
suppressPackageStartupMessages({
  library(fixest)
})
set.seed(20260625)

log_file <- path(logs_dir, "09b_event_study.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 09b_event_study at ", Sys.time())

# ============================================================
# Importing event data ----
# ============================================================

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))

# Trust self-transfers are not counted as family transfers.
fam_classes <- c("family_person", "family_other", "family_estate")
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# Load events into R; aggregation and estimation use in-memory data frames.
ev_data <- arrow::read_parquet(events_path, as_data_frame = TRUE)
message("Loaded ", format(nrow(ev_data), big.mark = ","), " events into R memory")

# ============================================================
# Building the state-month panel ----
# ============================================================

# Count all three family classes; missing class values contribute zero.
# Keep months with non-family deeds so their zero family counts remain observed.
ms <- ev_data %>%
  filter(between(sale_year, 2018L, 2023L)) %>%
  group_by(state, ym, sale_year, sale_month) %>%
  summarize(n_fam = sum(class %in% fam_classes), .groups = "drop") %>%
  filter(!state %in% territories)
assert_rows(ms, 2000, "event_study_state_month")

panel <- ms %>%
  complete(state, nesting(ym, sale_year, sale_month), fill = list(n_fam = 0)) %>%
  mutate(ca = as.integer(state == "CA"))
n_zero <- sum(panel$n_fam == 0)
panel$lnf <- if (n_zero == 0L) log(panel$n_fam) else log1p(panel$n_fam)
message("zero-count state-months: ", n_zero,
        if (n_zero > 0) " -> using log1p" else " -> using log")
states <- sort(unique(panel$state))
stopifnot("CA" %in% states, length(states) == 51)

# |- Pooled pre-period references ----
# Baseline = pooled 2018-2019 (pre-COVID, pre-anticipation). A single t-1
# reference (2020m10) is contaminated: Prop 19 was a known 2020 ballot measure,
# so late 2020 already carries anticipation and is a poor zero. Pooling the
# clean pre-period gives a flat baseline; coefficients are estimated for
# 2020m1-2023m12 relative to it.
ref_set <- sort(unique(panel$ym[panel$sale_year %in% 2018:2019]))
# ref_set <- 202003L
ref_str <- paste(ref_set, collapse = ",")   # inlined into the i(ref=c(...)) call
f_es <- function(treat) as.formula(
  paste0("lnf ~ i(ym, ", treat, ", ref = c(", ref_str, ")) | state + ym"))

parse_es <- function(m, label) {
  broom::tidy(m, conf.int = TRUE) %>%
    filter(str_detect(term, "^ym::")) %>%
    transmute(unit = label,
              ym = as.integer(str_extract(term, "(?<=ym::)[0-9]+")),
              estimate, std.error, conf.low, conf.high)
}

# ============================================================
# Estimating the California event study ----
# ============================================================
m_ca <- feols(f_es("ca"), data = panel, cluster = ~state)
summary(m_ca)
es_ca <- parse_es(m_ca, "CA") %>%
  arrange(ym) %>%
  mutate(date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)))

# ============================================================
# Estimating donor placebo bands ----
# ============================================================
es_one <- function(st) {
  message("Estimating placebo: ", st)
  d <- panel %>% mutate(trt = as.integer(state == st))
  m <- feols(f_es("trt"), data = d)
  broom::tidy(m) %>%
    filter(str_detect(term, "^ym::")) %>%
    transmute(state = st,
              ym = as.integer(str_extract(term, "(?<=ym::)[0-9]+")),
              estimate)
}
placebo <- map_dfr(states[states != "CA"], es_one)
placebo_band <- placebo %>%
  group_by(ym) %>%
  summarise(pl_lo = quantile(estimate, 0.025),
            pl_hi = quantile(estimate, 0.975),
            pl_sd = sd(estimate), .groups = "drop")

# ============================================================
# Exporting results ----
# ============================================================

# Combining the California event study results with the placebo band ----
es <- es_ca %>% left_join(placebo_band, by = "ym")


# Plotting the event study results ----
legislature_date <- as.Date("2020-06-24")  # Approved by the California State Legislature
eff_date         <- as.Date("2021-02-16")  # parent-child provisions take effect
xend        <- max(es$date) + 20
phase_rects <- tibble(
  phase = factor(c("Anticipation & Rush", "Post-reform"),
                 levels = c("Anticipation & Rush", "Post-reform")),
  xmin = c(legislature_date, eff_date),
  xmax = c(eff_date, xend))

fev <- ggplot(es, aes(date, estimate)) +
  geom_rect(data = phase_rects, inherit.aes = FALSE,
            aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf, fill = phase),
            alpha = 0.6) +
  scale_fill_manual(values = c(`Anticipation & Rush` = "#fbeccb",
                               `Post-reform` = "#f4ece9"), guide = "none") +
  geom_hline(yintercept = 0, color = "grey45", linewidth = 0.35) +
  geom_vline(xintercept = c(legislature_date, eff_date), linetype = "dashed",
             color = "grey35", linewidth = 0.45) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high),
                  color = palette_paper()[2], linewidth = 0.45, size = 0.22) +
  # phase labels along the top
  annotate("text", x = legislature_date + (eff_date - legislature_date) / 2, y = 0.46,
           label = "Anticipation & Rush", size = 3.0, color = "#9a6700",
           fontface = "bold.italic") +
  annotate("text", x = as.Date("2022-05-01"), y = 0.46,
           label = "Post-reform decline", size = 2.9, color = "grey40", fontface = "italic") +
  # event-date callouts near the dashed lines
  annotate("text", x = legislature_date - 14, y = -0.66,
           label = "Prop 19 pass date\nJune 2020",
           hjust = 1, vjust = 0, size = 2.6, color = "grey25", lineheight = 0.9) +
  annotate("text", x = eff_date + 14, y = -0.66, label = "Prop 19 Effective date\nFeb 2021",
           hjust = 0, vjust = 0, size = 2.6, color = "grey25", lineheight = 0.9) +
  scale_y_continuous(labels = scales::label_number(style_positive = "plus")) +
  scale_x_date(date_breaks = "6 months", date_labels = "%b %Y",
               expand = expansion(mult = c(0.01, 0.02))) +
  coord_cartesian(ylim = c(-0.74, 0.52)) +
  labs(x = NULL, y = "Family transfers in California (log points)") +
  theme(panel.grid.major.x = element_blank(),
        axis.text.x = element_text(angle = 0, size = 8))
# fev

write_csv_strict(es, path(tables_out_dir, "prop19_event_study_monthly.csv"))
saveRDS(list(model = m_ca, es = es, ref_set = ref_set),
        path(out_dir, "prop19_event_study.rds"))

# Sanity: anticipation months positive, 2022-2023 negative & outside placebo band
antic <- es %>% filter(ym >= 202011, ym <= 202102)
post  <- es %>% filter(ym >= 202201)
message(sprintf("Anticipation mean coef: %+.3f (max %+.3f at %d)",
                mean(antic$estimate), max(antic$estimate), antic$ym[which.max(antic$estimate)]))
message(sprintf("2022-2023 mean coef: %+.3f; %d of %d months below placebo 2.5%%",
                mean(post$estimate), sum(post$estimate < post$pl_lo), nrow(post)))
message("Finished 09b_event_study at ", Sys.time())
