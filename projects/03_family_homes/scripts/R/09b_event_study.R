# ============================================================
# 09b: RQ3 — Monthly event study of CA family transfers (Prop 19)
# Author: Saani Rawat
# Purpose: The annual event study hides the reform-year effect because the
#          pre-deadline rush (Nov 2020-Feb 2021) and post-deadline collapse
#          offset WITHIN 2021. A monthly event study with the reference month
#          set just before passage (2020m10 = t-1) separates them: a sharp
#          positive spike in the four anticipation months, then a persistent
#          negative level through 2022-2023.
#
#          Spec: log(family transfers) ~ i(ym, CA, ref = 2020m10) | state + ym,
#          California treated, the other 50 states + DC as controls. Clustered
#          (state) CIs for the plot; because California is a single treated
#          unit, we also compute a placebo band (each donor assigned treatment
#          in turn) as the honest single-unit inference.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: _outputs/tables/prop19_event_study_monthly.csv
#          _outputs/prop19_event_study.rds
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
suppressPackageStartupMessages({ library(fixest) })
set.seed(20260625)

log_file <- path(logs_dir, "09b_event_study.log")
sink(log_file, split = TRUE); on.exit(sink(), add = TRUE)
message("Starting 09b_event_study at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
ev <- glue("read_parquet('{sql_quote_path(path(data_dir, 'events.parquet'))}')")
fam_classes <- "('family_person','family_other','family_estate')"
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

ms <- dbGetQuery(con, glue("
  SELECT state, ym, sale_year, sale_month,
         SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS n_fam
  FROM {ev} WHERE sale_year BETWEEN 2018 AND 2023
  GROUP BY state, ym, sale_year, sale_month")) |>
  filter(!state %in% territories)
assert_rows(ms, 2000, "event_study_state_month")

panel <- ms |>
  complete(state, nesting(ym, sale_year, sale_month), fill = list(n_fam = 0)) |>
  mutate(ca = as.integer(state == "CA"))
n_zero <- sum(panel$n_fam == 0)
panel$lnf <- if (n_zero == 0L) log(panel$n_fam) else log1p(panel$n_fam)
message("zero-count state-months: ", n_zero,
        if (n_zero > 0) " -> using log1p" else " -> using log")
states <- sort(unique(panel$state))
stopifnot("CA" %in% states, length(states) == 51)

# Baseline = pooled 2018-2019 (pre-COVID, pre-anticipation). A single t-1
# reference (2020m10) is contaminated: Prop 19 was a known 2020 ballot measure,
# so late 2020 already carries anticipation and is a poor zero. Pooling the
# clean pre-period gives a flat baseline; coefficients are estimated for
# 2020m1-2023m12 relative to it.
ref_set <- sort(unique(panel$ym[panel$sale_year %in% 2018:2019]))
ref_str <- paste(ref_set, collapse = ",")   # inlined into the i(ref=c(...)) call
f_es <- function(treat) as.formula(
  paste0("lnf ~ i(ym, ", treat, ", ref = c(", ref_str, ")) | state + ym"))

parse_es <- function(m, label) {
  broom::tidy(m, conf.int = TRUE) |>
    filter(str_detect(term, "^ym::")) |>
    transmute(unit = label,
              ym = as.integer(str_extract(term, "(?<=ym::)[0-9]+")),
              estimate, std.error, conf.low, conf.high)
}

# ---- Main: California treated, clustered (state) CIs ----------------------
m_ca <- feols(f_es("ca"), data = panel, cluster = ~state)
es_ca <- parse_es(m_ca, "CA") |>
  arrange(ym) |>
  mutate(date = as.Date(sprintf("%d-%02d-01", ym %/% 100, ym %% 100)))

# ---- Placebo band: each donor assigned the treatment in turn --------------
es_one <- function(st) {
  d <- panel |> mutate(trt = as.integer(state == st))
  m <- feols(f_es("trt"), data = d)
  broom::tidy(m) |>
    filter(str_detect(term, "^ym::")) |>
    transmute(state = st,
              ym = as.integer(str_extract(term, "(?<=ym::)[0-9]+")),
              estimate)
}
placebo <- map_dfr(states[states != "CA"], es_one)
placebo_band <- placebo |>
  group_by(ym) |>
  summarise(pl_lo = quantile(estimate, 0.025),
            pl_hi = quantile(estimate, 0.975),
            pl_sd = sd(estimate), .groups = "drop")

es <- es_ca |> left_join(placebo_band, by = "ym")
write_csv_strict(es, path(tables_out_dir, "prop19_event_study_monthly.csv"))
saveRDS(list(model = m_ca, es = es, ref_set = ref_set),
        path(out_dir, "prop19_event_study.rds"))

# Sanity: anticipation months positive, 2022-2023 negative & outside placebo band
antic <- es |> filter(ym >= 202011, ym <= 202102)
post  <- es |> filter(ym >= 202201)
message(sprintf("Anticipation mean coef: %+.3f (max %+.3f at %d)",
                mean(antic$estimate), max(antic$estimate), antic$ym[which.max(antic$estimate)]))
message(sprintf("2022-2023 mean coef: %+.3f; %d of %d months below placebo 2.5%%",
                mean(post$estimate), sum(post$estimate < post$pl_lo), nrow(post)))
message("Finished 09b_event_study at ", Sys.time())
