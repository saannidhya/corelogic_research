# ============================================================
# 09: RQ3 — Cross-state synthetic control + defensible inference
# Author: Saani Rawat
# Purpose: Replace the naive 2019-scaling counterfactual + placebo-RANK
#          inference with a proper SYNTHETIC CONTROL for California
#          (donor-weighted, Abadie-Diamond-Hainmueller) and inference that
#          yields a real p-value / CI rather than a bare rank:
#            (i)  SCM in-space placebo: post/pre MSPE-ratio rank (Fisher p);
#            (ii) Conley-Taber CI from the donor-state placebo gap distribution.
#
#          California is the largest state, an OUTLIER in the LEVEL of family
#          transfers, so a level-matched SCM degenerates (puts ~all weight on
#          the single largest donor and fits terribly). We therefore match on a
#          SCALE-FREE normalized index: rel_fam = n_fam / (state's own 2018-2019
#          monthly mean). Every state sits near 1.0 pre-period, so SCM matches
#          the TRAJECTORY shape (seasonality, COVID dip, 2020-21 boom). This is
#          the donor-weighted analogue of the paper's own growth counterfactual,
#          and the post-period gap in rel units is the proportional effect.
# Inputs:  data/derived/03_family_homes/events.parquet
# Outputs: _outputs/tables/prop19_scm_series.csv      (CA vs synthetic)
#          _outputs/tables/prop19_scm_placebos.csv    (all-unit gap series)
#          _outputs/tables/prop19_scm_inference.csv   (MSPE rank, CT p/CI)
#          _outputs/tables/prop19_scm_weights.csv     (donor weights)
#          _outputs/prop19_scm.rds
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(tidysynth)
})

set.seed(20260625)

log_file <- path(logs_dir, "09_scm_inference.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 09_scm_inference at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev <- glue("read_parquet('{sql_quote_path(events_path)}')")
fam_classes <- "('family_person','family_other','family_estate')"
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# ---- state x month family-transfer counts, 2018-2023 ----------------------
ms <- dbGetQuery(con, glue("
  SELECT state, ym, sale_year, sale_month,
         SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS n_fam
  FROM {ev}
  WHERE sale_year BETWEEN 2018 AND 2023
  GROUP BY state, ym, sale_year, sale_month
")) |>
  filter(!state %in% territories)
assert_rows(ms, 2000, "scm_state_month")
stopifnot("CA" %in% ms$state, n_distinct(ms$state) == 51)

# Balanced panel + sequential time index.
tmap <- ms |> distinct(ym, sale_year, sale_month) |> arrange(ym) |>
  mutate(t = row_number())
panel <- ms |>
  left_join(tmap, by = c("ym", "sale_year", "sale_month")) |>
  complete(state, nesting(ym, sale_year, sale_month, t), fill = list(n_fam = 0)) |>
  arrange(state, t)

# Scale-free outcome: each state's transfers relative to its own 2018-2019 mean.
base_mean <- panel |>
  filter(sale_year %in% 2018:2019) |>
  group_by(state) |>
  summarise(base = mean(n_fam), .groups = "drop")
stopifnot(all(base_mean$base > 0))
panel <- panel |>
  left_join(base_mean, by = "state") |>
  mutate(rel_fam = n_fam / base)
base_ca <- base_mean$base[base_mean$state == "CA"]

t_treat <- tmap$t[tmap$ym == 202011]            # first anticipation month
pre_t   <- tmap$t[tmap$ym <= 202010]            # 2018m1 - 2020m10 (validated)
stopifnot(length(pre_t) == 34L, t_treat == 35L)

# ---- synthetic control on the normalized index, with in-space placebos -----
scm <- panel |>
  synthetic_control(outcome = rel_fam, unit = state, time = t,
                    i_unit = "CA", i_time = t_treat,
                    generate_placebos = TRUE) |>
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2018],
                     rel_2018 = mean(rel_fam, na.rm = TRUE)) |>
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2019],
                     rel_2019 = mean(rel_fam, na.rm = TRUE)) |>
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2020 & tmap$sale_month <= 10],
                     rel_2020 = mean(rel_fam, na.rm = TRUE)) |>
  generate_weights(optimization_window = pre_t) |>
  generate_control()

# CA vs synthetic series (rel units; level implied by applying CA's own base).
sc_series <- scm |>
  grab_synthetic_control(placebo = FALSE) |>
  rename(t = time_unit, ca_rel = real_y, synth_rel = synth_y) |>
  left_join(tmap, by = "t") |>
  mutate(ca_n = ca_rel * base_ca, synth_n = synth_rel * base_ca,
         gap = ca_rel - synth_rel,
         gap_rel = (ca_rel - synth_rel) / synth_rel)
write_csv_strict(sc_series, path(tables_out_dir, "prop19_scm_series.csv"))

pre_rmspe_ca <- sqrt(mean(sc_series$gap[sc_series$ym <= 202010]^2))
gap_ss_ca    <- mean(sc_series$gap[sc_series$ym >= 202201])           # rel units
gap_ss_pct   <- 100 * mean(sc_series$gap_rel[sc_series$ym >= 202201])  # % of synth
message(sprintf("CA pre-fit RMSPE (rel): %.4f | 2022-23 mean gap: %.4f (%.1f%% of counterfactual)",
                pre_rmspe_ca, gap_ss_ca, gap_ss_pct))

wts <- scm |> grab_unit_weights() |> arrange(desc(weight))
write_csv_strict(wts, path(tables_out_dir, "prop19_scm_weights.csv"))
message("Top donor weights: ",
        paste(sprintf("%s=%.2f", head(wts$unit, 6), head(wts$weight, 6)), collapse = ", "))

# ---- inference (i): SCM MSPE-ratio placebo (Fisher exact p) ----------------
sig <- scm |> grab_significance()
ca_sig <- sig |> filter(unit_name == "CA")
message(sprintf("SCM MSPE-ratio: CA rank %d/%d, Fisher p = %.4f",
                ca_sig$rank, nrow(sig), ca_sig$fishers_exact_pvalue))

# ---- inference (ii): Conley-Taber CI on the 2022-2023 gap ------------------
# Placebo (donor) post-period gaps form the no-effect sampling distribution.
# Guard against poorly-fit donors by keeping those with pre-RMSPE <= 5x CA's.
gaps_all <- scm |>
  grab_synthetic_control(placebo = TRUE) |>
  rename(unit = .id, is_placebo = .placebo, t = time_unit) |>
  left_join(tmap, by = "t") |>
  mutate(gap = real_y - synth_y,
         gap_rel = (real_y - synth_y) / synth_y)
write_csv_strict(
  gaps_all |> select(unit, is_placebo, ym, sale_year, sale_month, gap, gap_rel),
  path(tables_out_dir, "prop19_scm_placebos.csv"))

unit_summ <- gaps_all |>
  group_by(unit, is_placebo) |>
  summarise(pre_rmspe = sqrt(mean(gap[ym <= 202010]^2, na.rm = TRUE)),
            gap_ss    = mean(gap[ym >= 202201], na.rm = TRUE),
            .groups = "drop")
keep <- unit_summ |> filter(is_placebo == 1, pre_rmspe <= 5 * pre_rmspe_ca)
placebo_gaps <- keep$gap_ss
ct_p  <- (1 + sum(abs(placebo_gaps) >= abs(gap_ss_ca))) / (1 + length(placebo_gaps))
ct_ci <- gap_ss_ca - quantile(placebo_gaps, c(0.975, 0.025), names = FALSE)
message(sprintf("Conley-Taber: p = %.4f, 95%% CI (rel) = [%.3f, %.3f] over %d donors (pre-fit filtered)",
                ct_p, ct_ci[1], ct_ci[2], length(placebo_gaps)))

inf_out <- tibble(
  ca_pre_rmspe = pre_rmspe_ca,
  gap_ss_rel   = gap_ss_ca,
  gap_ss_pct   = gap_ss_pct,
  scm_mspe_rank = ca_sig$rank,
  scm_mspe_p    = ca_sig$fishers_exact_pvalue,
  ct_p = ct_p, ct_ci_lo = ct_ci[1], ct_ci_hi = ct_ci[2],
  n_donors_kept = length(placebo_gaps)
)
print(inf_out)
write_csv_strict(inf_out, path(tables_out_dir, "prop19_scm_inference.csv"))

saveRDS(list(scm = scm, series = sc_series, weights = wts, significance = sig,
             gaps_all = gaps_all, inference = inf_out, base_ca = base_ca),
        path(out_dir, "prop19_scm.rds"))
message("Finished 09_scm_inference at ", Sys.time())
