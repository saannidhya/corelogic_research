# ============================================================
# 10: RQ3 — Demographic (65+) adjustment of the cross-state SCM
# Author: Saani Rawat
# Purpose: Address the differential-mortality / aging confound the paper flags:
#          CA's elderly population and COVID mortality differ from donors, which
#          could mechanically move family-transfer volume. We re-run the
#          synthetic control on family transfers PER 65+ RESIDENT (ACS 1-year,
#          B09020_001). If the gap survives, the decline is not a demographic
#          artifact. (The 65+ stock is the tidycensus-supported channel; the
#          residual death-TIMING channel is bounded by magnitude — single-digit
#          mortality differentials cannot explain a ~33% transfer decline.)
# Inputs:  data/derived/03_family_homes/events.parquet
#          Census ACS 1-year via tidycensus (key in .env, gitignored)
# Outputs: data/external/state_pop65_acs.parquet
#          _outputs/tables/prop19_scm_pc_inference.csv
#          _outputs/prop19_scm_pc.rds
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(tidysynth)
  library(tidycensus)
})

set.seed(20260625)
log_file <- path(logs_dir, "10_mortality.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)
message("Starting 10_mortality at ", Sys.time())

# ---- Census API key from gitignored .env ----------------------------------
readRenviron(here::here(".env"))
key <- Sys.getenv("CENSUS_API_KEY")
stopifnot(nchar(key) > 30)
census_api_key(key, install = FALSE)

# ---- 65+ population by state-year (ACS 1-year; 2020 not released) ----------
name_to_abb <- tibble(NAME = c(state.name, "District of Columbia"),
                      state = c(state.abb, "DC"))
pull_year <- function(yr) {
  get_acs(geography = "state", variables = c(pop65 = "B09020_001"),
          year = yr, survey = "acs1") |>
    transmute(NAME, sale_year = yr, pop65 = estimate)
}
pop_raw <- map_dfr(c(2018, 2019, 2021, 2022, 2023), pull_year) |>
  inner_join(name_to_abb, by = "NAME") |>
  select(state, sale_year, pop65)
# 2020 ACS 1-year unavailable -> linear interpolate from 2019 and 2021.
pop_2020 <- pop_raw |> filter(sale_year %in% c(2019, 2021)) |>
  group_by(state) |>
  summarise(sale_year = 2020L, pop65 = mean(pop65), .groups = "drop")
pop65 <- bind_rows(pop_raw, pop_2020) |> arrange(state, sale_year)
stopifnot(n_distinct(pop65$state) == 51, nrow(pop65) == 51 * 6)
arrow::write_parquet(pop65, path(here("data", "external"), "state_pop65_acs.parquet"))
message("65+ population pulled: ", nrow(pop65), " state-years, CA 2023 = ",
        format(pop65$pop65[pop65$state == "CA" & pop65$sale_year == 2023], big.mark = ","))

# ---- state x month family transfers, joined to 65+ population --------------
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

tmap <- ms |> distinct(ym, sale_year, sale_month) |> arrange(ym) |>
  mutate(t = row_number())
panel <- ms |>
  left_join(tmap, by = c("ym", "sale_year", "sale_month")) |>
  complete(state, nesting(ym, sale_year, sale_month, t), fill = list(n_fam = 0)) |>
  left_join(pop65, by = c("state", "sale_year")) |>
  mutate(fam_pc = 1e5 * n_fam / pop65)            # transfers per 100k residents 65+
stopifnot(all(is.finite(panel$fam_pc)))

# Scale-free index: per-capita transfers relative to own 2018-2019 mean.
base_pc <- panel |> filter(sale_year %in% 2018:2019) |>
  group_by(state) |> summarise(base = mean(fam_pc), .groups = "drop")
panel <- panel |> left_join(base_pc, by = "state") |>
  mutate(rel_pc = fam_pc / base)

t_treat <- tmap$t[tmap$ym == 202011]
pre_t   <- tmap$t[tmap$ym <= 202010]

# ---- SCM on the per-65+-resident index ------------------------------------
scm <- panel |>
  synthetic_control(outcome = rel_pc, unit = state, time = t,
                    i_unit = "CA", i_time = t_treat, generate_placebos = TRUE) |>
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2018],
                     rel_2018 = mean(rel_pc, na.rm = TRUE)) |>
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2019],
                     rel_2019 = mean(rel_pc, na.rm = TRUE)) |>
  generate_predictor(time_window = tmap$t[tmap$sale_year == 2020 & tmap$sale_month <= 10],
                     rel_2020 = mean(rel_pc, na.rm = TRUE)) |>
  generate_weights(optimization_window = pre_t) |>
  generate_control()

series <- scm |> grab_synthetic_control(placebo = FALSE) |>
  rename(t = time_unit, ca_rel = real_y, synth_rel = synth_y) |>
  left_join(tmap, by = "t") |>
  mutate(gap = ca_rel - synth_rel, gap_rel = (ca_rel - synth_rel) / synth_rel)
pre_rmspe_ca <- sqrt(mean(series$gap[series$ym <= 202010]^2))
gap_ss_ca    <- mean(series$gap[series$ym >= 202201])
gap_ss_pct   <- 100 * mean(series$gap_rel[series$ym >= 202201])

sig <- scm |> grab_significance()
ca_sig <- sig |> filter(unit_name == "CA")

gaps_all <- scm |> grab_synthetic_control(placebo = TRUE) |>
  rename(unit = .id, is_placebo = .placebo, t = time_unit) |>
  left_join(tmap, by = "t") |> mutate(gap = real_y - synth_y)
unit_summ <- gaps_all |> group_by(unit, is_placebo) |>
  summarise(pre_rmspe = sqrt(mean(gap[ym <= 202010]^2, na.rm = TRUE)),
            gap_ss = mean(gap[ym >= 202201], na.rm = TRUE), .groups = "drop")
placebo_gaps <- unit_summ |> filter(is_placebo == 1, pre_rmspe <= 5 * pre_rmspe_ca) |> pull(gap_ss)
ct_p <- (1 + sum(abs(placebo_gaps) >= abs(gap_ss_ca))) / (1 + length(placebo_gaps))

inf_out <- tibble(
  outcome = "family transfers per 65+ resident",
  ca_pre_rmspe = pre_rmspe_ca, gap_ss_rel = gap_ss_ca, gap_ss_pct = gap_ss_pct,
  scm_mspe_rank = ca_sig$rank, scm_mspe_p = ca_sig$fishers_exact_pvalue, ct_p = ct_p)
print(inf_out)
write_csv_strict(inf_out, path(tables_out_dir, "prop19_scm_pc_inference.csv"))
saveRDS(list(scm = scm, series = series, inference = inf_out, pop65 = pop65),
        path(out_dir, "prop19_scm_pc.rds"))

# ---- raw vs demographic-adjusted comparison -------------------------------
raw <- read_csv(path(tables_out_dir, "prop19_scm_inference.csv"), show_col_types = FALSE)
message(sprintf("RAW SCM:        gap %.1f%%  MSPE-p %.3f", raw$gap_ss_pct, raw$scm_mspe_p))
message(sprintf("PER-65+ SCM:    gap %.1f%%  MSPE-p %.3f", gap_ss_pct, ca_sig$fishers_exact_pvalue))
message("Finished 10_mortality at ", Sys.time())
