# ============================================================
# 08: RQ3 — Within-California identification pillar (Prop 19)
# Author: Saani Rawat
# Purpose: A California-INTERNAL difference-in-differences. Prop 19 removed
#          the inherited-basis exclusion only for NON-occupant heirs /
#          non-primary residences; occupant-heir primary residences kept a
#          (narrowed) exclusion. So within CA there is a built-in, less-treated
#          control group, observed under identical state shocks (COVID
#          mortality, housing cycle, statewide anticipation).
#
#          Primary treated/control split uses absentee_buyer (recorded AT DEED
#          TIME in events.parquet, ~99% coverage) — treated = absentee
#          (non-occupant) heir, control = same-ZIP (occupant) heir. The
#          PropertyCharacteristics occupancy/type split is reported as
#          robustness (snapshot timing → potentially endogenous; see CLAUDE.md).
#
#          Inference: county-clustered (CA has 58 counties, so CRVE is
#          legitimate) PLUS a null-imposed Rademacher WILD CLUSTER BOOTSTRAP
#          implemented here (Cameron-Gelbach-Miller) — a defensible p-value,
#          not a placebo rank.
#
#          Estimated as an EVENT STUDY over the full 2017-2023 span (nothing
#          dropped); the static pre/post DiD (pre 2017-2019, post 2022-2023)
#          is reported alongside for a headline number. The control is only
#          LESS-treated, so the within-CA estimate is a LOWER BOUND.
# Inputs:  data/derived/03_family_homes/events.parquet
#          data/corelogic_extracts/by_state/prop/state=CA/*.parquet (robustness)
# Outputs: _outputs/tables/prop19_within_ca_monthly.csv      (figure series)
#          _outputs/tables/prop19_within_ca_eventstudy.csv   (ES coefficients)
#          _outputs/tables/prop19_within_ca_did.csv          (DiD + WCB p/CI)
#          _outputs/prop19_within_ca.rds
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20260625)

log_file <- path(logs_dir, "08_within_ca.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 08_within_ca at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

events_path <- path(data_dir, "events.parquet")
stopifnot(file_exists(events_path))
ev <- glue("read_parquet('{sql_quote_path(events_path)}')")
fam_classes <- "('family_person','family_other','family_estate')"

# Event-study reference = 2020 Q3, the last clean quarter before the Nov-2020
# anticipation window. yq index = sale_year*4 + quarter (consecutive integers).
yq_ref <- 2020L * 4L + 3L

# ---- (A) CA-aggregate monthly two-group series (for the F5 effect plot) ----
agg <- dbGetQuery(con, glue("
  SELECT ym, sale_year, sale_month, absentee_buyer AS treated, COUNT(*) AS n
  FROM {ev}
  WHERE state = 'CA' AND class IN {fam_classes}
    AND absentee_buyer IS NOT NULL
    AND sale_year BETWEEN 2017 AND 2023
  GROUP BY ym, sale_year, sale_month, absentee_buyer
  ORDER BY ym, treated
"))
assert_rows(agg, 150, "within_ca_monthly_agg")

agg_wide <- agg |>
  mutate(grp = if_else(treated == 1L, "absentee", "occupant")) |>
  select(ym, sale_year, sale_month, grp, n) |>
  pivot_wider(names_from = grp, values_from = n) |>
  arrange(ym) |>
  mutate(
    date = as.Date(sprintf("%d-%02d-01", sale_year, sale_month)),
    absentee_share = absentee / (absentee + occupant)
  )

# Index each group to its own 2019 monthly average; the DiD is the divergence
# of the two indices (and equivalently the change in the log ratio).
base19 <- agg_wide |>
  filter(sale_year == 2019) |>
  summarise(b_abs = mean(absentee), b_occ = mean(occupant))
agg_wide <- agg_wide |>
  mutate(
    idx_absentee = 100 * absentee / base19$b_abs,
    idx_occupant = 100 * occupant / base19$b_occ,
    log_ratio = log(absentee / occupant)
  )
write_csv_strict(agg_wide, path(tables_out_dir, "prop19_within_ca_monthly.csv"))
message("CA monthly absentee share: 2019=",
        round(mean(agg_wide$absentee_share[agg_wide$sale_year == 2019]), 3),
        "  2023=",
        round(mean(agg_wide$absentee_share[agg_wide$sale_year == 2023]), 3))

# ---- (B) County panel: (fips5, yq, treated) counts, 2017-2023 -------------
cty_raw <- dbGetQuery(con, glue("
  SELECT fips5,
         sale_year,
         CAST((sale_month - 1) / 3 AS INTEGER) + 1 AS quarter,
         absentee_buyer AS treated,
         COUNT(*) AS n
  FROM {ev}
  WHERE state = 'CA' AND class IN {fam_classes}
    AND absentee_buyer IS NOT NULL
    AND sale_year BETWEEN 2017 AND 2023
    AND fips5 IS NOT NULL AND fips5 <> ''
  GROUP BY fips5, sale_year, quarter, absentee_buyer
"))
assert_rows(cty_raw, 1000, "within_ca_county_quarter")

# Complete the panel so every (fips5, yq, treated) cell exists (n=0 if absent),
# required for an unbiased Poisson count model (no selection on positive cells).
all_yq <- cty_raw |>
  distinct(sale_year, quarter) |>
  mutate(yq = sale_year * 4L + quarter)
cty <- cty_raw |>
  mutate(yq = sale_year * 4L + quarter) |>
  complete(fips5, nesting(sale_year, quarter, yq), treated = c(0L, 1L),
           fill = list(n = 0L)) |>
  mutate(
    post = as.integer(sale_year >= 2022L),
    period = sale_year + (quarter - 1) / 4
  )
n_counties <- n_distinct(cty$fips5)
message("CA counties in panel: ", n_counties)

# Static-DiD sample: pre = 2017-2019, post = 2022-2023 (drop transition).
cty_did <- cty |> filter(sale_year %in% c(2017:2019, 2022:2023))

# ---- (C) Volume DiD via Poisson (handles zero cells) ----------------------
# fepois log-link: treated:post = the within-CA difference-in-differences in
# log family-transfer volume (treated = absentee/non-occupant heirs).
m_pois <- fepois(n ~ treated:post | fips5^treated + yq,
                 data = cty_did, cluster = ~fips5)
print(summary(m_pois))

# Event study (full span, ref = 2020 Q3): treated-minus-control by quarter.
m_es <- fepois(n ~ i(yq, treated, ref = yq_ref) | fips5^treated + yq,
               data = cty, cluster = ~fips5)

es_tab <- broom::tidy(m_es) |>
  filter(str_detect(term, "^yq::")) |>
  mutate(
    yq = as.integer(str_extract(term, "(?<=yq::)[0-9]+")),
    sale_year = yq %/% 4L,
    quarter = yq %% 4L,
    quarter = if_else(quarter == 0L, 4L, quarter),
    sale_year = if_else(yq %% 4L == 0L, sale_year - 1L, sale_year),
    date = as.Date(sprintf("%d-%02d-01", sale_year, (quarter - 1L) * 3L + 1L)),
    ci_lo = estimate - 1.96 * std.error,
    ci_hi = estimate + 1.96 * std.error
  ) |>
  bind_rows(tibble(term = "ref", yq = yq_ref, estimate = 0, std.error = 0,
                   sale_year = 2020L, quarter = 3L,
                   date = as.Date("2020-07-01"), ci_lo = 0, ci_hi = 0)) |>
  arrange(yq)
write_csv_strict(es_tab, path(tables_out_dir, "prop19_within_ca_eventstudy.csv"))

# ---- (D) Null-imposed Rademacher wild cluster bootstrap (CGM) -------------
# For the linear DiD on log(n) over a BALANCED set of counties (all cells > 0),
# clustered on county. This delivers a defensible p-value and percentile-t CI
# in place of a placebo rank. Balanced set avoids log(0); coverage reported.
pos_counties <- cty_did |>
  group_by(fips5) |>
  summarise(min_n = min(n), tot = sum(n), .groups = "drop") |>
  filter(min_n > 0)
bal <- cty_did |>
  filter(fips5 %in% pos_counties$fips5) |>
  mutate(ln_n = log(n))
cov_share <- sum(bal$n) / sum(cty_did$n)
message("Wild-bootstrap balanced panel: ", n_distinct(bal$fips5), " of ",
        n_counties, " counties, ", round(100 * cov_share, 1),
        "% of family-transfer volume.")

wild_cluster_boot <- function(data, yvar = "ln_n", cluster = "fips5",
                              param = "treated:post", B = 999L) {
  f_un <- as.formula(paste0(yvar, " ~ treated:post | fips5^treated + yq"))
  f_r  <- as.formula(paste0(yvar, " ~ 1 | fips5^treated + yq"))
  m_un <- feols(f_un, data = data, cluster = ~fips5)
  b_hat  <- coef(m_un)[param]
  se_hat <- se(m_un)[param]
  t_hat  <- b_hat / se_hat
  m_r <- feols(f_r, data = data)             # restricted: imposes H0 beta = 0
  yhat_r <- predict(m_r)
  uhat_r <- resid(m_r)
  cl <- as.character(data[[cluster]])
  gids <- unique(cl)
  tstar <- numeric(B)
  for (b in seq_len(B)) {
    w <- sample(c(-1L, 1L), length(gids), replace = TRUE)
    names(w) <- gids
    d2 <- data
    d2[[yvar]] <- yhat_r + uhat_r * w[cl]
    mb <- feols(f_un, data = d2, cluster = ~fips5)
    tstar[b] <- coef(mb)[param] / se(mb)[param]
  }
  p_boot <- (1 + sum(abs(tstar) >= abs(t_hat))) / (B + 1)
  qd <- quantile(tstar, c(0.975, 0.025), names = FALSE)
  ci <- b_hat - qd * se_hat                  # percentile-t 95% CI
  list(beta = unname(b_hat), se = unname(se_hat), t = unname(t_hat),
       p_boot = p_boot, ci_lo = ci[1], ci_hi = ci[2], B = B,
       n_clusters = length(gids))
}

m_lin <- feols(ln_n ~ treated:post | fips5^treated + yq,
               data = bal, cluster = ~fips5)
wcb <- wild_cluster_boot(bal, B = 999L)
message(sprintf("Within-CA DiD (linear, log volume): %.4f  CRVE-p=%.4f  WCB-p=%.4f  CI=[%.3f, %.3f]",
                wcb$beta, fixest::pvalue(m_lin)["treated:post"],
                wcb$p_boot, wcb$ci_lo, wcb$ci_hi))

# ---- (E) Robustness: PropertyCharacteristics occupancy/type split ----------
prop_ca_glob <- gsub("\\\\", "/", normalizePath(
  here("data/corelogic_extracts/by_state/prop/state=CA"), winslash = "/"))
pq <- glue("read_parquet('{sql_quote_path(file.path(prop_ca_glob, '*.parquet'))}')")
dbExecute(con, glue("
  CREATE OR REPLACE TEMP VIEW pca AS
  SELECT CAST(clip AS BIGINT) AS clip, owner_occupancy_code,
         property_indicator_code
  FROM {pq}"))

occ <- dbGetQuery(con, glue("
  SELECT e.fips5, e.sale_year,
         CAST((e.sale_month - 1) / 3 AS INTEGER) + 1 AS quarter,
         CASE WHEN p.owner_occupancy_code = 'A' THEN 1
              WHEN p.owner_occupancy_code = 'O' THEN 0 END AS occ_treated,
         COUNT(*) AS n
  FROM {ev} e
  JOIN pca p ON p.clip = e.clip
  WHERE e.state = 'CA' AND e.class IN {fam_classes}
    AND e.sale_year IN (2017,2018,2019,2022,2023)
    AND e.fips5 IS NOT NULL AND e.fips5 <> ''
    AND p.owner_occupancy_code IN ('A','O')
  GROUP BY 1,2,3,4
"))
occ <- occ |>
  filter(!is.na(occ_treated)) |>
  mutate(yq = sale_year * 4L + quarter, post = as.integer(sale_year >= 2022L))
m_occ <- fepois(n ~ occ_treated:post | fips5^occ_treated + yq,
                data = occ, cluster = ~fips5)
message(sprintf("Robustness (occupancy snapshot A vs O) DiD: %.4f (p=%.4f)",
                coef(m_occ)["occ_treated:post"],
                fixest::pvalue(m_occ)["occ_treated:post"]))

# ---- save -----------------------------------------------------------------
did_out <- tibble(
  spec = c("poisson_absentee", "linear_absentee_balanced", "poisson_occupancy_robust"),
  coef = c(coef(m_pois)["treated:post"], wcb$beta, coef(m_occ)["occ_treated:post"]),
  se   = c(se(m_pois)["treated:post"], wcb$se, se(m_occ)["occ_treated:post"]),
  crve_p = c(fixest::pvalue(m_pois)["treated:post"], fixest::pvalue(m_lin)["treated:post"],
             fixest::pvalue(m_occ)["occ_treated:post"]),
  wcb_p = c(NA, wcb$p_boot, NA),
  ci_lo = c(NA, wcb$ci_lo, NA),
  ci_hi = c(NA, wcb$ci_hi, NA),
  n = c(nobs(m_pois), nobs(m_lin), nobs(m_occ)),
  n_clusters = c(n_counties, wcb$n_clusters, n_distinct(occ$fips5))
)
print(did_out)
write_csv_strict(did_out, path(tables_out_dir, "prop19_within_ca_did.csv"))
saveRDS(list(pois = m_pois, es = m_es, lin = m_lin, occ = m_occ, wcb = wcb,
             monthly = agg_wide, es_tab = es_tab, did = did_out,
             coverage = cov_share),
        path(out_dir, "prop19_within_ca.rds"))

message("Finished 08_within_ca at ", Sys.time())
