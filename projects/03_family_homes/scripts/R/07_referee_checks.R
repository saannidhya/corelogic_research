# ============================================================
# 07: Referee checks — headline triple difference + leads
# Author: Saani Rawat
# Purpose: Estimate the headline volume DDD, annual event study,
#          and deed-count reconciliation.
# Inputs:  data/derived/03_family_homes/events.parquet (via events_panel_file in 00_setup.R)
# Outputs: prop19_ddd_volume.csv/rds, ref_event_study.csv/rds,
#          ref_reconciliation.csv,
#          ref_state_year_panel.csv
# Log:     10/22/2026: added the pooled family-vs-market triple difference
#          for volumes, the paper's headline post-reform estimate.
#          11/11/2026: removed the estate/trust-seller market-sale test.
# ============================================================

# 11/04/2026: removed state-reassignment inference; retained model specifications.
# 11/04/2026: restored other interfamily to the main family proxy.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20260610)

log_file <- path(logs_dir, "07_referee_checks.log")
sink(log_file, split = TRUE)
on.exit(sink(), add = TRUE)

message("Starting 07_referee_checks at ", Sys.time())

con <- open_corelogic_duckdb()
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)

events_path <- events_panel_path
stopifnot(file_exists(events_path))
# Section 4 measure (author's decision 2026-11-12): this legacy Prop 19 script uses the same
# family measure as Section 4 (bygroup_main in 00_groups.R, via family_s4_sql() in 00_setup.R),
# so the appendix synthetic control / event study match the main text. read_events_df(measure =
# "s4") labels those deeds 'family_main'.
ev <- events_s4_from(events_path)
fam_classes <- "('family_main')"
territories <- c("GU", "PR", "VI", "AS", "MP", "AE", "AP", "AA", "FM", "MH", "PW")

# ---- shared state-year panel (2017-2023, all years for event study) -------
sy_all <- dbGetQuery(con, glue("
  SELECT state, sale_year,
         SUM(CASE WHEN class IN {fam_classes} THEN 1 ELSE 0 END) AS n_fam,
         SUM(CASE WHEN class = 'market_sale' THEN 1 ELSE 0 END)  AS n_market
  FROM {ev}
  WHERE sale_year BETWEEN 2017 AND 2023
  GROUP BY state, sale_year
")) |>
  filter(!state %in% territories)
assert_rows(sy_all, 300, "ref_state_year_panel")

write_csv_strict(sy_all, path(tables_out_dir, "ref_state_year_panel.csv"))

sy_did <- sy_all |>
  filter(sale_year %in% prop19_did_years) |>
  mutate(post = as.integer(sale_year %in% prop19_post_years))
stopifnot(all(sy_did$n_fam > 0), all(sy_did$n_market > 0))

states <- sort(unique(sy_did$state))
stopifnot("CA" %in% states, length(states) >= 45)

# ---- Headline triple difference: family vs market deeds ----
# Group-specific state and year effects make the DDD equal the difference
# between the separate family and market DiD coefficients.
sy_stacked <- sy_did |>
  select(state, sale_year, post, n_fam, n_market) |>
  pivot_longer(c(n_fam, n_market), names_to = "grp", values_to = "n") |>
  mutate(fam = as.integer(grp == "n_fam"),
         ca = as.integer(state == "CA"))

m_ddd_vol <- feols(log(n) ~ ca:post:fam + ca:post | state^grp + sale_year^grp,
                   data = sy_stacked, cluster = ~state)
print(summary(m_ddd_vol))

ddd_vol_coef <- unname(coef(m_ddd_vol)["ca:post:fam"])
volume_models <- readRDS(path(out_dir, "prop19_did_volume.rds"))
net_ca <- unname(coef(volume_models$fam)["ca:post"] -
                 coef(volume_models$market_placebo)["ca:post"])
stopifnot(abs(ddd_vol_coef - net_ca) < 1e-8)

ddd_vol_summary <- tibble(
  estimate = ddd_vol_coef,
  pct_effect = 100 * (exp(ddd_vol_coef) - 1),
  crve_se_state = unname(se(m_ddd_vol)["ca:post:fam"]),
  n_states = length(states),
  nobs = nobs(m_ddd_vol)
)
message("Volume triple difference (family vs market, CA x Post):")
print(ddd_vol_summary, width = Inf)
write_csv_strict(ddd_vol_summary, path(tables_out_dir, "prop19_ddd_volume.csv"))
saveRDS(list(model = m_ddd_vol, summary = ddd_vol_summary),
        path(out_dir, "prop19_ddd_volume.rds"))

# ---- (c) Event-study leads/lags (volume) ----------------------------------
es_dat <- sy_all |> mutate(ca = as.integer(state == "CA"))
m_es <- feols(log(n_fam) ~ i(sale_year, ca, ref = 2019) | state + sale_year,
              data = es_dat, cluster = ~state)
ct <- as.data.frame(coeftable(m_es))
es_tab <- ct |>
  rownames_to_column("term") |>
  filter(grepl("sale_year::", term)) |>
  mutate(year = as.integer(gsub("sale_year::(\\d+):ca", "\\1", term))) |>
  select(year, estimate = Estimate, std.error = `Std. Error`)
message("Event study (CA x year, ref 2019; conventional state-clustered SEs; one treated state):")
print(es_tab)
write_csv_strict(es_tab, path(tables_out_dir, "ref_event_study.csv"))
saveRDS(m_es, path(out_dir, "ref_event_study.rds"))

# ---- (e) Reconciliation stats ---------------------------------------------
recon <- dbGetQuery(con, glue("
  SELECT
    SUM(CASE WHEN class IN {fam_classes} AND sale_year <= 2023 THEN 1 ELSE 0 END) AS fam_broad_0723,
    SUM(CASE WHEN class = 'market_sale' AND sale_year <= 2023 THEN 1 ELSE 0 END)  AS market_0723,
    SUM(CASE WHEN class = 'market_sale' AND sale_year <= 2023
              AND coalesce(new_construction, 0) <> 1 THEN 1 ELSE 0 END)           AS market_existing_0723,
    COUNT(DISTINCT CASE WHEN class IN {fam_classes} AND sale_year <= 2023
                        THEN clip END)                                            AS fam_clips_0723
  FROM {ev}
  WHERE sale_year BETWEEN 2007 AND 2023
"))
recon <- recon |>
  mutate(
    ratio_all = fam_broad_0723 / market_0723,
    ratio_existing = fam_broad_0723 / market_existing_0723,
    deeds_per_fam_clip = fam_broad_0723 / fam_clips_0723
  )
message("Reconciliation:")
print(as.data.frame(recon))
write_csv_strict(recon, path(tables_out_dir, "ref_reconciliation.csv"))

# ---- (f) The triple difference by route of family transfer (11/16/2026) ------
# Same design as the headline (state-year log counts, prop19_pre_years against
# prop19_post_years, market
# sales as the comparison deed group), run on each route of the family measure. Affidavits
# of death are the comparison: a death cannot be timed, and a home passing at death kept the
# old rule whenever the death came before the deadline. Table 4, Panel B (05_tables.R).
route_defs <- tribble(
  ~route,                 ~label,                                   ~filter_sql,
  "all",                  "All family transfers",                   "deed_cat = 'family_transfer'",
  "lifetime",             "Lifetime deeds",                         "fam_route IN ('lifetime','unknown')",
  "lifetime_surname",     "\\quad of which same surname",           "fam_route IN ('lifetime','unknown') AND deed_subcat = 'same_surname'",
  "trust",                "Through a trust",                        "fam_route = 'trust'",
  "affidavit",            "Affidavits of death",                    "fam_route = 'death_affidavit'",
  "no_affidavit",         "All except affidavits of death",         "deed_cat = 'family_transfer' AND fam_route <> 'death_affidavit'",
  # deeds outside the measure that the reform does not touch but refinancing does
  "same_owner",           "Not family: same owner on both sides",   "deed_cat = 'same_owner'",
  "own_trust",            "Not family: into or out of own trust",   "deed_subcat IN ('into_own_trust','trust_to_settlor')"
)
route_cols <- paste(glue("SUM(coalesce({route_defs$filter_sql}, FALSE)::INT) AS {route_defs$route}"),
                    collapse = ",\n         ")
sy_route <- dbGetQuery(con, glue("
  SELECT state, sale_year, SUM((deed_cat = 'market_sale')::INT) AS market,
         {route_cols}
  FROM read_parquet('{sql_quote_path(events_path)}')
  WHERE sale_year IN ({paste(prop19_did_years, collapse = ', ')})
    AND state NOT IN ('GU','PR','VI','AS','MP','AE','AP','AA','FM','MH','PW')
  GROUP BY 1, 2")) |>
  as_tibble() |>
  mutate(across(-state, as.numeric),
         ca = as.integer(state == "CA"), post = as.integer(sale_year %in% prop19_post_years))
stopifnot(n_distinct(sy_route$state) == 51, sum(sy_route$ca) == length(prop19_did_years))

ddd_route <- map_dfr(seq_len(nrow(route_defs)), function(i) {
  # states with the route recorded in every year compared (affidavits are absent in a few states)
  d <- sy_route |>
    transmute(state, sale_year, ca, post, fam = .data[[route_defs$route[i]]], market) |>
    filter(fam > 0, market > 0) |>
    group_by(state) |> filter(n() == length(prop19_did_years)) |> ungroup()
  m_did <- feols(log(fam) ~ ca:post | state + sale_year, d, cluster = ~state)
  long <- bind_rows(transmute(d, state, sale_year, ca, post, grp = "F", n = fam),
                    transmute(d, state, sale_year, ca, post, grp = "M", n = market)) |>
    mutate(f = as.integer(grp == "F"))
  m_ddd <- feols(log(n) ~ ca:post + ca:post:f | state^grp + sale_year^grp, long, cluster = ~state)
  ca_d <- filter(d, ca == 1)
  tibble(route = route_defs$route[i], label = route_defs$label[i], n_states = n_distinct(d$state),
         ca_per_year_pre  = mean(ca_d$fam[ca_d$post == 0]),
         ca_per_year_post = mean(ca_d$fam[ca_d$post == 1]),
         did = coeftable(m_did)["ca:post", 1],   did_se = coeftable(m_did)["ca:post", 2],
         ddd = coeftable(m_ddd)["ca:post:f", 1], ddd_se = coeftable(m_ddd)["ca:post:f", 2])
})
# the pooled row is the headline triple difference
stopifnot(abs(ddd_route$ddd[ddd_route$route == "all"] - ddd_vol_coef) < 1e-6)
write_csv_strict(ddd_route, path(tables_out_dir, "prop19_ddd_by_route.csv"))
print(as.data.frame(ddd_route |> mutate(across(where(is.numeric), ~ round(.x, 3)))))

# macros for the text (\PRoute<Route><Stat>)
route_tag <- c(all = "All", lifetime = "Life", lifetime_surname = "LifeSur", trust = "Trust",
               affidavit = "Aff", no_affidavit = "NoAff", same_owner = "SameOwner",
               own_trust = "OwnTrust")
f3 <- function(x) sub("-", "$-$", sprintf("%.3f", x), fixed = TRUE)
route_macros <- ddd_route |>
  mutate(tag = route_tag[route]) |>
  reframe(macro = paste0("PRoute", tag, c("DiD", "DDD", "DDDSE", "Pct", "Pre", "Post")),
          value = c(f3(did), f3(ddd), sprintf("%.3f", ddd_se),
                    sub("-", "", sprintf("%.0f", 100 * (exp(ddd) - 1)), fixed = TRUE),
                    format(round(ca_per_year_pre), big.mark = ","),
                    format(round(ca_per_year_post), big.mark = ",")), .by = route)
writeLines(c("% Generated by 07_referee_checks.R. Do not edit.",
             paste0("\\newcommand{\\", route_macros$macro, "}{", route_macros$value, "}"),
             paste0("\\newcommand{\\PRouteAffShare}{",
                    sprintf("%.0f", 100 * ddd_route$ca_per_year_pre[ddd_route$route == "affidavit"] /
                              ddd_route$ca_per_year_pre[ddd_route$route == "all"]), "}")),
           path(tables_dir, "prop19_route_numbers.tex"))

message("Finished 07_referee_checks at ", Sys.time())
