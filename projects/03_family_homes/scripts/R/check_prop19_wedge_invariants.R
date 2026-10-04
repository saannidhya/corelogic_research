#======================================================================#
# Purpose : Synthetic-data invariants for the Prop 19 wedge design. Tests
#           the functions as written in 04a_build_wedge.R and 04_prop19.R
#           (extracted by name from the parsed scripts, so no pipeline runs):
#           (1) Prop 13 roll-forward matches hand-multiplied BOE factors;
#           (2) first-exit risk-set accounting (at-risk falls by exits);
#           (3) Poisson hazard with log at-risk offset recovers a known
#               wedge x period semi-elasticity;
#           (4) implied change equals the known counterfactual difference
#               and the competing-risk shares sum to one.
# Name    : Saani Rawat
# Created : 10/02/2026
# Log     : 10/02/2026: created.
# Inputs  : scripts/R/04a_build_wedge.R, scripts/R/04_prop19.R,
#           scripts/R/inputs/ca_boe_inflation_factors.csv
# Outputs : console PASS/FAIL lines; stops on the first failure
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

suppressPackageStartupMessages({
  library(fixest)
})

set.seed(20261002)

#============================================#
# Extracting definitions from the scripts ----
#============================================#

# Evaluate only the named top-level assignments, so nothing heavy runs.
load_definitions <- function(file, names_wanted, env) {
  exprs <- parse(file)
  for (e in exprs) {
    if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
        as.character(e[[2]]) %in% names_wanted) {
      eval(e, envir = env)
    }
  }
  missing <- setdiff(names_wanted, ls(env))
  if (length(missing)) stop("Not found in ", basename(file), ": ", paste(missing, collapse = ", "))
  invisible(env)
}

defs <- new.env(parent = globalenv())
load_definitions(path(r_dir, "04a_build_wedge.R"),
                 c("boe_factors", "factor_index", "cum_at", "roll_forward"), defs)
load_definitions(path(r_dir, "04_prop19.R"),
                 c("antic_start_ym", "antic_end_ym", "post21_end_ym", "steady_start_ym",
                   "censor_ym", "fam_classes", "exit_types", "groups_all", "to_ym",
                   "ym_sequence", "build_wedge_cells", "add_treatment_terms",
                   "implied_change"), defs)

check <- function(ok, label) {
  if (!isTRUE(ok)) stop("FAIL: ", label)
  message("PASS: ", label)
}

#============================================#
# (1) Prop 13 roll-forward ----
#============================================#

f <- defs$boe_factors
by_hand <- 100000 * prod(f$factor[f$roll_year %in% 2009:2017])
check(isTRUE(all.equal(defs$roll_forward(100000, 2008, 2017), by_hand)),
      "roll-forward 2008 -> 2017 equals product of 2009..2017 factors")
check(isTRUE(all.equal(defs$roll_forward(500000, 2016, 2017), 500000 * 1.02)),
      "a 2016 sale is factored once by the 2017-18 factor (1.02)")
check(isTRUE(all.equal(defs$roll_forward(1, 2010, 2011), 1.00753)),
      "2011-12 factor is 1.00753 (BOE LTA 2023/056)")
check(all(f$factor <= 1.02), "no factor exceeds the 2% cap")

#============================================#
# (2) Risk-set accounting ----
#============================================#

# three parcels in one cell: a family exit in 2018-03, a market exit in
# 2022-06, one censored; plus one trust deed and one other deed while at risk
# (04a already drops first-deed dates that fall after the spell ends)
toy_parcels <- tibble(
  clip = 1:3, cohort = "d2017", fips5 = "06001", group = "nonhox", tenure = "long_held",
  wedge_bin = "w3_5k", wedge = c(4000, 4000, 4000),
  end_type = c("family_person", "market_other", "censored"),
  spell_end = as.Date(c("2018-03-15", "2022-06-01", "2024-06-30")),
  trust_first_date = as.Date(c(NA, "2020-01-10", NA)),
  other_first_date = as.Date(c(NA, NA, "2019-05-05"))
)
toy_cells <- with(defs, build_wedge_cells(toy_parcels))
risk_at <- function(ym) toy_cells$at_risk[toy_cells$ym == ym]
check(risk_at(201701L) == 3 && risk_at(201803L) == 3 && risk_at(201804L) == 2,
      "at-risk count drops the month after a family exit")
check(risk_at(202206L) == 2 && risk_at(202207L) == 1 && risk_at(202406L) == 1,
      "at-risk count drops after the market exit; censored parcel stays to 2024-06")
check(sum(toy_cells$fam) == 1 && sum(toy_cells$market_other) == 1 &&
        sum(toy_cells$trust_first) == 1 && sum(toy_cells$other_first) == 1,
      "each exit and first deed is counted exactly once")
check(all(abs(toy_cells$wbar - 4) < 1e-12), "wbar is the cell mean wedge in $ thousand")

#============================================#
# (3) Poisson offset recovers a known hazard ----
#============================================#

# simulate cells: hazard = h0 * county-month shock * exp(beta * wbar * steady)
beta_true <- -0.06
sim_cells <- expand_grid(fips5 = sprintf("06%03d", 1:30), bin = paste0("b", 1:6),
                         ym = defs$ym_sequence(201701L, 202406L)) %>%
  mutate(group = "nonhox", tenure = "long_held", hox = 0L,
         cell_id = as.integer(factor(paste(fips5, bin))),
         wbar = as.numeric(sub("b", "", bin)) * 1.5,
         at_risk = 20000,
         antic = as.integer(between(ym, defs$antic_start_ym, defs$antic_end_ym)),
         post21 = as.integer(ym > defs$antic_end_ym & ym <= defs$post21_end_ym),
         steady = as.integer(ym >= defs$steady_start_ym)) %>%
  group_by(fips5, ym) %>%
  mutate(shock = exp(rnorm(1, 0, 0.1))) %>%
  ungroup() %>%
  mutate(mu = at_risk * 0.002 * exp(0.02 * wbar) * shock * exp(beta_true * wbar * steady),
         fam = rpois(n(), mu)) %>%
  defs$add_treatment_terms()

m_sim <- fepois(fam ~ w_nonhox_antic + w_nonhox_post21 + w_nonhox_steady |
                  cell_id + fips5^ym, data = sim_cells, offset = ~log(at_risk))
b_hat <- coef(m_sim)["w_nonhox_steady"]
check(abs(b_hat - beta_true) < 3 * se(m_sim)["w_nonhox_steady"] && abs(b_hat - beta_true) < 0.01,
      sprintf("offset Poisson recovers beta (true %.3f, est %.4f)", beta_true, b_hat))

#============================================#
# (4) Implied change and share identity ----
#============================================#

# implied change uses the fitted model; compare with the analytic change at
# the estimated coefficient
defs$treatment_terms <- grep("^w_", names(sim_cells), value = TRUE)
delta_fn <- with(defs, implied_change(m_sim, sim_cells))
used <- sim_cells[obs(m_sim), ]
mu_hat <- fitted(m_sim)
delta_by_hand <- sum((mu_hat - mu_hat * exp(-b_hat * used$w_nonhox_steady))[used$steady == 1])
check(isTRUE(all.equal(delta_fn, delta_by_hand)),
      "implied_change equals fitted minus no-treatment counterfactual")
check(delta_fn < 0, "a negative beta implies fewer family transfers")

# shares: market + trust + other + no-deed = 1 by construction
deltas <- c(fam = -100, mkt_e = 12, mkt_o = 5, trust = 20, other = 3)
shares <- c(deltas[2:5] / -deltas[["fam"]], no_deed = 1 - sum(deltas[2:5]) / -deltas[["fam"]])
check(isTRUE(all.equal(sum(shares), 1)), "competing-risk shares sum to one")

message("All Prop 19 wedge invariants passed.")
