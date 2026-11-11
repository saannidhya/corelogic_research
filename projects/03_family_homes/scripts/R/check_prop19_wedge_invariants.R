#======================================================================#
# Purpose : Synthetic-data invariants for the Prop 19 wedge design. Tests
#           the functions as written in 04a_build_wedge.R and 04_prop19.R
#           (extracted by name from the parsed scripts, so no pipeline runs):
#           (1) Prop 13 roll-forward matches hand-multiplied BOE factors;
#           (2) first-exit risk-set accounting (at-risk falls by exits);
#           (3) other interfamily exits count as family ownership exits.
#           The retired dose-response estimator is outside the current pipeline.
# Name    : Saani Rawat
# Created : 10/02/2026
# Log     : 10/02/2026: created.
#           11/04/2026: verify retained accounting and other-interfamily exits.
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
                   "ym_sequence", "build_wedge_cells", "add_treatment_terms"), defs)

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
# (3) Other interfamily exits use the family channel ----
#============================================#

# The dose-response estimator and implied_change helper were retired before
# this revision. Test the retained production risk-set function directly.
other_parcels <- toy_parcels
other_parcels$end_type[1] <- "family_other"
other_cells <- with(defs, build_wedge_cells(other_parcels))
check(sum(other_cells$fam) == 1 && sum(other_cells$family_other) == 1,
      "other interfamily contributes once to family exits")
check(identical(other_cells$at_risk, toy_cells$at_risk),
      "same-surname and other-interfamily exits remove parcels at the same date")
check(sum(other_cells$own_exits) == sum(toy_cells$own_exits),
      "widening the family class preserves ownership-exit accounting")

message("All retained Prop 19 wedge accounting invariants passed.")
