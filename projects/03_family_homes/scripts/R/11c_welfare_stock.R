#======================================================================#
# Purpose : Stock and present-value versions of the welfare central case (simulated
#           JUE review, item A4; plan 2026-11-12_review-analyses.md, WP R3).
#           The central case of 11b_welfare_options.R is the first-year loss of one
#           annual cohort of missing transfers:
#               parcels a year x reallocation share x lambda x loss per parcel
#             = 15,834 x 0.5 x 0.5 x $1,482 = $5.9M (2023 dollars).
#           A tipped heir loses the wedge in every year the home is held, so here:
#             (a) the stock: the annual loss summed over the cohorts of missing
#                 transfers still held, from the retention curve of Fact 4 (share of
#                 family deeds not yet followed by a market sale, hazard_km_curves.csv),
#                 at mid-2024 (cohorts Mar 2021 - Jun 2024), ten years after the reform,
#                 and in the steady state;
#             (b) the present value of one cohort's loss over its holding period, at
#                 real discount rates of 3% and 5%.
#           Every other input of the central case is unchanged (population, occupancy
#           proxy, net-of-market shortfall, lambda, loss per parcel, real wedge constant).
#           The reallocation share is then disciplined by how often a family-transferred
#           home reaches the market within N years compared with a market purchase,
#           nationally (Fact 4) and in California, raw and reweighted to the family
#           state-and-cohort mix. Writes new files only; 11b and its macros are untouched.
# Name    : Saani Rawat
# Created : 11/13/2026
# Log     : 11/13/2026: created (WP R3).
# Inputs  : _outputs/tables/welfare_v2_units_quantity.csv, welfare_v2_populations.csv,
#           welfare_v2_grid.csv (11b); hazard_km_curves.csv (03, Fact 4);
#           bygroup_cf_monthly.csv (monthly shortfall); events.parquet (California and
#           reweighted retention curves, same code as 03_hazard.R)
# Outputs : _outputs/tables/r3_welfare_km_exits.csv, r3_welfare_retention.csv,
#           r3_welfare_alpha.csv, r3_welfare_stock.csv, TA_r3_welfare_stock.tex,
#           r3_macros_11c.csv; rebuilds r3_numbers.tex and r3_number_sources.csv
#======================================================================#

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

set.seed(20261113)

#============================================#
# Settings ----
#============================================#

duck_mem     <- Sys.getenv("FAM_DUCKDB_MEM_R3", "15GB")   # ~25% of the 64 GB machine
duck_threads <- 6L
spill_dir    <- "C:/Temp/wpR3_duckdb"                       # local disk, outside OneDrive
dir_create(spill_dir)

# central case of 11b (Gate 3, corrected 2026-11-12): kept unchanged
cen_pop <- "ii_main_measure"; cen_bound <- "net_of_market"; cen_alpha <- 0.5; lambda <- 0.5
range_cases <- tribble(~case, ~bound, ~alpha,
                       "low",     "beyond_spouse", 0.5,
                       "central", "net_of_market", 0.5,
                       "high",    "net_of_market", 1)

discount_rates <- c(0.03, 0.05)              # real, annual
horizon_months <- c(ten = 120L, twenty = 240L, thirty = 360L)
max_months     <- 1200L                      # 100 years: the "unlimited" horizon
tail_window    <- c(60L, 120L)               # constant hazard after month 120 = geometric mean over months 61-120
alpha_horizons <- c(two = 24L, five = 60L, ten = 120L)
# disciplined reallocation shares used in the table: market purchases reweighted to the family
# mix, ten-year horizon (the longest the data observe), California and all states
alpha_choices  <- list(disciplined_ca = list(region = "CA", weighting = "reweighted", months = 120L),
                       disciplined_us = list(region = "US", weighting = "reweighted", months = 120L))

censor_date <- as.Date("2024-06-30")         # as in 03_hazard.R
cohort_years <- 2008:2018                    # as in 03_hazard.R

fmt_num <- function(x, digits = 1) {
  out <- formatC(abs(x), digits = digits, format = "f", big.mark = ",")
  ifelse(x < 0 & round(abs(x), digits) > 0, paste0("\\ensuremath{-}", out), out)
}
read_result <- function(name) read_csv(path(tables_out_dir, name), show_col_types = FALSE)

#' Rebuild r3_numbers.tex and r3_number_sources.csv from every r3_macros_*.csv fragment
#' (same function in 27, 11c and 28; each script saves its fragment, the file is the union)
write_r3_macros <- function(macros, tag) {
  stopifnot(all(grepl("^RTHREE[A-Za-z]+$", macros$macro)), !anyDuplicated(macros$macro),
            all(nzchar(macros$value)))
  write_csv_strict(macros %>% mutate(script = tag),
                   path(tables_out_dir, paste0("r3_macros_", tag, ".csv")))
  frags <- dir_ls(tables_out_dir, regexp = "r3_macros_[0-9a-z]+\\.csv$")
  all_macros <- purrr::map_dfr(frags, ~ read_csv(.x, show_col_types = FALSE,
                                                 col_types = cols(.default = "c")))
  stopifnot(!anyDuplicated(all_macros$macro))
  writeLines(c("% Generated by 27_resale_ddd.R, 11c_welfare_stock.R, 28_fact1_parcels.R (WP R3). Do not edit.",
               paste0("\\newcommand{\\", all_macros$macro, "}{", all_macros$value, "}")),
             path(tables_out_dir, "r3_numbers.tex"))
  write_csv_strict(all_macros, path(tables_out_dir, "r3_number_sources.csv"))
  message("r3_numbers.tex: ", nrow(all_macros), " macros from ", length(frags), " fragment(s)")
}

log_file <- path(logs_dir, "11c_welfare_stock.log")
sink(log_file, split = TRUE)
message("Starting 11c_welfare_stock at ", Sys.time())

#============================================#
# Importing the central case (11b outputs) ----
#============================================#

qty  <- read_result("welfare_v2_units_quantity.csv")
pops <- read_result("welfare_v2_populations.csv")
grid <- read_result("welfare_v2_grid.csv")

dpp <- qty$deeds_per_year[qty$item == "units" & qty$name == "deeds_per_parcel_CA_main_2017_19"]
hole_months <- qty$deeds_per_year[qty$item == "units" & qty$name == "hole_months"]
loss_per_parcel <- pops$mean_loss_proxy_2023[pops$population == cen_pop]
parcels_year <- function(b) qty$parcels_per_year[qty$item == "quantity" & qty$name == b]
stopifnot(length(dpp) == 1L, length(loss_per_parcel) == 1L, hole_months == 40)

# first-year (flow) loss of one annual cohort, $M; must equal the 11b grid cells
range_cases <- range_cases %>%
  rowwise() %>%
  mutate(parcels_per_year = parcels_year(bound),
         flow_musd = parcels_per_year * alpha * lambda * loss_per_parcel / 1e6,
         grid_musd = grid$misallocation_cost_musd[grid$population == cen_pop &
                                                    grid$occupancy == "proxy_mail_eq_situs" &
                                                    grid$bound == bound & abs(grid$alpha - alpha) < 1e-9]) %>%
  ungroup()
stopifnot(all(abs(range_cases$flow_musd - range_cases$grid_musd) < 1e-9))
central_flow <- range_cases$flow_musd[range_cases$case == "central"]
message(sprintf("Central flow loss: $%.3fM (11b grid); low $%.3fM, high $%.3fM", central_flow,
                range_cases$flow_musd[1], range_cases$flow_musd[3]))

# monthly shortfall net of market sales, Mar 2021 - Jun 2024 (rebuilt as in 11b)
cfm <- read_result("bygroup_cf_monthly.csv") %>% filter(as.character(base_window) == "2019")
mn  <- cfm %>% filter(group == bygroup_main) %>% select(ym, ca_actual, cf)
mkt <- cfm %>% filter(group == "market_sale") %>% transmute(ym, mkt_factor = ca_actual / cf)
short_monthly <- inner_join(mn, mkt, by = "ym") %>%
  filter(ym >= bygroup_windows$hole_start, ym <= bygroup_windows$hole_end_main) %>%
  transmute(ym, deficit_deeds = cf * mkt_factor - ca_actual,
            age_months_jun2024 = (bygroup_windows$hole_end_main %/% 100L - ym %/% 100L) * 12L +
              (bygroup_windows$hole_end_main %% 100L - ym %% 100L))
stopifnot(nrow(short_monthly) == hole_months,
          abs(sum(short_monthly$deficit_deeds) -
                qty$deeds_total[qty$item == "quantity" & qty$name == cen_bound]) < 1)

#============================================#
# Retention curves: family deeds and market purchases (duckdb) ----
#============================================#

# Same spells as 03_hazard.R: dated events of the 2008-2018 cohorts, identical events collapsed,
# next market sale strictly after the deed, censored at 30 June 2024. Exits are kept by state and
# cohort year so the market curve can be reweighted to the family mix, and California read alone.
exits_file <- path(tables_out_dir, "r3_welfare_km_exits.csv")
if (!file_exists(exits_file) || Sys.getenv("R3_PULL", "0") == "1") {
  con <- open_corelogic_duckdb(memory_limit = duck_mem, threads = duck_threads)
  dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(spill_dir)}'"))
  ev <- events_main_from(events_panel_path)   # family measure of 2026-11-12, as 03 reads it
  create_dated_events(con, ev)
  dbExecute(con, glue("
    CREATE OR REPLACE TEMP TABLE cohort AS
    SELECT DISTINCT clip, state, sale_year, class, corp_buyer, absentee_buyer, sale_date
    FROM ev_dated
    WHERE sale_year BETWEEN {min(cohort_years)} AND {max(cohort_years)}
      AND class IN ('{paste(c(family_classes, 'market_sale'), collapse = \"','\")}')"))
  dbExecute(con, "
    CREATE OR REPLACE TEMP TABLE market_sales AS
    SELECT DISTINCT clip, sale_date AS msale_date FROM ev_dated WHERE class = 'market_sale'")
  km_exits <- dbGetQuery(con, glue("
    WITH s AS (
      SELECT c.state, c.sale_year,
             CASE WHEN c.class = 'market_sale' THEN 'market' ELSE 'family' END AS grp,
             CASE WHEN m.msale_date IS NOT NULL THEN datediff('month', c.sale_date, m.msale_date)
                  ELSE NULL END AS fail_m,
             datediff('month', c.sale_date, DATE '{censor_date}') AS censor_m
      FROM cohort c ASOF LEFT JOIN market_sales m
        ON c.clip = m.clip AND c.sale_date < m.msale_date
    ), typed AS (
      SELECT state, sale_year, grp,
        CASE WHEN fail_m IS NOT NULL AND fail_m <= censor_m THEN 'fail' ELSE 'censor' END AS exit_type,
        CASE WHEN fail_m IS NOT NULL AND fail_m <= censor_m
             THEN GREATEST(fail_m, 1) ELSE GREATEST(censor_m, 1) END AS exit_m
      FROM s WHERE censor_m >= 1
    )
    SELECT state, sale_year, grp, exit_type, exit_m, COUNT(*) AS n
    FROM typed GROUP BY ALL")) %>% as_tibble()
  dbDisconnect(con, shutdown = TRUE)
  write_csv_strict(km_exits, exits_file)
}
km_exits <- read_csv(exits_file, show_col_types = FALSE) %>%
  mutate(n = as.numeric(n), exit_m = as.integer(exit_m))
stopifnot(nrow(km_exits) > 1e4)
message("Spells in the exit table: ", format(sum(km_exits$n), big.mark = ","))

#' Kaplan-Meier from an exit table (03_hazard.R formulation)
#' @param ex Tibble with exit_type ('fail' / 'censor'), exit_m, n.
#' @param max_m Last month kept.
#' @return Tibble: t, d_t, r_t, h_t, surv.
km_curve <- function(ex, max_m = 120L) {
  ex %>%
    group_by(exit_type, exit_m) %>% summarise(n = sum(n), .groups = "drop") %>%
    pivot_wider(names_from = exit_type, values_from = n, values_fill = 0) %>%
    { if (!"censor" %in% names(.)) mutate(., censor = 0) else . } %>%
    { if (!"fail" %in% names(.)) mutate(., fail = 0) else . } %>%
    complete(exit_m = 1:max(exit_m), fill = list(fail = 0, censor = 0)) %>%
    arrange(exit_m) %>%
    mutate(n_total = sum(fail) + sum(censor),
           r_t = n_total - lag(cumsum(fail), default = 0) - lag(cumsum(censor), default = 0),
           h_t = if_else(r_t > 0, fail / r_t, 0),
           surv = cumprod(1 - h_t)) %>%
    filter(exit_m <= max_m) %>%
    transmute(t = exit_m, d_t = fail, r_t, h_t, surv)
}

#' Market curve reweighted to the family mix: the monthly hazard is averaged over cells
#' (state x cohort year) with the family deeds of each cell as weights, over the cells still
#' at risk in that month, then cumulated.
km_reweighted <- function(ex, cell_vars) {
  w <- ex %>% filter(grp == "family") %>% group_by(across(all_of(cell_vars))) %>%
    summarise(w = sum(n), .groups = "drop")
  cells_km <- ex %>% filter(grp == "market") %>%
    group_by(across(all_of(cell_vars))) %>%
    group_modify(~ km_curve(.x)) %>% ungroup() %>%
    inner_join(w, by = cell_vars)
  cells_km %>%
    filter(r_t > 0) %>%
    group_by(t) %>%
    summarise(h_t = sum(w * h_t) / sum(w), w_share = sum(w) / sum(.env$w$w), .groups = "drop") %>%
    arrange(t) %>%
    mutate(surv = cumprod(1 - h_t))
}

# national curves (Fact 4) rebuilt from the exit table, and checked against hazard_km_curves.csv
fact4 <- read_result("hazard_km_curves.csv")
nat_fam <- km_curve(km_exits %>% filter(grp == "family"))
nat_mkt <- km_curve(km_exits %>% filter(grp == "market"))
chk <- bind_rows(
  inner_join(nat_fam, fact4 %>% filter(class == "family_all") %>% select(t, surv_f4 = surv), by = "t"),
  inner_join(nat_mkt, fact4 %>% filter(class == "market_sale") %>% select(t, surv_f4 = surv), by = "t"))
message(sprintf("Fact 4 curves rebuilt: %d months, max gap %.2e", nrow(chk), max(abs(chk$surv - chk$surv_f4))))
stopifnot(nrow(chk) == 240L, max(abs(chk$surv - chk$surv_f4)) < 1e-9)

ca_ex <- km_exits %>% filter(state == "CA")
curves <- bind_rows(
  nat_fam %>% transmute(region = "US", curve = "family", weighting = "raw", t, h_t, surv),
  nat_mkt %>% transmute(region = "US", curve = "market", weighting = "raw", t, h_t, surv),
  km_reweighted(km_exits, c("state", "sale_year")) %>%
    transmute(region = "US", curve = "market", weighting = "reweighted", t, h_t, surv),
  km_curve(ca_ex %>% filter(grp == "family")) %>%
    transmute(region = "CA", curve = "family", weighting = "raw", t, h_t, surv),
  km_curve(ca_ex %>% filter(grp == "market")) %>%
    transmute(region = "CA", curve = "market", weighting = "raw", t, h_t, surv),
  km_reweighted(ca_ex, "sale_year") %>%
    transmute(region = "CA", curve = "market", weighting = "reweighted", t, h_t, surv)) %>%
  filter(t <= 120L)
stopifnot(all(count(curves, region, curve, weighting)$n == 120L))
write_csv_strict(curves, path(tables_out_dir, "r3_welfare_retention.csv"))

#============================================#
# Disciplining the reallocation share ----
#============================================#

# Share of family deeds and of market purchases followed by a market sale within N months
# (1 - Kaplan-Meier survival). Disciplined share: the part of market-purchase turnover that
# family holders forgo, alpha_N = max(0, 1 - F_family(N) / F_market(N)); the absolute gap
# F_market(N) - F_family(N) is the share of all family-held homes kept off the market.
surv_at <- function(r, cv, wt, m) curves$surv[curves$region == r & curves$curve == cv &
                                                curves$weighting == wt & curves$t == m]
alpha_tab <- crossing(region = c("US", "CA"), weighting = c("raw", "reweighted"),
                      months = unname(alpha_horizons)) %>%
  rowwise() %>%
  mutate(fam_sold = 1 - surv_at(region, "family", "raw", months),
         mkt_sold = 1 - surv_at(region, "market", weighting, months)) %>%
  ungroup() %>%
  mutate(ratio_fam_to_mkt = fam_sold / mkt_sold,
         alpha_relative = pmax(0, 1 - ratio_fam_to_mkt),
         alpha_absolute = pmax(0, mkt_sold - fam_sold))
print(as.data.frame(alpha_tab), digits = 4)
write_csv_strict(alpha_tab, path(tables_out_dir, "r3_welfare_alpha.csv"))
alpha_disc <- vapply(alpha_choices, function(a) {
  v <- alpha_tab$alpha_relative[alpha_tab$region == a$region & alpha_tab$weighting == a$weighting &
                                  alpha_tab$months == a$months]
  stopifnot(length(v) == 1L, v >= 0, v <= 1); v
}, numeric(1))
message("Disciplined reallocation shares (reweighted market, ten years): ",
        paste(sprintf("%s %.3f", names(alpha_disc), alpha_disc), collapse = "; "))

#============================================#
# Holding-period multiples ----
#============================================#

#' Survival by month 0..max_months: Kaplan-Meier to month 120, then a constant monthly
#' hazard equal to the geometric mean of the hazard over months 61-120.
extend_survival <- function(s120, max_m = max_months) {
  stopifnot(length(s120) == 120L)
  h_tail <- 1 - (s120[tail_window[2]] / s120[tail_window[1]])^(1 / diff(tail_window))
  s <- c(1, s120, s120[120] * (1 - h_tail)^seq_len(max_m - 120L))
  list(surv = s, h_tail = h_tail)        # s[k + 1] = survival at month k
}

#' Years held per missing transfer within a horizon (undiscounted or discounted),
#' trapezoid over months.
years_held <- function(s, months, rate = 0) {
  k <- 0:(months - 1L)
  disc <- (1 + rate)^(-(k + 0.5) / 12)
  sum((s[k + 1L] + s[k + 2L]) / 2 * disc) / 12
}

retention_sets <- list(
  US = curves %>% filter(region == "US", curve == "family", weighting == "raw") %>% arrange(t) %>% pull(surv),
  CA = curves %>% filter(region == "CA", curve == "family", weighting == "raw") %>% arrange(t) %>% pull(surv))

multiples <- purrr::imap_dfr(retention_sets, function(s120, reg) {
  ext <- extend_survival(s120)
  s <- ext$surv
  # parcels still held at the end of June 2024, per parcel of the 40-month shortfall
  held_jun2024 <- sum(short_monthly$deficit_deeds * s[short_monthly$age_months_jun2024 + 1L]) /
    sum(short_monthly$deficit_deeds)
  tibble(retention = reg,
         surv_120 = s[121], h_tail_monthly = ext$h_tail,
         h_tail_annual = 1 - (1 - ext$h_tail)^12,
         share_unsold_100y = s[max_months + 1L],
         # stock of a constant annual flow, in years of flow: cohorts 0..H months old
         years_ten = years_held(s, horizon_months[["ten"]]),
         years_twenty = years_held(s, horizon_months[["twenty"]]),
         years_thirty = years_held(s, horizon_months[["thirty"]]),
         years_steady = years_held(s, max_months),
         # present value of one cohort, years of first-year loss
         pv_years_3 = years_held(s, max_months, discount_rates[1]),
         pv_years_5 = years_held(s, max_months, discount_rates[2]),
         pv_years_3_ten = years_held(s, horizon_months[["ten"]], discount_rates[1]),
         pv_years_5_ten = years_held(s, horizon_months[["ten"]], discount_rates[2]),
         held_share_jun2024 = held_jun2024)
})
print(as.data.frame(multiples), digits = 4)

#============================================#
# Stock and present-value losses ----
#============================================#

# stock at mid-2024: the missing transfers of Mar 2021 - Jun 2024 still held, in parcels
stock_parcels_jun2024 <- function(held_share) sum(short_monthly$deficit_deeds) / dpp * held_share

loss_rows <- crossing(range_cases %>% select(case, bound, alpha_central = alpha, parcels_per_year),
                      retention = names(retention_sets),
                      alpha_case = c("as_central_case", names(alpha_disc))) %>%
  left_join(multiples, by = "retention") %>%
  mutate(alpha = if_else(alpha_case %in% names(alpha_disc), alpha_disc[alpha_case], alpha_central),
         loss_parcel_year = alpha * lambda * loss_per_parcel,     # $ a year per missing parcel
         flow_musd = parcels_per_year * loss_parcel_year / 1e6,
         stock_jun2024_musd = if_else(bound == cen_bound,
                                      stock_parcels_jun2024(held_share_jun2024) * loss_parcel_year / 1e6,
                                      NA_real_),
         stock_ten_musd = flow_musd * years_ten,
         stock_twenty_musd = flow_musd * years_twenty,
         stock_thirty_musd = flow_musd * years_thirty,
         stock_steady_musd = flow_musd * years_steady,
         pv3_musd = flow_musd * pv_years_3,
         pv5_musd = flow_musd * pv_years_5,
         pv3_ten_musd = flow_musd * pv_years_3_ten,
         pv5_ten_musd = flow_musd * pv_years_5_ten) %>%
  filter(!(alpha_case != "as_central_case" & case == "high")) %>%    # share 1 has no disciplined twin
  arrange(retention, alpha_case, factor(case, c("low", "central", "high")))
stopifnot(all(is.finite(loss_rows$stock_steady_musd)), all(loss_rows$flow_musd >= 0))
write_csv_strict(loss_rows, path(tables_out_dir, "r3_welfare_stock.csv"))
print(as.data.frame(loss_rows %>% select(retention, alpha_case, case, alpha, flow_musd, stock_jun2024_musd,
                                         stock_ten_musd, stock_steady_musd, pv3_musd, pv5_musd)), digits = 3)

#============================================#
# Small table ----
#============================================#

row_of <- function(ret, ac, cs) {
  r <- loss_rows %>% filter(retention == ret, alpha_case == ac, case == cs)
  stopifnot(nrow(r) == 1L); r
}
dm <- function(x) if (is.na(x)) "--" else formatC(x, digits = 1, format = "f", big.mark = ",")
tab_line <- function(label, r) {
  sprintf("%s & %s & %s & %s & %s & %s & %s & %s \\\\", label, formatC(r$alpha, digits = 2, format = "f"),
          dm(r$flow_musd), dm(r$stock_jun2024_musd), dm(r$stock_ten_musd), dm(r$stock_steady_musd),
          dm(r$pv3_musd), dm(r$pv5_musd))
}
m_us <- multiples %>% filter(retention == "US"); m_ca <- multiples %>% filter(retention == "CA")
ta <- c(
  "\\begin{table}[!ht]\\centering",
  "\\caption{The welfare central case as a flow, a stock and a present value (\\$ millions, 2023 dollars)}\\label{tab:r3_welfare_stock}",
  "{\\small\\setlength{\\tabcolsep}{4pt}",
  "\\begin{tabular}{lrrrrrrr}\\toprule",
  " & Share & First-year & \\multicolumn{3}{c}{Annual loss of the stock held} & \\multicolumn{2}{c}{PV of one cohort} \\\\",
  "\\cmidrule(lr){4-6}\\cmidrule(lr){7-8}",
  "Retention curve and case & $\\rho$ & loss & mid-2024 & year 10 & steady state & 3\\% & 5\\% \\\\\\midrule",
  "\\multicolumn{8}{l}{\\emph{Fact 4 retention, all states}} \\\\",
  tab_line("\\quad Central case", row_of("US", "as_central_case", "central")),
  tab_line("\\quad Low end (net of spouse deeds)", row_of("US", "as_central_case", "low")),
  tab_line("\\quad High end (share 1)", row_of("US", "as_central_case", "high")),
  tab_line("\\quad Central, share disciplined on all states", row_of("US", "disciplined_us", "central")),
  tab_line("\\quad Central, share disciplined on California", row_of("US", "disciplined_ca", "central")),
  "\\addlinespace\\multicolumn{8}{l}{\\emph{California retention}} \\\\",
  tab_line("\\quad Central case", row_of("CA", "as_central_case", "central")),
  tab_line("\\quad Central, share disciplined on all states", row_of("CA", "disciplined_us", "central")),
  "\\bottomrule\\end{tabular}}",
  paste0("\\begin{minipage}{0.96\\textwidth}\\vspace{3pt}{\\scriptsize\\textit{Notes:} ",
         "First-year loss: Table~\\ref{tab:welfare_grid}'s cell, parcels a year $\\times\\ \\rho\\ \\times\\ \\kappa\\ \\times$ the mean annual wedge the heir loses ",
         "(\\$", formatC(loss_per_parcel, digits = 0, format = "f", big.mark = ","), "), with every input unchanged. ",
         "A tipped heir loses the wedge each year the home is held, constant in real terms. ",
         "Holding follows the share of family deeds not yet followed by a market sale (Kaplan--Meier, 2008--2018 deeds, Fact 4 or California), ",
         "extended past ten years at the average monthly hazard of years six to ten. ",
         "Mid-2024: the missing transfers of March 2021 to June 2024, month by month, still held in June 2024. ",
         "Year 10 and steady state: the annual loss when ten years of cohorts, or all cohorts, are held; ",
         "a constant annual flow of missing transfers gives ", formatC(m_us$years_ten, digits = 1, format = "f"), " and ",
         formatC(m_us$years_steady, digits = 1, format = "f"), " years of first-year loss on the Fact 4 curve (",
         formatC(m_ca$years_ten, digits = 1, format = "f"), " and ", formatC(m_ca$years_steady, digits = 1, format = "f"),
         " on the California curve). PV: one cohort's loss over its holding period at a real rate of 3\\% or 5\\%. ",
         "Disciplined share: one minus the ratio of the share of family deeds followed by a market sale within ten years to the same share for ",
         "market purchases reweighted to the family state and cohort mix, floored at zero: ",
         formatC(alpha_disc[["disciplined_us"]], digits = 2, format = "f"), " on all states and ",
         formatC(alpha_disc[["disciplined_ca"]], digits = 2, format = "f"),
         " in California, where family deeds reach the market more often than market purchases at two, five and ten years. ",
         "Rows are scenarios, not confidence intervals.}\\end{minipage}\\end{table}"))
writeLines(ta, path(tables_out_dir, "TA_r3_welfare_stock.tex"))

#============================================#
# Macros ----
#============================================#

S_s <- "r3_welfare_stock.csv"; S_a <- "r3_welfare_alpha.csv"; S_r <- "r3_welfare_retention.csv"
cen <- row_of("US", "as_central_case", "central"); cen_ca <- row_of("CA", "as_central_case", "central")
dis <- row_of("US", "disciplined_us", "central");  dis_ca <- row_of("CA", "disciplined_us", "central")
low <- row_of("US", "as_central_case", "low");     high <- row_of("US", "as_central_case", "high")
a_get <- function(r, w, m, col) {
  v <- alpha_tab[[col]][alpha_tab$region == r & alpha_tab$weighting == w & alpha_tab$months == m]
  stopifnot(length(v) == 1L); v
}
macros <- tribble(
  ~macro, ~value, ~source, ~definition,
  "RTHREEWelfareFlow", fmt_num(cen$flow_musd), S_s, "first-year loss of one annual cohort, central case ($M; equals WViLossCentral)",
  "RTHREEWelfareStockMidTwentyFour", fmt_num(cen$stock_jun2024_musd), S_s,
    "annual loss of the missing transfers of Mar 2021 - Jun 2024 still held in Jun 2024, Fact 4 retention ($M)",
  "RTHREEWelfareStockTen", fmt_num(cen$stock_ten_musd), S_s, "annual loss when ten years of cohorts are held, Fact 4 retention ($M)",
  "RTHREEWelfareStockSteady", fmt_num(cen$stock_steady_musd), S_s, "steady-state annual loss of the stock, Fact 4 retention ($M)",
  "RTHREEWelfareStockSteadyLow", fmt_num(low$stock_steady_musd), S_s, "same, low end (net of spouse deeds, share 0.5)",
  "RTHREEWelfareStockSteadyHigh", fmt_num(high$stock_steady_musd), S_s, "same, high end (share 1)",
  "RTHREEWelfarePVThree", fmt_num(cen$pv3_musd), S_s, "PV of one cohort's loss over its holding period, 3% real ($M)",
  "RTHREEWelfarePVFive", fmt_num(cen$pv5_musd), S_s, "same at 5% real ($M)",
  "RTHREEWelfarePVThreeTen", fmt_num(cen$pv3_ten_musd), S_s, "PV at 3%, first ten years only ($M)",
  "RTHREEWelfareStockSteadyCA", fmt_num(cen_ca$stock_steady_musd), S_s, "steady-state annual loss, California retention ($M)",
  "RTHREEWelfarePVThreeCA", fmt_num(cen_ca$pv3_musd), S_s, "PV at 3%, California retention ($M)",
  "RTHREEWelfarePVFiveCA", fmt_num(cen_ca$pv5_musd), S_s, "PV at 5%, California retention ($M)",
  "RTHREEHoldYearsTen", fmt_num(cen$years_ten), S_s, "years of first-year loss held within ten years, Fact 4 curve",
  "RTHREEHoldYearsSteady", fmt_num(cen$years_steady), S_s, "expected years held (steady-state multiple), Fact 4 curve",
  "RTHREEHoldYearsSteadyCA", fmt_num(cen_ca$years_steady), S_s, "expected years held, California curve",
  "RTHREETailHazard", fmt_num(100 * cen$h_tail_annual), S_s, "annual market-sale hazard used after year ten, Fact 4 curve (percent)",
  "RTHREEFamUnsoldTen", fmt_num(100 * cen$surv_120), S_r, "family deeds not followed by a market sale within ten years, Fact 4 (percent)",
  "RTHREEAlphaDiscCA", fmt_num(alpha_disc[["disciplined_ca"]], 2), S_a,
    "disciplined reallocation share: CA, market reweighted to the family cohort mix, ten years (floored at 0)",
  "RTHREEAlphaDiscUS", fmt_num(alpha_disc[["disciplined_us"]], 2), S_a,
    "same, all states, market reweighted to the family state-and-cohort mix, ten years",
  "RTHREEAlphaRawTenUS", fmt_num(a_get("US", "raw", 120L, "alpha_relative"), 2), S_a, "same, all states, unweighted (Fact 4 curves)",
  "RTHREEAlphaAbsTenUS", fmt_num(a_get("US", "reweighted", 120L, "alpha_absolute"), 3), S_a,
    "absolute gap: reweighted market minus family share sold within ten years, all states",
  "RTHREEAlphaFiveCA", fmt_num(a_get("CA", "reweighted", 60L, "alpha_relative"), 2), S_a, "same, CA, reweighted, five years",
  "RTHREEAlphaTwoCA", fmt_num(a_get("CA", "reweighted", 24L, "alpha_relative"), 2), S_a, "same, CA, reweighted, two years",
  "RTHREEFamSoldTenCA", fmt_num(100 * a_get("CA", "reweighted", 120L, "fam_sold"), 1), S_a, "CA family deeds followed by a market sale within ten years (percent)",
  "RTHREEMktSoldTenCA", fmt_num(100 * a_get("CA", "reweighted", 120L, "mkt_sold"), 1), S_a, "CA market purchases, reweighted, sold within ten years (percent)",
  "RTHREEFamSoldTenUS", fmt_num(100 * a_get("US", "reweighted", 120L, "fam_sold"), 1), S_a, "all-state family deeds sold within ten years (percent)",
  "RTHREEMktSoldTenUS", fmt_num(100 * a_get("US", "reweighted", 120L, "mkt_sold"), 1), S_a, "all-state market purchases, reweighted, sold within ten years (percent)",
  "RTHREEWelfareFlowDisc", fmt_num(dis$flow_musd, 2), S_s, "first-year loss at the all-state disciplined share ($M; zero at the CA share)",
  "RTHREEWelfareStockTenDisc", fmt_num(dis$stock_ten_musd), S_s, "annual loss of ten years of cohorts at the all-state disciplined share, Fact 4 retention ($M)",
  "RTHREEWelfareStockSteadyDisc", fmt_num(dis$stock_steady_musd), S_s, "steady-state annual loss at the all-state disciplined share, Fact 4 retention ($M)",
  "RTHREEWelfarePVThreeDisc", fmt_num(dis$pv3_musd), S_s, "PV at 3% at the all-state disciplined share ($M)",
  "RTHREEWelfarePVFiveDisc", fmt_num(dis$pv5_musd), S_s, "PV at 5% at the all-state disciplined share ($M)",
  "RTHREEWelfareStockSteadyDiscCA", fmt_num(dis_ca$stock_steady_musd), S_s, "steady-state annual loss at the all-state disciplined share, CA retention ($M)"
)
write_r3_macros(macros, "11c")

message("Finished 11c_welfare_stock at ", Sys.time())
sink()
