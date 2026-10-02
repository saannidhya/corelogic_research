# ============================================================
# 11: Conditional allocation gains from the inheritance reform
# Inputs: saved DiD/SCM outputs, derived events, CA prop via loader
# Outputs: welfare_*.csv, generated TeX tables/macros, F8_welfare_stock
# Note: scenario calibration, not identified welfare or a confidence bound.
# ============================================================
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir, "11_welfare_helpers.R"))
set.seed(20260921)
con <- open_corelogic_duckdb(threads = 4L)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(path(data_dir, 'duckdb_tmp'))}'"))
ev <- glue("read_parquet('{sql_quote_path(path(data_dir, 'events.parquet'))}')")
fam_classes <- c("family_person", "family_other", "family_estate")
save_csv <- function(x, name) write_csv_strict(x, path(tables_out_dir, paste0("welfare_", name, ".csv")))
read_result <- function(name) read_csv(path(tables_out_dir, name), show_col_types = FALSE)
fmt <- function(x, digits = 0) formatC(x, digits = digits, format = "f", big.mark = ",")

# ---- 1. Reference flow: baseline-scaled net DiD -------------------------
did <- read_result("prop19_did_volume.csv")
perm <- read_result("ref_perm_volume.csv")
scm <- read_result("prop19_scm_inference.csv")
scm_pc <- read_result("prop19_scm_pc_inference.csv")
baseline <- dbGetQuery(con, glue("SELECT sale_year, COUNT(*) AS n FROM {ev}
  WHERE state='CA' AND sale_year IN (2018,2019)
  AND class IN ('family_person','family_other','family_estate') GROUP BY sale_year"))
stopifnot(nrow(baseline) == 2)
base_flow <- mean(baseline$n)
beta_net <- did$coef[did$model == "fam"] - did$coef[did$model == "market_placebo"]
flow <- base_flow * (1 - exp(beta_net))
save_csv(tibble(specification = c("net_DiD_baseline_scaled", "SCM_raw", "SCM_per_65plus"),
  decline_pct = c(100 * (1 - exp(beta_net)), -scm$gap_ss_pct, -scm_pc$gap_ss_pct),
  baseline = c(base_flow, NA, NA), annual_deeds = c(flow, NA, NA),
  placebo_p = c(perm$perm_p[perm$outcome == "net"], scm$scm_mspe_p, scm_pc$scm_mspe_p)), "effect_inputs")

# ---- 2. California calendar dates and non-overlapping family episodes --
create_dated_events(con, ev)
date_audit <- dbGetQuery(con, "SELECT state, COUNT(*) AS n,
  SUM(CASE WHEN sale_date IS NULL THEN 1 ELSE 0 END) AS invalid_dates,
  SUM(CASE WHEN sale_raw % 100 = 0 THEN 1 ELSE 0 END) AS imputed_day,
  SUM(CASE WHEN CAST((sale_raw / 100) % 100 AS INTEGER) <> sale_month THEN 1 ELSE 0 END) AS old_month_disagrees
  FROM ev_date_audit WHERE state='CA' GROUP BY state")
save_csv(date_audit, "date_audit")
dbExecute(con, "CREATE TEMP TABLE ca_events AS
  SELECT clip, class, sale_date FROM (
    SELECT clip, class, sale_date, ROW_NUMBER() OVER (PARTITION BY clip, sale_date
      ORDER BY CASE WHEN class='market_sale' THEN 0 ELSE 1 END, sale_raw, class) AS rn
    FROM ev_dated WHERE state='CA' AND sale_date <= DATE '2024-06-30'
      AND class IN ('market_sale','family_person','family_other','family_estate')
  ) WHERE rn=1")
collisions <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM (
  SELECT clip,sale_date FROM ev_dated WHERE state='CA' AND sale_date <= DATE '2024-06-30'
    AND class IN ('market_sale','family_person','family_other','family_estate')
  GROUP BY clip,sale_date HAVING COUNT(*)>1)")
save_csv(collisions, "parsed_date_collisions")
all_surv <- list(); episode_stats <- list()
for (group in c("broad", "same_surname")) {
  classes <- if (group == "broad") fam_classes else "family_person"
  dbExecute(con, glue("CREATE OR REPLACE TEMP TABLE episodes AS {family_episode_sql(classes)}"))
  stats <- dbGetQuery(con, "SELECT COUNT(*) AS n_episodes, COUNT(DISTINCT clip) AS n_parcels,
    SUM(n_family_deeds) AS n_deeds, MIN(start_date) AS first_start, MAX(start_date) AS last_start FROM episodes")
  stopifnot(stats$n_episodes > 10000, stats$n_deeds >= stats$n_episodes)
  overlaps <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM (
    SELECT *, LAG(next_market) OVER (PARTITION BY clip ORDER BY start_date) AS prior_end
    FROM episodes) WHERE start_date <= prior_end")$n
  stopifnot(overlaps == 0)
  episode_stats[[group]] <- mutate(stats, group = group, overlapping_episodes = overlaps)
  exits <- dbGetQuery(con, "SELECT
    datediff('day',start_date,coalesce(next_market,DATE '2024-06-30')) AS days,
    COUNT(*) AS n, SUM(CASE WHEN next_market IS NOT NULL THEN 1 ELSE 0 END) AS fail
    FROM episodes GROUP BY days ORDER BY days")
  km <- km_from_exits(exits)
  all_surv[[group]] <- tibble(group = group, month = 0:120,
                            survival = survival_at(km, (0:120)/12))
  save_csv(mutate(km, group = group), paste0("km_exits_", group))
}
surv <- bind_rows(all_surv)
save_csv(surv, "ca_survival")
save_csv(bind_rows(episode_stats), "episode_audit")

# ---- 3. CA characteristics: audit only, never infer a historical wedge --
sample_prop <- load_corelogic_prop(sample = TRUE)
value_fields <- grep("value|tax|assess|year|date|avm", names(sample_prop), value = TRUE)
save_csv(tibble(field = value_fields), "value_field_inventory")
needed <- unique(c("clip", "assessed_total_value", "market_total_value",
  "calculated_total_value", "total_tax_amount", "total_property_tax_rate_percent",
  "tax_year", "assessed_year", "calculated_total_value_source_code",
  "last_assessor_update_date", "taxroll_certification_date"))
needed <- intersect(needed, names(sample_prop))
message("Loading CA property columns via shared loader")
prop <- load_corelogic_prop(states = "CA", columns = needed, per_partition = TRUE)
audit_values <- lapply(setdiff(needed, "clip"), function(nm) {
  observed <- !is.na(prop[[nm]]) & trimws(as.character(prop[[nm]])) != ""
  x <- suppressWarnings(as.numeric(prop[[nm]]))
  good <- x[is.finite(x) & x > 0]
  tibble(field = nm, n_rows = length(x), share_nonmissing = mean(observed),
    share_numeric = mean(is.finite(x)),
    share_positive = if (any(is.finite(x))) mean(is.finite(x) & x > 0) else NA_real_,
    median_positive = if (length(good)) median(good) else NA_real_,
    p05_positive = if (length(good)) unname(quantile(good,.05)) else NA_real_,
    p95_positive = if (length(good)) unname(quantile(good,.95)) else NA_real_)
})
save_csv(bind_rows(audit_values), "value_audit")
save_csv(tibble(n_rows = nrow(prop), n_unique_clip = n_distinct(prop$clip, na.rm = TRUE),
  n_missing_clip = sum(is.na(prop$clip)),
  n_duplicate_valid_rows = sum(!is.na(prop$clip)) - n_distinct(prop$clip, na.rm = TRUE)), "value_keys")
if ("tax_year" %in% names(prop)) save_csv(count(prop, tax_year, name = "n"), "tax_years")
if ("assessed_year" %in% names(prop)) save_csv(count(prop, assessed_year, name = "n"), "assessed_years")
if ("calculated_total_value_source_code" %in% names(prop)) {
  save_csv(count(prop, calculated_total_value_source_code, name = "n"), "value_source_codes")
}
dbWriteTable(con,"prop_vintage",data.frame(clip=as.character(prop$clip),
  tax_year=suppressWarnings(as.integer(prop$tax_year))),overwrite=TRUE)
alignment <- dbGetQuery(con,"WITH p AS (
  SELECT TRY_CAST(clip AS BIGINT) AS clip,MAX(tax_year) AS tax_year
  FROM prop_vintage GROUP BY 1)
  SELECT COUNT(*) AS n_events,COUNT(p.clip) AS n_matched,
    AVG(CASE WHEN tax_year IS NOT NULL THEN
      CASE WHEN tax_year < year(e.sale_date)-1 THEN 1.0 ELSE 0.0 END END) AS share_stale_among_dated,
    MEDIAN(year(e.sale_date)-tax_year) AS median_vintage_gap
  FROM ca_events e LEFT JOIN p USING(clip)
  WHERE e.class<>'market_sale' AND year(e.sale_date) BETWEEN 2017 AND 2020")
save_csv(alignment,"value_event_alignment")
value_summary <- bind_rows(audit_values)
tax_dist <- count(prop,tax_year,name="n")
audit_table <- c("\\begin{table}[!ht]\\centering",
  "\\caption{California valuation audit}\\label{tab:welfare_data}",
  "{\\small\\begin{tabular}{lr}\\toprule Check & Result \\\\",
  "\\midrule",
  paste0("Property rows & ",fmt(nrow(prop))," \\\\"),
  paste0("Distinct nonmissing parcel IDs & ",fmt(n_distinct(prop$clip, na.rm = TRUE))," \\\\"),
  paste0("Rows missing parcel ID & ",fmt(sum(is.na(prop$clip)))," \\\\"),
  paste0("Market-value field observed (\\%) & ",fmt(100*value_summary$share_nonmissing[value_summary$field=="market_total_value"],1)," \\\\"),
  paste0("Tax-year 2008--2009 rows (\\% of all rows) & ",fmt(100*sum(tax_dist$n[tax_dist$tax_year %in% c(2008,2009)])/nrow(prop),1)," \\\\"),
  paste0("Matched, dated 2017--2020 events with stale tax year (\\%) & ",fmt(100*alignment$share_stale_among_dated,1)," \\\\"),
  paste0("Median event minus tax-year gap (years) & ",fmt(alignment$median_vintage_gap)," \\\\"),
  "\\bottomrule\\end{tabular}}",
  "\\begin{minipage}{0.95\\textwidth}\\vspace{3pt}{\\scriptsize\\textit{Notes:} Stale means more than one year before the event. The alignment audit uses California family events and the latest available tax year per parcel. These fields do not identify contemporaneous pre-reform market values or assessment gaps. Full field, key, missingness, and vintage diagnostics are saved by the welfare script.}\\end{minipage}\\end{table}")
writeLines(audit_table,path(tables_dir,"TA_welfare_data.tex"))
ids <- unique(as.character(prop$clip))
ca_ids <- dbGetQuery(con, "SELECT DISTINCT clip FROM ca_events")$clip
save_csv(tibble(n_ca_event_parcels = length(ca_ids),
  share_in_prop = mean(as.character(ca_ids) %in% ids)), "value_linkage")
rm(prop, sample_prop); gc()

# ---- 4. External parameters and complete scenario grid -----------------
# LAO Oct 2017, Report 3706: $3k-$4k for a long-held Los Angeles home.
# Source dollar year unspecified: assume publication-year dollars (2017).
# CPI-U annual average, all items, US city average, NSA, 1982-84=100.
# BLS historical table: https://www.bls.gov/regions/mid-atlantic/data/consumerpriceindexhistorical_us_table.htm
cpi_monthly <- read_csv(path(r_dir,"inputs","welfare_cpi.csv"),show_col_types=FALSE)
stopifnot(nrow(cpi_monthly)==24, all(table(cpi_monthly$year)==12),
          n_distinct(paste(cpi_monthly$year,cpi_monthly$month))==24)
cpi_annual <- cpi_monthly |> group_by(year) |> summarise(cpi=round(mean(cpi_u),3),.groups="drop")
cpi_2017 <- cpi_annual$cpi[cpi_annual$year==2017]
cpi_2023 <- cpi_annual$cpi[cpi_annual$year==2023]
stopifnot(cpi_2017==245.120,cpi_2023==304.702)
inflator <- cpi_2023 / cpi_2017
wedge_mid <- 3500 * inflator
save_csv(tibble(parameter = c("CPI_2017","CPI_2023","wedge_low_2017","wedge_mid_2017","wedge_high_2017"),
  value = c(cpi_2017,cpi_2023,3000,3500,4000),
  status = c("external","external","external benchmark","assumed midpoint","external benchmark"),
  source = c(rep("https://www.bls.gov/regions/mid-atlantic/data/consumerpriceindexhistorical_us_table.htm",2),
             rep("https://lao.ca.gov/Publications/Report/3706",3))), "external_inputs")
grid <- expand_grid(group = c("broad","same_surname"), alpha = c(0,.25,.5,.75,1),
  delay = c(0,5,10), wedge_2017 = c(1750,3000,3500,4000,7000),
  lambda = c(.25,.5,.75), hazard_scale = c(.5,1,2), discount = c(0,.03,.05)) |>
  mutate(scenario_id = row_number(), wedge = wedge_2017 * inflator)
annual <- vector("list", nrow(grid)); summary_rows <- vector("list", nrow(grid))
for (j in seq_len(nrow(grid))) {
  z <- grid[j, ]
  s <- surv |> filter(group == z$group, month %in% seq(0,108,12)) |> pull(survival)
  a <- calibrate_path(flow,z$alpha,z$delay,s,z$wedge,z$lambda,z$discount,z$hazard_scale)
  annual[[j]] <- mutate(a, scenario_id = j)
  summary_rows[[j]] <- bind_cols(z, tibble(stock_2031 = tail(a$stock,1),
    property_years = sum(a$stock), annual_gain_2031 = tail(a$annual_gain,1),
    pv_gain = sum(a$discounted_gain)))
}
summary_grid <- bind_rows(summary_rows); annual_grid <- bind_rows(annual)
save_csv(summary_grid, "scenarios")
save_csv(annual_grid, "annual_paths")

# ---- 5. Generated manuscript interfaces -------------------------------
main <- summary_grid |> filter(group=="broad",wedge_2017==3500,lambda==.5,
                               hazard_scale==1,discount==.03) |> arrange(alpha,delay)
make_table <- function(name, caption, label, columns, header, rows, notes) {
  writeLines(c("\\begin{table}[!ht]\\centering",paste0("\\caption{",caption,"}\\label{",label,"}"),
    "{\\small",paste0("\\begin{tabular}{",columns,"}\\toprule"),header,"\\midrule",rows,
    "\\bottomrule\\end{tabular}}",
    paste0("\\begin{minipage}{0.96\\textwidth}\\vspace{3pt}{\\scriptsize\\textit{Notes:} ",notes,
           "}\\end{minipage}\\end{table}")),path(tables_dir,name))
}
rows <- with(main, sprintf("%.2f & %d & %s & %s & %s & %s \\\\",alpha,delay,
  fmt(stock_2031/1000,1),fmt(property_years/1000,1),fmt(annual_gain_2031/1e6,1),fmt(pv_gain/1e6,1)))
make_table("T5_welfare.tex","Potential allocation gains: conditional calibration","tab:welfare",
  "rrrrrr","$\\alpha$ & Delay & Stock in 2031 & Property-years & Annual gain & Ten-year PV \\\\\n & (years) & (thousands) & (thousands) & (\\$m, 2031) & (\\$m) \\\\",rows,
  "All dollars are 2023 dollars. The assumed annual wedge is the CPI-adjusted LAO midpoint; $\\lambda=1/2$, the real discount rate is 3\\%, and persistence uses California broad-family parcel episodes. Cohorts enter annually in 2022--2031. No value of $\\alpha$ is empirically preferred. Rows are scenarios, not confidence intervals; zero response and ten-year delay give zero gains within the horizon.")
inputs <- c(
  paste0("Recorded-deed flow & ",fmt(flow)," per year & Net DiD scaled by 2018--2019 CA baseline \\\\"),
  paste0("Net DiD inference & $p=",fmt(perm$perm_p[perm$outcome=="net"],3),"$ & State placebo rank; not a welfare test \\\\"),
  paste0("Annual tax wedge & \\$",fmt(3000*inflator),"--\\$",fmt(4000*inflator)," & LAO long-held Los Angeles benchmark \\\\"),
  "Source dollar year & 2017 & Assumed publication-year dollars; CPI to 2023 \\\\",
  "Allocation conversion & $\\alpha\\in[0,1]$ & Assumed properties per missing deed \\\\",
  "Mean surplus gap & $\\lambda w$ & Uniform benchmark $\\lambda=1/2$ \\\\",
  "Persistence & CA parcel-episode survival & Proxy; marginal survival is not identified \\\\",
  "Delay / horizon & 0, 5, 10 / 10 years & Assumptions; grandfathered stock excluded \\\\")
make_table("T6_welfare_inputs.tex","Inputs to the welfare calibration","tab:welfare_inputs",
  "p{0.22\\textwidth}p{0.25\\textwidth}p{0.42\\textwidth}","Input & Value & Evidence status \\\\",inputs,
  "The tax wedge is an external benchmark, not a statewide marginal estimate. The SCM estimates are reported separately and do not enter the net-DiD reference flow. The conversion parameter includes legal-title-only responses and multiple deeds per property; it is not estimated using the national deeds-per-parcel ratio.")
reference <- main |> filter(alpha==.5,delay==5)
macro <- function(n,v) paste0("\\newcommand{\\",n,"}{",v,"}")
writeLines(c(macro("WelfareFlow",fmt(flow)),macro("WelfareBaseline",fmt(base_flow)),
  macro("WelfareNetPercent",fmt(100*(1-exp(beta_net)),1)),
  macro("WelfareWedge",fmt(wedge_mid)),macro("WelfareWedgeLow",fmt(3000*inflator)),
  macro("WelfareWedgeHigh",fmt(4000*inflator)),macro("WelfareInflator",fmt(inflator,4)),
  macro("WelfareExampleStock",fmt(reference$stock_2031)),
  macro("WelfareExampleAnnual",fmt(reference$annual_gain_2031/1e6,1)),
  macro("WelfareExamplePV",fmt(reference$pv_gain/1e6,1)),
  macro("WelfareCABroadSurvival",fmt(100*surv$survival[surv$group=="broad" & surv$month==120],1)),
  macro("WelfareCASameSurvival",fmt(100*surv$survival[surv$group=="same_surname" & surv$month==120],1))),
  path(tables_dir,"welfare_numbers.tex"))
# Existing national facts remain event-based; do not substitute CA episodes.
haz <- read_result("hazard_sold_within.csv")
hkm <- read_result("hazard_km_curves.csv")
hv <- function(cl,key) haz[[key]][haz$class==cl]
ks <- function(cl,t) hkm$surv[hkm$class==cl & hkm$t==t]
writeLines(c(
 macro("FamilySoldTwo",fmt(100*hv("family_person","sold_24m"),1)),
 macro("MarketSoldTwo",fmt(100*hv("market_sale","sold_24m"),1)),
 macro("FamilySoldFive",fmt(100*hv("family_person","sold_60m"),1)),
 macro("MarketSoldFive",fmt(100*hv("market_sale","sold_60m"),1)),
 macro("FamilyRetainedTen",fmt(100*ks("family_person",120))),
 macro("FamilySoldTen",fmt(100*(1-ks("family_person",120)))),
 macro("MarketSoldTen",fmt(100*(1-ks("market_sale",120)))),
 macro("FamilyConditionalSold",fmt(100*(1-ks("family_person",120)/ks("family_person",24)))),
 macro("MarketConditionalSold",fmt(100*(1-ks("market_sale",120)/ks("market_sale",24))))),
 path(tables_dir,"hazard_numbers.tex"))
fig_ids <- main |> filter(alpha %in% c(0,.25,.5,1)) |> select(scenario_id,alpha,delay)
fig <- annual_grid |> inner_join(fig_ids,by="scenario_id") |>
  mutate(delay = factor(delay,levels=c(0,5,10),labels=c("No delay","Five-year delay","Ten-year delay")))
p <- ggplot(fig,aes(year,stock/1000,color=factor(alpha),group=alpha))+
  geom_line(linewidth=.8)+facet_wrap(~delay,nrow=1)+
  scale_color_manual(values=palette_paper()[1:4],name=expression(alpha))+
  scale_x_continuous(breaks=c(2022,2025,2028,2031))+
  labs(x=NULL,y="Potentially avoided misallocation (thousand properties)")+
  theme(axis.text.x=element_text(angle=45,hjust=1))
ggsave(path(figures_dir,"F8_welfare_stock.pdf"),p,width=9,height=4.5)
ggsave(path(figures_dir,"F8_welfare_stock.png"),p,width=9,height=4.5,dpi=180)

# One-at-a-time sensitivities around the explicitly illustrative alpha=.5, L=5 case.
sens <- summary_grid |> filter(alpha==.5,delay==5) |>
  filter((group=="broad" & lambda==.5 & hazard_scale==1 & discount==.03) |
    (wedge_2017==3500 & group=="broad" & hazard_scale==1 & discount==.03) |
    (wedge_2017==3500 & group=="broad" & lambda==.5 & discount==.03) |
    (wedge_2017==3500 & group=="broad" & lambda==.5 & hazard_scale==1) |
    (wedge_2017==3500 & lambda==.5 & hazard_scale==1 & discount==.03))
sens_rows <- with(sens,sprintf("%s & %s & %.2f & %.1f & %.0f & %s \\\\",
  ifelse(group=="broad","Broad","Same surname"),fmt(wedge),lambda,hazard_scale,
  100*discount,fmt(pv_gain/1e6,1)))
make_table("TA_welfare_sensitivity.tex","Welfare sensitivity around an illustrative scenario","tab:welfare_sensitivity",
  "lrrrrr","Persistence & Wedge (\\$) & $\\lambda$ & Hazard scale & Discount (\\%) & PV (\\$m) \\\\",
  sens_rows,"Every row assumes $\\alpha=0.5$ and a five-year delay; neither assumption is estimated. Dollar values are 2023 dollars. Hazard scale $k$ replaces survival $S(a)$ with $S(a)^k$. All 4,050 joint scenarios are saved in the machine-readable output; this table changes one input at a time.")
capture.output(sessionInfo(),file=path(logs_dir,"11_welfare_sessionInfo.txt"))
dbDisconnect(con,shutdown=TRUE)
message("Welfare calibration complete: flow=",fmt(flow),"; scenarios=",nrow(summary_grid))
