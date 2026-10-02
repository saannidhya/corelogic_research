# Economic and accounting checks; synthetic cases do not read source data.
source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
source(path(r_dir,"11_welfare_helpers.R"))
con <- open_corelogic_duckdb(threads=2L)
raw <- data.frame(sale_raw=c(20200131,20200229,20200230,20201231,20200300))
dbWriteTable(con,"date_test",raw)
date_test <- dbGetQuery(con,glue("SELECT {event_date_sql()} AS d FROM date_test"))$d
stopifnot(identical(as.character(date_test),c("2020-01-31","2020-02-29",NA,"2020-12-31","2020-03-01")))
dbExecute(con,"CREATE TEMP TABLE ca_events AS SELECT * FROM (VALUES
 (1,'family_person',DATE '2008-01-01'),(1,'family_other',DATE '2009-01-01'),
 (1,'market_sale',DATE '2010-01-01'),(1,'family_person',DATE '2011-01-01'),
 (2,'family_person',DATE '2007-01-01'),(2,'family_person',DATE '2008-01-01'),
 (3,'family_other',DATE '2018-01-01'),(3,'family_other',DATE '2018-02-01'),
 (3,'market_sale',DATE '2025-01-01')
 ) t(clip,class,sale_date) WHERE sale_date <= DATE '2024-06-30'")
e <- dbGetQuery(con,family_episode_sql(c("family_person","family_other","family_estate")))
stopifnot(nrow(e)==3, sum(e$n_family_deeds)==5, !2 %in% e$clip,
          sum(is.na(e$next_market))==2)
km <- km_from_exits(data.frame(days=c(365,730,1095),n=c(1,1,2),fail=c(1,0,1)))
stopifnot(all.equal(km$survival,c(.75,.75,.375)),survival_at(km,0)==1)
s <- exp(-.04*(0:9))
calc <- function(alpha=.5,delay=0,wedge=4000,lambda=.5,discount=.03,hazard=1)
  calibrate_path(58000,alpha,delay,s,wedge,lambda,discount,hazard)
ref <- calc()
flat <- calibrate_path(100,1,0,rep(1,10),1000,.5,0)
stopifnot(identical(flat$stock,as.numeric(100*(1:10))),
          sum(flat$stock)==5500, sum(flat$discounted_gain)==2750000)
stopifnot(all(calc(alpha=0)$stock==0),all(calc(wedge=0)$annual_gain==0),
 all(calc(lambda=0)$annual_gain==0),all(calc(delay=10)$stock==0),
 all(calc(delay=5)$stock[1:5]==0),ref$stock[1]==.5*58000,
 all(calc(alpha=1)$annual_gain==2*ref$annual_gain),
 all(calc(wedge=8000)$annual_gain==2*ref$annual_gain),
 all(calc(delay=5)$stock<=ref$stock),
 all(calc(hazard=2)$stock<=ref$stock),
 sum(calc(discount=.05)$discounted_gain)<sum(ref$discounted_gain),
 all(ref$year>=2022),sum(ref$stock)<=10*tail(ref$stock,1))
scenarios <- read_csv(path(tables_out_dir,"welfare_scenarios.csv"),show_col_types=FALSE)
stopifnot(nrow(scenarios)==4050, all(scenarios$pv_gain>=0),
 all(scenarios$pv_gain[scenarios$alpha==0 | scenarios$delay==10]==0))
paths <- read_csv(path(tables_out_dir,"welfare_annual_paths.csv"),show_col_types=FALSE)
ca_survival <- read_csv(path(tables_out_dir,"welfare_ca_survival.csv"),show_col_types=FALSE)
episode_audit <- read_csv(path(tables_out_dir,"welfare_episode_audit.csv"),show_col_types=FALSE)
flow <- read_csv(path(tables_out_dir,"welfare_effect_inputs.csv"),show_col_types=FALSE)$annual_deeds[1]
stopifnot(nrow(paths)==10*nrow(scenarios), all(paths$year %in% 2022:2031),
          all(ca_survival$survival>=0 & ca_survival$survival<=1),
          all(episode_audit$overlapping_episodes==0))
joined <- left_join(paths,scenarios,by="scenario_id")
# The opening stock contains only the new 2022 cohort, never grandfathered stock.
opening <- filter(joined,year==2022)
stopifnot(all(abs(opening$stock - ifelse(opening$delay==0,opening$alpha*flow,0))<1e-7),
          all(abs(joined$annual_gain-joined$lambda*joined$wedge*joined$stock)<1e-6))
totals <- paths |> group_by(scenario_id) |>
  summarise(py_check=sum(stock),pv_check=sum(discounted_gain),.groups="drop") |>
  left_join(scenarios,by="scenario_id")
stopifnot(all(abs(totals$py_check-totals$property_years)<1e-6),
          all(abs(totals$pv_check-totals$pv_gain)<1e-4))
parameters <- c("group","alpha","delay","wedge_2017","lambda","hazard_scale","discount")
for (parameter in parameters[-1]) {
  grouping <- setdiff(parameters,parameter)
  direction <- if (parameter %in% c("delay","hazard_scale","discount")) -1 else 1
  comparisons <- scenarios |> arrange(.data[[parameter]]) |>
    group_by(across(all_of(grouping))) |>
    summarise(ok=all(direction*diff(pv_gain)>=-1e-5),.groups="drop")
  stopifnot(all(comparisons$ok))
}
dbDisconnect(con,shutdown=TRUE)
cat("PASS: calendar dates, repeated deeds, episode boundaries, censoring, survival, zero cases, timing, units, and comparative statics.\n")
