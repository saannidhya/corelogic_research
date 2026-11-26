# ============================================================
# 01d: Validate panel v2 taxonomy (counts, LAO benchmark, AF break, seeded read)
# Name   : Saani Rawat
# Created: 11/11/2026
# Purpose: Plan 2.5 validation of the fam_relation / fam_trigger / fam_vehicle
#          taxonomy built in 01_build_panel.R (panel v2).
# Inputs : data/derived/03_family_homes/events_v2.parquet
#          CoreLogic OT parquet (names, for the seeded read ONLY)
# Outputs: _outputs/tables/validation_vehicle_2019_v2.csv       national + CA, 2019
#          _outputs/tables/validation_trust_bounds_2019_v2.csv
#          _outputs/tables/validation_lao_benchmark_v2.csv
#          _outputs/tables/validation_ca_af_by_class_year_v2.csv
#          _outputs/tables/validation_ca_af_raw_by_year_v2.csv
#          _outputs/tables/validation_read_precision_v2.csv     (no names)
#          data/derived/03_family_homes/validation/read_sample_v2.csv
#                                      (WITH names; git-ignored data folder only)
# Modes  : STEP=tables (default) writes the count tables;
#          STEP=sample draws the seeded read sample (needs names from the raw store);
#          STEP=score reads validation/read_labels_v2.csv and writes precision.
# Log:
#   11/11/2026  Saani Rawat  first version
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

step <- Sys.getenv("STEP", "tables")
tmp_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "Temp", "fam_duckdb_tmp")
dir_create(tmp_dir)
con <- open_corelogic_duckdb(memory_limit = "16GB")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(tmp_dir)}'"))

panel_path <- path(data_dir, "events_v2.parquet")
stopifnot(file_exists(panel_path))
ev <- glue("read_parquet('{sql_quote_path(panel_path)}')")
validation_dir <- path(data_dir, "validation")
dir_create(validation_dir)

if (step == "tables") {

  # ==========================================================
  # 1. Vehicle and trust-bound counts, 2019 (national and California)
  # ==========================================================
  vehicle_2019 <- dbGetQuery(con, glue("
    SELECT fam_vehicle, COUNT(*) AS n_national,
           SUM((state = 'CA')::INT) AS n_ca
    FROM {ev} WHERE class LIKE 'family_%' AND sale_year = 2019
    GROUP BY 1 ORDER BY 2 DESC"))
  print(vehicle_2019)
  write_csv_strict(vehicle_2019, path(tables_out_dir, "validation_vehicle_2019_v2.csv"))

  bounds_2019 <- dbGetQuery(con, glue("
    SELECT 'national' AS scope, SUM(trust_fam_lo) AS lo, SUM(trust_fam_hi) AS hi,
           SUM((trust_dir = 't2t')::INT) AS t2t_excluded, COUNT(*) AS family_trust_rows
    FROM {ev} WHERE class = 'family_trust' AND sale_year = 2019
    UNION ALL
    SELECT 'CA', SUM(trust_fam_lo), SUM(trust_fam_hi),
           SUM((trust_dir = 't2t')::INT), COUNT(*)
    FROM {ev} WHERE class = 'family_trust' AND sale_year = 2019 AND state = 'CA'"))
  print(bounds_2019)
  write_csv_strict(bounds_2019, path(tables_out_dir, "validation_trust_bounds_2019_v2.csv"))

  # Table-2 comparison: old definition (trust_rel = 'clear', any direction) vs new bounds, all years
  bounds_compare <- dbGetQuery(con, glue("
    SELECT sale_year,
           SUM((trust_rel = 'clear')::INT)                              AS lower_old_trust_rel,
           SUM((trust_rel IN ('clear','ambiguous'))::INT)               AS upper_old_trust_rel,
           SUM(trust_fam_lo)                                            AS lower_new,
           SUM(trust_fam_hi)                                            AS upper_new,
           SUM((trust_dir = 't2t' AND trust_rel = 'clear')::INT)        AS t2t_clear
    FROM {ev} WHERE class = 'family_trust' GROUP BY 1 ORDER BY 1"))
  write_csv_strict(bounds_compare, path(tables_out_dir, "validation_trust_bounds_vs_table2_v2.csv"))
  cat("National lower bound, all years: old ", sum(bounds_compare$lower_old_trust_rel),
      " | new ", sum(bounds_compare$lower_new), " | trust-to-trust clear ",
      sum(bounds_compare$t2t_clear), "\n", sep = "")

  # ==========================================================
  # 2. LAO benchmark: California parent-child candidates, 2015-2019
  #    (LAO: 60,000-80,000 parent-child exclusions a year)
  # ==========================================================
  lao <- dbGetQuery(con, glue("
    SELECT sale_year,
      SUM((class IN ('family_person','family_estate','family_other')
           AND fam_relation = 'parent_child_candidate')::INT)            AS pc_direct,
      SUM((class = 'family_trust' AND fam_relation = 'parent_child_candidate'
           AND trust_fam_lo = 1)::INT)                                   AS pc_trust_lo,
      SUM((class = 'family_trust' AND fam_relation = 'parent_child_candidate'
           AND trust_fam_hi = 1)::INT)                                   AS pc_trust_hi,
      SUM((class IN ('family_person','family_estate','family_other')
           AND fam_relation = 'spouse')::INT)                            AS spouse_in_measure
    FROM {ev} WHERE state = 'CA' AND sale_year BETWEEN 2015 AND 2023
    GROUP BY 1 ORDER BY 1")) |>
    mutate(total_lo = pc_direct + pc_trust_lo, total_hi = pc_direct + pc_trust_hi)
  print(lao)
  write_csv_strict(lao, path(tables_out_dir, "validation_lao_benchmark_v2.csv"))

  # ==========================================================
  # 3. California AF deeds: reclassification of the buyer, or new records?
  # ==========================================================
  # (a) panel: every class, 2015-2020 (residential, deduplicated)
  af_class <- dbGetQuery(con, glue("
    SELECT sale_year, class, SUM((trust_kw)::INT) AS with_trust_name, COUNT(*) AS n
    FROM {ev} WHERE state = 'CA' AND doc_type = 'AF' AND sale_year BETWEEN 2015 AND 2020
    GROUP BY 1, 2 ORDER BY 2, 1"))
  af_tot <- af_class |>
    group_by(sale_year) |>
    summarise(n_all_classes = sum(n), with_trust_name = sum(with_trust_name), .groups = "drop")
  print(af_tot)
  write_csv_strict(af_class, path(tables_out_dir, "validation_ca_af_by_class_year_v2.csv"))

  # (b) raw OT, no dedupe, residential and not: counts and buyer-name-is-a-trust share
  af_raw <- dbGetQuery(con, glue("
    SELECT TRY_CAST(sale_derived_date AS BIGINT) // 10000 AS sale_year,
           residential_indicator AS residential,
           COUNT(*) AS n_af,
           SUM((upper(coalesce(buyer_1_full_name, '')) LIKE '%TRUST%')::INT) AS buyer_trust,
           SUM((upper(coalesce(seller_1_full_name, '')) LIKE '%TRUST%')::INT) AS seller_trust,
           SUM((coalesce(title_company_name, '') = 'ATTORNEY ONLY')::INT) AS title_attorney_only
    FROM read_parquet('{sql_quote_path(ot_parquet_glob())}', union_by_name = true, hive_partitioning = true)
    WHERE state = 'CA' AND sale_document_type_code = 'AF'
      AND TRY_CAST(sale_derived_date AS BIGINT) BETWEEN 20150101 AND 20201231
    GROUP BY 1, 2 ORDER BY 2, 1"))
  print(af_raw)
  write_csv_strict(af_raw, path(tables_out_dir, "validation_ca_af_raw_by_year_v2.csv"))

} else if (step == "sample") {

  # ==========================================================
  # 4. Seeded read sample (names appear ONLY in the git-ignored data folder)
  # ==========================================================
  # Deterministic seeded draw: order by hash(clip, sale_raw, seed). Ranges 2015-2023.
  seed <- 20261111L
  cells <- tribble(
    ~cell,                     ~where_sql,                                                                   ~n,
    "spouse_CA_IT",            "state = 'CA' AND fam_relation = 'spouse'",                                    100,
    "owner_retained_nm_fuzzy", "class = 'family_person' AND fam_relation_nm = 'owner_retained'",              100,
    "parent_child_direct",     "class IN ('family_person','family_other') AND fam_relation_nm = 'parent_child_candidate'", 100,
    "trust_to_other_person",   "fam_vehicle = 'trust_to_other_person'",                                      100,
    "into_trust_for_others",   "fam_vehicle = 'into_trust_for_others' AND tr_irrev IS NOT NULL AND tr_irrev + tr_resid + tr_child = 0", 100,
    "into_trust_keyword",      "fam_vehicle = 'into_trust_for_others' AND tr_irrev + tr_resid + tr_child > 0",  100,
    "trust_to_self_JF2021",    "fam_vehicle = 'trust_to_self' AND ym IN (202101, 202102)",                   50,
    "trust_to_self_2019",      "fam_vehicle = 'trust_to_self' AND sale_year = 2019",                         50,
    "cross_surname",           "class = 'family_other' AND fam_relation_nm = 'cross_surname'",               100,
    "trust_unlinked",          "fam_vehicle = 'trust_unlinked'",                                             100,
    "death_affidavit_trust",   "fam_vehicle = 'death_affidavit_trust' AND state = 'CA'",                     100
  )
  draws <- purrr::pmap_dfr(cells, function(cell, where_sql, n) {
    dbGetQuery(con, glue("
      SELECT '{cell}' AS cell, clip, sale_raw, state, sale_year, class, doc_type, fam_relation,
             fam_relation_nm, fam_trigger, fam_vehicle, trust_rel, trust_dir
      FROM {ev}
      WHERE {where_sql} AND sale_year BETWEEN 2015 AND 2023
      ORDER BY hash(clip, sale_raw, {seed}) LIMIT {n}"))
  })
  draws <- draws |> group_by(cell) |> mutate(idx = row_number()) |> ungroup()
  cat("Sample sizes:\n"); print(count(draws, cell))
  dbWriteTable(con, "draws", draws, overwrite = TRUE)

  # names from the raw store, restricted to the sampled states
  states_needed <- unique(draws$state)
  root <- normalizePath(here("data", "corelogic_extracts", "by_state", "ot"), winslash = "/")
  globs <- file.path(root, paste0("state=", states_needed), "year=*", "*.parquet")
  glob_sql <- paste0("['", paste(sql_quote_path(globs), collapse = "', '"), "']")
  named <- dbGetQuery(con, glue("
    SELECT d.*, upper(coalesce(r.seller_1_full_name, '')) AS seller,
           upper(coalesce(r.buyer_1_full_name, '')) AS buyer1,
           upper(coalesce(r.buyer_2_full_name, '')) AS buyer2
    FROM draws d
    JOIN (SELECT TRY_CAST(clip AS BIGINT) AS clip, TRY_CAST(sale_derived_date AS BIGINT) AS sale_raw,
                 seller_1_full_name, buyer_1_full_name, buyer_2_full_name
          FROM read_parquet({glob_sql}, union_by_name = true)
          WHERE TRY_CAST(clip AS BIGINT) IN (SELECT clip FROM draws)) r
      ON r.clip = d.clip AND r.sale_raw = d.sale_raw
    -- duplicate raw rows can run in opposite directions; keep the raw row that agrees with the
    -- panel's trust direction (the panel kept one of them), then break ties by name
    QUALIFY ROW_NUMBER() OVER (PARTITION BY d.cell, d.idx ORDER BY
        CASE WHEN d.trust_dir = 't2p' AND upper(r.seller_1_full_name) LIKE '%TRUST%'
                  AND upper(coalesce(r.buyer_1_full_name, '')) NOT LIKE '%TRUST%' THEN 0
             WHEN d.trust_dir = 'p2t' AND upper(coalesce(r.buyer_1_full_name, '')) LIKE '%TRUST%'
                  AND upper(coalesce(r.seller_1_full_name, '')) NOT LIKE '%TRUST%' THEN 0
             WHEN d.trust_dir = 't2t' AND upper(r.seller_1_full_name) LIKE '%TRUST%'
                  AND upper(coalesce(r.buyer_1_full_name, '')) LIKE '%TRUST%' THEN 0
             WHEN d.trust_dir IS NULL THEN 0 ELSE 1 END,
        r.seller_1_full_name, r.buyer_1_full_name) = 1
    ORDER BY d.cell, d.idx"))
  stopifnot(nrow(named) == nrow(draws))
  write_csv_strict(named, path(validation_dir, "read_sample_v2.csv"))

} else if (step == "chain_tables") {

  # ==========================================================
  # 6. trust_chain: coverage by year, with and without a 10-year look-back cap
  # ==========================================================
  # Coverage = share of class = 'family_trust' trust-to-person deeds with a chain found
  # (any of heir / self / other / unclear). The cap keeps chains with a gap <= 3,652 days;
  # without a cap, chain-found rises mechanically with the panel window (starts 2007).
  chain_cov <- dbGetQuery(con, glue("
    SELECT sale_year, CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS grp,
           COUNT(*) AS n_t2p,
           AVG((trust_chain <> 'no_chain')::INT)                                AS found_nocap,
           AVG((trust_chain <> 'no_chain' AND trust_chain_gap_days <= 3652)::INT) AS found_cap10,
           AVG((trust_chain = 'heir')::INT)                                     AS heir_nocap,
           AVG((trust_chain = 'heir' AND trust_chain_gap_days <= 3652)::INT)    AS heir_cap10
    FROM {ev} WHERE class = 'family_trust' AND trust_dir = 't2p' AND sale_year BETWEEN 2012 AND 2023
    GROUP BY 1, 2 ORDER BY 2, 1"))
  print(tibble::as_tibble(chain_cov), n = 40)
  write_csv_strict(chain_cov, path(tables_out_dir, "validation_trust_chain_coverage_v2.csv"))

  # trust_chain x fam_vehicle, 2019 and 2021, national and California
  chain_x <- dbGetQuery(con, glue("
    SELECT sale_year, fam_vehicle, trust_chain,
           COUNT(*) AS n_national,
           SUM((state = 'CA')::INT) AS n_ca,
           SUM((trust_chain_gap_days <= 3652)::INT) AS n_national_cap10,
           SUM((state = 'CA' AND trust_chain_gap_days <= 3652)::INT) AS n_ca_cap10
    FROM {ev} WHERE class = 'family_trust' AND trust_dir = 't2p' AND sale_year IN (2019, 2021)
    GROUP BY ALL ORDER BY 1, 2, 3"))
  print(tibble::as_tibble(chain_x), n = 80)
  write_csv_strict(chain_x, path(tables_out_dir, "validation_trust_chain_by_vehicle_v2.csv"))

} else if (step == "sample_chain") {

  # ==========================================================
  # 7. Seeded read for trust_chain (names appear ONLY in the git-ignored data folder)
  # ==========================================================
  seed <- 20261112L
  cells <- tribble(
    ~cell,                      ~where_sql,                                                                                  ~n,
    "chain_heir_cap10",         "trust_chain = 'heir' AND trust_chain_gap_days <= 3652",                                      100,
    "name_rule_t2o_not_heir",   "fam_vehicle = 'trust_to_other_person' AND trust_chain <> 'heir'",                            50
  )
  draws <- purrr::pmap_dfr(cells, function(cell, where_sql, n) {
    dbGetQuery(con, glue("
      SELECT '{cell}' AS cell, clip, sale_raw, state, sale_year, doc_type, fam_vehicle, trust_chain,
             trust_chain_named, trust_chain_gap_days,
             CAST(strftime(strptime(CAST(sale_raw AS VARCHAR), '%Y%m%d') - to_days(trust_chain_gap_days), '%Y%m%d') AS BIGINT) AS prior_raw
      FROM {ev}
      WHERE {where_sql} AND class = 'family_trust' AND trust_dir = 't2p' AND sale_year BETWEEN 2015 AND 2023
      ORDER BY hash(clip, sale_raw, {seed}) LIMIT {n}"))
  })
  draws <- draws |> group_by(cell) |> mutate(idx = row_number()) |> ungroup()
  print(count(draws, cell))
  dbWriteTable(con, "draws", draws, overwrite = TRUE)
  states_needed <- unique(draws$state)
  root <- normalizePath(here("data", "corelogic_extracts", "by_state", "ot"), winslash = "/")
  globs <- file.path(root, paste0("state=", states_needed), "year=*", "*.parquet")
  glob_sql <- paste0("['", paste(sql_quote_path(globs), collapse = "', '"), "']")
  # raw rows for the conveyance (sale_raw) and for the prior into-trust deed (prior_raw)
  dbExecute(con, glue("
    CREATE OR REPLACE TEMP TABLE raw_names AS
    SELECT TRY_CAST(clip AS BIGINT) AS clip, TRY_CAST(sale_derived_date AS BIGINT) AS d,
           upper(coalesce(seller_1_full_name, '')) AS seller, upper(coalesce(buyer_1_full_name, '')) AS buyer1,
           upper(coalesce(buyer_2_full_name, '')) AS buyer2
    FROM read_parquet({glob_sql}, union_by_name = TRUE)
    WHERE TRY_CAST(clip AS BIGINT) IN (SELECT clip FROM draws)"))
  named <- dbGetQuery(con, "
    SELECT d.*, c.seller AS conv_seller, c.buyer1 AS conv_buyer1, c.buyer2 AS conv_buyer2,
           p.seller AS prior_seller, p.buyer1 AS prior_buyer
    FROM draws d
    LEFT JOIN (SELECT * FROM raw_names WHERE seller LIKE '%TRUST%' AND buyer1 NOT LIKE '%TRUST%') c
      ON c.clip = d.clip AND c.d = d.sale_raw
    LEFT JOIN (SELECT * FROM raw_names WHERE buyer1 LIKE '%TRUST%' AND seller NOT LIKE '%TRUST%') p
      ON p.clip = d.clip AND p.d = d.prior_raw
    QUALIFY ROW_NUMBER() OVER (PARTITION BY d.cell, d.idx ORDER BY c.seller, c.buyer1, p.seller, p.buyer1) = 1
    ORDER BY d.cell, d.idx")
  stopifnot(nrow(named) == nrow(draws))
  write_csv_strict(named, path(validation_dir, "read_sample_chain_v2.csv"))

} else if (step == "score_chain") {

  labels <- readr::read_csv(path(validation_dir, "read_labels_chain_v2.csv"), show_col_types = FALSE)
  prec <- labels |>
    group_by(cell) |>
    summarise(n_read = n(), n_uncertain = sum(is.na(label)), n_correct = sum(label == 1, na.rm = TRUE),
              share_correct = mean(label, na.rm = TRUE),
              se = sqrt(share_correct * (1 - share_correct) / sum(!is.na(label))), .groups = "drop")
  print(prec)
  write_csv_strict(prec, path(tables_out_dir, "validation_chain_read_precision_v2.csv"))

} else if (step == "main_shares") {

  # ==========================================================
  # 9. Relation mix of the MAIN measure (main_noaf_trust_heir), 2017-2019
  # ==========================================================
  if (!exists("bygroup_defs", inherits = FALSE)) source(path(r_dir, "00_groups.R"))
  main_filter <- bygroup_defs$filter_sql[bygroup_defs$name == "main_noaf_trust_heir"]
  stopifnot(length(main_filter) == 1)
  shares <- dbGetQuery(con, glue("
    SELECT CASE WHEN state = 'CA' THEN 'CA' ELSE 'other' END AS grp, coalesce(fam_relation_nm, 'NULL') AS fam_relation_nm,
           COUNT(*) AS n
    FROM {ev} WHERE sale_year BETWEEN 2017 AND 2019 AND ({main_filter})
    GROUP BY ALL ORDER BY 1, 3 DESC")) |>
    dplyr::group_by(grp) |> dplyr::mutate(share = n / sum(n)) |> dplyr::ungroup()
  print(as.data.frame(shares))
  write_csv_strict(shares, path(tables_out_dir, "validation_main_measure_relation_shares_v2.csv"))

} else if (step == "score_all") {

  # ==========================================================
  # 8. All read-precision cells in one table, each with its criterion (11/11/2026)
  # ==========================================================
  # label: 1 right, 0 wrong, NA cannot be told from the names. Draws are from the 15:46 panel
  # (before the cross-surname rule); the cross-surname rows are re-scored against the rebuilt panel.
  lab_main  <- readr::read_csv(path(validation_dir, "read_labels_v2.csv"), show_col_types = FALSE)
  lab_chain <- readr::read_csv(path(validation_dir, "read_labels_chain_v2.csv"), show_col_types = FALSE)
  lab_xs    <- readr::read_csv(path(validation_dir, "read_labels_crosssur_v2.csv"), show_col_types = FALSE)
  keys <- dplyr::bind_rows(lab_main, lab_chain, lab_xs) |> dplyr::distinct(clip, sale_raw)
  dbWriteTable(con, "lab_keys", keys, overwrite = TRUE)
  new_rel <- dbGetQuery(con, glue("
    SELECT e.clip, e.sale_raw, e.fam_relation_nm AS new_rel_nm
    FROM {ev} e JOIN lab_keys k ON k.clip = e.clip AND k.sale_raw = e.sale_raw"))
  attach_rel <- function(x) dplyr::left_join(x, new_rel, by = c("clip", "sale_raw"))

  crit <- tibble::tribble(
    ~cell, ~criterion, ~population,
    "death_affidavit_trust", "document type AF with a trust as the buyer", "fam_vehicle = death_affidavit_trust, CA",
    "spouse_CA_IT", "two natural persons with a shared surname (0: trust or entity party; NA: different surnames)", "fam_relation = spouse (IT/IC/DS), 15:46 panel",
    "owner_retained_nm_fuzzy", "the seller is still on title in the buyer names", "class family_person, fam_relation_nm = owner_retained",
    "parent_child_direct", "two different natural persons sharing a surname (NA: CA IT spouse deeds)", "family_person/other, fam_relation_nm = parent_child_candidate",
    "trust_to_other_person", "trust to a surname-linked natural person (0: entity buyer or same person; NA: surname-only trust)", "fam_vehicle = trust_to_other_person",
    "into_trust_for_others", "trust named for a different individual or for children (0: named for the transferor, entity, plan type)", "fam_vehicle = into_trust_for_others, no keyword",
    "into_trust_keyword", "same as into_trust_for_others, keyword-promoted deeds", "fam_vehicle = into_trust_for_others, keyword flag",
    "trust_to_self_JF2021", "the buyer is the trust's named settlor (0: different named person or entity)", "fam_vehicle = trust_to_self, Jan-Feb 2021",
    "trust_to_self_2019", "the buyer is the trust's named settlor (0: different named person or entity)", "fam_vehicle = trust_to_self, 2019",
    "trust_unlinked", "no family link between the parties (0: initial, nickname, name-order or spelling link missed)", "fam_vehicle = trust_unlinked",
    "chain_heir_cap10", "chain A -> trust -> B holds and B is a different natural person with A's surname", "trust_chain = heir, gap <= 3652 days",
    "name_rule_t2o_not_heir", "name rule holds on the names (trust names an individual; B different natural person, same surname)", "trust_to_other_person and trust_chain <> heir"
  )
  prec_one <- function(x) dplyr::summarise(x, n_read = dplyr::n(), n_uncertain = sum(is.na(label)),
                                            n_correct = sum(label == 1, na.rm = TRUE),
                                            share_correct = mean(label, na.rm = TRUE),
                                            se = sqrt(share_correct * (1 - share_correct) / sum(!is.na(label))))
  prec_cells <- dplyr::bind_rows(lab_main, lab_chain) |>
    dplyr::filter(cell %in% crit$cell) |>
    dplyr::group_by(cell) |> prec_one() |> dplyr::left_join(crit, by = "cell")

  # cross-surname: the same three questions, before and after the rule
  xs_old <- lab_main |> dplyr::filter(cell == "cross_surname") |> attach_rel()
  xs_new <- xs_old |> dplyr::filter(new_rel_nm == "cross_surname")
  moved  <- lab_xs |> dplyr::filter(cell == "moved") |> attach_rel() |> dplyr::filter(new_rel_nm == "owner_retained")
  stays  <- lab_xs |> dplyr::filter(cell == "stays") |> attach_rel() |> dplyr::filter(new_rel_nm == "cross_surname")
  xs_cells <- dplyr::bind_rows(
    prec_one(xs_old) |> dplyr::mutate(cell = "cross_surname_before_rule",
      criterion = "the label itself: two natural persons with genuinely different surnames (0: spelling variant, same person, shared surname, entity)",
      population = "fam_relation_nm = cross_surname on the 15:46 panel (Stage B read, 100 deeds)"),
    prec_one(xs_new) |> dplyr::mutate(cell = "cross_surname_after_rule",
      criterion = "the label itself: two natural persons with genuinely different surnames",
      population = "the same 100 deeds that are still cross_surname after the rule (same labels)"),
    prec_one(moved) |> dplyr::mutate(cell = "cross_surname_moved_to_owner_retained",
      criterion = "same person: the two names are one individual or one household retitling",
      population = "prototype read, deeds the final rule moves to owner_retained (CA and other mixed)"),
    prec_one(stays) |> dplyr::mutate(cell = "cross_surname_stays",
      criterion = "a family transfer between distinct people: the rule left no same-person deed behind (not the same test as the label)",
      population = "prototype read, deeds that stay cross_surname after the rule (CA and other mixed)"))
  prec_all <- dplyr::bind_rows(prec_cells, xs_cells) |>
    dplyr::select(cell, criterion, population, n_read, n_uncertain, n_correct, share_correct, se)
  print(as.data.frame(prec_all[, c("cell", "n_read", "n_uncertain", "n_correct", "share_correct")]))
  write_csv_strict(prec_all, path(tables_out_dir, "validation_read_precision_v2.csv"))

} else if (step == "score") {

  # ==========================================================
  # 5. Precision from hand-coded labels (1 = rule's label is right on the read)
  # ==========================================================
  labels <- readr::read_csv(path(validation_dir, "read_labels_v2.csv"), show_col_types = FALSE)
  # label: 1 = rule's label right on the read, 0 = wrong, NA = cannot be told from the names
  prec <- labels |>
    group_by(cell) |>
    summarise(n_read = n(), n_uncertain = sum(is.na(label)),
              n_correct = sum(label == 1, na.rm = TRUE),
              share_correct = mean(label, na.rm = TRUE),
              se = sqrt(share_correct * (1 - share_correct) / sum(!is.na(label))), .groups = "drop")
  print(prec)
  write_csv_strict(prec, path(tables_out_dir, "validation_read_precision_v2.csv"))
}
