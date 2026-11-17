# ============================================================
# 01c: Reconcile panel v2 against the reference panel (read-only)
# Name   : Saani Rawat
# Created: 11/11/2026
# Purpose: Prove that events_v2.parquet reproduces events.parquet on every original
#          column (class above all) before any reader is switched to v2.
# Inputs : data/derived/03_family_homes/events.parquet     (reference, read only)
#          data/derived/03_family_homes/events_v2[_dev].parquet
# Outputs: _outputs/tables/reconcile_panel_v2[_dev].csv
#          _outputs/logs/01c_reconcile_panel_v2[_dev].log
# Modes  : FAM_DEV=1  dev panel (FAM_DEV_STATES, default VT,RI,CA) vs the same states in
#                     the reference panel; key-level comparison.
#          default    full panel; GATE: stops (exit 1) if any check fails.
# Log:
#   11/11/2026  Saani Rawat  first version
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

dev_mode   <- Sys.getenv("FAM_DEV", "0") == "1"
dev_states <- strsplit(Sys.getenv("FAM_DEV_STATES", "VT,RI,CA"), ",")[[1]]
sfx        <- ifelse(dev_mode, "_v2_dev", "_v2")
new_file   <- Sys.getenv("FAM_PANEL_OUT", ifelse(dev_mode, "events_v2_dev.parquet", "events_v2.parquet"))

sink(path(logs_dir, paste0("01c_reconcile_panel", sfx, ".log")), split = TRUE)
on.exit(sink(), add = TRUE)
cat("Reconciling ", new_file, " at ", format(Sys.time()), " | dev_mode=", dev_mode, "\n", sep = "")

tmp_dir <- file.path(Sys.getenv("LOCALAPPDATA"), "Temp", "fam_duckdb_tmp")
dir_create(tmp_dir)
con <- open_corelogic_duckdb(memory_limit = "16GB")
on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(tmp_dir)}'"))

old_path <- path(data_dir, "events.parquet")
new_path <- path(data_dir, new_file)
stopifnot(file_exists(old_path), file_exists(new_path))

state_filter <- if (dev_mode) {
  paste0("WHERE state IN ('", paste(dev_states, collapse = "','"), "')")
} else ""
old <- glue("(SELECT * FROM read_parquet('{sql_quote_path(old_path)}') {state_filter})")
new <- glue("read_parquet('{sql_quote_path(new_path)}')")

# ------------------------------------------------------------
# Schema: the original columns must be first, with identical names and types
# ------------------------------------------------------------
sch_old <- dbGetQuery(con, glue("DESCRIBE SELECT * FROM {old}"))[, c("column_name", "column_type")]
sch_new <- dbGetQuery(con, glue("DESCRIBE SELECT * FROM {new}"))[, c("column_name", "column_type")]
orig_cols <- sch_old$column_name
new_cols  <- setdiff(sch_new$column_name, orig_cols)
cat("Original columns: ", length(orig_cols), " | new columns: ", length(new_cols), "\n", sep = "")

checks <- list()
add_check <- function(name, pass, detail) {
  checks[[length(checks) + 1L]] <<- tibble(check = name, pass = isTRUE(pass), detail = as.character(detail))
  cat(sprintf("[%s] %s: %s\n", ifelse(isTRUE(pass), "PASS", "FAIL"), name, detail))
}
add_check("original columns: names and types identical",
          identical(sch_new[seq_along(orig_cols), ], sch_old), paste(length(orig_cols), "columns"))

# ------------------------------------------------------------
# Row counts and class counts
# ------------------------------------------------------------
n_old <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM {old}"))$n
n_new <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM {new}"))$n
cat("Rows: reference ", format(n_old, big.mark = ","), " | v2 ", format(n_new, big.mark = ","), "\n", sep = "")

cls_old <- dbGetQuery(con, glue("SELECT class, COUNT(*) AS n FROM {old} GROUP BY 1 ORDER BY 1"))
cls_new <- dbGetQuery(con, glue("SELECT class, COUNT(*) AS n FROM {new} GROUP BY 1 ORDER BY 1"))
print(full_join(cls_old, cls_new, by = "class", suffix = c("_ref", "_v2")))

tr_old <- dbGetQuery(con, glue("SELECT coalesce(trust_dir,'-') d, coalesce(trust_rel,'-') r, COUNT(*) AS n FROM {old} GROUP BY 1,2 ORDER BY 1,2"))
tr_new <- dbGetQuery(con, glue("SELECT coalesce(trust_dir,'-') d, coalesce(trust_rel,'-') r, COUNT(*) AS n FROM {new} GROUP BY 1,2 ORDER BY 1,2"))

if (!dev_mode) {
  # ----------------------------------------------------------
  # FULL panel gate
  # ----------------------------------------------------------
  add_check("row count = 148,711,427", n_new == 148711427 && n_old == 148711427,
            paste("ref", n_old, "v2", n_new))
  add_check("class counts identical (global)", isTRUE(all.equal(cls_old, cls_new, check.attributes = FALSE)),
            paste(cls_new$class, cls_new$n, collapse = "; "))
  add_check("trust_dir x trust_rel counts identical", isTRUE(all.equal(tr_old, tr_new, check.attributes = FALSE)),
            paste(nrow(tr_new), "cells"))

  # Key-level comparison (148.7M-row join). `class` must match on EVERY key; the other original
  # columns can differ only on exact ties of the original dedupe keys, which the October build
  # resolved arbitrarily (cross-state duplicates, and duplicate rows with different zip / b2).
  kd <- dbGetQuery(con, glue("
    SELECT COUNT(*) AS n_keys,
           SUM((a.class <> b.class)::INT)                                          AS class_diff,
           SUM((a.state <> b.state)::INT)                                          AS state_diff,
           SUM((coalesce(a.trust_dir,'') <> coalesce(b.trust_dir,''))::INT)        AS trust_dir_diff,
           SUM((coalesce(a.trust_rel,'') <> coalesce(b.trust_rel,''))::INT)        AS trust_rel_diff,
           SUM((a.trust_prior_found <> b.trust_prior_found)::INT)                  AS prior_found_diff,
           SUM((a.absentee_buyer IS DISTINCT FROM b.absentee_buyer)::INT)          AS absentee_diff,
           SUM((a.amt IS DISTINCT FROM b.amt)::INT)                                AS amt_diff,
           SUM((a.fips5 <> b.fips5)::INT)                                          AS fips_diff
    FROM read_parquet('{sql_quote_path(old_path)}') a
    JOIN {new} b USING (clip, sale_raw)"))
  print(t(kd))
  add_check("same key set (every reference key in v2, same count)", kd$n_keys == 148711427 && n_new == kd$n_keys,
            paste(kd$n_keys, "shared keys"))
  add_check("class identical on every key", kd$class_diff == 0, paste(kd$class_diff, "differences"))
  cat("INFO tie-affected columns (arbitrary in the October build): state ", kd$state_diff,
      " | trust_rel ", kd$trust_rel_diff, " | trust_dir ", kd$trust_dir_diff, " | absentee ", kd$absentee_diff,
      " | prior_found ", kd$prior_found_diff, " | amt ", kd$amt_diff, " | fips ", kd$fips_diff, "
", sep = "")
  tr_cmp <- full_join(tr_old, tr_new, by = c("d", "r"), suffix = c("_ref", "_v2")) |>
    mutate(diff = n_v2 - n_ref) |> filter(diff != 0)
  cat("trust cells that differ:
"); print(tr_cmp)
} else {
  # ----------------------------------------------------------
  # DEV panel: key-level comparison on the loaded states
  # ----------------------------------------------------------
  dbExecute(con, glue("CREATE TEMP TABLE o AS SELECT clip, sale_raw, class, coalesce(trust_rel,'') AS trust_rel FROM {old}"))
  dbExecute(con, glue("CREATE TEMP TABLE w AS SELECT clip, sale_raw, class, coalesce(trust_rel,'') AS trust_rel FROM {new}"))
  miss_old <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM o ANTI JOIN w USING (clip, sale_raw)")$n
  extra    <- dbGetQuery(con, "SELECT COUNT(*) AS n FROM w ANTI JOIN o USING (clip, sale_raw)")$n
  cdiff    <- dbGetQuery(con, "SELECT COUNT(*) AS n, SUM((o.class <> w.class)::INT) AS class_diff, SUM((o.trust_rel <> w.trust_rel)::INT) AS rel_diff FROM o JOIN w USING (clip, sale_raw)")
  add_check("every reference key present in v2", miss_old == 0, paste(miss_old, "reference keys missing"))
  add_check("class identical on shared keys", cdiff$class_diff == 0,
            paste(format(cdiff$n, big.mark = ","), "shared keys,", cdiff$class_diff, "class differences"))
  # extra keys must be cross-partition duplicates, i.e. present in the reference panel under another state
  dbExecute(con, glue("CREATE TEMP TABLE ex AS SELECT w.clip, w.sale_raw FROM w ANTI JOIN o USING (clip, sale_raw)"))
  ex_other <- dbGetQuery(con, glue("SELECT COUNT(*) AS n FROM ex JOIN read_parquet('{sql_quote_path(old_path)}') e ON e.clip = ex.clip AND e.sale_raw = ex.sale_raw"))$n
  add_check("extra v2 keys are cross-state duplicates in the reference", extra == ex_other,
            paste(extra, "extra keys;", ex_other, "found under another state"))
  cat("INFO trust_rel differs on ", cdiff$rel_diff, " shared keys (prior_settlor sees loaded states only)\n", sep = "")
}

# ------------------------------------------------------------
# Optional: every column of a previous v2 file identical on every key (FAM_PREV_V2=<file name>)
# Per-state row counts and a hash over all of that file's columns, no join needed.
# ------------------------------------------------------------
prev_name <- Sys.getenv("FAM_PREV_V2", "")
if (nzchar(prev_name)) {
  prev_path <- path(data_dir, prev_name)
  stopifnot(file_exists(prev_path))
  prev <- glue("read_parquet('{sql_quote_path(prev_path)}')")
  prev_cols <- dbGetQuery(con, glue("DESCRIBE SELECT * FROM {prev}"))$column_name
  # FAM_PREV_EXCEPT=col1,col2: columns allowed to differ (hash excludes them; see the key-level check below)
  except_cols <- Sys.getenv("FAM_PREV_EXCEPT", "")
  except_cols <- if (nzchar(except_cols)) strsplit(except_cols, ",")[[1]] else character(0)
  prev_cols <- setdiff(prev_cols, except_cols)
  hash_prev <- paste(sprintf("coalesce(CAST(%s AS VARCHAR), '~')", prev_cols), collapse = ", ")
  hp <- function(src) dbGetQuery(con, glue("
    SELECT state, COUNT(*) AS n, SUM(hash({hash_prev})::HUGEINT) AS h FROM {src} GROUP BY state ORDER BY state"))
  hp_prev <- hp(prev); hp_new <- hp(new)
  cmp_prev <- inner_join(hp_prev, hp_new, by = "state", suffix = c("_prev", "_new"))
  add_check(paste0("all ", length(prev_cols), " columns of ", prev_name, " identical on every key (per-state hash)"),
            nrow(cmp_prev) == nrow(hp_prev) && nrow(cmp_prev) == nrow(hp_new) &&
              all(cmp_prev$n_prev == cmp_prev$n_new) && all(cmp_prev$h_prev == cmp_prev$h_new),
            paste(sum(cmp_prev$h_prev != cmp_prev$h_new), "states differ of", nrow(cmp_prev)))
  if (length(except_cols) > 0) {
    # the allowed differences must sit on rows that carried a given old value only
    only_old <- Sys.getenv("FAM_PREV_ONLY_ROWS", "")      # e.g. fam_relation_nm='cross_surname'
    chg <- paste(sprintf("a.%s IS DISTINCT FROM b.%s", except_cols, except_cols), collapse = " OR ")
    kd2 <- dbGetQuery(con, glue("
      SELECT COUNT(*) AS n_keys, SUM(({chg})::INT) AS n_changed,
             SUM((({chg}) AND NOT ({only_old}))::INT) AS n_changed_outside
      FROM {new} a JOIN {prev} b USING (clip, sale_raw)"))
    print(t(kd2))
    add_check(paste0(paste(except_cols, collapse = "/"), " differ only on rows where old ", only_old),
              kd2$n_changed_outside == 0, paste(kd2$n_changed, "changed;", kd2$n_changed_outside, "outside"))
    # transition matrix old -> new relation, national and California 2019
    tm <- dbGetQuery(con, glue("
      SELECT b.fam_relation_nm AS old_rel_nm, a.fam_relation_nm AS new_rel_nm,
             COUNT(*) AS n_all_years,
             SUM((a.sale_year = 2019)::INT) AS n_2019,
             SUM((a.sale_year = 2019 AND a.state = 'CA')::INT) AS n_2019_ca
      FROM {new} a JOIN {prev} b USING (clip, sale_raw)
      WHERE a.fam_relation_nm IS DISTINCT FROM b.fam_relation_nm
      GROUP BY ALL ORDER BY 3 DESC"))
    print(tm)
    write_csv_strict(tm, path(tables_out_dir, "validation_cross_surname_transition_v2.csv"))
  }
}

# ------------------------------------------------------------
# New columns: fill rates, no all-NULL column
# ------------------------------------------------------------
fill_sql <- paste(sprintf("AVG(CASE WHEN %s IS NOT NULL THEN 1.0 ELSE 0 END) AS %s", new_cols, new_cols), collapse = ", ")
fill <- dbGetQuery(con, glue("SELECT {fill_sql} FROM {new}"))
fill_long <- tibble(column = names(fill), fill_rate = as.numeric(unlist(fill)))
print(fill_long, n = 50)
add_check("no all-NULL new column", all(fill_long$fill_rate > 0), paste(sum(fill_long$fill_rate == 0), "all-NULL columns"))

fill_state <- dbGetQuery(con, glue("
  SELECT state, COUNT(*) AS n,
         AVG(CASE WHEN rel_code IS NOT NULL THEN 1.0 ELSE 0 END) AS rel_code,
         AVG(CASE WHEN own_code IS NOT NULL THEN 1.0 ELSE 0 END) AS own_code,
         AVG(CASE WHEN occ_code IS NOT NULL THEN 1.0 ELSE 0 END) AS occ_code,
         AVG(title_co) AS title_co, AVG(mail_eq_situs) AS mail_eq_situs,
         AVG(CASE WHEN given_differ IS NOT NULL THEN 1.0 ELSE 0 END) AS given_differ
  FROM {new} WHERE class LIKE 'family_%' GROUP BY state ORDER BY state"))
write_csv_strict(fill_state, path(tables_out_dir, paste0("audit_newcol_fill_by_state", sfx, ".csv")))

res <- bind_rows(checks)
write_csv_strict(res, path(tables_out_dir, paste0("reconcile_panel", sfx, ".csv")))
cat("Checks passed: ", sum(res$pass), " of ", nrow(res), "\n", sep = "")
if (!all(res$pass)) {
  cat("RECONCILIATION FAILED\n")
  if (!dev_mode) quit(status = 1)
}
cat("Finished at ", format(Sys.time()), "\n", sep = "")
