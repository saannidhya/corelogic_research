#======================================================================#
# Purpose : Project setup, sourced at the top of every analysis script:
#             source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))
#           The only file in this project that touches paths. It also holds
#           the events-panel reader, the family measures and the Prop 19 dates.
# Name    : Saani Rawat
# Created : 06/09/2026
# Log     : 11/04/2026: family definitions; exhibit redirect (FAM_EXHIBIT_DIR).
#           11/12/2026: main family measure without same-person and entity
#                       deeds; Section 4 measure; read_events_df(measure = ).
#           11/16/2026: panel file is events.parquet (no _v2 suffix); tidied.
#                       Family measure locked on deed_cat = 'family_transfer'
#                       (direct deeds, three trust tiers, every affidavit of death).
# Settings: FAM_EXHIBIT_DIR  write manuscript tables/figures somewhere else
#           FAM_DUCKDB_TEMP  duckdb spill directory
#           FAM_DUCKDB_MEM   default duckdb memory cap (default 12GB)
#======================================================================#

suppressPackageStartupMessages({
  library(here)
  library(tidyverse)
  library(arrow)
  library(duckdb)
  library(DBI)
  library(fs)
  library(glue)
  library(scales)
})

#============================================#
# Shared utilities ----
#============================================#

source(here("shared_utils", "R", "corelogic_loader.R"))
source(here("shared_utils", "R", "data_dictionary.R"))
source(here("shared_utils", "R", "filters.R"))
source(here("shared_utils", "R", "theme_paper.R"))
theme_set(theme_paper())

#============================================#
# Paths ----
#============================================#

# |- scripts and their outputs ----
project_dir     <- here("projects", "03_family_homes")
scripts_dir     <- path(project_dir, "scripts")
r_dir           <- path(scripts_dir, "R")
out_dir         <- path(r_dir, "_outputs")
tables_out_dir  <- path(out_dir, "tables")
figures_out_dir <- path(out_dir, "figures")
logs_dir        <- path(out_dir, "logs")
source(path(r_dir, "00_date_helpers.R"))

# |- manuscript exhibits ----
# FAM_EXHIBIT_DIR redirects manuscript tables/figures (refresh runs compared
# against the draft before it is updated); unset = write into the manuscript.
manuscript_dir <- path(project_dir, "manuscript")
tables_dir     <- path(manuscript_dir, "tables")
figures_dir    <- path(manuscript_dir, "figures")
exhibit_root   <- Sys.getenv("FAM_EXHIBIT_DIR", "")
if (nzchar(exhibit_root)) {
  tables_dir  <- path(exhibit_root, "tables")
  figures_dir <- path(exhibit_root, "figures")
}

# |- derived data (gitignored) ----
data_dir <- here("data", "derived", "03_family_homes")

dir_create(c(data_dir, out_dir, tables_out_dir, figures_out_dir, logs_dir,
             tables_dir, figures_dir))

#============================================#
# DuckDB and small helpers ----
#============================================#

# |- CoreLogic parquet store ----
ot_parquet_glob <- function() {
  root <- normalizePath(here("data", "corelogic_extracts", "by_state", "ot"),
                        winslash = "/", mustWork = TRUE)
  gsub("\\\\", "/", file.path(root, "state=*", "year=*", "*.parquet"))
}

prop_parquet_glob <- function() {
  root <- normalizePath(here("data", "corelogic_extracts", "by_state", "prop"),
                        winslash = "/", mustWork = TRUE)
  gsub("\\\\", "/", file.path(root, "state=*", "*.parquet"))
}

sql_quote_path <- function(x) {
  gsub("'", "''", x, fixed = TRUE)
}

# |- connection ----
# FAM_DUCKDB_TEMP moves the spill directory to a local disk outside OneDrive;
# FAM_DUCKDB_MEM sets the default memory cap. Neither changes a result.
duckdb_temp_dir <- Sys.getenv("FAM_DUCKDB_TEMP", unset = as.character(path(data_dir, "duckdb_tmp")))
open_corelogic_duckdb <- function(memory_limit = Sys.getenv("FAM_DUCKDB_MEM", unset = "12GB"),
                                  threads = max(1L, parallel::detectCores() - 1L)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dbExecute(con, glue("PRAGMA memory_limit='{memory_limit}'"))
  dir_create(duckdb_temp_dir)
  dbExecute(con, glue("PRAGMA temp_directory='{sql_quote_path(duckdb_temp_dir)}'"))
  dbExecute(con, glue("PRAGMA threads={threads}"))
  con
}

# |- guards and writers ----
#' Assert a query result is non-degenerate before it is used downstream.
#' A script that exits 0 is not proof the result is real: this guards against
#' silent zero-row joins and empty subsamples.
assert_rows <- function(df, min_rows, label) {
  if (!is.data.frame(df) || nrow(df) < min_rows) {
    stop("Degenerate result for ", label, ": ",
         ifelse(is.data.frame(df), nrow(df), NA), " rows (need >= ", min_rows, ")")
  }
  invisible(df)
}

write_csv_strict <- function(x, file) {
  dir_create(path_dir(file))
  readr::write_csv(x, file, na = "")
  message("Wrote: ", file)
}

#============================================#
# Events panel ----
#============================================#

# The deed-event panel written by 01_build_panel.R. Every script reads it through
# events_panel_path (or the helpers below), never by file name.
events_panel_file <- "events.parquet"
events_panel_path <- path(data_dir, events_panel_file)

#' Read the events panel into R without the audit / taxonomy string columns
#'
#' The string columns (trust_dir, trust_rel, doc_type, doc_grp, rel_code, own_code,
#' occ_code, fam_relation, fam_relation_nm, fam_trigger, fam_vehicle) add about
#' 1 GB per copy and exhausted R memory in scripts that hold the full panel, so
#' they are dropped by default. Query them through duckdb/arrow, or request them
#' with `columns =`: an explicit `columns` overrides `drop`.
#' @param events_path Path to the events panel (default: events_panel_path).
#' @param drop Columns to skip when `columns` is NULL.
#' @param columns Optional required column names; select before reading into R.
#' @param measure Which family measure the returned `class` column carries.
#'   "main" (default): same-person and entity deeds of the class-based measure are
#'   relabelled 'family_samep_entity', so `class %in% family_classes` is the DIRECT
#'   component of the family measure and its upper bound is that plus
#'   'family_samep_entity'. For the full measure filter on deed_cat instead.
#'   "s4": rows in the Section 4 measure are 'family_main', other rows of the
#'   class-based measure are 'family_samep_entity' (legacy Prop 19 scripts).
#'   "old": panel `class` untouched.
#' @return Tibble of events.
read_events_df <- function(events_path = events_panel_path,
                           drop = c("trust_dir", "trust_rel", "trust_prior_found",
                                    "doc_type", "doc_grp", "rel_code", "own_code",
                                    "occ_code", "fam_relation", "fam_relation_nm",
                                    "fam_trigger", "fam_vehicle"),
                           columns = NULL,
                           measure = c("main", "s4", "old")) {
  measure <- match.arg(measure)
  available <- names(arrow::open_dataset(events_path)$schema)
  if (!is.null(columns)) stopifnot(all(columns %in% available))
  keep <- if (is.null(columns)) setdiff(available, drop) else columns
  if (measure == "old" || !"class" %in% keep) {
    return(arrow::read_parquet(events_path, col_select = all_of(keep), as_data_frame = TRUE))
  }
  cls <- if (measure == "main") family_class_main_sql else family_class_s4_sql()
  sel <- paste(ifelse(keep == "class", paste0(cls, " AS class"), paste0('"', keep, '"')),
               collapse = ", ")
  con <- open_corelogic_duckdb()
  on.exit(dbDisconnect(con, shutdown = TRUE), add = TRUE)
  # arrow = TRUE keeps the column types of arrow::read_parquet (int64 clip as integer64)
  out <- dbGetQuery(con, glue("SELECT {sel} FROM read_parquet('{sql_quote_path(events_path)}')"),
                    arrow = TRUE)
  tibble::as_tibble(out)
}

#============================================#
# Family measures ----
#============================================#
# All are operational proxies: the vendor's interfamily flag does not verify kinship.

# |- the family measure (locked 11/16/2026) ----
# A family transfer is a recorded deed that moves a home, with no sale, from one person (or
# that person's trust) to a different person, during life or at death. The panel carries it
# as deed_cat = 'family_transfer'; 01_build_panel.R (section 6) holds the rules.
#   fam_route  lifetime | death_affidavit | probate | unknown   direct deeds
#              trust                                             title leaves a trust for
#                                                                someone other than its settlor
# Affidavits of death with a trust party (deed_subcat = 'affidavit_trust_party') are in the
# measure with fam_route = 'death_affidavit'; they are neither direct deeds nor the trust route.
family_main_sql      <- "(deed_cat = 'family_transfer')"
family_direct_sql    <- paste0("(deed_cat = 'family_transfer' AND fam_route <> 'trust' ",
                               "AND deed_subcat <> 'affidavit_trust_party')")
family_trust_sql     <- "(deed_cat = 'family_transfer' AND fam_route = 'trust')"
family_affidavit_sql <- "(deed_cat = 'family_transfer' AND fam_route = 'death_affidavit')"

# |- upper bounds ----
# Direct deeds: add back the flagged deeds with the same owner or an entity on both sides,
# which is the class-based measure below. Trust deeds: add the two cells the names cannot
# settle (a surname-only trust; an unpriced deed to a person with another surname).
family_narrow_classes <- c("family_person", "family_estate")
family_classes        <- c(family_narrow_classes, "family_other")
family_classes_sql    <- paste0("('", paste(family_classes, collapse = "','"), "')")
family_trust_upper_subcats <- c("trust_surname_only", "trust_to_unlinked_unpriced")
family_trust_upper_sql <- paste0("(deed_subcat IN ('",
                                 paste(family_trust_upper_subcats, collapse = "','"), "'))")

# |- the measure through `class`, for scripts that count classes ----
# read_events_df(measure = "main") and events_main_from() relabel `class` so that
# `class %in% family_measure_classes` IS the family measure:
#   family_person / family_estate / family_other   direct deeds in the measure
#   family_via_trust       family transfers whose class is family_trust: the three trust
#                          tiers and the affidavits of death with a trust party
#   family_samep_entity    same-owner and entity deeds of the class-based measure (out)
#   family_trust           the trust deeds that are not family transfers (out)
family_via_trust_label <- "family_via_trust"
family_samep_label     <- "family_samep_entity"
family_measure_classes <- c(family_classes, family_via_trust_label)
family_measure_classes_sql <- paste0("('", paste(family_measure_classes, collapse = "','"), "')")
family_class_main_sql <- paste0(
  "CASE WHEN deed_cat = 'family_transfer' AND class = 'family_trust' THEN '", family_via_trust_label, "'",
  " WHEN class IN ", family_classes_sql, " AND deed_cat <> 'family_transfer' THEN '", family_samep_label, "'",
  " ELSE class END")

# In-memory version for a data frame that carries `class` and `fam_relation_nm` but not
# deed_cat: it relabels the direct deeds only (04a_build_wedge.R).
family_excluded_relations <- c("owner_retained", "unrelated_entity")
family_excluded_sql <- paste0("('", paste(family_excluded_relations, collapse = "','"), "')")

#' Same relabel on an in-memory data frame that carries `class` and `fam_relation_nm`.
recode_family_main <- function(df) {
  df$class <- ifelse(df$class %in% family_classes &
                       coalesce(df$fam_relation_nm, "") %in% family_excluded_relations,
                     family_samep_label, df$class)
  df
}

#' SQL FROM-subquery over the events panel with the relabelled `class` (direct component).
events_main_from <- function(events_path = events_panel_path) {
  paste0("(SELECT * REPLACE (", family_class_main_sql, " AS class) FROM read_parquet('",
         sql_quote_path(events_path), "'))")
}

# |- Section 4 measure (bygroup_main, defined in 00_groups.R) ----
# The legacy Prop 19 scripts use it so the appendix synthetic control and event study
# match Section 4.
# 11/16/2026: the Proposition 19 designs (04, 07, 09, 09b, 10, 14) use the locked family
# measure itself. With every affidavit of death counted the series is continuous across the
# 2018 vendor change, so no narrower Section 4 measure is needed. The by-group scripts
# (16-28) still take their groups from 00_groups.R.
family_s4_sql <- function() family_main_sql

# |- Proposition 19 triple difference: years compared ----
# 11/18/2026: baseline moved from 2017-2019 to 2018-2019, the baseline of the monthly event
# study. 2020 and 2021 (anticipation and the deadline) stay out.
prop19_pre_years  <- 2018:2019
prop19_post_years <- 2022:2023
prop19_did_years  <- c(prop19_pre_years, prop19_post_years)
prop19_pre_label  <- paste0(min(prop19_pre_years), "--", max(prop19_pre_years))
prop19_post_label <- paste0(min(prop19_post_years), "--", max(prop19_post_years))

family_class_s4_sql <- function() {
  paste0("CASE WHEN coalesce(", family_s4_sql(), ", FALSE) THEN 'family_main' WHEN class IN ",
         family_classes_sql, " THEN '", family_samep_label, "' ELSE class END")
}

events_s4_from <- function(events_path = events_panel_path) {
  paste0("(SELECT * REPLACE (", family_class_s4_sql(), " AS class) FROM read_parquet('",
         sql_quote_path(events_path), "'))")
}

# |- deed groups ----
source(path(r_dir, "00_groups.R"))

#============================================#
# Analysis window and Prop 19 dates ----
#============================================#

window_start_year <- 2007L   # Owner Transfer coverage is dense from 2007
window_end_year   <- 2024L   # data stop around August 2024; 2024 is a partial year
full_year_end     <- 2023L   # last complete calendar year

prop19_ballot_date  <- 20200624L  # Legislature placed Prop 19 on the ballot
prop19_pass_date    <- 20201103L  # Prop 19 passed (election day)
prop19_pc_effective <- 20210216L  # parent-child exclusion changes effective

# Anticipation starts at ballot placement (decided 10/22/2026); every Prop 19
# design fits its pre-period through May 2020 and treats June 2020 onward.
prop19_antic_start_ym <- 202006L    # first anticipation month (ballot placement)
prop19_antic_end_ym   <- 202102L    # deadline month
prop19_pre_end_ym     <- 202005L    # last clean pre-period month

prop19_event_dates <- as.Date(c(ballot    = "2020-06-24",
                                passage   = "2020-11-03",
                                effective = "2021-02-16"))
prop19_event_labels <- c(ballot    = "Placed on ballot\nJun 24, 2020",
                         passage   = "Passed by voters\nNov 3, 2020",
                         effective = "Parent-child rules\neffective Feb 16, 2021")

#' Prop 19 event lines and labels for monthly date-axis figures
#'
#' Ballot placement and the effective date are dashed; voter passage is dotted.
#' Labels sit left of the ballot and passage lines (passage one row lower) and
#' right of the effective-date line, so the three never overlap.
#' @param label_y y position of the top label row, in data units.
#' @param label_step vertical drop of the passage label below label_y.
#' @param show_labels FALSE draws the lines only.
#' @return List of ggplot layers.
prop19_event_layers <- function(label_y, label_step = 0, show_labels = TRUE,
                                label_size = 2.6) {
  event_lines <- tibble(
    event = names(prop19_event_dates),
    date = unname(prop19_event_dates),
    label = unname(prop19_event_labels[names(prop19_event_dates)]),
    line_type = if_else(event == "passage", "dotted", "dashed"),
    label_x = date + if_else(event == "effective", 14, -14),
    label_y = if_else(event == "passage", label_y - label_step, label_y),
    label_hjust = if_else(event == "effective", 0, 1)
  )
  layers <- list(
    geom_vline(data = event_lines, aes(xintercept = date, linetype = I(line_type)),
               color = "grey35", linewidth = 0.45)
  )
  if (show_labels) {
    layers <- c(layers, list(
      # white backing keeps a label legible where it crosses a neighbouring line
      geom_label(data = event_lines, inherit.aes = FALSE,
                 aes(x = label_x, y = label_y, label = label, hjust = label_hjust),
                 vjust = 1, size = label_size, color = "grey25", lineheight = 0.9,
                 fill = "white", border.colour = NA,
                 label.padding = unit(0.1, "lines"))
    ))
  }
  layers
}
