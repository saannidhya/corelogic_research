#' Project setup — sourced at the top of every analysis script.
#'
#' This is the ONLY file in this project that touches paths.
#' All other scripts begin with:
#'   source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

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

# ---- shared utilities ----
source(here("shared_utils", "R", "corelogic_loader.R"))
source(here("shared_utils", "R", "data_dictionary.R"))
source(here("shared_utils", "R", "filters.R"))
source(here("shared_utils", "R", "theme_paper.R"))
theme_set(theme_paper())

# ---- project-scoped paths ----
project_dir    <- here("projects", "03_family_homes")
scripts_dir    <- path(project_dir, "scripts")
r_dir          <- path(scripts_dir, "R")
source(path(r_dir, "00_date_helpers.R"))
out_dir        <- path(r_dir, "_outputs")
tables_out_dir <- path(out_dir, "tables")
figures_out_dir <- path(out_dir, "figures")
logs_dir       <- path(out_dir, "logs")
manuscript_dir <- path(project_dir, "manuscript")
tables_dir     <- path(manuscript_dir, "tables")
figures_dir    <- path(manuscript_dir, "figures")
# FAM_EXHIBIT_DIR redirects manuscript tables/figures (refresh runs compared
# against the draft before it is updated); unset = write into the manuscript.
exhibit_root <- Sys.getenv("FAM_EXHIBIT_DIR", "")
if (nzchar(exhibit_root)) {
  tables_dir  <- path(exhibit_root, "tables")
  figures_dir <- path(exhibit_root, "figures")
}

# Project-specific derived data lives here (gitignored)
data_dir <- here("data", "derived", "03_family_homes")

dir_create(data_dir)
dir_create(out_dir)
dir_create(tables_out_dir)
dir_create(figures_out_dir)
dir_create(logs_dir)
dir_create(tables_dir)
dir_create(figures_dir)

# ---- duckdb-over-parquet helpers (pattern established in project 02) ----
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

open_corelogic_duckdb <- function(memory_limit = "12GB",
                                  threads = max(1L, parallel::detectCores() - 1L)) {
  con <- dbConnect(duckdb::duckdb(), dbdir = ":memory:")
  dbExecute(con, glue("PRAGMA memory_limit='{memory_limit}'"))
  dbExecute(con, glue("PRAGMA threads={threads}"))
  con
}

#' Assert a query result is non-degenerate before it is used downstream.
#' (MEMORY.md [LEARN:data]: a script that exits 0 is not proof the result is
#' real — guard against silent zero-row joins / empty subsamples.)
assert_rows <- function(df, min_rows, label) {
  if (!is.data.frame(df) || nrow(df) < min_rows) {
    stop("Degenerate result for ", label, ": ",
         ifelse(is.data.frame(df), nrow(df), NA), " rows (need >= ", min_rows, ")")
  }
  invisible(df)
}

#' Read events.parquet into R without the trust-rule audit columns
#'
#' The string columns trust_dir/trust_rel (01, 2026-10-31) add ~1 GB per copy
#' and exhausted R memory in scripts that hold the full panel in memory.
#' Query trust_rel through duckdb/arrow instead.
#' @param events_path Path to events.parquet.
#' @param drop Columns to skip.
#' @return Tibble of events.
read_events_df <- function(events_path,
                           drop = c("trust_dir", "trust_rel", "trust_prior_found")) {
  keep <- setdiff(names(arrow::open_dataset(events_path)$schema), drop)
  arrow::read_parquet(events_path, col_select = all_of(keep), as_data_frame = TRUE)
}

write_csv_strict <- function(x, file) {
  dir_create(path_dir(file))
  readr::write_csv(x, file, na = "")
  message("Wrote: ", file)
}

# ---- analysis window constants ----
window_start_year <- 2007L   # OT density begins 2007 (probe 2026-06-09)
window_end_year   <- 2024L   # data truncate ~Aug 2024; treat 2024 as partial
full_year_end     <- 2023L   # last complete calendar year
prop19_ballot_date    <- 20200624L  # Legislature placed Prop 19 on the ballot
prop19_pass_date      <- 20201103L  # Prop 19 passed (election day)
prop19_pc_effective   <- 20210216L  # parent-child exclusion changes effective

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

