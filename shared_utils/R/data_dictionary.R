#' CoreLogic Data Dictionary parser
#'
#' Parses a CoreLogic dd.txt file into a tibble for use by the loader and by
#' analysis code (`describe_col()`).
#'
#' Expected dd.txt format: tab-separated with header row containing
#' FIELD, TYPE, START, END, DESCRIPTION columns. Tolerant of variations
#' in column order; matches by header name (case-insensitive).

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
})

#' Parse a CoreLogic data dictionary file
#'
#' @param dd_path Path to dd.txt (tab-separated)
#' @return Tibble with columns: name, type, start_pos, end_pos, description
#' @export
parse_data_dictionary <- function(dd_path) {
  stopifnot(file.exists(dd_path))
  raw <- read_tsv(dd_path, show_col_types = FALSE, progress = FALSE)
  # Normalize column names: lowercase, map to standard names
  names(raw) <- tolower(names(raw))
  required <- c("field", "type", "start", "end", "description")
  missing_cols <- setdiff(required, names(raw))
  if (length(missing_cols) > 0) {
    stop("dd.txt missing required columns: ", paste(missing_cols, collapse = ", "))
  }
  tibble(
    name = raw$field,
    type = raw$type,
    start_pos = as.integer(raw$start),
    end_pos = as.integer(raw$end),
    description = raw$description
  )
}

#' Look up the description for a CoreLogic column
#'
#' @param col_name Column name (matched against dd$name)
#' @param dd Data dictionary tibble from `parse_data_dictionary()`
#' @return Description string
#' @export
describe_col <- function(col_name, dd) {
  row <- dd[dd$name == col_name, ]
  if (nrow(row) == 0) {
    stop("Column '", col_name, "' not found in data dictionary")
  }
  row$description[1]
}

# ---- sale_document_type_code lookup ----------------------------------------
#
# CoreLogic's code table for Owner Transfer `sale_document_type_code` is not
# public. Meanings were assembled from CoreLogic's public deed-type vocabulary
# (RiPT / HistoryPro code pages) and validated against the full OT store
# (state concentration, deed category, price, interfamily/REO flags, and
# seller/buyer names). See the `confidence` and `evidence` columns.
# NULL codes are represented by the sentinel "(missing)".

#' Path to the curated code meanings (committed; contains no CoreLogic data)
sale_document_type_meanings_path <- function() {
  here::here("shared_utils", "R", "reference", "sale_document_type_meanings.csv")
}

#' Path to the built lookup with counts (gitignored; aggregates of licensed data)
sale_document_type_codes_path <- function() {
  here::here("data", "corelogic_extracts", "_reference", "sale_document_type_codes.csv")
}

#' Read the curated sale_document_type_code meanings
#'
#' @param path Path to the meanings CSV (overridable for tests)
#' @return Tibble with columns: code, meaning, group, confidence, evidence
#' @export
read_sale_document_type_meanings <- function(path = sale_document_type_meanings_path()) {
  stopifnot(file.exists(path))
  # na = character(): keep codes such as "NA"-like strings literal
  out <- read_csv(path, col_types = cols(.default = col_character()),
                  na = character(), progress = FALSE)
  if (anyDuplicated(out$code)) {
    stop("Duplicate codes in ", path, ": ",
         paste(unique(out$code[duplicated(out$code)]), collapse = ", "))
  }
  out
}

#' Sale document type lookup, with counts and proportions when available
#'
#' Returns the built table (counts, shares, profile proportions) if
#' `shared_utils/R/build_sale_document_type_codes.R` has been run;
#' otherwise falls back to the curated meanings only.
#'
#' @param with_counts If TRUE (default), use the built table when present.
#' @return Tibble, one row per code.
#' @export
sale_document_type_codes <- function(with_counts = TRUE) {
  built <- sale_document_type_codes_path()
  if (with_counts && file.exists(built)) {
    return(read_csv(built, na = "", show_col_types = FALSE, progress = FALSE,
                    col_types = cols(code = col_character(), .default = col_guess())))
  }
  if (with_counts) {
    message("Built lookup not found; returning meanings only. Run ",
            "shared_utils/R/build_sale_document_type_codes.R to add counts.")
  }
  read_sale_document_type_meanings()
}

#' Describe sale_document_type_code values
#'
#' @param code Character vector of codes; NA maps to "(missing)".
#' @param lookup Tibble from `read_sale_document_type_meanings()`.
#' @return Character vector of meanings (NA for codes not in the lookup).
#' @export
describe_sale_document_type <- function(code, lookup = read_sale_document_type_meanings()) {
  key <- ifelse(is.na(code), "(missing)", code)
  unname(setNames(lookup$meaning, lookup$code)[key])
}
