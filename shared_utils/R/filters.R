#' Common cleaners for CoreLogic data

suppressPackageStartupMessages({
  library(dplyr)
})

#' Valid US state / territory / APO codes
VALID_STATE_CODES <- c(
  # 50 states
  "AL","AK","AZ","AR","CA","CO","CT","DE","FL","GA","HI","ID","IL","IN","IA",
  "KS","KY","LA","ME","MD","MA","MI","MN","MS","MO","MT","NE","NV","NH","NJ",
  "NM","NY","NC","ND","OH","OK","OR","PA","RI","SC","SD","TN","TX","UT","VT",
  "VA","WA","WV","WI","WY",
  # DC + US territories
  "DC", "PR", "GU", "AS", "VI", "MP", "FM", "MH", "PW",
  # Military APO codes
  "AA", "AE", "AP"
)

#' Check if a state code is valid
#' @param code Character vector. State / territory / APO codes.
#' @return Logical vector.
is_valid_state_code <- function(code) {
  if (length(code) == 0) return(logical(0))
  vapply(code, function(x) {
    if (is.na(x)) return(FALSE)
    if (!is.character(x)) return(FALSE)
    if (nchar(x) != 2) return(FALSE)
    toupper(x) %in% VALID_STATE_CODES
  }, logical(1), USE.NAMES = FALSE)
}

#' Check whether a CoreLogic indicator flag is set
#'
#' The parquet store holds OT indicators as 0/1 doubles; older extracts and
#' test fixtures use "Y"/"N". NA counts as not set.
#' @param x Numeric, logical, or character flag vector.
#' @return Logical vector, never NA.
is_flag_set <- function(x) {
  if (is.logical(x)) return(!is.na(x) & x)
  if (is.numeric(x)) return(!is.na(x) & round(x) == 1)
  !is.na(x) & toupper(trimws(x)) %in% c("Y", "1", "1.0", "TRUE")
}

#' Filter to arms-length transactions
#'
#' Drops intra-family transfers, foreclosures (by default), and
#' zero/nominal-price sales. Tunable thresholds.
#'
#' Column names expected (post normalize_cols() in the conversion script):
#'   - sale_amount                          : numeric sale price
#'   - interfamily_related_indicator        : 0/1 (or "Y"/"N") intra-family flag
#'   - foreclosure_reo_indicator            : 0/1 (or "Y"/"N") foreclosure flag
#'   - foreclosure_reo_sale_indicator       : 0/1 (or "Y"/"N") REO sale flag
#'
#' Backward-compatible: also accepts legacy UPPER_SNAKE names
#'   (INTRA_FAMILY_FLAG, SALE_AMOUNT, TRANSACTION_TYPE) used in the
#'   synthetic test fixtures.
#'
#' @param df Data frame with CoreLogic OT columns
#' @param min_price Minimum sale price to keep (default 1000)
#' @param allow_foreclosure If TRUE, keep foreclosure transactions
#' @return Filtered tibble
filter_arms_length <- function(df, min_price = 1000, allow_foreclosure = FALSE) {
  out <- df
  cols <- names(out)

  # Drop intra-family
  if ("interfamily_related_indicator" %in% cols) {
    out <- out |> filter(!is_flag_set(interfamily_related_indicator))
  } else if ("INTRA_FAMILY_FLAG" %in% cols) {
    out <- out |> filter(is.na(INTRA_FAMILY_FLAG) | INTRA_FAMILY_FLAG != "Y")
  }

  # Drop zero/nominal-price
  if ("sale_amount" %in% cols) {
    out <- out |> filter(!is.na(sale_amount), sale_amount >= min_price)
  } else if ("SALE_AMOUNT" %in% cols) {
    out <- out |> filter(!is.na(SALE_AMOUNT), SALE_AMOUNT >= min_price)
  }

  # Drop foreclosures unless explicitly allowed
  if (!allow_foreclosure) {
    if ("foreclosure_reo_indicator" %in% cols) {
      out <- out |> filter(!is_flag_set(foreclosure_reo_indicator))
    } else if ("TRANSACTION_TYPE" %in% cols) {
      out <- out |> filter(is.na(TRANSACTION_TYPE) | TRANSACTION_TYPE != "FORECLOSURE")
    }
  }

  out
}
