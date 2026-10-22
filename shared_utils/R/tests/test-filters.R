library(testthat)
library(here)
library(dplyr)

source(here("shared_utils/R/filters.R"))

make_ot_sample <- function() {
  tibble::tribble(
    ~APN,    ~SELLER_NAME,         ~BUYER_NAME,           ~SALE_AMOUNT, ~TRANSACTION_TYPE, ~INTRA_FAMILY_FLAG,
    "A001",  "SMITH JOHN",         "DOE JANE",            250000,       "RESALE",          "N",
    "A002",  "JOHNSON BANK NA",    "FORECLOSURE TRUST",   180000,       "FORECLOSURE",     "N",
    "A003",  "SMITH ROBERT",       "SMITH MARY",          1,            "RESALE",          "Y",
    "A004",  "ABC LLC",            "XYZ LLC",             500000,       "RESALE",          "N",
    "A005",  NA,                   "DOE JANE",            300000,       "RESALE",          "N"
  )
}

test_that("filter_arms_length drops intra-family transfers", {
  result <- filter_arms_length(make_ot_sample())
  expect_false("A003" %in% result$APN)
})

test_that("filter_arms_length drops zero/nominal-price sales", {
  result <- filter_arms_length(make_ot_sample(), min_price = 100)
  expect_false("A003" %in% result$APN)
})

test_that("filter_arms_length keeps a clean resale", {
  result <- filter_arms_length(make_ot_sample())
  expect_true("A001" %in% result$APN)
})

test_that("filter_arms_length drops foreclosures by default", {
  result <- filter_arms_length(make_ot_sample())
  expect_false("A002" %in% result$APN)
})

test_that("filter_arms_length keeps foreclosures when allow_foreclosure = TRUE", {
  result <- filter_arms_length(make_ot_sample(), allow_foreclosure = TRUE)
  expect_true("A002" %in% result$APN)
})

test_that("filter_arms_length drops 0/1 numeric flags as stored in the parquet store", {
  ot <- tibble::tibble(
    APN = c("B001", "B002", "B003", "B004"),
    sale_amount = c(250000, 250000, 250000, 250000),
    interfamily_related_indicator = c(0, 1, NA, 0),
    foreclosure_reo_indicator = c(0, 0, 0, 1)
  )
  expect_equal(filter_arms_length(ot)$APN, c("B001", "B003"))
  expect_equal(filter_arms_length(ot, allow_foreclosure = TRUE)$APN, c("B001", "B003", "B004"))
})

test_that("validate_state_code accepts 50 states + DC + valid territories + APO", {
  expect_true(is_valid_state_code("CA"))
  expect_true(is_valid_state_code("OH"))
  expect_true(is_valid_state_code("DC"))
  expect_true(is_valid_state_code("PR"))
  expect_true(is_valid_state_code("GU"))
  expect_true(is_valid_state_code("AE"))  # military APO
})

test_that("validate_state_code rejects malformed codes", {
  expect_false(is_valid_state_code("XX"))
  expect_false(is_valid_state_code("12011"))
  expect_false(is_valid_state_code("63.94"))
  expect_false(is_valid_state_code("A"))
  expect_false(is_valid_state_code(""))
  expect_false(is_valid_state_code(NA))
})
