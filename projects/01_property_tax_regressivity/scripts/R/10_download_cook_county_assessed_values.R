source(here::here(
  "projects/01_property_tax_regressivity/scripts/R/00_setup.R"
))

suppressPackageStartupMessages({
  library(jsonlite)
})

#' Download Cook County assessor-assessed values from the Socrata API.
#'
#' The v3 query endpoint is the catalog URL supplied by the data publisher.
#' Socrata's resource endpoint is used here because it supports reliable
#' limit/offset pagination for a complete download.
download_assessed_values <- function(
    output_dir = external_data_dir,
    page_size = 50000L,
    max_rows = Inf,
    start_offset = 0L,
    max_attempts = 5L,
    timeout_seconds = 300L) {
  dir_create(output_dir)

  api_url <- paste0(
    "https://datacatalog.cookcountyil.gov/resource/uzyt-m557.json"
  )
  catalog_url <- paste0(
    "https://datacatalog.cookcountyil.gov/api/v3/views/uzyt-m557/query.json"
  )
  output_path <- path(output_dir, "assessor_assessed_values.csv")
  metadata_path <- path(output_dir, "metadata.json")

  old_timeout <- getOption("timeout")
  options(timeout = timeout_seconds)
  on.exit(options(timeout = old_timeout), add = TRUE)

  if (start_offset == 0L && file_exists(output_path)) {
    file_delete(output_path)
  }

  total_rows <- as.integer(start_offset)
  offset <- as.integer(start_offset)

  repeat {
    request_size <- as.integer(min(page_size, max_rows - total_rows))
    if (request_size <= 0L) {
      break
    }
    query_url <- paste0(
      api_url,
      "?%24limit=", format(request_size, scientific = FALSE),
      "&%24offset=", format(offset, scientific = FALSE),
      "&%24order=row_id"
    )
    temp_path <- tempfile(fileext = ".json")
    on.exit(unlink(temp_path), add = TRUE)
    downloaded <- FALSE
    for (attempt in seq_len(max_attempts)) {
      result <- try(
        download.file(query_url, temp_path, mode = "wb", quiet = TRUE),
        silent = TRUE
      )
      if (!inherits(result, "try-error")) {
        downloaded <- TRUE
        break
      }
      if (attempt < max_attempts) {
        Sys.sleep(2 ^ attempt)
      }
    }
    if (!downloaded) {
      stop(
        "Failed to download offset ", offset, " after ", max_attempts,
        " attempts.",
        call. = FALSE
      )
    }

    page <- fromJSON(temp_path, simplifyDataFrame = TRUE)
    if (length(page) == 0L) {
      break
    }
    if (!is.data.frame(page)) {
      stop("The Socrata API returned a non-tabular response.", call. = FALSE)
    }

    page <- as_tibble(page)
    if (start_offset == 0L && total_rows == nrow(page)) {
      write_csv(page, output_path, na = "")
    } else {
      write_csv(page, output_path, na = "", append = TRUE, col_names = FALSE)
    }
    total_rows <- total_rows + nrow(page)
    log_msg("Downloaded rows ", offset + 1L, " through ",
            offset + nrow(page))

    if (nrow(page) < request_size || total_rows >= max_rows) {
      break
    }
    offset <- as.integer(offset + request_size)
  }

  if (total_rows == 0L) {
    stop("The Socrata API returned no rows.", call. = FALSE)
  }

  columns <- names(page)

  metadata <- list(
    source_catalog_url = catalog_url,
    download_api_url = api_url,
    retrieved_at_utc = format(Sys.time(), tz = "UTC", usetz = TRUE),
    row_count = total_rows,
    column_count = length(columns),
    columns = columns,
    page_size = page_size
  )
  write_json(metadata, metadata_path, auto_unbox = TRUE, pretty = TRUE)

  invisible(list(
    output_path = output_path,
    metadata_path = metadata_path,
    row_count = total_rows
  ))
}

page_size_env <- as.integer(Sys.getenv("COOK_COUNTY_PAGE_SIZE", "50000"))
max_rows_env <- as.numeric(Sys.getenv("COOK_COUNTY_MAX_ROWS", "0"))
max_rows <- if (max_rows_env > 0) max_rows_env else Inf
output_dir_env <- Sys.getenv("COOK_COUNTY_OUTPUT_DIR", external_data_dir)
start_offset_env <- as.integer(Sys.getenv("COOK_COUNTY_START_OFFSET", "0"))
download_assessed_values(
  output_dir = output_dir_env,
  page_size = page_size_env,
  max_rows = max_rows,
  start_offset = start_offset_env
)
