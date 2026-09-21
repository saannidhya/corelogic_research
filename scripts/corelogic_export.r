#' Export Ohio CoreLogic Owner Transfer data to CSV
#'
#' Reads every year-partition parquet file under
#' data/corelogic_extracts/by_state/ot/state=OH/ into a single tibble
#' (via the shared loader, per the CoreLogic data protocol) and writes
#' the combined data to data/derived/ as one CSV.

suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(readr)
  library(fs)
  library(haven)
})

source(here("shared_utils", "R", "corelogic_loader.R"))

# states = "OH", years = NULL pulls every available year-partition and
# row-binds them into one tibble. per_partition = TRUE reads OH's own
# schema directly instead of unifying across all 50+ states' partitions,
# which otherwise fails on collect() due to cross-state type drift.
oh_ot <- load_corelogic_ot(states = "OH", years = NULL, per_partition = TRUE)

message("Loaded ", nrow(oh_ot), " rows spanning years ",
        paste(range(oh_ot$year), collapse = "-"))

# Raw-CSV parsing corruption occasionally produces a runaway field that
# swallows adjacent columns/rows into one pathologically long string
# (observed: a multi-hundred-MB `buyer_1_full_name` value). Such values
# aren't usable data, bloat exports, and break write_dta(); null them out
# and name the affected record so the source can be investigated/quarantined.
max_str_bytes <- 5000L
char_cols <- names(oh_ot)[sapply(oh_ot, is.character)]
for (cn in char_cols) {
  n_bytes <- nchar(oh_ot[[cn]], type = "bytes")
  bad <- which(!is.na(n_bytes) & n_bytes > max_str_bytes)
  if (length(bad) > 0) {
    message("WARNING: ", length(bad), " oversized value(s) in `", cn,
            "` (up to ", max(n_bytes[bad]), " bytes) -- nulling out. ",
            "Affected clip(s): ", paste(oh_ot$clip[bad], collapse = ", "),
            ". Likely raw-CSV parsing corruption; investigate source record(s).")
    oh_ot[[cn]][bad] <- NA_character_
  }
}

oh_ot %>% group_by(year) %>%
  summarise(
    total_properties = n(),
    prop = n()/nrow(.),
    .groups = "drop"
  ) %>%
  print(n = Inf)        

# View(oh_ot[1:1000,])

out_dir <- here("data", "derived")
dir_create(out_dir, recurse = TRUE)

out_path <- path(out_dir, "corelogic_ot_oh_all_years.csv")
write_csv(oh_ot, out_path)

# Stata variable names are capped at 32 characters. Truncate (uniquely)
# for the .dta export only; the original long name is kept as each
# variable's Stata label so it still shows up via `describe`.
truncate_stata_names <- function(names_in, max_len = 32) {
  truncated <- substr(names_in, 1, max_len)
  seen <- character(0)
  out <- character(length(truncated))
  for (i in seq_along(truncated)) {
    nm <- truncated[i]
    if (nm %in% seen) {
      k <- 2L
      repeat {
        suffix <- as.character(k)
        nm <- paste0(substr(truncated[i], 1, max_len - nchar(suffix)), suffix)
        if (!(nm %in% seen)) break
        k <- k + 1L
      }
    }
    out[i] <- nm
    seen <- c(seen, nm)
  }
  out
}

oh_ot_dta <- oh_ot
for (i in seq_along(oh_ot_dta)) {
  attr(oh_ot_dta[[i]], "label") <- names(oh_ot_dta)[i]
}
names(oh_ot_dta) <- truncate_stata_names(names(oh_ot_dta))

out_path_dta <- path(out_dir, "corelogic_ot_oh_all_years.dta")
haven::write_dta(oh_ot_dta, out_path_dta)

message("Wrote ", out_path)
message("Wrote ", out_path_dta)

