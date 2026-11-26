# ============================================================
# 01e: Hand-coded labels for the seeded read of panel v2 deeds
# Name   : Saani Rawat
# Created: 11/11/2026
# Purpose: Stores the labels coded while reading read_sample_v2.csv (drawn by
#          01d STEP=sample). Labels only: no names enter this file or any output.
# Label  : 1 = the rule's label is right on the read; 0 = wrong; NA = the names
#          cannot settle it (e.g. a surname-only family trust, where the settlor is not named).
# Criteria per cell (what "right" means):
#   spouse_CA_IT            two natural persons with a shared surname (0: trust or entity party;
#                           NA: different surnames, which is consistent with spouses who keep names)
#   owner_retained_nm_fuzzy the seller is still on title in the buyer names (name variant, add or drop a co-owner)
#   parent_child_direct     two different natural persons sharing a surname (0: same person, typo, trust;
#                           NA: California IT deeds, which are spouse deeds by document type)
#   trust_to_other_person   trust to a surname-linked natural person (0: entity buyer or same person;
#                           NA: surname-only trust, settlor not named)
#   into_trust_for_others / into_trust_keyword
#                           the trust is named for a different individual or for children (0: named for the
#                           transferor, an entity, or a plan type; NA: surname-only or compound names)
#   trust_to_self_*         the buyer is the trust's named settlor (0: a different named person or an entity;
#                           NA: surname-only trust name)
#   cross_surname           two natural persons with genuinely different surnames (0: spelling variant, same
#                           person, shared surname, trust or entity)
#   trust_unlinked          no family link between the parties (0: an initial, nickname, name-order or
#                           spelling link the rule missed)
#   death_affidavit_trust   document type AF with a trust as the buyer
# Run    : Rscript 01e_read_labels_v2.R  (writes data/derived/03_family_homes/validation/read_labels_v2.csv)
# Log:
#   11/11/2026  Saani Rawat  first version, labels from the 11/11/2026 read
# ============================================================

source(here::here("projects/03_family_homes/scripts/R/00_setup.R"))

# n reads per cell, and the indices coded 0 / NA / 1 (every other index is 1 unless "ones" is given)
spec <- list(
  spouse_CA_IT = list(n = 100,
    zero = c(7, 19, 32, 37),
    na = c(4, 11, 13, 15, 17, 20, 30, 35, 36, 39, 41, 47, 49, 50, 53, 57, 59, 62, 64, 65, 69, 71,
           74, 75, 80, 83, 84, 87, 88, 94, 96, 98)),
  owner_retained_nm_fuzzy = list(n = 100,
    zero = c(12, 20, 22, 23, 29, 34, 38, 39, 48, 54, 73, 83, 92, 98)),
  parent_child_direct = list(n = 100,
    zero = c(15, 49, 77, 78, 89, 97), na = c(27, 34, 36, 56, 57, 62)),
  trust_to_other_person = list(n = 100,
    zero = c(1, 5, 24, 30, 49, 83, 56, 69, 75),
    na = c(3, 4, 8, 9, 10, 11, 12, 14, 15, 17, 18, 23, 25, 26, 27, 28, 31, 32, 33, 34, 35, 37, 38,
           42, 45, 46, 54, 57, 58, 62, 64, 65, 66, 70, 71, 72, 77, 79, 84, 85, 87, 89, 95, 96)),
  into_trust_for_others = list(n = 100,
    ones = c(1, 4, 16, 21, 26, 27, 40, 47, 48, 73, 76, 77, 82, 84, 92, 98),
    zero = c(6, 11, 19, 41, 44, 45, 52, 55, 60, 62, 67, 28, 34, 66, 57, 70, 36, 46, 81, 93, 20,
             50, 59, 94, 53)),
  into_trust_keyword = list(n = 100,
    ones = c(2, 4, 6, 34, 38, 43, 84, 95, 99, 14, 19, 41, 65),
    zero = c(3, 5, 9, 10, 13, 16, 24, 30, 31, 35, 37, 40, 45, 46, 48, 52, 53, 61, 63, 66, 67, 68,
             75, 80, 88, 91, 96, 23, 94)),
  trust_to_self_JF2021 = list(n = 50,
    ones = c(2, 6, 7, 8, 13, 17, 19, 20, 21, 22, 25, 26, 27, 30, 31, 33, 37, 39, 40, 41, 42, 43,
             44, 45, 46, 49),
    zero = 47),
  trust_to_self_2019 = list(n = 50,
    ones = c(1, 3, 6, 8, 12, 14, 15, 20, 21, 22, 23, 24, 25, 27, 28, 29, 32, 33, 35, 36, 37, 38,
             40, 41, 42, 43, 44, 45, 46, 48, 49, 50),
    zero = c(16, 30, 47)),
  cross_surname = list(n = 100,
    zero = c(2, 4, 5, 6, 7, 9, 10, 11, 13, 14, 15, 16, 17, 20, 21, 22, 23, 26, 27, 28, 29, 30, 32,
             33, 34, 35, 36, 37, 40, 41, 43, 44, 45, 47, 49, 50, 51, 53, 54, 56, 57, 59, 63, 64,
             65, 67, 69, 70, 72, 73, 74, 75, 76, 77, 79, 81, 82, 83, 86, 90, 93, 94, 95, 97, 98,
             99, 100)),
  trust_unlinked = list(n = 100,
    zero = c(5, 20, 25, 29, 31, 34, 38, 43, 47, 50, 60, 63, 64, 66, 68, 70, 75, 76, 77, 86, 87,
             89, 90, 91, 97, 98, 100)),
  death_affidavit_trust = list(n = 100)
)

labels <- purrr::imap_dfr(spec, function(s, cell) {
  idx <- seq_len(s$n)
  label <- if (!is.null(s$ones)) rep(NA_real_, s$n) else rep(1, s$n)
  if (!is.null(s$ones)) label[s$ones] <- 1
  label[s$zero] <- 0
  if (!is.null(s$na)) label[s$na] <- NA_real_
  tibble(cell = cell, idx = idx, label = label)
})

# join to the sample keys so a label can only attach to the deed it was coded for
sample_keys <- readr::read_csv(path(data_dir, "validation", "read_sample_v2.csv"),
                               col_select = c(cell, idx, clip, sale_raw), show_col_types = FALSE)
labels <- left_join(labels, sample_keys, by = c("cell", "idx"))
stopifnot(!anyNA(labels$clip), nrow(labels) == nrow(sample_keys))
readr::write_csv(labels, path(data_dir, "validation", "read_labels_v2.csv"), na = "")
cat("Wrote labels:", nrow(labels), "reads in", n_distinct(labels$cell), "cells\n")

# ============================================================
# Chain-verified trust-to-heir read (11/11/2026; samples from 01d STEP=sample_chain)
# chain_heir_cap10        right = a person A moved the parcel into a trust, and that trust later conveys to a
#                         different natural person B who shares A's surname; 0 = B is an entity, or B is
#                         the same person as A (name variant or generational suffix); NA = cannot tell
# name_rule_t2o_not_heir  right = the name rule's label holds on the names: the trust names an individual and
#                         B is a different natural person who shares the surname; 0 = entity buyer, B is the
#                         settlor, or no surname link; NA = surname-only family trust (settlor not named)
# ============================================================
chain_spec <- list(
  chain_heir_cap10 = list(n = 100, zero = c(2, 4, 21, 29, 30, 42, 47, 44, 62, 73, 96), na = 97),
  name_rule_t2o_not_heir = list(n = 50,
    ones = c(2, 3, 5, 7, 13, 15, 16, 17, 19, 21, 27, 28, 31, 32, 35, 38, 40, 41, 43, 45, 46, 48),
    zero = c(1, 8, 10, 14, 23, 26, 30, 36, 49, 50))
)
chain_labels <- purrr::imap_dfr(chain_spec, function(s, cell) {
  label <- if (!is.null(s$ones)) rep(NA_real_, s$n) else rep(1, s$n)
  if (!is.null(s$ones)) label[s$ones] <- 1
  label[s$zero] <- 0
  if (!is.null(s$na)) label[s$na] <- NA_real_
  tibble(cell = cell, idx = seq_len(s$n), label = label)
})
chain_keys <- readr::read_csv(path(data_dir, "validation", "read_sample_chain_v2.csv"),
                              col_select = c(cell, idx, clip, sale_raw), show_col_types = FALSE)
chain_labels <- left_join(chain_labels, chain_keys, by = c("cell", "idx"))
stopifnot(!anyNA(chain_labels$clip), nrow(chain_labels) == nrow(chain_keys))
readr::write_csv(chain_labels, path(data_dir, "validation", "read_labels_chain_v2.csv"), na = "")
cat("Wrote chain labels:", nrow(chain_labels), "reads\n")
