#' Deed-group list for the by-group re-runs (Phase 3) -- defined ONCE here.
#'
#' Sourced by 00_setup.R (one line, next to `family_classes`) once the panel
#' worker releases it; until then scripts may source this file directly.
#' A second sourcing is harmless (guard below).
#'
#' Each group is a SQL filter on events.parquet columns, so the same string
#' runs in duckdb on the panel and on the state-month count cube.
#'
#' WHY cross-state groups use fam_relation_nm, fam_trigger and fam_vehicle:
#' the California triple difference compares CA with the other 50 states + DC,
#' so a group must be defined identically in every state. Spouse status is
#' identified only by document type IT/IC (CA) and DS (AZ), so fam_relation
#' (which has a 'spouse' value) is used only by the two scope = "ca_only"
#' descriptive series. fam_relation_nm is fam_relation without the
#' document-type spouse rule and works from names alone in every state.
#'
#' Columns: name, label, filter_sql, scope ("cross" | "ca_only"), role,
#' pre_years (list; base years of the annual designs), es_ref_from (first month
#' of the pooled event-study reference, through 201912).

if (!exists("bygroup_defs", inherits = FALSE)) {

  # ---- building blocks ----
  bg_main <- "class IN ('family_person','family_estate','family_other')"
  if (exists("family_classes", inherits = TRUE)) {
    stopifnot(identical(bg_main, paste0("class IN ('",
                paste(family_classes, collapse = "','"), "')")))
  }
  # Death affidavits (AF) and AJ: the 2018 California vendor break moves AF
  # deeds with a trust buyer out of the main proxy (fam_vehicle =
  # 'death_affidavit_trust'), so variants run with all AF out and all AF in.
  bg_af   <- "coalesce(doc_type,'') IN ('AF','AJ')"
  bg_tlo  <- "(trust_fam_lo = 1 AND coalesce(fam_vehicle,'') <> 'death_affidavit_trust')"
  bg_thi  <- "(trust_fam_hi = 1 AND coalesce(fam_vehicle,'') <> 'death_affidavit_trust')"
  bg_noaf <- paste0("(", bg_main, " AND NOT ", bg_af, ")")
  # Chain-verified trust-to-heir deeds (panel rebuilt 11/11 15:46): an earlier non-market deed
  # moved the home from person A into a trust with no market sale in between, and the trust then
  # deeded it to B (same surname, different person). 10-year look-back = 3652 days; the look-back
  # is complete only for deeds from 2017 on, so designs restrict these groups to 2017 onward.
  bg_heir <- paste0("coalesce(trust_chain = 'heir' AND trust_chain_gap_days <= 3652 AND ",
                    "coalesce(fam_vehicle,'') <> 'death_affidavit_trust', FALSE)")
  # Distinct-people family measure (author's decision, 11/12): the proxy without death affidavits,
  # without same-person (owner_retained) and entity (unrelated_entity) deeds, plus chain-verified
  # trust-to-heir deeds. Chain heirs are kept whatever their name-rule relation, because the chain
  # itself (an earlier deed moved the home from person A into a trust; the trust then deeded it to
  # B, same surname, different person) establishes two distinct people. Spouse deeds stay.
  bg_distinct <- paste0("((", bg_noaf, " AND coalesce(fam_relation_nm, '') NOT IN ('owner_retained', 'unrelated_entity')) OR ",
                        bg_heir, ")")
  bg_veh  <- function(v) paste0("fam_vehicle = '", v, "'")

  bg_row <- function(name, label, filter_sql, scope, role,
                     pre_years = 2017:2019, es_ref_from = 201801L) {
    tibble::tibble(name = name, label = label, filter_sql = filter_sql,
                   scope = scope, role = role,
                   pre_years = list(as.integer(pre_years)),
                   es_ref_from = as.integer(es_ref_from))
  }

  bygroup_defs <- dplyr::bind_rows(
    bg_row("market_sale", "Market sales (comparator)", "class = 'market_sale'",
           "cross", "denominator"),
    bg_row("main_proxy", "Main proxy (legacy headline)", bg_main,
           "cross", "exposed (legacy; contains the 2018 break)"),
    bg_row("main_noaf", "Main proxy, all death affidavits out (variant A)", bg_noaf,
           "cross", "exposed (headline candidate)"),
    bg_row("main_allaf", "Main proxy, all death affidavits in (variant B)",
           paste0("(", bg_main, " OR ", bg_veh("death_affidavit_trust"), ")"),
           "cross", "exposed (headline candidate)",
           pre_years = 2019L, es_ref_from = 201806L),
    bg_row("lifetime_pc", "Direct lifetime, parent-child candidate",
           paste0(bg_veh("direct"), " AND fam_trigger = 'lifetime'",
                  " AND fam_relation_nm = 'parent_child_candidate'"),
           "cross", "exposed"),
    bg_row("lifetime_other", "Direct lifetime, other relations (includes WEAK cross_surname cell, 33% correct)",
           paste0(bg_veh("direct"), " AND fam_trigger = 'lifetime'",
                  " AND fam_relation_nm IN ('cross_surname','unknown','unrelated_entity')"),
           "cross", "reported only"),
    bg_row("owner_retained", "Owner retained (same person retitled): outside the measure",
           paste0(bg_veh("direct"), " AND fam_relation_nm = 'owner_retained'"),
           "cross", "outside the measure (same-person retitles, mixed)"),
    bg_row("death_direct_noaf", "Death deeds without trust party, no affidavits",
           paste0(bg_veh("direct"), " AND fam_trigger = 'death' AND NOT ", bg_af),
           "cross", "comparison (cannot be timed)"),
    bg_row("death_affidavit_all", "All affidavits of death, direct and trust-party",
           paste0(bg_af, " AND fam_vehicle IN ('direct','death_affidavit_trust')"),
           "cross", "comparison (cannot be timed)",
           pre_years = 2019L, es_ref_from = 201806L),
    bg_row("trust_to_other", "Trust to a different person", bg_veh("trust_to_other_person"),
           "cross", "exposed"),
    bg_row("into_trust_others", "Person into a trust for others (WEAK: 39% correct in seeded read)",
           bg_veh("into_trust_for_others"), "cross", "exposed"),
    bg_row("trust_to_self", "Trust to the settlor", bg_veh("trust_to_self"),
           "cross", "refinance placebo"),
    bg_row("into_own_trust", "Person into own trust", bg_veh("into_own_trust"),
           "cross", "refinance placebo"),
    bg_row("trust_to_trust", "Trust to trust", bg_veh("trust_to_trust"),
           "cross", "reported only"),
    bg_row("trust_unlinked", "Trust deeds, no surname link", bg_veh("trust_unlinked"),
           "cross", "reported only"),
    bg_row("trust_lo", "Trust family transfers, lower bound", bg_tlo,
           "cross", "exposed"),
    bg_row("trust_hi", "Trust family transfers, upper bound", bg_thi,
           "cross", "exposed"),
    bg_row("main_noaf_trust_lo", "Variant A plus trust lower bound",
           paste0("(", bg_noaf, " OR ", bg_tlo, ")"),
           "cross", "exposed (candidate main measure)"),
    bg_row("main_noaf_trust_hi", "Variant A plus trust upper bound",
           paste0("(", bg_noaf, " OR ", bg_thi, ")"),
           "cross", "exposed (candidate main measure)"),
    bg_row("main_noaf_trust_t2p", "Variant A plus trust to other person (reliable trust cell)",
           paste0(bg_noaf, " OR ", bg_veh("trust_to_other_person")),
           "cross", "exposed"),
    bg_row("trust_heir_chain", "Trust-to-heir deeds, chain-verified (10-year look-back)",
           bg_heir, "cross", "exposed"),
    bg_row("main_noaf_trust_heir", "Including same-person and entity deeds (previous main measure): variant A plus chain-verified trust-to-heir",
           paste0("(", bg_noaf, " OR ", bg_heir, ")"),
           "cross", "exposed (sensitivity: includes same-person and entity deeds)"),
    bg_row("main_noaf_trust_heir_hi", "Upper bound: MAIN measure plus all trust to other person",
           paste0("(", bg_noaf, " OR ", bg_heir, " OR ", bg_veh("trust_to_other_person"), ")"),
           "cross", "exposed (upper bound of the MAIN measure)"),
    bg_row("main_noaf_trust_heir_xcs", "MAIN measure without cross-surname deeds (sensitivity; the earlier 33% scored the label, not family status)",
           paste0("((", bg_noaf, " OR ", bg_heir, ") AND coalesce(fam_relation_nm, '') <> 'cross_surname')"),
           "cross", "exposed (sensitivity: MAIN measure excluding cross-surname)"),
    bg_row("cross_surname", "Cross-surname deeds within the proxy without affidavits (after the relation rule: 73% read correct as distinct people with different surnames)",
           paste0("(", bg_noaf, " AND fam_relation_nm = 'cross_surname')"),
           "cross", "reported only"),
    bg_row("main_noaf_trust_heir_xor", "MAIN measure without same-person retitles (owner_retained)",
           paste0("((", bg_noaf, " OR ", bg_heir, ") AND coalesce(fam_relation_nm, '') <> 'owner_retained')"),
           "cross", "exposed (sensitivity: MAIN measure excluding owner_retained)"),
    bg_row("main_noaf_trust_heir_xore", "MAIN measure without owner_retained and unrelated_entity",
           paste0("((", bg_noaf, " OR ", bg_heir, ") AND coalesce(fam_relation_nm, '') NOT IN ('owner_retained', 'unrelated_entity'))"),
           "cross", "exposed (sensitivity: MAIN measure excluding owner_retained and unrelated_entity)"),
    bg_row("main_owner_retained", "owner_retained deeds inside the previous main measure (outside the measure)",
           paste0("((", bg_noaf, " OR ", bg_heir, ") AND fam_relation_nm = 'owner_retained')"),
           "cross", "outside the measure"),
    bg_row("main_unrelated_entity", "unrelated_entity deeds inside the previous main measure (outside the measure)",
           paste0("((", bg_noaf, " OR ", bg_heir, ") AND fam_relation_nm = 'unrelated_entity')"),
           "cross", "reported only"),
    bg_row("main_distinct", "MAIN measure: distinct people (proxy without affidavits, same-person and entity deeds, plus chain-verified heirs)",
           bg_distinct, "cross", "exposed (MAIN measure, author's decision)"),
    bg_row("main_distinct_hi", "Upper bound: MAIN measure plus other trust-to-other-person deeds not same-person",
           paste0("(", bg_distinct, " OR (", bg_veh("trust_to_other_person"),
                  " AND coalesce(fam_relation_nm, '') <> 'owner_retained'))"),
           "cross", "exposed (upper bound of the MAIN measure)"),
    # Added 11/12 (R1, 22-23): the MAIN measure without the chain-verified heirs, so it needs no
    # look-back and is defined the same way before 2017 (placebo years and base years 2008-2016).
    bg_row("main_distinct_noheir", "MAIN measure without chain-verified trust-to-heir deeds (no look-back; comparable before 2017)",
           paste0("(", bg_noaf, " AND coalesce(fam_relation_nm, '') NOT IN ('owner_retained', 'unrelated_entity'))"),
           "cross", "reported only (MAIN measure without chain heirs; R1 placebo years)"),
    bg_row("main_distinct_xspouse_ca", "MAIN measure without spouse deeds, California",
           paste0("state = 'CA' AND ", bg_distinct, " AND coalesce(fam_relation, '') <> 'spouse'"),
           "ca_only", "reported only"),
    bg_row("trust_heir_nochain", "Trust to other person without a verified chain",
           paste0(bg_veh("trust_to_other_person"), " AND NOT ", bg_heir),
           "cross", "reported only"),
    bg_row("refi_exposed", "Refinance-exposed retitles (union)",
           paste0("(", bg_veh("direct"), " AND fam_relation_nm = 'owner_retained') OR ",
                  bg_veh("trust_to_self"), " OR ", bg_veh("into_own_trust")),
           "cross", "refinance placebo"),
    bg_row("spouse_ca", "Spouse deeds (document types IT, IC), California",
           "state = 'CA' AND fam_relation = 'spouse'",
           "ca_only", "comparison (legally unaffected; refinance-exposed)"),
    bg_row("main_noaf_xspouse_ca", "Variant A without spouse deeds, California",
           paste0("state = 'CA' AND ", bg_noaf, " AND coalesce(fam_relation,'') <> 'spouse'"),
           "ca_only", "reported only")
  )
  stopifnot(nrow(bygroup_defs) == 37L, !anyDuplicated(bygroup_defs$name),
            all(bygroup_defs$scope %in% c("cross", "ca_only")))
  bygroup_main <- "main_distinct"        # the Section 4 family measure (author's decision 11/12)
  bygroup_chain_groups <- c("trust_heir_chain", "main_noaf_trust_heir", "main_noaf_trust_heir_hi",
                            "trust_heir_nochain", "main_distinct", "main_distinct_hi",
                            "main_distinct_xspouse_ca", "main_noaf_trust_heir_xcs",
                            "main_noaf_trust_heir_xor", "main_noaf_trust_heir_xore",
                            "main_owner_retained", "main_unrelated_entity")   # 2017 onward only (look-back completeness)
  stopifnot(bygroup_main %in% bygroup_defs$name)
  rm(bg_main, bg_af, bg_tlo, bg_thi, bg_noaf, bg_heir, bg_distinct, bg_veh, bg_row)

  # ---- windows shared by the by-group designs (YYYYMM; policy dates in 00_setup.R) ----
  bygroup_windows <- list(
    jan_feb_2021  = c(202101L, 202102L),
    mar_apr_2021  = c(202103L, 202104L),
    nine_month    = c(202006L, 202102L),
    hole_start    = 202103L,
    hole_end_main = 202406L,                 # last usable month (data end Aug 2024, partial)
    hole_end_sens = c(202312L, 202405L),
    deadline_raw  = 20210215L                # last day under the old rules; 16 Feb is new rules
  )
  bygroup_min_ca_deeds <- 500L               # CA deeds a year, both periods, for a regression row
}
