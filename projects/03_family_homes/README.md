# Project: Keeping the House in the Family — Non-Market Transfers and the Supply of Homes

**Slug:** `family_homes`
**Number:** 03
**Started:** 2026-06-09
**Started at SHA:** 4702a28
**Status:** REVIEW (simulated JUE peer review done 2026-11-12; target journal: Journal of Urban Economics)

---

## Current state (2026-11-12)

This section describes the paper and pipeline as they stand; the sections further down are history.

**Measures.** One name per measure (the manuscript's Appendix Table `tab:glossary` is the reference):
- *Main family measure* (Sections 2, 3 and the wedge sample): same-surname, estate/executor and other interfamily deeds between two distinct people. Same-person and entity deeds are removed (author's decision, 2026-11-12); spouse deeds stay. Code: `family_classes` read through `events_main_from()` / `read_events_df(measure = "main")` in `scripts/R/00_setup.R`.
- *Upper bound*: the main family measure plus the same-person and entity deeds (the earlier proxy).
- *Section 4 measure* (Proposition 19 designs and the welfare calibration): the main family measure without affidavits of death, plus chain-verified trust-to-heir deeds from 2017. Code: `bygroup_main` (`main_distinct`) in `scripts/R/00_groups.R`.
- *Trust-inclusive measure*: the Section 4 measure plus every other trust-to-person deed (`main_distinct_hi`).

**Panel.** `data/derived/03_family_homes/events.parquet` (built by `01_build_panel.R`). The family measure is `deed_cat = 'family_transfer'`, locked 2026-11-16 (`research_spec.md`, Taxonomy of record); `01b_taxonomy_tables.R` writes Tables 1 and 2 to `_outputs/taxonomy/`. As of 2026-11-16 the analysis scripts and the manuscript have NOT been rerun on this panel. Decisions behind the measures are recorded in `quality_reports/plans/2026-11-10_discussant-response.md` and the gate memos under `quality_reports/gate1/`–`gate3/`.

**Scripts added for the discussant response.**
- `16_bygroup_cube.R` to `19_bygroup_parcels.R` (with `17b`–`17f`, `18b`): every Section 4 design by deed group (rush, deadline ratios, triple difference, event study, rush and shortfall, resale, occupancy, deeds per parcel).
- `20_section4_exhibits.R`: Section 4 tables, figures and `s4_numbers.tex` (staged in `_outputs/section4_v2/`, then copied to `manuscript/`).
- `21_s23_exhibits.R`: Sections 2–3 tables and `s23_numbers.tex` (staged in `_outputs/section23/`; `S23_PULL=1` re-pulls the cached counts).
- `22_same_person_audit.R`: the same-person and entity audit behind the 2026-11-12 measure.
- `11b_welfare_options.R`: the welfare calibration of Section 5.3 (`welfare_v2_numbers.tex`, `TA_welfare_grid.tex`). `11_welfare.R` and its outputs are legacy and not used by the paper.

**Run order** (from the repository root, `Rscript projects/03_family_homes/scripts/R/<script>`):
1. `01_build_panel.R`, then the checks `01c_reconcile_panel_v2.R` and `01d_validate_panel_v2.R`.
2. `22_same_person_audit.R` (inputs to 21).
3. Main family measure: `02_facts.R`, `03_hazard.R`, `13_property_types.R`, `04a_build_wedge.R`, `04b_wedge_figures.R`, then `quality_reports/plans/2026-10-07_verify_wedge_panel.R` (writes the wedge audit summary that 04c reads), then `04c_wedge_tables.R`.
4. Section 4 measure (legacy designs and appendix): `04_prop19.R`, `07_referee_checks.R`, `09_synthetic_control.R`, `09b_event_study.R`, `10_mortality.R`, `14_trust_substitution.R`.
5. Exhibits and macros: `05_tables.R`, `06_figures.R`, `06c_family_per_deed.R`, `06d_manuscript_numbers.R`.
6. By-group designs: `16`, `17`, `17b`–`17f`, `18`, `18b`, `19`.
7. `20_section4_exhibits.R`, `21_s23_exhibits.R`, `11b_welfare_options.R`; copy the staged tables, figures and macro files into `manuscript/tables/` and `manuscript/figures/`.
8. Compile `manuscript/paper.tex` (three XeLaTeX passes with bibtex; commands in the repository `CLAUDE.md`).

**Quality reports.** Reproducibility audit, claims check, proofreading (applied) and the simulated JUE review are in `quality_reports/phase9/`; the running log is `quality_reports/session_logs/2026-11-11_discussant-response.md`.

---

## Research Question

> How large is the non-market, intra-family channel of US housing turnover; what happens to homes after they pass within families; and does the tax price of keeping a home in the family (California Proposition 19) causally affect whether inherited homes reach the market?

(See `research_spec.md` for the full spec.)

## Key Files

(Historical list; the current scripts and run order are under "Current state" above.)

- `research_spec.md` — formal spec
- `scripts/R/00_setup.R` — paths, packages, duckdb helpers
- `scripts/R/01_build_panel.R` — national transfer-event panel + name-based taxonomy
- `scripts/R/02_facts.R` — RQ1 national facts (volume, composition, trends)
- `scripts/R/03_hazard.R` — RQ2 post-transfer time-to-market-sale hazard
- `scripts/R/04a_build_wedge.R` — CA parcel tax wedge from the 2008–09 roll (Prop 13 roll-forward, hedonic AVM, repeat-sales index) + first-exit spells for fixed 2017/2019 risk sets
- `scripts/R/04_prop19.R` — RQ3 Prop 19: aggregate retiming, long-held × CA × post DDD, wedge dose-response hazards by 2008 occupancy, competing risks, robustness, headline numbers (legacy volume DiD / sold24 / absentee kept as appendix outputs)
- `scripts/R/check_prop19_wedge_invariants.R` — synthetic checks of roll-forward, risk-set accounting, offset Poisson, competing-risk decomposition
- `scripts/R/09_synthetic_control.R` — appendix donor-weighted synthetic control; fit/gap summaries only
- `scripts/R/09b_event_study.R` — monthly TWFE event study (2018–19 baseline)
- `scripts/R/10_mortality.R` — tidycensus 65+ pull + per-65+ demographic-adjusted SCM
- `scripts/R/05_tables.R` — manuscript tables
- `scripts/R/11_welfare.R` — conditional allocation-gain calibration, CA parcel-episode persistence, valuation audit, and generated welfare exhibits
- `scripts/R/11_welfare_helpers.R` — episode and finite-horizon accounting functions
- `scripts/R/check_welfare_invariants.R` — synthetic date, episode, survival, timing, and welfare accounting checks
- `scripts/R/06_figures.R` — manuscript figures (F1–F7, SCM, and monthly event study)
- `manuscript/paper.tex` — manuscript draft
- `slides/seminar.tex` — seminar slides (Beamer source of truth)

## Status History

| Date | Status | Note |
|---|---|---|
| 2026-06-09 | SCOPING | Created (manual scaffold from `projects/_template/`; `/new-project` skill unavailable in session) |
| 2026-06-09 | EXPLORATION | Feasibility probes confirmed flag semantics, name taxonomy, and a visible Prop 19 spike |
| 2026-06-09 | WRITING | Full pipeline run (148.7M events); first complete manuscript draft compiled (23pp) |
| 2026-06-10 | WRITING | Panel rebuilt with retitle class and data fixes; permutation inference and direct supply test added; numbers recomputed; draft v2 compiled (25pp) |
| 2026-06-25 | WRITING | Section 6 (Prop 19 causal) rebuilt: synthetic control (MSPE-ratio p=0.039) + Conley–Taber replace the placebo rank; monthly event study; 65+ demographic bracket (−26% to −33%); within-CA DiD tried and dropped; F5–F7 swapped to SCM figures, old placebo-rank + sold24 figures relegated to appendix |
| 2026-09-21 | WRITING | Reordered national facts → Prop 19 → framework and conditional future allocation gains. Added 4,050 calibration scenarios, CA parcel-episode survival, public-wedge provenance, and valuation audit. Explicit calendar-date parsing reproduces prior hazard and policy results. |
| 2026-11-12 | REVIEW | Discussant-response plan (Phases 0–9): panel v2 with deed taxonomy and trust chain; same-person and entity deeds removed; Section 4 rebuilt by deed group; welfare recalibrated; reproducibility audit, claims check and approved proofreading applied; simulated JUE review (desk reject, two referee reports) and its text fixes applied. |

## Welfare revision and reproduction

The current paper uses ordinary sections: introduction, data, four national
facts, Proposition 19, framework with potential allocation gains, and
conclusion. The third contribution is a conditional calibration, not an
estimate of realized welfare. The net-DiD placebo result remains material;
the two SCM specifications are kept separate from the deed-flow reference.

Restore the repository's R environment with `renv::restore()` before running
analysis. After the existing pipeline outputs are available, run from the
repository root:

```text
Rscript projects/03_family_homes/scripts/R/11_welfare.R
Rscript projects/03_family_homes/scripts/R/check_welfare_invariants.R
```

The welfare script reads the derived event panel, saved policy estimates,
and California property fields through the shared loader. It writes audit
and scenario CSVs under `scripts/R/_outputs/tables/`, plus manuscript tables,
number macros, and Figure 8. It does not write raw data. The external CPI
input and its source documentation are in `scripts/R/inputs/`.

The revision memo, numerical comparison with pre-revision outputs, build
logs, and visual checks are under
`projects/03_family_homes/quality_reports/family_welfare_2026-09-21/`.
Historical status entries below refer to the earlier section numbering.

## Family definition and descriptive denominator as of 2026-11-04 (superseded; see "Current state")

The main operational family-transfer proxy includes `family_person`,
`family_estate`, and `family_other`. These classes are centralized as
`family_classes` in `scripts/R/00_setup.R`. Trust deeds and same-person
retitles remain separate. The vendor relationship flag does not independently
verify kinship, inheritance, occupancy, or a change in beneficial ownership.

Figures 1 and 2 divide family deeds by **all residential deed events**.
Figure 1A stacks same-surname-plus-estate and other interfamily components;
Figure 1B uses the complete main proxy over time. Table 2 reports annual
all-deed counts and shares. The existing policy designs, treatment windows,
market-sale comparison, and inference procedures are retained. Explicitly
labeled family-plus-market denominators remain in the property-correlates
and property-type comparisons.

`06d_manuscript_numbers.R` exports policy and profile macros with a numerical
source CSV; `05_tables.R` exports the descriptive macros. Both must be rerun
after their analysis inputs change. `06c_family_per_deed.R` retains the
separate, labeled state-share figure requested during the definition discussion.

The dependency-ordered refresh and verification logs live under
`quality_reports/wide_refresh_2026-11-04/`; its plan is
`quality_reports/plans/2026-11-04_restore-wide-family.md`.

## Next Steps

- [x] November 4 refresh: restore other interfamily deeds, use all-deed shares in Figures 1–2 and Table 2, rerun affected analyses, update Section 5 and manuscript, and verify the PDF.
- [x] Pipeline (01), national facts (02), hazard (03), Prop 19 core (04), first full draft
- [x] Section 6 rebuild (June 2026) — synthetic control + monthly event study + 65+ demographic bracket; figures swapped
- [ ] Consider a compact table combining the triple difference, SCM, and demographic-adjusted SCM results, which currently appear in separate tables/figures and text.
- [ ] Within-CA descriptive paragraph (Prop 19 concentrated in the primary-residence channel) + update the mortality "planned refinement" threats bullet
- [ ] Tier-2 robustness: bunching excess-mass + bootstrap, HonestDiD sensitivity, trust/LLC substitution bounds
- [ ] Re-run `/review-paper --peer` on the rebuilt Section 6; `renv::snapshot()` for the new packages (tidysynth, tidycensus)

## Status definitions

SCOPING → EXPLORATION → ANALYSIS → WRITING → REVIEW → SUBMITTED → R&R → PUBLISHED

Project status is maintained manually in this README.

## Figures and quality reports

Figure exporters write PNG only; manuscript figure references use PNG. Project-specific plans, reviews, audit artifacts and session logs live in `projects/03_family_homes/quality_reports/`. Root `quality_reports/` is for shared or repository-wide work.

### Current inference and presentation (2026-11-04)

The main policy designs are the annual triple difference and monthly event study.
SCM and the per-65+ demographic sensitivity are in the appendix. State-placebo
rank p-values, donor-gap intervals, and donor placebo bands have been removed
from active scripts and outputs. The within-CA comparison (`08_within_ca.R`),
the estate/trust-seller test and `00_inference_helpers.R` were removed on 2026-11-11. Conventional state-clustered standard errors
remain qualified because California is the only treated state. Historical
session entries below/above describe prior versions, not the current workflow.
