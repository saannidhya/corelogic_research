# Project: Keeping the House in the Family — Non-Market Transfers and the Supply of Homes

**Slug:** `family_homes`
**Number:** 03
**Started:** 2026-06-09
**Started at SHA:** 4702a28
**Status:** WRITING

---

## Research Question

> How large is the non-market, intra-family channel of US housing turnover; what happens to homes after they pass within families; and does the tax price of keeping a home in the family (California Proposition 19) causally affect whether inherited homes reach the market?

(See `research_spec.md` for the full spec.)

## Key Files

- `research_spec.md` — formal spec
- `scripts/R/00_setup.R` — paths, packages, duckdb helpers
- `scripts/R/01_build_panel.R` — national transfer-event panel + name-based taxonomy
- `scripts/R/02_facts.R` — RQ1 national facts (volume, composition, trends)
- `scripts/R/03_hazard.R` — RQ2 post-transfer time-to-market-sale hazard
- `scripts/R/04_prop19.R` — RQ3 Prop 19 bunching counterfactual + volume DiD
- `scripts/R/06b_prop19_scm_figures.R` — SCM figures (F5–F7) + monthly event study (Section 6 rebuild)
- `scripts/R/08_within_ca.R` — within-CA DiD (built, dropped as causal — wrong sign; descriptive only)
- `scripts/R/09_scm_inference.R` — donor-weighted synthetic control + MSPE-ratio + Conley–Taber inference
- `scripts/R/09b_event_study.R` — monthly TWFE event study (2018–19 baseline) + placebo band
- `scripts/R/10_mortality.R` — tidycensus 65+ pull + per-65+ demographic-adjusted SCM
- `scripts/R/05_tables.R` — manuscript tables
- `scripts/R/06_figures.R` — manuscript figures (F1–F4 + legacy)
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

## Next Steps

- [x] Pipeline (01), national facts (02), hazard (03), Prop 19 core (04), first full draft
- [x] Section 6 rebuild (June 2026) — synthetic control + monthly event study + 65+ demographic bracket; figures swapped
- [ ] Table 4: add SCM / event-study / demographic-adjusted rows (currently shows only the legacy DiD)
- [ ] Within-CA descriptive paragraph (Prop 19 concentrated in the primary-residence channel) + update the mortality "planned refinement" threats bullet
- [ ] Tier-2 robustness: bunching excess-mass + bootstrap, HonestDiD sensitivity, trust/LLC substitution bounds
- [ ] Re-run `/review-paper --peer` on the rebuilt Section 6; `renv::snapshot()` for the new packages (tidysynth, tidycensus)

## Status definitions

SCOPING → EXPLORATION → ANALYSIS → WRITING → REVIEW → SUBMITTED → R&R → PUBLISHED

Project status is maintained manually in this README.
