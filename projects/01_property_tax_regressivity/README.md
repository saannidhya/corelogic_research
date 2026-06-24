# Project: Property Tax Assessment Regressivity

**Slug:** `property_tax_regressivity`
**Number:** 01
**Started:** 2026-05-19
**Started at SHA:** d81d9d1
**Status:** EXPLORATION

---

## Research Question

> How much local-government tax revenue is misallocated by assessor information asymmetry, and which mechanism — information decay (reassessment cycle), information acquisition cost (transaction density), assessor institutional incentives, appeals technology, or transaction-frequency-driven staleness — drives the regressivity quantitatively?

(See [`research_spec.md`](research_spec.md) for the full spec.)

## Key Files

- [`research_spec.md`](research_spec.md) — formal spec
- [`scripts/R/00_setup.R`](scripts/R/00_setup.R) — paths, packages (`fixest`, `modelsummary`, `duckdb`, `glue`)
- [`scripts/R/01_clean.R`](scripts/R/01_clean.R) — Berry-replication panel builder (national 2007–2010)
- [`scripts/R/02_replicate_berry.R`](scripts/R/02_replicate_berry.R) — within-jurisdiction regressivity regression
- `scripts/R/02b_decile_analysis.R` — decile-ratio analysis (TBD)
- `scripts/R/03_tables.R` — manuscript tables (template; to be filled)
- `scripts/R/04_figures.R` — manuscript figures (template; to be filled)
- [`manuscript/paper.tex`](manuscript/paper.tex) — manuscript draft
- [`slides/seminar.tex`](slides/seminar.tex) — seminar slides (Beamer source of truth)

## Replication note

The Berry (2021) benchmark has been replicated with caveats: within-jurisdiction beta is about -0.44 on the 2007-2010 housing-bust window, compared with Berry's reported -0.37.

## Phase status

| Phase | Status | Output |
|---|---|---|
| 1. Berry replication | ✅ DONE (with caveats) | Within-jurisdiction β = −0.44, N = 11.9M, 1,773 jurisdictions |
| 1.5. Robustness sub-regressions | TODO | Year-by-year, residential-only, state-level β |
| 2. Reduced-form mechanism tests | TODO | H2–H6 tests (cycle length, density, institutions, appeals, transaction freq) |
| 3. Structural estimation (SMM) | TODO | Bayesian-assessor model in Julia |
| 4. Robustness + writing | TODO | Cook County event study, alt FE, manuscript |

## Status History

| Date | Status | Note |
|---|---|---|
| 2026-05-19 | SCOPING | Created via `/new-project property_tax_regressivity` |
| 2026-05-19 | SCOPING | Spec approved after research scoping and citation audit |
| 2026-05-29 | EXPLORATION | Phase 1 Berry replication REPLICATED-WITH-CAVEATS; advancing to Phase 1.5 + Phase 2 |

## Next Steps

- [ ] Phase 1.5 robustness: year-by-year sub-regressions, residential-only re-run, state-level β heatmap
- [ ] Phase 2: assemble reassessment-cycle data (Lincoln Institute), assessor institutional features (IAAO), ACS tract estimates (`tidycensus`)
- [ ] Phase 2: border-MSA design for H2; Bartik shift-share IV for H3
- [ ] Expand the property-tax-regressivity literature review in `Bibliography_base.bib`
- [ ] Contact UC CoreLogic liaison about refreshed prop extract (would unlock 2011–2024 sample window)

## Status definitions

SCOPING → **EXPLORATION** → ANALYSIS → WRITING → REVIEW → SUBMITTED → R&R → PUBLISHED

Project status is maintained manually in this README.
