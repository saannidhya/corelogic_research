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
- `scripts/R/04_prop19.R` — RQ3 Prop 19 event study + DiD
- `scripts/R/05_tables.R` — manuscript tables
- `scripts/R/06_figures.R` — manuscript figures
- `manuscript/paper.tex` — manuscript draft
- `slides/seminar.tex` — seminar slides (Beamer source of truth)

## Status History

| Date | Status | Note |
|---|---|---|
| 2026-06-09 | SCOPING | Created (manual scaffold from `projects/_template/`; `/new-project` skill unavailable in session) |
| 2026-06-09 | EXPLORATION | Feasibility probes confirmed flag semantics, name taxonomy, and a visible Prop 19 spike |
| 2026-06-09 | WRITING | Full pipeline run (148.7M events); first complete manuscript draft compiled (23pp) |
| 2026-06-10 | WRITING | Panel rebuilt with retitle class and data fixes; permutation inference and direct supply test added; numbers recomputed; draft v2 compiled (25pp) |

## Next Steps

- [x] Feasibility probes (flag semantics, name population, clip linkage, Prop 19 visibility)
- [ ] Lit review — intergenerational housing transfers, Prop 13/19, transfer-tax timing, misallocation
- [ ] Build national transfer-event panel with taxonomy (01)
- [ ] National facts (02), hazard (03), Prop 19 causal core (04)
- [ ] First full manuscript draft and internal review

## Status definitions

SCOPING → EXPLORATION → ANALYSIS → WRITING → REVIEW → SUBMITTED → R&R → PUBLISHED

Project status is maintained manually in this README.
