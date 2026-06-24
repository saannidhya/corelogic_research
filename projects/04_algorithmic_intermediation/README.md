# Project: The Machine in the Market — Algorithmic Intermediation and Price Discovery in Housing

**Slug:** `algorithmic_intermediation`
**Number:** 04
**Started:** 2026-06-10
**Started at SHA:** d278fa4
**Status:** REVIEW

---

## Research Question

> Does the entry — and abrupt exit — of AI-based intermediaries trading on
> algorithmic valuation models improve information aggregation (price discovery)
> in decentralized housing markets?

(See `research_spec.md` for the full spec.)

## Key Files

- `research_spec.md` — formal spec
- `scripts/R/00_setup.R` — paths, sources, packages
- `scripts/R/01_extract_ibuyer_deeds.R` — iBuyer deed identification (national scan)
- `scripts/R/02_build_zip_panel.R` — ZIP × quarter outcome panel
- `scripts/R/03_hedonic_dispersion.R` — hedonic residual dispersion (price discovery)
- `scripts/R/04_entry_did.R` — staggered-entry event studies
- `scripts/R/05_exit_did.R` — Zillow Offers shutdown (Nov 2021) DiD / triple diff
- `scripts/R/06_tables.R`, `07_figures.R` — manuscript exhibits
- `manuscript/paper.tex` — manuscript draft (+ `appendix.tex` proofs)

## Status History

| Date | Status | Note |
|---|---|---|
| 2026-06-10 | SCOPING | Created (scaffolded from `projects/_template/`) |
| 2026-06-10 | ANALYSIS | Design validated on AZ deeds (17.5k iBuyer purchases 2019–21) |
| 2026-06-11 | REVIEW | Full draft (44pp) with verified theory + real estimates; verification agents + peer-review loop running |
| 2026-06-13 | REVIEW | R&R complete (48pp). Reproducibility 165/165, CoVe 27/27. QJE peer review R1 (2× major) → R2 conditional accept (8/10 resolved). Reframed to honest finding: clean price-discovery NULL (tight bound) + confounded volume. |

## Next Steps

- [x] Topic survey + novelty check vs BMPS (JPE) and JREFE spillover papers
- [ ] National iBuyer extraction + ZIP-quarter panel
- [ ] Entry + exit designs
- [ ] Manuscript + peer-review loop

## Status definitions

SCOPING → EXPLORATION → ANALYSIS → WRITING → REVIEW → SUBMITTED → R&R → PUBLISHED

See `.claude/rules/project-lifecycle.md` for transition guidance.
