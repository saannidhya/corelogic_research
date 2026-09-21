# AGENTS.md — CoreLogic Research Codebase

**Project:** CoreLogic property + transaction research
**Researcher:** Saani Rawat (Assistant Professor of Real Estate, Marquette University; Economics PhD, University of Cincinnati)
**Branch:** main
**Stack:** R (analysis) · Python (scraping) · Julia (modeling) · LaTeX (manuscripts)
**Forked from:** pedrohcgs/Codex-my-workflow v1.8.0

---

## Core Principles

- **Plan first** — enter plan mode before non-trivial tasks; save plans to `quality_reports/plans/`
- **Verify after** — compile/render and confirm output at the end of every task
- **Read-only raw data** — `C:\CoreLogic\` is never written, deleted, or modified
- **Single loader entry point** — all CoreLogic reads go through `shared_utils/R/corelogic_loader.R`
- **Single source of truth** — Beamer `.tex` is authoritative for slides; Quarto `.qmd` derives from it
- **Quality gates** — nothing ships below 85/100 (advisory; halt-and-ask in `/commit`)
- **[LEARN] tags** — when corrected, save `[LEARN:category] wrong → right` to [MEMORY.md](MEMORY.md)

Cross-session context lives in [MEMORY.md](MEMORY.md); past plans, specs, and session logs are in [quality_reports/](quality_reports/).

---

## CoreLogic Data Contract (READ THIS)

- **Raw extracts:** `C:\CoreLogic\` — READ-ONLY. No tool call may write, delete, or modify anything under this path.
- **Working data:** repo `data/` (gitignored) holds parquet conversions, samples, baseline wrappers, external data, derived data.
- **Reads:** All project scripts read CoreLogic via `shared_utils/R/corelogic_loader.R` (R), `shared_utils/python/corelogic_loader.py` (Python), or `shared_utils/julia/corelogic_loader.jl` (Julia). Never read raw files directly from project code.
- **Conversion:** `shared_utils/R/convert_raw_to_parquet.R` is run once per extract refresh; it transcodes `C:\CoreLogic\housing\{OwnerTransfer,PropertyCharacteristics}\by_state\*.csv` to `data/corelogic_extracts/by_state/` and quarantines junk-state files to `_quarantine/`.

See [.Codex/rules/corelogic-data-protocol.md](.Codex/rules/corelogic-data-protocol.md) for details.

---

## Folder Structure

```
corelogic_research/
├── AGENTS.md, MEMORY.md, README.md         # Project memory + docs
├── Bibliography_base.bib                    # MASTER bibliography (root)
├── .Codex/                                 # Skills, rules, agents, hooks
├── templates/                               # Project + spec + session templates
├── quality_reports/                         # Plans, specs, session logs (root-level)
├── Preambles/header.tex                     # SHARED LaTeX header
├── shared_utils/{R,python,julia}/           # Cross-project libraries
├── data/                                    # GITIGNORED — see data/README.md
│   ├── corelogic_extracts/by_state/         # Parquet store
│   ├── corelogic_baseline/                  # Prior Ohio outputs + PROVENANCE.md
│   ├── external/                            # ACS, weather, Zillow, etc.
│   └── derived/                             # Per-project cleaned data
├── projects/                                # Per-paper folders
│   ├── _template/                           # Scaffold for /new-project
│   └── 0N_<slug>/                           # One per paper
├── explorations/                            # Sandbox
└── master_supporting_docs/                  # Reference PDFs, prior slides
```

---

## Current Projects

| # | Slug | Status | Topic | Manuscript | Slides |
|---|------|--------|-------|------------|--------|
| 01 | `property_tax_regressivity` | EXPLORATION | How much tax revenue is misallocated by assessor information asymmetry, and which mechanism (info decay / acquisition cost / institutions / appeals / transaction freq) drives it? Berry (2021 SSRN) replication + structural extension | [projects/01_property_tax_regressivity/manuscript/paper.tex](projects/01_property_tax_regressivity/manuscript/paper.tex) | [projects/01_property_tax_regressivity/slides/seminar.tex](projects/01_property_tax_regressivity/slides/seminar.tex) |
| 03 | `family_homes` | EXPLORATION | How large is the non-market intra-family channel of housing turnover, what happens to homes after family transfers, and does the tax price of dynastic holding (CA Prop 19) causally affect whether inherited homes reach the market? | [projects/03_family_homes/manuscript/paper.tex](projects/03_family_homes/manuscript/paper.tex) | [projects/03_family_homes/slides/seminar.tex](projects/03_family_homes/slides/seminar.tex) |

---

## Commands

```bash
# LaTeX (3-pass, XeLaTeX only) — example for a project paper
cd projects/01_<slug>/manuscript && TEXINPUTS=../../../Preambles:$TEXINPUTS xelatex -interaction=nonstopmode paper.tex
BIBINPUTS=../../..:$BIBINPUTS bibtex paper
TEXINPUTS=../../../Preambles:$TEXINPUTS xelatex -interaction=nonstopmode paper.tex
TEXINPUTS=../../../Preambles:$TEXINPUTS xelatex -interaction=nonstopmode paper.tex

# Quality score
python scripts/quality_score.py projects/01_<slug>/manuscript/paper.tex

# Run shared_utils smoke tests
Rscript shared_utils/R/tests/run_tests.R

# Initial CoreLogic parquet conversion (one-time)
Rscript shared_utils/R/convert_raw_to_parquet.R
```

---

## Quality Thresholds (advisory)

| Score | Checkpoint | Meaning |
|-------|------------|---------|
| 85 | Commit | Good enough to save |
| 92 | PR | Ready for deployment |
| 97 | Excellence | Aspirational |

Enforced by `/commit` (halts + asks for override); not enforced by a git pre-commit hook.

---

## Environment Reproducibility

- **R:** `renv` — `renv::restore()` to install pinned versions
- **Python:** `uv` — `uv sync` to install from `pyproject.toml` / `uv.lock`
- **Julia:** `Pkg.instantiate()` from the repo's `Project.toml`

---

## Skills Quick Reference

| Command | What It Does |
|---------|--------------|
| `/new-project <slug>` | Scaffold a new project under `projects/NN_<slug>/` |
| `/interview-me [topic]` | Formalize a research idea into a spec (saves to project) |
| `/lit-review [topic]` | Literature search + synthesis |
| `/research-ideation [topic]` | Generate RQs + hypotheses + strategies |
| `/preregister [--style osf\|aspredicted\|aea-rct]` | Draft preregistration |
| `/data-analysis [dataset]` | End-to-end R analysis |
| `/audit-reproducibility [paper]` | Check paper ↔ code numeric tolerance |
| `/review-paper [file]` | Manuscript review (single / `--adversarial` / `--peer <journal>`) |
| `/respond-to-referees [report] [manuscript]` | R&R response drafting |
| `/seven-pass-review [file]` | Seven parallel adversarial reviews |
| `/verify-claims [file]` | CoVe fact-check of citations + numerical claims |
| `/review-r [file]` | R code quality review |
| `/compile-latex [file]` | 3-pass XeLaTeX + bibtex (Beamer seminar slides / manuscripts) |
| `/visual-audit [file]` | Slide layout audit |
| `/proofread [file]` | Grammar/typo/overflow review |
| `/validate-bib` | Cross-reference citations |
| `/commit [msg]` | Stage, commit, PR, merge |
| `/learn [skill-name]` | Extract discovery into persistent skill |
| `/context-status` | Show session health + context usage |
| `/checkpoint [topic]` | Save state snapshot |
| `/permission-check` | Diagnose permission layers |
