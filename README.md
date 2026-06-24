# CoreLogic Research

Research code and manuscript sources for projects using CoreLogic property and deed records. The repository is organized around applied housing, real-estate, and local-public-finance papers, with shared data loaders and common manuscript infrastructure at the root.

Maintained by [Saani Rawat](https://www.marquette.edu/business/faculty-staff/saani-rawat.php), Assistant Professor of Real Estate at Marquette University.

## Projects

| Folder | Project | Status | Core question |
|---|---|---:|---|
| `projects/01_property_tax_regressivity` | Property Tax Assessment Regressivity | Exploration | How much tax revenue is misallocated by assessment regressivity, and which mechanisms drive it? |
| `projects/02_cash_buyer_premium` | Cash Buyer Premium | Writing | What do national deed records reveal about cash buyers, mortgage buyers, investors, and transaction prices? |
| `projects/03_family_homes` | Keeping the House in the Family | Writing | How large is the non-market intra-family housing-transfer channel, and how does tax policy affect whether inherited homes reach the market? |
| `projects/04_algorithmic_intermediation` | Algorithmic Intermediation and Price Discovery | Review | Did iBuyer entry and exit improve information aggregation in decentralized housing markets? |

Each project contains its own `README.md`, `research_spec.md`, numbered scripts, manuscript source, tables, figures, and slides where available.

## Repository Layout

- `projects/` - paper-specific analysis, manuscript, and slide folders.
- `shared_utils/` - reusable CoreLogic loaders, filters, data dictionaries, themes, and tests.
- `scripts/` - shared one-off data-building scripts that are not project-specific.
- `Preambles/` - shared LaTeX preamble files.
- `Bibliography_base.bib` - shared bibliography entries.
- `data/` - ignored working data directory; only structure and provenance notes are tracked.
- `renv.lock`, `pyproject.toml`, `uv.lock`, `Project.toml`, `Manifest.toml` - R, Python, and Julia environment metadata.

## Data

CoreLogic raw extracts and working parquet files are not redistributed. Local data live under `data/` and are ignored by Git, except for directory placeholders and documentation. See `data/README.md` and `data/corelogic_baseline/PROVENANCE.md` for the local data layout and provenance notes.

The intended pattern is:

1. Keep licensed raw extracts outside the public repository.
2. Convert or wrap local inputs into ignored working data under `data/`.
3. Run project scripts in numbered order from each project folder.
4. Track code, manuscript source, tables, figures, and selected compiled PDFs only when useful for review.

## Reproducibility

R is the main analysis language, with selected Python and Julia utilities where useful. The repository tracks environment files for all three ecosystems:

- R: `renv.lock`
- Python: `pyproject.toml` and `uv.lock`
- Julia: `Project.toml` and `Manifest.toml`

Most project scripts start from a project-local `scripts/R/00_setup.R` and write intermediate outputs to ignored data or output directories. Large parquet, Arrow, Feather, and raw data files are intentionally ignored.

## Local-Only Workflow Files

Agentic workflow files, scratch explorations, review logs, local memory, temp renders, and planning artifacts are kept out of the public repository. They may exist locally for the maintainer's workflow, but `.gitignore` excludes paths such as `.claude/`, `.codex/`, `quality_reports/`, `projects/*/quality_reports/`, `explorations/`, `tmp/`, and workflow templates.

## License

Code is released under the MIT License. CoreLogic data are not included and remain subject to the relevant academic license agreements.
