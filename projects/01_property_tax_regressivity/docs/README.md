# Reference papers — `docs/`

This directory holds reference papers cited in the project spec.
**PDFs are gitignored** (copyright caution — don't push academic papers to git).

## Expected contents

| Filename | Citation | Where to download |
|---|---|---|
| `berry2021.pdf` | Berry, Christopher (2021) "Reassessing the Property Tax" | [SSRN abstract 3800536](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3800536) |
| `berry_wang2024.pdf` | Berry & Wang (2024) "Property Tax Assessment and Housing Market Cycles" | [Syracuse CPR PDF](https://www.maxwell.syr.edu/docs/default-source/research/cpr/property-tax-webinar-series/2023-2024/berry-and-wang-2024-accessible.pdf) |
| `avenancio-leon_howard2022.pdf` | Avenancio-León & Howard (2022 *QJE*) "The Assessment Gap" | [QJE](https://doi.org/10.1093/qje/qjac009) or [Minneapolis Fed WP](https://www.minneapolisfed.org/research/institute-working-papers/the-assessment-gap-racial-inequalities-in-property-taxation) |
| `hodge_komarek_mcallister2025.pdf` | Hodge, Komarek, McAllister (2025) "A Double Negative" *Public Finance Review* | [SAGE DOI 10.1177/10911421241280456](https://journals.sagepub.com/doi/abs/10.1177/10911421241280456) |
| `schleicher2025.pdf` | Schleicher, David (2025) "Your House Is Worth More Than They Think" *HJoL* 62.1 | [HJoL](https://journals.law.harvard.edu/jol/2025/02/22/your-house-is-worth-more-than-they-think-the-strange-case-of-property-tax-regressivity/) |
| `besley_coate2003.pdf` | Besley & Coate (2003) "Elected vs. Appointed Regulators" *JEEA* 1(5) | [Wiley](https://onlinelibrary.wiley.com/doi/abs/10.1162/154247603770383424) |
| `dube_lester_reich2010.pdf` | Dube, Lester, Reich (2010) "Min Wage Effects Across State Borders" *REStat* 92(4) | [MIT Press REStat](https://direct.mit.edu/rest/article/92/4/945/57855) |

## Why gitignored

- Many of these are published in subscription journals; redistribution is copyright-restricted
- Working papers on SSRN have their own licensing terms
- This is a public-ish GitHub repo even if licensed for personal research use
- Files are large (1-3 MB each); git is the wrong place for binary academic PDFs

## How to use

1. Download each paper from the link above to this directory
2. Reference paths in code: `here::here("projects/01_property_tax_regressivity/docs/berry2021.pdf")`
3. Use these local PDFs only for private research and claim verification.
