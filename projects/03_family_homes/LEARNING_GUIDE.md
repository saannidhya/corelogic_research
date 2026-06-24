# Learning Guide — Project 03: `family_homes`

**"Keeping the House in the Family: Non-Market Transfers and the Supply of American Homes"**
Saani Rawat · Status: WRITING (draft v2, 25pp) · Target: top-5 general interest

> A 60-minute, self-guided re-onboarding to the entire project: the research, the
> code, the data flow, the setup. Read top-to-bottom the first time; thereafter use
> Part 4 (the itinerary) as the index and Part 5 (key files) as the cheat sheet.

---

## 1. What is this project?

Housing economics studies the market *with prices* — recorded sales, indexed,
aggregated into turnover, liquidity, and supply statistics. This paper exploits a
simple institutional fact that the priced market misses: **a deed is filed on *any*
ownership change, not just a sale.** The same county-recorder trail that logs a
\$400k arm's-length sale also logs a mother quitclaiming her house to her son for
nothing. MLS data, repeat-sales indices, and mortgage records discard those
zero-price events *by construction*. CoreLogic's Owner Transfer (deed) file keeps
them — so deeds reveal a **second, non-market channel of housing circulation** that
the priced market cannot see. That is the project's core "aha": the unit of analysis
is the *deed event*, not the *sale*, and the gap between the two is the family
channel. Using 144M residential deed events (2007–2023; 148.7M through mid-2024), the
paper builds the first national deeds-based accounting of intra-family housing
transfers: ~19.7M family-to-family transfers, one for every 3.6 market sales, nine in
ten at a recorded price of zero.

The research question has **three sub-questions**, each its own exhibit:

1. **National measurement (RQ1, descriptive).** How large is the family channel?
   Answer: ~19.7M transfers 2007–2023, a stable family:market ratio of ~0.28 every
   year, concentrated in older/cheaper/senior-exempt homes — the fingerprint of a
   bequest pipeline, not disguised sales. (Scripts 01→02; Tables 1–2, Figures 1–2.)
2. **The family lock (RQ2, descriptive-dynamic).** What happens to a home *after* it
   passes within a family? Answer is bimodal: ~10% of cross-surname (estate-type)
   transfers reach the market within a year (settlement waypoints), but **62% of
   same-surname transfers have still never sold a decade later** — stickier than even
   investor-held market purchases. Selection *is* the content; no causal claim.
   (Script 03; Table 3, Figure 3.)
3. **The Prop 19 causal test (RQ3).** Does the *tax price of keeping a home in the
   family* causally affect whether inherited homes reach the market? California's
   Proposition 19 (passed Nov 3 2020, parent-child provisions effective Feb 16 2021)
   abruptly ended assessment inheritance for non-occupant heirs. The deeds record the
   response: family transfers ran **+31% (≈29k deeds) above counterfactual** in the
   four pre-deadline months (February 2021, normally the quietest month, became the
   busiest in the series), then fell to a persistently lower level — **−35% gross /
   −23% net of a market-sale placebo** in 2022–23, a missing flow of ~58k deeds/yr,
   commensurate with the 60k–80k tax-excluded inherited properties the state's
   Legislative Analyst counted annually. (Scripts 04 + 07; Table 4, Figures 4–5.)

**Why Prop 19 is the experiment — the keep-or-sell wedge (paper §5, eq. 2).** A house
yields flow value `u_m` to the marginal market buyer; with tax rate `t` on assessed
value, its price capitalizes the tax burden: `p = u_m / (r + t)`. An heir keeps the
house iff `(u_h − tA)/r ≥ p`, i.e. **`u_h ≥ u_m − t(p − A)`** (eq. 2). If succession
triggers reassessment (`A = p`), this collapses to `u_h ≥ u_m`: the family keeps only
homes it values more than the market — allocatively harmless. But if the heir
*inherits* the parent's old assessment (`A = A₀ < p`, the pre-Prop-19 California
regime), the family keeps the house whenever `u_h ≥ u_m − t(p − A₀)`. Every heir with
`u_h` in `[u_m − t(p − A₀), u_m)` retains a home the market values *more* than she
does. The **wedge `t(p − A₀)`** is the annual tax the code pays families to keep homes
off the market. Prop 19 set `A = p` for non-occupant heirs and closed the wedge — a
clean, dated, single-state shock. The subtle prediction (paper §5, last paragraph):
because *the recorded family deed is itself the outcome of the keep-or-sell choice*,
the supply release shows up as **missing family transfers (extensive margin)**, not as
faster resale of the transfers that survive. The marginal would-have-been-landlord
heirs simply never take title; the succession ends in a future estate sale. So the
model predicts post-reform surviving transfers should have a *lower* subsequent
sale hazard (selection toward genuine holders) — and the direct "where are the missing
homes?" test (07) returns a clean null, read as *deferral*, not failure.

---

## 2. Annotated folder / file tree

```
projects/03_family_homes/
├── README.md                         Status (WRITING), RQ, key-file index, status history
├── research_spec.md                  Frozen taxonomy + 5 exhibits + identification threats (the contract)
├── LEARNING_GUIDE.md                 ← you are here
│
├── scripts/R/                        All analysis is R + duckdb-over-parquet
│   ├── 00_setup.R                    ONLY file that touches paths; loads shared_utils, duckdb helpers, window constants
│   ├── 01_build_panel.R              ★ THE keystone: one pass over OT parquet → events.parquet + name-based taxonomy
│   ├── 02_facts.R                    RQ1 national/state volumes, ratios, validation moments, prop-join correlates
│   ├── 03_hazard.R                   RQ2 time-to-next-market-sale; KM survival by class (exit-table algorithm)
│   ├── 04_prop19.R                   RQ3 bunching counterfactual + volume DiD + sold24 DDD + absentee DiD (fixest)
│   ├── 05_tables.R                   Builds T1–T4 .tex from _outputs/ only (no data access)
│   ├── 06_figures.R                  Builds F1–F5 .pdf/.png from _outputs/ only (no data access)
│   ├── 07_referee_checks.R           ★ Permutation (placebo-state) inference + direct supply test + event-study leads
│   └── _outputs/                     gitignored intermediates (see below); the numeric source of truth for the paper
│       ├── tables/*.csv              Every headline number lands here as a CSV first
│       ├── logs/*.log               Per-script run logs (sink)
│       ├── fact_headline_ratios.rds  RQ1 ratios (also .csv)
│       ├── hazard_km_curves.rds      RQ2 KM curves for F3 (also .csv)
│       ├── prop19_did_volume.rds     RQ3 volume DiD fixest models → T4
│       ├── prop19_ddd_hazard.rds     RQ3 sold24 DiD/DDD fixest models → T4
│       ├── prop19_absentee_did.rds   RQ3 absentee composition model → T4
│       ├── ref_perm_volume.rds       07 permutation p-values for volume DiD
│       └── ref_*.rds / ref_*.csv     07 estate-seller test, event study, reconciliation
│
├── scripts/python/, scripts/julia/   present but unused (only .gitkeep) — this project is pure R
│
├── manuscript/
│   ├── paper.tex                     The draft: abstract, intro, §2 data, §3 facts, §4 lock, §5 model, §6 Prop19, §7 magnitudes, §8 concl
│   ├── appendix.tex                  Data construction; permutation-p summary; trust-inclusive robustness; raw 2×2 means (Table A2)
│   ├── paper.pdf / .bbl / .aux ...   compiled artifacts (3-pass XeLaTeX + bibtex)
│   ├── refs-note.md                  bibliography working notes
│   ├── tables/
│   │   ├── T1_taxonomy.tex           ← 05 ← fact_validation_moments.csv   (event classes × fingerprints)
│   │   ├── T2_volumes.tex            ← 05 ← fact_headline_ratios.csv      (annual volumes + ratio)
│   │   ├── T3_hazard.tex             ← 05 ← hazard_sold_within.csv        (sold-within 12/24/36/60m by class)
│   │   └── T4_prop19.tex             ← 05 ← prop19_did_volume/ddd_hazard/absentee_did.rds (the 5-column regression table)
│   └── figures/
│       ├── F1_volumes.pdf            ← 06 ← fact_headline_ratios.csv      (deed events by class, 2007–2023)
│       ├── F2_state_ratio.pdf        ← 06 ← fact_state_class_2017_2023.csv (family:market ratio by state; CA leads)
│       ├── F3_km_curves.pdf          ← 06 ← hazard_km_curves.rds          (KM survival to first market sale)
│       ├── F4_prop19_bunching.pdf    ← 06 ← prop19_ca_counterfactual_monthly.csv (the bunching plot — "crown jewel")
│       └── F5_sold24_cohorts.pdf     ← 06 ← prop19_cohort_cells.csv       (sold-within-24m, CA vs rest, by cohort)
│
├── slides/seminar.tex                Beamer source of truth (seminar deck)
│
└── quality_reports/
    ├── research_spec.md (see root)   — the spec also lives at project root
    ├── lit_review_family_transfers.md  /lit-review output (bequests, Prop 13/19, transfer-tax notch, misallocation)
    ├── reviews/
    │   ├── 2026-06-10_methods_referee.md  Major Revision 59/100; C1 single cluster, C2 retitle contamination, C3 release unobserved
    │   └── 2026-06-10_r_reviewer.md       2 CRIT / 8 MAJ / 16 MIN code review; dedupe determinism, schema drift, ZIP, join fan-out
    └── session_logs/
        └── 2026-06-09_first-draft-sprint.md  zero→draft sprint + the v2 rebuild log (read this for "why is the code shaped this way")
```

**Data store (outside the project folder):**
```
data/derived/03_family_homes/events.parquet   ← built by 01; the ONLY data 02/03/04/07 read
data/corelogic_extracts/by_state/ot/state=XX/year=YYYY/*.parquet   raw deeds (read by 01 only)
data/corelogic_extracts/by_state/prop/state=XX/*.parquet           property chars (read by 02 only, for correlates)
```

---

## 3. The pipeline: data → code → output → paper

```
C:\CoreLogic\…  (RAW, READ-ONLY — never touched by project code)
        │  one-time: shared_utils/R/convert_raw_to_parquet.R
        ▼
data/corelogic_extracts/by_state/{ot,prop}/…/*.parquet   (partitioned parquet store)
        │
        │  00_setup.R   sources shared_utils/R/corelogic_loader.R + sets paths/constants/duckdb helpers
        ▼
┌──────────────────────────────────────────────────────────────────────────┐
│ 01_build_panel.R   ★ THE KEYSTONE                                          │
│   reads  OT parquet via duckdb glob (ot_parquet_glob())                    │
│   does   slim+type cast → classify (taxonomy) → dedupe (1 row/clip-date)   │
│   writes data/derived/03_family_homes/events.parquet  (~148.7M rows)       │
│          + audit_class/name/absentee_by_year.csv + schema-drift guards     │
└──────────────────────────────────────────────────────────────────────────┘
        │ events.parquet is the single source of truth for everything below
        ├───────────────┬───────────────┬───────────────────────────────┐
        ▼               ▼               ▼                               ▼
   02_facts.R       03_hazard.R     04_prop19.R                    07_referee_checks.R
   (RQ1)            (RQ2)           (RQ3 main)                     (RQ3 inference + direct test)
   + prop join                     uses fixest                    uses fixest
        │               │               │                               │
        ▼               ▼               ▼                               ▼
   _outputs/tables/*.csv  +  _outputs/*.rds  (the numeric source of truth)
        │
        ├──────────────► 05_tables.R   → manuscript/tables/T1–T4.tex     (reads _outputs only)
        └──────────────► 06_figures.R  → manuscript/figures/F1–F5.pdf    (reads _outputs only)
                                 │
                                 ▼
                          manuscript/paper.tex   \input{tables/…}, \includegraphics{figures/…}
                                 │  3-pass XeLaTeX + bibtex
                                 ▼
                          manuscript/paper.pdf
```

**How CoreLogic is read (the loader protocol — see `.claude/rules/corelogic-data-protocol.md`).**
`C:\CoreLogic\` is **read-only**; no project code writes there. All reads go through the
single seam `shared_utils/R/corelogic_loader.R` (`load_corelogic_ot/prop/baseline_oh`),
which serves the partitioned parquet store and falls back to raw streaming. *This
project's twist:* 01/02 do not call `load_corelogic_ot()` directly — for a one-pass
148M-row scan they use **duckdb over the same parquet glob**, via helpers defined in
`00_setup.R` (`ot_parquet_glob()`, `prop_parquet_glob()`, `open_corelogic_duckdb()`).
That keeps the read pointed at the canonical parquet store (the loader's format) while
getting duckdb's out-of-core SQL. **Derived data** lives at
`data/derived/03_family_homes/events.parquet` (gitignored); `_outputs/` holds the CSV/RDS
the paper quotes. Dev mode: `FAM_DEV=1` restricts 01 to VT+RI and writes `events_dev.parquet`.

**Output → paper crosswalk (memorize this):**

| Paper exhibit | Built by | From `_outputs/` artifact | Upstream script |
|---|---|---|---|
| **T1** taxonomy + fingerprints | 05 | `fact_validation_moments.csv` | 02 |
| **T2** annual volumes + ratio | 05 | `fact_headline_ratios.csv` | 02 |
| **T3** sold-within by class | 05 | `hazard_sold_within.csv` | 03 |
| **T4** Prop 19 regressions (5 cols) | 05 | `prop19_did_volume.rds`, `prop19_ddd_hazard.rds`, `prop19_absentee_did.rds` | 04 |
| **F1** volumes by class | 06 | `fact_headline_ratios.csv` | 02 |
| **F2** state family:market ratio | 06 | `fact_state_class_2017_2023.csv` | 02 |
| **F3** KM survival | 06 | `hazard_km_curves.rds` | 03 |
| **F4** bunching plot | 06 | `prop19_ca_counterfactual_monthly.csv` | 04 |
| **F5** sold24 by cohort | 06 | `prop19_cohort_cells.csv` | 04 |
| Permutation *p*-values (text + appendix) | — | `ref_perm_volume.csv`, `ref_perm_ddd.csv`, `ref_estate_seller_did.csv` | 07 |

---

## 4. The 60-minute learning path

Front-loaded on the *conceptual core* (the taxonomy in 01, the wedge model in §5) and
the *single-treated-cluster inference idea* (07 permutation). Ends hands-on.

### 0–8 min — The pitch and the question
- **Read:** `README.md` (whole), then `research_spec.md` "Motivation (the Glaeser
  pitch)" + "Taxonomy (frozen before regressions)".
- **Why:** Lock in the one-sentence RQ and the three sub-questions before any code.
  The taxonomy is *frozen in the spec* — the code merely implements it. Note that
  trust self-transfers are *never* counted as family transfers (the headline-honesty
  rule).

### 8–18 min — The keystone: taxonomy + national event panel (`01_build_panel.R`)
- **Read:** `scripts/R/01_build_panel.R` top to bottom. Focus:
  - The `ot_slim` view (lines ~53–99): which raw columns survive, the `TRY_CAST`
    typing, the `fips5`/ZIP normalization (the `.0` DOUBLE-rendering join bug), the
    residential + date filters.
  - The `ot_classed` CASE ladder (lines ~124–134): **this is the taxonomy.** Order
    matters — `family_trust` → `family_retitle` → `family_person` → `family_estate` →
    `family_other` → `market_sale` → … `same_surname`, `same_person`, `trust_kw`,
    `estate_kw` are the name-string features that turn CoreLogic's flags into classes.
  - The dedupe `ROW_NUMBER() OVER (PARTITION BY clip, sale_raw ORDER BY …)` (lines
    ~153–159): note the *total* ordering (class, dtype, names) added for
    bit-reproducibility.
  - The schema-drift **guards** (lines ~176–190): `interfam` NULL count = 0, interfam
    share in [0.10, 0.60], ≥51 states. A `TRY_CAST` silently NULLing a flag would
    misroute family events — these assertions catch it.
- **Why:** Everything downstream reads only `events.parquet`. If you understand this
  file, you understand what every number in the paper is made of. This is *the* single
  most important file.

### 18–26 min — RQ1 national facts (`02_facts.R`)
- **Read:** `scripts/R/02_facts.R`. Focus: `fam_classes` definition (trust excluded);
  the headline-ratios query (lines ~50–68 → `fact_headline_ratios.csv`); the prop join
  (`prop_slim` deduped to 1 row/clip, `match_rate` asserted ≥ 0.5 → 61.2% actual). Map
  each query to T1/T2/F1/F2.
- **Why:** RQ1 is the paper's measurement spine; this is where "1 per 3.6 market sales"
  and "CA = 2.3× national" come from.

### 26–34 min — RQ2 the family lock (`03_hazard.R`)
- **Read:** `scripts/R/03_hazard.R`. Focus: cohort restriction 2008–2018 (≥66 months
  follow-up), the `spells` table (LEFT JOIN cohort to *next* market sale), the
  **exit-table KM algorithm** (lines ~116–153) — exits counted per month in SQL, risk
  sets built cumulatively in R to avoid a grid×spells cross join. Read off the headline:
  62% of same-surname transfers unsold at 10y.
- **Why:** The "lock" is the descriptive payoff that motivates the causal section; the
  KM construction is a reusable pattern.

### 34–44 min — The wedge model (paper §5) + RQ3 main (`04_prop19.R`)
- **Read:** `paper.tex` §5 "A Simple Framework" (eq. 1, **eq. 2 — the keep-or-sell
  wedge**, and the "recorded family transfer is itself the outcome" paragraph). Then
  `scripts/R/04_prop19.R`:
  - (a) bunching counterfactual (lines ~62–89): CA(2019, same month) × donor growth;
    excess in the Nov2020–Feb2021 window → `prop19_bunching_summary.csv`.
  - (b) volume DiD (lines ~94–126): `feols(log(n_fam) ~ ca:post | state + sale_year)`;
    market-sale placebo; trust-inclusive robustness.
  - (c) sold24 DiD/DDD (lines ~131–210) and (d) absentee DiD.
- **Why:** Eq. 2 is *why Prop 19 is the experiment*. The code section (b) is the
  headline causal estimate (−0.425 fam, −0.163 placebo → −0.263 net).

### 44–52 min — Single-treated-cluster inference (`07_referee_checks.R`)
- **Read:** `scripts/R/07_referee_checks.R`. Focus: **(a) permutation inference**
  (lines ~64–100) — `perm_coef()` reassigns the "treatment" to each donor state in
  turn, re-estimates the DiD, and ranks CA's coefficient in the absolute-value
  distribution (`p_rank`). Then **(b) the direct supply test** (estate/trust-seller
  market sales, CA vs donors → the "where are the missing homes?" null) and **(c)** the
  event-study leads.
- **Why:** California is the *only* treated state, so clustered SEs are not
  interpretable (Conley–Taber logic; this was referee CRITICAL C1). The permutation
  *p*-values — not the CRVE SEs in T4 — are the paper's actual basis for inference.
  This is the most important methodological idea in the empirical section.

### 52–60 min — HANDS-ON: trace a headline number end-to-end
Pick the bunching number and follow it from CSV → script → paper.

1. **Open the output.** `_outputs/tables/prop19_bunching_summary.csv`:
   `excess_antic_window = 28866.45`, `excess_pct = 30.51`.
2. **Find where it was computed.** `04_prop19.R` lines ~75–88: the `window_antic`
   filter (`ym 202011–202102`) and `bunch <- tibble(excess_antic_window = sum(...$excess), …)`.
3. **Find it in the paper.** `paper.tex` abstract/intro: "Family transfers surged 31
   percent above counterfactual… an excess of 28,866 deeds." (§6.2 repeats: actual
   123,486 vs counterfactual 94,620.)
4. **(Optional) reproduce a second one yourself.** Sum the volume columns of
   `_outputs/tables/fact_headline_ratios.csv`: `fam_broad` sums to 19,686,227 and
   `market` to 71,076,255, so the ratio is 0.277 and `1/0.277 = 3.61` — exactly the
   abstract's "nearly 20 million… one family transfer for every 3.6 market sales." (Run:
   `Rscript -e "d<-read.csv('projects/03_family_homes/scripts/R/_outputs/tables/fact_headline_ratios.csv'); sum(d$market)/sum(d$fam_broad)"`.)
- **Why:** This closes the loop — paper claim ↔ `_outputs/` CSV ↔ generating script —
  which is exactly the cross-artifact dependency graph the repo's review protocol
  enforces. After this you can audit any number in the paper.

---

## 5. Key files (understand these and you own the project)

1. **`scripts/R/01_build_panel.R`** — the keystone. The frozen taxonomy (the CASE
   ladder), the dedupe, the schema guards. Every downstream number is a query against
   the `events.parquet` this file writes.
2. **`research_spec.md`** — the contract. The taxonomy, the five exhibits, and the
   identification threats were all decided here *before* regressions. When code and
   paper seem to disagree, the spec is the tiebreaker.
3. **`manuscript/paper.tex` §5 (the model) + the intro** — the economics. Eq. 2
   (`u_h ≥ u_m − t(p − A₀)`, the keep-or-sell wedge) is why Prop 19 identifies anything,
   and the "deed is endogenous to the tax regime" paragraph is why the supply release is
   an *extensive-margin* (missing-transfers) prediction, not a faster-resale one.
4. **`scripts/R/07_referee_checks.R`** — the inference. Placebo-state permutation is the
   real basis for the causal *p*-values (single treated cluster ⇒ CRVE invalid), plus
   the direct supply test whose null grounds the "deferral" reading.
5. **`scripts/R/04_prop19.R`** — the causal engine. Bunching counterfactual + volume DiD
   + sold24 DDD + absentee DiD; produces the RDS objects that become Table 4 and the
   monthly CSV behind Figure 4.

*Honorable mention:* `00_setup.R` (the only path-touching file; duckdb helpers + window
constants) and `quality_reports/session_logs/2026-06-09_first-draft-sprint.md` (explains
*why* the code is shaped the way it is — the v2 rebuild added `family_retitle`,
deterministic dedupe, ZIP/fips normalization, and the permutation inference in response
to the two archived review reports).
```
```
