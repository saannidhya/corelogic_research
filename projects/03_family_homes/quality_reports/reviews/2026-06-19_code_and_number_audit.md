# Code Review + Number Audit (v2) — 2026-06-19

**Reviewer role:** Combined R code reviewer (review-r) + reproducibility auditor (audit-reproducibility).
**Paper:** `projects/03_family_homes/manuscript/paper.tex` (v2 draft, Preliminary).
**Scope:** Scripts 00–07; outputs in `scripts/R/_outputs/`; every numeric claim in paper.tex + appendix.tex + T1–T4.
**Constraint honored:** Did NOT run the 01–07 CoreLogic pipeline. All verification is against existing `_outputs/` CSV/RDS plus tiny throwaway recomputations (ratios, `exp(coef)-1`, KM survival reads). No write/modify under `C:\CoreLogic`.

**Verdict:** PASS WITH NOTES. Numbers are exceptionally clean — 41 of 43 traced claims PASS, 0 FAIL, 2 UNVERIFIABLE (1 external citation, 1 appendix coverage figure with no output source). Code is well-engineered and reproducible. The one persistent issue is a **protocol divergence (M8): the pipeline bypasses the CoreLogic loader**, reading the 148M-row parquet store via project-local duckdb globs. No correctness consequence, but it violates the single-seam contract and remains OPEN.

---

## JOB A — R CODE REVIEW

### Status of prior (v1) R-review findings

| ID | v1 issue | v2 status | Note |
|----|----------|-----------|------|
| C1 | single-treated-cluster CRVE | **RESOLVED** | 07 implements placebo-state permutation for volume DiD (fam/market/net), estate-seller DiD, and sold24 DDD; text + T4 note rely on permutation p, not CRVE. |
| C2 | non-deterministic dedupe (tied same-day rows) | **RESOLVED** | 01 `ROW_NUMBER()` ordering is now total: `pcat, amt DESC NULLS LAST, class, dtype NULLS LAST, b1_full, s1_full`. Reproducible across thread counts. |
| M1 | month-00 leaks into ym/bunching | **RESOLVED** | 01 WHERE clause `sale_month BETWEEN 1 AND 12`; guard `n_bad_month == 0`. |
| M2 | TRY_CAST/union_by_name could NULL interfam silently | **RESOLVED** | 01 hard guards: `n_interfam_null == 0`, `interfam_share ∈ (0.10, 0.60)`, `n_states ≥ 51`. |
| M3 | ZIP leading zeros / 9-digit numeric ZIPs | **RESOLVED** | 01 length-aware lpad for both ZIP fields (3–5 → pad to 5; 8–9 → pad to 9, take prefix). |
| M4 | prop join fan-out | **RESOLVED** | 02 `prop_slim` deduped to 1 row/clip via ROW_NUMBER; `stopifnot(match_rate$n_events == n_ev_recent)` enforces 1:1. |
| M5 | KM pooled nationally vs state×year | **PARTIAL (as disclosed)** | Still pooled; paper adds a four-point caveat paragraph (sec 4) and says stratified KM is "planned for the next draft." Acceptable for a Preliminary draft; remains a real limitation. |
| M6 | log() zero-cell silent drop | **RESOLVED** | 04 `stopifnot(all(n_fam>0), all(n_market>0), all(n_fam_wtrust>0))` before `feols(log(...))`. |
| M7 | DDD post-cohort selection / retiming | **RESOLVED (framing)** | Estimand stated explicitly in text (policy-relevant total effect on post cohort). |
| M8 | duckdb helpers duplicate loader; bypass protocol | **OPEN** | See Critical/Major below. Still not promoted to `shared_utils`; project reads CoreLogic parquet directly via `ot_parquet_glob()` / `open_corelogic_duckdb()`. |

Minor items from v1 (ADMINISTRAT% over-match, GROUP BY ALL clip bug, F3 limits) are confirmed FIXED in code: 01 uses `%ADMINISTRATOR%`/`%ADMINISTRATRIX%` (not `ADMINISTRAT%`), 04 carries an explicit comment that `clip` must be in the spells SELECT list (the 0.65-vs-0.10 bug), and 06 F3 uses `scale_y_continuous(limits=...)`.

### NEW findings

**CRITICAL:** none.

**MAJOR**

- **A-M1 (CoreLogic loader bypass — protocol violation; = prior M8, OPEN).** `00_setup.R` defines `ot_parquet_glob()`, `prop_parquet_glob()`, and `open_corelogic_duckdb()`, and scripts 01/02/04/07 read the raw parquet store directly with `read_parquet({glob_sql})` over `data/corelogic_extracts/by_state/{ot,prop}/...`. `.claude/rules/corelogic-data-protocol.md` states every CoreLogic read from project code must go through `shared_utils/R/corelogic_loader.R`, and that project code "never calls `arrow::open_dataset()`, `duckdb::dbConnect()`, ... against a CoreLogic path directly." This is a genuine divergence. Two honest caveats on severity: (i) it touches the parquet store under repo `data/`, not raw `C:\CoreLogic\` (read-only contract is not violated); (ii) the existing loader (`load_corelogic_ot()`) materializes into an R tibble via arrow, which is infeasible for a 148M-row single-pass aggregation — so the *spirit* (one tested seam, swap-able backend) is right but the loader has no streaming/duckdb entry point to satisfy it. Correct fix is to add a duckdb-returning function to the loader (e.g., `corelogic_duckdb_view()`) and route scripts through it, not to keep a private copy in `00_setup.R`. No numeric consequence; classified Major because it is an explicit, repeated rule break that the prior review already flagged and that the loader could be extended to honor.

**MINOR (new, 6)**

- **A-m1.** 02 `value_q` filters `total_value > 1000` and 03/04 use the prop join, but the value-quintile and age-profile correlates rest on a 61% prop match (`fact_prop_match_rate.csv` = 0.6094). The match-rate guard (`< 0.5 → stop`) protects against degeneracy but the paper does not flag that the Fact-3 correlates are computed on the matched 61% subsample, which could be selected (older/owned-longer parcels match better). Worth a footnote; the appendix discloses the 61% match but not its potential selectivity for the age/value gradients.
- **A-m2.** 03 `hazard_sold_within.csv` includes `family_retitle` (n=4,125,877) but the cohort total in 03 differs from the T3-reported `family_retitle` claim in the text (text/sec 4 cites retitle 9.1% vs trust 8.9% at 24m). The CSV gives retitle 24m = 0.0905 (9.1%, rounds correctly) and trust 0.0893 (8.9%) — PASS — but note 03's `cohort` includes `family_retitle` while the T3 table (05_tables.R) filters it out. Intentional and consistent, just easy to misread.
- **A-m3.** 02 `med_pos_price` uses duckdb `median()` (exact, fine) but T1's `family_estate` median ($596k, n=1,905) and `estate_noninterfam` rows are reported in T1.tex though omitted from the paper's narrative; these tiny classes (n=665 in hazard, n=1,905 in taxonomy) carry near-zero weight but appear in the published T1 — acceptable, but the `family_estate` row (1,905 events) invites a "why is estate median 6x market?" reader question with no footnote.
- **A-m4.** 04 counterfactual `ca_cf = ca_base * dn_actual / dn_base` anchors to CA's 2019 calendar-month pattern. If any 2019 CA month had `dn_base` or `ca_base` anomalies the ratio is sensitive; no guard on `dn_base > 0`. In practice all months are large, so no failure, but a `stopifnot(all(dn_base > 0))` would harden it.
- **A-m5.** Repeated literal territory vector `c("GU","PR","VI","AS","MP","AE","AP","AA","FM","MH","PW")` appears in 04 (twice), 06, 07 — should be a single constant in `00_setup.R` (07 already names it `territories`; 04/06 inline it). DRY/maintainability.
- **A-m6.** `set.seed()` is set in every script (good) but nothing downstream is stochastic except the permutation inference, which is a deterministic full enumeration over 51 states (no sampling) — so the seeds are inert. Harmless, but the seed gives a false impression that results depend on RNG; a one-line comment that permutation is exhaustive (not Monte Carlo) would clarify.

**Strengths worth recording.** Degeneracy guards are exemplary and clearly motivated by MEMORY.md lessons (`assert_rows`, match-rate `stop`, `n_spells > 1e6`, `interfam_share` band, the documented GROUP-BY-ALL clip comment). The dedupe ordering is fully total. The KM exit-table construction (count exits in SQL, build risk sets cumulatively in R) is correct and avoids a grid×spells cross join — I checked the risk-set algebra: `r_t = n_total - lag(cumsum(fail)) - lag(cumsum(censor))`, `surv = cumprod(1 - fail/r_t)`, which is the standard KM estimator. Permutation inference correctly uses `feols` without cluster SE on the placebo runs (coefficient-only) and ranks |CA| in the |distribution|.

---

## JOB B — NUMBER AUDIT

Tolerance applied: counts EXACT to stated precision; percentages ±0.5pp; coefficients ±0.01; derived ratios within rounding.

**Headline totals.** `audit_class_by_year.csv` aggregates: total all years (incl. 2024) = 148,711,427 (paper: 148.7M ✓); 2007–2023 = 143,999,250 (paper: 144.0M ✓). Family broad 2007–2023 = 19,686,227 (paper "nearly 20 million" ✓); same-surname 13,610,459 + other 6,073,863 = 19,684,322, +estate 1,905 = 19,686,227 (paper says 13.6M + 6.1M = 19.7M ✓). `ref_reconciliation.csv`: 16,487,141 distinct fam clips (paper 16.5M ✓), deeds/clip = 1.1940 (paper 1.19 ✓).

**Class counts (T1 N column)** all match `fact_validation_moments.csv` / `audit_class` exactly: market 71,076,255; family same-surname 13,610,459; family other 6,073,863; trust 12,074,757; same-person retitle 7,123,579; other non-arms 33,980,735. (Paper rounds to 71.1M / 13.6M / 6.1M / 12.1M / 7.1M / 34.0M — all ✓.)

**Ratios.** Aggregate fam_broad/market = 0.277 → rounds to 0.28 ✓; 1/0.277 = 3.61 (paper "3.6" ✓). Yearly ratio_broad range 0.253–0.335 (paper "0.25–0.34" ✓ — the max is 0.335 in 2012, rounds to 0.34 within tolerance). 2009 = 0.294 (paper 0.29 ✓), 2021 = 0.279 (paper 0.28 ✓). Same-surname yearly 653,050–1,047,039 (paper "0.7 to 1.0 million" ✓). Existing-stock ratio 0.2961 (footnote 0.30 ✓).

**Geography.** State ratios (`fact_state_class_2017_2023.csv`): CA 0.6091 (paper 0.61 ✓), 1/0.6091 = 1.642 (paper "1.6 market sales" ✓); UT 0.5295, ID 0.5282 (paper "0.53" ✓). National pooled (2017–2023, sum/sum) = 0.2649 (paper 0.26 ✓); CA/national = 2.299 (paper "2.3×" ✓). CA trust 2,219,857 / total 5,836,530 = 0.3803 (paper "38 percent … 2.2 million" ✓).

**Fact 3 (correlates).** Age bins (fam share of fam+market): 10–29 = 19.53% (paper "20%" — rounds ✓), 30–49 = 22.43% (paper "22%" ✓), 50–74 = 25.97% (paper "26%" ✓). Senior-exempt 25.96% vs non-exempt 22.47% (paper "26 versus 22" ✓). Value quintile 1 = 25.08% → quintile 5 = 20.66% (paper "25% → 21%" ✓; monotone decline confirmed). Prop match 0.6094 (paper "61 percent" ✓).

**T1 fingerprints.** same-surname zero-price 90.29% (paper "90%" ✓); quitclaim 39.61% vs market 2.01% (paper "40% … 2%" ✓); median $97,000 vs market $224,000 (paper ✓); corp buyer share same-surname 5.66e-6 ≈ 0 (paper "essentially absent" ✓), market corp 10.24% (paper "10%" ✓).

**Hazard (`hazard_sold_within.csv` + KM).** same-surname 24m = 11.15% vs market 8.90% (paper "11.1 vs 8.9" ✓); same-surname 60m = 23.13% vs market 23.75% (paper "23.1 vs 23.7" ✓). KM year-10 (t=120) survival: same-surname surv = 0.6210 → 37.9% sold (paper "38%"), market surv 0.5773 → 42.3% sold (paper "42%"); 1−0.379 = 62.1% never sold (paper "62% / three in five" ✓). Cross-surname (family_other) sold 12m = 10.30%, 24m = 14.93% vs market 5.32%/9.10% (paper "10%/15% vs 5%/9%" ✓). Retitle 24m = 9.05% vs trust 8.93% (paper "9.1 vs 8.9" ✓). Conditional-on-surviving-2yr 30% vs 36% over next 8yr: KM (0.6210/0.8867)→ ~30% and (0.5773/0.9090)→ ~36% ✓.

**Prop 19 volume DiD (`prop19_did_volume.csv` / .rds).** fam coef −0.4253, se 0.0352 (T4 −0.4253(0.0352) ✓; exp−1 = −34.6% → paper "35%" ✓). market placebo −0.1626, se 0.0293 (T4 ✓; exp−1 = −15.0% → "15%" ✓). net = −0.4253−(−0.1626) = −0.2627 → exp−1 = −23.1% (paper "23 percent" ✓). 58,000/yr: net (1−exp)×251,256 = 58,044, and 0.23×251,256 = 57,789 (paper "roughly 58,000" ✓; gross (1−exp(−0.4253))×251,256 = 87,042 → paper "gross estimate 87,000" ✓). Trust-inclusive (appendix): −0.2405, se 0.0269 (appendix −0.241(0.027) ✓; exp−1 = −21.4% → "21 percent" ✓). T4 fit stats verified from RDS: N = 255/255/1,836/1,836/1,836; R2 = 0.98575/0.98961/0.84936/0.85147/0.90764 — all match T4.tex exactly.

**Permutation p.** `ref_perm_volume.csv`: fam p = 0.0980 (rank 5/51) → paper "p ≈ 0.10, fifth of fifty-one" ✓; net p = 0.1765 (rank 9) → "p ≈ 0.18, ninth" ✓. `ref_perm_ddd.csv`: 0.6667 → paper "0.67" ✓. `ref_estate_seller_did.csv`: levels p = 0.7059 → "0.71" ✓; share p = 0.6275 → "0.63" ✓.

**Bunching (`prop19_bunching_summary.csv`).** Nov2020–Feb2021 actual 123,486 vs cf 94,619.5 → excess 28,866.5, 30.51% (paper "123,486 vs 94,620 … excess 28,866 … 31 percent" ✓). Post-window Mar2021–Dec2022 shortfall 85,365, 15.6% (paper "85,000 … 16 percent" ✓; sec threats uses 85,365 explicitly ✓). 2023 CA fam 147,441 vs 2018–19 avg 251,256 → −41.3% (paper "147,441 … 251,256 … 41 percent" ✓). Pull-forward bound: 28,866/85,365 = 33.8% (paper "at most a third" ✓).

**T4 raw means / appendix A1 (`prop19_cohort_cells.csv` aggregated).** Family CA pre 16.06% (n=470,163), post 12.29% (n=247,417); RoUS 13.05% (n=1,793,673), 11.08% (n=1,118,028). Market CA 6.15% (n=788,627), 4.65% (n=421,807); RoUS 6.92% (n=7,722,553), 6.49% (n=4,495,875). All match appendix Table A1 exactly (16.1/12.3, 13.1/11.1, 6.2/4.7, 6.9/6.5; N's identical). DiD: fam (16.06−12.29)−(13.05−11.08) = 3.77−1.97 = 1.80pp (paper "−1.8pp" ✓); market (6.15−4.65)−(6.92−6.49) = 1.50−0.43 = 1.07pp (paper "−1.1pp" ✓); triple = 1.80−1.07 = 0.73 ≈ 0.74pp (appendix −0.74pp / DDD coef −0.007358 ✓). T4 hazard coefs −0.0178/−0.0104 ✓; DDD ca:post:fam −0.007358 ✓; absentee −0.0016, se 0.0032 (T4 −0.0016 ✓, paper "−0.2 percentage points" — −0.16pp rounds to −0.2 ✓).

**Event study (`ref_event_study.csv`).** 2018 −0.0278 ("−0.03" ✓), 2020 +0.0560 ("+0.06" ✓), 2021 −0.0110 ("−0.01" ✓), 2022 −0.3259 ("−0.33"; exp−1 = −27.8% → "28 percent" ✓), 2023 −0.4408 ("−0.44"; exp−1 = −35.6% → "36 percent" ✓), 2017 +0.1536 ("+0.15 wobble" ✓).

**Estate-seller share (`ref_ca_estate_seller_counts.csv`).** Share of CA market: pre (2017–19) 28.38% → post (2022–23) 31.91% (paper "28.4 to 31.9 percent" ✓). Levels DiD coef −0.0799 → exp−1 = −7.7% (paper "fall 8 percent" ✓); share-net coef +0.0827 → "+8 log points" (paper ✓).

**Coverage (appendix).** Absentee observed 2007 = 0.7771 (paper "77 percent" ✓), 2023 = 0.9771 (paper "98 percent" — 97.7% rounds ✓).

### Claims that did NOT fully pass

- **Appendix surname-missingness "84 percent (seller) / 66 percent (buyer)" — UNVERIFIABLE.** `audit_name_population_by_year.csv` reports `interfam_share`, `same_surname_share`, `trust_share`, `estate_share` of *all* events; it does NOT report the share of *interfamily* events with a non-missing seller/buyer surname. No output CSV/RDS contains these two numbers, and no script computes "surname non-missing conditional on interfamily." The claim may be true but is not reproducible from the committed outputs. Recommend adding a moment to 01's `audit_names` query (e.g., `AVG(CASE WHEN interfam=1 THEN (s1_last<>'') END)`).
- **LAO "60,000–80,000 inherited properties/yr" — UNVERIFIABLE-against-code (external citation).** This is a `\citep{lao2017inheritance}` figure, not a computed number; flagged per instructions. The paper's claim that 58,000 "sits inside" 60,000–80,000 is an arithmetic comparison and holds.

No FAIL: every computed claim traced lands within tolerance.

---

## Cross-artifact summary

- **Reproducibility:** PASS — 41/43 traced claims within tolerance, 0 FAIL, 2 unverifiable (1 external citation, 1 appendix coverage figure lacking an output source).
- **Code quality:** 0 critical, 1 major (loader-bypass protocol divergence, = prior M8, OPEN), 6 minor.
- **Escalation:** None. The single Major (A-M1) has no effect on any paper claim — it is a protocol/maintainability issue, filed as a follow-up, not a paper blocker.
- **Recommended before submission:** (1) route CoreLogic reads through an extended loader (add a duckdb seam to `shared_utils/R/corelogic_loader.R`); (2) add the surname-missingness moment to 01 so the 84%/66% appendix claim is reproducible; (3) footnote the 61% prop-match subsample selectivity for Fact 3; (4) deliver the stated stratified KM (M5 still PARTIAL).
