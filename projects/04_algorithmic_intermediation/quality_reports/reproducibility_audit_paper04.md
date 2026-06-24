# Reproducibility Audit — Project 04 (Algorithmic Intermediation)

**Manuscript:** `projects/04_algorithmic_intermediation/manuscript/paper.tex` + `appendix.tex`
**Artifacts:** `projects/04_algorithmic_intermediation/scripts/R/_outputs/*.rds`, `manuscript/tables/*.tex`, `data/derived/04_algorithmic_intermediation/ibuyer_deeds.parquet`
**Date:** 2026-06-11
**Method:** Every numeric claim in the prose (and every headline cell of the six generated tables) extracted and compared against values read directly from the RDS artifacts via `Rscript`/`readRDS()`. Permutation p-values were **recomputed** from the raw 200x5 t-statistic matrix (`exit_permutation.rds`) against `exit_main.rds` t-statistics. Derived arithmetic was recomputed from artifact values. Internal consistency between `key_numbers.rds` and the standalone model artifacts (`exit_main.rds`, `entry_es_models.rds`, `exit_robustness.rds`, `exit_heterogeneity.rds`, `entry_heterogeneity.rds`) was verified with `all.equal()` — all TRUE.

**Tolerances:** integers exact; coefficients/SEs within rounding at stated precision; p-values at same significance level; derived arithmetic ±5% relative or attributable to stated rounding.

---

## Summary

| Category | PASS | FAIL | UNMATCHED |
|---|---|---|---|
| Sample counts / integers (prose) | 16 | 0 | 2 |
| Exit coefficients, SEs, permutation p (prose) | 20 | 0 | 0 |
| Entry coefficients, SEs, pre-trend p (prose) | 11 | 0 | 1 |
| Heterogeneity & sorting (prose) | 3 | 0 | 0 |
| Summary stats & match validation (prose) | 9 | 0 | 0 |
| Derived arithmetic (recomputed) | 13 | 0 | 0 |
| Appendix donut estimates | 4 | 0 | 0 |
| Table cells vs RDS (T1, T2, T3, T4, T5, TA1) | 89 | 0 | 0 |
| **Total** | **165** | **0** | **3** |

External institutional facts (SEC/press-sourced) are out of artifact scope and listed at the end.

**OVERALL VERDICT: PASS.** Every artifact-verifiable numeric claim in the manuscript matches the RDS outputs within stated tolerances. Zero failures. The three UNMATCHED items are audit-scope numbers that do not actually appear in the manuscript prose (no claim to verify), not discrepancies.

---

## PASS — Sample counts / integers (exact match required)

| # | Claim (location) | Reported | Artifact | Source | Verdict |
|---|---|---|---|---|---|
| 1 | Arm's-length transactions (paper L255; abstract "42 million") | 42,382,121 | 42,382,121 | `key_numbers$n_sales_panel` | PASS |
| 2 | State-ZIP units (L255) | 18,657 | 18,657 | `key_numbers$n_zips_panel` | PASS |
| 3 | CBSAs (L255) | 525 | 525 | `key_numbers$n_cbsas_panel` | PASS |
| 4 | States (abstract, L255) | 22 | 22 | `key_numbers$n_states_panel` | PASS |
| 5 | iBuyer-linked deeds (L50, L259) | 376,256 | 376,256 | `key_numbers$n_ibuyer_deeds`; `nrow(ibuyer_deeds.parquet)` | PASS |
| 6 | iBuyer purchases (L259) | 193,054 | 193,054 | `key_numbers$n_ib_purchases` | PASS |
| 7 | iBuyer dispositions (L259) | 185,635 | 185,635 | `key_numbers$n_ib_dispositions` | PASS |
| 8 | 2021 purchases (L259) | 67,626 | 67,626 | `key_numbers$buys_2021`; recount from parquet | PASS |
| 9 | Exit sample ZIPs (L347) | 2,425 | 2,425 | `exit_sample_meta$n_zip` | PASS |
| 10 | Exit sample CBSAs (L347) | 27 | 27 | `exit_sample_meta$n_cbsa` | PASS |
| 11 | Exit ZIP-quarters, dispersion (L347) | 49,130 | 49,130 | `exit_main$disp$nobs` | PASS |
| 12 | Exit ZIP-quarters, volume (L347) | 53,343 | 53,343 | `exit_main$lvol$nobs` | PASS |
| 13 | Entry ZIP-quarters, dispersion (L330) | 264,485 | 264,485 | `entry_es_models$disp$nobs` | PASS |
| 14 | Entry ZIP-quarters, volume/prices (L330) | 320,667 | 320,667 | `entry_es_models$lvol$nobs` | PASS |
| 15 | Sorting ZIPs (L360) | 8,509 | 8,509 | `sorting_stats$n_zips` | PASS |
| 16 | Permutation reshuffles (T5 notes) | 200 | 200 | `nrow(exit_permutation)` | PASS |

## PASS — Exit design coefficients (prose; "log points" = 100 x raw)

| # | Claim (location) | Reported | Artifact (raw) | Source / term | Verdict |
|---|---|---|---|---|---|
| 17 | Volume effect (abstract "8"; L52, L343: -8.0, se 3.5) | -8.0 (3.5) | -0.080418 (0.035447) | `exit_main$lvol` `zshare:post` | PASS |
| 18 | Surviving-machine loading (L52, L343: -0.7, se 0.7) | -0.7 (0.7) | -0.0065794 (0.0069183) | `exit_main$lvol` `post:odshare` | PASS |
| 19 | Drop-liquidation volume (L52, L343, L366: -9.9, se 3.7) | -9.9 (3.7) | -0.098791 (0.036642) | `exit_rob$lvol$drop_liq` | PASS |
| 20 | Donut volume (L52, L343, App L179: -8.7, se 3.6) | -8.7 (3.6) | -0.086762 (0.036339) | `exit_donut$lvol` | PASS |
| 21 | ZIP-cluster volume (L366: -8.0, se 2.8) | -8.0 (2.8) | -0.080418 (0.027639) | `exit_rob$lvol$zip_clust` | PASS |
| 22 | Weighted volume (L366: -7.5, se 3.0) | -7.5 (3.0) | -0.074699 (0.030328) | `exit_rob$lvol$weighted` | PASS |
| 23 | Top-quartile binary volume (L366: -1.5, se 1.3) | -1.5 (1.3) | -0.015082 (0.012698) | `exit_rob$lvol$binary` `zhi:post` | PASS |
| 24 | Dispersion effect (abstract "0.8"; L52, L347: 0.84, se 1.06) | 0.84 (1.06) | 0.0083657 (0.0106325) | `exit_main$disp` `zshare:post` | PASS |
| 25 | IQR-based dispersion (L347: -0.20, se 0.63) | -0.20 (0.63) | -0.0019844 (0.0062687) | `exit_main$iqr` | PASS |
| 26 | Left tail (L347: 0.52 pp, se 0.54) | 0.52 (0.54) | 0.0051719 (0.0054140) | `exit_main$ltail` | PASS |
| 27 | Donut left tail (L347, App: 0.81, se 0.55) | 0.81 (0.55) | 0.0081311 (0.0055273) | `exit_donut$ltail` | PASS |
| 28 | Median price (L347: +1.5, se 1.8) | 1.5 (1.8) | 0.0154625 (0.0175785) | `exit_main$lmedp` | PASS |
| 29 | Dispersion, drop-liq (L366: 0.86, se 1.03) | 0.86 (1.03) | 0.0086056 (0.0103188) | `exit_rob$disp$drop_liq` | PASS |
| 30 | Dispersion, cell-min-10 (L366: 0.91, se 0.94) | 0.91 (0.94) | 0.0090763 (0.0094267) | `exit_rob$disp$cellmin10` | PASS |
| 31 | Dispersion, winsor-95 (L366: 0.73, se 1.19) | 0.73 (1.19) | 0.0072883 (0.0118696) | `exit_rob$winsor95$disp` | PASS |
| 32 | Dispersion, excl. Phoenix (L366: 0.72, se 1.20) | 0.72 (1.20) | 0.0071669 (0.0119532) | `exit_rob$disp$no_phoenix` | PASS |
| 33 | Dispersion, weighted (L366: 1.59, se 1.25) | 1.59 (1.25) | 0.0159375 (0.0124847) | `exit_rob$disp$weighted` | PASS |
| 34 | Permutation p, volume (abstract, L52, L343, L366: 0.03) | 0.03 | 0.030 | recomputed: `mean(abs(perm_t) >= abs(-2.2687))`, n=200 | PASS |
| 35 | Permutation p, dispersion (L347: 0.45) | 0.45 | 0.450 | recomputed: `mean(abs(perm_t) >= abs(0.7868))` | PASS |
| 36 | All five robustness variants "of the model's sign" (L366) | all positive | 0.0073–0.0159, all > 0 | `exit_rob$disp$*` | PASS |

Recomputed permutation p for all five outcomes (lvol 0.030, disp 0.450, iqr 0.725, lmedp 0.395, ltail 0.300) matches `key_numbers$perm_p` and T5 column (5) exactly.

## PASS — Entry design (prose)

| # | Claim (location) | Reported | Artifact (raw) | Source | Verdict |
|---|---|---|---|---|---|
| 37 | Volume ATT (L330: -2.2, se 1.3) | -2.2 (1.3) | -0.022028 (0.012984) | `entry_es_models$lvol$agg` ATT | PASS |
| 38 | Volume pre-trend p (L330: 0.05) | 0.05 | 0.0501 | `entry_es_models$lvol$pretrend_p` | PASS |
| 39 | SD-dispersion ATT (L330: +0.15, se 0.27) | 0.15 (0.27) | 0.0015279 (0.0026605) | `entry_es_models$disp$agg` | PASS |
| 40 | SD-dispersion pre-trend p (L330: 0.01) | 0.01 | 0.01135 | `entry_es_models$disp$pretrend_p` | PASS |
| 41 | IQR ATT (L50 "0.4"; L330: -0.39, se 0.22, p 0.08) | -0.39 (0.22), p 0.08 | -0.0039121 (0.0022296), p 0.08004 | `entry_es_models$iqr$agg` | PASS |
| 42 | IQR pre-trend joint p (L50, L330: 0.77) | 0.77 | 0.76754 | `entry_es_models$iqr$pretrend_p` | PASS |
| 43 | Median price ATT (L330: 1.2, se 0.4) | 1.2 (0.4) | 0.011723 (0.0037785) | `entry_es_models$lmedp$agg` | PASS |
| 44 | Median-price pre-trend p (L330: < 0.001) | <0.001 | 1.76e-06 | `entry_es_models$lmedp$pretrend_p` | PASS |
| 45 | IQR robustness range (L330: -0.26 to -0.63, all 5 negative) | -0.26 to -0.63 | sq -0.470, wt -0.256, twfe -0.577, antic -0.309, precovid -0.634 | `entry_robustness$iqr` | PASS |
| 46 | Anticipation IQR (L366: -0.31) | -0.31 | -0.0030882 | `entry_robustness$iqr$antic` | PASS |
| 47 | Pre-COVID IQR (L366: -0.63, se 0.20) | -0.63 (0.20) | -0.0063361 (0.0020210) | `entry_robustness$iqr$precovid` | PASS |

## PASS — Heterogeneity and sorting (prose)

| # | Claim (location) | Reported | Artifact | Source | Verdict |
|---|---|---|---|---|---|
| 48 | Exit dispersion terciles (L360: 0.80, 1.63, 0.13, all insig.) | 0.80 / 1.63 / 0.13 | 0.0080110 (p .651) / 0.0162774 (p .200) / 0.0012631 (p .903) | `exit_heterogeneity$disp` `zshare:post:factor(read_terc)1/2/3` | PASS |
| 49 | Entry tercile differentials (L360: -0.09, -0.22, both insig.) | -0.09 / -0.22 | -0.00091496 (p .773) / -0.0021837 (p .578) | `entry_heterogeneity$disp$terc` `read_terc::2:post`, `::3:post` | PASS |
| 50 | Sorting slope (L360: -1.28 pp per SD-log-sqft) | -1.28 | -1.27852 | `sorting_stats$slope` | PASS |

## PASS — Summary statistics and match validation (prose)

| # | Claim (location) | Reported | Artifact | Source | Verdict |
|---|---|---|---|---|---|
| 51 | Mean Zillow exposure (L90, L343, L374: 0.17%) | 0.17 | 0.168132 | `exit_sample_meta$zshare_mean` | PASS |
| 52 | 90th-pct exposure (L90: 0.45%) | 0.45 | 0.450544 | `exit_sample_meta$zshare_p90` | PASS |
| 53 | 99th-pct winsorization cap (L90: 0.87%) | 0.87 | 0.874768 | `exit_sample_meta$w99z` | PASS |
| 54 | Surviving-machine mean exposure (L90: 0.70%) | 0.70 | 0.700892 | `exit_sample_meta$odshare_mean` | PASS |
| 55 | Baseline within-cell dispersion (L374: 40.9 log pts) | 40.9 | 0.408672 | `key_numbers$mean_disp` | PASS |
| 56 | Median price (L374, T1: $173,000) | 173,000 | 173,000 | `key_numbers$median_price` (see Note N2) | PASS |
| 57 | Hedonic R2 range and mean (App L175, T1: 0.34–0.85, mean 0.60) | 0.34–0.85; 0.60 | 0.34194–0.84936; 0.59723 | `hedonic_fit_stats$r2` (330 state-years) | PASS |
| 58 | iBuyer purchase residual (L259: -2.9 log pts) | -2.9 | -0.028988 | `key_numbers$mean_ibuy_resid` | PASS |
| 59 | Disposition residual (L259: +0.8 log pts) | +0.8 | 0.0081702 | `key_numbers$mean_isell_resid` | PASS |

## PASS — Derived arithmetic (recomputed from artifact values)

| # | Claim (location) | Reported | Recomputed | Verdict |
|---|---|---|---|---|
| 60 | Volume decline at mean exposure (L343, L374: 1.4%) | 1.4 | 100 x 0.080418 x 0.168132 = 1.352 | PASS (rounding) |
| 61 | Volume decline at p90 (L343, L374: 3.6%) | 3.6 | 100 x 0.080418 x 0.450544 = 3.623 | PASS |
| 62 | Dispersion at mean exposure (L374: 0.14 log pts) | 0.14 | 100 x 0.0083657 x 0.168132/100 = 0.1407 | PASS |
| 63 | Share of baseline (L347, L374: "three-tenths of one percent") | 0.3% | 0.1407/40.867 = 0.344% | PASS (verbal rounding to tenths) |
| 64 | Upper 95% CI share of baseline (L347, L374: 1.2%) | 1.2% | 100 x 0.0302212 x 0.168132/40.867 = 1.243% | PASS |
| 65 | Dollarized point estimate (L374: ~$240) | ~$240 | 0.0014065 x 173,000 = $243.3 | PASS |
| 66 | Dollarized upper bound (L374: ~$850) | ~$850 | direct $879; via rounded 1.2%: 0.012 x 0.4087 x 173,000 = $848 | PASS (3.3% gap, within 5%; see Note N3) |
| 67 | Deed footprint at mean exposure (L343, L374: ~0.3% of cell volume) | ~0.3% | 2 x 0.168132 = 0.336% | PASS |
| 68 | rho with k=6, v_m ~ 0.3% (L376: ~2%) | ~2% | 1-(1-0.003363)^6 = 2.001% | PASS |
| 69 | rho at 10x share (L378: "approaches one-fifth") | ~1/5 | 1-(1-0.03363)^6 = 0.1855 | PASS |
| 70 | Left-tail cutoff (L269: residual < -0.25 = "roughly 22 percent below") | ~22% | 1-exp(-0.25) = 22.12% | PASS |
| 71 | Zestimate error ratio (L76: "3.6 times larger") | 3.6 | 6.9/1.9 = 3.632 | PASS (arithmetic on external inputs) |
| 72 | Inventory to liquidate (L84: "roughly 18,000") | ~18,000 | 9,790 + 8,172 = 17,962 | PASS (arithmetic on external inputs) |

## PASS — Appendix donut specification (App L179)

| # | Claim | Reported | Artifact (raw) | Source | Verdict |
|---|---|---|---|---|---|
| 73 | Volume -8.7 (se 3.6) | -8.7 (3.6) | -0.086762 (0.036339) | `exit_donut$lvol` | PASS |
| 74 | Dispersion +1.0 (se 1.1) | 1.0 (1.1) | 0.010019 (0.010913) | `exit_donut$disp` | PASS |
| 75 | IQR 0.0 (se 0.7) | 0.0 (0.7) | 0.0000736 (0.0067030) | `exit_donut$iqr` | PASS |
| 76 | Left tail +0.81 (se 0.55) | 0.81 (0.55) | 0.0081311 (0.0055273) | `exit_donut$ltail` | PASS |
| 77 | Median price +1.4 (se 2.0) | 1.4 (2.0) | 0.014059 (0.020420) | `exit_donut$lmedp` | PASS |

## PASS — Generated tables vs RDS artifacts (89 cells)

**T1_summary.tex (16 cells):** States 22; ZIP codes 18,657; CBSAs 525; ZIP-quarter cells 768,414 (= `key_numbers$n_cells_panel`); sales 42,382,121; median price 173,000; mean residual SD 0.409; mean R2 0.60; Opendoor 133,227; Offerpad 33,406; Zillow Offers 21,895; RedfinNow 4,526; total purchases 193,054; total dispositions 185,635; purchase residual -0.029; disposition residual 0.008. Firm-level purchase counts verified directly by recounting `buyer_firm` in `ibuyer_deeds.parquet` (exact match). **ALL PASS.**

**T2_entry.tex (23 cells):** All 20 coefficient/SE cells match `entry_es_models` (col 1) and `entry_robustness$*$state_qtr/weighted/twfe` (cols 2–4) at 3-decimal rounding, e.g. col(2) lvol -0.021 (0.012) vs -0.020808 (0.011698); col(3) lvol -0.031 (0.013) vs -0.031455 (0.012716); col(4) disp -0.010 (0.004) vs -0.009543 (0.004068). Observations 320,667; pre-trend p "0.050; 0.011". Significance stars consistent with artifact p-values throughout. **ALL PASS.**

**T3_exit.tex (12 cells):** All 10 coefficient/SE cells match `exit_main` tidy tables at 4-decimal rounding (e.g., -0.0804 (0.0354) vs -0.080418 (0.035447)); N column 53,343 / 49,130 matches nobs. **ALL PASS.**

**T4_heterogeneity.tex (5 cells):** 0.0080 (0.0175), 0.0163 (0.0124), 0.0013 (0.0102) vs `exit_heterogeneity$disp`; -0.0009 (0.0032), -0.0022 (0.0039) vs `entry_heterogeneity$disp$terc`. **ALL PASS.**

**T5_robustness.tex (25 cells):** All 20 coefficient/SE cells match `exit_robustness` (drop_liq, binary, zip_clust, weighted) at 4-decimal rounding; permutation p column (0.030, 0.450, 0.725, 0.395, 0.300) matches the recomputed values exactly. **ALL PASS.**

**TA1_waterfall.tex (8 cells):** Exact match with `filter_waterfall.rds`, all 8 stages: 93,909,691 / 50,187,509 / 49,494,349 / 48,756,024 / 43,577,709 / 42,535,591 / 42,535,581 / 42,382,121. **ALL PASS.**

---

## FAIL

None.

---

## UNMATCHED

| # | Item | Status |
|---|---|---|
| U1 | "8,304 entry ZIPs" (audit-scope list) | **Not claimed in manuscript prose.** Artifact value exists and is 8,304 (`entry_panel_meta$n_zip`); appears only in `quality_reports/key_numbers.md`. Nothing to verify in the paper. |
| U2 | "64/480 entered CBSAs" (audit-scope list) | **Not claimed in manuscript prose.** Artifact values 64 / 480 (`entry_panel_meta$n_cbsa_entered`, `$n_cbsa_total`); not stated in paper.tex or appendix.tex. |
| U3 | Entry left-tail pre-trend p = 0.005 (audit-scope list) | **Not claimed in manuscript prose or tables.** Artifact value 0.00511 (`entry_es_models$ltail$pretrend_p`). |

**External institutional facts (out of artifact scope — sourced from SEC filings/press, see `quality_reports/institutional_facts.md`):** $45T housing stock; ~70,000 iBuyer purchases 2021 (paper's own count 67,626 is consistent); 1.3% of U.S. purchases; 12.1% Phoenix 2021Q3; 25 metro markets / 29-month expansion; Zillow Q3-2021 9,680 bought / 3,032 sold; $304M write-down; $240–265M expected Q4 losses; ~25% workforce reduction; 9,790 homes owned + 8,172 under contract; ~7,000 homes shopped for ~$2.8B; Zestimate error rates 1.9% on-market / 6.9% off-market; Opendoor $573M write-down / $1.353B FY2022 net loss; RedfinNow exit Nov 9, 2022; Buchak et al. figures (3.1 pp discount, ~5% gross spread, >80% vs 68% R2, half of inventory resold within 3 months); entry/announcement dates. These cannot be verified against RDS artifacts and were not graded.

---

## Notes (non-failing observations)

- **N1.** `panel_build_meta$n_zips` = 18,333 (distinct ZIP5) differs from `key_numbers$n_zips_panel` = 18,657 (distinct state x ZIP5 pairs). The paper's claim is "18,657 **state-ZIP units**" (L255), which matches the correct artifact. T1's row label "ZIP codes" is loose wording for the state-ZIP count — a labeling nuance, not a numeric error.
- **N2.** L374 says "the median **exposed-ZIP** house of \$173,000." The artifact (`key_numbers$median_price` = 173,000, also T1 "Median sale price") is the median over **all** panel cells, not exposed ZIPs specifically. The number matches the artifact; the "exposed-ZIP" attribution is not separately supported by any artifact. Consider rewording or computing the exposed-ZIP median.
- **N3.** The ~$850 upper-bound dollarization reproduces exactly via the paper's rounded 1.2%-of-baseline figure (0.012 x 0.4087 x 173,000 = $848); the unrounded chain gives $879. Both are within the ±5% tolerance.
- **N4.** `ibuyer_state_summary.rds` columns are total deeds (purchases + dispositions; column sums 376,256), not purchases. T1 Panel B's firm purchase split was therefore verified by recounting `buyer_firm` in `data/derived/04_algorithmic_intermediation/ibuyer_deeds.parquet` — exact match.
- **N5.** Cross-artifact consistency: `key_numbers.rds` blocks are bit-identical (`all.equal` TRUE) to the standalone artifacts they summarize (`exit_main`, `entry_es_models$agg`, `exit_robustness`, `exit_heterogeneity`, `entry_heterogeneity$disp$terc`), so the table generator and the prose draw from a single consistent source.
