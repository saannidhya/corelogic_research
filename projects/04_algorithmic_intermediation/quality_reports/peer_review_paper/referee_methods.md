# Referee Report — Referee B (Methods)

**Paper:** "The Machine in the Market: Algorithmic Intermediation and Price Discovery in the Housing Market"
**Journal:** Quarterly Journal of Economics
**Disposition:** CREDIBILITY
**Critical peeve:** DiD without a pre-treatment covariate balance table across treatment intensity
**Constructive peeve:** Threat-motivated robustness over robustness theater
**Date:** 2026-06-11

## Summary

The paper asks whether algorithmic intermediaries (iBuyers) improved liquidity and price discovery for third parties in the U.S. housing market, and answers using the abrupt November 2021 Zillow Offers shutdown. Exposure is Zillow's share of ZIP transactions over 2018Q2–2021Q3; the control loading is the analogous share for the surviving machines (Opendoor + Offerpad); identification is within CBSA-by-quarter, off *which* machine a ZIP was exposed to. The headline finding is a liquidity withdrawal: arm's-length volume fell 8.0 log points per percentage point of Zillow exposure (s.e. 3.5; permutation p = 0.03), with a null on the surviving-machine loading (−0.7, s.e. 0.7); the titular price-discovery effect is an undistinguishable-from-zero bound (+0.84 log points, s.e. 1.06) that the paper argues is the magnitude its model predicts at the machine's ~0.3 percent share of trade. The design idea — surviving-machine exposure as the counterfactual loading — is genuinely clever, the entry design is honestly demoted to descriptive when its pre-trends fail, and the replication apparatus is exemplary (an independent audit verifies 165/165 numeric claims). But the exit design, on which the paper stakes everything, is currently under-defended in exactly the places where it is most vulnerable: there is no covariate balance evidence for the Zillow-tilt-vs-Opendoor-tilt contrast; the volume outcome mechanically contains the machine's own deeds, so a non-trivial fraction of the headline coefficient is subtraction, not contraction; the continuous-dose result is not shown to be monotone in dose and its one binary check is null; and the inference package (27 clusters, 200 permutation draws of unexamined exchangeability) cannot yet certify p = 0.03. All of my requested fixes are feasible with data the authors already have — several are one regression away in their own scripts.

## Pre-scoring sanity checks (reduced-form)

| Check | Verdict | Evidence |
|---|---|---|
| Estimand matches question | PASS (with note) | Title question (price discovery) is answered by a bound; headline causal claim is liquidity. Disclosed candidly. |
| Sign check | PASS | Volume falls on exit; dispersion/tail signs match the model. |
| Magnitude check | PARTIAL | "Several times the deed footprint" rests on a volume outcome that *includes* the machine's deeds (Major 2). |
| Dynamics / pre-trends | FAIL | Exit event study's pre-period "is the treatment switching on." No joint pre-trend test for any exit outcome. |
| Clustering matches assignment | PARTIAL | CBSA clustering right, but G = 27 with no wild-cluster bootstrap. |
| Sample composition stability | PARTIAL | Dispersion panel loses ~8% of cells to n>=5; inclusion plausibly treatment-correlated post-exit; untested. |
| Covariate balance shown | FAIL | No balance table anywhere (Major 1). |

**Two checks FAIL → composite capped at 70.**

## Dimension scores

| # | Dimension | Weight | Score | Weighted |
|---|---|---|---|---|
| 1 | Identification | 45% | 58 | 26.1 |
| 2 | Estimation / outcome construction | 20% | 65 | 13.0 |
| 3 | Inference | 20% | 60 | 12.0 |
| 4 | Robustness | 10% | 62 | 6.2 |
| 5 | Replication | 5% | 92 | 4.6 |
| | **Composite** | | | **62** |

## Major concerns

### 1. The ODshare control's validity is asserted, never shown: no balance table, and an untested mean-reversion confound
The design's whole weight rests on the claim that, within CBSA-quarter and conditional on overall iBuyer loading, Zillow-tilted ZIPs would have trended like Opendoor-tilted ZIPs after 2021Q4, with zero evidence of pre-treatment comparability. Zillow entered later (2018 vs 2014), scaled most in 2021 under Project Ketchup, overriding its algorithm to chase volume — so Zshare is disproportionately weighted toward 2021-froth ZIPs that would mean-revert hardest in 2022-23 with or without the shutdown. ODshare absorbs this only if Opendoor selected on the same froth, but the paper argues Opendoor was disciplined.
**What would change my mind:** (i) A balance table across Zshare terciles (within-CBSA, conditional on ODshare): 2019 price level, stock age/homogeneity, SFR share, investor/cash shares, 2019–2021Q3 appreciation, 2020–21 volume growth, rate-sensitivity proxy, with normalized differences. (ii) Split exposure into Zshare(2018Q2–2020Q4) and Zshare(2021Q1–Q3); show the volume effect loads on the pre-Ketchup component; and/or control for 2020–21 appreciation × quarter FE.

### 2. The volume outcome contains the machine's own deeds — part of the headline is mechanical
`n_sales = n()` counts every filtered arm's-length deed, including iBuyer purchases and resales; `lvol = log(n_sales)`. Per pp of Zshare, Zillow's deeds are ~1% (purchases) + ~1% (resales) of window transactions, present pre and absent post — a mechanical contribution on the order of −1.5 to −2 of the reported −8.0. The clean outcome is one line: `log(n_sales − n_ib_buy − n_ib_sell)`.
**What would change my mind:** Re-estimate volume DiD, event study, and donut with household-only volume; restate the multiplier on that basis; report joint pre-trend tests (excluding ramp quarters) for every exit outcome.

### 3. The continuous-dose result is not shown monotone; the binary check is null; computed robustness unreported
Exposure is right-skewed (mean 0.17, p90 0.45, cap 0.87); the continuous slope is identified by the tail. Top-quartile binary = −1.5 (s.e. 1.3), about half what the slope implies. Section 5.2 promises tercile dose-response that never appears. Script computes winsor-95 and excl-Phoenix for ALL outcomes but the manuscript reports them only for dispersion.
**What would change my mind:** (i) Exposure-tercile/quartile-bin dummies × Post, monotone in dose; (ii) full robustness grid for all outcomes incl. volume; (iii) dose-gap arithmetic reconciling binary and continuous.

### 4. Inference: few clusters, too few permutation draws, exchangeability in dispute
G = 27, no wild cluster bootstrap. At p̂ = 0.03 over 200 draws the MC interval straddles 0.05. Reshuffling Zshare within CBSA destroys the observable-correlation and spatial autocorrelation the sorting result documents.
**What would change my mind:** (i) Wild cluster bootstrap (Webb weights); (ii) >=999 draws, p = (1 + #exceed)/(B+1); (iii) a scheme respecting the assignment mechanism, or a pre-period placebo permutation centered on zero.

### 5. Hedonic-dispersion outcome: treatment-correlated cell selection and noise
Within-cell demeaning is well done (disposes of generated-regressor worry). Remaining: (1) cells require n>=5; high-exposure ZIP-quarters differentially drop post-exit → treatment-selected sample; (2) heteroskedastic noise (n=5 → cell-SD SE ~35% of sigma); weighted estimate nearly doubles; (3) left-tail text says raw residual < −0.25 but code uses demeaned.
**What would change my mind:** (i) extensive-margin regression P(n>=5) on Zshare × Post; (ii) balanced-cell variant; (iii) state preferred weighting and dollarize on it; (iv) reconcile left-tail text vs code.

## Minor concerns
1. `read_terc = ntile(readability, 3)` computed on ZIP-quarter rows, not distinct ZIPs.
2. Undisclosed filters `sales_w >= 50`, `stock_resi >= 500`; report # zero-exposure control ZIPs.
3. Reference-quarter 2021Q3 is Zillow's heaviest buying; show donut event study re-referenced to 2020Q4.
4. Entry IQR-dispersion is post-hoc outcome selection; needs multiple-testing caveat.
5. §4.3 "all outcomes from non-iBuyer trades" contradicted by volume construction.
6. Report ODshare placebo loading for every robustness variant.
7. QJE tables: three-decimal point estimates with SEs; report # clusters in every note.
8. State winsorization mechanics in notes.

## Positive observations
- Reproducibility discipline is the best I have seen: independent audit 165/165, seeds set, permutation matrix shipped, every exhibit traces to a numbered script.
- Honest demotion of the entry design when pre-trends failed — exactly right and rare.
- Robustness checks that exist are threat-named, not theater.
- Entity-matching validation (67,626 vs ~70,000; −2.9 log pts reproducing Buchak et al.'s −3.1pp) is a model of measurement validation.
- Within-cell demeaning quietly disposes of the ZIP-year generated-regressor problem.

## Assessment and recommendation
**Composite score: 62/100** (capped at 70 by two failed sanity checks).
**Recommendation: MAJOR REVISION.** The exit design is a genuinely good idea and the empirical infrastructure is unusually trustworthy, but the central identifying assumption rests on institutional narrative alone, the headline coefficient is partly mechanical, the dose is undefended, and inference cannot yet certify p = 0.03. Every fix is feasible with data and code already in hand. If household-volume survives in −5 to −7, balance and vintage-split come back clean, dose-response is monotone, and inference holds at 999+ draws with a wild bootstrap, this becomes a strong paper.
