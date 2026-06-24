# Desk Review: The Machine in the Market: Algorithmic Intermediation and Price Discovery in the Housing Market

**Calibrated to:** Quarterly Journal of Economics (QJE)
**Date:** 2026-06-11
**Paper:** projects/04_algorithmic_intermediation/manuscript/paper.tex (+ appendix.tex)
**Novelty check:** OFF — Novelty probe: skipped (offline)

## Verdict

**SEND OUT**

## One-paragraph contribution statement (my understanding)

The paper asks whether algorithmic intermediaries (iBuyers) — the first agents to post firm,
capital-backed, model-generated prices in the U.S. housing market — improved price discovery and
liquidity for third parties who never traded with them. It builds an adverse-selection model of a
machine dealer (sorting into homogeneous segments; forecast-error variance as a tax on the spread;
an operational winner's curse; public-deed quotes compressing third-party price dispersion), then
tests the spillovers using the abrupt November 2021 Zillow Offers shutdown, with ZIPs exposed to the
surviving machines (Opendoor/Offerpad) as controls within metro-quarter cells. The identified
finding is a liquidity externality: arm's-length volume fell 8.0 log points per percentage point of
Zillow exposure (permutation p = 0.03), several times the machine's own deed footprint, strengthening
when wind-down quarters are excluded. The information externality is reported as a bound: dispersion
rose a statistically indistinguishable 0.84 log points per exposure point, which the paper argues is
exactly the magnitude the model's comp-window arithmetic predicts at the machine's ~0.3 percent share
of trade.

## Desk-reject analysis

Not applicable — no desk-reject criterion is met:

- **Fit:** Identification-first empirical work + theory with sharp predictions is the QJE house
  style. The exit natural experiment ("which machine was your ZIP exposed to") is the kind of design
  one could teach in a graduate class.
- **Contribution:** Statable in one sentence (externality of algorithmic market-making on third-party
  trade and price discovery, identified from a forecast-driven exit). Clear separation from
  Buchak et al. (2025), whose question is the dealer's P&L, not the market's externality.
- **No fatal design flaw visible at desk.** The entry design's failed pre-trends are disclosed and
  demoted to descriptive; identification leans entirely on the exit, where the threats (control
  validity, mechanical exposure ramp, mean reversion) are acknowledged and partially addressed
  (donut, drop-liquidation, ODshare placebo). These are referee questions, not desk kills.
- **Reproducibility:** Independent audit PASS — 165/165 artifact-verifiable numeric claims within
  tolerance, zero failures (quality_reports/reproducibility_audit_paper04.md). Permutation p-values
  were independently recomputed from the raw t-statistic matrix.
- **Theory verification:** Fresh-context proof check found one real error (Prop. 1 optimum failed on
  an Assumption-1-feasible sub-region) which was fixed pre-submission by strengthening Assumption 1
  (the s < 2w - (D-c) bound now appears in the manuscript); all other Layer-1/Layer-2 claims verify
  (quality_reports/proof_verification.md).

**Central tension (flagged for referees, not resolved at desk).** The headline causal finding is a
liquidity effect; the price-discovery (information) effect — the question in the title — is a bound
that cannot be distinguished from zero, defended via a calibration argument (k = 6 comps,
V1 = V0/2, buyer error one-third of residual variance) that the data do not pin down. Whether "the
null is exactly what the theory predicts at this scale" is a publishable QJE answer, or an
unfalsifiable save, is the substantive question this review must settle. Related magnitude puzzle:
the volume effect is several times the machine's deed footprint, but the model's Proposition 4(i)
predicts roughly one-for-one removal; the thin-market multiplier reading is imported from the
search literature, not derived in the model.

**Table format note (QJE override):** QJE convention is three-decimal point estimates with SEs in
parentheses; the manuscript reports one-decimal log points. Cosmetic; fix at revision.

## Novelty probes

Skipped (offline mode). No prior-work assertion is made; recommend the author cross-check recent
working papers on the Zillow Offers shutdown as an event study before submission.

## Send-out plan

Proceed to referee selection below. Two referees, distinct dispositions drawn from the QJE pool
(CREDIBILITY 0.40, STRUCTURAL 0.20, MEASUREMENT 0.15, POLICY 0.10, THEORY 0.10, SKEPTIC 0.05).
Draw 1: CREDIBILITY (p = 0.40). Pool re-normalized without CREDIBILITY
(STRUCTURAL 0.33, MEASUREMENT 0.25, POLICY 0.17, THEORY 0.17, SKEPTIC 0.08). Draw 2: STRUCTURAL.
Role mapping: CREDIBILITY → methods referee (QJE Identification weight 35 → 45);
STRUCTURAL → domain referee (the model's quantitative predictions are load-bearing for the
paper's interpretation of the bound).

## Referee Selection

| Referee | Disposition | Critical peeve | Constructive peeve |
|---|---|---|---|
| Referee A (domain) | STRUCTURAL | Structural estimation: parameter plausibility check required (compare to prior literature) | Rewards a clear "what this paper does not show" paragraph that honestly bounds the claims |
| Referee B (methods) | CREDIBILITY | Covariate balance absent — DiD papers without a balance table for pre-treatment covariates across treatment status | Appreciates when robustness checks are motivated by specific threats |

## Referee assignment

**Referee A — domain referee. Disposition: STRUCTURAL.**
Peeves: [critical] parameter plausibility checks required for any structural/calibration claim;
[constructive] rewards honest "what this paper does not show" bounding.
QJE domain adjustments: Substance 20 → 25 (clever > competent).
Scrutinize specifically:
1. The Section 7.1 model-magnitude matching: the claim that the dispersion bound "sits exactly
   where the model puts it" rests on assumed values (k = 6 comp window, V1 = V0/2, buyer error =
   1/3 of residual variance). Are these defensible against prior literature (e.g., AVM error rates,
   appraisal-comp practices), and would plausible alternatives let the model predict an effect the
   data *could* reject? Is the bound informative or reverse-engineered?
2. The liquidity multiplier: the model predicts ~one-for-one deed removal (Prop. 4(i)); the estimate
   is several times the footprint. Does the theory actually explain the headline number, or is the
   option-value/thin-market amplification a post-hoc graft? Should the model be extended (listing
   margin with an outside-option quote) or the claim softened?
3. Contribution positioning vs. Buchak et al. (2025) and the AVM-advisory literature: given the
   titular information externality returns bounded-at-zero, is "the externality is real, positive,
   and second-order" a general-interest contribution, and is the externality-vs-P&L distinction
   sharp enough to carry a QJE paper?

**Referee B — methods referee. Disposition: CREDIBILITY.**
Peeves: [critical] covariate balance for DiD must be shown, not asserted; [constructive]
appreciates threat-motivated (non-theater) robustness.
QJE methods adjustments: Identification 35 → 45; Robustness 15 → 10.
Scrutinize specifically:
1. The ODshare control's validity: within metro-quarter, are Zillow-tilted ZIPs comparable to
   Opendoor-tilted ZIPs on pre-treatment observables (price levels, stock age/homogeneity, 2020-21
   appreciation, rate-sensitivity, Zestimate coverage)? No balance table is apparent in the
   manuscript — demand one across Zshare terciles, and probe whether Zillow's later entry (2018)
   and Project-Ketchup-era buying selected ZIPs on transitory 2021 froth.
2. The mechanical exposure ramp and mean reversion: exposure is built from realized 2018Q2-2021Q3
   purchases, so the volume event study's pre-period is the treatment switching on, and post-2021
   declines could be reversion of the very boom Zillow chased or fueled. Does the donut
   specification genuinely purge this? Why does the top-quartile binary version collapse to
   -1.5 (s.e. 1.3) — right-tail dose, or fragility of the continuous-dose result (Callaway et al.
   2024 assumptions)?
3. Inference: 27 CBSA clusters; permutation with 200 within-CBSA reshuffles of Zshare. Is the
   randomization scheme faithful to the exposure's spatial correlation, and are 200 draws enough
   to certify p = 0.03? Check sensitivity to the 99th-percentile winsorization of exposure.
