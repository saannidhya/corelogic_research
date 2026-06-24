# Editorial Decision: The Machine in the Market

**Journal:** Quarterly Journal of Economics (QJE)
**Date:** 2026-06-11
**Decision:** MAJOR REVISION (Revise & Resubmit)

## Decision

Both referees recommend major revision; neither identifies a fatal flaw; and — unusually — both note that every requested fix is feasible with data and code the author already possesses. I concur. The exit natural experiment, the surviving-machine control loading, the 42M-deed infrastructure, and the candor of the writing are QJE-caliber. The paper is not yet acceptable because its interpretive spine over-reaches the evidence in three specific, repairable ways. I am issuing an R&R with the concerns below classified FATAL / ADDRESSABLE / TASTE.

## Synthesis of referee concerns

| # | Concern | Ref. | Class | Editor's weight |
|---|---|---|---|---|
| A | **Volume outcome includes the machine's own deeds** — part of the −8.0 headline is mechanical subtraction, not contraction | B-Major2 | ADDRESSABLE | **MUST.** One line of code (`n_sales − n_ib_buy − n_ib_sell`). Re-run volume DiD, event study, donut on household-only volume; restate the multiplier. This is the single most important revision. |
| B | **No covariate balance; untested mean-reversion/froth confound** | B-Major1, A-Major4 | ADDRESSABLE | **MUST.** Balance table across Zshare terciles conditional on ODshare; vintage-split exposure (pre-Ketchup 2018Q2–2020Q4 vs ramp 2021Q1–Q3) and show the effect loads on the pre-Ketchup component. |
| C | **Calibration undisciplined, unfalsifiable, internally inconsistent** (calibrates to SD when own framework says IQR; assumed σ_a contradicts Zillow's published 6.9%) | A-Major1, A-Major2 | ADDRESSABLE | **MUST.** Pin σ_a from Zillow's own purchase-residual dispersion in the deeds; calibrate to the IQR measure (the information-clean one); replace "sits exactly where the model puts it / is sized" with a one-sided power statement (data rule out an externality larger than X). |
| D | **Prop 4(i) violated ~8× by the headline; multiplier is extra-model** | A-Major3 | ADDRESSABLE (TASTE on which fix) | **MUST soften or model.** Either extend the model with a listing margin (quote as outside option) or present the multiplier as reduced-form amplification beyond the model's mechanical channel. Editor accepts either; symmetric standards required. |
| E | **Inference cannot certify p = 0.03** (27 clusters, 200 draws, exchangeability) | B-Major4 | ADDRESSABLE | **MUST.** >=999 permutation draws with p=(1+#exceed)/(B+1); wild cluster bootstrap (Webb weights); pre-period placebo permutation centered on zero. |
| F | **Dose-response not shown monotone; computed robustness unreported** | B-Major3 | ADDRESSABLE | **MUST.** Exposure-tercile dose-response; full robustness grid for ALL outcomes incl. volume (winsor-95, excl-Phoenix); ODshare placebo in every variant. |
| G | **Hedonic-dispersion: treatment-correlated cell selection; left-tail text≠code** | B-Major5 | ADDRESSABLE | **SHOULD.** Extensive-margin P(n>=5) on Zshare×Post; reconcile raw vs demeaned left tail; state preferred weighting. |
| H | **Destruction vs retiming; welfare claim outruns design** | A-Major4 | ADDRESSABLE | **SHOULD.** Full event-study path through 2024 + decay test; RedfinNow Nov-2022 exit as a second, smaller event (model predicts scaled replication, depletion does not). |
| I | **Framing: titular question returns a bound** | A-Major5, B-summary | TASTE | **MUST adjust framing.** Lead with the identified liquidity result; present the information externality as a disciplined bound. The abstract's last line ("a balance sheet, not a signal") already points the right way. |
| — | Minors: tercile-on-distinct-ZIPs, undisclosed filters, QJE table conventions, footnote-1 proof, multiple-testing caveat on entry IQR | both | TASTE | Address in revision. |

## What a successful revision looks like

The referees agree on the empirical bar: if household-only volume survives in the −5 to −7 range, the balance table and vintage split come back clean (effect on pre-Ketchup exposure, balance on appreciation), the dose-response is monotone, and inference holds at 999+ draws with a wild bootstrap, the liquidity result is established and this becomes a strong paper. The information externality should be reframed from "confirmed at the model's predicted magnitude" to "bounded below an economically small ceiling at achieved scale." That is a genuine and publishable finding — arguably a cleaner one than the title currently promises.

## Reproducibility note

The independent audit (165/165 PASS) and the pre-submission proof verification (one error caught and fixed) earn the paper the benefit of the doubt on execution. The concerns above are about design validity and interpretation, not about whether the reported numbers are the numbers the code produces — they are.
