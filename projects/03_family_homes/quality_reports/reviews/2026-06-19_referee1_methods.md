# Referee 1 — Methods / Identification (agent) — 2026-06-19

**Target journal (calibration):** *American Economic Journal: Economic Policy*
**Recommendation:** Major Revision
**Composite score:** 64 / 100
**Draft:** v2 (paper.tex). Prior cycle: `2026-06-10_methods_referee.md`.

## Headline verdict

The national deeds-based accounting and the within-state bunching/retiming evidence are
excellent and close to airtight, but the load-bearing causal claim — the steady-state
35% / 23% (≈58k/yr) decline — rests on the paper's *own* permutation p of about 0.10
(0.18 net), which the prose narrates as established. At AEJ:Policy the design is
publishable; the framing of the policy magnitude is not yet inference-defensible. The
revision is about re-anchoring the inference and the writing to what the placebo
distribution actually supports.

## Strengths

1. **Bunching/retiming is the identification crown jewel.** Within-state, day-specific
   statutory deadline, a 31% four-month spike (28,866 excess deeds), February flipping
   from the quietest to the busiest month in the series — no slow-moving confounder can
   mimic it.
2. **Inference handled with rare honesty for a single-treated-cluster design.** CRVE
   explicitly disclaimed; placebo-state permutation correctly implemented (verified by
   hand against `ref_perm_volume_all.csv`: CA = 5/51, p = 0.098); unflattering p-values
   reported in the open.
3. **Strong reproducibility posture** — degeneracy guards, schema-drift assertions,
   total-ordering dedupe, ZIP leading-zero normalization, documented `GROUP BY ALL` bug
   fix. Would survive an AEA Data Editor pass with modest cleanup.
4. **Convincing taxonomy construct-validation** (90% zero-price, 40% quitclaim, corporate
   buyers absent from same-surname transfers); the v1 interspousal/co-owner contamination
   (C2) is fully resolved via the `family_retitle` class.
5. **Candor about its weakest results** (the sold-within-24m DDD null at p=0.67, the
   deferral reading) — the problem is framing, not honesty.

## CRITICAL

- **C1 (OPEN; new framing of a v1-resolved mechanic).** The headline causal magnitude
  rests on permutation p ≈ 0.10 (gross, 5/51) and 0.18 (net, 9/51) — neither clears 0.05
  — yet the abstract/body narrate the decline as established ("excise its tax-motivated
  core"). The *net* estimate (the one converted to 58k deeds/yr and matched to the LAO
  count) has the **weaker** inference. Two-sided absolute-value ranking pools CA's
  negative coefficient against positive placebo outliers (MS +1.00, WV +0.61); a one-sided
  test still leaves NV (−0.435) and UT (−0.446) above CA. **Fix:** re-frame the
  steady-state as suggestive/not-significant, *or* strengthen via synthetic control with a
  disciplined donor pool + RMSPE-ratio placebo test.
- **C2 (PARTIAL from v1 C3).** The "real but deferred" reading of the estate-seller null
  (p=0.71/0.63) is unfalsifiable within the paper's own 2022–2023 window — decline + rise
  = release; decline + no rise = deferred release; no 2022–2023 outcome can reject the
  thesis. The null is decorative, not discriminating, for this sample. **Fix:** state the
  supply-release magnitude is NOT identified in this sample; leave it as an out-of-sample
  prediction *with a power calculation* for the future estate-seller test.

## MAJOR

- **M1 — single-pre-year counterfactual.** Bunching counterfactual is anchored to a single
  pre-year (2019) seasonal profile (`04_prop19.R:62-72`), validated out of sample only in
  2018 and the first 10 months of 2020. Any 2019 idiosyncrasy propagates into the 31%.
  **Fix:** re-anchor to a 2017–2019 average seasonal base; show the four-month spike
  survives.
- **M2 (PARTIAL from v1 M3) — the 2017 lead.** The 2017 event-study lead is +0.154
  (`ref_event_study.csv`) — roughly a third of the 2023 effect and larger than the 2018 +
  2021 leads combined — but the text dismisses it as "one wobble." Cannot rule out that the
  2022–2023 decline is partly mean reversion from an elevated late-2010s level. **Fix:**
  report a DiD anchored to 2018–2019 only as its own T4 row with its own permutation p; add
  a placebo-in-time test; plot the event study with permutation bands, not CRVE bands.
- **M3 — contaminated placebo, signed in the paper's favor.** Prop 19's own portability arm
  (over-55 movers, Apr 2021) and the predicted inherited-home release both push CA market
  sales *up*, so the market-sale placebo over-states CA's slump and makes the netted family
  decline too small (a conservative bound). The abstract headlines the lower bracket edge
  (23%, p=0.18). **Fix:** use an untreated cycle control (new-construction or
  corporate-to-corporate CA market sales).
- **M4 (PARTIAL from v1 C3) — the direct test can't see estates.** The "direct supply test"
  is >99.9% trust-keyword, not estate-keyword — `ref_ca_estate_seller_counts.csv` shows
  `n_mkt_estate` is only 75–150/yr in CA vs ~100k–155k/yr for the trust-dominated
  aggregate. The test cannot see estate liquidations at usable volume, and its permutation
  pool is degenerate (MA −4.00, RI −2.96, CT −2.44 log swings on near-empty cells).
  **Fix:** report an estate-keyword-only series + its power; build a parcel-linked
  next-market-sale test reusing `03_hazard.R`; use PPML / drop micro-states from the
  permutation.
- **M5 (constructive) — the silent pivot.** The sold-within-24m DDD was the spec's
  *designated* headline causal estimate (`research_spec.md:82`) and is now a null
  (−0.74pp, p=0.67, `ref_perm_ddd.csv`) with a flat absentee share; the paper pivots to the
  volume margin without acknowledging the pivot, and "rules out the main alternative"
  overstates an estimate at p=0.67. **Fix:** label the conditional margins as underpowered
  nulls, rest the selection prediction on model + volume, add a power calculation.
- **Threat assessment.** None of the five threats is individually fatal, but differential
  mortality/migration is under-weighted and deferred — CA's below-average COVID mortality
  plus elevated out-of-state succession can manufacture part of the steady-state decline
  through a channel the market placebo does not absorb. The CDC-WONDER adjustment belongs
  in the main specification, not a revision item.

## Minor

12 items logged (see structured summary; mostly wording precision around the inference
claims and figure annotations).

## Journal fit

AEJ:Policy is a reasonable target — the assessment-inheritance/housing-supply question is
squarely in scope, and the national deeds accounting is a genuine public-economics
contribution; the bunching evidence alone could anchor a paper here. But the draft
currently frames itself as a clean causal evaluation of Prop 19's supply effect, which is
not yet inference-defensible.

- **Path 1 (recommended):** re-frame for AEJ:Policy as *parallel family market + sharp
  retiming + a suggestive-not-significant steady-state decline + a falsifiable deferral
  prediction.*
- **Path 2:** if the strong causal magnitude is to headline, add synthetic control with a
  disciplined donor pool and the mortality adjustment in the main spec; if it survives,
  QJE/AER become plausible given the data novelty; otherwise the descriptive contribution
  still fits AEJ:Policy or JPubE.

The writing is currently pitched a notch above what the inference supports.
