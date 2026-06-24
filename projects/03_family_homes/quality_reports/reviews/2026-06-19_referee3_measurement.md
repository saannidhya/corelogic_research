# Referee 3 — Measurement / Data Validity (agent) — 2026-06-19

**Target journal (calibration):** *Journal of Urban Economics*
**Recommendation:** Major Revision
**Composite score:** 72 / 100
**Frame:** treats the paper as a descriptive measurement paper whose entire edifice rests on a name-based deed taxonomy applied to CoreLogic.

## Headline verdict

A fully reproducible, carefully constructed measurement paper whose every checked number
ties exactly to the replication outputs and whose internal falsification (retitles tracking
trusts at 9.1 vs 8.9%) is convincing. But the paper's load-bearing construct-validity claim
— that the same-surname class is a **strict lower bound** — is one-sided (ignores
common-surname false positives and conditioning on the vendor interfamily flag), the flag's
provenance and time-stability are undocumented, and two facts are conditioned in ways the
author's own spec flagged as wrong then deferred. Measurement papers live or die on construct
validity, and that dimension is currently asserted rather than demonstrated.

## Strengths

1. **Exemplary reproducibility:** every spot-checked number (T1 counts, KM falsification
   9.1/8.9%, bunching excess 28,866, all permutation p-values, 1.19 deeds/clip) ties exactly
   to `_outputs/tables/*.csv`; dedupe made bit-reproducible with total ordering and real
   schema-drift guards (interfam-null = 0, share bounds, ≥51 states).
2. **Genuinely clever internal falsification:** same-person retitles track trust
   self-transfers at 9.1 vs 8.9% sold within 24m, exactly as pure-paperwork events should.
3. **Taxonomy fingerprints in T1** (90% zero-price, 40% quitclaim, ~0% corporate buyers, $97k
   vs $224k median) are hard to reconcile with anything but gifts/bequests.
4. **Candid reporting of the conditional-margin nulls** (DDD p=0.67, flat absentee share)
   rather than burying them; honest about pooled-KM composition limits.
5. **Substantive v1→v2 response on inference:** permutation/placebo-state inference for the
   single treated cluster is correctly implemented and reported.

## CRITICAL

- **C1 (NEW) — the "strict lower bound" claim is not airtight.** The bound argument only
  addresses false *negatives* (name changes, missing names); it ignores false *positives*
  from common surnames (Smith/Garcia/Nguyen/Lee) within the interfamily pool, concentrated in
  exactly the high-Hispanic/Asian states (CA, TX) that drive the geographic result. The
  Appendix claim "missing names can only push true family transfers into *other*, never into
  *market*" is also false for the flag-failure mode (a flag-missed family transfer with
  pcat=A and price ≥ $10k lands in `market_sale`). The bound is one-sided and conditional on
  an untested assumption that the vendor flag is a superset of true family transfers. **Fix:**
  surname-frequency false-positive audit (ratio after dropping top-N surnames) + drop
  "strict" or reframe explicitly.
- **C2 (NEW; semantic — v1 only handled the parsing-level NULL guard).** The CoreLogic
  interfamily flag and arm's-length category are treated as ground truth with no provenance.
  Five of six family/trust classes require `interfam=1`, yet the paper never states the
  field's origin or whether its definition/coverage is stable. The interfamily share trends
  up 0.236 → 0.306 across the sample (`audit_name_population_by_year.csv`) — real rise,
  vendor-algorithm change, or coverage improvement is indistinguishable, which threatens Fact
  1's "stable channel" and the event-study leads. **Fix:** document the field verbatim from
  the data dictionary; add the state-by-year flag-coverage table the spec promised; add a
  names-only family series independent of the vendor flag.

## MAJOR

- **M1 — coverage non-uniformity under-disclosed for Facts 1–3.** Absentee-ZIP coverage rises
  77 → 98%, prop-characteristics match is only 61% (`fact_prop_match_rate.csv`: 0.609 on
  49.6M events), interfamily share trends up. Fact 3's age/value/senior gradient is computed
  on the 61%-matched subsample with no characterization of who the 39% non-matches are — the
  "family carries the old stock" gradient could be a match-selection artifact. **Fix:**
  compare matched vs unmatched populations on pre-match observables.
- **M2 (OPEN, v1-M5) — KM pools nationally**, contradicting the spec's explicit Exhibit-2
  instruction to condition on state × year. Family transfers concentrate in low-turnover
  states, so the headline "38 vs 42% at 10y" is composition-confounded; `sold_60m` is also
  non-stationary across cohorts (0.184 → 0.25). Deferred again to "the next draft" — not
  acceptable at revision stage for the paper's second core contribution. **Fix:** stratified
  KM or a Cox model with state + cohort-year fixed effects before the pooled marginal goes in
  the abstract.
- **M3 — validation benchmarks asserted in prose, not tabulated.** LAO comparison (58k inside
  60–80k) is done well, but there is no ACS-mobility cross-check, no quantitative comparison
  to `delventhal2026inherited`, and `fact_validation_moments.csv` contains taxonomy
  fingerprints, not external moments. **Fix:** a small table of my-count vs prior-count vs
  external anchors.
- **M4 (OPEN, v1 minor) — dedupe tiebreak deflates the family ratio.** "Prefer arm's-length
  then highest consideration" (`01_build_panel.R:155`) keeps the market record on same-day
  (clip,date) collisions, dropping the family event — mechanically deflating the
  family:market numerator and inflating the denominator. Conservative for the thesis but
  undocumented in magnitude. **Fix:** report collision counts and the ratio under a
  family-preferring tiebreak.
- **M5 (PARTIALLY NEW) — the estate-seller "direct test" is the wrong proxy.** It is carried
  almost entirely by trust-seller sales (estate-only counts ~75–150/yr in CA per
  `ref_ca_estate_seller_counts.csv`); trust-seller sales are dominated by ordinary
  estate-planning households, not post-succession estate sales. The p=0.71 null may be an
  underpowered-wrong-proxy result, not evidence of deferral. **Fix:** a positive control
  showing the proxy can detect succession sales, or reframe the null as proxy weakness.

## Minor

9 items logged.

## Journal fit

JUE is appropriate **if reframed as the measurement contribution it actually is** — a
national deeds-based accounting of the non-market housing channel, with Prop 19 as a
*motivating* natural experiment rather than the headline causal estimate (the
conditional-margin nulls and single-treated-cluster inference cannot carry a causal
headline). The durable contribution is the descriptive facts (Facts 1–3 and the family
lock), well-suited to JUE's administrative-data-measurement tradition. Alternatives: *Journal
of Housing Economics* (more naturally a measurement venue, lower causal bar) and *Regional
Science and Urban Economics*. The spec's top-5 ambition is not supported by the current
causal evidence, but the measurement contribution alone is JUE-grade once the
construct-validity overclaims are fixed.
