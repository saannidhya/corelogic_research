# Research Spec: Keeping the House in the Family

**Project:** 03_family_homes
**Date:** 2026-06-09
**Status:** DRAFT (institutional details pending confirmation)
**Target outlet:** top-5 general interest (QJE/AER/JPE-style), Glaeser-register writing

---

## One-sentence RQ

How large is the non-market, intra-family channel of US housing turnover; what happens
to homes after they pass within families; and does the tax price of keeping a home in
the family (California Proposition 19) causally affect whether inherited homes reach
the market?

## Motivation (the Glaeser pitch)

Housing economics treats the market sale as the unit of analysis. But deeds records
show that for every two American homes sold on the market, roughly one changes hands
inside a family — by quitclaim, trust, or estate deed, almost always at zero stated
price. These transfers are invisible in MLS data, excluded from price indices, and
absent from the canonical turnover statistics. If family-transferred homes
systematically fail to return to the market, the family is a first-order housing-market
institution that rations a scarce asset by birth rather than by price — and federal and
state tax law actively subsidizes it.

## Verified data facts (initial probes, 2026-06-09)

- OT usable window: **2007–mid-2024** (~160M deed records; 2024 truncates ~August).
- `primary_category_code`: A = arm's-length market sale (2.6% interfamily flag, 8.2%
  zero price, median positive price $260k); B = non-arm's-length (62% interfamily, 85%
  zero price); C = ~purely family/administrative (96% interfamily, 97% zero price).
- `deed_category_type_code`: G (grant/warranty), Q (quitclaim; 66% interfamily, 87%
  zero price), U (3% interfamily — mortgage-adjacent/unknown; to exclude or test), T (tiny).
- `interfamily_related_indicator` 0/1, fully populated.
- Names: seller surname populated 84% (fam) / 64% (nonfam); buyer surname 66/75%.
  Among interfamily 2010–2024: same-surname 18.9M, trust-involved 13.4M (42% of those
  in CA), estate-keyword ~43k (probate flows mostly through other deed text),
  other 7.2M.
- Clip linkage: 73M clips with any 2007–2024 event; 41.5M with ≥2 events.
- Residential annual volumes: interfamily 1.9M (2008) → 3.6M (2021); arm's-length
  3.5M (2009) → 6.6M (2021).
- CA Prop 19 anticipation/retiming visible in raw monthly interfamily counts
  (surge Nov 2020–Feb 2021, Feb 2021 +81% YoY; 2022 collapse below 2019 baseline).

## Taxonomy (frozen before regressions)

Event classification for residential deed records:

1. **Market sale**: `primary_category_code = 'A'`, price ≥ $10k (project-02 convention),
   not interfamily.
2. **Family transfer (inter-person)**: interfamily = 1 AND same-surname buyer/seller
   (non-empty), no trust keyword. Proxy for gifts/inheritance between relatives.
   *Caveat: misses transfers to married daughters w/ name change; undercount → bounds.*
3. **Trust self-transfer**: buyer or seller name contains TRUST (estate planning;
   control stays in family — counted separately, not as a transfer of control).
4. **Estate/probate**: EXECUTOR/ESTATE/PROBATE keywords or C-category death-linked.
5. **Other non-arm's-length**: remainder of interfamily + B/C (divorce, title moves).

Headline counts reported for (2) alone [conservative] and (2)+(4)+share of (5) [upper];
trust transfers shown separately, never in the headline.

## Empirical design

### Exhibit 1 (RQ1, descriptive): The parallel housing market
National + state volumes 2007–2023 by taxonomy class; family:market ratio; geography
(state map); property-level correlates within tract (age of structure, value tier,
senior-exemption parcels).

### Exhibit 2 (RQ2, descriptive-dynamic): The family lock
For transfer events 2008–2018 (≥5.5y followup), Kaplan–Meier time-to-next
arm's-length sale by event class, conditioning on state × year; absentee share
(mailing ≠ situs) after event, family vs market benchmark. Selection is the point —
no causal claim.

### Exhibit 3 (RQ3, causal): Prop 19
(a) Retiming/bunching: CA monthly parent-child transfer counts vs control
states, Nov 2020–Feb 2021 window; excess mass + post-period missing mass.
(b) Level effect: DiD of family-transfer volume CA vs control states, 2022–2023 steady
state vs 2017–2019 baseline.
(c) **Supply release (the headline causal estimate):** among family-transferred homes,
hazard of arm's-length sale within h months; compare CA post-2021 transfer cohorts vs
pre-2019 cohorts, differenced against the same cohort contrast in non-CA states
(DDD). Prediction: post-Prop-19 CA family-recipient homes sell sooner (no more
inherited tax basis for non-occupant heirs).
(d) Composition: post-2021 CA family transfers shift toward owner-occupant recipients
(homestead-exemption take-up on transferred parcel; absentee share falls).

### Exhibit 4 (RQ4, mechanism): under-utilization
Absentee + vacancy proxies for family-held vs market-bought homes within tract;
heterogeneity by assessment-gap proxy in CA (years since last market sale of parcel).

### Exhibit 5 (RQ5, policy arithmetic): national counterfactual
Prop 19 hazard shift × stock of family-held homes; explicit assumptions; reported as
range. Short section.

## Identification threats & answers

- **COVID confound in 2020–21 CA**: bunching design uses within-window retiming +
  donor states; steady-state DiD uses 2022–23 vs 2017–19, skipping the pandemic window.
- **Trust contamination**: taxonomy excludes trust self-transfers from headline; CA
  robustness re-runs with trust-inclusive and exclusive definitions.
- **Recording heterogeneity**: state×year coverage audit table in appendix; within-state
  designs for all causal claims.
- **Name-change undercount**: bounds; robustness using interfamily flag alone.
- **Right-censoring**: survival methods; cohorts chosen with ≥30 months followup.

## Data requirements (all verified available)

- OT: clip, dates, price, pcat, dtype, interfamily, names, corporate flags, buyer
  mailing address, situs address, state, fips.
- Prop: clip, tract (census_id), lat/lon, year built, sqft, owner occupancy, exemptions
  (homestead/senior), assessed values, land use.
- External: FHFA state HPI (already in data/external). FRED 30y mortgage rate (cached
  pattern from project 02) if needed for context figures.

## Deliverables

- `manuscript/paper.tex` full first draft (intro, facts, design, results, mechanism,
  policy, conclusion) with all numbers from `scripts/R/_outputs/`.
- Internal review notes and reproducibility checks are kept locally.

## Open questions

- [ ] Verifier: Prop 19 dates/mechanics, LAO predictions, novelty claims.
- [ ] Vacancy-indicator coverage audit (RQ4 GO/NO-GO).
- [ ] Whether 2006 partial-year data usable for placebo (probably not; exclude).
