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

## Taxonomy of record (locked 2026-11-16)

This section is the definition of record. It supersedes the 2026-11-12 section below. The rules live in `scripts/R/01_build_panel.R` (section 6) and the panel `data/derived/03_family_homes/events.parquet` carries them as three columns: `deed_cat`, `deed_subcat`, `fam_route`. Tables 1 and 2 come from `scripts/R/01b_taxonomy_tables.R`.

**A family transfer** is a recorded deed that moves a home, with no sale, from one person (or that person's trust) to a different person, during life or at death. In the panel it is `deed_cat = 'family_transfer'`. The vendor's interfamily flag is required throughout; the flag does not verify kinship.

Lifetime gifts, transfers at death and probate transfers all count. A transfer at death is a family transfer even when no tax motive is involved.

| `fam_route` | What it is | Example (invented names) |
|---|---|---|
| `lifetime` | Direct deed between two distinct people, no trust, no entity, no death document | `SMITH JOHN` -> `SMITH MARY` by grant or quitclaim deed |
| `death_affidavit` | Affidavit of death (document types AF, AJ): a survivor takes title. Counted with or without a trust party (author's decision 2026-11-16, "Count all affidavits"); the trust-party ones are `deed_subcat = 'affidavit_trust_party'` | `SMITH JOHN` (deceased) -> `SMITH MARY`; `SMITH JOHN` (deceased) -> `SMITH FAMILY TRUST` |
| `probate` | Executor's, administrator's or personal representative's deed, deed of distribution, or an estate keyword in a name | `ESTATE OF JOHN SMITH` -> `SMITH MARY` |
| `unknown` | Direct deed whose document type gives no route (corrections, re-recordings) | |
| `trust` | Title leaves a trust for someone other than its settlor (three tiers below) | |

**Family transfers through a trust (the three core tiers, `deed_subcat`).** Lifetime gifts by a living trustee and distributions after the settlor's death are not separated: both carry out the settlor's plan for who gets the home. Affidavits of death with a trust party are family transfers on the `death_affidavit` route, not part of the trust route.

| Tier | Rule | Counts | Does not count |
|---|---|---|---|
| `trust_heir_chain` | An earlier non-market deed moved the home from person A into a trust; the trust deeds it to B, same surname as A, different person; ten-year look-back, complete from 2017. The chain wins over the name rule. | `SMITH JOHN` -> `SMITH FAMILY TRUST`, later `SMITH FAMILY TRUST` -> `SMITH MARY` | The same chain ending `SMITH FAMILY TRUST` -> `SMITH JOHN` (the settlor takes the home back) |
| `trust_named_other` | The trust names one person and deeds to a different person with that surname; the chain does not show the buyer is the settlor | `JOHN A SMITH TRUST` -> `SMITH MARY` | `JOHN A SMITH TRUST` -> `SMITH JOHN A` (settlor); `SMITH FAMILY TRUST` -> `SMITH MARY` with no chain (surname-only trust: heir or settlor, the names cannot say) |
| `trust_to_trust_other` | Two trusts share a name (any word of 3+ letters, usually the surname) and each names someone the other does not. `MARY JONES TRUST` -> `MARY SMITH TRUST` also passes, and could be one person | `JOHN A SMITH TRUST` -> `MARY SMITH TRUST` | `JOHN SMITH TRUST` -> `JOHN SMITH 2019 TRUST` (same names) |

**Flagged deeds that are not family transfers.**
- `same_owner`: the seller is still on title. `same_owners` (`CHASE NANCY` -> `CHASE NANCY B`; a couple -> one of them) and `new_person_joins` (`SMITH JOHN` -> `SMITH JOHN & MARY`). Author's decision 2026-11-16: `new_person_joins` stays out.
- `entity`: a company, lender or agency on either side.
- `trust`: `into_own_trust` (`SMITH JOHN` -> `JOHN SMITH TRUST`), `trust_to_settlor`, `trust_to_trust_same`, `into_trust_for_others`, `into_trust_unlinked`, `trust_to_trust_unclear`, `trust_to_unlinked_priced`, and the two upper-bound cells `trust_surname_only` and `trust_to_unlinked_unpriced`.

**Upper bound.** The measure, plus flagged same-owner (not the exact retitles) and entity deeds, plus the two upper-bound trust cells.

**Same-owner name rule (narrowed 2026-11-16).** Names are compared person by person. A first name matches when equal or one is a 3+ letter prefix of the other; a first initial matches only when a second name backs it on both sides, or when two buyers agree with sellers on initials. A middle initial never matches a first name. Conflicting middle initials mean two people. A generational suffix blocks a match.

**Legal basis for the trust rules (California).** Revenue and Taxation Code 62(d) and Property Tax Rule 462.160: a transfer into a revocable trust and a transfer back to the trustor are not changes in ownership; a change occurs when the trust becomes irrevocable, usually at the trustor's death, and the date of death is the transfer date (BOE Proposition 19 FAQ, Parent-Child 9); transfers between spouses are excluded (section 63). Not checked for other states.

**Known limits.** (1) The trust route covers 30 percent of California's flagged deeds that leave a trust for someone other than the settlor (73 percent with the upper-bound cells). (2) Probate document types are almost absent in California (486 deeds a year), so the probate route is identified mainly in other states. (3) The 2018 vendor change moves many affidavits of death into the trust class; the `death_affidavit` route falls from 118,084 (2017) to 72,910 (2019). (4) Outside California about 94,000 trust deeds a year carry no vendor flag and are not in any trust cell. (5) Trusts abbreviated `F/TR` or `L/TR` are not caught by the trust keyword. (6) The read-precision figures of 2026-11-11 were coded by Claude, predate these rules, and need a read by the author. (7) Affidavits of death, ruled 2026-11-16: all count. Direct affidavits alone are not comparable across the 2018 vendor change (California 18.0 per 100 in 2017, 15.8 in 2018); with trust-party affidavits counted the series is continuous (18.4, 18.8, 19.1 in 2017-2019). A home whose trust later deeds it to an heir can be counted twice (an affidavit, then the deed). (8) 152,020 California affidavits (2007-2023) carry the vendor's `OWNER RECORD` placeholder as a party, are labelled entity and are out; 88,539 more carry no flag. About 54 percent of California's affidavits of death reach the measure. (9) Outside California 338,599 probate or estate deeds that are not arm's-length carry no vendor flag and are out, against 686,000 in the measure. (10) The trust name rule still matches a person's first name to an initial in the trust name: in California in 2019, 5,755 of 15,889 trust-to-person deeds labelled back-to-settlor rest on an initial alone (against 8,383 labelled as naming another person). Some are settlors of trusts named by initials (`SMITH J & M TRUST` -> `SMITH MARY`), some are heirs; unread.

**Counts, 2007-2023 (panel built 2026-11-16 20:46).** 13,123,501 family transfers of 143,999,250 deed events: 9.1 per 100 nationally, 17.0 in California, upper bound 14.6. By route: lifetime (with unknown) 10,082,792; affidavit 1,792,348 (335,470 with a trust party); probate 683,395; trust 564,966. Tables: `_outputs/taxonomy/T1_taxonomy.tex`, `T2_volumes.tex`, `TA_taxonomy_detail.tex` (appendix, every cell).

## Taxonomy of record (2026-11-12; superseded 2026-11-16)

This section is the definition the paper uses; the lists further down are the history that led to it.

**Classes** (`class` in `events_v2.parquet`, from `01_build_panel.R`): `market_sale` (arm's-length category A, not interfamily, no price floor); `family_person` (interfamily, surname match, not the exact same person, no trust); `family_estate` (estate/executor keyword residual); `family_other` (interfamily, no surname match); `family_trust` (interfamily with a trust keyword); `family_retitle` (exact same-person retitle); `estate_noninterfam`; `other_nonarms`.

**Dimensions** for the `family_*` classes: `fam_relation` (spouse by document type in California and Arizona only; owner_retained; parent_child_candidate; unrelated_entity; cross_surname; unknown), `fam_relation_nm` (the same without the document-type spouse line), `fam_trigger` (death, death likely, lifetime, unknown), `fam_vehicle` (direct or a trust route), and the trust chain (`trust_chain`, `trust_chain_gap_days`; "heir" within 3,652 days = chain-verified trust-to-heir deed).

**Same-person and entity exclusion (author's decision, 2026-11-12).** A deed in `family_person`, `family_estate` or `family_other` whose `fam_relation_nm` is `owner_retained` (the same person or household on both sides by the name rules: name variants, initial-only matches, the cross-surname same-person rule) or `unrelated_entity` (an entity on either side) is not a family transfer. Such deeds are relabelled `family_samep_entity` (`family_class_main_sql`, `events_main_from()`, `recode_family_main()` in `00_setup.R`) and leave the family measure everywhere. The exact buyer-equals-seller test alone missed almost all of them (`quality_reports/2026-11-12_same_person_retitles.md`). Spouse deeds stay in the measure.

**Measures** (one name each; the paper's Appendix Table `tab:glossary`):
- Main family measure: `family_classes` after the exclusion (Sections 2–3, wedge sample).
- Upper bound: the main family measure plus the excluded same-person and entity deeds (the earlier class-based proxy).
- Section 4 measure: the main family measure without affidavits of death, plus chain-verified trust-to-heir deeds from 2017 (`bygroup_main` = `main_distinct` in `00_groups.R`).
- Trust-inclusive measure: the Section 4 measure plus every other trust-to-person deed (`main_distinct_hi`).

Read precision of each cell, with the criterion it was scored against, is in `_outputs/tables/validation_read_precision_v2.csv` and the paper's Table `tab:readprec`. The vendor flag does not verify kinship.

## Taxonomy (frozen before regressions; original 2026-06-09, superseded by the taxonomy of record above)

Event classification for residential deed records:

1. **Market sale**: `primary_category_code = 'A'`, no price floor,
   not interfamily. (Changed 2026-10-31 from price ≥ $10k: in non-disclosure states
   ~37% of category-A deeds have missing/zero price; ODU symposium discussant.)
2. **Family transfer (inter-person)**: interfamily = 1 AND same-surname buyer/seller
   (non-empty), no trust keyword. Proxy for gifts/inheritance between relatives.
   *Caveat: misses transfers to married daughters w/ name change; undercount → bounds.*
3. **Trust self-transfer**: buyer or seller name contains TRUST (estate planning;
   control stays in family — counted separately, not as a transfer of control).
4. **Estate/probate**: EXECUTOR/ESTATE/PROBATE keywords or C-category death-linked.
5. **Other non-arm's-length**: remainder of interfamily + B/C (divorce, title moves).

Headline counts reported for (2) alone [conservative] and (2)+(4)+share of (5) [upper];
trust transfers shown separately, never in the headline.

### Taxonomy as built in panel v2 (2026-11-11; `01_build_panel.R`, file `events_v2.parquet`)

`class` and the 29 original columns are unchanged. Panel v2 adds three dimensions for
`family_*` classes (NULL elsewhere), all from document type and name rules:

- `fam_relation`: spouse (document types IT, IC in California; DS in Arizona only) /
  owner_retained / parent_child_candidate / unrelated_entity / cross_surname / unknown.
  `fam_relation_nm` is the same CASE **without** the document-type spouse line. Spouse is
  identified only in CA and AZ, so every group used in a cross-state comparison must be
  built from `fam_relation_nm`; `fam_relation` is for California descriptive series.
  Spouse deeds stay inside the family measure and are separable.
- `fam_trigger`: death (death document type or estate keyword) / death_likely (trust to a
  surname-linked person, `trust_rel` clear or ambiguous) / lifetime (includes trust to own
  name, `trust_rel = 'self'`) / unknown.
- `fam_vehicle`: direct / into_own_trust / into_trust_for_others / trust_to_other_person /
  trust_to_self / trust_unlinked / trust_to_trust / death_affidavit_trust (AF, AJ with a
  trust party: isolates the 2018 California vendor break).

Trust bounds `trust_fam_lo`, `trust_fam_hi` (family_trust rows; NULL for trust-to-trust):
lower = `trust_rel` clear, plus person-into-own-trust deeds whose trust name has an
irrevocable / residence / children keyword and links to the person; upper = lower plus
`ambiguous` trust-to-person. `trust_to_self` is outside both bounds (it behaves like
refinance retitling; a comparison series in Phase 3). The successor-trustee keyword rule
was dropped: the string almost never appears in the names. `trust_rel` is unchanged, so the
10-31 Table 2 columns still reproduce.

**Build, reconciliation and validation record (2026-11-11).** `events_v2.parquet` (148,711,427 rows,
3.49 GB, full build 11.4 min) has the 29 original columns unchanged, `class` identical on all
148,711,427 keys, plus 22 new columns (including `rec_raw`, the recording date). The dedupe order is
now total (every output column is a sort key, `rec_raw` last), so a rebuild gives the same rows.
Against the October `events.parquet`, 20,501 keys (all with 2+ raw rows) tied on the October keys,
which that build resolved arbitrarily: `state` differs on 232, `trust_rel` on 19, `absentee_buyer`
on 20,445 (three `trust_dir x trust_rel` cells move by -4, -3, +7). Accepted as an artifact of the
October build. Legacy scripts (`04`, `07`, `09b`, `14`) stay on `events.parquet` until the full
pipeline rerun; new Phase 3 scripts read `events_v2.parquet` explicitly. Script 07's headline is
-0.250424 on v1 and -0.250421 on v2.

Read validation (seeded, about 100 deeds per cell, hand-coded; share correct among reads the names
can settle, uncertain reads in brackets): death_affidavit_trust 100% [0]; trust_to_self 96% (Jan-Feb
2021, n=50) and 91% (2019, n=50) [23, 15]; spouse_CA_IT 94% [32]; parent_child_direct 94% [6];
trust_to_other_person 84% [44]; owner_retained (fuzzy name rule) 86% [0]; trust_unlinked 73% [0];
into_trust_for_others 39% [59]; into_trust_keyword 31% [58]; cross_surname 33% [0].
**Weak cells:** `into_trust_for_others` (most deeds go into the transferor's own or joint trust, with
compound surname trust names) and `cross_surname` (mostly spelling variants, the same person, or a
shared surname). Do not build a headline on either without a rule revision.

**Chain-verified trust-to-heir (`trust_chain`, 2026-11-11; user, Gate 1).** The user's wording: "if buyer
-> trust -> seller that we see in the data and if buyer and seller have same last name, then it is a
family deed transfer via trust." As built, on `class = 'family_trust'` trust-to-person deeds (NULL
elsewhere): find the earlier non-market deed that moved the parcel from a person A into a trust (a deed
whose buyer name equals the conveying trust's name is preferred, `trust_chain_named = 1`; otherwise the
most recent into-trust deed, 0). The chain is broken, and the deed is `no_chain`, if a market sale of
the parcel falls strictly between the two deeds or no into-trust deed exists in the data. Otherwise A
(prior seller) is compared with the buyers B: `heir` = same surname, given names differ (or a
JR/SR/II/III/IV suffix separates them); `self` = same surname and given names match; `other` = surnames
differ; `unclear` = same surname but a given name is missing. `trust_chain_gap_days` = days between the two
deeds. Existing columns (`trust_dir`, `trust_rel`, `trust_prior_found`) are untouched. Limits: only the
first seller of the prior deed is observed (no second seller), so a surviving spouse listed as co-settlor
appears as `heir`; spouses and children cannot be separated.
**Look-back caveat.** The panel starts in 2007, so chain-found rises mechanically with the deed's year.
Use `trust_chain_gap_days <= 3652` (10 years, available for every deed from 2017). Chain found,
trust-to-person deeds, no cap / 10-year cap: California 2017 57.5% / 55.7%, 2019 67.8% / 62.7%, 2023
66.4% / 55.5%; other states 2017 44.1% / 43.1%, 2019 48.3% / 43.8%, 2023 48.0% / 38.1%.
2019 counts, `heir` (uncapped / 10-year cap): national 11,218 / 10,015, California 4,578 / 4,172. Of 33,635
name-rule `trust_to_other_person` deeds nationally, 9,604 are `heir`, 20,679 `no_chain`, 1,864 `self`,
1,487 `other`; of 72,075 `trust_unlinked`, 418 are `heir` (CA 223). Read precision: `heir` with the
10-year cap 89% of 99 determinable (1 uncertain; errors are entity buyers and name variants of A);
name-rule `trust_to_other_person` that is not `heir` 69% of 32 determinable (18 uncertain, mostly
surname-only family trusts).

**Cross-surname deeds: same-person rule (2026-11-11; rebuild 23:53).** `fam_relation` and `fam_relation_nm` only;
`class`, trust columns and all other columns are unchanged, and the family measure is class-based, so it
does not change. Applied to the former `cross_surname` cell (class `family_other`, surnames differ by the
flag fields), in this order: (1) an entity party (LLC, INC, TRUST, HOLDINGS, OWNER RECORD and similar in
the seller or buyer-1 name) -> `unrelated_entity`; (2) strict same person -> `owner_retained`: the name
token sets are equal in any order, or the first given name (3+ letters) matches with a similar surname
(Jaro-Winkler >= 0.85 on the first word, or one contains the other) and compatible middle initials, or the
first given name and the same middle initial match under any surname; a generational suffix (JR, SR, II,
III, IV) on either side blocks all three; (3) the names share a surname token (the `same_surname` flag
missed it) -> the same-surname branch (`owner_retained` if given names match without a suffix, else
`parent_child_candidate`, else `unknown`); (4) otherwise `cross_surname`. 2,640,784 deeds changed (all
were `cross_surname`; checked against the 15:46 file). California 2019, of 121,101: 30,484 to
`owner_retained`, 9,890 to `parent_child_candidate`, 15,556 to `unrelated_entity`, 2 to `unknown`;
65,169 stay. National 2019: 229,351 changed. Read validation: the Stage B read scored the label
`cross_surname` 33% correct (criterion: two natural persons with genuinely different surnames; 67 of 100
were variants, the same person, a shared surname or an entity). On the same 100 deeds, the 41 that still
carry the label after the rule score 73%. A separate read of the rule: deeds moved to `owner_retained` 43
of 43 the same person (prototype sample, 50 before the suffix guard, two JR father-son pairs now blocked);
deeds left as `cross_surname` 95% two different people (a different test: no same-person deed left
behind). `validation_read_precision_v2.csv` now carries the criterion of each cell. Relation mix of the
main measure (`main_noaf_trust_heir`, 2017-19): California owner_retained 32.9%, parent_child_candidate
32.0%, cross_surname 26.6%, unrelated_entity 8.5%; other states owner_retained 34.9%,
parent_child_candidate 45.0%, unrelated_entity 10.6%, cross_surname 9.4%. In California 23.5% of the
remaining cross-surname deeds are spouse document types (IT/IC), against 6.0% elsewhere.

**OPEN for Gate 1 (user decides):** (1) which bound enters the main measure; (2) trust-to-trust:
the 10-31 rule below counts clear trust-to-trust in the lower bound; the new bounds exclude
it and report it as its own vehicle cell; (3) whether `trust_to_self` belongs in any bound.

### Family trust-mediated transfers (rule approved 2026-10-31; implemented 2026-11-02 as `trust_rel`)

Third Table 2 definition, alongside same-surname and broad. Distinguishes a parent moving a
home *into* their own trust (self-transfer, not family) from a trust conveying to or from a
relative. Scope: trust keyword (TRUST, TR, TTEE, REV, LIV) on buyer or seller, interfamily
flag = 1, priced arm's-length trustee sales excluded (stay market sales).

**Name parsing.** Person names are `LAST FIRST MIDDLE`. In trust names drop generic words
(TRUST, LIVING, REVOCABLE, FAMILY, FAM, REV, LIV, TTEE, SETTLEMENT, PRESERVATION, SEPARATE,
PROPERTY) and numbers; split hyphenated surnames. Remaining non-surname tokens = the trust's
named individuals. Tests per person on the deed: *surname link* (surname appears in trust
name); *given-name match* (given name matches a named individual; an initial matches, a
prefix of 3+ letters matches).

**Classes, in order.**
1. *Self-transfer (excluded from family)*: person→trust or trust→person with surname link
   AND given-name match; also person→trust with surname link and a trust name with no named
   individuals.
2. *Clear family trust transfer (lower bound)*: surname link, trust names an individual, NO
   given-name match; trust→person (buyer is relative/heir) or person→trust (into a
   relative's trust).
3. *Ambiguous (upper bound only)*: trust→person, surname link, trust name has no given
   names (`SMITH FAMILY TRUST` → `SMITH JOHN`), not class 1.
4. *Prior-deed override*: if the parcel's earlier deed moved it into this trust from person
   P, trust→buyer matching P is class 1; a different buyer sharing P's surname is class 2.
   Overrides name-based result.
5. *Not family*: no surname link or unrelated trust (trustee sales; ~74% of trust→person).
6. *Trust→trust*: matching individuals = 1; disjoint individuals, shared surname = 2; no
   named individuals = 3.

**Table 2 column.** Family, trust-mediated: lower = class 2, upper = classes 2+3. Class 1
reported as a memo line and excluded from every family column.

**Sample-based size** (8 states x 4 years): clear ~10.6% of flagged trust deeds, ambiguous
~7%; nationally a 5-15% addition to same-surname family count. Known limits: includes
spouse-to-spouse; no settlor death date; nicknames/maiden names misclassified. Validate by
reading ~100 deeds per class before use. Implementation needs new name-relation flag columns
in `01_build_panel.R` (events.parquet stores no names).

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
