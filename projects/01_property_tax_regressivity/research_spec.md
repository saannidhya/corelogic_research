# Research Spec: Property Tax Assessment Regressivity

**Slug:** `property_tax_regressivity`
**Date drafted:** 2026-05-19
**Status:** APPROVED (citation audit completed 2026-05-19; see citation audit notes)
**Paper type:** Reduced-form + structural (mixed; reduced-form replication → structural estimation → mechanism decomposition)
**Researcher:** Saani Rawat (Marquette University)
**Spec produced via:** research scoping session, 2026-05-19

---

## Research Question

> **How much local-government tax revenue is misallocated by assessor information asymmetry, and which mechanism — information decay (reassessment cycle), information acquisition cost (transaction density), assessor institutional incentives (elected vs. appointed), appeals technology (owner wealth), or transaction-frequency-driven information staleness — drives the regressivity quantitatively?**

**One-sentence pitch:** Berry (2021) documents that property-tax assessments are regressive nationally; we quantify the resulting tax-revenue misallocation across deciles and structurally identify which of five mechanisms is quantitatively most important — and use the structural model to evaluate the welfare and revenue effects of three counterfactual reforms.

---

## Motivation

Property taxes raise approximately $500 billion per year in the United States, accounting for ~72% of local-government tax revenue and ~47% of own-source general revenue [Berry 2021, citing US Census 2016]. They are the dominant funding source for public schools, local infrastructure, and municipal services. The fairness and efficiency of the property tax therefore depends on the accuracy of property assessments.

A growing literature documents that assessments are systematically *regressive*: low-priced properties are assessed at a higher fraction of their true value than high-priced properties, so the effective tax rate (tax bill ÷ sale price) is decreasing in property value within most jurisdictions. Berry (2021 SSRN working paper) estimates a within-jurisdiction tax-rate elasticity of −0.37 across 26 million US residential sales 2007–2017: bottom-decile properties pay more than twice the effective rate of top-decile properties in the same jurisdiction. Avenancio-León & Howard (2022 *QJE*) document that assessment ratios are also higher for properties owned by minority households, contributing to racial wealth gaps. Hodge, Komarek, & McAllister (2025 *Public Finance Review*) extend the framework to capitalization, showing that buyers partially price in expected over-assessment.

What the literature has NOT done — and what creates publication and policy room for this paper — is (a) quantify the **fiscal redistribution** that regressivity creates (over-taxed low-end owners are effectively subsidizing under-taxed high-end owners; how much, where, and to whom?), and (b) structurally identify *which* of several plausible mechanisms is quantitatively responsible. Berry attributes regressivity primarily to assessor information asymmetry (assessors observe fewer attributes than buyers and sellers). Other candidates — political-economy appeal asymmetry, reassessment-cycle staleness, model misspecification at the price-distribution tails, and transaction-frequency-driven information freshness — have been raised but not formally compared. A structural model that estimates each mechanism's contribution allows policy-relevant counterfactuals: how much regressivity would be eliminated if reassessment cycles were shortened? If appeals were costless? If assessors were appointed rather than elected?

This paper matters to the researcher beyond publication: property taxation was the topic of his job-market paper, and local-government finance is a primary research agenda at Marquette. Property-tax misallocation is one of the largest fiscal-equity issues in US local public finance and is salient to the Milwaukee/Wisconsin policy environment.

---

## Hypotheses

| # | Hypothesis | Direction | Falsifiable? |
|---|------------|-----------|--------------|
| H1 | Within-jurisdiction assessment ratios are decreasing in sale price (replication of Berry 2021) | Negative elasticity, expected ≈ −0.30 to −0.45 | Yes — sign and magnitude testable directly |
| H2 | Regressivity is larger in jurisdictions with longer reassessment cycles, holding institutional features constant | Positive cycle-length elasticity | Yes — cross-state variation in state-mandated cycles |
| H3 | Regressivity is smaller in census tracts with higher transaction density (more comparable sales available to assessor), within-jurisdiction | Negative density elasticity | Yes — within-jurisdiction tract-level test |
| H4 | Regressivity is larger in jurisdictions with elected (vs. appointed) assessors | Positive elected dummy coefficient | Yes — cross-jurisdiction comparison, controlling for institutional confounders |
| H5 | Appeal rates and appeal-success rates are increasing in owner wealth, conditional on assessment-ratio bin | Positive wealth elasticity in appeals | Yes — tract income (ACS) variation in observed appeals |
| H6 (novel) | Properties that transact more frequently have assessment ratios closer to 1.0 (less stale info); price-decile differences in transaction frequency mediate a measurable share of overall regressivity | Negative frequency-deviation relationship; positive mediation share | Yes — parcel-level transaction counts as mediating variable |

Hypotheses H2–H6 are the structural mechanism tests. Hypothesis H1 is the empirical foundation; if it fails to replicate, the paper pivots.

---

## Identification Strategy

### Three-source structural identification

The structural model (Section below) has five primitives to estimate. No single instrument identifies all of them; the strategy combines three sources of exogenous variation, each pinning down a different primitive.

**Source 1 — Reassessment cycle length (cross-state)** → identifies the information-decay parameter δ.
- State statutes set reassessment cycles: MA (annual), CA (Prop 13 hybrid), TX (3-year with caps), IL (4-year), OH (6-year), etc.
- Cycle length is largely a feature of long-standing state constitutional or statutory provisions, set decades ago and unrelated to current property-market dynamics.
- **Primary identification:** within-MSA-but-cross-state-line variation, using border-MSAs (e.g., Cincinnati OH/KY/IN; Kansas City MO/KS; Memphis TN/MS/AR; Texarkana TX/AR; Washington DC/MD/VA). Same housing market, different state-mandated cycle length.
- **Methodological precedent:** border-county minimum-wage design (Dube, Lester, & Reich 2010 *REStat*). To our knowledge, not previously applied to assessment cycle length.

**Source 2 — Neighborhood transaction density (within-jurisdiction)** → identifies the information-acquisition cost parameter κ.
- Within the same jurisdiction (same legal/institutional setup), tracts with thicker comparable-sales markets give assessors better signals at lower cost.
- Endogeneity concern: dense-trading tracts are different (higher turnover, gentrification dynamics, demographic composition).
- **IV solution:** shift-share Bartik instrument for tract-level transaction density, based on housing-stock age × family-lifecycle composition from ACS. The shift component is exogenous to current market conditions; the share is pre-determined.
- **Within-tract panel variation** in transaction density (driven by macro housing-cycle shocks) provides additional within-tract identification as a robustness check.

**Source 3 — Assessor institutional regime (cross-jurisdiction)** → identifies the assessor objective parameter λ (weight on equity vs. revenue).
- Cross-jurisdiction variation in: elected vs. appointed assessor; term length; statutory ratio-study reporting requirements; jurisdiction size (county vs. township).
- These institutional features are set by state law or long-standing local custom — exogenous to current market dynamics.
- Identification: compare regressivity controlling for institutional regime, conditional on state and time fixed effects.
- **Connects to a second literature:** Besley & Coate (2003 *JEEA*) on elected vs. appointed regulators. Gives the paper a second pitch angle to political-economy referees.

### Identification of the appeals technology and transaction-frequency mechanisms

- **Appeals (c_appeal):** Use ACS tract-level median income × assessment-ratio variation. Owners with higher wealth in same assessment-ratio bin should appeal more. The wealth-elasticity of appeals identifies the appeals cost.
- **Transaction frequency (H6):** Compute parcel-level transaction counts in 10-year rolling windows. Test whether the within-jurisdiction regressivity coefficient attenuates after controlling for transaction frequency × decile interaction. This is a mediator-variable test of Berry's information story.

### Threats to identification

| Threat | Mitigation |
|---|---|
| Cycle length endogenous to state political conditions | Use cycle changes set decades ago (>20 years stable); pre-period and placebo states |
| Border-MSA homogeneity assumption | Test for cross-border price-level differences; show parallel pre-trends in regressivity coefficients on border samples |
| Transaction density endogenous to local market conditions | Bartik IV (housing-stock age × family lifecycle, ACS); within-tract panel variation |
| Institutional regime endogenous to local preferences | Restrict to jurisdictions where regime was set by state constitution (institutional persistence); use state-level rather than local-level variation |
| Selection into appeals | Use ACS tract income, not individual income (less selection); robustness with neighbor-comparison appeals |
| CoreLogic missing 30%+ of assessments in some counties | Document coverage by county; restrict main analysis to ≥80% coverage counties; show robustness to coverage threshold |

---

## Structural Model (sketch)

A Bayesian assessor faces an information-acquisition problem. For each parcel i in jurisdiction j at time t:

**(1) True value:**
$$V_{ijt} = \exp(X_i \beta + u_i + \varepsilon_{it})$$
where $X_i$ is the vector of observable hedonic attributes (recorded in CoreLogic property characteristics: square footage, beds, baths, year built, etc.), $u_i \sim N(0, \sigma_u^2)$ is the vector of attributes observable to buyers and sellers but NOT to the assessor (kitchen quality, micro-location, structural condition), and $\varepsilon_{it} \sim N(0, \sigma_\varepsilon^2)$ is an idiosyncratic shock unobservable to all.

**(2) Assessor's signal set:**
$$S_{ijt} = \{X_i, \{V_{kj t'}, X_k\}_{k \in C_{ij}(e_i, \tau_j, d_j)}\}$$
where $C_{ij}$ is the set of comparable sales the assessor uses for parcel i. The size and freshness of $C_{ij}$ depends on: assessor effort $e_i$, jurisdiction reassessment cycle $\tau_j$, and tract transaction density $d_j$.

**(3) Bayesian posterior:**
$$A_{ijt} = E[V_{ijt} \mid S_{ijt}]$$
Posterior variance shrinks in $|C_{ij}|$ and grows in $(t - t_{\text{last reassess}})$ at rate $\delta$.

**(4) Assessor's objective:**
$$\max_{e_i} \, \lambda \cdot \mathbb{E}[\text{Equity}(A_{ijt}, V_{ijt})] - (1-\lambda) \cdot \mathbb{E}[\text{RevenueLoss}] - \kappa \cdot e_i - \mathbb{E}[c_{\text{appeal}} \cdot \mathbf{1}(\text{appeal})]$$
where equity is a quadratic loss in $(A_{ijt} - V_{ijt})$ and revenue loss penalizes systematic under-assessment. $\lambda$ is the assessor objective weight (elected vs. appointed, term length).

**(5) Appeals technology:**
$$\Pr[\text{appeal}_{ijt}] = \Phi(\alpha_0 + \alpha_1 (A_{ijt} - V_{ijt})_+ + \alpha_2 W_{ijt})$$
where $W_{ijt}$ is owner wealth (proxied by tract income from ACS), $\alpha_2 > 0$ captures wealth-driven differential access to the appeals process.

**(6) Decay between reassessments:**
Between reassessment dates spaced $\tau_j$ apart, $A_{ijt}$ is held fixed while $V_{ijt}$ drifts. The accumulated gap is increasing in $\tau_j$ at rate $\delta$.

**Primitives to estimate:** $\theta = (\sigma_u^2, \kappa, \lambda, \delta, c_{\text{appeal}}, \alpha_2)$.

**Welfare object of interest (revenue gap):**
$$\Delta_{\text{revenue}}(j) = \sum_{i \in j} (A_{ijt} - V_{ijt}) \cdot \tau_{jt}$$
decomposed by sale-price decile. Counterfactuals: $\Delta_{\text{revenue}}$ under (i) $\tau_j = 1$ for all $j$ (annual reassessment), (ii) $c_{\text{appeal}} = 0$ (frictionless appeals), (iii) all assessors appointed ($\lambda = \lambda_{\text{appointed}}$).

---

## Estimator: Simulated Method of Moments (SMM)

For Saani's first structural model. SMM is the right choice because:
- Likelihood for the Bayesian-assessor model is not closed-form (involves expectation over posterior given a stochastic comparable-set)
- SMM is forgiving on small samples relative to MLE
- Moments are conceptually transparent — easier to motivate to referees
- Software stack ready: Julia `Optim.jl` (deterministic) or `BlackBoxOptim.jl` (global), `DataFrames.jl`, `DuckDB.jl` for data loading

**Moments to match:**

| # | Moment | Identifies |
|---|---|---|
| 1 | Within-jurisdiction regressivity coefficient (Berry's -0.37) | $\sigma_u^2 \times \kappa$ composite |
| 2 | Cross-state regressivity gradient on cycle length $\tau$ | $\delta$ |
| 3 | Within-jurisdiction regressivity gradient on tract density $d$ | $\kappa$ |
| 4 | Appeal rate $\times$ tract income gradient | $\alpha_2$ |
| 5 | Elected-vs-appointed regressivity gap | $\lambda$ |
| 6 | Reassessment-cycle event-study (assessment-ratio jumps at reassess) | $\delta$ (second moment) |

Standard errors via bootstrap (block-bootstrap on jurisdiction-year clusters). Identification verified by Jacobian rank check at estimated parameters.

---

## Data Requirements

| Source | Variables | Coverage | Status |
|---|---|---|---|
| CoreLogic OT (Owner Transfer) | clip (parcel ID), sale_amount, sale_derived_date, transaction_type, interfamily_related_indicator, foreclosure_reo_indicator, deed_situs_state, deed_situs_county, deed_situs_zip | National, 2007–2024 | **Available** — `data/corelogic_extracts/by_state/ot/state=*/year=*/*.parquet` (~26M residential sales) |
| CoreLogic Prop (Property Characteristics) | clip, assessed_value, market_value, land_use_code, year_built, bedrooms, bathrooms, living_area_sqft, lot_size | National | **Available** — `data/corelogic_extracts/by_state/prop/state=*/*.parquet` |
| CoreLogic prior Ohio baseline | OT and Prop geocoded with county-subdivision and place joins | Ohio only | **Available** — `data/corelogic_baseline/{ot,prop}_oh_*.parquet` |
| ACS 5-year tract estimates | Median household income, race composition, housing stock age, family lifecycle composition | National, 2009–2022 | **To acquire** — `tidycensus` R package, ~1 day |
| State reassessment cycle laws | State-level reassessment cycle (years), mandate type (annual vs cyclical), exemptions | All states | **To assemble** — Lincoln Institute of Land Policy "Significant Features of the Property Tax" database, or NCSL property tax review |
| Assessor institutional features | Elected/appointed, term length, jurisdiction type (county/township), ratio-study reporting requirements | All states with assessor jurisdiction | **To assemble** — IAAO (International Association of Assessing Officers) directory; state SecState offices |
| TIGER/Line shapefiles | Census tract, county, county subdivision | National | **To acquire** — `tigris` R package, automatic |
| Cook County reassessment event | Sharp 2018 regime change (Berrios → Kaegi) | Cook County, IL only | **Robustness only** — secondary data source for the event study check |

**Unit of analysis:** parcel-transaction for replication and reduced-form mechanism tests; jurisdiction-year for structural moment matching.

**Sample size:** ~26M residential transactions (Berry's effective sample); reduced to ~15-18M after arms-length filter + ≥80% county-coverage restriction.

---

## Empirical Strategy

### Phase 1: Replicate Berry (3-6 weeks)

1. **Setup:** `00_setup.R` already wired; modify to add `library(fixest)`, `library(modelsummary)`.
2. **Clean:** `01_clean.R` reads national OT + Prop via `load_corelogic_ot()` + `load_corelogic_prop()`, joins on `clip`, applies `filter_arms_length()`, computes `assessment_ratio = assessed_value / sale_amount`. Saves intermediate parquet at `data/derived/01_property_tax_regressivity/national_panel.parquet`.
3. **Replicate:** `02_replicate_berry.R` runs Berry's main regression: $\log(\text{tax\_rate}_{ij}) = \alpha_j + \beta \log(\text{sale\_price}_{ij}) + \varepsilon_{ij}$ with jurisdiction fixed effects $\alpha_j$. Target estimate: $\hat\beta \approx -0.37$.
4. **Compare to Berry's Table 4:** report our $\hat\beta$ for matching Berry's 2007–2017 sample; report $\hat\beta$ for our extended 2007–2024 sample.
5. **Replication report:** save a local replication note with the sample definition, regression specification, and comparison to Berry's published estimate.

**Pass/fail criterion:** $|\hat\beta_{\text{ours}} - \hat\beta_{\text{Berry}}| < 0.05$ on matching sample (well within Berry's standard errors).

### Phase 2: Reduced-form mechanism tests (4-8 weeks)

For each mechanism, a separate reduced-form test:
- **H2 (cycle length):** Border-MSA design. Sample to border-MSA tracts; regress regressivity coefficient on state cycle length with MSA fixed effects.
- **H3 (density):** Within-jurisdiction tract-level. Regress assessment ratio on tract transaction density with jurisdiction × year FE. IV with Bartik shift-share.
- **H4 (institution):** Cross-jurisdiction. Regress regressivity coefficient on elected dummy with state FE.
- **H5 (appeals):** Where appeal-rate data available (subset of counties), regress appeal rate on tract income × assessment-ratio bin.
- **H6 (transaction frequency, novel):** Parcel-level. Mediator-variable analysis. Test fraction of regressivity coefficient that survives controlling for transaction-frequency-decile interaction.

Each test produces a row in Table 3 (Mechanism Tests).

### Phase 3: Structural estimation (12-18 weeks)

1. **Model implementation (Julia):** `scripts/julia/01_model.jl` implements Bayesian assessor model with closed-form posteriors. `scripts/julia/02_simulator.jl` simulates panel of parcels given parameters.
2. **Moment computation:** `scripts/julia/03_moments.jl` computes the 6 moments listed above from both simulated and observed data.
3. **SMM optimizer:** `scripts/julia/04_smm.jl` uses `Optim.jl` (or `BlackBoxOptim.jl` for global search) to minimize weighted distance between simulated and observed moments. Identity weight matrix in first stage; optimal weight matrix in second stage.
4. **Standard errors:** block bootstrap on jurisdiction-year clusters, 500 reps.
5. **Identification check:** Jacobian rank at estimated parameters (numerical derivatives).
6. **Counterfactuals:** compute $\Delta_{\text{revenue}}$ under (i) $\tau = 1$, (ii) $c_{\text{appeal}} = 0$, (iii) all-appointed. Decompose by price decile.

### Phase 4: Robustness + writing (8-12 weeks)

- Cook County 2018 Berrios → Kaegi event study (separate identification check)
- Border-MSA results limited to single MSAs with very thick coverage
- Alternative arms-length filter thresholds
- Alternative jurisdiction definitions (school district vs county vs municipality)
- Manuscript draft and internal review
- Pre-submission review against AEJ: Applied or Journal of Public Economics standards

---

## Outputs Plan

### Manuscript tables

- **T1:** Summary statistics (parcel-level, by price decile)
- **T2:** Replication of Berry — within-jurisdiction regressivity coefficient (matching sample + extended)
- **T3:** Mechanism tests, one row per hypothesis H2–H6
- **T4:** Structural estimates (parameter point estimates, SE, identification statistics)
- **T5:** Counterfactual revenue gap by price decile (baseline + 3 reforms)
- **T6:** Robustness — alternative arms-length filters, sample restrictions, FE specifications

### Manuscript figures

- **F1:** Map — within-jurisdiction regressivity coefficient by county (heatmap, US map)
- **F2:** Within-jurisdiction binscatter of assessment ratio vs. log(sale price), by reassessment-cycle quartile
- **F3:** Border-MSA scatter — regressivity coefficient by state, MSA pairs annotated
- **F4:** Decomposition — share of regressivity attributable to each mechanism
- **F5:** Revenue-gap counterfactuals, by price decile, under 3 reforms

---

## Expected Results

The researcher explicitly stated no prior — let the data inform. The interesting ex-ante questions:

1. **Magnitude of revenue gap:** $5B/yr? $20B/yr? $50B/yr nationally? Order-of-magnitude matters for the policy framing.
2. **Mechanism rankings:** Berry's information-asymmetry claim implies $\delta$ and $\kappa$ dominate. If the structural estimates instead show $\alpha_2$ (appeals wealth elasticity) and $\lambda$ (assessor objective) dominate, that's a major reframing — regressivity becomes a *political-economy* finding, not a pure-information finding.
3. **Counterfactual policy impact:** Which reform — shorter cycles, costless appeals, appointed assessors — yields the largest reduction in regressivity? If shorter cycles eliminate 60%+ of the gap, policy implication is clear: states with long cycles should shorten them.

The "surprising finding that would make this a top-3 paper" would be: **appeals technology and assessor institutions quantitatively swamp information asymmetry**, contradicting Berry's mechanism story even while replicating his descriptive result. That would be the AEJ:Applied / JPubE pitch.

---

## Contribution

This paper differs from Berry (2021) in three ways: (a) it quantifies the fiscal redistribution (revenue gap by decile) rather than only documenting regressivity exists, (b) it structurally identifies the relative contribution of five mechanisms rather than asserting one informally, and (c) it uses the structural estimates to evaluate counterfactual policy reforms. From Avenancio-León & Howard (2022 *QJE*) it differs by focusing on price-decile rather than racial regressivity and by structural estimation rather than reduced-form decomposition. From the legal-scholarship literature (Atuahene; Harvard JoL 2025) it differs by being a quantitative economics paper with a formal model, suitable for an economics journal.

**Journal targets (in preference order):** *AEJ: Applied*, *JPubE*, *National Tax Journal*, *Real Estate Economics*. The political-economy framing also opens *AEJ: Economic Policy* if mechanism-decomposition is the headline.

---

## Timeline

| Milestone | Target date |
|---|---|
| Spec approved + citation audit complete | 2026-05-20 |
| Literature review complete, ~30 entries in `Bibliography_base.bib` | 2026-05-25 |
| Phase 1 (Berry replication) complete + report | 2026-06-15 |
| Phase 2 (reduced-form mechanisms) complete | 2026-08-01 |
| EXPLORATION → ANALYSIS transition | 2026-08-15 |
| Phase 3 (structural estimation) complete | 2026-11-15 |
| ANALYSIS → WRITING transition | 2026-12-01 |
| Phase 4 (robustness + draft) complete | 2027-02-15 |
| Internal review | 2027-03-01 |
| WRITING → REVIEW transition | 2027-03-15 |
| Pre-submission journal-fit review | 2027-04-01 |
| Submit to AEJ:Applied | 2027-04-15 |

Aggressive but feasible. Structural estimation timeline can extend; reduced-form-only "Paper 1a" carve-out is available if structural stalls.

---

## Open Questions

1. **State reassessment cycle data source.** Lincoln Institute database needs verification — does it have current cycle lengths or stale data? Backup: manually compile from state DOR websites for the 10 largest states.
2. **Assessor election data.** Where does this live? IAAO directory is start; FOIA-able from SecState offices. May need RA time.
3. **Border-MSA sample size.** Sufficient parcels with cross-border comparison? Need to count parcels in border-tract sample. Could limit identification power.
4. **ACS tract income vs. parcel-level wealth proxy.** Tract income is a coarse proxy. Could augment with mortgage data (CoreLogic mortgage records, separate licensing) — defer to v2 of paper.
5. **Computational scale.** SMM with 5–7 primitives, 6 moments, simulating ~5M parcels — likely 4–12 hours per iteration on workstation. JuMP+Ipopt for derivative-based optimizer cuts this if model is differentiable.
6. **Race interaction.** Avenancio-León & Howard's racial regressivity result complements ours. Should we estimate a parallel model with race-conditional info gap, or leave that to a follow-up paper?

---

## Decision references

Local decision notes document three major choices: national all-states scope, a three-source identification strategy, and SMM rather than MLE or pure calibration for the structural extension.

---

## Citation Audit

The following citations appear in the Motivation, Identification Strategy, and Contribution sections and were checked during the initial citation audit:

| # | Citation | Claim |
|---|---|---|
| C1 | Berry, Christopher (2021) "Reassessing the Property Tax" SSRN working paper | Working paper (not yet published); 26M residential sales 2007–2017; within-jurisdiction tax-rate elasticity = −0.37; bottom decile pays >2× top decile effective rate |
| C2 | Berry, Christopher and Wang, Xiaoyan (2024) "Property Tax Assessment and Housing Market Cycles" Syracuse CPR working paper | Cited as follow-up Berry paper exploring time-series variation |
| C3 | Avenancio-León, Carlos and Howard, Troup (2022) "The Assessment Gap: Racial Inequalities in Property Taxation" *QJE* | Documents that assessment ratios are higher for properties owned by minority households |
| C4 | Hodge, Timothy R., Komarek, Timothy M., and McAllister, Andrew (2025) "A Double Negative: Capitalizing on Assessment Regressivity" *Public Finance Review* | Buyers partially price in expected over-assessment (capitalization) |
| C5 | Schleicher, David (2025) "Your House Is Worth More Than They Think: The Strange Case of Property Tax Regressivity" *Harvard Journal on Legislation* 62.1 | Legal scholarship synthesis of regressivity literature |
| C6 | Besley, Timothy and Coate, Stephen (2003) "Elected Versus Appointed Regulators: Theory and Evidence" *Journal of the European Economic Association* 1(5):1176-1206 | Methodological precedent for elected vs. appointed institutional comparison |
| C7 | Dube, Arindrajit, Lester, T. William, and Reich, Michael (2010) "Minimum Wage Effects Across State Borders" *REStat* | Methodological precedent for border-county design |
| C8 | Atuahene, Bernadette (USC Gould Law). Key works: "Predatory Cities" (2020 *California Law Review*); "Taxed Out" (2019 *UC Irvine Law Review*, co-authored) | Cited as the legal-scholarship line on civil-rights framing of Detroit property tax overassessment |

### Citation Audit Notes (2026-05-19)

| Citation | Status | Evidence |
|---|---|---|
| C1 (Berry 2021) | **PARTIAL** | PDF confirms author, affiliation, March 2021 draft, 26M sample 2007-2017, -0.37 elasticity, >2x decile gap. SSRN 403-blocked; working-paper status confirmed by the PDF itself but not independently re-validated. |
| C2 (Berry & Wang 2024) | **PARTIAL** | Syracuse Maxwell URL hosts a PDF in the property-tax webinar series 2023-2024 named "berry-and-wang-2024". PDF was binary and not parseable; title and findings unverified beyond filename. |
| C3 (Avenancio-León & Howard 2022 QJE) | **PASS** | QJE Vol 137, Issue 3, August 2022. Documents 10–13% higher tax burden on Black and Hispanic residents within jurisdiction. |
| C4 (Hodge, Komarek, McAllister 2025) | **FAIL → CORRECTED** | DOI 10.1177/10911421241280456 resolves to **Public Finance Review**, NOT *Urban Affairs Review* as originally drafted. Spec text and table corrected. |
| C5 (Harvard JoL 2025) | **PASS** | Confirmed: Schleicher, David. "Your House Is Worth More Than They Think." HJoL 62.1 (Winter 2025). Author name added. |
| C6 (Besley & Coate 2003) | **FAIL → CORRECTED** | Published in **Journal of the European Economic Association** 1(5):1176-1206, NOT the *American Economic Review* as originally drafted. Spec text and table corrected. |
| C7 (Dube, Lester, Reich 2010) | **PASS** | REStat 92(4):945-964, 2010. Border-county design confirmed. |
| C8 (Atuahene) | **PASS** | Confirmed USC Gould Law affiliation. Key works added: "Predatory Cities" (California Law Review 2020), "Taxed Out" (UC Irvine Law Review 2019). |

**Summary:** 4 PASS, 2 PARTIAL, 2 FAIL. Both FAILs were wrong journal attributions and were corrected in-place above. PARTIALs (C1, C2) are gaps in independent access (SSRN 403, Syracuse PDF binary), not contradictions of the claims.

**Spec status:** DRAFT → **APPROVED** (citations reconciled). Sources: see footer of this spec.

### Source URLs (checked 2026-05-19)

- [Berry 2021 SSRN](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3800536) (access blocked at verification time; PDF locally at `docs/berry2021.pdf`)
- [Berry & Wang 2024 Syracuse Maxwell PDF](https://www.maxwell.syr.edu/docs/default-source/research/cpr/property-tax-webinar-series/2023-2024/berry-and-wang-2024-accessible.pdf)
- [Avenancio-León & Howard 2022 QJE (Minneapolis Fed working-paper page)](https://www.minneapolisfed.org/research/institute-working-papers/the-assessment-gap-racial-inequalities-in-property-taxation)
- [Hodge, Komarek, McAllister 2025 PFR (SAGE)](https://journals.sagepub.com/doi/abs/10.1177/10911421241280456)
- [Schleicher 2025 HJoL](https://journals.law.harvard.edu/jol/2025/02/22/your-house-is-worth-more-than-they-think-the-strange-case-of-property-tax-regressivity/)
- [Besley & Coate 2003 JEEA (Wiley)](https://onlinelibrary.wiley.com/doi/abs/10.1162/154247603770383424)
- [Dube, Lester, Reich 2010 REStat (MIT Press)](https://direct.mit.edu/rest/article/92/4/945/57855/Minimum-Wage-Effects-Across-State-Borders)
- [Bernadette Atuahene profile (Wikipedia)](https://en.wikipedia.org/wiki/Bernadette_Atuahene)
