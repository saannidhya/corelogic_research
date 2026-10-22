# Assessment models and regressivity: feasibility and collaboration memo

Prepared for Saani Rawat on 2026-10-18. Targeted public-source research, not an exhaustive literature review. Suggested email: [draft](2026-10-18_tanner_regan_email_draft.md).

## Recommendation

Answer Tanner **yes**: the desired data are feasible in at least one major U.S. jurisdiction. Start with Cook County as a production-model laboratory. Keep King County as a documented regression-based comparison; Philadelphia is a plausible office to approach if its underlying modeling files can be obtained.

The prospective paper should ask: **How much assessment regressivity is avoidable with the assessor’s existing information, how much requires better information, and how do administrative stages change the resulting allocation?** This is a research proposal, not an established gap. A successful pilot must establish both reproducibility and a contribution beyond the close papers below.

Do not pitch the fact that conditional-mean predictions compress dispersion as a new discovery. Tanner’s own research page lists *The economic consequences of statistical property valuation: Evidence from Kampala, Uganda* as work in progress. Ask how the U.S. project could extend his framework rather than explaining his idea back to him as a new proposal. [Tanner’s research page](https://tannerregan.github.io/research.html), work-in-progress section, accessed 2026-10-18. The present task did not recover the current full paper or establish its detailed assumptions.

## 1. What can actually be obtained?

### Cook County: strongest starting point

The official residential repository documents features, code, separate training and assessment inputs, and historical model versions. It lists township-level linear regressions for 2009–2017 and countywide LightGBM from 2021. Its assessment population includes sold and unsold homes; it is **not** the complement of the training sales. The public input links cover several years, including 2024. Exact estimation/test membership and the chosen production artifact still require verification. Condominiums use a separate model. [CCAO residential repository](https://github.com/ccao-data/model-res-avm), “Prior Models,” “Data Used,” “Getting Data.”

For 2024, the documentation distinguishes the early run used for Rogers Park and West from the subsequent run used for other Chicago townships. Match each township to its actual run rather than assuming one annual model. The following two later-run input URLs returned HTTP 200 to unauthenticated HEAD requests during this task:

- [2024 assessment input](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id%3D2024-03-17-stupefied-maya/assessment_data.parquet): 309,845,333 bytes.
- [2024 training input](https://ccao-data-public-us-east-1.s3.amazonaws.com/models/inputs/res/2024/run_id%3D2024-03-17-stupefied-maya/training_data.parquet): 163,303,649 bytes.

These checks establish file availability, **not** schema completeness, exact sample reconstruction, or a reproduced model. No full input files were downloaded.

| Tanner’s requirement | Evidence and remaining verification |
|---|---|
| Actual characteristics | Documented features and public input files; check year-specific transformations and vintage. |
| Mapping from characteristics to value | Public code and historical versions; verify fitted artifact, parameters, seed, and run manifest. |
| Training and prediction observations | Separate model-development and assessment files; recover exact estimation/validation/holdout membership. |

The current assessed-values dataset provides `mailed_tot`, `certified_tot`, and `board_tot` by parcel and year. These are assessed values, requiring the appropriate assessment-level conversion before comparison with predicted market values. The current schema does **not** itself include a raw prediction field. The appeals dataset is separate, may contain open cases, and warns of gaps in historical appeals. Its stale notice expecting restoration by end-2025 is not evidence that restoration happened. [Assessed values](https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Assessed-Values/uzyt-m557), [appeals](https://datacatalog.cookcountyil.gov/Property-Taxation/Assessor-Appeals/y282-6ig3). Both metadata/schema endpoints were read directly during this task.

The office’s earlier release explicitly discusses model-stage values and warns that modeling values are absent before 2019. Treat that as evidence of a historical release, not proof that the current assessed-values table contains those columns. Raw production predictions must be obtained from model artifacts, an appropriate archived release, or the office. [Original valuation-data release](https://datacatalog.cookcountyil.gov/stories/s/Cook-County-Assessor-Valuation-Data-Release/p2kt-hk36/), February 2020 update.

The desk-review policy confirms that analysts can change software-generated values and documents review criteria. It says an unusually high or low prediction ratio alone does not justify a change. Thus a model-to-mailed difference is not automatically an appeal effect or discretionary favoritism: it can reflect corrected characteristics and other review decisions. Obtain correction/reason logs if explaining those differences becomes central. [Desk-review SOP](https://github.com/ccao-data/wiki/blob/master/SOPs/Desk-Review.md), “Desk Review Procedure.”

Useful official access route: the Data Department invites questions at **Assessor.data@cookcountyil.gov**. A future inquiry should request the chosen run’s frozen artifacts and stage definitions, not generic access to information already public. No inquiry was sent. [2023 open-data refresh](https://datacatalog.cookcountyil.gov/stories/s/Assessor-2023-Open-Data-Refresh/9bqn-cfsv/).

### King County: concrete regression-based alternative

The 2024 Downtown Seattle condominium report includes a model specification (printed p. 9), a numerical calibration formula and exceptions (p. 10 onward), sales used (p. 19 onward), and sales removed (p. 47 onward). It therefore supplies unusually concrete evidence for Tanner’s model and training-sample questions. The prediction-population summary is on p. 1, but a complete parcel-level population is described as available in assessor files; this task did not retrieve it. Some variable coding also requires further documentation. This is a specific condo submarket, not evidence that every King County property is valued identically. [Official report](https://kingcounty.gov/en/dept/assessor/buildings-and-property/property-value-and-information/reports/area-reports/2024/-/media/king-county/depts/assessor/buildings-property/reports/area-reports/2024/commercial/700_02.ashx). PDF indices differ from printed page numbers by five.

### Philadelphia: credible, but less complete access verified

OPA’s tax-year-2027 methodology explicitly describes multiple regression, property characteristics, geographic stratification, time adjustments, and applying coefficient files to sold and unsold parcels. The published overview does not establish that exact fitted coefficients, frozen input matrices, and training/holdout membership are downloadable. Ask for those files before committing to a replication there. [Official methodology overview](https://www.phila.gov/media/20260629163819/opa-tax-year-2027-mass-appraisal-valuation-methodology-full-overview.pdf), PDF pp. 5–13; [methodology archive](https://www.phila.gov/documents/assessment-methodologies/).

The answer to Saani’s question about whether U.S. offices actually use statistical models is therefore unambiguously yes, based on these specific offices. Do not infer universal adoption from them. A Lincoln/IAAO survey also documents use and adoption barriers, but its respondent statistics are not a nationally representative census of offices. [Bidanset and Rakow, July 2022](https://www.lincolninst.edu/publications/working-papers/2019-survey-use-automated-valuation-models-avms-in-government-assessment/).

## 2. What does regression to the mean establish?

**Interpretation:** Tanner’s mechanism is compelling as an explanation to quantify, but “regression models are designed to be regressive” is an imprecise way to describe it. Separate the prediction target, information set, estimation procedure, and tax-allocation objective. An accurate conditional-mean predictor can be compressed relative to realized prices without any intent to favor expensive homes. That alone does not identify which practical remedy will help.

An illustrative log-price model makes the measurement issue precise. This is an algebraic example developed here, not a claim about the exact functional form in Tanner’s paper or Cook County’s model:

\[
p=m(X)+u,\quad E[u\mid X]=0,\qquad a=c+m(X).
\]

Here \(p\) is log sale price, \(a\) is log predicted assessment, and \(c\) is a constant assessment scaling factor. The population slope in the regression of \(a-p\) on \(p\), with an intercept, is

\[
\beta=\frac{\operatorname{Cov}(a,p)}{\operatorname{Var}(p)}-1
=-\frac{\operatorname{Var}(u)}{\operatorname{Var}(p)}.
\]

For an in-sample OLS fit of log price with an intercept, regressing fitted-log-price minus log price on log price gives the exact identity **\(\beta=R^2-1\)**. It is therefore insufficient to fit a regression, find a negative assessment-ratio slope in that same sample, and call this a new behavioral finding. The identity is not a general formula for every valuation model, retransformation, or held-out sample.

Now distinguish persistent log value from transaction noise: \(v=m(X)+r\), \(p=v+\eta\). Under mean independence of \(r\) from \(X\), and independence of \(\eta\) from \((X,r)\), the same calculation gives

\[
\beta=-\frac{\operatorname{Var}(r)+\operatorname{Var}(\eta)}{\operatorname{Var}(p)}.
\]

The residual can therefore contain both information about durable property quality missing from the assessor’s records and temporary sale-price noise. The empirical distinction matters: gathering better characteristics may help with the first; treating every transaction-specific price deviation as taxable value can reproduce noise. Out-of-sample evaluation addresses fitting optimism but does not by itself remove noisy-value-proxy bias.

Measurement concerns are established in the literature: McMillen and Singh warn about regression-based measures and propose complementary distributional approaches. Those approaches are useful checks; neither ranks nor a distribution comparison should be described as automatically immune to noisy sale prices. [2022 working-paper abstract](https://www.lincolninst.edu/publications/working-papers/measures-vertical-inequality-in-assessments/).

Do not assume that greater flexibility or less regularization always improves equity, or that an inverse rescaling of compressed predictions is a free correction. Evaluate accuracy, tax shares, and horizontal equity jointly on unseen data. A different loss function changes the statistical target; compare its implications rather than labeling it inherently fair.

## 3. A feasible study, and what it could identify

**Primary unit:** parcel × assessment year, retaining transaction IDs for sale records and card/building IDs until aggregation is validated. **Initial scope:** one completed Cook County reassessment cycle and eligible residential classes. **Initial output:** reproducible production mapping and counterfactual incidence, not a causal effect of statewide reform.

Use a small, pre-specified comparison matrix:

| Change | Hold fixed | Question |
|---|---|---|
| Algorithm/functional form | Frozen assessor information and training records | How much can alternative approximation of the same information change outcomes? |
| Information set | Learning/tuning protocol and evaluation population | Which additional pre-assessment information changes persistent errors and tax shares? |
| Objective/calibration | Information, data split, and model capacity where possible | How does the policy objective change the accuracy/equity tradeoff? |
| Administrative stage | Same parcel-year and evaluation sale | How do model, mailed, and certified outcomes differ? |

These are conditional computational counterfactuals. Changing one input to a reproducible algorithm identifies that input’s effect within the specified pipeline; it does not identify a causal effect of institutional adoption. Algorithm/information effects can interact. Report the full matrix or alternate orderings rather than forcing an order-dependent sequence into uniquely identified mechanism shares.

Freeze the valuation date and what was observable then. Test on later eligible sales, time-adjusted to the valuation date, with no access to their sale prices during fitting, correction, or model selection. Use parcel-disjoint evaluation so earlier sales of the same home do not silently appear in training unless that overlap is deliberate and reported. Check whether held-out homes have updated characteristics unavailable for unsold homes. Keep an untouched test set after selecting counterfactuals.

Repeat transactions from CoreLogic can inform whether errors persist and can support value-proxy sensitivity work. They are not automatically valid instruments: persistent omitted quality, renovations, local appreciation, selection into resale, and assessor use of earlier sales can matter. Use a latent-value/repeated-measure framework only with explicit assumptions and sensitivity bounds. An alternative is independently obtained appraisals; access has not been established. Never use the outcome sale price to determine the counterfactual assessment or deployment-time price segment.

Evaluate held-out prediction errors by value group, calibrated aggregate assessment levels, within-area horizontal dispersion, several vertical-equity measures, and persistence of parcel-level errors. Keep sold-sample evidence distinct from extrapolation to all homes. Reweighting on observables cannot solve selection on unobserved condition or sale readiness. Greater within-tract regressivity by itself does not establish a neighborhood-anchoring mechanism.

For incidence, a first transparent exercise fixes the levy \(L_j\) within a taxing district and assigns \(T_i^{cf}=L_j B_i^{cf}/\sum_{k\in j}B_k^{cf}\), where \(B\) is the relevant taxable base. Start with a simplified eligible-residential allocation if necessary and label it. Actual bill counterfactuals need overlapping taxing districts, other property classes, equalization, exemptions, and applicable limits. A fall in assessed values is not automatically lost revenue when rates adjust. Do not equate low property value with low household income, or assessment regressivity with complete welfare incidence.

## 4. Closest papers and reading priorities

This is a dated supplement to the existing [project literature review](../quality_reports/lit_review_regressivity_DRAFT.md), preserving that file and its notes. No Zotero records or master bibliography entries were changed.

| Priority / source | Verified version and coverage | Relevance |
|---|---|---|
| 1. Meng Liu, *Algorithmic Governance and Tax Equity: Evidence from Property Assessment Reform in Cook County* | April 8, 2026 working paper; SSRN 6543863; [author PDF](https://www.meng-liu.com/uploads/1/2/2/6/122677253/algo_property_tax_mengliu_04082026.pdf), abstract, introduction, institutional setting, and contribution skimmed. [SSRN metadata](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=6543863). | Direct overlap: Cook County reform, price/race gradients, pooling, and mailed versus post-appeal assessments. Do not pitch a simple Cook reform event study or stage comparison as sufficient novelty. No independent assessment of its causal validity was performed. |
| 1. Evelyn Smith, Emma Harvey, Christopher Berry, Jacob Goldin, Daniel E. Ho, *Tradeoffs are Domain Dependent: Improving Accuracy and Fairness in Property Tax Assessments* | FAccT 2026; DOI 10.1145/3805689.3812318; [arXiv v1](https://arxiv.org/abs/2605.15020), May 14, 2026; [HTML](https://arxiv.org/html/2605.15020v1), abstract, framework, data/model/evaluation sections and Appendix C skimmed. | Direct overlap with richer information, alternative prediction models, and accuracy/equity comparisons using national Cotality/CoreLogic data. A generic feature-enrichment experiment is already occupied. |
| 1. Tanner Regan, *The economic consequences of statistical property valuation: Evidence from Kampala, Uganda* | [Current research listing](https://tannerregan.github.io/research.html), work in progress; metadata only. No current full-text version recovered. | Obtain from Tanner before fixing the U.S. contribution or claiming a theoretical result is new. |
| 1. Natee Amornsiripanitch, *Why Are Residential Property Tax Rates Regressive?* | [Philadelphia Fed WP 22-02](https://www.philadelphiafed.org/consumer-finance/mortgage-markets/why-are-residential-property-tax-rates-regressive), January 2022 abstract read; PDF accessed, not fully read. [SSRN record](https://papers.ssrn.com/sol3/papers.cfm?abstract_id=3729072) reports revision April 19, 2024. | Covers information/valuation methods, measurement, and reappraisal; the 2022 and 2024 abstracts differ. Do not treat the old decomposition or framing as the current version without checking it. |
| 1. Daniel McMillen and Ruchi Singh, *Measures of Vertical Inequality in Assessments* | [Lincoln WP](https://www.lincolninst.edu/publications/working-papers/measures-vertical-inequality-in-assessments/), August 2022 abstract read. Published version recorded in existing project bibliography as JHE 2023; that publication record was not independently reverified here. | Essential measurement guardrail. Full methodological reading needed before selecting a primary equity statistic. |
| 2. Ozan Candogan, Feiyu Han, Haihao Lu, *Achieving Fairness and Accuracy in Regressive Property Taxation* | [arXiv:2312.05996v1](https://arxiv.org/abs/2312.05996), December 10, 2023; abstract only. | Segmented valuation model and fairness measures; check full paper before proposing segmentation as a contribution. Publication status beyond this preprint not established here. |
| 2. Daniel McMillen and Ruchi Singh, *Assessment persistence* | [Publisher abstract](https://www.sciencedirect.com/science/article/abs/pii/S016604622600061X), 2026; DOI 10.1016/j.regsciurbeco.2026.104251; abstract only. | Direct warning that cross-sectional regressivity and persistence of individual assessment errors are different objects; a persistence exercise alone is insufficient novelty. |

**Potential distinction to investigate:** combine Tanner’s theory with an exactly reconstructed production information set to distinguish avoidable persistent valuation error from transaction noise, then assess feasible allocation rules under that information constraint. The literature search does not establish that this combination is unoccupied. The binding constraint is now the contribution and credible measurement of persistent value, alongside production-run reproducibility; it is not simply obtaining any property data.

## 5. Cheapest informative pilot and next conversation

1. **Access/reproduction gate.** Select one completed assessment year, freeze the appropriate township/run mapping, retrieve inputs and fitted outputs, and verify sample flags and feature vintages. Reproduce published predictions to a documented tolerance before developing counterfactuals. If the fitted artifact is unavailable, ask the office; a fresh refit is not an exact reproduction.
2. **Stage/linkage gate.** Validate string PINs with leading zeros, parcel-year uniqueness, card aggregation, assessment-level units, and stage completion. Distinguish PIN from CLIP. Report CoreLogic linkage rates by price group and geography and manually inspect split/merged parcel cases. A county-only pilot can proceed if CoreLogic linkage is poor.
3. **Contribution/measurement gate.** Read Tanner’s current paper and the two close 2026 papers in full; reproduce the simple shrinkage benchmark. Decide whether credible repeated-value evidence adds enough beyond existing studies. Only then select the small counterfactual matrix and evaluation period.
4. **Decision.** Proceed if the production process can be reconstructed and the persistent-value/information question remains substantively distinct. If only current assessments and generic covariates are obtainable, narrow the claim or change setting; additional machine learning does not resolve an unidentified mechanism.

For a future office inquiry, ask for: (a) frozen production run ID, commit, fitted model and tuning metadata; (b) training/validation/test manifests and sales exclusion flags; (c) exact characteristics as known at valuation; (d) raw and post-processing outputs linked to mailed values; (e) correction/review reason codes; (f) archived completeness and property-ID crosswalk guidance. This task prepares the research question and reply only; no message was sent to Tanner or an office.

## 6. Fit with the current repository

The working [manuscript](../manuscript/paper.tex), data section, and [README](../README.md) identify an assessment snapshot predominantly from 2008–2009 and a 2007–2010 analysis window. Consequently, do not promise a national contemporary assessment panel. County administrative records can supply the contemporary model/assessment side. CoreLogic’s transaction history can potentially add repeated sales and broader context, subject to matching and date coverage.

Existing [IV notes](../quality_reports/specs/iv_division_bias_results.md) and the [methods memo](../quality_reports/iv_methods_memo.md) show that measurement is already an active issue in this project. Their coefficients and identifying claims were not audited or rerun here; do not put them in the email as verified new evidence. In particular, a rejected overidentification test is not resolved by saying the sample is large, and a repeat-sales IV is not automatically a LATE.

This proposed collaboration extends the mechanism question; it does not silently replace the current manuscript or commit Saani to a new empirical pipeline.

## Search and verification record

- Search date: 2026-10-18, America/Chicago. Sources: official county/city documentation, official repository and data schemas, author sites, arXiv, SSRN, Lincoln Institute, and Philadelphia Fed. Search snippets and secondary sources were used to locate primary materials; substantive claims above rely on the linked primary sources.
- Representative queries: “Cook County assessor residential valuation model github training data assessment”; “Philadelphia assessment model code training data”; “King County assessor residential regression report”; “property tax regressivity regression to mean measurement error”; exact titles/authors for the close papers; Tanner’s research page.
- Executed access checks: HTTP HEAD for the two 2024 public inputs; direct metadata/schema retrieval for `uzyt-m557`, `y282-6ig3`, and `wvhk-k5uv` via `https://datacatalog.cookcountyil.gov/api/views/{id}.json`.
- Not executed: model reproduction, full input inspection, CoreLogic joins, sample counts, empirical regressions, or tax simulations. No raw CoreLogic data were read or modified.
- Limits: this is a targeted feasibility and contribution screen. Papers are labeled by actual reading coverage. No independent verifier-agent pass was run; the local verify-claims skill’s referenced agent/protocol files are absent. Source support and wording were checked directly.
