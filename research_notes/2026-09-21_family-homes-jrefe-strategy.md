# Family homes: revision strategy after the JREFE desk rejection

Date: 2026-09-21. Status: recommendations, not adopted specifications.

Evidence: complete 29-page September 2, 2026 paper.pdf, including appendix; supplied decision letter; project README, research spec, and the header of 08_within_ca.R. Numerical findings below are reported manuscript results, not independently reproduced estimates. Framework and Table 4 were also checked visually. No manuscript, analysis code, or data were changed.

## Recommendation

Agree with the editor that the paper needs a stronger account of economic consequences. Do not interpret this as a requirement to build a large structural welfare model. The paper already has a keep-or-sell model (Section 5). Its immediate problem is the gap between an estimated response of recorded deeds and the claimed response of housing allocation and welfare.

Recommended next version: national measurement plus tax-sensitive family conveyancing, a repaired framework, and a focused attempt to measure real allocation. Journal of Housing Economics is the most natural next economics-field target in my assessment; JRER and JHR are alternatives for a stronger measurement/transactions version. REE and JUE are ambitious options after substantial empirical improvement, not easier fallbacks from JREFE. These are judgments, not acceptance predictions.

## What the paper establishes and what it does not

| Evidence | Location | Interpretation |
|---|---|---|
| Approximately 19.7 million family deeds, covering 16.5 million distinct parcels | Tables 1-2, pp. 7-8, footnote 1 | Large channel of recorded legal transfers. Distinguish events from homes and transfers from deaths. |
| At ten years, about 62% of same-surname family homes versus 58% of market purchases remain unsold | Section 4 and Figure 3, pp. 10-12 | Four percentage points of unadjusted retention difference, not 62% of houses inefficiently withheld. |
| Approximately 31% anticipatory excess | Section 6.2, pp. 14-15 | Strong evidence of tax-sensitive transfer timing. Does not identify the timing of occupancy changes or death. |
| SCM decline 33%, placebo p=0.039; per-65+ decline 26%, p=0.137 | Section 6.3, pp. 16-17 | Persistent recorded-transfer response with specification-dependent uncertainty. Two estimates do not create an identified interval or establish steady state. |
| Roughly 58,000 missing deeds after market-sale adjustment | Table 4 and Section 7 | Derived quantity for a different specification; appendix reports placebo p=0.176 for the placebo-netted effect. Do not attach SCM significance to this count. |
| Resale triple difference -0.74 percentage points, p=0.67; absentee share approximately flat | Sections 6.4 and Appendix B | No persuasive independent confirmation of the selection mechanism. Conditioning on surviving family deeds changes the population. |
| No detected relative rise in estate/trust market sales | Section 6.5, p. 20 | Consistent with delayed succession, but not proof that missing deeds are predominantly inter vivos or that future sales will rise. Report uncertainty and measurement coverage. |

The strongest overstatement is the claim on pp. 4, 13, and 20 that supply release is real but deferred. Replace it with a conditional prediction. Not rejecting a sales null does not establish delay, much less eventual welfare gains.

## Why the existing framework needs repair

1. **Ownership and use differ.** An heir can retain title while renting the property to another household. That household can be well matched to the home even if no market sale occurs. Conversely, a buyer can leave a purchased house vacant. A sales count does not establish housing-service allocation.
2. **Preferences count.** Attachment, family insurance, saved moving costs, and transaction-cost savings can justify retention. These are benefits to include, not errors to assume away.
3. **The policy changes multiple incentives.** Proposition 19 preserves a conditional exclusion for qualifying owner-occupant heirs and includes a separate portability reform. It does not simply eliminate every allocation wedge. It can encourage an heir to move into a home as well as encourage a sale.
4. **Advance deeds are a separate decision.** Paperwork can respond to taxes years before any actual succession or change in housing use. The current static model cannot turn the decline in that paperwork into an identified number of eventual sales.
5. **Tax payments are not themselves resource losses.** Reduced family tax benefits and increased public revenue belong on opposite sides of an incidence account. Social gains require changes in real use, costs, externalities, or explicitly weighted distribution.

The BOE explains both the occupancy conditions and separate operative dates: [Proposition 19](https://www.boe.ca.gov/prop19/). Glaeser and Luttmer's empirical misallocation exercise studies consumption patterns across demographic groups; it does not infer misallocation solely from low turnover: [AEA article](https://www.aeaweb.org/articles?id=10.1257%2F000282803769206188).

## Smallest useful framework

Distinguish advance title planning from the eventual succession decision. At succession, include sale, heir occupancy, rental, and vacancy/occasional use. Map each choice to observable deeds, occupancy, rental activity, and tax assessments. Allow rental management costs, attachment, transaction costs, and the occupancy-conditioned tax benefit.

The useful predictions concern which margins change under which conditions. They should permit no immediate allocation change despite a large deed response. A lower post-transfer sale hazard is not automatic: it requires a connection between the characteristics selecting heirs into retention and their future sale hazards.

In the present paper's much narrower static, fixed-price, costless setting, let tau_i = t(p_i-A_i) be the annual tax advantage and d_i = u_mi-u_hi the annual valuation gap. A household induced to retain solely by that wedge satisfies 0 < d_i <= tau_i. For a *known set of actual allocation switchers*, this bounds the corresponding annual direct allocation loss by the sum of their wedges. It does not identify the switchers, the valuation gaps, or total policy welfare. Multiplying 58,000 missing deeds by an average tax benefit, house value, or an arbitrary one-half factor would not estimate welfare. A finite-horizon present-value exercise would also need succession timing, survival of the allocation, and discounting.

Do not calibrate a dollar welfare estimate until real behavioral switchers and their relevant wedges can be credibly measured. A sensitivity calculation must be labeled as a scenario, with zero immediate allocation change admitted by current evidence.

## Incidence ledger for any eventual welfare extension

Comparator: inheritance exclusion versus the narrowed exclusion, separating the portability component. Specify the horizon, affected cohort, government boundary, price year, and financing assumption before computing.

| Actor/component | Potential consequence | Evidence needed | Current status |
|---|---|---|---|
| Parents and heirs | Tax liabilities, attachment, family insurance, tenure choice | Historical assessments, eligibility, ownership/use histories, valuation assumptions | Deed behavior observed; benefits and preferences not identified |
| Buyers/occupants | Changes in matching and access | Actual allocation changes, occupant characteristics, alternative housing options | Not identified by missing deeds |
| Tenants | Rental availability, rents, displacement or continuity | Rental listings/leases/occupancy histories | Not measured in the supplied results |
| Governments | Revenue and administrative costs | Tax rolls, reassessments, collection and administration | Fiscal effect not estimated |
| Real resources | Moving, selling, legal planning, management and vacancy costs | Relevant quantities and costs | Unmeasured |

Avoid double-counting sale prices or capitalized tax benefits as additional surplus. Keep distributional weights explicit. No MVPF ratio is justified from current inputs.

## Revision priorities

1. **Align the claims with the evidence now.** Use recorded-transfer responses, not established supply release or welfare losses, in the abstract and conclusion. A possible title is "Keeping the House in the Family: Property Taxes and Non-Market Housing Transfers."
2. **Validate the central measurement contribution.** Audit a stratified sample against deed/assessor records where feasible; distinguish trust-involved conveyances from demonstrated self-retitling. Same surname plus an administrative flag is not logically a strict lower bound without a false-positive argument. Show classification stability around the reform. Distinguish legal transfer dates from recording dates and deaths from gifts.
3. **Improve the retention comparison.** Standardize or match on location, cohort, property age/value, and observable prior tenure. Report risk sets and censoring at ten years. Explain the roughly four-point gap relative to market buyers. No observed arm's-length sale is not a complete ownership-chain proof of continuous family control.
4. **Seek one decisive real-outcome exhibit.** Follow a population defined before the reform, including homes that never generate a subsequent family deed. Study market entry, heir occupancy, rental continuation/conversion, and vacancy. A historical assessment/ownership panel or linkage to deaths and succession records would be much more useful than additional algebra. Confirm access and temporal coverage before promising these extensions.
5. **Revisit mechanism tests without selecting results by sign.** The README records that the within-California occupant/absentee design was tried and dropped as causal after an unexpected sign. The script classifies using buyer ZIP at the deed; the property occupancy fields are a later snapshot. Neither automatically supplies exogenous pre-policy exposure. Interpret and disclose the diagnostic; rebuild with valid prereform measures if possible. A present-day assessment gap can reflect the policy itself.
6. **Make uncertainty coherent.** Put estimates and their own placebo inference together. Explain donor weights, exclusions, and pre-fit. Treat the sales adjustment as a sensitivity specification because portability and inheritance responses can affect its denominator. Call 2022-2023 a post-reform period rather than demonstrated steady state. An elderly-population denominator is not direct mortality adjustment and is not automatically a conservative bound.

Before adding an ambitious model, also compare the contribution carefully with the paper already cited by Coven, Golder, Gupta, and Ndiaye on taxes and allocation under financial constraints. A broad lifecycle model would move this paper into a more demanding comparison; the distinctive advantage here is observed parcel-level family conveyancing.

## Journal strategy

These assessments draw on official scopes but the suggested revision thresholds are my editorial judgment.

| Outlet | Suitable version | Role of welfare/model |
|---|---|---|
| [Journal of Housing Economics](https://shop.elsevier.com/journals/journal-of-housing-economics/1051-1377) | National housing-transfer measurement plus credible policy evidence | Preferred next target after substantive revision. Compact framework useful; dollar welfare not inherently required. |
| [Journal of Real Estate Research](https://www.tandfonline.com/journals/rjer20) | Validated transaction taxonomy, turnover and institutional consequences | Substantial new empirics matter more than a large model; unsupported calibration adds vulnerability. |
| [Journal of Housing Research](https://www.tandfonline.com/journals/rjrh20) | Focused empirical paper on housing transfers and subsequent transaction outcomes | Large welfare exercise unnecessary for the proposed contribution. |
| [Real Estate Economics](https://www.areuea.org/real-estate-economics) | Stronger real-estate allocation or market consequence | Ambitious option; welfare can help if identified, but a strong empirical contribution can stand without it. |
| [Journal of Urban Economics](https://shop.elsevier.com/journals/journal-of-urban-economics/0094-1190) | Clear result on allocation, matching, tenure or mobility | Ambitious option after real-outcome evidence. Formal welfare model optional, not a substitute for identification. |
| [National Tax Journal](https://www.journals.uchicago.edu/new) | California-centered paper on inheritance taxation, planning, tax base and incidence | Fiscal stakes and institutional precision may be more useful than a full housing-market welfare model. |
| [Housing Policy Debate](https://www.tandfonline.com/journals/rhpd20), [Housing Studies](https://www.tandfonline.com/journals/chos20) | Reframed around housing access, intergenerational advantage, tenants and policy tradeoffs | A market-efficiency calibration could distract if it neglects family benefits and distribution. These outlets require substantive audience-specific reframing. |

JREFE's [published scope](https://link.springer.com/journal/11146/aims-and-scope) includes housing, institutions and public policy. Read this rejection primarily as a contribution/significance assessment of this manuscript, not a general incompatibility of the topic. The letter does not invite resubmission. It does not establish a journal-wide welfare-model requirement.

JHE has published directly related work on housing wealth and bequests: [Legacies of homeownership](https://www.sciencedirect.com/science/article/abs/pii/S1051137715300103). That is evidence of audience fit, not a guarantee about this manuscript.

Decision rule: attempt the historical-data/real-outcome improvement first. If it produces credible allocation evidence, consider REE/JUE and a disciplined welfare extension. If it does not, retain the measurement and tax-planning contribution, narrow the claims, and target JHE/JRER/JHR rather than forcing a welfare number.
