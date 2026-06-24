# Research Spec: The Machine in the Market

**Slug:** `algorithmic_intermediation`
**Date drafted:** 2026-06-10
**Status:** APPROVED

---

## Research Question

> Does algorithmic intermediation — iBuyers quoting firm prices from statistical
> valuation models — improve information aggregation in decentralized housing
> markets, and what happens to price discovery when the algorithm exits?

Why this matters: Housing is the largest household asset class (~$45T in the US)
and the canonical decentralized market: heterogeneous assets, infrequent trade,
no market maker. iBuyers are the first deployment of AI pricing at transaction
scale in this market. Whether machine quotes act as public signals that sharpen
price discovery — or merely cream-skim easy homes — determines the welfare
stakes of AI in the housing market. The Nov 2021 Zillow Offers collapse provides
the first large-scale natural experiment in *removing* an algorithmic trader
from a consumer asset market.

## Hypotheses

| # | Hypothesis | Direction | Falsifiable? |
|---|------------|-----------|--------------|
| H1 | iBuyer entry reduces within-ZIP hedonic price dispersion among non-iBuyer trades | − | yes |
| H2 | iBuyer entry raises arm's-length transaction volume | + | yes |
| H3 | Zillow's exit raises dispersion in Zillow-exposed ZIPs relative to Opendoor-exposed ZIPs | + | yes |
| H4 | Effects concentrate in "machine-readable" (homogeneous) segments | hetero | yes |

## Identification Strategy

**Setting:** Staggered iBuyer entry across CBSAs (2014–2021); abrupt nationwide
Zillow Offers shutdown (announced 2021-11-02), driven by the firm's national
pricing-algorithm failure, not local conditions.
**Method:** Callaway–Sant'Anna staggered DiD (entry); DiD + triple-difference
(exit) with continuous exposure = pre-period Zillow purchase share.
**Identifying assumption (exit design):** Conditional on CBSA × quarter fixed
effects and pre-period iBuyer activity, ZIPs with high Zillow exposure would have
trended like ZIPs with high Opendoor/Offerpad exposure absent the shutdown.
**Threats:** (1) 2022 rate shock hit Sunbelt iBuyer markets hardest → addressed
by using Opendoor-exposed ZIPs as controls (same selection, algorithm stayed);
(2) Zillow's inventory liquidation is a supply shock bundled with the
information shock → separate flow (volume) from information (dispersion)
outcomes, drop quarters with active liquidation in robustness; (3) endogenous
entry → entry design uses not-yet-treated controls + event-study pretrends.

## Data Requirements

| Source | Variables | Coverage (geo / time) | Notes |
|---|---|---|---|
| CoreLogic OT | sale amount/date, buyer/seller names, corp indicators, ZIP, FIPS, deed/sale type, resale & arm's-length flags | iBuyer-active states, 2000–2024 | loader: `load_corelogic_ot()` |
| CoreLogic Prop | clip, sqft, beds, baths, year built, lat/long, CBSA, land use | same states | hedonics + stock counts |
| Public record | entry/exit dates per market, entity aliases | — | fact-pack agent, cross-validated vs deeds |

## Empirical Strategy

1. Sample: single-family + condo arm's-length deeds, price $10k–$10M,
   non-interfamily, resale or new construction.
2. Treatment: iBuyer = buyer/seller name matches curated entity-alias regex.
3. Primary spec (exit):
   `y_zt = β·ZillowShare_z × Post_t + γ·OpendoorShare_z × Post_t + α_z + δ_{c(z),t} + ε_zt`
4. SEs clustered at CBSA level (entry) and ZIP level w/ CBSA-cluster robustness (exit).
5. Robustness: drop liquidation quarters (2021Q4–2022Q2); binary exposure terciles;
   Callaway–Sant'Anna for entry; permutation inference on exposure.

## Outputs Plan

- Tables: T1 summary stats; T2 entry effects (volume, dispersion); T3 exit DiD;
  T4 heterogeneity by machine-readability; T5 robustness
- Figures: F1 iBuyer activity time series/map; F2 entry event study; F3 exit
  event study (Zillow vs Opendoor exposure); F4 dispersion around exit

## Timeline

| Milestone | Target date |
|---|---|
| ANALYSIS → WRITING | 2026-06-10 |
| WRITING → REVIEW | 2026-06-10 |
| Submit to QJE/JPE | TBD (user decision) |

## Open Questions

- Final state list (derive from national scan, not assumption)
- Whether listing-based outcomes (time on market) are needed — not available in
  deeds; we proxy liquidity with resale spells + volume
