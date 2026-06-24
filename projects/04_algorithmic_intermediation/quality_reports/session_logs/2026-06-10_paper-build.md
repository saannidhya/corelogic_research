# Session Log: Project 04 build — 2026-06-10

## Goal
Autonomous end-to-end build of a top-5-targeted paper: "The Machine in the
Market: Algorithmic Intermediation and Price Discovery in the Housing Market."
User pre-authorized the full pipeline including agent reviews.

## Key decisions
- **Topic selection** (after web survey): iBuyer price-discovery spillovers +
  Zillow Offers shutdown natural experiment. Rejected: data centers (taken by
  NBER WP 35194; treatment data accuracy risk), AI labor exposure (only ~2 yrs
  post-ChatGPT data). Differentiation vs BMPS (JPE R&R): they study the
  dealer's own trades; we study market-quality spillovers + the exit.
- **Theory**: two-layer model. Seller layer: ε ~ U[−w,w], movers (λ, urgency D)
  vs non-movers, machine spread m, cost c, forecast error a ~ U[−s,s].
  Closed forms: m* = w + c; Π* = λ[(D−c)² − s²/3]/4w; V* = λ(D−c)/2w.
  Buyer layer: linear forecasts from k comps + prob. ρ(v_m) = 1−(1−v_m)^k of
  a machine comp (common error a). Props: P1 sorting (operate iff w ≤ w̄),
  P2 collapse (s² tax; winner's curse Cov(V,a) = λs²/6w > 0), P3 dispersion
  strictly decreasing in machine share (exit effect ∝ pre-exposure), P4
  volume/fire-sale. Assumption 1: 0 < c < D, 2w > D−c, s < min(c, (D−c)/2).
- **Data bug found + fixed**: parquet store has per-partition type drift
  (bool-typed all-NA partitions for buyer_2/seller_2/corporate-indicator;
  double-vs-string for zip/fips/property-indicator). Fix: drop the 3 broken
  columns (entity always first-listed party; corp proxy via name regex),
  normalize mixed-type columns via as.numeric. Audit:
  explorations/schema_audit_20260610.R. All 51 states verified loading.
- **iBuyer patterns**: + SEC-verified aliases (OD Arizona/Nevada/Texas LLC).

## Pipeline state (as of this log)
- 01 national scan: DONE — 376,256 deeds (193,054 buys / 185,635 sells);
  state ranking matches institutional record (TX AZ FL GA NC lead).
- 02 panel build: RUNNING (22 states).
- 03–07: written, queued.
- Manuscript: agent drafting paper.tex + appendix.tex from verified model
  spec with [TBD-] tokens for all result numbers.
- Lit review + institutional fact pack: DONE (agents), on disk in
  quality_reports/. Bibliography_base.bib merged +40 entries (no collisions).

## Verification plan
- Fill [TBD] tokens ONLY from out_dir artifacts (key_numbers.md digest).
- Fresh-context proof-check agent on the model section.
- /verify-claims (CoVe) on institutional/citation claims.
- /review-paper --peer (editor + 2 referees) → revise → re-review.
- 3-pass XeLaTeX compile verification.

## Open questions
- 2024 deed truncation (~Aug): panel ends 2024Q2 (handled).
- RedfinNow exit Nov 2022 = possible second experiment (robustness only).

## 2026-06-11 update — estimation complete, draft finalized

- Hedonics re-run all 22 states under NA-robust spec (AZ/MD lack bedroom data
  → missing dummies; IN per-partition loader fallback). 556,334 cells.
- State-aware keys everywhere (312 cross-state ZIP5s); FE unit = state_zip.
- Final results: EXIT volume −0.0804 (0.0355)** per pp Zillow exposure, perm
  p=.030; stronger drop-liq (−0.099) and donut (−0.087); OD placebo −0.007 ns.
  Dispersion +0.0084 (0.0106) ns = BOUND; IQR ≈ 0; tail +0.005 ns. ENTRY:
  pre-trends fail except IQR (−0.0039, p=.08). Heterogeneity unresolvable.
- HONEST REFRAME of paper: liquidity channel = headline causal finding
  (several × deed footprint); information channel = bounded, and the model's
  comp-window arithmetic (ρ≈2% at observed share) predicts exactly that
  smallness → "sized, not rejected". Abstract/intro/results/interpretation/
  conclusion all rewritten to match. All 67 [TBD] tokens filled from RDS
  artifacts only.
- Validation wins: waterfall final = panel total EXACTLY (42,382,121); 2021
  purchases 67,626 vs industry ~70k; purchase residual −2.9 log pts ≈ BMPS
  −3.1pp; RedfinNow only in its true markets.
- Proof-check agent (fresh context): found 1 real error (Assumption 1 allowed
  D>2w sub-region where m*=w+c fails) → fixed with third clause
  s < 2w−(D−c) + appendix branch repair; 3 imprecisions fixed.
- Compile: 44 pages, 0 undefined citations/references, 4 figures + 6 tables.
- In flight: reproducibility-audit agent + CoVe claim-verifier agent
  (relaunched after session-limit deaths). Next: /review-paper --peer.

## 2026-06-13 update — verification, peer review, and R&R

**Verification (R1 draft):**
- Reproducibility audit: 165/165 PASS, 0 FAIL (permutation p-values recomputed
  from raw draws). Two cosmetic attribution fixes (ZIP→state-ZIP label;
  exposed-ZIP→panel median).
- CoVe: 27/27 resolved — 26 verified (Barton quote verbatim from SEC 8-K; all
  academic venues/volumes confirmed), 1 stale figure fixed ($45T→$50T).

**Peer review (QJE --peer):** editor SEND OUT; 2 referees both MAJOR REVISION,
no fatal flaws. Referee B (CREDIBILITY) caught a real bug: the volume outcome
included the machine's own deeds. Referee A (STRUCTURAL) caught the calibration
being unfalsifiable + the Prop-4 multiplier being extra-model.

**R&R — the empirics changed the story, and I followed the data:**
- Household-only volume: −10.0 log pts (was −8.0 gross), perm p .01, wild p .01.
- BUT the pre-period placebo on volume is significant (p=.01): exposed ZIPs were
  on a differential DOWNWARD volume path before the shutdown (event study: +8 to
  +11 log pts in 2019 → negative post; biggest declines in 2023). The volume
  result is CONFOUNDED by Zillow's selection into cooling Sunbelt markets.
- Vintage split uninformative: Zillow's pre-2021 exposure ≈ 0 (it scaled late),
  so almost all variation is the 2021 ramp — exactly the froth whose reversion
  cannot be ruled out.
- RedfinNow second event: null (underpowered).
- Information outcomes: CLEAN null — disp +0.84 ns, IQR −0.20 ns, raw tail +0.39
  ns; clean pre-period placebos (disp p=.56, tail p=.22). Calibration: sigma_a
  pinned from Zillow's own deed residuals (0.188); IQR ceiling 0.6% of baseline,
  tight enough to REJECT the model's most aggressive cells. Cross-firm sigma_a
  (Zillow 0.188 > Opendoor 0.174) corroborates Corollary 1.

**Honest reframe (the key editorial call):** the cleanly-IDENTIFIED result is the
tightly-bounded price-discovery NULL; the large volume decline is presented as
suggestive-but-confounded, not causal. Abstract, intro, results, interpretation,
welfare, conclusion all rewritten to lead with the null. "What this paper does
not show" paragraph foregrounds the volume identification weakness. This is the
accurate paper, per the user's "everything must be fully accurate."

**Compile:** 48 pages, 0 undefined citations/references, worst overfull 15.7pt
(wide tables wrapped in \resizebox). All R2 prose numbers trace to key_numbers.md.

**In flight:** R2 editor pass classifying each prior concern Resolved/Partial/
Not-addressed + judging null-as-contribution.
