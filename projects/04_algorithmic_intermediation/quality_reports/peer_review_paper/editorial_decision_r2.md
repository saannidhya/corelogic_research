# Editorial Decision (R2): The Machine in the Market

**Calibrated to:** Quarterly Journal of Economics (QJE) · **Round:** R2 (`--r2`) · **Date:** 2026-06-13
**Referee dispositions carried from R1:** Referee A = STRUCTURAL; Referee B = CREDIBILITY

## Decision: MINOR REVISION (conditional accept on a short, mechanical list)

Each prior concern verified against the revised `paper.tex` + `appendix.tex` and the
regenerated artifacts. Every cited number appears identically in both the manuscript and
its artifact.

## Per-concern resolution (R1 concerns A–I)

| # | R1 concern | R2 status | Evidence |
|---|---|---|---|
| A | Volume includes machine's own deeds | RESOLVED | Primary volume household-only; T3 −0.1004 (se .035) hh vs −0.0804 gross — hh *larger*, so contraction not subtraction. |
| B | No balance; froth/mean-reversion | PARTIALLY ADDRESSED | T6 balance added (balanced on appreciation .14/.09, volume growth −.09/−.12; unbalanced on stock composition). Vintage split uninformative — pre-Ketchup exposure ≈ 0 — disclosed, not faked. |
| C | Calibration undisciplined/unfalsifiable; SD-vs-IQR | RESOLVED (+ stale TA2 note, now fixed) | σ_a pinned (0.188), calibrated to IQR, one-sided ceiling 0.6% of baseline; grid rejects aggressive cells. |
| C' | SD/IQR decomposition vs SD calibration | RESOLVED | §6.2/§7.1 read information off composition-free IQR. |
| D | Prop 4(i) violated ~8×; multiplier extra-model | RESOLVED | Multiplier reframed as reduced-form amplification beyond the model's mechanical channel. |
| E | Inference (27 clusters, 200 perms) | RESOLVED | 999-draw permutation, Webb wild bootstrap, pre-period placebo. |
| F | Dose-response not shown; computed robustness unreported | RESOLVED | T7-B tercile dose-response (sig at every tercile, flat); T5 full grid both primary outcomes. |
| G | Hedonic cell selection; raw-vs-demeaned tail | RESOLVED | T7-C extensive margin +0.024 (p .02), opposite sign from null-biasing attrition; both tails in T3. |
| H | Destruction-vs-retiming; RedfinNow | PARTIALLY ADDRESSED | RedfinNow null (+0.1, se 2.8), read as underpowered; welfare respects asymmetry. Formal decay test gestured, not tabulated (no longer load-bearing). |
| I | Framing: titular question returns a bound | RESOLVED (decisive) | Paper rebuilt to lead with the clean price-discovery null; −10 volume presented as confounded, non-causal. |

**Tally:** RESOLVED 8 · PARTIALLY ADDRESSED 2 (data foreclosing the test, disclosed) · NOT ADDRESSED 0 · FATAL 0.

## Judgment: is a model-disciplined, tightly-bounded NULL a QJE contribution?

Yes — conditionally — and this revision is what makes it so. The pivotal item was the volume
pre-period placebo (perm p = 0.012): exposed ZIPs are on a differential downward path
pre-shutdown. Had the author kept selling −10 log points as a clean liquidity externality, this
would be a reject. Instead the author (1) quarantines the confound (placebo failure, late-2023
declines, uninformative vintage split, RedfinNow non-replication all in the main text and
abstract), (2) stands the paper on a clean leg (price-discovery placebos pass, disp p 0.555,
tail p 0.216; extensive-margin check positive, killing the spurious-null story), and (3) makes
the null informative (σ_a pinned; model predicts inside the ceiling for central calibrations,
above it for the aggressive cells the data reject).

What carries the paper: a clean, model-disciplined, tightly-bounded null on the titular
question; a theory that earns its keep independently (sorting visible in deeds; selective exit
by forecast quality corroborated cross-firm, Zillow σ_a 0.188 > Opendoor 0.174; the winner's
curse line by line); and the cleanest available experiment separating an executable quote from
an advisory signal. What keeps it short of a clear accept is structural: the most economically
interesting number (liquidity) is the one the setting cannot identify. That is the right trade
and should be rewarded.

## Updated recommendation: MINOR REVISION (conditional accept)

MUST (mechanical, no new analysis): (1) Fix the TA2 calibration note (was self-contradicting —
now corrected to state the ceiling lies below the aggressive cells, which the design rejects).
(2) Reconcile decimal conventions between tables and text.

SHOULD: (3) Formal decay/joint pre-trend test on the full 2024 volume path + cumulative-volume
line. (4) State preferred dispersion weighting; dollarize the ceiling, not a point.

MAY push back: (5) The author may defend the bounded-null-plus-theory package and decline to
manufacture a volume causal claim the setting cannot support — the editor agrees.
