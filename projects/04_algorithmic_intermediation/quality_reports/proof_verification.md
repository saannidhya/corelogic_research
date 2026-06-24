# Proof Verification Report — Section 3 + Appendix A

**Manuscript:** `projects/04_algorithmic_intermediation/manuscript/paper.tex` (Sec. 3) and `appendix.tex` (Appendix A)
**Method:** independent recomputation of every mathematical claim from the stated primitives — hand algebra plus numerical verification (`proof_check.R` in this folder; R 4.6.0; all output reproduced below).
**Date:** 2026-06-10

## Verdict summary

| | Count |
|---|---|
| CORRECT | 20 |
| IMPRECISE | 3 (+2 trivial nits noted in passing) |
| ERROR | 1 |

**The one error matters:** Proposition 1's claim that $m^* = w + c$ is the global optimum is **false on a sub-region of the parameter space that Assumption 1 permits**, namely $s > 2w + c - D$ (possible whenever $D > 2w$, which Assumption 1 allows since it only requires $D < 2w + c$). On that sub-region the true optimal spread is strictly greater than $w+c$ and the closed forms for $\Pi^*$, $V^*$, $\bar w$, the error tax, $V(a)$, $\Pi(a)$, $\mathrm{Cov}(V,a)$, $\bar s^2(w)$, and Corollary 1 all fail quantitatively. The fix is a one-line strengthening of Assumption 1 (see E1), after which **every** Layer-1 claim verifies exactly.

---

## Claim-by-claim table

| # | Claim (location) | Verified by | Status | Notes |
|---|---|---|---|---|
| 1 | Lemma 1: acceptance mass $(\tau_\delta+w)/2w$; $\E[\varepsilon\mid\text{acc}]=(\tau_\delta-w)/2$; pool profit $[(\delta-c)^2-(w+c-\mu)^2]/4w$ (eq. 4) | algebra + numeric (10k random draws, max err 4e-15; MC from primitives) | **CORRECT** | Per-trade profit $= \varepsilon_i + \mu - c$; difference-of-squares factorization $A+B = 2(\delta-c)$, $A-B = 2(w+c-\mu)$ checks exactly. |
| 2 | Text after Lemma 1: non-mover contribution $[c^2-(w+c-\mu)^2]/4w < 0$ whenever pool active ($\mu<w$) | algebra | **CORRECT** | Nit: the interior formula applies for $\mu \in (-w, w)$; for $\mu \le -w$ (all non-movers accept) the contribution is $\mu - c < 0$, still negative, but not by this formula. |
| 3 | Prop 1: $m^* = w + c$ is the global optimum | algebra + grid search | **ERROR** | Fails when $s > 2w+c-D$, which Assumption 1 permits. See E1. Verified exact (argmax dev ≤ 5e-15 over 28 random good-region draws) when $s \le 2w+c-D$. |
| 4 | Prop 1: at $m^*$ only movers trade, $\tau_0 = a-w-c < -w$ since $a \le s < c$ | algebra | **CORRECT** | |
| 5 | Prop 1, eq. (5): $V^* = \lambda(D-c)/(2w)$ | algebra + numeric | **CORRECT** conditional on E1 fix | Exact in good region (diff ≤ 6e-17); overstates true volume at the true optimum in the bad region (B1: claim 0.4688, truth at argmax 0.4235). |
| 6 | Prop 1, eq. (6): $\Pi^* = \lambda[(D-c)^2 - s^2/3]/(4w)$ | algebra + numeric | **CORRECT** conditional on E1 fix | Exact in good region (diff ≤ 2e-10, integration error); in bad region overstates the true max (B1: claim 0.34688 vs truth 0.34243). |
| 7 | Prop 1: $V^*, \Pi^*$ strictly ↓ in $w$; $m^*$ strictly ↑ in $w$; cutoff $\bar w = \lambda[(D-c)^2-s^2/3]/(4F)$ (eq. 7) | algebra | **CORRECT** given the formulas | $\Pi^*$ numerator $>0$ since $s^2 < (D-c)^2/4$. $\bar w$ is exactly $\Pi^* \ge F$ rearranged. |
| 8 | Appendix Step 1, branches (a)/(b), continuity at $\mu = w$, deterministic peak $\mu = w+c$ | algebra | **CORRECT** | Both branch values at $\mu=w$ equal $\lambda[(D-c)^2-c^2]/4w$ — checks. Nits: profit is flat at 0 (not "strictly decreasing") for $\mu > D+w$; region $\mu < \min(w, D-w)$ not explicitly covered in Step 1 (it is increasing there). Harmless for the optimum. |
| 9 | Appendix Step (c): quadratic branch "applies state by state" on $m \in [w+s, D+w-s]$, giving $\E_a\Pi(m) = \lambda[(D-c)^2-(w+c-m)^2-s^2/3]/4w$ | algebra + numeric | **ERROR** (proof hole; same root cause as #3) | True only if $m - s \ge D - w$. At $m = w+c$ this is $s \le 2w+c-D$, not implied by Assumption 1. See E1. |
| 10 | Appendix Step (d), lower corner: non-mover contributions never positive; case $m < 2c-D-w+s$ nonpositive profit; $h(\mu)$ concave with roots $2c-D-w$, $D-w$, $h \ge 0$ between; strict final inequality via $(c-s)^2 > 0$ | algebra + numeric (5k random root/sign checks, max err 1.4e-14, no sign violations) | **CORRECT** | Both roots verified symbolically: $h(D-w)=0$, $h(2c-D-w)=0$. The bound chain is valid. |
| 11 | Appendix Step (e), upper corner: $\mu > D+w-2s \ge w+c$; profit $\lambda\max\{0,\cdot\}$; decreasing in $m$; $(D-c-s)^2 \ge s^2/3$ from $D-c \ge 2s \ge s(1+1/\sqrt3)$ | algebra + numeric (1e5 draws, min margin 1.3e-4 ≥ 0; boundary case $D-c=2s$ gives $\tfrac23 s^2>0$) | **CORRECT** | $1 + 1/\sqrt3 \approx 1.577 < 2$ — inequality chain valid. |
| 12 | Prop 2(i), eq. (9): $\E_a\Pi(m^*) = \Pi(m^*;a{=}0) - \lambda s^2/(12w)$; "tax" $= \lambda\sigma_a^2/(4w)$ | algebra + numeric (match to 1e-10) | **CORRECT** conditional on E1 fix | $\lambda\sigma_a^2/4w = \lambda s^2/12w$ — internally consistent. |
| 13 | Prop 2(i), eq. (10): operate iff $s^2 \le \bar s^2(w) = 3[(D-c)^2 - 4wF/\lambda]$ | algebra | **CORRECT** conditional on E1 fix | Exact rearrangement of $\Pi^* \ge F$; consistent with $\bar w$. Caveat: the "regime shift raises $s$ above $\bar s$" comparative static implicitly requires the raised $s$ to stay within Assumption 1's range for the formula to apply (the direction is intuitive beyond it, but not established). |
| 14 | Prop 2(ii), eqs. (11)–(12): $V(a) = \lambda(D-c+a)/2w$; margin $(D-c-a)/2$; $\mathrm{Cov}(V,a) = \lambda s^2/(6w)$; $\Pi(a) = \lambda[(D-c)^2-a^2]/4w$ minimized at $|a|=s$ | algebra + numeric (11-point $a$-grid exact; Cov by 2e5-point integration = 0.0033333333 vs claim 0.0033333333) | **CORRECT** conditional on E1 fix | Margin algebra re-derived: $w - a + (\tau_D - w)/2 = (D-c-a)/2$ ✓. Note $\Pi(a)$ is equally minimal at $a = -s$ (lowest-volume state); the text's "lowest in its highest-volume states" is true but the minimum is not unique. |
| 15 | Prop 2(iii): $\partial\bar s^2/\partial w = -12F/\lambda < 0$ | algebra | **CORRECT** | |
| 16 | Corollary 1: shock $\kappa$ removes exactly firms with $s_j > \bar s/\kappa$ | algebra | **IMPRECISE** | See I3. Appendix interval $(\bar s/\kappa, \bar s]$ is right; main-text phrasing sweeps in firms that never operated, and both versions implicitly require $\kappa s_j$ to remain within Assumption 1 (+E1 fix) bounds. |
| 17 | Prop 3, eq. (12): $V_0 = \sigma_\xi^2/k$, $V_1 = (k/\sigma_\xi^2 + 1/\sigma_a^2)^{-1}$ | algebra + MC | **CORRECT** | Diffuse-prior GLS. |
| 18 | Identities (A.1): $V_0^2 k/\sigma_\xi^2 = V_0$; $V_1^2 k/\sigma_\xi^2 = V_1(1 - V_1/\sigma_a^2)$ | algebra + numeric (10k random configs, max err 6e-16) | **CORRECT** | |
| 19 | Error decomposition $\hat v_1 - v = V_1(k/\sigma_\xi^2)\bar\xi + (V_1/\sigma_a^2)a$; gap $(V_1/\sigma_a^2)a$; within-var $V_1^2k/\sigma_\xi^2$; unconditional $\Var = V_1$ | algebra + MC | **CORRECT** | |
| 20 | Prop 3, eq. (13): $W_B(\rho) = \rho V_1^2k/\sigma_\xi^2 + (1-\rho)V_0 + \rho(1-\rho)V_1^2/\sigma_a^2$ | full MC (3 configs incl. uniform $a$ with $\sigma_a^2 = s^2/3$; rel. err ≤ 7e-4 at 10M draws/point) | **CORRECT** | $W_B$ = $\E_a$[cross-sectional variance of buyer errors given common $a$] — the mixture-variance derivation checks, incl. $\E_a[\text{gap}^2] = V_1^2/\sigma_a^2$. Distribution-free in $a$ (only 2nd moments enter), so U$[-s,s]$ vs normal is immaterial — verified both. |
| 21 | Quadratic form $W_B = V_0 + \rho(V_1 - V_0) - \rho^2V_1^2/\sigma_a^2$; $dW_B/d\rho$ linear, $= V_1 - V_0 < 0$ at $\rho=0$, $= V_1 - V_0 - 2V_1^2/\sigma_a^2 < 0$ at $\rho=1$; strict decrease on $[0,1]$; decreasing in $v_m$ via $\rho' = k(1-v_m)^{k-1} > 0$ | algebra + numeric (0 violations / 5000 random configs × 401-point $\rho$ grids) | **CORRECT** | Equivalent form: $W_B = \rho V_1 + (1-\rho)V_0 - \rho^2V_1^2/\sigma_a^2$. |
| 22 | Prop 3(ii): $\Delta W_B = \rho^{pre}[(V_0 - V_1) + \rho^{pre}V_1^2/\sigma_a^2] > 0$, strictly ↑ in $\rho^{pre}$ | algebra + numeric (0 violations / 5000) | **CORRECT** | Matches the text's "$\rho(v_m^{pre})(V_0 - V_1 + \text{cross-terms})$". |
| 23 | Information structure: $\rho(v_m) = 1-(1-v_m)^k$ = "probability at least one of $k$ comps is a machine trade" vs. projection with $k$ household comps **plus** the machine signal | consistency check | **IMPRECISE** | See I2. The two descriptions are not the same information set. Nit: $\rho$ "strictly increasing and concave" is only weakly concave at $k=1$. |
| 24 | Prop 4(i): $V_{0,total} = \lambda + (1-\lambda)\nu$; total with machine $= V_{0,total} + \lambda P(\varepsilon \le \tau_D)$, one-for-one in purchases | algebra + simulation (4M households × 3 values of $a$; match to MC error ~1e-4) | **CORRECT** | $2P + (1-P) = 1 + P$ arithmetic verified; non-movers never sell to machine at $m^*$ (checked $\tau_0 < -w$ in sim). |
| 25 | Prop 4(ii): with $b=0$, $>t$-below-fundamental trades $=$ mover sales for $t \le \theta D$; fire-sale share falls per eq. (15) | algebra + simulation | **IMPRECISE** | See I1: the statement needs $\hat v_i = v$ as well as $b=0$; the appendix proof quietly adds it ("benchmark with $\hat v_i = v$"). The share comparison itself is correct: $x/(x+K)$ strictly increasing in $x$, $P(\varepsilon > \tau_D) < 1$. Simulation matches both shares to ~1e-4. |
| 26 | Notation/consistency: $\mu = m - a$; $\tau_\delta = \delta - \mu$; $\sigma_a^2 = s^2/3$ used consistently across layers; eqs. (9)/(10) vs (6)/(7) mutually consistent; every text claim has an appendix proof | line-by-line audit | **CORRECT** | De-spreading logic is sound: the machine's deed price is the quote $q = v + a - m^*$ regardless of the accepted $\varepsilon_i$ (firm quote), so $q + m^* = v + a$ carries no selection bias — internally consistent. Footnote 1's claim that endogenizing $\sigma_\xi^2$ "amplifies … without changing any sign" is asserted, not proven (acknowledged as outside the one-step problem). |

---

## ERROR

### E1. Proposition 1 ($m^* = w+c$) fails on an Assumption-1-feasible sub-region; proof hole in Appendix Step (c)

**The problem.** Step (c) claims that for $m \in [w+s,\, D+w-s]$ "the movers-only branch applies state by state," and computes $\E_a\Pi(m)$ from the interior (quadratic) Lemma-1 expression. But the quadratic expression for the mover pool requires the threshold to be interior, $\tau_D = D - \mu \le w$, i.e. $\mu \ge D - w$. When $D > 2w$ (which Assumption 1 permits — it only imposes $D < 2w + c$), realizations with $\mu = m - a \in [w, D-w)$ have **all movers accepting** and deterministic profit $\lambda(\mu - c)$, which is *strictly below* the quadratic expression (the appendix's own $h(\mu) \ge 0$ inequality, Step (d)). At $m = w+c$ such realizations occur with positive probability iff $s > 2w + c - D$.

**Why the optimum actually moves.** The deterministic profit's slope on the all-accept region is $\lambda$, strictly larger than the quadratic branch's slope at the junction ($\lambda(2w+c-D)/2w < \lambda$). Averaging over the window $[w+c-s,\, w+c+s]$, the first-order condition at $m = w+c$ evaluates to
$$\frac{d\,\E_a\Pi}{dm}\Big|_{m = w+c} = \lambda\,(s - Y)\Big[1 - \frac{s+Y}{4w}\Big] > 0, \qquad Y \equiv 2w + c - D \in (0, s),$$
so expected profit is strictly increasing at $w+c$: the claimed optimum is not a local maximum, let alone global. Intuitively, when the AVM can overshoot far enough that the entire mover pool accepts, the firm widens the spread beyond $w+c$ to insure against those all-accept states.

**Numerical confirmation** (Assumption 1 verified for each draw; full output in `proof_check.R` log):

| Draw | $(\lambda, D, c, w, s)$ | $2w{+}c{-}D$ | true argmax $m$ | claimed $w{+}c$ | true max $\E\Pi$ | claimed $\Pi^*$ |
|---|---|---|---|---|---|---|
| B1 | (0.5, 2, 0.5, 0.8, 0.3) | 0.10 | **1.4421** | 1.300 | 0.342427 | 0.346875 |
| B2 | (0.5, 0.8, 0.2, 0.35, 0.15) | 0.10 | **0.5832** | 0.550 | 0.125299 | 0.125893 |
| B3 | (0.4, 1.5, 0.3, 0.62, 0.2) | 0.04 | **1.0395** | 0.920 | 0.227020 | 0.230108 |
| G5 | (0.15, 3, 0.5, 1.3, 0.24) | 0.10 | **1.9174** | 1.800 | 0.179261 | 0.179735 |

In B1, the claimed $\Pi^*$ overstates the true maximized profit by 1.3% and overstates $\E\Pi(w+c)$ by 4.5%; claimed $V^*$ (0.46875) overstates volume at the true optimum (0.42348) by 10.7%. Everything downstream of eq. (5) — eqs. (5)–(11), (13)–(14) [error tax, $\bar s^2$, $V(a)$, $\Pi(a)$, $\mathrm{Cov}(V,a)$], $\bar w$, and Corollary 1 — is quantitatively wrong on this sub-region.

**Conversely**, on the complementary region $s \le 2w + c - D$: over 5 designed draws plus 28 random Assumption-1 draws, the numeric argmax equals $w + c$ to ≤ 5e-15 and $\Pi^*$, $V^*$, $V(a)$, $\Pi(a)$, $\mathrm{Cov}(V,a)$, and the error-tax identity hold to integration precision. **The theorem and all downstream formulas are exactly right once the parameter region is restricted.**

**Minimal fix (one line).** Strengthen Assumption 1's third condition to
$$0 < s < \min\{c,\ (D-c)/2,\ 2w - (D - c)\},$$
(equivalently: replace the second condition with $2w \ge D - c + s$; or, simpler but stronger, add $D \le 2w$, under which $2w + c - D \ge c > s$ holds automatically). Then in Step (c) note that $m - s \ge w + s - s \ge D - w$ for all $m$ in the interior range — wait, the clean statement: at any $m \ge w+s$, $\mu \ge m - s \ge w \ge D - w$ when $D \le 2w$, or $\mu \ge w$ plus $s \le 2w+c-D$ guarantees $\mu \ge D - w$ on the relevant window at $m^*$; one sentence justifying "state by state" closes the hole. No other change to the proof is needed — Steps (d) and (e) are valid as written.

---

## IMPRECISE

### I1. Proposition 4(ii): statement conditions on $b = 0$ only, proof also requires $\hat v_i = v$

The proposition asserts that with $b = 0$, household trades more than $t$ below fundamental ($t \le \theta D$) "are exactly mover sales." With buyer estimation error active, the price is $p_i = \hat v_i + \varepsilon_i - \theta D\,\ind{\text{mover}} + b_i$ (eq. 14), so a *non-mover* sale with $\hat v_i - v < -t$ also executes more than $t$ below fundamental — the equivalence fails. The appendix proof silently adds the missing condition ("consider the benchmark with $\hat v_i = v$"), so the proof proves a different (weaker) statement than the proposition makes.
**Fix:** in the proposition statement, replace "With $b = 0$," with "With $b = 0$ and abstracting from buyer estimation error ($\hat v_i = v$),".

### I2. Layer 2: $\rho$'s comp-window story does not match the projection's information set

The text defines $\rho(v_m) = 1 - (1-v_m)^k$ as "the probability that at least one of $k$ comps is a machine trade," but the projection underlying $V_1$ (eq. 12 and the appendix) gives the informed buyer $k$ household comps **plus** the machine signal — i.e., the machine deed does not occupy a comp slot. If it did, the informed buyer would have $k-1$ household comps plus the machine signal, $V_1' = ((k-1)/\sigma_\xi^2 + 1/\sigma_a^2)^{-1}$, and $V_1' < V_0$ holds iff $\sigma_a^2 < \sigma_\xi^2$ — so the headline monotonicity ($W_B$ decreasing in $\rho$) could *fail* under the displacement reading when the machine's de-spread signal is noisier than a single household comp. As written, the math is correct for the "$k$ household comps + add-on signal" structure; only the verbal motivation conflicts.
**Fix:** restate as "every buyer observes $k$ household comps; with probability $\rho(v_m)$ the buyer's window additionally contains a machine deed," keeping $1-(1-v_m)^k$ as the (now slightly heuristic) functional form — or rederive with displacement and add the condition $\sigma_a^2 < \sigma_\xi^2$.

### I3. Corollary 1: wording and implicit domain

(a) Main text: "removes exactly the firms with $s_j > \bar s/\kappa$" — firms with $s_j > \bar s$ were not operating, so they cannot be "removed." The appendix's interval $(\bar s/\kappa, \bar s]$ is the correct statement; copy it into the text. (b) Both versions implicitly assume the post-shock errors $\kappa s_j$ still satisfy Assumption 1's bounds (and the E1-fixed bound), since the exit threshold is derived from the closed form $\Pi^*$. Beyond that range the conclusion is intuitive (more error cannot raise profit) but unproven.
**Fix:** "…removes exactly the previously active firms with $s_j \in (\bar s/\kappa, \bar s]$, provided $\kappa s_j$ continues to satisfy Assumption 1."

### Minor nits (no table rows)

- Appendix Step 1 summary: deterministic profit is **flat at zero** for $\mu > D+w$, not "strictly decreasing beyond" $w+c$; and the region $\mu < D - w$ (relevant when $D < 2w$… more precisely $\mu < \min(w, D-w)$ plus $\mu \le -w$) is not explicitly analyzed in Step 1 (profit is increasing there; one sentence would close it). Neither affects the optimum.
- Text after Lemma 1: "negative whenever the non-mover pool is active ($\mu < w$)" — the quoted formula applies on $\mu \in (-w, w)$; for $\mu \le -w$ the contribution is $\mu - c < 0$ (still negative, different expression).
- $\rho(v_m)$ is only weakly concave at $k = 1$.
- Prop 2(ii): $\Pi(a)$ is minimized at **both** $a = \pm s$; the highest-volume state ($a = s$) is *a* lowest-profit state, tied with the lowest-volume state ($a = -s$). The claim as worded is true; a clause acknowledging the tie would be cleaner.

---

## Numerical checks: parameters and results

All from `proof_check.R` (same folder), R 4.6.0, fixed seeds (42/7/11/12/13/99/14/15/123). Integration: trapezoid over $a$-grids up to 200,001 points; grid-search over $m$ (2,500–26,000 points) refined by `optimize()` to tol 1e-10–1e-12.

**Lemma 1.** Identity checked at 10,000 random $(w, c, \delta, \tau)$: max abs deviation 4.0e-15. Monte Carlo from primitives ($w{=}0.9, c{=}0.5, \delta{=}1.3, \tau{=}0.4$, 4M draws): pool profit 0.108264 vs formula 0.108333; mass 0.721985 vs 0.722222; $\E[\varepsilon\mid\text{acc}]$ −0.250046 vs −0.25. ✓

**Proposition 1 — good region** ($s \le 2w+c-D$). Draws G1 (0.3, 1, 0.4, 0.5, 0.2), G2 (0.7, 2, 1.2, 1.0, 0.35), G3 (0.5, 1, 0.5, 1.0, 0.2), G4 (0.9, 0.6, 0.1, 0.3, 0.09): argmax $= w+c$ to ≤ 3e-13; $\E\Pi(w{+}c)$ matches $\Pi^*$ to ≤ 2e-10; $\E V(w{+}c)$ matches $V^*$ to ≤ 6e-17. Random audit: 30 Assumption-1 draws → 28 good-region, max |argmax − (w+c)| = 5.3e-15; the 2 bad-region draws deviated by +0.060 and +0.198. ✓ / ✗ as predicted by the $s \lessgtr 2w+c-D$ classification, with zero exceptions.

**Proposition 1 — bad region.** Table in E1 above (B1–B3, G5). ✗

**Proposition 2** (at $\lambda{=}0.5, D{=}1, c{=}0.5, w{=}1, s{=}0.2$; good region): $V(a)$ and $\Pi(a)$ match claimed formulas exactly on an 11-point $a$-grid; $\mathrm{Cov}(V,a)$ numeric 0.0033333333 = $\lambda s^2/6w$; error-tax identity $\E\Pi(m^*) = \Pi(a{=}0) - \lambda s^2/12w$: 0.02958333 both sides. ✓

**Corner inequalities.** $h(\mu)$ roots at $2c-D-w$ and $D-w$ verified to 1.4e-14 over 5,000 random draws with no interior sign violations; $(D-c-s)^2 \ge s^2/3$ under $D-c \ge 2s$: min margin 1.3e-4 over 1e5 draws, boundary value $\tfrac23 s^2 > 0$. ✓

**Proposition 3.** Identities (A.1), the quadratic form for $W_B$, and endpoint-derivative signs: max error 5.6e-16 over 10,000 random $(k, \sigma_\xi^2, \sigma_a^2, \rho)$. Full Monte Carlo of $W_B$ (20,000 segments × 500 buyers per $\rho$-point; informed buyers share one segment-level $a$):
- Config A ($k{=}8, \sigma_\xi{=}1, \sigma_a{=}0.6$, normal $a$): sim vs formula rel. err ≤ 1.4e-4 across $\rho \in \{0,.2,.4,.6,.8,1\}$, e.g. $\rho{=}0.4$: 0.108303 vs 0.108287.
- Config B ($k{=}4, \sigma_\xi{=}1, \sigma_a{=}1.2$ — machine signal noisier than $V_0$): rel. err ≤ 6.6e-4; $W_B$ still strictly decreasing, as the formula requires.
- Config C ($a \sim U[-0.9, 0.9]$, $\sigma_a^2 = s^2/3 = 0.27$, $k{=}8$): rel. err ≤ 6.2e-4 — confirms distribution-independence.
Monotone decrease of $W_B$ on $[0,1]$: 0 violations in 5,000 random configs × 401-point grids. $\Delta W_B > 0$, increasing in $\rho^{pre}$, equal to the closed form: 0 violations in 5,000 configs. ✓

**Proposition 4** ($\lambda{=}0.4, D{=}1, c{=}0.4, w{=}0.8, s{=}0.15, \nu{=}0.1, \theta{=}0.6$, $m^* = 1.2$; 4M households; $a \in \{-0.15, 0, 0.105\}$): total recorded volume equals $V_{0,total} + \lambda P(\varepsilon \le \tau_D)$ to MC error (e.g. $a{=}0$: 0.609946 sim vs 0.609946 identity); machine purchases match $\lambda(\tau_D + w)/2w$ (0.149929 vs 0.150); fire-sale shares match eq. (15) (no machine: 0.869195 vs 0.869565; with machine: 0.805950 vs 0.806452). Non-mover threshold $\tau_0 < -w$ confirmed in every state. ✓

---

## Bottom line

The model's architecture is sound and the algebra is almost everywhere exactly right — Lemma 1, the corner analysis, the entire Layer-2 projection apparatus, and Proposition 4's counting all survive hostile recomputation. The single substantive defect is the unstated regularity condition $s \le 2w + c - D$: Assumption 1 as drafted admits parameters under which Proposition 1's optimal spread, and every closed form built on it, is wrong. One added inequality in Assumption 1 (plus one justifying sentence in Step (c)) repairs the entire Layer-1 edifice; the three IMPRECISE items are statement-level wording fixes that do not require new mathematics.
