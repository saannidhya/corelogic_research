# proof_check.R -- independent numerical verification of paper.tex Section 3 + appendix.tex Appendix A
# Referee recomputation: do NOT trust the text; recompute from primitives.
# Run: Rscript proof_check.R
options(digits = 10)
set.seed(42)

cat("==========================================================\n")
cat("PART 0: Lemma 1 pool-profit identity (random mu, delta)\n")
cat("==========================================================\n")
# Pool with urgency delta, eps ~ U[-w,w], threshold tau = delta - mu interior.
# Contribution = mass * E[per-trade profit | accept]
#              = ((tau+w)/(2w)) * (mu - c + (tau-w)/2)
# Claim: equals ((delta-c)^2 - (w+c-mu)^2) / (4w)
lem1_err <- replicate(10000, {
  w <- runif(1, 0.1, 3); c <- runif(1, 0.01, 2); delta <- runif(1, 0, 3)
  # interior tau in [-w, w]: mu = delta - tau
  tau <- runif(1, -w, w); mu <- delta - tau
  lhs <- ((tau + w)/(2*w)) * (mu - c + (tau - w)/2)
  rhs <- ((delta - c)^2 - (w + c - mu)^2) / (4*w)
  abs(lhs - rhs)
})
cat(sprintf("Lemma 1 identity: max abs error over 10000 random draws = %.3e\n", max(lem1_err)))

# Also Monte-Carlo from the primitive: eps ~ U[-w,w]
w <- 0.9; c <- 0.5; delta <- 1.3; tau <- 0.4; mu <- delta - tau
eps <- runif(4e6, -w, w)
acc <- eps <= tau
mc <- mean(acc * (mu - c + eps))     # mass-weighted mean profit per house in pool
cl <- ((delta - c)^2 - (w + c - mu)^2)/(4*w)
cat(sprintf("Lemma 1 MC check (w=.9,c=.5,delta=1.3,tau=.4): MC=%.6f formula=%.6f\n", mc, cl))
cat(sprintf("  mass: MC=%.6f formula=%.6f | E[eps|acc]: MC=%.6f formula=%.6f\n",
            mean(acc), (tau+w)/(2*w), mean(eps[acc]), (tau-w)/2))

cat("\n==========================================================\n")
cat("PART 1: Layer-1 grid search over spread m (Prop 1, Prop 2)\n")
cat("==========================================================\n")
# Deterministic profit at realized spread mu = m - a, handling ALL branches
# (pool fully accepting: mass 1, E[eps]=0; inactive: 0; interior: Lemma 1).
pool_contrib <- function(mu, delta, c, w) {
  tau <- delta - mu
  ifelse(tau >= w, mu - c,
  ifelse(tau <= -w, 0,
         ((tau + w)/(2*w)) * (mu - c + (tau - w)/2)))
}
Pi_det <- function(mu, lam, D, c, w)
  lam * pool_contrib(mu, D, c, w) + (1 - lam) * pool_contrib(mu, 0, c, w)

trap <- function(vals, x) sum((vals[-1] + vals[-length(vals)])/2 * diff(x))

EPi <- function(m, lam, D, c, w, s, n = 4001) {
  a <- seq(-s, s, length.out = n)
  trap(Pi_det(m - a, lam, D, c, w), a) / (2*s)
}
mass_D <- function(mu, D, w) pmin(1, pmax(0, ((D - mu) + w)/(2*w)))
EVol <- function(m, lam, D, c, w, s, n = 4001) {
  a <- seq(-s, s, length.out = n)
  trap(lam * mass_D(m - a, D, w), a) / (2*s)
}

check_draw <- function(lam, D, c, w, s, label) {
  stopifnot(c > 0, c < D, 2*w > D - c, s > 0, s < min(c, (D - c)/2))
  good <- (s <= 2*w + c - D)   # condition for quadratic branch state-by-state at m* = w+c
  mgrid <- seq(1e-3, D + w + s + 0.2, length.out = 4000)
  ep <- vapply(mgrid, EPi, 0, lam = lam, D = D, c = c, w = w, s = s, n = 1501)
  m0 <- mgrid[which.max(ep)]
  opt <- optimize(function(m) EPi(m, lam, D, c, w, s, n = 8001),
                  lower = max(1e-3, m0 - 0.05), upper = m0 + 0.05,
                  maximum = TRUE, tol = 1e-10)
  mstar_claim <- w + c
  Pi_claim <- lam * ((D - c)^2 - s^2/3)/(4*w)
  V_claim  <- lam * (D - c)/(2*w)
  epi_at_claim <- EPi(mstar_claim, lam, D, c, w, s, n = 8001)
  cat(sprintf("\n[%s] lam=%.3f D=%.3f c=%.3f w=%.3f s=%.3f  2w+c-D=%.3f  region=%s\n",
              label, lam, D, c, w, s, 2*w + c - D,
              ifelse(good, "GOOD (s <= 2w+c-D)", "BAD  (s >  2w+c-D)")))
  cat(sprintf("  argmax m    : numeric=%.6f  claimed w+c=%.6f  diff=%+.3e\n",
              opt$maximum, mstar_claim, opt$maximum - mstar_claim))
  cat(sprintf("  max E Pi    : numeric=%.8f  claimed Pi*=%.8f  diff=%+.3e\n",
              opt$objective, Pi_claim, opt$objective - Pi_claim))
  cat(sprintf("  E Pi(w+c)   : numeric=%.8f  claimed Pi*=%.8f  diff=%+.3e\n",
              epi_at_claim, Pi_claim, epi_at_claim - Pi_claim))
  cat(sprintf("  E Vol(w+c)  : numeric=%.8f  claimed V*=%.8f  diff=%+.3e\n",
              EVol(mstar_claim, lam, D, c, w, s, n = 8001), V_claim,
              EVol(mstar_claim, lam, D, c, w, s, n = 8001) - V_claim))
  invisible(c(mhat = opt$maximum, mclaim = mstar_claim, good = good))
}

# --- Good-region draws (Assumption 1 AND s <= 2w+c-D) ---
check_draw(0.30, 1.00, 0.40, 0.50, 0.20, "G1")
check_draw(0.70, 2.00, 1.20, 1.00, 0.35, "G2")
check_draw(0.50, 1.00, 0.50, 1.00, 0.20, "G3")
check_draw(0.90, 0.60, 0.10, 0.30, 0.09, "G4")
check_draw(0.15, 3.00, 0.50, 1.30, 0.24, "G5")

# --- Bad-region draws (Assumption 1 holds, but s > 2w+c-D) ---
check_draw(0.50, 2.00, 0.50, 0.80, 0.30, "B1")
check_draw(0.50, 0.80, 0.20, 0.35, 0.15, "B2")
check_draw(0.40, 1.50, 0.30, 0.62, 0.20, "B3")

# --- Random draws satisfying Assumption 1; classify and test ---
cat("\n--- 30 random Assumption-1 draws: |argmax - (w+c)| by region ---\n")
set.seed(7)
res <- t(replicate(30, {
  D <- runif(1, 0.5, 3); c <- runif(1, 0.05, 0.95) * D
  w <- (D - c)/2 + runif(1, 0.005, 1.2)            # ensures 2w > D-c
  s <- runif(1, 0.2, 0.99) * min(c, (D - c)/2)
  good <- (s <= 2*w + c - D)
  lam <- runif(1, 0.1, 0.9)
  mgrid <- seq(1e-3, D + w + s + 0.2, length.out = 2500)
  ep <- vapply(mgrid, EPi, 0, lam = lam, D = D, c = c, w = w, s = s, n = 801)
  m0 <- mgrid[which.max(ep)]
  opt <- optimize(function(m) EPi(m, lam, D, c, w, s, n = 4001),
                  lower = max(1e-3, m0 - 0.06), upper = m0 + 0.06,
                  maximum = TRUE, tol = 1e-10)
  c(good = good, dev = opt$maximum - (w + c))
}))
cat(sprintf("GOOD draws: n=%d, max |argmax-(w+c)| = %.3e\n",
            sum(res[, "good"] == 1), max(abs(res[res[, "good"] == 1, "dev"]))))
if (any(res[, "good"] == 0)) {
  cat(sprintf("BAD  draws: n=%d, deviations argmax-(w+c): ", sum(res[, "good"] == 0)))
  cat(sprintf("%+.4f ", res[res[, "good"] == 0, "dev"]), "\n")
}

cat("\n==========================================================\n")
cat("PART 2: Conditional volume / profit / Cov(V,a)  (Prop 2)\n")
cat("==========================================================\n")
# Good-region parameters
lam <- 0.5; D <- 1; c <- 0.5; w <- 1; s <- 0.2
mst <- w + c
agrid <- seq(-s, s, length.out = 11)
cat("a      V(a) numeric    V(a) claim     Pi(a) numeric   Pi(a) claim\n")
for (a in agrid) {
  Vn <- lam * mass_D(mst - a, D, w); Vc <- lam * (D - c + a)/(2*w)
  Pn <- Pi_det(mst - a, lam, D, c, w); Pc <- lam * ((D - c)^2 - a^2)/(4*w)
  cat(sprintf("%+.3f  %.8f   %.8f   %.8f   %.8f\n", a, Vn, Vc, Pn, Pc))
}
# Cov(V,a) by integration
n <- 200001; a <- seq(-s, s, length.out = n)
V <- lam * mass_D(mst - a, D, w)
EV <- trap(V, a)/(2*s); EVa <- trap(V * a, a)/(2*s)
cat(sprintf("\nCov(V,a): numeric=%.10f  claimed lam*s^2/(6w)=%.10f\n",
            EVa - EV * 0, lam * s^2/(6*w)))   # E[a]=0 so Cov = E[Va]
cat(sprintf("  (E[V]=%.8f vs claim %.8f; E[a]=%.2e)\n", EV, lam*(D-c)/(2*w), trap(a, a)/(2*s)))
# error-tax decomposition: E Pi(m*) = Pi(m*; a=0) - lam s^2/(12 w)
cat(sprintf("Error tax: E Pi(m*)=%.8f ; Pi(a=0) - lam s^2/(12w)=%.8f\n",
            EPi(mst, lam, D, c, w, s, n = 8001),
            Pi_det(mst, lam, D, c, w) - lam * s^2/(12*w)))

cat("\n==========================================================\n")
cat("PART 3: Appendix corner-case inequalities (Step d, e)\n")
cat("==========================================================\n")
# h(mu) = (D-c)^2 - (w+c-mu)^2 - 4w(mu-c): roots at 2c-D-w and D-w, >=0 between
set.seed(11)
hmax_err <- replicate(5000, {
  D <- runif(1, 0.5, 3); c <- runif(1, 0.05, 0.95)*D; w <- runif(1, 0.05, 2)
  r1 <- 2*c - D - w; r2 <- D - w
  h <- function(mu) (D - c)^2 - (w + c - mu)^2 - 4*w*(mu - c)
  e1 <- abs(h(r1)); e2 <- abs(h(r2))
  mid <- runif(1, min(r1, r2), max(r1, r2))
  c(e1, e2, -min(0, h(mid)))   # h(mid) should be >= 0
})
cat(sprintf("h root errors max=%.2e ; min h(interior) violation max=%.2e\n",
            max(hmax_err[1:2, ]), max(hmax_err[3, ])))
# (D-c-s)^2 >= s^2/3 given D-c >= 2s
set.seed(12)
viol <- replicate(100000, {
  s <- runif(1, 0.001, 2); Dc <- 2*s + runif(1, 0, 3)   # D-c >= 2s
  (Dc - s)^2 - s^2/3
})
cat(sprintf("(D-c-s)^2 - s^2/3 with D-c>=2s: min over 1e5 draws = %.6f (>=0 required)\n", min(viol)))
# worst case at D-c = 2s exactly: (s)^2 - s^2/3 = (2/3)s^2 > 0
cat(sprintf("boundary D-c=2s, s=1: value = %.6f\n", (2*1 - 1)^2 - 1/3))

cat("\n==========================================================\n")
cat("PART 4: Layer 2 -- projection MSEs, identities, W_B (Prop 3)\n")
cat("==========================================================\n")
# Identities A.1 over random parameters
set.seed(13)
id_err <- replicate(10000, {
  k <- sample(1:20, 1); sx2 <- runif(1, 0.05, 4); sa2 <- runif(1, 0.05, 4)
  V0 <- sx2/k; V1 <- 1/(k/sx2 + 1/sa2)
  e1 <- abs(V0^2 * k/sx2 - V0)
  e2 <- abs(V1^2 * k/sx2 - V1 * (1 - V1/sa2))
  # quadratic form: W_B(rho) = V0 + rho(V1-V0) - rho^2 V1^2/sa2
  rho <- runif(1)
  WB1 <- rho * V1^2 * k/sx2 + (1 - rho) * V0 + rho * (1 - rho) * V1^2/sa2
  WB2 <- V0 + rho * (V1 - V0) - rho^2 * V1^2/sa2
  e3 <- abs(WB1 - WB2)
  # monotonicity: dWB/drho < 0 at both endpoints
  d0 <- (V1 - V0); d1 <- (V1 - V0) - 2 * V1^2/sa2
  e4 <- max(0, d0, d1)  # should be < 0 -> e4 = 0
  max(e1, e2, e3, e4)
})
cat(sprintf("Identities A.1 + quadratic form + endpoint signs: max err = %.3e\n", max(id_err)))

# Full Monte Carlo of the buyer-error dispersion W_B
mc_WB <- function(k, sx, sa, rho, M = 20000, B = 500, dist_a = "normal", s_unif = NULL) {
  a_seg <- if (dist_a == "normal") rnorm(M, 0, sa) else runif(M, -s_unif, s_unif)
  V0 <- sx^2/k; V1 <- 1/(k/sx^2 + 1/sa^2)
  g  <- rep(1:M, each = B)
  xibar <- rnorm(M * B, 0, sx/sqrt(k))
  inf   <- runif(M * B) < rho
  a_all <- a_seg[g]
  e <- ifelse(inf, V1 * (k/sx^2) * xibar + (V1/sa^2) * a_all, xibar)
  s1 <- rowsum(e, g); s2 <- rowsum(e^2, g)
  vg <- (s2 - s1^2/B)/(B - 1)
  mean(vg)
}
WB_formula <- function(k, sx, sa, rho) {
  V0 <- sx^2/k; V1 <- 1/(k/sx^2 + 1/sa^2)
  rho * V1^2 * k/sx^2 + (1 - rho) * V0 + rho * (1 - rho) * V1^2/sa^2
}
set.seed(99)
cat("\nConfig A: k=8, sigma_xi=1, sigma_a=0.6 (a ~ Normal)\n")
cat("rho    W_B sim        W_B formula    rel.err\n")
for (rho in c(0, 0.2, 0.4, 0.6, 0.8, 1.0)) {
  sim <- mc_WB(8, 1, 0.6, rho); f <- WB_formula(8, 1, 0.6, rho)
  cat(sprintf("%.1f    %.6f     %.6f     %+.2e\n", rho, sim, f, (sim - f)/f))
}
cat("\nConfig B: k=4, sigma_xi=1, sigma_a=1.2 (machine signal noisier than V0? V0=0.25, sa2=1.44)\n")
for (rho in c(0, 0.25, 0.5, 0.75, 1.0)) {
  sim <- mc_WB(4, 1, 1.2, rho); f <- WB_formula(4, 1, 1.2, rho)
  cat(sprintf("%.2f   %.6f     %.6f     %+.2e\n", rho, sim, f, (sim - f)/f))
}
cat("\nConfig C: a ~ U[-s,s], s=0.9 => sigma_a^2=s^2/3=0.27; k=8, sigma_xi=1\n")
sa_u <- sqrt(0.9^2/3)
for (rho in c(0.3, 0.7)) {
  sim <- mc_WB(8, 1, sa_u, rho, dist_a = "unif", s_unif = 0.9)
  f <- WB_formula(8, 1, sa_u, rho)
  cat(sprintf("%.1f    %.6f     %.6f     %+.2e\n", rho, sim, f, (sim - f)/f))
}
# Monotonic decrease of formula on [0,1] for many random configs
set.seed(14)
mono_viol <- replicate(5000, {
  k <- sample(1:20, 1); sx <- runif(1, 0.1, 2); sa <- runif(1, 0.1, 2)
  rg <- seq(0, 1, length.out = 401)
  any(diff(WB_formula(k, sx, sa, rg)) >= 0)
})
cat(sprintf("\nW_B strictly decreasing on [0,1]: violations in 5000 random configs = %d\n", sum(mono_viol)))
# Exit increase Delta W_B and its monotonicity in rho_pre
set.seed(15)
dwb_viol <- replicate(5000, {
  k <- sample(1:20, 1); sx <- runif(1, 0.1, 2); sa <- runif(1, 0.1, 2)
  V0 <- sx^2/k; V1 <- 1/(k/sx^2 + 1/sa^2)
  rp <- sort(runif(2))
  d1 <- WB_formula(k, sx, sa, 0) - WB_formula(k, sx, sa, rp[1])
  d2 <- WB_formula(k, sx, sa, 0) - WB_formula(k, sx, sa, rp[2])
  claimed1 <- rp[1] * ((V0 - V1) + rp[1] * V1^2/sa^2)
  (d1 <= 0) || (d2 <= d1) || abs(d1 - claimed1) > 1e-12
})
cat(sprintf("Delta W_B > 0, increasing in rho_pre, matches closed form: violations = %d\n", sum(dwb_viol)))

cat("\n==========================================================\n")
cat("PART 5: Prop 4 -- deed counting and fire-sale share\n")
cat("==========================================================\n")
lam <- 0.4; D <- 1; c <- 0.4; w <- 0.8; s <- 0.15; nu <- 0.10; theta <- 0.6
mst <- w + c
set.seed(123)
for (a in c(-s, 0, 0.7*s)) {
  N <- 4e6
  mover <- runif(N) < lam
  eps <- runif(N, -w, w)
  tauD <- a - mst + D
  sell_machine <- mover & (eps <= tauD)             # non-movers: tau0 = a-m* < -w, never
  stopifnot(a - mst < -w)                            # check non-mover threshold below -w
  lister <- (!mover) & (runif(N) < nu)
  hh_mover <- mover & !sell_machine
  vol_no_machine <- (sum(mover) + sum(lister))/N
  vol_with <- (2*sum(sell_machine) + sum(hh_mover) + sum(lister))/N
  purchases <- sum(sell_machine)/N
  cat(sprintf("a=%+.3f: V0_total sim=%.6f claim=%.6f | total sim=%.6f claim(V0+purch)=%.6f\n",
              a, vol_no_machine, lam + (1 - lam)*nu, vol_with, vol_no_machine + purchases))
  cat(sprintf("          machine purchases sim=%.6f  claim lam*P(eps<=tauD)=%.6f\n",
              purchases, lam * (tauD + w)/(2*w)))
  # fire-sale share among household trades (b=0, hatv=v): mover sales are exactly t-below trades, t<=theta*D
  fs_no  <- sum(mover)/(sum(mover) + sum(lister))
  fs_with <- sum(hh_mover)/(sum(hh_mover) + sum(lister))
  PgtT <- 1 - (tauD + w)/(2*w)
  cat(sprintf("          fire-sale share: no-machine sim=%.6f claim=%.6f | with sim=%.6f claim=%.6f\n",
              fs_no, lam/(lam + (1 - lam)*nu),
              fs_with, lam*PgtT/(lam*PgtT + (1 - lam)*nu)))
}

cat("\n==========================================================\n")
cat("PART 6: BAD-region detail (B1): truth vs claimed formulas\n")
cat("==========================================================\n")
lam <- 0.5; D <- 2; c <- 0.5; w <- 0.8; s <- 0.3
cat(sprintf("Assumption 1: c in (0,D) %s; 2w=%.2f > D-c=%.2f %s; s=%.2f < min(c,(D-c)/2)=%.2f %s\n",
            "OK", 2*w, D - c, "OK", s, min(c, (D - c)/2), "OK"))
cat(sprintf("But 2w + c - D = %.2f < s = %.2f  => quadratic branch NOT valid state-by-state at m=w+c\n",
            2*w + c - D, s))
mgrid <- seq(0.9, 2.2, length.out = 26001)
ep <- vapply(mgrid, EPi, 0, lam = lam, D = D, c = c, w = w, s = s, n = 4001)
mhat <- mgrid[which.max(ep)]
opt <- optimize(function(m) EPi(m, lam, D, c, w, s, n = 20001),
                lower = mhat - 0.01, upper = mhat + 0.01, maximum = TRUE, tol = 1e-12)
cat(sprintf("true argmax m = %.6f  (claimed w+c = %.6f)\n", opt$maximum, w + c))
cat(sprintf("true max E Pi = %.8f\n", opt$objective))
cat(sprintf("true E Pi(w+c) = %.8f\n", EPi(w + c, lam, D, c, w, s, n = 20001)))
cat(sprintf("claimed Pi* = lam[(D-c)^2 - s^2/3]/(4w) = %.8f\n", lam*((D - c)^2 - s^2/3)/(4*w)))
cat(sprintf("true E Vol(w+c) = %.8f   claimed V* = %.8f\n",
            EVol(w + c, lam, D, c, w, s, n = 20001), lam*(D - c)/(2*w)))
cat(sprintf("true E Vol(argmax) = %.8f\n", EVol(opt$maximum, lam, D, c, w, s, n = 20001)))

cat("\nDone.\n")
