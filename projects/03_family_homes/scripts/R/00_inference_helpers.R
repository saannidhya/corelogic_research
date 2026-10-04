#======================================================================#
# Purpose : Shared inference helpers for the Prop 19 analyses: wild
#           cluster bootstrap, county pairs bootstrap for count models,
#           permutation and Conley-Taber inference for a single treated
#           state, and a relative-magnitudes pre-trend breakdown value.
# Name    : Saani Rawat
# Created : 10/02/2026
# Log     : 10/02/2026: created. wild_cluster_boot() moved here from
#           08_within_ca.R and generalized (defaults reproduce 08 exactly).
# Inputs  : none (sourced by 04_prop19.R and 08_within_ca.R)
# Outputs : none
#======================================================================#

#============================================#
# Wild cluster bootstrap (linear models) ----
#============================================#

#' Null-imposed Rademacher wild cluster bootstrap (Cameron-Gelbach-Miller).
#'
#' Defaults reproduce the within-CA DiD in 08_within_ca.R draw for draw:
#' same formula, same cluster ordering, same Rademacher sampling call.
#'
#' @param data estimation data
#' @param yvar outcome column
#' @param cluster cluster column
#' @param param coefficient name tested at zero
#' @param B number of bootstrap draws
#' @param rhs right-hand side of the unrestricted model
#' @param fe fixed-effect part shared by both models
#' @return list with beta, se, t, bootstrap p, percentile-t CI, B, n_clusters
wild_cluster_boot <- function(data, yvar = "ln_n", cluster = "fips5",
                              param = "treated:post", B = 999L,
                              rhs = "treated:post",
                              fe = "fips5^treated + yq") {
  f_un <- as.formula(paste0(yvar, " ~ ", rhs, " | ", fe))
  f_r  <- as.formula(paste0(yvar, " ~ 1 | ", fe))
  f_cl <- as.formula(paste0("~", cluster))

  # unrestricted fit gives the estimate and the analytic CRVE t-stat
  m_un <- fixest::feols(f_un, data = data, cluster = f_cl)
  b_hat  <- coef(m_un)[param]
  se_hat <- fixest::se(m_un)[param]
  t_hat  <- b_hat / se_hat

  # restricted fit imposes H0: beta = 0
  m_r <- fixest::feols(f_r, data = data)
  yhat_r <- predict(m_r)
  uhat_r <- resid(m_r)

  cl <- as.character(data[[cluster]])
  gids <- unique(cl)
  tstar <- numeric(B)
  for (b in seq_len(B)) {
    w <- sample(c(-1L, 1L), length(gids), replace = TRUE)
    names(w) <- gids
    d2 <- data
    d2[[yvar]] <- yhat_r + uhat_r * w[cl]
    mb <- fixest::feols(f_un, data = d2, cluster = f_cl)
    tstar[b] <- coef(mb)[param] / fixest::se(mb)[param]
  }

  p_boot <- (1 + sum(abs(tstar) >= abs(t_hat))) / (B + 1)
  qd <- quantile(tstar, c(0.975, 0.025), names = FALSE)
  ci <- b_hat - qd * se_hat  # percentile-t 95% CI
  list(beta = unname(b_hat), se = unname(se_hat), t = unname(t_hat),
       p_boot = p_boot, ci_lo = ci[1], ci_hi = ci[2], B = B,
       n_clusters = length(gids))
}

#============================================#
# County pairs bootstrap (count models) ----
#============================================#

#' Cluster pairs bootstrap for any fitted statistic.
#'
#' Resamples clusters with replacement. A cluster drawn twice enters as two
#' distinct clusters, so cluster-interacted fixed effects stay separate.
#'
#' @param data estimation data
#' @param cluster cluster column (overwritten with bootstrap ids in draws)
#' @param stat_fun function(data) returning a named numeric vector
#' @param B number of draws
#' @return tibble with one row per draw and one column per statistic
cluster_pairs_boot <- function(data, cluster, stat_fun, B = 199L) {
  cluster_ids <- unique(data[[cluster]])
  rows_by_cluster <- split(seq_len(nrow(data)), data[[cluster]])

  draws <- vector("list", B)
  for (b in seq_len(B)) {
    drawn <- sample(cluster_ids, length(cluster_ids), replace = TRUE)
    d_b <- purrr::imap_dfr(drawn, function(cl_id, k) {
      d_k <- data[rows_by_cluster[[as.character(cl_id)]], , drop = FALSE]
      d_k[[cluster]] <- paste0(cl_id, "_b", k)
      d_k
    })
    draws[[b]] <- tryCatch(stat_fun(d_b), error = function(e) NULL)
  }

  failed <- sum(vapply(draws, is.null, logical(1)))
  if (failed > 0) message("NOTE: ", failed, " of ", B, " bootstrap draws failed and were dropped")
  dplyr::bind_rows(purrr::compact(draws))
}

#============================================#
# Single-treated-state inference ----
#============================================#

#' Two-sided rank p-value of the treated statistic among all units.
#' The treated unit is included in the reference set (Fisher exact form).
rank_p_value <- function(estimates, treated_unit, unit_col = "state", est_col = "estimate") {
  est <- estimates[[est_col]]
  treated_est <- est[estimates[[unit_col]] == treated_unit]
  stopifnot(length(treated_est) == 1L)
  mean(abs(est) >= abs(treated_est))
}

#' Conley-Taber p-value and CI from the control units' placebo estimates.
#' Placebo estimates approximate the sampling distribution under no effect.
conley_taber <- function(treated_est, placebo_est, level = 0.95) {
  placebo_est <- placebo_est[is.finite(placebo_est)]
  alpha <- 1 - level
  ct_p <- (1 + sum(abs(placebo_est) >= abs(treated_est))) / (1 + length(placebo_est))
  ct_ci <- treated_est - quantile(placebo_est, c(1 - alpha / 2, alpha / 2), names = FALSE)
  tibble::tibble(estimate = treated_est, ct_p = ct_p,
                 ct_ci_lo = ct_ci[1], ct_ci_hi = ct_ci[2],
                 n_placebo = length(placebo_est))
}

#============================================#
# Pre-trend sensitivity ----
#============================================#

#' Approximate relative-magnitudes breakdown value (Rambachan-Roth 2023).
#'
#' Under the RM restriction, the post-period trend violation is at most
#' Mbar times the largest consecutive pre-period change. For a single
#' post coefficient the 95% CI excludes zero for every Mbar below
#' (|beta| - 1.96 se) / max|delta pre|. This is a transparent stand-in
#' for HonestDiD, which is not installed in the project library.
#'
#' @param pre_coefs pre-period event-study coefficients in time order,
#'   including the normalized reference period (0)
#' @param post_beta post-period coefficient
#' @param post_se its standard error
rm_breakdown <- function(pre_coefs, post_beta, post_se) {
  max_pre_change <- max(abs(diff(pre_coefs)))
  tibble::tibble(
    post_beta = post_beta, post_se = post_se,
    max_pre_change = max_pre_change,
    mbar_breakdown = max(0, (abs(post_beta) - 1.96 * post_se) / max_pre_change)
  )
}
