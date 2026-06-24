# Follow-up: i()-based event study + wald keep debug
suppressMessages({library(fixest); library(dplyr); library(broom)})
set.seed(1)
n <- 4000
d <- data.frame(
  id   = rep(1:200, each = 20),
  qid  = rep(1:20, 200),
  cbsa = rep(1:20, each = 200),
  zshare  = rep(runif(200), each = 20),
  odshare = rep(runif(200), each = 20)
)
d$cohort <- ifelse(d$id %% 3 == 0, 99999L, 8L + (d$id %% 4))
d$y <- rnorm(n) + 0.3 * (d$cohort != 99999L & d$qid >= d$cohort)

m1 <- feols(y ~ sunab(cohort, qid, ref.p = -1) | id + qid,
            data = d, cluster = ~cbsa, notes = FALSE)
cat("--- wald keep='::-' ---\n")
w1 <- tryCatch(fixest::wald(m1, keep = "::-"), error = function(e) conditionMessage(e))
print(w1)

# i()-based exposure event study with explicit reference
m4 <- feols(y ~ i(qid, zshare, ref = 14) + i(qid, odshare, ref = 14) | id + cbsa^qid,
            data = d, cluster = ~cbsa, notes = TRUE)
cat("--- i() terms (first 6) ---\n")
print(head(tidy(m4)$term, 6))
cat("--- n coefs: ", nrow(tidy(m4)), " (expect 38 = 2 x 19)\n")

# previous qfac version: count kept coefficients (collinearity check)
d$qfac <- relevel(factor(d$qid), ref = "14")
m2 <- feols(y ~ zshare:qfac + odshare:qfac | id + cbsa^qid,
            data = d, cluster = ~cbsa)
cat("--- qfac version n coefs: ", nrow(tidy(m2)), " (40 means no level dropped)\n")
