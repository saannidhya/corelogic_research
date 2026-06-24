# Synthetic check: fixest term-name formats used by scripts 04-07 extraction code
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
cat("--- sunab tidy terms (first 4) ---\n")
print(head(tidy(m1)$term, 4))
cat("--- agg att tidy ---\n")
print(tidy(summary(m1, agg = "att")))
cat("--- wald pre-period ---\n")
w <- tryCatch(fixest::wald(m1, keep = "::-(1[0-9]|[2-9])$"),
              error = function(e) paste("ERR:", conditionMessage(e)))
print(w)

d$post <- as.integer(d$qid >= 15)
d$qfac <- relevel(factor(d$qid), ref = "14")
m2 <- feols(y ~ zshare:qfac + odshare:qfac | id + cbsa^qid,
            data = d, cluster = ~cbsa, notes = FALSE)
cat("--- zshare:qfac interaction terms (first 5) ---\n")
print(head(tidy(m2)$term, 5))

d$terc <- ntile(d$zshare, 3)
m3 <- feols(y ~ zshare:post:factor(terc) + odshare:post | id + cbsa^qid,
            data = d, cluster = ~cbsa, notes = FALSE)
cat("--- triple-interaction terms ---\n")
print(tidy(m3)$term)
