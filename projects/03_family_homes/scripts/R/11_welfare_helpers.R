# Pure accounting and SQL helpers for the conditional welfare calibration.

family_episode_sql <- function(classes) {
  quoted <- paste0("'", classes, "'", collapse = ",")
  glue("WITH ordered AS (
    SELECT *,
      SUM(CASE WHEN class = 'market_sale' THEN 1 ELSE 0 END)
        OVER (PARTITION BY clip ORDER BY sale_date ROWS UNBOUNDED PRECEDING) AS market_spell,
      MIN(CASE WHEN class = 'market_sale' THEN sale_date END)
        OVER (PARTITION BY clip ORDER BY sale_date
          ROWS BETWEEN 1 FOLLOWING AND UNBOUNDED FOLLOWING) AS next_market
    FROM ca_events
  ), episodes AS (
    SELECT clip, market_spell, MIN(sale_date) AS start_date,
      MIN(next_market) AS next_market, COUNT(*) AS n_family_deeds
    FROM ordered WHERE class IN ({quoted}) GROUP BY clip, market_spell
  ) SELECT * FROM episodes
    WHERE start_date BETWEEN DATE '2008-01-01' AND DATE '2018-12-31'")
}

km_from_exits <- function(exits) {
  exits <- exits[order(exits$days), ]
  exits$risk <- sum(exits$n) - c(0, head(cumsum(exits$n), -1))
  exits$survival <- cumprod(1 - exits$fail / exits$risk)
  stopifnot(all(exits$risk > 0), all(exits$fail <= exits$risk),
            all(exits$survival >= 0), all(exits$survival <= 1),
            all(diff(exits$survival) <= 1e-12))
  exits
}

survival_at <- function(km, years) {
  c(1, km$survival)[findInterval(years * 365.25, km$days) + 1L]
}

calibrate_path <- function(flow, alpha, delay, survival, wedge, lambda,
                           discount, hazard_scale = 1, first_year = 2022L) {
  stopifnot(length(survival) == 10, flow >= 0, alpha >= 0, alpha <= 1,
            all(is.finite(survival)), all(survival >= 0 & survival <= 1),
            all(diff(survival) <= 1e-12), delay >= 0, delay == floor(delay),
            wedge >= 0, lambda >= 0, lambda <= 1,
            discount >= 0, hazard_scale > 0)
  years <- first_year + 0:9
  stock <- vapply(0:9, function(k) {
    ages <- k - (0:k) - delay
    ages <- ages[ages >= 0]
    alpha * flow * sum(survival[ages + 1L]^hazard_scale)
  }, numeric(1))
  gains <- lambda * wedge * stock
  # Cohorts enter at start of each year; value year-end flows at start-2022.
  data.frame(year = years, stock = stock, annual_gain = gains,
             discounted_gain = gains / (1 + discount)^(1:10))
}
