#' 04_entry_did.R — Staggered iBuyer entry: Sun–Abraham event studies
#'
#' Design 1. CBSA-level entry = first quarter with >= ENTRY_MIN_BUYS iBuyer
#' purchases (any firm). ZIP5 x quarter panel, 2012Q1–2021Q3 (pre-exit).
#' Controls: ZIPs in never-entered CBSAs within the same states.
#' Estimator: Sun & Abraham (2021) interaction-weighted event study via
#' fixest::sunab, cluster at CBSA.
#'
#' Outcomes: log volume, hedonic-residual dispersion (SD), log median price,
#' left-tail share. Heterogeneity: ZIP machine-readability.
#'
#' Outputs: out_dir/entry_es_<outcome>.rds, out_dir/entry_att_table.rds,
#'          out_dir/entry_panel_meta.rds

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))
set.seed(20260610)

ENTRY_MIN_BUYS <- 10L
ES_START_QID <- 2012L * 4L      # 2012Q1
ES_END_QID   <- round(2021.50 * 4)  # 2021Q3

panel <- read_parquet(path(data_dir, "zip_quarter_panel.parquet"))
disp  <- read_parquet(path(data_dir, "zip_quarter_dispersion.parquet"))
zcov  <- read_parquet(path(data_dir, "zip_covariates.parquet"))

# NOTE: ~300 ZIP5s appear in two states (border ZIPs + typos), so all keys
# are state-aware and the panel unit is state x ZIP5 (zid).
df <- panel |>
  left_join(disp |> select(state, zip5, yq, n_hedonic, disp_sd, disp_iqr,
                           left_tail),
            by = c("state", "zip5", "yq")) |>
  left_join(zcov |> select(state, zip5, cbsa, stock_resi, sfr_share,
                           sd_lsqft_stock, share_post1990),
            by = c("state", "zip5")) |>
  filter(!is.na(cbsa), stock_resi >= 500) |>
  mutate(qid = as.integer(round(yq * 4)),
         zid = paste0(state, "_", zip5))

# ---- CBSA entry cohorts ----
cbsa_qtr <- df |>
  group_by(cbsa, qid) |>
  summarise(ib_buys = sum(n_ib_buy), .groups = "drop")
entry <- cbsa_qtr |>
  filter(ib_buys >= ENTRY_MIN_BUYS) |>
  group_by(cbsa) |>
  summarise(entry_qid = min(qid), .groups = "drop")
df <- df |>
  left_join(entry, by = "cbsa") |>
  mutate(cohort = ifelse(is.na(entry_qid), 99999L, entry_qid))

es_df <- df |>
  filter(qid >= ES_START_QID, qid <= ES_END_QID) |>
  mutate(
    lvol    = log(n_sales),
    lmedp   = log(med_price),
    readability = -scale(sd_lsqft_stock)[, 1]   # homogeneous stock = readable
  )

meta <- list(
  n_cbsa_entered  = sum(entry$entry_qid <= ES_END_QID, na.rm = TRUE),
  n_cbsa_total    = n_distinct(es_df$cbsa),
  n_zip           = n_distinct(es_df$zid),
  n_cells         = nrow(es_df),
  entry_dist      = entry |> mutate(yq = entry_qid / 4) |> count(yq),
  first_entry_yq  = min(entry$entry_qid) / 4,
  threshold       = ENTRY_MIN_BUYS
)
saveRDS(meta, path(out_dir, "entry_panel_meta.rds"))
message("Entered CBSAs: ", meta$n_cbsa_entered, " / ", meta$n_cbsa_total,
        " | ZIPs: ", meta$n_zip)

outcomes <- c(lvol = "lvol", disp = "disp_sd", iqr = "disp_iqr",
              lmedp = "lmedp", ltail = "left_tail")

# Pre-trend joint test runs on the TWFE event-study version: the raw
# Sun-Abraham cohort interactions are too numerous for a full-rank joint
# Wald under CBSA clustering (verified: singular VCOV -> NA).
es_df <- es_df |>
  mutate(
    rel  = ifelse(cohort == 99999L, -1L, qid - cohort),
    relb = pmax(pmin(rel, 12L), -13L)   # bin endpoints; never-treated at ref
  )

es_models  <- list()
for (nm in names(outcomes)) {
  y <- outcomes[[nm]]
  fml_es <- as.formula(paste0(y, " ~ sunab(cohort, qid, ref.p = -1) | zid + qid"))
  m <- feols(fml_es, data = es_df, cluster = ~cbsa, notes = FALSE)
  m_tw <- feols(as.formula(paste0(y, " ~ i(relb, ref = -1) | zid + qid")),
                data = es_df, cluster = ~cbsa, notes = FALSE)
  # Joint test of pre-event coefficients -12..-2 (binned -13 excluded)
  pre_p <- tryCatch(fixest::wald(m_tw, keep = "::-(1[0-2]|[2-9])$")$p,
                    error = function(e) NA_real_)
  es_models[[nm]] <- list(
    coefs = broom::tidy(m, conf.int = TRUE),
    agg   = summary(m, agg = "att") |> broom::tidy(conf.int = TRUE),
    nobs  = nobs(m),
    pretrend_p = pre_p
  )
  message(nm, ": ATT = ",
          round(es_models[[nm]]$agg$estimate[1], 4),
          " (se ", round(es_models[[nm]]$agg$std.error[1], 4), "), N = ", nobs(m),
          ", pretrend p = ", round(pre_p, 3))
}
saveRDS(es_models, path(out_dir, "entry_es_models.rds"))

# ---- robustness ----
rob <- list()
att_of <- function(m) summary(m, agg = "att") |> broom::tidy(conf.int = TRUE)
for (nm in names(outcomes)) {
  y <- outcomes[[nm]]
  m1 <- feols(as.formula(paste0(y, " ~ sunab(cohort, qid, ref.p = -1) | zid + state^qid")),
              data = es_df, cluster = ~cbsa, notes = FALSE)
  m2 <- feols(as.formula(paste0(y, " ~ sunab(cohort, qid, ref.p = -1) | zid + qid")),
              data = es_df, weights = ~stock_resi, cluster = ~cbsa, notes = FALSE)
  post <- es_df |> mutate(treated_post = as.integer(cohort != 99999L & qid >= cohort))
  m3 <- feols(as.formula(paste0(y, " ~ treated_post | zid + qid")),
              data = post, cluster = ~cbsa, notes = FALSE)
  # anticipation: shift entry one quarter earlier
  antic <- es_df |> mutate(cohort = ifelse(cohort == 99999L, cohort, cohort - 1L))
  m4 <- feols(as.formula(paste0(y, " ~ sunab(cohort, qid, ref.p = -1) | zid + qid")),
              data = antic, cluster = ~cbsa, notes = FALSE)
  # pre-COVID: end panel 2019Q4 (cohorts entering by then)
  pre <- es_df |> filter(qid <= 2019L * 4L + 3L)
  m5 <- feols(as.formula(paste0(y, " ~ sunab(cohort, qid, ref.p = -1) | zid + qid")),
              data = pre, cluster = ~cbsa, notes = FALSE)
  rob[[nm]] <- list(
    state_qtr = att_of(m1),
    weighted  = att_of(m2),
    twfe      = broom::tidy(m3, conf.int = TRUE),
    antic     = att_of(m4),
    precovid  = att_of(m5)
  )
}
saveRDS(rob, path(out_dir, "entry_robustness.rds"))

# ---- heterogeneity: machine readability (within CBSA x quarter) ----
het_df <- es_df |>
  filter(cohort != 99999L) |>
  mutate(
    post = as.integer(qid >= cohort),
    read_terc = ntile(readability, 3)   # 1 = least readable, 3 = most
  )
het <- list()
for (nm in names(outcomes)) {
  y <- outcomes[[nm]]
  m_cont <- feols(as.formula(paste0(y, " ~ post:readability | zid + cbsa^qid")),
                  data = het_df, cluster = ~cbsa, notes = FALSE)
  # post main effect is absorbed by cbsa^qid, so the three post-x-tercile
  # dummies are collinear; estimate differentials vs tercile 1 explicitly.
  m_terc <- feols(as.formula(paste0(y, " ~ i(read_terc, post, ref = 1) | zid + cbsa^qid")),
                  data = het_df, cluster = ~cbsa, notes = FALSE)
  het[[nm]] <- list(cont = broom::tidy(m_cont, conf.int = TRUE),
                    terc = broom::tidy(m_terc, conf.int = TRUE))
}
saveRDS(het, path(out_dir, "entry_heterogeneity.rds"))

message("04_entry_did.R complete.")
