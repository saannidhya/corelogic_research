#' 05_exit_did.R — The Zillow Offers shutdown (announced 2021-11-02)
#'
#' Design 2. Within CBSAs where Zillow Offers operated, compare ZIPs by
#' pre-period Zillow purchase share (exposure) before/after 2021Q4, holding
#' the continuing iBuyers' exposure (Opendoor + Offerpad) as the
#' counterfactual loading that nets out Sunbelt-cooling selection:
#'
#'   y_zt = beta (Zshare_z x Post_t) + gamma (ODshare_z x Post_t)
#'          + alpha_z + delta_{cbsa x t} + e_zt
#'
#' The identified contrast: ZIPs that lost their algorithmic buyer vs ZIPs
#' whose algorithmic buyer stayed, within CBSA-quarter.
#'
#' PRIMARY volume outcome is HOUSEHOLD-ONLY volume (referee B Major 2):
#'   lvol = log(n_sales - n_ib_buy - n_ib_sell)
#' so the coefficient is not mechanically contaminated by the machine's own
#' disappearing deeds. The gross-volume version is retained as lvol_gross.
#'
#' Outputs: out_dir/exit_main.rds, exit_es_models.rds, exit_robustness.rds,
#'          exit_sample_meta.rds, exit_vintage.rds, exit_doseresponse.rds,
#'          exit_permutation.rds (999 draws), exit_placebo_perm.rds,
#'          exit_wildboot.rds, exit_extensive_margin.rds

source(here::here("projects/04_algorithmic_intermediation/scripts/R/00_setup.R"))
set.seed(20260610)

W_START <- round(2019.00 * 4)   # 2019Q1
W_END   <- round(2024.25 * 4)   # 2024Q2 (full path, referee A H: decay test)
EXIT_QID <- round(EXIT_YQ * 4)  # 2021Q4
EXPO_START <- round(ZILLOW_EXPO_START * 4)
EXPO_END   <- round(ZILLOW_EXPO_END * 4)
RAMP_START <- round(2021.00 * 4)            # 2021Q1 (Project-Ketchup ramp)
PRE_END    <- RAMP_START - 1L               # 2020Q4

panel <- read_parquet(path(data_dir, "zip_quarter_panel.parquet"))
disp  <- read_parquet(path(data_dir, "zip_quarter_dispersion.parquet"))
zcov  <- read_parquet(path(data_dir, "zip_covariates.parquet"))

# NOTE: ~300 ZIP5s appear in two states (border ZIPs + typos), so all keys
# are state-aware and the panel unit is state x ZIP5 (zid).
disp_sel <- disp |>
  select(state, zip5, yq, n_hedonic, disp_sd, disp_iqr, left_tail,
         any_of("left_tail_raw"))
df <- panel |>
  left_join(disp_sel, by = c("state", "zip5", "yq")) |>
  left_join(zcov |> select(state, zip5, cbsa, stock_resi, sd_lsqft_stock),
            by = c("state", "zip5")) |>
  filter(!is.na(cbsa), stock_resi >= 500) |>
  mutate(qid = as.integer(round(yq * 4)),
         zid = paste0(state, "_", zip5),
         n_hh = pmax(n_sales - n_ib_buy - n_ib_sell, 0L))
if (!"left_tail_raw" %in% names(df)) df$left_tail_raw <- NA_real_

# ---- exposure shares over the Zillow operating window ----
# total + vintage-split (pre-Ketchup 2018Q2-2020Q4 vs ramp 2021Q1-2021Q3)
expo <- df |>
  filter(qid >= EXPO_START, qid <= EXPO_END) |>
  group_by(state, zip5, zid, cbsa) |>
  summarise(
    sales_w   = sum(n_sales),
    z_buys    = sum(n_zillow_buy),
    z_pre     = sum(n_zillow_buy[qid <= PRE_END]),
    z_ramp    = sum(n_zillow_buy[qid >= RAMP_START]),
    od_buys   = sum(n_opendoor_buy) + sum(n_offerpad_buy),
    rf_buys   = sum(n_redfin_buy),
    sales_pre = sum(n_sales[qid <= PRE_END]),
    .groups = "drop"
  ) |>
  filter(sales_w >= 50) |>
  mutate(
    zshare    = 100 * z_buys  / sales_w,
    zshare_pre  = 100 * z_pre  / pmax(sales_pre, 1),
    zshare_ramp = 100 * z_ramp / sales_w,
    odshare   = 100 * od_buys / sales_w,
    rfshare   = 100 * rf_buys / sales_w
  )

# Winsorize exposures at the 99th pct among positive values
wins <- function(x, p = 0.99) pmin(x, quantile(x[x > 0], p, na.rm = TRUE))
w99z  <- quantile(expo$zshare[expo$zshare > 0],  0.99, na.rm = TRUE)
w99od <- quantile(expo$odshare[expo$odshare > 0], 0.99, na.rm = TRUE)
expo <- expo |>
  mutate(zshare = pmin(zshare, w99z), odshare = pmin(odshare, w99od),
         zshare_pre = wins(zshare_pre), zshare_ramp = wins(zshare_ramp))

# Zillow CBSAs: any Zillow purchase in window
z_cbsas <- expo |> group_by(cbsa) |> summarise(zb = sum(z_buys)) |>
  filter(zb >= 50) |> pull(cbsa)

es_df <- df |>
  inner_join(expo |> select(state, zip5, zshare, zshare_pre, zshare_ramp,
                            odshare, sales_w),
             by = c("state", "zip5")) |>
  filter(cbsa %in% z_cbsas, qid >= W_START, qid <= W_END) |>
  mutate(
    post      = as.integer(qid >= EXIT_QID),
    lvol      = log(pmax(n_hh, 1)),     # PRIMARY: household-only volume
    lvol_gross = log(n_sales),          # secondary: includes machine deeds
    lmedp     = log(med_price),
    has_disp  = as.integer(!is.na(disp_sd))
  )

n_zero_ctrl <- expo |> filter(cbsa %in% z_cbsas, zshare == 0) |> nrow()
meta <- list(
  n_zip = n_distinct(es_df$zid), n_cbsa = n_distinct(es_df$cbsa),
  n_cells = nrow(es_df), n_zero_ctrl = n_zero_ctrl,
  zshare_mean = mean(expo$zshare[expo$cbsa %in% z_cbsas]),
  zshare_p90  = quantile(expo$zshare[expo$cbsa %in% z_cbsas], 0.90),
  odshare_mean = mean(expo$odshare[expo$cbsa %in% z_cbsas]),
  zshare_pre_mean = mean(expo$zshare_pre[expo$cbsa %in% z_cbsas]),
  zshare_ramp_mean = mean(expo$zshare_ramp[expo$cbsa %in% z_cbsas]),
  w99z = w99z, w99od = w99od,
  filters = "sales_w >= 50; stock_resi >= 500; CBSA Zillow buys >= 50"
)
saveRDS(meta, path(out_dir, "exit_sample_meta.rds"))
message("Exit sample: ", meta$n_zip, " ZIPs in ", meta$n_cbsa,
        " CBSAs; ", n_zero_ctrl, " zero-exposure control ZIPs")

# Primary outcome set: household volume + information outcomes.
outcomes <- c(lvol = "lvol", lvol_gross = "lvol_gross",
              disp = "disp_sd", iqr = "disp_iqr",
              ltail = "left_tail", ltail_raw = "left_tail_raw",
              lmedp = "lmedp")

zcoef <- function(tt) tt[grepl("zshare", tt$term) & !grepl("pre|ramp", tt$term), ][1, ]
odcoef <- function(tt) tt[grepl("odshare", tt$term), ][1, ]

# ---- main DiD ----
main <- list()
for (nm in names(outcomes)) {
  y <- outcomes[[nm]]
  if (all(is.na(es_df[[y]]))) next
  m <- feols(as.formula(paste0(y, " ~ zshare:post + odshare:post | zid + cbsa^qid")),
             data = es_df, cluster = ~cbsa, notes = FALSE)
  main[[nm]] <- list(tidy = broom::tidy(m, conf.int = TRUE), nobs = nobs(m))
  b <- zcoef(broom::tidy(m))
  message(nm, ": zshare x post = ", round(b$estimate, 5),
          " (se ", round(b$std.error, 5), ", p ", round(b$p.value, 3), ")")
}
saveRDS(main, path(out_dir, "exit_main.rds"))

# ---- vintage split (referee B Major 1 / referee A 4): does the effect load
#      on pre-Ketchup exposure rather than the 2021 froth-chasing ramp? ----
vintage <- list()
for (nm in c("lvol", "disp", "ltail")) {
  y <- outcomes[[nm]]
  m <- feols(as.formula(paste0(
    y, " ~ zshare_pre:post + zshare_ramp:post + odshare:post | zid + cbsa^qid")),
    data = es_df, cluster = ~cbsa, notes = FALSE)
  vintage[[nm]] <- broom::tidy(m, conf.int = TRUE)
}
saveRDS(vintage, path(out_dir, "exit_vintage.rds"))

# ---- event studies (full path through 2024Q2; ref 2021Q3) ----
REF_QID <- EXIT_QID - 1L
es_models <- list()
for (nm in c("lvol", "lvol_gross", "disp", "ltail")) {
  y <- outcomes[[nm]]
  m <- feols(as.formula(paste0(
    y, " ~ i(qid, zshare, ref = ", REF_QID, ") + i(qid, odshare, ref = ",
    REF_QID, ") | zid + cbsa^qid")),
    data = es_df, cluster = ~cbsa, notes = FALSE)
  es_models[[nm]] <- list(tidy = broom::tidy(m, conf.int = TRUE), nobs = nobs(m))
}
saveRDS(es_models, path(out_dir, "exit_es_models.rds"))

# Joint pre-trend tests (every outcome): coefficients at qid < REF_QID
pretrend <- list()
for (nm in c("lvol", "disp", "ltail")) {
  m <- es_models[[nm]]$tidy
  # refit to get a wald; use a fresh fit object
  fit <- feols(as.formula(paste0(
    outcomes[[nm]], " ~ i(qid, zshare, ref = ", REF_QID,
    ") + i(qid, odshare, ref = ", REF_QID, ") | zid + cbsa^qid")),
    data = es_df, cluster = ~cbsa, notes = FALSE)
  pre_qids <- sort(unique(es_df$qid[es_df$qid < REF_QID & es_df$qid >= W_START]))
  keeppat <- paste0("qid::(", paste(pre_qids, collapse = "|"), "):zshare")
  pretrend[[nm]] <- tryCatch(fixest::wald(fit, keeppat)$p,
                             error = function(e) NA_real_)
}
saveRDS(pretrend, path(out_dir, "exit_pretrend.rds"))

# ---- dose-response: exposure terciles on DISTINCT ZIPs (referee B Major 3,
#      minor 1) ----
zexp <- es_df |> distinct(zid, zshare)
pos <- zexp$zshare[zexp$zshare > 0]
cut1 <- quantile(pos, 1/3); cut2 <- quantile(pos, 2/3)
zexp <- zexp |>
  mutate(zterc = case_when(zshare == 0 ~ 0L, zshare <= cut1 ~ 1L,
                           zshare <= cut2 ~ 2L, TRUE ~ 3L))
dose_df <- es_df |> left_join(zexp |> select(zid, zterc), by = "zid")
doseresp <- list()
for (nm in c("lvol", "disp")) {
  m <- feols(as.formula(paste0(
    outcomes[[nm]], " ~ i(zterc, post, ref = 0) + odshare:post | zid + cbsa^qid")),
    data = dose_df, cluster = ~cbsa, notes = FALSE)
  doseresp[[nm]] <- broom::tidy(m, conf.int = TRUE)
}
saveRDS(doseresp, path(out_dir, "exit_doseresponse.rds"))

# ---- extensive margin: is cell inclusion (n_hedonic>=5) treatment-driven?
#      (referee B Major 5) ----
em <- feols(has_disp ~ zshare:post + odshare:post | zid + cbsa^qid,
            data = es_df, cluster = ~cbsa, notes = FALSE)
saveRDS(broom::tidy(em, conf.int = TRUE), path(out_dir, "exit_extensive_margin.rds"))

# ---- heterogeneity: readability terciles on DISTINCT ZIPs (minor 1 fix) ----
zread <- zcov |>
  filter(!is.na(sd_lsqft_stock)) |>
  mutate(readability = -scale(sd_lsqft_stock)[, 1]) |>
  select(state, zip5, readability)
zread_z <- es_df |> distinct(state, zip5) |>
  inner_join(zread, by = c("state", "zip5")) |>
  mutate(read_terc = ntile(readability, 3))
het_df <- es_df |> inner_join(zread_z |> select(state, zip5, read_terc),
                              by = c("state", "zip5"))
het <- list()
for (nm in c("disp", "lvol")) {
  m <- feols(as.formula(paste0(
    outcomes[[nm]], " ~ zshare:post:factor(read_terc) + odshare:post | zid + cbsa^qid")),
    data = het_df, cluster = ~cbsa, notes = FALSE)
  het[[nm]] <- broom::tidy(m, conf.int = TRUE)
}
saveRDS(het, path(out_dir, "exit_heterogeneity.rds"))

# ---- robustness (ALL outcomes; referee B Major 3) ----
rob <- list()
liq_drop <- es_df |> filter(!(qid %in% (EXIT_QID:(EXIT_QID + 2L))))
top_q <- quantile(expo$zshare[expo$zshare > 0 & expo$cbsa %in% z_cbsas], 0.75)
bin_df <- es_df |> mutate(zhi = as.integer(zshare >= top_q))
cell10 <- es_df |> filter(is.na(n_hedonic) | n_hedonic >= 10)
no_phx <- es_df |> filter(!(suppressWarnings(as.numeric(fips)) %in% c(4013, 4021)))
# 95th-pct winsorization
w95z  <- quantile(expo$zshare[expo$zshare > 0],  0.95, na.rm = TRUE)
w95od <- quantile(expo$odshare[expo$odshare > 0], 0.95, na.rm = TRUE)
w95_df <- es_df |>
  mutate(zshare95 = pmin(zshare, w95z), odshare95 = pmin(odshare, w95od))
for (nm in names(outcomes)) {
  y <- outcomes[[nm]]
  if (all(is.na(es_df[[y]]))) next
  f0 <- paste0(y, " ~ zshare:post + odshare:post | zid + cbsa^qid")
  rob[[nm]] <- list(
    drop_liq   = broom::tidy(feols(as.formula(f0), data = liq_drop, cluster = ~cbsa, notes = FALSE), conf.int = TRUE),
    binary     = broom::tidy(feols(as.formula(paste0(y, " ~ zhi:post + odshare:post | zid + cbsa^qid")), data = bin_df, cluster = ~cbsa, notes = FALSE), conf.int = TRUE),
    zip_clust  = broom::tidy(feols(as.formula(f0), data = es_df, cluster = ~zid, notes = FALSE), conf.int = TRUE),
    weighted   = broom::tidy(feols(as.formula(f0), data = es_df, weights = ~sales_w, cluster = ~cbsa, notes = FALSE), conf.int = TRUE),
    cellmin10  = broom::tidy(feols(as.formula(f0), data = cell10, cluster = ~cbsa, notes = FALSE), conf.int = TRUE),
    no_phoenix = broom::tidy(feols(as.formula(f0), data = no_phx, cluster = ~cbsa, notes = FALSE), conf.int = TRUE),
    winsor95   = broom::tidy(feols(as.formula(paste0(y, " ~ zshare95:post + odshare95:post | zid + cbsa^qid")), data = w95_df, cluster = ~cbsa, notes = FALSE), conf.int = TRUE)
  )
}
saveRDS(rob, path(out_dir, "exit_robustness.rds"))

# ---- inference 1: permutation (999 draws, within-CBSA reshuffle of Zshare) ----
N_PERM <- 999L
perm_outcomes <- c("lvol", "disp", "ltail")
perm_t <- matrix(NA_real_, nrow = N_PERM, ncol = length(perm_outcomes),
                 dimnames = list(NULL, perm_outcomes))
zip_x <- es_df |> distinct(zid, cbsa, zshare, odshare)
for (p in 1L:N_PERM) {
  shuf <- zip_x |> group_by(cbsa) |> mutate(zshare_p = sample(zshare)) |>
    ungroup() |> select(zid, zshare_p)
  pd <- es_df |> select(-any_of("zshare_p")) |> left_join(shuf, by = "zid")
  for (nm in perm_outcomes) {
    y <- outcomes[[nm]]
    m <- tryCatch(feols(as.formula(paste0(y, " ~ zshare_p:post + odshare:post | zid + cbsa^qid")),
                        data = pd, cluster = ~cbsa, notes = FALSE),
                  error = function(e) NULL)
    if (!is.null(m)) {
      tt <- broom::tidy(m); r <- tt[grepl("zshare_p", tt$term), ][1, ]
      perm_t[p, nm] <- r$statistic
    }
  }
}
saveRDS(perm_t, path(out_dir, "exit_permutation.rds"))

# ---- inference 2: pre-period placebo permutation (fake post = 2020Q3 on the
#      pre-shutdown sample only; should center on zero) ----
pre_df <- es_df |> filter(qid < EXIT_QID) |>
  mutate(post = as.integer(qid >= round(2020.5 * 4)))
N_PLAC <- 999L
plac_t <- matrix(NA_real_, nrow = N_PLAC, ncol = length(perm_outcomes),
                 dimnames = list(NULL, perm_outcomes))
zip_xp <- pre_df |> distinct(zid, cbsa, zshare, odshare)
plac_obs <- sapply(perm_outcomes, function(nm) {
  m <- feols(as.formula(paste0(outcomes[[nm]], " ~ zshare:post + odshare:post | zid + cbsa^qid")),
             data = pre_df, cluster = ~cbsa, notes = FALSE)
  tt <- broom::tidy(m); zcoef(tt)$statistic
})
for (p in 1L:N_PLAC) {
  shuf <- zip_xp |> group_by(cbsa) |> mutate(zshare_p = sample(zshare)) |>
    ungroup() |> select(zid, zshare_p)
  pd <- pre_df |> select(-any_of("zshare_p")) |> left_join(shuf, by = "zid")
  for (nm in perm_outcomes) {
    m <- tryCatch(feols(as.formula(paste0(outcomes[[nm]], " ~ zshare_p:post + odshare:post | zid + cbsa^qid")),
                        data = pd, cluster = ~cbsa, notes = FALSE),
                  error = function(e) NULL)
    if (!is.null(m)) { tt <- broom::tidy(m); plac_t[p, nm] <- tt[grepl("zshare_p", tt$term), ][1, ]$statistic }
  }
}
saveRDS(list(obs = plac_obs, perm = plac_t), path(out_dir, "exit_placebo_perm.rds"))

# ---- inference 3: wild cluster bootstrap (Webb 6-pt, null-imposed) for the
#      two headline coefficients (referee B Major 4) ----
webb <- c(-sqrt(3/2), -1, -sqrt(1/2), sqrt(1/2), 1, sqrt(3/2))
B_WCB <- 999L
wild_boot <- function(y) {
  # restricted model imposes H0: zshare:post = 0
  mr <- feols(as.formula(paste0(y, " ~ odshare:post | zid + cbsa^qid")),
              data = es_df, cluster = ~cbsa, notes = FALSE)
  yhat_r <- predict(mr); u_r <- resid(mr)
  mf <- feols(as.formula(paste0(y, " ~ zshare:post + odshare:post | zid + cbsa^qid")),
              data = es_df, cluster = ~cbsa, notes = FALSE)
  t_obs <- zcoef(broom::tidy(mf))$statistic
  cl <- es_df$cbsa
  tb <- numeric(B_WCB)
  for (b in 1L:B_WCB) {
    wts <- sample(webb, length(unique(cl)), replace = TRUE)
    names(wts) <- unique(cl)
    ystar <- yhat_r + u_r * wts[as.character(cl)]
    dstar <- es_df; dstar$.ystar <- ystar
    mb <- tryCatch(feols(as.formula(".ystar ~ zshare:post + odshare:post | zid + cbsa^qid"),
                         data = dstar, cluster = ~cbsa, notes = FALSE),
                   error = function(e) NULL)
    tb[b] <- if (is.null(mb)) NA_real_ else zcoef(broom::tidy(mb))$statistic
  }
  tb <- tb[is.finite(tb)]
  list(t_obs = t_obs, p = (1 + sum(abs(tb) >= abs(t_obs))) / (1 + length(tb)),
       B = length(tb))
}
wildboot <- list(lvol = wild_boot("lvol"), disp = wild_boot("disp_sd"))
saveRDS(wildboot, path(out_dir, "exit_wildboot.rds"))

message("05_exit_did.R complete.")
message("  household-volume zshare:post = ",
        round(zcoef(main$lvol$tidy)$estimate, 4),
        " (perm p ", round((1 + sum(abs(perm_t[, "lvol"]) >= abs(zcoef(main$lvol$tidy)$statistic), na.rm = TRUE)) / (1 + sum(is.finite(perm_t[, "lvol"]))), 3),
        "; wild p ", round(wildboot$lvol$p, 3), ")")
