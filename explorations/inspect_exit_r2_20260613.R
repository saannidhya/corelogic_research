# Inspect R&R exit outputs: household-volume event study, donut, RedfinNow, balance
od <- "projects/04_algorithmic_intermediation/scripts/R/_outputs"
es <- readRDS(file.path(od, "exit_es_models.rds"))
tt <- es[["lvol"]][["tidy"]]
z <- tt[grepl("^qid::[0-9]+:zshare$", tt[["term"]]), ]
z[["q"]] <- as.numeric(gsub("[^0-9]", "", z[["term"]]))
z <- z[order(z[["q"]]), ]
z[["yq"]] <- z[["q"]] / 4
cat("--- household-volume event study (zshare x quarter); ref 2021Q3 ---\n")
print(data.frame(yq = round(z[["yq"]], 2), est = round(z[["estimate"]], 3),
                 se = round(z[["std.error"]], 3), t = round(z[["statistic"]], 2)))

d <- readRDS(file.path(od, "exit_donut.rds"))
dz <- d[["lvol"]][["tidy"]]
cat("\n--- donut hh volume zshare:post:",
    round(dz[["estimate"]][grepl("zshare", dz[["term"]])][1], 4),
    "(se", round(dz[["std.error"]][grepl("zshare", dz[["term"]])][1], 4), ")\n")

rf <- readRDS(file.path(od, "redfin_event.rds"))
cat("\n--- RedfinNow event ---\n"); print(rf)

bs <- readRDS(file.path(od, "balance_stats.rds"))
cat("\n--- balance normalized diffs (T3 high vs T1 / vs zero) ---\n")
print(round(cbind(t1 = bs[["ndiff_t1"]], zero = bs[["ndiff_zero"]]), 3))

cal <- readRDS(file.path(od, "calibration.rds"))
cat("\n--- calibration ---\n")
cat("sigma_a Zillow:", round(cal[["sigma_a_zillow"]], 4),
    "| base_sd:", round(cal[["base_sd"]], 4), "| base_iqr:", round(cal[["base_iqr"]], 4), "\n")
cat("IQR 95% ceiling (log pts):", round(cal[["ub_iqr_logpts"]], 3),
    "(", round(cal[["ub_iqr_pct"]], 2), "% of baseline IQR)\n")
cat("rho k6 purchases:", round(cal[["rho_k6_purchases"]], 4),
    "| footprint:", round(cal[["rho_k6_footprint"]], 4), "\n")
cat("max predicted effect (log pts):", round(cal[["max_pred_logpts"]], 4),
    "| cells data would reject:", sum(cal[["grid"]][["rejected"]]), "of", nrow(cal[["grid"]]), "\n")
