od <- "projects/04_algorithmic_intermediation/scripts/R/_outputs"
xr <- readRDS(file.path(od, "exit_robustness.rds"))
zc <- function(tt) {
  r <- tt[grepl("^z(share|hi)(95)?:post$", tt[["term"]]), ]
  round(r[["estimate"]][1] * 100, 2)   # log points
}
for (nm in c("lvol", "disp")) {
  v <- xr[[nm]]
  cat(nm, ": dropliq", zc(v$drop_liq), "binary", zc(v$binary),
      "zipclust", zc(v$zip_clust), "weighted", zc(v$weighted),
      "cell10", zc(v$cellmin10), "nophx", zc(v$no_phoenix),
      "wins95", zc(v$winsor95), "\n")
}
er <- readRDS(file.path(od, "entry_robustness.rds"))
att <- function(tt) {
  r <- tt[grepl("att|ATT", tt[["term"]], ignore.case = TRUE), ]
  if (nrow(r) == 0) r <- tt[1, ]
  round(r[["estimate"]][1] * 100, 2)
}
ase <- function(tt) {
  r <- tt[grepl("att|ATT", tt[["term"]], ignore.case = TRUE), ]
  if (nrow(r) == 0) r <- tt[1, ]
  round(r[["std.error"]][1] * 100, 2)
}
cat("entry iqr antic", att(er$iqr$antic), "precovid", att(er$iqr$precovid),
    "se", ase(er$iqr$precovid), "\n")
