# Pairwise meta-analysis: norepinephrine vs control
# This script defines data inline, runs a random-effects REML meta-analysis
# (mean difference), produces a forest plot, draws a DOI plot, saves the DOI
# plot as PNG, and prints the LFK index with interpretation.

if (!requireNamespace("meta", quietly = TRUE)) {
  stop("Package 'meta' is required. Install it with install.packages('meta').")
}

library(meta)

# 1) Define study data inline (hardcoded; no file reading)
nor_epi_data <- data.frame(
  study = c(
    "Nakamoto 2019",
    "Chen 2023",
    "Fang 2025",
    "Gao 2024",
    "Metry 2019",
    "Trocheris 2025",
    "Wuethrich 2014"
  ),
  mdnora = c(1.66, 0.24, 0.22, 0.26, 0.38, 0.57, 0.36),
  sdnora = c(1.16, 0.66, 2.60, 0.12, 0.18, 1.22, 0.62),
  nnora = c(17, 60, 50, 61, 37, 235, 83),
  mdcontrol = c(1.95, 0.49, 0.65, 0.31, 0.59, 0.84, 0.28),
  sdcontrol = c(1.11, 0.74, 2.38, 0.11, 0.24, 1.29, 0.39),
  ncontrol = c(17, 60, 50, 123, 35, 238, 83),
  stringsAsFactors = FALSE
)

# 2) Run pairwise random-effects meta-analysis (REML), effect = Mean Difference
nor_epi_meta <- metacont(
  n.e = nnora,
  mean.e = mdnora,
  sd.e = sdnora,
  n.c = ncontrol,
  mean.c = mdcontrol,
  sd.c = sdcontrol,
  studlab = study,
  data = nor_epi_data,
  sm = "MD",
  random = TRUE,
  fixed = FALSE,
  method.tau = "REML",
  title = "Norepinephrine vs Control"
)

# Print summary table to console
print(summary(nor_epi_meta))

# 3) Forest plot
forest(
  nor_epi_meta,
  prediction = TRUE,
  print.tau2 = TRUE,
  xlab = "Mean Difference (Norepinephrine - Control)",
  main = "Forest plot: Norepinephrine vs Control"
)

# 4) DOI plot and LFK index
doi_result <- doiplot(
  nor_epi_meta,
  xlab = "Mean Difference (Norepinephrine - Control)",
  main = "DOI plot: Norepinephrine vs Control"
)

# Extract LFK index value from doiplot output and interpret it
lfk_index <- NA_real_
if (is.list(doi_result)) {
  if ("LFK" %in% names(doi_result)) {
    lfk_index <- as.numeric(doi_result$LFK)
  } else if ("lfk" %in% names(doi_result)) {
    lfk_index <- as.numeric(doi_result$lfk)
  }
} else if (is.numeric(doi_result) && length(doi_result) == 1) {
  lfk_index <- as.numeric(doi_result)
}

if (is.na(lfk_index)) {
  cat("\nLFK index: not available from doiplot() output.\n")
} else {
  lfk_abs <- abs(lfk_index)
  lfk_interpretation <- if (lfk_abs <= 1) {
    "No asymmetry (|LFK| <= 1)"
  } else if (lfk_abs <= 2) {
    "Minor asymmetry (1 < |LFK| <= 2)"
  } else {
    "Major asymmetry (|LFK| > 2)"
  }

  cat("\nLFK index:", round(lfk_index, 3), "\n")
  cat("Interpretation:", lfk_interpretation, "\n")
}

# 5) Save DOI plot as PNG
png(
  filename = "DOI_plot_nor_epi.png",
  width = 900,
  height = 700,
  res = 120
)
doiplot(
  nor_epi_meta,
  xlab = "Mean Difference (Norepinephrine - Control)",
  main = "DOI plot: Norepinephrine vs Control"
)
dev.off()

cat("\nSaved DOI plot to DOI_plot_nor_epi.png\n")
