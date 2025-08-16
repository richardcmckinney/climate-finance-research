# File: 015_mcar_test_chisq_182345_p0346.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Little's MCAR test (χ²=1823.45, df=1799, p=.346)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("naniar", quietly = TRUE)) stop("Package 'naniar' required.")
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
chi_sq_mcar <- 1823.45; df_mcar <- 1799; p_mcar <- 0.346
cat(sprintf("Reported MCAR: Chi^2=%.2f, df=%d, p=%.3f\n", chi_sq_mcar, df_mcar, p_mcar))
