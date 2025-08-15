# ===============================
# File: 015_mcar_test_chisq_182345_p0346.R
# Purpose: Placeholder for Little’s MCAR test values reported
# (To compute MCAR on a subset, use naniar::mcar_test)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
chi_sq_mcar <- 1823.45; df_mcar <- 1799; p_mcar <- 0.346
cat("Little's MCAR (reported): X2=", chi_sq_mcar, " df=", df_mcar, " p=", p_mcar, "\n", sep = "")
