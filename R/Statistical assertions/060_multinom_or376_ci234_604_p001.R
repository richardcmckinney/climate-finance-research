# File: 060_multinom_or376_ci234_604_p001.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "OR=3.76, 95% CI [2.34, 6.04], p<.001"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("nnet", quietly = TRUE)) stop("Package 'nnet' required.")
cat("This analysis requires a data.frame 'clean_data' with columns: reg_barrier, region, stakeholder, experience.\n")
# Example (uncomment and adapt when clean_data is available):
# multi_model <- nnet::multinom(reg_barrier ~ region + stakeholder + experience, data = clean_data)
# print(exp(coef(multi_model)))
# print(confint(multi_model))
