# File: 064_vc_mean_531_sd119.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "venture capitalists scored M=5.31 (SD=1.19)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

vc_impact <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "1"]))
cat("Mean:", round(mean(vc_impact, na.rm=TRUE),2), " SD:", round(sd(vc_impact, na.rm=TRUE),2), "\n")
