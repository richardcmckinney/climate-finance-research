# File: 063_esg_mean_376_sd131.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "ESG investors scored M=3.76 (SD=1.31)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

esg_impact <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "8"]))
cat("Mean:", round(mean(esg_impact, na.rm=TRUE),2), " SD:", round(sd(esg_impact, na.rm=TRUE),2), "\n")
