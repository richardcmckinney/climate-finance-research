# File: 031_vc_mean_584_sd121.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Table 1: Venture Capitalists Mean 5.84 (SD=1.21)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
vc_tech_scores <- suppressWarnings(as.numeric(data$Q3.6_1[data$Q2.1 == "1"]))
cat("Mean:", round(mean(vc_tech_scores, na.rm=TRUE),2), " SD:", round(sd(vc_tech_scores, na.rm=TRUE),2), "\n")
