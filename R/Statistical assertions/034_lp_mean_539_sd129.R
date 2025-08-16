# File: 034_lp_mean_539_sd129.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Limited Partners Mean 5.39 (SD=1.29)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
lp_tech_scores <- suppressWarnings(as.numeric(data$Q7.8_4[data$Q2.1 == "4"]))
cat("Mean:", round(mean(lp_tech_scores, na.rm=TRUE),2), " SD:", round(sd(lp_tech_scores, na.rm=TRUE),2), "\n")
