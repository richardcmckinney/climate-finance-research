# File: 032_gov_mean_461_sd138.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Government Agencies Mean 4.61 (SD=1.38)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
gov_tech_scores <- suppressWarnings(as.numeric(data$Q4.5_7[data$Q2.1 == "2"]))
cat("Mean:", round(mean(gov_tech_scores, na.rm=TRUE),2), " SD:", round(sd(gov_tech_scores, na.rm=TRUE),2), "\n")
