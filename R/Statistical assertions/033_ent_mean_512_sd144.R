# File: 033_ent_mean_512_sd144.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Entrepreneurs Mean 5.12 (SD=1.44)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
ent_tech_scores <- suppressWarnings(as.numeric(data$Q5.8_6[data$Q2.1 == "3"]))
cat("Mean:", round(mean(ent_tech_scores, na.rm=TRUE),2), " SD:", round(sd(ent_tech_scores, na.rm=TRUE),2), "\n")
