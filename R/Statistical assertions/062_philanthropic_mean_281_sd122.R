# File: 062_philanthropic_mean_281_sd122.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "philanthropic organizations scored M=2.81 (SD=1.22)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

phil_impact <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "6"]))
cat("Mean:", round(mean(phil_impact, na.rm=TRUE),2), " SD:", round(sd(phil_impact, na.rm=TRUE),2), "\n")
