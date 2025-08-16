# File: 053_na_cost_competitiveness_89pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "89% citation rate"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

na_data   <- subset(data, Q2.2 == "1")
na_cost   <- sum(grepl("cost", na_data$Q3.11, ignore.case = TRUE), na.rm = TRUE)
na_total  <- sum(!is.na(na_data$Q3.11))
na_cost_pct <- (na_cost / na_total) * 100
cat("NA cost-competitiveness %:", round(na_cost_pct,2), "\n")
