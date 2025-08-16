# File: 010_high_networth_individuals_78_60pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "high net-worth individuals (n=78, 6.0%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
hnwi_count <- sum(data$Q2.1 == "5" & data$Status == "IP Address", na.rm = TRUE)
hnwi_pct <- (hnwi_count / nrow(data_clean)) * 100
cat("HNWI count:", hnwi_count, " Percent:", round(hnwi_pct,2), "\n")
