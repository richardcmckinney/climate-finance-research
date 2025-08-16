# File: 012_esg_investors_42_32pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "ESG investors (n=42, 3.2%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
esg_count <- sum(data$Q2.1 == "8" & data$Status == "IP Address", na.rm = TRUE)
esg_pct <- (esg_count / nrow(data_clean)) * 100
cat("ESG count:", esg_count, " Percent:", round(esg_pct,2), "\n")
