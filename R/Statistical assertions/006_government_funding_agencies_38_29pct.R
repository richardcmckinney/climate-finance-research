# File: 006_government_funding_agencies_38_29pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "government funding agencies (n=38, 2.9%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
gov_count <- sum(data$Q2.1 == "2" & data$Status == "IP Address", na.rm = TRUE)
gov_pct <- (gov_count / nrow(data_clean)) * 100
cat("Government count:", gov_count, " Percent:", round(gov_pct,2), "\n")
