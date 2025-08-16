# File: 013_other_unspecified_657_503pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "other/unspecified (n=657, 50.3%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
other_count <- sum(data$Q2.1 == "12" & data$Status == "IP Address", na.rm = TRUE)
other_pct <- (other_count / nrow(data_clean)) * 100
cat("Other/unspecified count:", other_count, " Percent:", round(other_pct,2), "\n")
