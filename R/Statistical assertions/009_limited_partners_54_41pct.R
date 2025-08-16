# File: 009_limited_partners_54_41pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "limited partners (n=54, 4.1%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
lp_count <- sum(data$Q2.1 == "4" & data$Status == "IP Address", na.rm = TRUE)
lp_pct <- (lp_count / nrow(data_clean)) * 100
cat("LP count:", lp_count, " Percent:", round(lp_pct,2), "\n")
