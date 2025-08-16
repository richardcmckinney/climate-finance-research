# File: 008_philanthropic_orgs_16_12pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "philanthropic organizations (n=16, 1.2%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
phil_count <- sum(data$Q2.1 == "6" & data$Status == "IP Address", na.rm = TRUE)
phil_pct <- (phil_count / nrow(data_clean)) * 100
cat("Philanthropic count:", phil_count, " Percent:", round(phil_pct,2), "\n")
