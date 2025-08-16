# File: 011_family_offices_46_35pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "family offices (n=46, 3.5%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
fo_count <- sum(data$Q2.1 == "7" & data$Status == "IP Address", na.rm = TRUE)
fo_pct <- (fo_count / nrow(data_clean)) * 100
cat("Family offices count:", fo_count, " Percent:", round(fo_pct,2), "\n")
