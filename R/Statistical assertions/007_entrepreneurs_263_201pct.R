# File: 007_entrepreneurs_263_201pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "entrepreneurs (n=263, 20.1%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
ent_count <- sum(data$Q2.1 == "3" & data$Status == "IP Address", na.rm = TRUE)
ent_pct <- (ent_count / nrow(data_clean)) * 100
cat("Entrepreneurs count:", ent_count, " Percent:", round(ent_pct,2), "\n")
