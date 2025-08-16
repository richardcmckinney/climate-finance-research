# File: 005_vc_firms_113_86pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "venture capital firms (n=113, 8.6%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
if (!("Status" %in% names(data))) stop("Column 'Status' not found.")
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)
vc_count <- sum(data$Q2.1 == "1" & data$Status == "IP Address", na.rm = TRUE)
vc_pct <- (vc_count / nrow(data_clean)) * 100
cat("VC count:", vc_count, " Percent:", round(vc_pct,2), "\n")
