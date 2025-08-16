# File: 038_overall_market_readiness_81pct_ci789_830.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Overall, 81% (95% CI [78.9%, 83.0%]) identified market readiness"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

market_count <- sum(grepl("market", data$Q3.11, ignore.case = TRUE), na.rm = TRUE)
total_responses <- sum(!is.na(data$Q3.11))
print(prop.test(market_count, total_responses, conf.level = 0.95))
