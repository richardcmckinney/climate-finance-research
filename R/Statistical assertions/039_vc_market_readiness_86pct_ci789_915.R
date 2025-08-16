# File: 039_vc_market_readiness_86pct_ci789_915.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "VCs (86%, 95% CI [78.9%, 91.5%])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

vc_market_count <- sum(grepl("market", data$Q3.11[data$Q2.1 == "1"], ignore.case = TRUE), na.rm = TRUE)
vc_total <- sum(data$Q2.1 == "1" & !is.na(data$Q3.11))
print(prop.test(vc_market_count, vc_total, conf.level = 0.95))
