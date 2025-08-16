# File: 041_ent_market_readiness_78pct_ci728_826.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "entrepreneurs (78%, 95% CI [72.8%, 82.6%])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

ent_market_count <- sum(grepl("market", data$Q5.11[data$Q2.1 == "3"], ignore.case = TRUE), na.rm = TRUE)
ent_total <- sum(data$Q2.1 == "3" & !is.na(data$Q5.11))
print(prop.test(ent_market_count, ent_total, conf.level = 0.95))
