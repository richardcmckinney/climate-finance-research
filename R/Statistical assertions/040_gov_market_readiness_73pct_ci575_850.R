# File: 040_gov_market_readiness_73pct_ci575_850.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "government agencies (73%, 95% CI [57.5%, 85.0%])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

gov_market_count <- sum(grepl("market", data$Q4.7[data$Q2.1 == "2"], ignore.case = TRUE), na.rm = TRUE)
gov_total <- sum(data$Q2.1 == "2" & !is.na(data$Q4.7))
print(prop.test(gov_market_count, gov_total, conf.level = 0.95))
