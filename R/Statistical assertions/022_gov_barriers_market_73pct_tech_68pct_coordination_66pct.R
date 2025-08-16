# File: 022_gov_barriers_market_73pct_tech_68pct_coordination_66pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Government Agencies: Market Readiness (73%), Technology Risk (68%), Coordination Challenges (66%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
gov_data <- subset(data, Q2.1 == "2")
gov_market <- sum(grepl("1", gov_data$Q4.7), na.rm = TRUE) / nrow(gov_data) * 100
gov_tech   <- sum(grepl("2", gov_data$Q4.7), na.rm = TRUE) / nrow(gov_data) * 100
gov_coord  <- sum(grepl("3", gov_data$Q4.7), na.rm = TRUE) / nrow(gov_data) * 100
cat("Gov market:", round(gov_market,2), " tech:", round(gov_tech,2), " coord:", round(gov_coord,2), "\n")
