# File: 036_corr_tech_market_r0342_p001_ci261_419.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "technology risks showed significant positive correlation with market risks (r=.342, p<.001, 95% CI [.261, .419])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

tech_risk   <- suppressWarnings(as.numeric(data$Q3.6_1))
market_risk <- suppressWarnings(as.numeric(data$Q3.6_2))
print(cor.test(tech_risk, market_risk, method = "pearson"))