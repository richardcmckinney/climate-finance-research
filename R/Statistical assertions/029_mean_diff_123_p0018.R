# File: 029_mean_diff_123_p0018.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "mean difference=1.23, p=.018"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
tech_risk_data <- na.omit(data.frame(
  stakeholder = factor(data$Q2.1),
  tech_risk = suppressWarnings(as.numeric(data$Q3.6_1))
))
aov_result <- aov(tech_risk ~ stakeholder, data = tech_risk_data)
print(TukeyHSD(aov_result)$stakeholder["1-2", ])
