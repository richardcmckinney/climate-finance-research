# File: 028_anova_f7651_384_p001_eta0040.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "F(7,651)=3.84, p<.001, η²=.040, small effect"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("effectsize", quietly = TRUE)) stop("Package 'effectsize' required.")
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
tech_risk_data <- na.omit(data.frame(
  stakeholder = factor(data$Q2.1),
  tech_risk = suppressWarnings(as.numeric(data$Q3.6_1))
))
aov_result <- aov(tech_risk ~ stakeholder, data = tech_risk_data)
print(summary(aov_result))
print(effectsize::eta_squared(aov_result))
