# File: 046_regression_beta043_se012_p001.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "β=0.43, SE=0.12, p<.001"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

model_data <- data.frame(
  investment_likely = suppressWarnings(as.numeric(data$Q3.8_8)),
  international     = suppressWarnings(as.numeric(data$Q3.7))
)
model <- glm(investment_likely ~ international, data = model_data, family = gaussian())
print(summary(model))
