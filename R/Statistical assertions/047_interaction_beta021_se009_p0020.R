# File: 047_interaction_beta021_se009_p0020.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "interaction term: β=0.21, SE=0.09, p=.020"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

model_data <- data.frame(
  investment_likely = suppressWarnings(as.numeric(data$Q3.8_8)),
  international     = suppressWarnings(as.numeric(data$Q3.7)),
  is_vc             = as.numeric(data$Q2.1 == "1")
)
model_int <- glm(investment_likely ~ international * is_vc, data = model_data, family = gaussian())
print(summary(model_int))
