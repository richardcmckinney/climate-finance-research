# File: 065_anova_f7498_1892_p001_eta0210.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "F(7,498)=18.92, p<.001, η²=.210, large effect"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("effectsize", quietly=TRUE)) stop("effectsize required")
impact_data <- na.omit(data.frame(
  stakeholder = factor(data$Q2.1),
  impact      = suppressWarnings(as.numeric(data$Q3.3))
))
aov_impact <- aov(impact ~ stakeholder, data = impact_data)
print(summary(aov_impact))
print(effectsize::eta_squared(aov_impact))
