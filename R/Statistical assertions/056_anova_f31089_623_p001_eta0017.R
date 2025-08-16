# File: 056_anova_f31089_623_p001_eta0017.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "F(3,1089)=6.23, p<.001, η²=.017"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("effectsize", quietly=TRUE)) stop("effectsize required")
geo_risk_data <- na.omit(data.frame(
  region  = factor(data$Q2.2),
  reg_risk = suppressWarnings(as.numeric(data$Q3.6_3))
))
aov_geo <- aov(reg_risk ~ region, data = geo_risk_data)
print(summary(aov_geo))
print(effectsize::eta_squared(aov_geo))
