# File: 059_tukey_europe_vs_other_diff107_ci038_176_p01.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "exceeded other regions by 1.07 points (95% CI [0.38, 1.76], p<.01)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

geo_risk_data <- na.omit(data.frame(
  region  = factor(data$Q2.2),
  reg_risk = suppressWarnings(as.numeric(data$Q3.6_3))
))
aov_geo <- aov(reg_risk ~ region, data = geo_risk_data)
print(TukeyHSD(aov_geo)$region["2-4", ])
