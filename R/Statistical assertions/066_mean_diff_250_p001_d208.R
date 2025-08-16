# File: 066_mean_diff_250_p001_d208.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "mean difference=2.50, p<.001, d=2.08"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("effectsize", quietly=TRUE)) stop("effectsize required")
impact_data <- na.omit(data.frame(
  stakeholder = factor(data$Q2.1),
  impact      = suppressWarnings(as.numeric(data$Q3.3))
))
aov_impact <- aov(impact ~ stakeholder, data = impact_data)
print(TukeyHSD(aov_impact)$stakeholder["1-6", ])
vc_impact  <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "1"]))
phil_impact <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "6"]))
print(effectsize::cohens_d(vc_impact, phil_impact, pooled_sd = TRUE))
