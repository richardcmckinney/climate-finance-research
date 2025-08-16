# File: 067_mean_diff_155_p001_d121.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "mean difference=1.55, p<.001, d=1.21"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("effectsize", quietly=TRUE)) stop("effectsize required")
impact_data <- na.omit(data.frame(
  stakeholder = factor(data$Q2.1),
  impact      = suppressWarnings(as.numeric(data$Q3.3))
))
aov_impact <- aov(impact ~ stakeholder, data = impact_data)
print(TukeyHSD(aov_impact)$stakeholder["1-8", ])
vc_impact  <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "1"]))
esg_impact <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "8"]))
print(effectsize::cohens_d(vc_impact, esg_impact, pooled_sd = TRUE))
