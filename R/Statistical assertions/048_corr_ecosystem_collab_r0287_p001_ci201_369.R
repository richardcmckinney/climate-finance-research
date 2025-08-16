# File: 048_corr_ecosystem_collab_r0287_p001_ci201_369.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "r=.287, p<.001, 95% CI [.201, .369], small-to-medium effect"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

ecosystem_support <- suppressWarnings(as.numeric(data$Q3.10))
collab_preference <- suppressWarnings(as.numeric(data$Q3.8_10))
print(cor.test(ecosystem_support, collab_preference))
