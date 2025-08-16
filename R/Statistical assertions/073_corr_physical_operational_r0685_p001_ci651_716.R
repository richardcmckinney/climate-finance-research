# File: 073_corr_physical_operational_r0685_p001_ci651_716.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "physical climate risks and operational risks (r=.685, p<.001, 95% CI [.651, .716])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

physical_risk    <- suppressWarnings(as.numeric(data$Q12.13_1))
operational_risk <- suppressWarnings(as.numeric(data$Q12.13_2))
print(cor.test(physical_risk, operational_risk))
