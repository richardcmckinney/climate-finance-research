# File: 037_corr_by_group_vc_r0384_gov_r0298.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "correlation was consistent across stakeholder groups, ranging from r=.298 (government) to r=.384 (VCs)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

gov_data <- subset(data, Q2.1 == "2")
print(cor(suppressWarnings(as.numeric(gov_data$Q4.5_7)),
          suppressWarnings(as.numeric(gov_data$Q4.5_8)), use="complete.obs"))

vc_data <- subset(data, Q2.1 == "1")
print(cor(suppressWarnings(as.numeric(vc_data$Q3.6_1)),
          suppressWarnings(as.numeric(vc_data$Q3.6_2)), use="complete.obs"))
