# File: 024_vc_tech_risks_52pct_ci428_611.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "52% (95% CI [42.8%, 61.1%]) rated technology risks as critical"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
vc_data <- subset(data, Q2.1 == "1")
tech_critical <- sum(suppressWarnings(as.numeric(vc_data$Q3.6_1)) >= 6, na.rm = TRUE)
print(prop.test(tech_critical, nrow(vc_data), conf.level = 0.95))
