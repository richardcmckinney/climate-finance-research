# File: 052_europe_regulatory_86pct_ci817_895.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "86% (95% CI [81.7%, 89.5%]) identifying regulatory impediments"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

europe_data <- subset(data, Q2.2 == "2")
europe_reg  <- sum(grepl("regulatory", europe_data$Q3.11, ignore.case = TRUE), na.rm = TRUE)
europe_total <- sum(!is.na(europe_data$Q3.11))
print(prop.test(europe_reg, europe_total, conf.level = 0.95))
