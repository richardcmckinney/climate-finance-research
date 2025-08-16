# File: 054_na_regulatory_78pct_ci732_823.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "78% (95% CI [73.2%, 82.3%])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

na_data <- subset(data, Q2.2 == "1")
na_reg  <- sum(grepl("regulatory", na_data$Q3.11, ignore.case = TRUE), na.rm = TRUE)
na_total <- sum(!is.na(na_data$Q3.11))
print(prop.test(na_reg, na_total, conf.level = 0.95))
