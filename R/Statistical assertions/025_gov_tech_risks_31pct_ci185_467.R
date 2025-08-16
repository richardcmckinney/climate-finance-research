# File: 025_gov_tech_risks_31pct_ci185_467.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "31% of government agencies (95% CI [18.5%, 46.7%])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
gov_data <- subset(data, Q2.1 == "2")
gov_tech_critical <- sum(suppressWarnings(as.numeric(gov_data$Q4.5_7)) >= 6, na.rm = TRUE)
print(prop.test(gov_tech_critical, nrow(gov_data), conf.level = 0.95))
