# File: 026_ent_tech_risks_38pct_ci324_442.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "38% of entrepreneurs (95% CI [32.4%, 44.2%])"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
ent_data <- subset(data, Q2.1 == "3")
ent_tech_critical <- sum(suppressWarnings(as.numeric(ent_data$Q5.8_6)) >= 6, na.rm = TRUE)
print(prop.test(ent_tech_critical, nrow(ent_data), conf.level = 0.95))
