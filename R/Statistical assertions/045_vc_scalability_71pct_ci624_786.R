# File: 045_vc_scalability_71pct_ci624_786.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "71% of venture capitalists (95% CI [62.4%, 78.6%]) rated international scalability"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

vc_international <- sum(suppressWarnings(as.numeric(data$Q3.7[data$Q2.1 == "1"])) >= 6, na.rm = TRUE)
vc_total_int <- sum(data$Q2.1 == "1" & !is.na(data$Q3.7))
print(prop.test(vc_international, vc_total_int, conf.level = 0.95))