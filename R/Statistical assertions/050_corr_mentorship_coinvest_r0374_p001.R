# File: 050_corr_mentorship_coinvest_r0374_p001.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "mentorship correlated with co-investment interest (r=.374, p<.001)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

mentorship <- as.numeric(grepl("2", data$Q3.10))
coinvest   <- suppressWarnings(as.numeric(data$Q3.8_12))
print(cor.test(mentorship, coinvest))
