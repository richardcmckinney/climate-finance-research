# File: 051_corr_strategic_syndication_r0322_p001.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "strategic counsel correlated with syndication preferences (r=.322, p<.001)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

strategic   <- as.numeric(grepl("3", data$Q3.10))
syndication <- suppressWarnings(as.numeric(data$Q3.8_13))
print(cor.test(strategic, syndication))
