# File: 049_corr_networking_partnership_r0449_p001.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Networking support correlated with partnership preferences (r=.449, p<.001)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

networking  <- as.numeric(grepl("1", data$Q3.10))
partnership <- suppressWarnings(as.numeric(data$Q3.8_11))
print(cor.test(networking, partnership))
