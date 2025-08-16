# File: 042_chisq_7df_4523_p001_cramersv0186.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "χ²(7)=45.23, p<.001, Cramér's V=.186, small-to-medium effect"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("vcd", quietly = TRUE)) stop("Package 'vcd' required.")
market_by_group <- table(data$Q2.1, grepl("market", data$Q3.11, ignore.case = TRUE))
print(chisq.test(market_by_group))
print(vcd::assocstats(market_by_group))
