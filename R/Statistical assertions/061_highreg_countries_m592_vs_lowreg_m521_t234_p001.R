# File: 061_highreg_countries_m592_vs_lowreg_m521_t234_p001.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Germany, Denmark, Netherlands M=5.92 vs less regulated M=5.21, t(234)=3.45, p<.001"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

high_reg_countries <- c("Germany","Denmark","Netherlands")
high_reg <- subset(data, hq_country %in% high_reg_countries)
low_reg  <- subset(data, !(hq_country %in% high_reg_countries) & Q2.2 == "2")
print(t.test(suppressWarnings(as.numeric(high_reg$Q3.6_3)),
             suppressWarnings(as.numeric(low_reg$Q3.6_3))))
