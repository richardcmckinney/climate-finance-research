# File: 055_chisq_784_p0005_phi0089.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "χ²(1)=7.84, p=.005, φ=.089"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

geo_reg_table <- matrix(c(359, 59, 347, 98), nrow = 2)
print(chisq.test(geo_reg_table))
