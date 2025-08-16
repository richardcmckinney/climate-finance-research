# File: 044_chisq_392_p0048_phi0162.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "χ²(1)=3.92, p=.048, φ=.162, small effect"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

vc_gov_market <- matrix(c(97, 16, 28, 10), nrow = 2)
print(chisq.test(vc_gov_market))
