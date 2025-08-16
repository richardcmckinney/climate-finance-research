# File: 027_chisq_473_p0030_phi0174.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "χ²(1)=4.73, p=.030, φ=.174, small-to-medium effect"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

cont_table <- matrix(c(59, 54, 12, 26), nrow = 2)
chi_result <- chisq.test(cont_table)
if (!requireNamespace("vcd", quietly = TRUE)) stop("Package 'vcd' required for assocstats.")
print(chi_result)
print(vcd::assocstats(cont_table))
