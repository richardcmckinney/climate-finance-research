# File: 043_diff_vc_gov_13pct_points.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "13 percentage point difference (86% vs. 73%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

diff_vc_gov <- 86 - 73
cat("Difference in percentage points:", diff_vc_gov, "\n")
