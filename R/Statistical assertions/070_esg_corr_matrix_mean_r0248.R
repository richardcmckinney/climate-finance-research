# File: 070_esg_corr_matrix_mean_r0248.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "ESG investors showed mean r=.248"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

esg_data <- data[data$Q2.1 == "8", c("Q10.6_8","Q10.6_9","Q10.6_10","Q10.6_11","Q10.6_12")]
esg_cor_matrix <- cor(apply(esg_data, 2, function(x) suppressWarnings(as.numeric(x))), use="complete.obs")
cat("Mean r:", round(mean(esg_cor_matrix[lower.tri(esg_cor_matrix)]),3), "\n")
