# File: 069_gov_corr_matrix_mean_r0312.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "government agencies showed mean r=.312"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

gov_data <- data[data$Q2.1 == "2", c("Q4.5_7","Q4.5_8","Q4.5_9","Q4.5_10","Q4.5_11")]
gov_cor_matrix <- cor(apply(gov_data, 2, function(x) suppressWarnings(as.numeric(x))), use="complete.obs")
cat("Mean r:", round(mean(gov_cor_matrix[lower.tri(gov_cor_matrix)]),3), "\n")
