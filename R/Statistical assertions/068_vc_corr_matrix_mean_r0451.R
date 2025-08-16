# File: 068_vc_corr_matrix_mean_r0451.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "venture capitalists showed mean r=.451"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

vc_data <- data[data$Q2.1 == "1", c("Q3.6_1","Q3.6_2","Q3.6_3","Q3.6_4","Q3.6_5")]
vc_cor_matrix <- cor(apply(vc_data, 2, function(x) suppressWarnings(as.numeric(x))), use="complete.obs")
cat("Mean r:", round(mean(vc_cor_matrix[lower.tri(vc_cor_matrix)]),3), "\n")
