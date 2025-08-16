# File: 071_phil_corr_matrix_mean_r0156.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "philanthropic organizations showed mean r=.156"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

phil_data <- data[data$Q2.1 == "6", c("Q6.6_9","Q6.6_10","Q6.6_11","Q6.6_12","Q6.6_13")]
phil_cor_matrix <- cor(apply(phil_data, 2, function(x) suppressWarnings(as.numeric(x))), use="complete.obs")
cat("Mean r:", round(mean(phil_cor_matrix[lower.tri(phil_cor_matrix)]),3), "\n")
