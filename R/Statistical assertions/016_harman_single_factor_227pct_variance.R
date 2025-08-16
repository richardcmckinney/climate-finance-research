# File: 016_harman_single_factor_227pct_variance.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Harman's single-factor test... first factor explaining 22.7% of variance"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' required.")
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
all_items <- data[, grep("^(Q3\\.|Q4\\.|Q5\\.)", names(data), perl = TRUE), drop = FALSE]
all_items_numeric <- apply(all_items, 2, function(x) suppressWarnings(as.numeric(x)))
fa_single <- psych::fa(na.omit(all_items_numeric), nfactors = 1, rotate = "none")
first_factor_var <- fa_single$values[1] / sum(fa_single$values) * 100
cat("First factor variance %:", round(first_factor_var, 2), "\n")
