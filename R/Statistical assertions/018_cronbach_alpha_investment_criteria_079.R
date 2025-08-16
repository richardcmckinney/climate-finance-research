# File: 018_cronbach_alpha_investment_criteria_079.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Investment criteria importance (α = .79)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' required.")
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
investment_items <- data[, grep("^Q3\\.8_", names(data), value = TRUE), drop = FALSE]
investment_numeric <- apply(investment_items, 2, function(x) suppressWarnings(as.numeric(x)))
print(psych::alpha(na.omit(investment_numeric)))
