# File: 020_cronbach_alpha_support_mechanisms_086.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Support mechanism effectiveness (α = .86)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' required.")
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
support_items <- data[, grep("^Q3\\.9_", names(data), value = TRUE), drop = FALSE]
support_numeric <- apply(support_items, 2, function(x) suppressWarnings(as.numeric(x)))
print(psych::alpha(na.omit(support_numeric)))
