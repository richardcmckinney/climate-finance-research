# File: 019_cronbach_alpha_climate_risk_091.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Climate risk assessment (α = .91)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' required.")
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
risk_items <- data[, grep("^Q12\\.13_", names(data), value = TRUE), drop = FALSE]
risk_numeric <- apply(risk_items, 2, function(x) suppressWarnings(as.numeric(x)))
print(psych::alpha(na.omit(risk_numeric)))
