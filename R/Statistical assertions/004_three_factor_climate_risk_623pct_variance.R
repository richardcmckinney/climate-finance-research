# File: 004_three_factor_climate_risk_623pct_variance.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Three-Factor Climate Risk Model... (62.3% variance)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

suppressWarnings(suppressMessages({ if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' required.") }))
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)

risk_cols <- grep("^Q12\\.13_", names(data), value = TRUE)[1:10]
risk_items <- data[, risk_cols, drop = FALSE]
risk_items_clean <- na.omit(apply(risk_items, 2, function(x) suppressWarnings(as.numeric(x))))
pca_result <- psych::principal(risk_items_clean, nfactors = 3, rotate = "varimax")
variance_explained <- sum(pca_result$values[1:3]) / sum(pca_result$values) * 100
cat("Variance explained (3-factor):", round(variance_explained, 2), "%\n")
