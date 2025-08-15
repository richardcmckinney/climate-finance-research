# ===============================
# File: 004_three_factor_climate_risk_623pct_variance.R
# Purpose: PCA on Q12.13_* items → 3 factors; total variance explained
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
risk_items <- data[, grep("^Q12\\.13_", names(data))]
risk_num <- as.data.frame(lapply(risk_items, function(x) as.numeric(x)))
risk_num <- na.omit(risk_num)
pca_result <- principal(risk_num, nfactors = 3, rotate = "varimax")
variance_explained <- 100 * sum(pca_result$values[1:3]) / sum(pca_result$values)
cat("Variance explained by 3 factors (%):", round(variance_explained, 1), "\n")
