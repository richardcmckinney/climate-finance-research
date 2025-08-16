# ===============================
# File: 082_total_variance_explained_623pct.R
# Purpose: Total variance explained by first 3 PCA components (%)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
risk <- as.data.frame(lapply(data[, grep("^Q12\\.13_", names(data))], as.numeric)); risk <- na.omit(risk)
pca <- principal(risk, nfactors=3, rotate="varimax")
var_pct <- 100 * sum(pca$values[1:3]) / sum(pca$values)
cat("Total variance (3 comps) %:", round(var_pct,1), "\n")
