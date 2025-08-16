# File: 074_factor1_physical_operational_eig347_var248pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Factor 1: Physical-Operational (Eigenvalue: 3.47, 24.8% variance)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("psych", quietly=TRUE)) stop("psych required")
risk_cols   <- grep("^Q12\\.13_", names(data), value = TRUE)[1:10]
risk_numeric <- apply(data[, risk_cols, drop=FALSE], 2, function(x) suppressWarnings(as.numeric(x)))
pca <- psych::principal(na.omit(risk_numeric), nfactors = 3, rotate = "varimax")
cat("Eigen 1:", round(pca$values[1],2), " Var%:", round(pca$Vaccounted[1,1]*100,2), "\n")
