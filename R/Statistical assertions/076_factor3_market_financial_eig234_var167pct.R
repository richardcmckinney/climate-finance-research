# ===============================
# File: 076_factor3_market_financial_eig234_var167pct.R
# Purpose: PCA Factor3 summary
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
risk <- as.data.frame(lapply(data[, grep("^Q12\\.13_", names(data))], as.numeric)); risk <- na.omit(risk)
pca <- principal(risk, nfactors=3, rotate="varimax")
cat("Eigen3:", round(pca$values[3],2), " Var3(%):", round(pca$Vaccounted["Proportion Var",3]*100,1), "\n")
