# ===============================
# File: 079_loading_market0723_financial0698.R
# Purpose: Loadings for market/financial items on Factor3 (rows 5:6)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
risk <- as.data.frame(lapply(data[, grep("^Q12\\.13_", names(data))], as.numeric)); risk <- na.omit(risk)
pca <- principal(risk, nfactors=3, rotate="varimax")
print(pca$loadings[5:6, 3, drop=FALSE])
