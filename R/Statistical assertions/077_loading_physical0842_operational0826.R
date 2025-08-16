# ===============================
# File: 077_loading_physical0842_operational0826.R
# Purpose: Loadings for two items on Factor1 (illustrative: rows 1:2)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
risk <- as.data.frame(lapply(data[, grep("^Q12\\.13_", names(data))], as.numeric)); risk <- na.omit(risk)
pca <- principal(risk, nfactors=3, rotate="varimax")
print(pca$loadings[1:2, 1, drop=FALSE])
