# ===============================
# File: 078_loading_policy0794_regulatory0781.R
# Purpose: Loadings for policy/reg items on Factor2 (rows 3:4)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
risk <- as.data.frame(lapply(data[, grep("^Q12\\.13_", names(data))], as.numeric)); risk <- na.omit(risk)
pca <- principal(risk, nfactors=3, rotate="varimax")
print(pca$loadings[3:4, 2, drop=FALSE])
