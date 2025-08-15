# ===============================
# File: 019_cronbach_alpha_climate_risk_091.R
# Purpose: Cronbach’s alpha for climate risk (Q12.13_*)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
items <- data[, grep("^Q12\\.13_", names(data))]
num <- as.data.frame(lapply(items, as.numeric))
print(psych::alpha(na.omit(num)))
