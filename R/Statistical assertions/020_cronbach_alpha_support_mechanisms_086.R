# ===============================
# File: 020_cronbach_alpha_support_mechanisms_086.R
# Purpose: Cronbach’s alpha for support mechanisms (Q3.9_*)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
items <- data[, grep("^Q3\\.9_", names(data))]
num <- as.data.frame(lapply(items, as.numeric))
print(psych::alpha(na.omit(num)))
