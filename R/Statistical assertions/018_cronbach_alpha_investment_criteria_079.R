# ===============================
# File: 018_cronbach_alpha_investment_criteria_079.R
# Purpose: Cronbach’s alpha for investment criteria (all Q3.8_*)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
items <- data[, grep("^Q3\\.8_", names(data))]
num <- as.data.frame(lapply(items, as.numeric))
print(psych::alpha(na.omit(num)))
