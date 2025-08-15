# ===============================
# File: 017_cronbach_alpha_barrier_perceptions_084.R
# Purpose: Cronbach’s alpha for barrier perceptions composite (Q3.6_1..Q3.6_5)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
items <- data[, c("Q3.6_1","Q3.6_2","Q3.6_3","Q3.6_4","Q3.6_5")]
num <- as.data.frame(lapply(items, as.numeric))
print(psych::alpha(na.omit(num)))
