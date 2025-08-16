# ===============================
# File: 081_bartlett_chisq45_234167_p001.R
# Purpose: Bartlett’s test of sphericity
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
risk <- as.data.frame(lapply(data[, grep("^Q12\\.13_", names(data))], as.numeric)); risk <- na.omit(risk)
print(cortest.bartlett(cor(risk), n = nrow(risk)))
