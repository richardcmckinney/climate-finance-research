# ===============================
# File: 033_ent_mean_512_sd144.R
# Purpose: Descriptives for Entrepreneur tech risk (Q5.8_6)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
x <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="3")$Q5.8_6)
cat("Mean:", round(mean(x, na.rm=TRUE),2), " SD:", round(sd(x, na.rm=TRUE),2), "\n")
