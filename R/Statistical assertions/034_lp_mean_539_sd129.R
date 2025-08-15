# ===============================
# File: 034_lp_mean_539_sd129.R
# Purpose: Descriptives for LP tech risk (Q7.8_4)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
x <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="4")$Q7.8_4)
cat("Mean:", round(mean(x, na.rm=TRUE),2), " SD:", round(sd(x, na.rm=TRUE),2), "\n")
