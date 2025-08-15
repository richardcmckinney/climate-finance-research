# ===============================
# File: 031_vc_mean_584_sd121.R
# Purpose: Descriptives for VC tech risk (Q3.6_1)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
x <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="1")$Q3.6_1)
cat("Mean:", round(mean(x, na.rm=TRUE),2), " SD:", round(sd(x, na.rm=TRUE),2), "\n")
