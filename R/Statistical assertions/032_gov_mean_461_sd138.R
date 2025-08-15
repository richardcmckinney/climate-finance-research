# ===============================
# File: 032_gov_mean_461_sd138.R
# Purpose: Descriptives for Gov tech risk (Q4.5_7)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
x <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="2")$Q4.5_7)
cat("Mean:", round(mean(x, na.rm=TRUE),2), " SD:", round(sd(x, na.rm=TRUE),2), "\n")
