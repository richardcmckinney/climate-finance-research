# ===============================
# File: 038_overall_market_readiness_81pct_ci789_830.R
# Purpose: Overall market readiness identification from Q3.11 text
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10)
k <- sum(grepl("market", analytic$Q3.11, ignore.case=TRUE), na.rm=TRUE)
n <- sum(!is.na(analytic$Q3.11))
print(prop.test(k, n, conf.level = 0.95))
