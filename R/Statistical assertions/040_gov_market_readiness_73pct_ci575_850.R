# ===============================
# File: 040_gov_market_readiness_73pct_ci575_850.R
# Purpose: Government-only market readiness (Q4.7 text)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
gov <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="2")
k <- sum(grepl("market", gov$Q4.7, ignore.case=TRUE), na.rm=TRUE)
n <- sum(!is.na(gov$Q4.7))
print(prop.test(k, n, conf.level = 0.95))
