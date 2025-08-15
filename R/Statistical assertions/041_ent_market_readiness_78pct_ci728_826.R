# ===============================
# File: 041_ent_market_readiness_78pct_ci728_826.R
# Purpose: Entrepreneur market readiness (Q5.11 text)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
ent <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="3")
k <- sum(grepl("market", ent$Q5.11, ignore.case=TRUE), na.rm=TRUE)
n <- sum(!is.na(ent$Q5.11))
print(prop.test(k, n, conf.level = 0.95))
