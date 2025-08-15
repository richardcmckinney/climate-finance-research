# ===============================
# File: 039_vc_market_readiness_86pct_ci789_915.R
# Purpose: VC-only market readiness (Q3.11 text)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="1")
k <- sum(grepl("market", vc$Q3.11, ignore.case=TRUE), na.rm=TRUE)
n <- sum(!is.na(vc$Q3.11))
print(prop.test(k, n, conf.level = 0.95))
