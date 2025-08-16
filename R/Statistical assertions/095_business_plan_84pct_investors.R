# ===============================
# File: 095_business_plan_84pct_investors.R
# Purpose: % of investors (VC, Gov, Family Office, ESG) providing business plan dev (Q3.10 '7')
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
inv <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1 %in% c("1","2","7","8"))
k <- sum(grepl("\\b7\\b", inv$Q3.10), na.rm=TRUE); n <- sum(!is.na(inv$Q3.10))
cat("Business plan support (%):", round(100*k/n,1), "\n")
