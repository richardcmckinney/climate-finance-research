# ===============================
# File: 052_europe_regulatory_86pct_ci817_895.R
# Purpose: % of Europe respondents citing regulatory impediments (Q3.11 text)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
eu <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.2=="2")
k <- sum(grepl("regulatory", eu$Q3.11, ignore.case=TRUE), na.rm=TRUE)
n <- sum(!is.na(eu$Q3.11))
print(prop.test(k, n, conf.level = 0.95))
