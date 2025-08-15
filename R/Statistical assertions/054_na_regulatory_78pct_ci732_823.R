# ===============================
# File: 054_na_regulatory_78pct_ci732_823.R
# Purpose: North America regulatory impediments (Q3.11 text) with CI
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
na <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.2=="1")
k <- sum(grepl("regulatory", na$Q3.11, ignore.case=TRUE), na.rm=TRUE)
n <- sum(!is.na(na$Q3.11))
print(prop.test(k, n, conf.level = 0.95))
