# ===============================
# File: 045_vc_scalability_71pct_ci624_786.R
# Purpose: % of VCs rating international scalability >= 6 (Q3.7)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="1")
k <- sum(as.numeric(vc$Q3.7) >= 6, na.rm=TRUE)
n <- sum(!is.na(vc$Q3.7))
print(prop.test(k, n, conf.level = 0.95))
