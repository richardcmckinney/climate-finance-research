# ===============================
# File: 024_vc_tech_risks_52pct_ci428_611.R
# Purpose: Proportion of VCs rating tech risks as critical (Q3.6_1 >= 6)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "1")
crit <- sum(as.numeric(vc$Q3.6_1) >= 6, na.rm = TRUE)
print(prop.test(crit, nrow(vc), conf.level = 0.95))
