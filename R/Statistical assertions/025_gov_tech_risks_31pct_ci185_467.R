# ===============================
# File: 025_gov_tech_risks_31pct_ci185_467.R
# Purpose: Proportion of Government rating tech risk critical (Q4.5_7 >= 6)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
gov <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "2")
crit <- sum(as.numeric(gov$Q4.5_7) >= 6, na.rm = TRUE)
print(prop.test(crit, nrow(gov), conf.level = 0.95))
