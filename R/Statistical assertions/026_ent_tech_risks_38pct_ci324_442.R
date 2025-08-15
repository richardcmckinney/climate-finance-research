# ===============================
# File: 026_ent_tech_risks_38pct_ci324_442.R
# Purpose: Proportion of Entrepreneurs rating tech risk critical (Q5.8_6 >= 6)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
ent <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "3")
crit <- sum(as.numeric(ent$Q5.8_6) >= 6, na.rm = TRUE)
print(prop.test(crit, nrow(ent), conf.level = 0.95))
