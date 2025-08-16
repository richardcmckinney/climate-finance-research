# ===============================
# File: 094_corr_mentorship_partnership_r0374_p001.R
# Purpose: Mentorship (Q3.10 '2') vs Strategic partnerships (Q3.10 '6')
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
ment <- as.numeric(grepl("\\b2\\b", data$Q3.10))
partner <- as.numeric(grepl("\\b6\\b", data$Q3.10))
print(cor.test(ment, partner))
