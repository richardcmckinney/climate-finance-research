# ===============================
# File: 050_corr_mentorship_coinvest_r0374_p001.R
# Purpose: Mentorship flag (Q3.10 contains '2') vs co-invest (Q3.8_12)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
mentorship <- as.numeric(grepl("\\b2\\b", data$Q3.10))
coinvest <- as.numeric(data$Q3.8_12)
print(cor.test(mentorship, coinvest, use="complete.obs"))
