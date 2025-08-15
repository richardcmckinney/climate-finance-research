# ===============================
# File: 049_corr_networking_partnership_r0449_p001.R
# Purpose: Correlation networking support (Q3.10 contains '1') vs partnership (Q3.8_11)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
networking <- as.numeric(grepl("\\b1\\b", data$Q3.10))
partner <- as.numeric(data$Q3.8_11)
print(cor.test(networking, partner, use="complete.obs"))
