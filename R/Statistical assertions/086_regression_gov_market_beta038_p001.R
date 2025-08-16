# ===============================
# File: 086_regression_gov_market_beta038_p001.R
# Purpose: Gov-only: (Q4.6_7) ~ market (Q12.13_5) + financial (Q12.13_6)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
gov <- subset(data, Q2.1=="2")
fit <- lm(as.numeric(Q4.6_7) ~ as.numeric(Q12.13_5) + as.numeric(Q12.13_6), data = gov)
print(summary(fit))
