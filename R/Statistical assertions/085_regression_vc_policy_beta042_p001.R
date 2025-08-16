# ===============================
# File: 085_regression_vc_policy_beta042_p001.R
# Purpose: VC-only: Investment likely (Q3.8_8) ~ policy (Q12.13_3) + regulatory (Q12.13_4)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- subset(data, Q2.1=="1")
fit <- lm(as.numeric(Q3.8_8) ~ as.numeric(Q12.13_3) + as.numeric(Q12.13_4), data = vc)
print(summary(fit))
