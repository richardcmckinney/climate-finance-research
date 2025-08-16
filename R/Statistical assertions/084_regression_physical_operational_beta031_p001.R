# ===============================
# File: 084_regression_physical_operational_beta031_p001.R
# Purpose: LM: Investment likely (Q3.8_8) ~ physical (Q12.13_1) + operational (Q12.13_2)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
fit <- lm(as.numeric(Q3.8_8) ~ as.numeric(Q12.13_1) + as.numeric(Q12.13_2), data = data)
print(summary(fit))
