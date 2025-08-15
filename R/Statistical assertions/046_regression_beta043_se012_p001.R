# ===============================
# File: 046_regression_beta043_se012_p001.R
# Purpose: Linear model: investment_likely (Q3.8_8) ~ international (Q3.7)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
df <- data.frame(y = as.numeric(data$Q3.8_8), x = as.numeric(data$Q3.7))
fit <- lm(y ~ x, df)
print(summary(fit))
