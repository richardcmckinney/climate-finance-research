# ===============================
# File: 047_interaction_beta021_se009_p0020.R
# Purpose: Interaction model with VC indicator
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
df <- data.frame(y = as.numeric(data$Q3.8_8),
                 x = as.numeric(data$Q3.7),
                 is_vc = as.numeric(data$Q2.1 == "1"))
fit <- lm(y ~ x * is_vc, df)
print(summary(fit))
