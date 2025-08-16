# ===============================
# File: 091_interaction_beta018_se007_p0011.R
# Purpose: LM with interaction: investment ~ market_readiness * regulatory_barriers
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
df <- data.frame(
  inv = as.numeric(data$Q3.8_8),
  mr  = as.numeric(data$Q3.6_2),
  rb  = as.numeric(data$Q3.6_3)
)
fit <- lm(inv ~ mr * rb, df)
print(summary(fit))
