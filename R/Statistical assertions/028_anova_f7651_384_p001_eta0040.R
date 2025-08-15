# ===============================
# File: 028_anova_f7651_384_p001_eta0040.R
# Purpose: One-way ANOVA of tech risk (Q3.6_1) by stakeholder (Q2.1)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(effectsize))
df <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10, 
             select=c(Q2.1,Q3.6_1))
df$Q3.6_1 <- as.numeric(df$Q3.6_1); df <- na.omit(df)
fit <- aov(Q3.6_1 ~ factor(Q2.1), data=df)
print(summary(fit)); print(eta_squared(fit))
