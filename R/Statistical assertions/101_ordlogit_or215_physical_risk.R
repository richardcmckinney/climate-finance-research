# ===============================
# File: 101_ordlogit_or215_physical_risk.R
# Purpose: Ordinal logistic regression: Q12.13_1 ~ Q3.3 (impact focus)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(MASS))
df <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10)
fit <- polr(as.factor(Q12.13_1) ~ as.numeric(Q3.3), data=df, Hess=TRUE)
print(exp(coef(fit))); print(exp(confint(fit)))
