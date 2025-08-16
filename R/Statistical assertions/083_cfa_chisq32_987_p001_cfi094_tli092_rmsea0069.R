# ===============================
# File: 083_cfa_chisq32_987_p001_cfi094_tli092_rmsea0069.R
# Purpose: CFA 3-factor model via lavaan (physical, policy, market)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(lavaan))
model <- '
  physical =~ Q12.13_1 + Q12.13_2
  policy   =~ Q12.13_3 + Q12.13_4
  market   =~ Q12.13_5 + Q12.13_6
'
fit <- cfa(model, data=data, std.lv=TRUE, missing="fiml")
print(fitMeasures(fit, c("chisq","df","pvalue","cfi","tli","rmsea")))
