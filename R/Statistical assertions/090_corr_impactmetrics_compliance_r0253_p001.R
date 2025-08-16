# ===============================
# File: 090_corr_impactmetrics_compliance_r0253_p001.R
# Purpose: Correlation Q12.1_14 (impact metrics) vs Q12.1_15 (compliance automation)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
print(cor.test(as.numeric(data$Q12.1_14), as.numeric(data$Q12.1_15), use="complete.obs"))
