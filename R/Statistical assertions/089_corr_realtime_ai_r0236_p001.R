# ===============================
# File: 089_corr_realtime_ai_r0236_p001.R
# Purpose: Correlation Q12.1_12 (real-time data) vs Q12.1_13 (AI risk assess)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
print(cor.test(as.numeric(data$Q12.1_12), as.numeric(data$Q12.1_13), use="complete.obs"))
