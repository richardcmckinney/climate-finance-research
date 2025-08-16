# ===============================
# File: 088_corr_dealflow_collab_r0338_p001.R
# Purpose: Correlation Q12.1_10 (deal flow) vs Q12.1_11 (collab tools)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
print(cor.test(as.numeric(data$Q12.1_10), as.numeric(data$Q12.1_11), use="complete.obs"))
