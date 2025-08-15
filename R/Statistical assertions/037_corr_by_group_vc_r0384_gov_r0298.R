# ===============================
# File: 037_corr_by_group_vc_r0384_gov_r0298.R
# Purpose: Group-wise correlations (VC and Gov) between Tech and Market risk
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
gov <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="2")
vc  <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="1")
cat("Gov r:",
    cor(as.numeric(gov$Q4.5_7), as.numeric(gov$Q4.5_8), use="complete.obs"), "\n")
cat("VC r:",
    cor(as.numeric(vc$Q3.6_1), as.numeric(vc$Q3.6_2), use="complete.obs"), "\n")
