# ===============================
# File: 093_corr_funding_regulatory_r0478_p001.R
# Purpose: Funding (Q3.10 '5') vs Regulatory support (Q3.10 '3')
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
funding <- as.numeric(grepl("\\b5\\b", data$Q3.10))
reg <- as.numeric(grepl("\\b3\\b", data$Q3.10))
print(cor.test(funding, reg))
