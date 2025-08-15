# ===============================
# File: 036_corr_tech_market_r0342_p001_ci261_419.R
# Purpose: Pearson correlation between Tech risk (Q3.6_1) and Market risk (Q3.6_2)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
x <- as.numeric(data$Q3.6_1); y <- as.numeric(data$Q3.6_2)
print(cor.test(x, y, method="pearson", use="complete.obs"))
