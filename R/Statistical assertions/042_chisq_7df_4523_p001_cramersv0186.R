# ===============================
# File: 042_chisq_7df_4523_p001_cramersv0186.R
# Purpose: Chi-square across stakeholder (Q2.1) x market-mention (Q3.11)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10)
mat <- table(analytic$Q2.1, grepl("market", analytic$Q3.11, ignore.case=TRUE))
chi <- chisq.test(mat)
V <- sqrt(chi$statistic / (sum(mat) * (min(dim(mat))-1)))
cat("X2:", round(chi$statistic,2), " df:", chi$parameter, " p:", chi$p.value, " V:", round(V,3), "\n")
