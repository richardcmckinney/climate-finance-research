# ===============================
# File: 055_chisq_784_p0005_phi0089.R
# Purpose: 2x2 chi-square for geo x regulatory mention (example table)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
tab <- matrix(c(359,59,347,98), nrow=2, byrow=TRUE)
chi <- chisq.test(tab); phi <- sqrt(chi$statistic / sum(tab))
cat("X2:", round(chi$statistic,2), " p:", chi$p.value, " phi:", round(phi,3), "\n")
