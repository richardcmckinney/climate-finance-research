# ===============================
# File: 044_chisq_392_p0048_phi0162.R
# Purpose: 2x2 chi-square for VC vs Gov market readiness (example table)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
tab <- matrix(c(97,16,28,10), nrow=2, byrow=TRUE)
chi <- chisq.test(tab)
phi <- sqrt(chi$statistic / sum(tab))
cat("X2:", round(chi$statistic,2), " p:", chi$p.value, " phi:", round(phi,3), "\n")
