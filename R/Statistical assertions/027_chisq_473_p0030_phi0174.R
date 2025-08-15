# ===============================
# File: 027_chisq_473_p0030_phi0174.R
# Purpose: 2x2 Chi-square and phi from a contingency table (example counts)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
tab <- matrix(c(59,54,12,26), nrow = 2, byrow = TRUE)
chisq <- chisq.test(tab)
phi <- sqrt(chisq$statistic / sum(tab))
cat("Chi-square:", round(chisq$statistic,2), " p:", chisq$p.value, " phi:", round(phi,3), "\n")
