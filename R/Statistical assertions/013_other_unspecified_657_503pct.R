# ===============================
# File: 013_other_unspecified_657_503pct.R
# Purpose: Count & % of Other/Unspecified stakeholders (Q2.1 == "12")
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
other_n <- sum(analytic$Q2.1 == "12", na.rm = TRUE)
other_pct <- 100 * other_n / nrow(analytic)
cat("Other/Unspecified count:", other_n, " Other %:", round(other_pct, 1), "\n")
