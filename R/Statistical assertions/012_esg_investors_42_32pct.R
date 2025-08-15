# ===============================
# File: 012_esg_investors_42_32pct.R
# Purpose: Count & % of ESG investors
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
esg_n <- sum(analytic$Q2.1 == "8", na.rm = TRUE)
esg_pct <- 100 * esg_n / nrow(analytic)
cat("ESG count:", esg_n, " ESG %:", round(esg_pct, 1), "\n")
