# ===============================
# File: 006_government_funding_agencies_38_29pct.R
# Purpose: Count & % of Government agencies
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
gov_n <- sum(analytic$Q2.1 == "2", na.rm = TRUE)
gov_pct <- 100 * gov_n / nrow(analytic)
cat("Government count:", gov_n, " Government %:", round(gov_pct, 1), "\n")
