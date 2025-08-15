# ===============================
# File: 008_philanthropic_orgs_16_12pct.R
# Purpose: Count & % of Philanthropic organizations
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
phil_n <- sum(analytic$Q2.1 == "6", na.rm = TRUE)
phil_pct <- 100 * phil_n / nrow(analytic)
cat("Philanthropic count:", phil_n, " Philanthropic %:", round(phil_pct, 1), "\n")
