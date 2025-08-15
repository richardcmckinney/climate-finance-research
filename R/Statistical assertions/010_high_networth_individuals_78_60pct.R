# ===============================
# File: 010_high_networth_individuals_78_60pct.R
# Purpose: Count & % of HNWI
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
hnwi_n <- sum(analytic$Q2.1 == "5", na.rm = TRUE)
hnwi_pct <- 100 * hnwi_n / nrow(analytic)
cat("HNWI count:", hnwi_n, " HNWI %:", round(hnwi_pct, 1), "\n")
