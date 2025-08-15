# ===============================
# File: 005_vc_firms_113_86pct.R
# Purpose: Count & % of VCs in analytic sample
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
vc_n <- sum(analytic$Q2.1 == "1", na.rm = TRUE)
vc_pct <- 100 * vc_n / nrow(analytic)
cat("VC count:", vc_n, " VC %:", round(vc_pct, 1), "\n")
