# ===============================
# File: 009_limited_partners_54_41pct.R
# Purpose: Count & % of Limited Partners (LP)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
lp_n <- sum(analytic$Q2.1 == "4", na.rm = TRUE)
lp_pct <- 100 * lp_n / nrow(analytic)
cat("LP count:", lp_n, " LP %:", round(lp_pct, 1), "\n")
