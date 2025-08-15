# ===============================
# File: 011_family_offices_46_35pct.R
# Purpose: Count & % of Family Offices
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
fo_n <- sum(analytic$Q2.1 == "7", na.rm = TRUE)
fo_pct <- 100 * fo_n / nrow(analytic)
cat("Family Offices count:", fo_n, " Family Offices %:", round(fo_pct, 1), "\n")
