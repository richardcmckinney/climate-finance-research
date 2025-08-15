# ===============================
# File: 007_entrepreneurs_263_201pct.R
# Purpose: Count & % of Entrepreneurs
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
ent_n <- sum(analytic$Q2.1 == "3", na.rm = TRUE)
ent_pct <- 100 * ent_n / nrow(analytic)
cat("Entrepreneurs count:", ent_n, " Entrepreneurs %:", round(ent_pct, 1), "\n")
