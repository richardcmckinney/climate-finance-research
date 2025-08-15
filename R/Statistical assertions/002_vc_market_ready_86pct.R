# ===============================
# File: 002_vc_market_ready_86pct.R
# Purpose: % of VCs citing market readiness (searching text in Q3.11)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "1")
market_ready <- sum(grepl("market", vc$Q3.11, ignore.case = TRUE), na.rm = TRUE)
vc_market_pct <- 100 * market_ready / nrow(vc)
cat("VC market-ready (%):", round(vc_market_pct, 2), "\n")
