# File: 002_vc_market_ready_86pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Venture capitalists demand market-ready ventures (86%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv
#        Ensure required packages are installed (psych, effectsize, pwr, vcd, naniar, lavaan, nnet, MASS, car).

suppressWarnings(suppressMessages({
  required_pkgs <- c("psych","effectsize","pwr","vcd","naniar","lavaan","nnet","MASS","car")
  for (p in required_pkgs) { if (!requireNamespace(p, quietly = TRUE)) { message(sprintf("Package '%s' not installed; attempting to proceed if not needed in this script.", p)) } }
}))

data <- tryCatch({
  read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
}, error = function(e) { stop("Could not read CSV: ", e) })

if (!("Status" %in% names(data))) stop("Column 'Status' not found.")
vc_data <- subset(data, Q2.1 == "1" & Status == "IP Address")
market_ready <- sum(grepl("market", vc_data$Q3.11, ignore.case = TRUE), na.rm = TRUE)
vc_market_pct <- (market_ready / nrow(vc_data)) * 100
cat("VC market-ready %:", round(vc_market_pct, 2), "\n")
