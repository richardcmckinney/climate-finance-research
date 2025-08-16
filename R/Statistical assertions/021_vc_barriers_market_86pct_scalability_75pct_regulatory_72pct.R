# File: 021_vc_barriers_market_86pct_scalability_75pct_regulatory_72pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Venture Capitalists: Market Readiness (86%), Scalability Issues (75%), Regulatory Uncertainty (72%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv
#        Ensure required packages are installed (psych, effectsize, pwr, vcd, naniar, lavaan, nnet, MASS, car).

# ---- Setup ----
suppressWarnings(suppressMessages({
  required_pkgs <- c("psych","effectsize","pwr","vcd","naniar","lavaan","nnet","MASS","car")
  for (p in required_pkgs) { if (!requireNamespace(p, quietly = TRUE)) { message(sprintf("Package '%s' not installed; attempting to proceed if not needed in this script.", p)) } }
}))

# Load data (literal path to the attached file)
data <- tryCatch({
  read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
}, error = function(e) {
  stop("Could not read CSV at /mnt/data/survey_responses_anonymized_preliminary.csv: ", e)
})

# Convenience: treat common columns
# Ensure key columns exist (Status, Progress)
if (!("Status" %in% names(data))) stop("Column 'Status' not found.")
if (!("Progress" %in% names(data))) stop("Column 'Progress' not found.")

# Clean subset similar to manuscript logic
data_clean <- subset(data, Status == "IP Address" & suppressWarnings(as.numeric(Progress)) >= 10)

vc_data <- subset(data, Q2.1 == "1")
vc_market <- sum(grepl("1", vc_data$Q3.11), na.rm = TRUE) / nrow(vc_data) * 100
vc_scale <- sum(grepl("2", vc_data$Q3.11), na.rm = TRUE) / nrow(vc_data) * 100
vc_reg <- sum(grepl("3", vc_data$Q3.11), na.rm = TRUE) / nrow(vc_data) * 100
cat("VC market:", round(vc_market,2), " scale:", round(vc_scale,2), " reg:", round(vc_reg,2), "\n")
