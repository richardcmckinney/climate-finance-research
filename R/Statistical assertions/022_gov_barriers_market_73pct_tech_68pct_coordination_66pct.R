# File: 022_gov_barriers_market_73pct_tech_68pct_coordination_66pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Government Agencies: Market Readiness (73%), Technology Risk (68%), Coordination Challenges (66%)"
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

gov_data <- subset(data, Q2.1 == "2")
gov_market <- sum(grepl("1", gov_data$Q4.7), na.rm = TRUE) / nrow(gov_data) * 100
gov_tech <- sum(grepl("2", gov_data$Q4.7), na.rm = TRUE) / nrow(gov_data) * 100
gov_coord <- sum(grepl("3", gov_data$Q4.7), na.rm = TRUE) / nrow(gov_data) * 100
cat("Gov market:", round(gov_market,2), " tech:", round(gov_tech,2), " coord:", round(gov_coord,2), "\n")
