# File: 042_chisq_7df_4523_p001_cramersv0186.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "χ²(7)=45.23, p<.001, Cramér's V=.186, small-to-medium effect"
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

if (!requireNamespace("vcd", quietly = TRUE)) stop("Package 'vcd' required.")
market_by_group <- table(data$Q2.1, grepl("market", data$Q3.11, ignore.case = TRUE)); print(chisq.test(market_by_group)); print(vcd::assocstats(market_by_group))
