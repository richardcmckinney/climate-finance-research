# File: 053_na_cost_competitiveness_89pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "89% citation rate"
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

na_data <- subset(data, Q2.2 == "1"); na_cost <- sum(grepl("cost", na_data$Q3.11, ignore.case = TRUE), na.rm = TRUE); na_total <- sum(!is.na(na_data$Q3.11)); na_cost_pct <- (na_cost / na_total) * 100; cat("NA cost-competitiveness %:", round(na_cost_pct,2), "\n")
