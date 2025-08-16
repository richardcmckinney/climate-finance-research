# File: 024_vc_tech_risks_52pct_ci428_611.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "52% (95% CI [42.8%, 61.1%]) rated technology risks as critical"
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
tech_critical <- sum(suppressWarnings(as.numeric(vc_data$Q3.6_1)) >= 6, na.rm = TRUE)
test <- prop.test(tech_critical, nrow(vc_data), conf.level = 0.95)
print(test)
