# File: 037_corr_by_group_vc_r0384_gov_r0298.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "correlation was consistent across stakeholder groups, ranging from r=.298 (government) to r=.384 (VCs)"
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

gov_data <- subset(data, Q2.1 == "2"); print(cor(suppressWarnings(as.numeric(gov_data$Q4.5_7)), suppressWarnings(as.numeric(gov_data$Q4.5_8)), use="complete.obs"))
vc_data <- subset(data, Q2.1 == "1"); print(cor(suppressWarnings(as.numeric(vc_data$Q3.6_1)), suppressWarnings(as.numeric(vc_data$Q3.6_2)), use="complete.obs"))
