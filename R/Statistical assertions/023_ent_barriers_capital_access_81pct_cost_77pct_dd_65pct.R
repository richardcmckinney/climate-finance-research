# File: 023_ent_barriers_capital_access_81pct_cost_77pct_dd_65pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Entrepreneurs: Capital Access (81%), High Cost of Capital (77%), Long Due Diligence (65%)"
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

ent_data <- subset(data, Q2.1 == "3")
ent_capital <- sum(grepl("1", ent_data$Q5.11), na.rm = TRUE) / nrow(ent_data) * 100
ent_cost <- sum(grepl("2", ent_data$Q5.11), na.rm = TRUE) / nrow(ent_data) * 100
ent_dd <- sum(grepl("3", ent_data$Q5.11), na.rm = TRUE) / nrow(ent_data) * 100
cat("Ent capital:", round(ent_capital,2), " cost:", round(ent_cost,2), " DD:", round(ent_dd,2), "\n")
