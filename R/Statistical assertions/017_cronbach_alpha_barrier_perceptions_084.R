# File: 017_cronbach_alpha_barrier_perceptions_084.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Cronbach's alpha: Barrier perceptions composite (α = .84)"
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

if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' required.")
barrier_items <- data[, c("Q3.6_1","Q3.6_2","Q3.6_3","Q3.6_4","Q3.6_5")]
barrier_numeric <- apply(barrier_items, 2, function(x) suppressWarnings(as.numeric(x)))
print(psych::alpha(na.omit(barrier_numeric)))
