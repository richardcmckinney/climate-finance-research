# File: 046_regression_beta043_se012_p001.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "β=0.43, SE=0.12, p<.001"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv
<<<<<<< HEAD

model_data <- data.frame(
  investment_likely = suppressWarnings(as.numeric(data$Q3.8_8)),
  international     = suppressWarnings(as.numeric(data$Q3.7))
)
model <- glm(investment_likely ~ international, data = model_data, family = gaussian())
print(summary(model))
=======
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

model_data <- data.frame(investment_likely = suppressWarnings(as.numeric(data$Q3.8_8)), international = suppressWarnings(as.numeric(data$Q3.7)))
model <- glm(investment_likely ~ international, data = model_data, family = gaussian()); print(summary(model))
>>>>>>> test/repro-scrubber-and-snapshot
