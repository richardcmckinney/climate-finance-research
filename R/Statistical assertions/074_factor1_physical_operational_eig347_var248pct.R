# File: 074_factor1_physical_operational_eig347_var248pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Factor 1: Physical-Operational (Eigenvalue: 3.47, 24.8% variance)"
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

risk_cols <- grep("^Q12\.13_", names(data), value = TRUE)[1:10]; risk_numeric <- apply(data[, risk_cols, drop=FALSE], 2, function(x) suppressWarnings(as.numeric(x))); if (!requireNamespace("psych", quietly=TRUE)) stop("psych required"); pca <- psych::principal(na.omit(risk_numeric), nfactors = 3, rotate = "varimax"); cat("Eigen 1:", round(pca$values[1],2), " Var%:", round(pca$Vaccounted[1,1]*100,2), "\n")
