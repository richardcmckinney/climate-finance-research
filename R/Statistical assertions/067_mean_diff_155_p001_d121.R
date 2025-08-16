# File: 067_mean_diff_155_p001_d121.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "mean difference=1.55, p<.001, d=1.21"
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

impact_data <- na.omit(data.frame(stakeholder = factor(data$Q2.1), impact = suppressWarnings(as.numeric(data$Q3.3)))); aov_impact <- aov(impact ~ stakeholder, data = impact_data); print(TukeyHSD(aov_impact)$stakeholder["1-8", ]); if (!requireNamespace("effectsize", quietly=TRUE)) stop("effectsize required"); vc_impact <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "1"])); esg_impact <- suppressWarnings(as.numeric(data$Q3.3[data$Q2.1 == "8"])); print(effectsize::cohens_d(vc_impact, esg_impact, pooled_sd = TRUE))
