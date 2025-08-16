# File: 017_cronbach_alpha_barrier_perceptions_084.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Cronbach's alpha: Barrier perceptions composite (α = .84)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

if (!requireNamespace("psych", quietly = TRUE)) stop("Package 'psych' required.")
data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
barrier_items <- data[, c("Q3.6_1","Q3.6_2","Q3.6_3","Q3.6_4","Q3.6_5")]
barrier_numeric <- apply(barrier_items, 2, function(x) suppressWarnings(as.numeric(x)))
print(psych::alpha(na.omit(barrier_numeric)))
