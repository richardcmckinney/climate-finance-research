# File: 023_ent_barriers_capital_access_81pct_cost_77pct_dd_65pct.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Entrepreneurs: Capital Access (81%), High Cost of Capital (77%), Long Due Diligence (65%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
ent_data <- subset(data, Q2.1 == "3")
ent_capital <- sum(grepl("1", ent_data$Q5.11), na.rm = TRUE) / nrow(ent_data) * 100
ent_cost    <- sum(grepl("2", ent_data$Q5.11), na.rm = TRUE) / nrow(ent_data) * 100
ent_dd      <- sum(grepl("3", ent_data$Q5.11), na.rm = TRUE) / nrow(ent_data) * 100
cat("Ent capital:", round(ent_capital,2), " cost:", round(ent_cost,2), " DD:", round(ent_dd,2), "\n")
