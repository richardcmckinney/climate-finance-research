# File: 014_geographic_distribution_na_eu_asia_other.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "Geographic distribution: North America (34%), Europe (32%), Asia (18%), Other (16%)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
geo_vec <- data$Q2.2[data$Status == "IP Address"]
geo_vec <- geo_vec[!is.na(geo_vec)]
geo_table <- table(geo_vec)
geo_pcts <- prop.table(geo_table) * 100
print(round(geo_pcts, 2))
