# File: 035_all_resp_mean_528_sd136.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "All Respondents Mean 5.28 (SD=1.36)"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

data <- read.csv("/mnt/data/survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE, check.names = FALSE)
vc_tech_scores  <- suppressWarnings(as.numeric(data$Q3.6_1[data$Q2.1 == "1"]))
gov_tech_scores <- suppressWarnings(as.numeric(data$Q4.5_7[data$Q2.1 == "2"]))
ent_tech_scores <- suppressWarnings(as.numeric(data$Q5.8_6[data$Q2.1 == "3"]))
lp_tech_scores  <- suppressWarnings(as.numeric(data$Q7.8_4[data$Q2.1 == "4"]))
all_tech_scores <- c(vc_tech_scores, gov_tech_scores, ent_tech_scores, lp_tech_scores)
cat("Mean:", round(mean(all_tech_scores, na.rm=TRUE),2), " SD:", round(sd(all_tech_scores, na.rm=TRUE),2), "\n")
