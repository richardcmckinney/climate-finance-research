# ===============================
# File: 001_surveying_1307_stakeholders.R
# Purpose: Count final analytic sample (Status == "IP Address" & Progress >= 10)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
n_complete <- sum(data$Status == "IP Address" & as.numeric(data$Progress) >= 10, na.rm = TRUE)
cat("Total respondents (analytic sample):", n_complete, "\n")
