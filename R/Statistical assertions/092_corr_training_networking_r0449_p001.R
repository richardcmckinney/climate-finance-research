# ===============================
# File: 092_corr_training_networking_r0449_p001.R
# Purpose: Training (Q3.10 contains '4') vs Networking (Q3.10 contains '1')
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
training <- as.numeric(grepl("\\b4\\b", data$Q3.10))
networking <- as.numeric(grepl("\\b1\\b", data$Q3.10))
print(cor.test(training, networking))