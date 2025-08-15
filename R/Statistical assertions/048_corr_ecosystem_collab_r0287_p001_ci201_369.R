# ===============================
# File: 048_corr_ecosystem_collab_r0287_p001_ci201_369.R
# Purpose: Correlation ecosystem support (Q3.10 numeric proxy) vs collaboration preference (Q3.8_10)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
x <- as.numeric(data$Q3.10)   # if coded numeric
y <- as.numeric(data$Q3.8_10)
print(cor.test(x, y, use="complete.obs"))
