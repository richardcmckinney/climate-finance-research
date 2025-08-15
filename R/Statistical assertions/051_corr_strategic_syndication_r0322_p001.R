# ===============================
# File: 051_corr_strategic_syndication_r0322_p001.R
# Purpose: Strategic counsel flag (Q3.10 '3') vs syndication prefs (Q3.8_13)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
strategic <- as.numeric(grepl("\\b3\\b", data$Q3.10))
syndication <- as.numeric(data$Q3.8_13)
print(cor.test(strategic, syndication, use="complete.obs"))
