# ===============================
# File: 053_na_cost_competitiveness_89pct.R
# Purpose: North America cost-competitiveness citation rate (Q3.11 text)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
na <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.2=="1")
k <- sum(grepl("cost", na$Q3.11, ignore.case=TRUE), na.rm=TRUE)
n <- sum(!is.na(na$Q3.11))
cat("NA cost-competitiveness (%):", round(100*k/n,1), "\n")
