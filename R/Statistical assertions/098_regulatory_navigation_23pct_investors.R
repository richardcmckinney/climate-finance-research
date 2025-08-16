# ===============================
# File: 098_regulatory_navigation_23pct_investors.R
# Purpose: % investors providing regulatory navigation (Q3.10 '8')
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
inv <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1 %in% c("1","2","7","8"))
k <- sum(grepl("\\b8\\b", inv$Q3.10), na.rm=TRUE); n <- sum(!is.na(inv$Q3.10))
cat("Regulatory navigation (%):", round(100*k/n,1), "\n")
