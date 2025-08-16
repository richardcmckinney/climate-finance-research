# ===============================
# File: 099_vc_customer_acquisition_31pct.R
# Purpose: % of VCs providing customer acquisition/pilot facilitation (Q3.10 '9')
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="1")
k <- sum(grepl("\\b9\\b", vc$Q3.10), na.rm=TRUE); n <- sum(!is.na(vc$Q3.10))
cat("VC customer acquisition (%):", round(100*k/n,1), "\n")
# Note: This code calculates the percentage of venture capitalists (VCs) who provide customer acquisition or pilot facilitation services based on survey responses.