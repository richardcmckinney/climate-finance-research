# ===============================
# File: 021_vc_barriers_market_86pct_scalability_75pct_regulatory_72pct.R
# Purpose: VC barrier selection rates from multi-select text-coded Q3.11 (1=market,2=scalability,3=regulatory)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "1")
pct <- function(pat) 100 * sum(grepl(pat, vc$Q3.11), na.rm = TRUE)/nrow(vc)
cat("VC market:", round(pct("\\b1\\b"),1), " scale:", round(pct("\\b2\\b"),1),
    " regulatory:", round(pct("\\b3\\b"),1), "\n")
