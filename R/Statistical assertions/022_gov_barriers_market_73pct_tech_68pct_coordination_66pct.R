# ===============================
# File: 022_gov_barriers_market_73pct_tech_68pct_coordination_66pct.R
# Purpose: Government barrier selection rates (Q4.7; 1=market,2=technology,3=coordination)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
gov <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "2")
pct <- function(pat) 100 * sum(grepl(pat, gov$Q4.7), na.rm = TRUE)/nrow(gov)
cat("Gov market:", round(pct("\\b1\\b"),1), " tech:", round(pct("\\b2\\b"),1),
    " coordination:", round(pct("\\b3\\b"),1), "\n")
