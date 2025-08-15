# ===============================
# File: 023_ent_barriers_capital_access_81pct_cost_77pct_dd_65pct.R
# Purpose: Entrepreneur barrier selection (Q5.11; 1=capital access,2=cost of capital,3=due diligence)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
ent <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "3")
pct <- function(pat) 100 * sum(grepl(pat, ent$Q5.11), na.rm = TRUE)/nrow(ent)
cat("Capital:", round(pct("\\b1\\b"),1), " Cost:", round(pct("\\b2\\b"),1),
    " Due diligence:", round(pct("\\b3\\b"),1), "\n")
