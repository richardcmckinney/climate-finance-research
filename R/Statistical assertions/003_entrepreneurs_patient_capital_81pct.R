# ===============================
# File: 003_entrepreneurs_patient_capital_81pct.R
# Purpose: % of entrepreneurs citing patient capital (Q5.11 text search)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
ent <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10 & Q2.1 == "3")
need_capital <- sum(grepl("capital", ent$Q5.11, ignore.case = TRUE), na.rm = TRUE)
ent_capital_pct <- 100 * need_capital / nrow(ent)
cat("Entrepreneurs patient capital (%):", round(ent_capital_pct, 2), "\n")
