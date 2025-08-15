# ===============================
# File: 035_all_resp_mean_528_sd136.R
# Purpose: Pooled descriptive mean/SD across selected stakeholder tech risk measures
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
vc <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="1")$Q3.6_1)
gov <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="2")$Q4.5_7)
ent <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="3")$Q5.8_6)
lp  <- as.numeric(subset(data, Status=="IP Address" & as.numeric(Progress)>=10 & Q2.1=="4")$Q7.8_4)
x <- c(vc,gov,ent,lp)
cat("All respondents mean:", round(mean(x, na.rm=TRUE),2), " SD:", round(sd(x, na.rm=TRUE),2), "\n")
