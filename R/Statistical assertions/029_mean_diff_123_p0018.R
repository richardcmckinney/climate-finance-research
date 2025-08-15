# ===============================
# File: 029_mean_diff_123_p0018.R
# Purpose: Tukey mean difference between VC (1) and Government (2) on Q3.6_1
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
df <- subset(data, Status=="IP Address" & as.numeric(Progress)>=10, select=c(Q2.1,Q3.6_1))
df$Q3.6_1 <- as.numeric(df$Q3.6_1); df <- na.omit(df)
fit <- aov(Q3.6_1 ~ factor(Q2.1), data=df)
print(TukeyHSD(fit)$`factor(Q2.1)`["1-2", , drop=FALSE])
