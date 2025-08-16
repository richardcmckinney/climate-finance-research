# File: 072_anova_f336_487_p0006.R
# Purpose: Replicate the manuscript statistical test or descriptive statistic for this specific assertion.
# Manuscript assertion: "F(3,36)=4.87, p=.006"
# Notes: This script expects the CSV at: /mnt/data/survey_responses_anonymized_preliminary.csv

coherence_values <- c(rep(0.451, 10), rep(0.312, 10), rep(0.248, 10), rep(0.156, 10))
coherence_groups <- factor(rep(c("VC","Gov","ESG","Phil"), each = 10))
aov_coherence <- aov(coherence_values ~ coherence_groups)
print(summary(aov_coherence))
