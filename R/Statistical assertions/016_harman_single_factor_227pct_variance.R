# ===============================
# File: 016_harman_single_factor_227pct_variance.R
# Purpose: Harman’s single-factor test proxy via 1-factor FA
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
suppressPackageStartupMessages(library(psych))
all_items <- data[, grep("^(Q3\\.|Q4\\.|Q5\\.)", names(data))]
all_num <- as.data.frame(lapply(all_items, function(x) as.numeric(x)))
all_num <- na.omit(all_num)
fa1 <- fa(all_num, nfactors = 1, rotate = "none")
first_var <- 100 * fa1$values[1] / sum(fa1$values)
cat("Harman 1st factor variance (%):", round(first_var, 1), "\n")
