# ===============================
# File: 087_corr_tech_features_r0195_r0452.R
# Purpose: Range of intercorrelations among tech features (Q12.1_*)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
X <- as.data.frame(lapply(data[, grep("^Q12\\.1_", names(data))], as.numeric))
C <- cor(X, use="complete.obs")
rng <- range(C[lower.tri(C)])
cat("Intercorrelation range:", paste(round(rng,3), collapse=" to "), "\n")
