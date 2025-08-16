# ===============================
# File: 100_corr_sector_support_fundraising_r0287_p001.R
# Purpose: Sector-specific support (Q3.10 '10') vs fundraising success proxy (Q3.8_14)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
sector <- as.numeric(grepl("\\b10\\b", data$Q3.10))
fund <- as.numeric(data$Q3.8_14)
print(cor.test(sector, fund, use="complete.obs"))
