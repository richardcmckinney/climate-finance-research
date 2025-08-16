# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "philanthropic organizations scored M=2.81 (SD=1.22), ESG investors scored M=3.76 (SD=1.31), venture capitalists scored M=5.31 (SD=1.19)"
# Purpose: Calculate mean financial vs impact orientation scores by stakeholder group

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_FIN_IMPACT_1to7 <- "financial_vs_impact_focus"  # 1=pure impact, 7=pure financial

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    financial_vs_impact_focus = as.numeric(financial_vs_impact_focus)
  )

# Calculate by group
groups <- c("Philanthropic", "ESG Investor", "Venture Capital")
for(grp in groups) {
  grp_data <- df %>% 
    filter(stakeholder == grp) %>%
    pull(financial_vs_impact_focus) %>%
    na.omit()
  
  mean_val <- mean(grp_data)
  sd_val <- sd(grp_data)
  n_grp <- length(grp_data)
  
  print(paste(grp, ": M =", round(mean_val, 2), ", SD =", round(sd_val, 2), ", n =", n_grp))
}

# Expected: Philanthropic M=2.81 (SD=1.22), ESG M=3.76 (SD=1.31), VC M=5.31 (SD=1.19)