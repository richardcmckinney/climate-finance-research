# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "The difference between VCs and government agencies was statistically significant (χ²(1)=4.73, p=.030, φ=.174)"
# Purpose: Compare VCs vs Government on technology risk criticality using chi-square test

library(tidyverse)
library(janitor)
library(vcd)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_TECH_RISK_CRIT <- "tech_risk_critical"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    tech_risk_critical = as.integer(tech_risk_critical)
  )

# Filter for VCs and Government only
vc_gov_data <- df %>% 
  filter(stakeholder %in% c("Venture Capital", "Government Agency"))

# Create contingency table
cont_table <- table(vc_gov_data$stakeholder, vc_gov_data$tech_risk_critical)

# Chi-square test
chi_result <- chisq.test(cont_table)
print(chi_result)

# Effect size (Cramér's V / phi for 2x2 table)
assoc_stats <- assocstats(cont_table)
print(assoc_stats)

# Expected: χ²(1)=4.73, p=.030, φ=.174