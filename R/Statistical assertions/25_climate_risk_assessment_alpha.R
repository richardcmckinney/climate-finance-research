# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Climate risk assessment (α = .91)"
# Purpose: Calculate internal consistency reliability for climate risk assessment items

library(tidyverse)
library(janitor)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - climate risk assessment items
RISK_ITEMS <- c("risk_physical", "risk_operational", "risk_policy", "risk_regulatory",
                "risk_market", "risk_financial", "risk_technology", "risk_supply_chain",
                "risk_reputational", "risk_litigation")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(RISK_ITEMS)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# Calculate Cronbach's alpha
alpha_result <- alpha(df)
print(paste("Cronbach's alpha for climate risk assessment:", 
            round(alpha_result$total$raw_alpha, 2)))

# Expected: α = .91