# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Little's MCAR test indicated missing data were Missing Completely At Random (χ²=1823.45, df=1799, p=.346)"
# Purpose: Test whether missing data pattern is MCAR

library(tidyverse)
library(janitor)
library(BaylorEdPsych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - select key analysis variables
KEY_VARS <- c("tech_risk_rating", "market_risk_rating", "regulatory_risk_rating",
              "financial_vs_impact_focus", "investment_likelihood")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(KEY_VARS)) %>%
  mutate(across(everything(), as.numeric))

# Little's MCAR test
mcar_result <- LittleMCAR(df)
print(mcar_result)

# Expected: χ²=1823.45, df=1799, p=.346 