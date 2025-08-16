# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "KMO measure (.847) and Bartlett's test (χ²(45)=2341.67, p<.001)"
# Purpose: Test factorability of risk items using KMO and Bartlett's test

library(tidyverse)
library(janitor)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - risk factor items
EFA_ITEMS <- c("risk_physical", "risk_operational", "risk_policy", "risk_regulatory",
               "risk_market", "risk_financial", "risk_technology", "risk_supply_chain",
               "risk_reputational", "risk_litigation")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(EFA_ITEMS)) %>%
  mutate(across(everything(), as.numeric))

# Remove rows with missing values
risk_data <- na.omit(df)

# KMO test
kmo_result <- KMO(risk_data)
print(paste("KMO MSA:", round(kmo_result$MSA, 3)))

# Bartlett's test
cor_matrix <- cor(risk_data)
n_obs <- nrow(risk_data)
bartlett_result <- cortest.bartlett(cor_matrix, n = n_obs)
print(bartlett_result)

# Expected: KMO = .847, Bartlett's χ²(45)=2341.67, p<.001 