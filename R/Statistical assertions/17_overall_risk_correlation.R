# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Three-factor solution explained 62.3% of total variance"
# Purpose: Conduct exploratory factor analysis with 3 factors and calculate variance explained

library(tidyverse)
library(janitor)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
EFA_ITEMS <- c("risk_physical", "risk_operational", "risk_policy", "risk_regulatory",
               "risk_market", "risk_financial", "risk_technology", "risk_supply_chain",
               "risk_reputational", "risk_litigation")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(EFA_ITEMS)) %>%
  mutate(across(everything(), as.numeric))

risk_data <- na.omit(df)

# Principal component analysis with varimax rotation
pca_result <- principal(risk_data, nfactors = 3, rotate = "varimax")

# Print results
print(pca_result)

# Calculate total variance explained
total_var <- sum(pca_result$values[1:3]) / length(pca_result$values) * 100
print(paste("Total variance explained by 3 factors:", round(total_var, 1), "%"))

# Print factor loadings
print("Factor loadings:")
print(pca_result$loadings)

# Expected: 62.3% total variance, with specific loadings as stated