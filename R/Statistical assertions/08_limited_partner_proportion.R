# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "technology risks showed significant positive correlation with market risks (r=.342, p<.001, 95% CI [.261, .419])"
# Purpose: Calculate Pearson correlation between technology and market risk ratings

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TECH_RISK_MEAN <- "tech_risk_rating"
COL_MARKET_RISK_MEAN <- "market_risk_rating"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    tech_risk_rating = as.numeric(tech_risk_rating),
    market_risk_rating = as.numeric(market_risk_rating)
  )

# Correlation test
cor_result <- cor.test(df$tech_risk_rating, df$market_risk_rating, 
                       method = "pearson", use = "complete.obs")
print(cor_result)

# Expected: r=.342, p<.001, 95% CI [.261, .419] 