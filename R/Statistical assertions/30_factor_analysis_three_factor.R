# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 30: "Three-factor solution explained 67.2% of variance (KMO = .91)"
# Purpose: Perform exploratory factor analysis on key variables

library(tidyverse)
library(janitor)
library(psych)
library(GPArotation)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - items for factor analysis
FA_ITEMS <- c("tech_risk_rating", "market_risk_rating", "regulatory_risk_rating",
              "funding_confidence", "market_readiness", "technology_maturity",
              "team_capability", "competitive_advantage", "scalability_potential",
              "support_networking", "support_mentorship", "support_strategic_counsel")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(FA_ITEMS)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# Check Kaiser-Meyer-Olkin measure of sampling adequacy
kmo_result <- KMO(df)
print(paste("KMO measure of sampling adequacy:", round(kmo_result$MSA, 2)))

# Determine number of factors
parallel_analysis <- fa.parallel(df, fm = "ml", fa = "fa")
print(paste("Parallel analysis suggests", parallel_analysis$nfact, "factors"))

# Three-factor solution
fa_result <- fa(df, nfactors = 3, rotate = "oblimin", fm = "ml")

# Calculate variance explained
variance_explained <- sum(fa_result$values[1:3]) / ncol(df) * 100
print(paste("Three-factor solution explains", round(variance_explained, 1), "% of variance"))

# Print factor loadings
print("Factor loadings:")
print(fa_result$loadings, cutoff = 0.3)

# Expected: Three-factor solution explained 67.2% of variance (KMO = .91)