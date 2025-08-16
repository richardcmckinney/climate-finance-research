# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "F(7,498)=18.92, p<.001, η²=.210, large effect"
# Purpose: Test for differences in financial vs impact orientation across all stakeholder groups

library(tidyverse)
library(janitor)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_FIN_IMPACT_1to7 <- "financial_vs_impact_focus"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    financial_vs_impact_focus = as.numeric(financial_vs_impact_focus)
  ) %>%
  filter(!is.na(financial_vs_impact_focus))

# ANOVA
aov_model <- aov(financial_vs_impact_focus ~ stakeholder, data = df)
summary(aov_model)

# Effect size
eta_sq <- eta_squared(aov_model)
print(eta_sq)

# Post-hoc comparisons
library(multcomp)
tukey_result <- TukeyHSD(aov_model)
print(tukey_result)

# Expected: F(7,498)=18.92, p<.001, η²=.210