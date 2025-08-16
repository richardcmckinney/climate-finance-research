# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "One-way ANOVA revealed significant differences across groups (F(7,651)=3.84, p<.001, η²=.040)"
# Purpose: Test for differences in technology risk ratings across all stakeholder groups

library(tidyverse)
library(janitor)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_TECH_RISK_MEAN <- "tech_risk_rating"  # Continuous 1-7 scale

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    tech_risk_rating = as.numeric(tech_risk_rating)
  ) %>%
  filter(!is.na(tech_risk_rating))

# One-way ANOVA
aov_model <- aov(tech_risk_rating ~ stakeholder, data = df)
summary(aov_model)

# Effect size
eta_sq <- eta_squared(aov_model)
print(eta_sq)

# Expected: F(7,651)=3.84, p<.001, η²=.040