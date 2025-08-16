# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 28: "Technology readiness levels differed significantly (χ²(12) = 47.3, p < .001)"
# Purpose: Test differences in technology readiness levels across stakeholder groups

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_TRL <- "technology_readiness_level"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    technology_readiness_level = factor(technology_readiness_level, levels = 1:9)
  ) %>%
  filter(!is.na(stakeholder), !is.na(technology_readiness_level))

# Create contingency table
tbl <- table(df$stakeholder, df$technology_readiness_level)
print("Technology Readiness Level by Stakeholder:")
print(tbl)

# Chi-square test
chi_test <- chisq.test(tbl)
print(paste("Chi-square test: χ²(", chi_test$parameter, ") = ", 
            round(chi_test$statistic, 1), ", p = ", 
            format(chi_test$p.value, scientific = TRUE), sep = ""))

# Expected: χ²(12) = 47.3, p < .001