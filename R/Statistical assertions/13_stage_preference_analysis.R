# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "χ²(1)=7.84, p=.005, φ=.089"
# Purpose: Test for significant difference in regulatory concerns between Europe and North America

library(tidyverse)
library(janitor)
library(vcd)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_REGION <- "region"
COL_REGULATORY_CRIT <- "regulatory_barrier_critical"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    region = factor(region),
    regulatory_barrier_critical = as.integer(regulatory_barrier_critical)
  ) %>%
  filter(region %in% c("Europe", "North America"))

# Contingency table
cont_table <- table(df$region, df$regulatory_barrier_critical)

# Chi-square test
chi_result <- chisq.test(cont_table)
print(chi_result)

# Effect size
assoc_stats <- assocstats(cont_table)
print(assoc_stats)

# Expected: χ²(1)=7.84, p=.005, φ=.089 