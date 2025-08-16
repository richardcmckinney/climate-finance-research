# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 26: "Support mechanism effectiveness (α = .86)"
# Purpose: Calculate internal consistency reliability for support mechanism items

library(tidyverse)
library(janitor)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - support mechanism items
SUPPORT_ITEMS <- c("support_networking", "support_mentorship", "support_strategic_counsel",
                   "support_training", "support_funding", "support_partnerships")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(SUPPORT_ITEMS)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# Calculate Cronbach's alpha
alpha_result <- alpha(df)
print(paste("Cronbach's alpha for support mechanisms:", 
            round(alpha_result$total$raw_alpha, 2)))

# Expected: α = .86