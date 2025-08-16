# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 27: "Barrier assessment scale (α = .89)"
# Purpose: Calculate internal consistency reliability for barrier assessment items

library(tidyverse)
library(janitor)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - barrier assessment items
BARRIER_ITEMS <- c("barrier_regulatory", "barrier_market_access", "barrier_funding",
                   "barrier_technology_gaps", "barrier_talent", "barrier_infrastructure",
                   "barrier_policy", "barrier_awareness")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(BARRIER_ITEMS)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# Calculate Cronbach's alpha
alpha_result <- alpha(df)
print(paste("Cronbach's alpha for barrier assessment:", 
            round(alpha_result$total$raw_alpha, 2)))

# Expected: α = .89