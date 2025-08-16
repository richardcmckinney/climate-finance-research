# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Cronbach's alpha: Barrier perceptions composite (α = .84)"
# Purpose: Calculate internal consistency reliability for barrier perception items

library(tidyverse)
library(janitor)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - barrier perception items
BARRIER_ITEMS <- c("barrier_tech", "barrier_market", "barrier_regulatory",
                   "barrier_financial", "barrier_ecosystem")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(BARRIER_ITEMS)) %>%
  mutate(across(everything(), as.numeric)) %>%
  na.omit()

# Calculate Cronbach's alpha
alpha_result <- alpha(df)
print(paste("Cronbach's alpha for barrier perceptions:", round(alpha_result$total$raw_alpha, 2)))

# Expected: α = .84