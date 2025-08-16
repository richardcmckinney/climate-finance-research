# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Networking support correlated with partnership preferences (r=.449, p<.001)"
# Purpose: Calculate correlations between support types provided and collaboration preferences

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SUPPORT_NETWORKING <- "support_networking"
COL_SUPPORT_MENTORSHIP <- "support_mentorship"
COL_SUPPORT_STRATEGIC <- "support_strategic_counsel"
COL_PREF_PARTNERSHIP <- "preference_partnerships"
COL_PREF_COINVEST <- "preference_coinvest"
COL_PREF_SYNDICATION <- "preference_syndication"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(across(all_of(c(COL_SUPPORT_NETWORKING, COL_SUPPORT_MENTORSHIP, COL_SUPPORT_STRATEGIC,
                         COL_PREF_PARTNERSHIP, COL_PREF_COINVEST, COL_PREF_SYNDICATION)), 
                as.numeric))

# Correlation 1: Networking support with partnership preferences
cor1 <- cor.test(df$support_networking, df$preference_partnerships, use = "complete.obs")
print(paste("Networking-Partnership correlation: r =", round(cor1$estimate, 3), 
            ", p =", format(cor1$p.value, scientific = FALSE)))

# Correlation 2: Mentorship with co-investment
cor2 <- cor.test(df$support_mentorship, df$preference_coinvest, use = "complete.obs")
print(paste("Mentorship-Coinvestment correlation: r =", round(cor2$estimate, 3),
            ", p =", format(cor2$p.value, scientific = FALSE)))

# Correlation 3: Strategic counsel with syndication
cor3 <- cor.test(df$support_strategic_counsel, df$preference_syndication, use = "complete.obs")
print(paste("Strategic-Syndication correlation: r =", round(cor3$estimate, 3),
            ", p =", format(cor3$p.value, scientific = FALSE)))

# Expected: Networking-Partnership r=.449, Mentorship-Coinvest r=.374, Strategic-Syndication r=.322 