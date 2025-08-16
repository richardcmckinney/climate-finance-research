# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 45: "Technology gaps were identified by 73% of respondents (95% CI [69.8%, 76.1%])"
# Purpose: Calculate proportion identifying technology gaps as significant barrier

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TECH_GAP <- "barrier_technology_gaps"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(barrier_technology_gaps = as.integer(barrier_technology_gaps >= 4)) %>%
  filter(!is.na(barrier_technology_gaps))

n_total <- nrow(df)
n_tech_gap <- sum(df$barrier_technology_gaps, na.rm = TRUE)
prop_tech_gap <- n_tech_gap / n_total

# Wilson CI for proportion
ci <- BinomCI(n_tech_gap, n_total, conf.level = 0.95, method = "wilson")

print(paste("Technology gaps identified by:", round(prop_tech_gap * 100, 1), "% of respondents"))
print(paste("95% CI: [", round(ci[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci[,"upr.ci"] * 100, 1), "%]", sep=""))

# Break down by stakeholder group
gap_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    tech_gap_pct = round(mean(barrier_technology_gaps) * 100, 1),
    .groups = "drop"
  )

print("Technology gap identification by stakeholder:")
print(gap_by_stakeholder)

# Expected: 73% of respondents (95% CI [69.8%, 76.1%])