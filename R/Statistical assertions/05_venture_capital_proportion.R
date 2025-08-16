# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "31% of government agencies (95% CI [18.5%, 46.7%])"
# Purpose: Calculate proportion of government agencies rating technology risk as critical

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_TECH_RISK_CRIT <- "tech_risk_critical"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    tech_risk_critical = as.integer(tech_risk_critical)
  )

# Filter for Government only
gov_data <- df %>% filter(stakeholder == "Government Agency")

n_gov <- nrow(gov_data)
n_critical <- sum(gov_data$tech_risk_critical, na.rm = TRUE)
prop_critical <- n_critical / n_gov

ci <- BinomCI(n_critical, n_gov, conf.level = 0.95, method = "wilson")
print(paste("Government rating tech risk critical:", round(prop_critical * 100, 1), "%"))
print(paste("95% CI: [", round(ci[,"lwr.ci"] * 100, 1), "%, ", round(ci[,"upr.ci"] * 100, 1), "%]", sep=""))
# Expected: 31% (95% CI [18.5%, 46.7%]) 