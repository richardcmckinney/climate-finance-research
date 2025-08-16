# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Among VCs, 52% (95% CI [42.8%, 61.1%]) rated technology risks as critical"
# Purpose: Calculate proportion of VCs rating technology risk as critical with Wilson CI

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_TECH_RISK_CRIT <- "tech_risk_critical"  # Binary: 1=critical, 0=not

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    tech_risk_critical = as.integer(tech_risk_critical)
  )

# Filter for VCs only
vc_data <- df %>% filter(stakeholder == "Venture Capital")

# Calculate proportion and CI
n_vc <- nrow(vc_data)
n_critical <- sum(vc_data$tech_risk_critical, na.rm = TRUE)
prop_critical <- n_critical / n_vc

# Wilson CI
ci <- BinomCI(n_critical, n_vc, conf.level = 0.95, method = "wilson")
print(paste("VCs rating tech risk critical:", round(prop_critical * 100, 1), "%"))
print(paste("95% CI: [", round(ci[,"lwr.ci"] * 100, 1), "%, ", round(ci[,"upr.ci"] * 100, 1), "%]", sep=""))
# Expected: 52% (95% CI [42.8%, 61.1%])