# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "71% of venture capitalists (95% CI [62.4%, 78.6%]) rated international scalability as critical"
# Purpose: Calculate proportion of VCs rating international scalability as critical/very important

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_INTL_SCALE_CRIT <- "intl_scalability_critical"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    intl_scalability_critical = as.integer(intl_scalability_critical)
  )

# VCs only
vc_data <- df %>% filter(stakeholder == "Venture Capital")
n_vc <- nrow(vc_data)
n_intl_critical <- sum(vc_data$intl_scalability_critical, na.rm = TRUE)
prop_intl <- n_intl_critical / n_vc

ci <- BinomCI(n_intl_critical, n_vc, conf.level = 0.95, method = "wilson")
print(paste("VCs rating international scalability critical:", round(prop_intl * 100, 1), "%"))
print(paste("95% CI: [", round(ci[,"lwr.ci"] * 100, 1), "%, ", round(ci[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: 71% (95% CI [62.4%, 78.6%])