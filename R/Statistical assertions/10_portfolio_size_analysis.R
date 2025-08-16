# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "VCs (86%, 95% CI [78.9%, 91.5%]), government agencies (73%, 95% CI [57.5%, 85.0%]), entrepreneurs (78%, 95% CI [72.8%, 82.6%])"
# Purpose: Calculate market readiness barrier proportions by stakeholder group

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_MARKET_READY_CRIT <- "market_readiness_critical"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    market_readiness_critical = as.integer(market_readiness_critical)
  )

# Calculate by group
groups <- c("Venture Capital", "Government Agency", "Entrepreneur")
for(grp in groups) {
  grp_data <- df %>% filter(stakeholder == grp)
  n_grp <- nrow(grp_data)
  n_critical <- sum(grp_data$market_readiness_critical, na.rm = TRUE)
  prop <- n_critical / n_grp
  ci <- BinomCI(n_critical, n_grp, conf.level = 0.95, method = "wilson")
  
  print(paste(grp, ": ", round(prop * 100, 1), "% ",
              "(95% CI [", round(ci[,"lwr.ci"] * 100, 1), "%, ", 
              round(ci[,"upr.ci"] * 100, 1), "%])", sep=""))
}

# Expected: VCs 86% [78.9%, 91.5%], Govt 73% [57.5%, 85.0%], Entrepreneurs 78% [72.8%, 82.6%]