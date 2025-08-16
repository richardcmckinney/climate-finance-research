# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Overall, 81% (95% CI [78.9%, 83.0%]) identified market readiness as a significant challenge"
# Purpose: Calculate overall proportion identifying market readiness as significant barrier

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MARKET_READY_CRIT <- "market_readiness_critical"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(market_readiness_critical = as.integer(market_readiness_critical))

n_total <- nrow(df)
n_market_critical <- sum(df$market_readiness_critical, na.rm = TRUE)
prop_market <- n_market_critical / n_total

ci <- BinomCI(n_market_critical, n_total, conf.level = 0.95, method = "wilson")
print(paste("Market readiness as barrier:", round(prop_market * 100, 1), "%"))
print(paste("95% CI: [", round(ci[,"lwr.ci"] * 100, 1), "%, ", round(ci[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: 81% (95% CI [78.9%, 83.0%]) 