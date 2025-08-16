# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 29: "Funding stage distribution: 34% pre-seed (95% CI [30.8%, 37.4%])"
# Purpose: Calculate proportion in pre-seed funding stage with Wilson confidence interval

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_FUNDING_STAGE <- "funding_stage"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(funding_stage = factor(funding_stage)) %>%
  filter(!is.na(funding_stage))

# Calculate pre-seed proportion
n_total <- nrow(df)
n_preseed <- sum(df$funding_stage == "Pre-seed", na.rm = TRUE)
prop_preseed <- n_preseed / n_total

# Wilson CI for proportion
ci <- BinomCI(n_preseed, n_total, conf.level = 0.95, method = "wilson")

print(paste("Pre-seed funding stage:", round(prop_preseed * 100, 1), "%"))
print(paste("95% CI: [", round(ci[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci[,"upr.ci"] * 100, 1), "%]", sep=""))

# Show full distribution
stage_dist <- table(df$funding_stage)
prop_dist <- prop.table(stage_dist) * 100
print("Full funding stage distribution:")
print(round(prop_dist, 1))

# Expected: 34% pre-seed (95% CI [30.8%, 37.4%])