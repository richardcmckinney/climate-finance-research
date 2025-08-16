# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "86% (95% CI [81.7%, 89.5%]) [European] identifying regulatory impediments... 78% (95% CI [73.2%, 82.3%]) [North American]"
# Purpose: Compare regulatory barrier perceptions between Europe and North America

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_REGION <- "region"
COL_REGULATORY_CRIT <- "regulatory_barrier_critical"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    region = factor(region),
    regulatory_barrier_critical = as.integer(regulatory_barrier_critical)
  )

# Europe
europe_data <- df %>% filter(region == "Europe")
n_europe <- nrow(europe_data)
n_reg_europe <- sum(europe_data$regulatory_barrier_critical, na.rm = TRUE)
prop_europe <- n_reg_europe / n_europe
ci_europe <- BinomCI(n_reg_europe, n_europe, conf.level = 0.95, method = "wilson")

print(paste("Europe regulatory barriers:", round(prop_europe * 100, 1), "%",
            "(95% CI [", round(ci_europe[,"lwr.ci"] * 100, 1), "%, ",
            round(ci_europe[,"upr.ci"] * 100, 1), "%])"))

# North America
na_data <- df %>% filter(region == "North America")
n_na <- nrow(na_data)
n_reg_na <- sum(na_data$regulatory_barrier_critical, na.rm = TRUE)
prop_na <- n_reg_na / n_na
ci_na <- BinomCI(n_reg_na, n_na, conf.level = 0.95, method = "wilson")

print(paste("North America regulatory barriers:", round(prop_na * 100, 1), "%",
            "(95% CI [", round(ci_na[,"lwr.ci"] * 100, 1), "%, ",
            round(ci_na[,"upr.ci"] * 100, 1), "%])"))

# Expected: Europe 86% [81.7%, 89.5%], North America 78% [73.2%, 82.3%] 