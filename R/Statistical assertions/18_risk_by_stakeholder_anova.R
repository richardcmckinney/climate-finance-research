# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "physical climate risks and operational risks showed the highest correlation (r=.685, p<.001, 95% CI [.651, .716])"
# Purpose: Calculate correlation between physical and operational risk ratings

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_PHYSICAL_RISK_MEAN <- "physical_risk_rating"
COL_OPERATIONAL_RISK_MEAN <- "operational_risk_rating"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    physical_risk_rating = as.numeric(physical_risk_rating),
    operational_risk_rating = as.numeric(operational_risk_rating)
  )

# Correlation test
cor_result <- cor.test(df$physical_risk_rating, df$operational_risk_rating,
                       method = "pearson", use = "complete.obs")
print(cor_result)
print(paste("Correlation: r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ",
            round(cor_result$conf.int[2], 3), "]"))

# Expected: r=.685, p<.001, 95% CI [.651, .716]