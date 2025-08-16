# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "only 23% of investors report providing regulatory navigation support"
# Purpose: Calculate proportion of investors providing regulatory navigation support

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_SUPPORT_REGULATORY <- "support_regulatory_navigation"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    support_regulatory_navigation = as.integer(support_regulatory_navigation)
  )

# Filter for investor groups only
investor_groups <- c("Venture Capital", "Government Agency", "ESG Investor", "Family Office")
investor_data <- df %>% filter(stakeholder %in% investor_groups)

n_investors <- nrow(investor_data)
n_providing_reg <- sum(investor_data$support_regulatory_navigation, na.rm = TRUE)
prop_reg_support <- n_providing_reg / n_investors * 100

print(paste("Investors providing regulatory navigation support:", 
            round(prop_reg_support, 1), "%"))

# Expected: 23% of investors report providing regulatory navigation support