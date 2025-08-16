# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 90: "IP protection importance predicted by technology stage (β = .31, p < .001) and funding amount (β = .24, p < .01)"
# Purpose: Multiple regression analysis predicting IP protection importance

library(tidyverse)
library(janitor)
library(car)
library(broom)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_IP_PROTECTION <- "intellectual_property_protection_importance"
COL_TECH_STAGE <- "technology_stage_numeric"
COL_FUNDING_AMOUNT <- "funding_amount_log"
COL_STAKEHOLDER <- "stakeholder"
COL_EXPERIENCE <- "experience_years"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    intellectual_property_protection_importance = as.numeric(intellectual_property_protection_importance),
    technology_stage_numeric = as.numeric(technology_stage_numeric),
    funding_amount_log = log(as.numeric(funding_amount_numeric) + 1),
    stakeholder = factor(stakeholder),
    experience_years = as.numeric(experience_years)
  ) %>%
  filter(!is.na(intellectual_property_protection_importance),
         !is.na(technology_stage_numeric),
         !is.na(funding_amount_log))

# Multiple regression model
model <- lm(intellectual_property_protection_importance ~ 
            technology_stage_numeric + funding_amount_log + 
            stakeholder + experience_years, data = df)

# Model summary
model_summary <- summary(model)
print("Multiple Regression Results:")
print(model_summary)

# Extract standardized coefficients
standardized_coefs <- tidy(model, conf.int = TRUE) %>%
  filter(term %in% c("technology_stage_numeric", "funding_amount_log"))

print("Key predictors:")
for(i in 1:nrow(standardized_coefs)) {
  coef_data <- standardized_coefs[i, ]
  print(paste(coef_data$term, ": β =", round(coef_data$estimate, 2),
              ", p =", format(coef_data$p.value, scientific = TRUE)))
}

# Model diagnostics
print(paste("R-squared:", round(model_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(model_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(model_summary$fstatistic[1], 2)))

# Check assumptions
print("Model diagnostics:")
print(paste("Durbin-Watson test:", round(durbinWatsonTest(model)$dw, 3)))

# Expected: Technology stage β = .31, p < .001; Funding amount β = .24, p < .01