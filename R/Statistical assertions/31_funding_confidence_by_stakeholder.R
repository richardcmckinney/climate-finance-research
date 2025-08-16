# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 31: "Funding confidence differed significantly by stakeholder (F(3,1303) = 18.4, p < .001, η² = .041)"
# Purpose: Test differences in funding confidence across stakeholder groups using one-way ANOVA

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_FUNDING_CONFIDENCE <- "funding_confidence_rating"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder, levels = c("Venture Capital", "Government Agency", 
                                               "Entrepreneur", "Limited Partner")),
    funding_confidence_rating = as.numeric(funding_confidence_rating)
  ) %>%
  filter(!is.na(stakeholder), !is.na(funding_confidence_rating))

# Descriptive statistics by stakeholder group
desc_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_confidence = round(mean(funding_confidence_rating), 2),
    sd_confidence = round(sd(funding_confidence_rating), 2),
    se_confidence = round(sd(funding_confidence_rating) / sqrt(n()), 3),
    .groups = "drop"
  )

print("Funding confidence by stakeholder group:")
print(desc_stats)

# Check assumptions for ANOVA
# Levene's test for homogeneity of variances
levene_test <- leveneTest(funding_confidence_rating ~ stakeholder, data = df)
print("Levene's test for homogeneity of variances:")
print(levene_test)

# One-way ANOVA
anova_model <- lm(funding_confidence_rating ~ stakeholder, data = df)
anova_result <- Anova(anova_model, type = "III")

# Calculate effect size (eta squared)
eta_squared_result <- eta_squared(anova_result)

print("One-way ANOVA Results:")
print(anova_result)
print(paste("Effect size (η²):", round(eta_squared_result$Eta2_partial[1], 3)))

# Post-hoc comparisons with Bonferroni correction
if (anova_result$`Pr(>F)`[1] < 0.05) {
  posthoc <- emmeans(anova_model, ~ stakeholder)
  pairwise_comp <- pairs(posthoc, adjust = "bonferroni")
  
  print("Post-hoc pairwise comparisons (Bonferroni corrected):")
  print(pairwise_comp)
  
  # Extract effect sizes for significant comparisons
  print("Estimated marginal means:")
  print(posthoc)
}

# Model diagnostics
print("Model fit statistics:")
print(paste("R-squared:", round(summary(anova_model)$r.squared, 3)))
print(paste("Sample size:", nrow(df)))

# Expected: F(3,1303) = 18.4, p < .001, η² = .041