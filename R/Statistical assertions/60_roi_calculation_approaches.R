# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 60: "ROI calculation approaches varied significantly by stakeholder (F(3,1303) = 12.7, p < .001, η² = .029)"
# Purpose: Test differences in ROI calculation preferences across stakeholder groups

library(tidyverse)
library(janitor)
library(car)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
COL_ROI_METHOD <- "roi_calculation_method_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    roi_calculation_method_score = as.numeric(roi_calculation_method_score)
  ) %>%
  filter(!is.na(stakeholder), !is.na(roi_calculation_method_score))

# Descriptive statistics by group
desc_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_roi = round(mean(roi_calculation_method_score), 2),
    sd_roi = round(sd(roi_calculation_method_score), 2),
    .groups = "drop"
  )

print("ROI calculation approach scores by stakeholder:")
print(desc_stats)

# One-way ANOVA
anova_model <- lm(roi_calculation_method_score ~ stakeholder, data = df)
anova_result <- Anova(anova_model, type = "III")

# Calculate effect size (eta squared)
eta_squared <- eta_squared(anova_result)

print("ANOVA Results:")
print(anova_result)
print(paste("Effect size (η²):", round(eta_squared$Eta2_partial[1], 3)))

# Post-hoc comparisons with Bonferroni correction
if (anova_result$`Pr(>F)`[1] < 0.05) {
  posthoc <- pairwise.t.test(df$roi_calculation_method_score, df$stakeholder, 
                            p.adjust.method = "bonferroni")
  print("Post-hoc comparisons (Bonferroni corrected):")
  print(posthoc)
}

# Expected: F(3,1303) = 12.7, p < .001, η² = .029