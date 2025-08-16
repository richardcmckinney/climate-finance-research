# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 35: "Competitive advantage ratings correlated significantly with funding success (r = .391, p < .001)"
# Purpose: Correlation analysis between competitive advantage assessment and funding success outcomes

library(tidyverse)
library(janitor)
library(boot)
library(broom)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_COMPETITIVE_ADVANTAGE <- "competitive_advantage_rating"
COL_FUNDING_SUCCESS <- "funding_success_score"
COL_STAKEHOLDER <- "stakeholder"
COL_SECTOR <- "sector_focus"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    competitive_advantage_rating = as.numeric(competitive_advantage_rating),
    funding_success_score = as.numeric(funding_success_score),
    stakeholder = factor(stakeholder),
    sector_focus = factor(sector_focus)
  ) %>%
  filter(!is.na(competitive_advantage_rating), !is.na(funding_success_score)) %>%
  # Remove outliers (ratings should be within expected ranges)
  filter(competitive_advantage_rating >= 1, competitive_advantage_rating <= 7,
         funding_success_score >= 0, funding_success_score <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    comp_adv_mean = round(mean(competitive_advantage_rating), 2),
    comp_adv_sd = round(sd(competitive_advantage_rating), 2),
    funding_success_mean = round(mean(funding_success_score), 2),
    funding_success_sd = round(sd(funding_success_score), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$competitive_advantage_rating, df$funding_success_score, 
                      method = "pearson", conf.level = 0.95)

print("Pearson correlation results:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval for robustness
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$competitive_advantage_rating, d$funding_success_score, 
             use = "complete.obs"))
}

set.seed(123)
boot_result <- boot(df, boot_cor, R = 10000)
boot_ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation:")
print(paste("[", round(boot_ci$percent[4], 3), ", ", round(boot_ci$percent[5], 3), "]", sep=""))

# Correlation by stakeholder group
cor_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%  # Only groups with sufficient sample size
  summarise(
    n = n(),
    correlation = round(cor(competitive_advantage_rating, funding_success_score, 
                           use = "complete.obs"), 3),
    .groups = "drop"
  )

print("Correlation by stakeholder group (n ≥ 30):")
print(cor_by_stakeholder)

# Partial correlation controlling for sector
if(length(unique(df$sector_focus)) > 1) {
  # Create dummy variables for sector
  df_with_dummies <- df %>%
    filter(!is.na(sector_focus)) %>%
    mutate(sector_numeric = as.numeric(sector_focus))
  
  partial_cor <- pcor.test(df_with_dummies$competitive_advantage_rating,
                          df_with_dummies$funding_success_score,
                          df_with_dummies$sector_numeric)
  
  print("Partial correlation (controlling for sector):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness check
spearman_result <- cor.test(df$competitive_advantage_rating, df$funding_success_score,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(non-parametric validation)"))

# Linear regression for additional context
lm_model <- lm(funding_success_score ~ competitive_advantage_rating, data = df)
lm_summary <- summary(lm_model)

print("Linear regression results:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))

# Categorize competitive advantage ratings for cross-tabulation
df_categorized <- df %>%
  mutate(
    comp_adv_category = case_when(
      competitive_advantage_rating <= 3 ~ "Low (1-3)",
      competitive_advantage_rating <= 5 ~ "Moderate (4-5)",
      competitive_advantage_rating <= 7 ~ "High (6-7)"
    ),
    funding_success_category = case_when(
      funding_success_score <= 3 ~ "Low Success",
      funding_success_score <= 7 ~ "Moderate Success",
      funding_success_score <= 10 ~ "High Success"
    )
  )

cross_tab <- table(df_categorized$comp_adv_category, df_categorized$funding_success_category)
print("Cross-tabulation of competitive advantage and funding success categories:")
print(cross_tab)
print("Row percentages:")
print(round(prop.table(cross_tab, 1) * 100, 1))

# Expected: r = .391, p < .001