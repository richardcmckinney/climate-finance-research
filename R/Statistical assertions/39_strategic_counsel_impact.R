# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 39: "Strategic counsel impact showed moderate correlation with decision confidence (r = .327, 95% CI [.282, .371])"
# Purpose: Correlation analysis between strategic counsel impact and investment/funding decision confidence

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STRATEGIC_COUNSEL <- "strategic_counsel_impact_rating"
COL_DECISION_CONFIDENCE <- "decision_confidence_rating"
COL_STAKEHOLDER <- "stakeholder"
COL_EXPERIENCE <- "experience_years"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    strategic_counsel_impact_rating = as.numeric(strategic_counsel_impact_rating),
    decision_confidence_rating = as.numeric(decision_confidence_rating),
    stakeholder = factor(stakeholder),
    experience_years = as.numeric(experience_years)
  ) %>%
  filter(!is.na(strategic_counsel_impact_rating), 
         !is.na(decision_confidence_rating)) %>%
  # Ensure variables are within expected ranges
  filter(strategic_counsel_impact_rating >= 1, strategic_counsel_impact_rating <= 7,
         decision_confidence_rating >= 1, decision_confidence_rating <= 7)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    counsel_mean = round(mean(strategic_counsel_impact_rating), 2),
    counsel_sd = round(sd(strategic_counsel_impact_rating), 2),
    confidence_mean = round(mean(decision_confidence_rating), 2),
    confidence_sd = round(sd(decision_confidence_rating), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$strategic_counsel_impact_rating, 
                      df$decision_confidence_rating,
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
  return(cor(d$strategic_counsel_impact_rating, d$decision_confidence_rating,
             use = "complete.obs"))
}

set.seed(123)
boot_result <- boot(df, boot_cor, R = 10000)
boot_ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation:")
print(paste("[", round(boot_ci$percent[4], 3), ", ", round(boot_ci$percent[5], 3), "]", sep=""))

# Effect size interpretation
cor_magnitude <- abs(cor_result$estimate)
effect_size_interpretation <- case_when(
  cor_magnitude < 0.1 ~ "Negligible",
  cor_magnitude < 0.3 ~ "Small", 
  cor_magnitude < 0.5 ~ "Medium",
  cor_magnitude < 0.7 ~ "Large",
  TRUE ~ "Very Large"
)

print(paste("Effect size interpretation:", effect_size_interpretation))

# Correlation by stakeholder group
cor_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%
  do(
    correlation = cor.test(.$strategic_counsel_impact_rating, .$decision_confidence_rating)
  ) %>%
  mutate(
    n = df %>% filter(stakeholder == .$stakeholder[1]) %>% nrow(),
    r = map_dbl(correlation, ~ round(.x$estimate, 3)),
    ci_lower = map_dbl(correlation, ~ round(.x$conf.int[1], 3)),
    ci_upper = map_dbl(correlation, ~ round(.x$conf.int[2], 3)),
    p_value = map_dbl(correlation, ~ .x$p.value)
  ) %>%
  select(-correlation)

print("Correlation by stakeholder group (n ≥ 30):")
print(cor_by_stakeholder)

# Partial correlation controlling for experience
if(sum(!is.na(df$experience_years)) > 50) {
  df_experience <- df %>%
    filter(!is.na(experience_years))
  
  partial_cor <- pcor.test(df_experience$strategic_counsel_impact_rating,
                          df_experience$decision_confidence_rating,
                          df_experience$experience_years)
  
  print("Partial correlation (controlling for experience):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness
spearman_result <- cor.test(df$strategic_counsel_impact_rating,
                           df$decision_confidence_rating,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(decision_confidence_rating ~ strategic_counsel_impact_rating, 
               data = df)
lm_summary <- summary(lm_model)

print("Linear regression results:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))

# Standardized regression coefficient (beta)
df_standardized <- df %>%
  mutate(
    counsel_z = scale(strategic_counsel_impact_rating)[,1],
    confidence_z = scale(decision_confidence_rating)[,1]
  )

lm_standardized <- lm(confidence_z ~ counsel_z, data = df_standardized)
beta_coefficient <- coef(lm_standardized)[2]
print(paste("Standardized β:", round(beta_coefficient, 3)))

# Cross-tabulation for categorical analysis
df_categories <- df %>%
  mutate(
    counsel_level = case_when(
      strategic_counsel_impact_rating <= 3 ~ "Low Impact",
      strategic_counsel_impact_rating <= 5 ~ "Moderate Impact", 
      strategic_counsel_impact_rating <= 7 ~ "High Impact"
    ),
    confidence_level = case_when(
      decision_confidence_rating <= 3 ~ "Low Confidence",
      decision_confidence_rating <= 5 ~ "Moderate Confidence",
      decision_confidence_rating <= 7 ~ "High Confidence"
    )
  )

cross_tab <- table(df_categories$counsel_level, df_categories$confidence_level)
print("Cross-tabulation of strategic counsel impact and decision confidence levels:")
print(cross_tab)
print("Row percentages:")
print(round(prop.table(cross_tab, 1) * 100, 1))

# Chi-square test for categorical association
chi_test <- chisq.test(cross_tab)
print(paste("Chi-square test: χ² =", round(chi_test$statistic, 2),
            ", df =", chi_test$parameter,
            ", p =", format(chi_test$p.value, scientific = TRUE)))

# Cramér's V for effect size
cramers_v <- sqrt(chi_test$statistic / (sum(cross_tab) * (min(dim(cross_tab)) - 1)))
print(paste("Cramér's V:", round(cramers_v, 3)))

# Detailed breakdown by stakeholder and impact level
impact_breakdown <- df_categories %>%
  group_by(stakeholder, counsel_level) %>%
  summarise(
    n = n(),
    mean_confidence = round(mean(decision_confidence_rating), 2),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(stakeholder, counsel_level)

if(nrow(impact_breakdown) > 0) {
  print("Decision confidence by stakeholder and counsel impact level (n ≥ 10):")
  print(impact_breakdown)
}

# Confidence in correlation estimate
cor_se <- sqrt((1 - cor_result$estimate^2) / (nrow(df) - 2))
cor_z <- 0.5 * log((1 + cor_result$estimate) / (1 - cor_result$estimate))
print(paste("Standard error of correlation:", round(cor_se, 4)))

# Expected: r = .327, 95% CI [.282, .371]