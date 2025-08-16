# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 59: "Cost-benefit analysis methods usage correlated with investment decision confidence (r = .384, p < .001)"
# Purpose: Correlation analysis between cost-benefit analysis methods usage and investment decision confidence levels

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_CBA_METHODS <- "cost_benefit_analysis_methods_usage"
COL_DECISION_CONFIDENCE <- "investment_decision_confidence_rating"
COL_STAKEHOLDER <- "stakeholder"
COL_INVESTMENT_EXPERIENCE <- "investment_experience_years"
COL_DEAL_COMPLEXITY <- "deal_complexity_rating"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    cost_benefit_analysis_methods_usage = as.numeric(cost_benefit_analysis_methods_usage),
    investment_decision_confidence_rating = as.numeric(investment_decision_confidence_rating),
    stakeholder = factor(stakeholder),
    investment_experience_years = as.numeric(investment_experience_years),
    deal_complexity_rating = as.numeric(deal_complexity_rating)
  ) %>%
  filter(!is.na(cost_benefit_analysis_methods_usage), 
         !is.na(investment_decision_confidence_rating)) %>%
  # Ensure variables are within expected ranges
  filter(cost_benefit_analysis_methods_usage >= 1, cost_benefit_analysis_methods_usage <= 10,
         investment_decision_confidence_rating >= 1, investment_decision_confidence_rating <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    cba_mean = round(mean(cost_benefit_analysis_methods_usage), 2),
    cba_sd = round(sd(cost_benefit_analysis_methods_usage), 2),
    confidence_mean = round(mean(investment_decision_confidence_rating), 2),
    confidence_sd = round(sd(investment_decision_confidence_rating), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$cost_benefit_analysis_methods_usage, 
                      df$investment_decision_confidence_rating,
                      method = "pearson", conf.level = 0.95)

print("Cost-benefit analysis × Investment decision confidence correlation:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$cost_benefit_analysis_methods_usage, d$investment_decision_confidence_rating,
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
    correlation = cor.test(.$cost_benefit_analysis_methods_usage, .$investment_decision_confidence_rating)
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

# Partial correlation controlling for investment experience
if(sum(!is.na(df$investment_experience_years)) > 50) {
  df_experience <- df %>%
    filter(!is.na(investment_experience_years))
  
  partial_cor <- pcor.test(df_experience$cost_benefit_analysis_methods_usage,
                          df_experience$investment_decision_confidence_rating,
                          df_experience$investment_experience_years)
  
  print("Partial correlation (controlling for investment experience):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness
spearman_result <- cor.test(df$cost_benefit_analysis_methods_usage,
                           df$investment_decision_confidence_rating,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(investment_decision_confidence_rating ~ cost_benefit_analysis_methods_usage, 
               data = df)
lm_summary <- summary(lm_model)

print("Linear regression: Decision confidence ~ CBA methods:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))

# Multiple regression with control variables
control_vars <- c("investment_experience_years", "deal_complexity_rating")
available_controls <- control_vars[control_vars %in% names(df)]

if(length(available_controls) > 0) {
  df_controls <- df %>%
    select(investment_decision_confidence_rating, cost_benefit_analysis_methods_usage, 
           stakeholder, all_of(available_controls)) %>%
    filter(complete.cases(.))
  
  if(nrow(df_controls) > 100) {
    formula_str <- paste("investment_decision_confidence_rating ~ cost_benefit_analysis_methods_usage + stakeholder +", 
                         paste(available_controls, collapse = " + "))
    
    lm_multiple <- lm(as.formula(formula_str), data = df_controls)
    multiple_summary <- summary(lm_multiple)
    
    print("Multiple regression with controls:")
    print(multiple_summary)
    
    # Extract CBA coefficient
    cba_coef <- multiple_summary$coefficients["cost_benefit_analysis_methods_usage", ]
    print("CBA methods coefficient (controlled):")
    print(paste("β =", round(cba_coef[1], 3)))
    print(paste("SE =", round(cba_coef[2], 3)))
    print(paste("p =", format(cba_coef[4], scientific = TRUE)))
  }
}

# Cross-tabulation for categorical analysis
df_categories <- df %>%
  mutate(
    cba_level = case_when(
      cost_benefit_analysis_methods_usage <= 4 ~ "Low CBA Usage",
      cost_benefit_analysis_methods_usage <= 7 ~ "Moderate CBA Usage",
      cost_benefit_analysis_methods_usage <= 10 ~ "High CBA Usage"
    ),
    confidence_level = case_when(
      investment_decision_confidence_rating <= 4 ~ "Low Confidence",
      investment_decision_confidence_rating <= 7 ~ "Moderate Confidence",
      investment_decision_confidence_rating <= 10 ~ "High Confidence"
    )
  )

cross_tab <- table(df_categories$cba_level, df_categories$confidence_level)
print("Cross-tabulation of CBA usage and decision confidence levels:")
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

# Quartile analysis for CBA usage
df_quartiles <- df %>%
  mutate(
    cba_quartile = ntile(cost_benefit_analysis_methods_usage, 4),
    cba_quartile_label = case_when(
      cba_quartile == 1 ~ "Q1 (Lowest CBA Usage)",
      cba_quartile == 2 ~ "Q2 (Low-Moderate)",
      cba_quartile == 3 ~ "Q3 (Moderate-High)",
      cba_quartile == 4 ~ "Q4 (Highest CBA Usage)"
    )
  )

quartile_analysis <- df_quartiles %>%
  group_by(cba_quartile_label) %>%
  summarise(
    n = n(),
    cba_range = paste(round(min(cost_benefit_analysis_methods_usage), 1), "-",
                     round(max(cost_benefit_analysis_methods_usage), 1)),
    mean_confidence = round(mean(investment_decision_confidence_rating), 2),
    sd_confidence = round(sd(investment_decision_confidence_rating), 2),
    .groups = "drop"
  )

print("Decision confidence by CBA usage quartiles:")
print(quartile_analysis)

# ANOVA across quartiles
anova_quartiles <- lm(investment_decision_confidence_rating ~ factor(cba_quartile), 
                     data = df_quartiles)
anova_summary <- summary(anova_quartiles)

print("ANOVA across CBA usage quartiles:")
print(paste("F(", anova_summary$df[1], ",", anova_summary$df[2], ") =",
            round(anova_summary$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_summary$fstatistic[1], 
                                 anova_summary$df[1], 
                                 anova_summary$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Analysis by deal complexity (if available)
if(sum(!is.na(df$deal_complexity_rating)) > 50) {
  df_complexity <- df %>%
    filter(!is.na(deal_complexity_rating)) %>%
    mutate(
      complexity_level = case_when(
        deal_complexity_rating <= 4 ~ "Low Complexity",
        deal_complexity_rating <= 7 ~ "Moderate Complexity",
        deal_complexity_rating >= 8 ~ "High Complexity"
      )
    )
  
  complexity_analysis <- df_complexity %>%
    group_by(complexity_level) %>%
    summarise(
      n = n(),
      mean_cba = round(mean(cost_benefit_analysis_methods_usage), 2),
      mean_confidence = round(mean(investment_decision_confidence_rating), 2),
      correlation = round(cor(cost_benefit_analysis_methods_usage, 
                             investment_decision_confidence_rating), 3),
      .groups = "drop"
    )
  
  print("Analysis by deal complexity:")
  print(complexity_analysis)
}

# High confidence threshold analysis (ratings ≥ 8)
high_confidence <- df_categories %>%
  filter(confidence_level == "High Confidence") %>%
  group_by(cba_level) %>%
  summarise(
    n = n(),
    percentage = round(n / nrow(df_categories %>% 
                               filter(cba_level == unique(cba_level))) * 100, 1),
    .groups = "drop"
  )

print("High decision confidence by CBA usage level:")
print(high_confidence)

# Standardized regression coefficient (beta)
df_standardized <- df %>%
  mutate(
    cba_z = scale(cost_benefit_analysis_methods_usage)[,1],
    confidence_z = scale(investment_decision_confidence_rating)[,1]
  )

lm_standardized <- lm(confidence_z ~ cba_z, data = df_standardized)
beta_coefficient <- coef(lm_standardized)[2]
print(paste("Standardized β =", round(beta_coefficient, 3)))

# Curvilinear relationship test
lm_quadratic <- lm(investment_decision_confidence_rating ~ 
                   cost_benefit_analysis_methods_usage + 
                   I(cost_benefit_analysis_methods_usage^2), 
                   data = df)

quadratic_summary <- summary(lm_quadratic)
print("Quadratic relationship test:")
print(paste("R-squared (quadratic):", round(quadratic_summary$r.squared, 3)))
print(paste("R-squared improvement:", round(quadratic_summary$r.squared - lm_summary$r.squared, 4)))

# Threshold analysis: CBA usage impact on confidence
threshold_analysis <- df %>%
  mutate(
    high_cba_usage = cost_benefit_analysis_methods_usage >= 7,
    high_confidence = investment_decision_confidence_rating >= 7
  )

threshold_table <- table(threshold_analysis$high_cba_usage, threshold_analysis$high_confidence)
print("High CBA usage vs High confidence crosstab:")
print(threshold_table)
print("Row percentages:")
print(round(prop.table(threshold_table, 1) * 100, 1))

# Odds ratio calculation
if(all(dim(threshold_table) == c(2, 2))) {
  odds_ratio <- (threshold_table[2,2] * threshold_table[1,1]) / 
                (threshold_table[2,1] * threshold_table[1,2])
  print(paste("Odds ratio (high CBA → high confidence):", round(odds_ratio, 2)))
}

# Experience-stratified analysis
if(sum(!is.na(df$investment_experience_years)) > 100) {
  df_exp_strata <- df %>%
    filter(!is.na(investment_experience_years)) %>%
    mutate(
      experience_level = case_when(
        investment_experience_years <= 5 ~ "Low Experience (≤5 years)",
        investment_experience_years <= 15 ~ "Moderate Experience (6-15 years)",
        investment_experience_years > 15 ~ "High Experience (>15 years)"
      )
    )
  
  exp_correlations <- df_exp_strata %>%
    group_by(experience_level) %>%
    filter(n() >= 30) %>%
    summarise(
      n = n(),
      correlation = round(cor(cost_benefit_analysis_methods_usage, 
                             investment_decision_confidence_rating), 3),
      .groups = "drop"
    )
  
  print("CBA-Confidence correlation by experience level:")
  print(exp_correlations)
}

# Expected: r = .384, p < .001