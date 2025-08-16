# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 62: "Discount rate applications correlated with risk assessment methodology (r = .276, p < .001)"
# Purpose: Correlation analysis between discount rate application practices and risk assessment methodology sophistication

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_DISCOUNT_RATE_APPLICATION <- "discount_rate_application_sophistication"
COL_RISK_ASSESSMENT_METHODOLOGY <- "risk_assessment_methodology_score"
COL_STAKEHOLDER <- "stakeholder"
COL_FINANCIAL_MODELING_EXPERIENCE <- "financial_modeling_experience_years"
COL_INVESTMENT_SIZE <- "typical_investment_size_numeric"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    discount_rate_application_sophistication = as.numeric(discount_rate_application_sophistication),
    risk_assessment_methodology_score = as.numeric(risk_assessment_methodology_score),
    stakeholder = factor(stakeholder),
    financial_modeling_experience_years = as.numeric(financial_modeling_experience_years),
    typical_investment_size_numeric = as.numeric(typical_investment_size_numeric)
  ) %>%
  filter(!is.na(discount_rate_application_sophistication), 
         !is.na(risk_assessment_methodology_score)) %>%
  # Ensure variables are within expected ranges
  filter(discount_rate_application_sophistication >= 1, discount_rate_application_sophistication <= 10,
         risk_assessment_methodology_score >= 1, risk_assessment_methodology_score <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    discount_rate_mean = round(mean(discount_rate_application_sophistication), 2),
    discount_rate_sd = round(sd(discount_rate_application_sophistication), 2),
    risk_assessment_mean = round(mean(risk_assessment_methodology_score), 2),
    risk_assessment_sd = round(sd(risk_assessment_methodology_score), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$discount_rate_application_sophistication, 
                      df$risk_assessment_methodology_score,
                      method = "pearson", conf.level = 0.95)

print("Discount rate application × Risk assessment methodology correlation:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$discount_rate_application_sophistication, d$risk_assessment_methodology_score,
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
    correlation = cor.test(.$discount_rate_application_sophistication, .$risk_assessment_methodology_score)
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

# Partial correlation controlling for financial modeling experience
if(sum(!is.na(df$financial_modeling_experience_years)) > 50) {
  df_experience <- df %>%
    filter(!is.na(financial_modeling_experience_years))
  
  partial_cor <- pcor.test(df_experience$discount_rate_application_sophistication,
                          df_experience$risk_assessment_methodology_score,
                          df_experience$financial_modeling_experience_years)
  
  print("Partial correlation (controlling for financial modeling experience):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness
spearman_result <- cor.test(df$discount_rate_application_sophistication,
                           df$risk_assessment_methodology_score,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(risk_assessment_methodology_score ~ discount_rate_application_sophistication, 
               data = df)
lm_summary <- summary(lm_model)

print("Linear regression: Risk assessment ~ Discount rate application:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))

# Multiple regression with control variables
control_vars <- c("financial_modeling_experience_years")
if("typical_investment_size_numeric" %in% names(df)) {
  # Log transform investment size due to likely skewness
  df <- df %>%
    mutate(investment_size_log = log(typical_investment_size_numeric + 1))
  control_vars <- c(control_vars, "investment_size_log")
}

df_controls <- df %>%
  select(risk_assessment_methodology_score, discount_rate_application_sophistication, 
         stakeholder, all_of(control_vars)) %>%
  filter(complete.cases(.))

if(nrow(df_controls) > 100) {
  formula_str <- paste("risk_assessment_methodology_score ~ discount_rate_application_sophistication + stakeholder +", 
                       paste(control_vars, collapse = " + "))
  
  lm_multiple <- lm(as.formula(formula_str), data = df_controls)
  multiple_summary <- summary(lm_multiple)
  
  print("Multiple regression with controls:")
  print(multiple_summary)
  
  # Extract discount rate coefficient
  discount_coef <- multiple_summary$coefficients["discount_rate_application_sophistication", ]
  print("Discount rate application coefficient (controlled):")
  print(paste("β =", round(discount_coef[1], 3)))
  print(paste("SE =", round(discount_coef[2], 3)))
  print(paste("p =", format(discount_coef[4], scientific = TRUE)))
}

# Cross-tabulation for categorical analysis
df_categories <- df %>%
  mutate(
    discount_level = case_when(
      discount_rate_application_sophistication <= 4 ~ "Basic Application",
      discount_rate_application_sophistication <= 7 ~ "Moderate Application",
      discount_rate_application_sophistication <= 10 ~ "Advanced Application"
    ),
    risk_methodology_level = case_when(
      risk_assessment_methodology_score <= 4 ~ "Basic Methodology",
      risk_assessment_methodology_score <= 7 ~ "Moderate Methodology",
      risk_assessment_methodology_score <= 10 ~ "Advanced Methodology"
    )
  )

cross_tab <- table(df_categories$discount_level, df_categories$risk_methodology_level)
print("Cross-tabulation of discount rate application and risk methodology levels:")
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

# Quartile analysis for discount rate application
df_quartiles <- df %>%
  mutate(
    discount_quartile = ntile(discount_rate_application_sophistication, 4),
    discount_quartile_label = case_when(
      discount_quartile == 1 ~ "Q1 (Least Sophisticated)",
      discount_quartile == 2 ~ "Q2 (Low-Moderate)",
      discount_quartile == 3 ~ "Q3 (Moderate-High)",
      discount_quartile == 4 ~ "Q4 (Most Sophisticated)"
    )
  )

quartile_analysis <- df_quartiles %>%
  group_by(discount_quartile_label) %>%
  summarise(
    n = n(),
    discount_range = paste(round(min(discount_rate_application_sophistication), 1), "-",
                          round(max(discount_rate_application_sophistication), 1)),
    mean_risk_methodology = round(mean(risk_assessment_methodology_score), 2),
    sd_risk_methodology = round(sd(risk_assessment_methodology_score), 2),
    .groups = "drop"
  )

print("Risk assessment methodology by discount rate application quartiles:")
print(quartile_analysis)

# ANOVA across quartiles
anova_quartiles <- lm(risk_assessment_methodology_score ~ factor(discount_quartile), 
                     data = df_quartiles)
anova_summary <- summary(anova_quartiles)

print("ANOVA across discount rate application quartiles:")
print(paste("F(", anova_summary$df[1], ",", anova_summary$df[2], ") =",
            round(anova_summary$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_summary$fstatistic[1], 
                                 anova_summary$df[1], 
                                 anova_summary$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Analysis by investment size (if available)
if(sum(!is.na(df$typical_investment_size_numeric)) > 50) {
  df_investment <- df %>%
    filter(!is.na(typical_investment_size_numeric)) %>%
    mutate(
      investment_size_category = case_when(
        typical_investment_size_numeric <= 1e6 ~ "Small (<$1M)",
        typical_investment_size_numeric <= 1e7 ~ "Medium ($1-10M)",
        typical_investment_size_numeric <= 1e8 ~ "Large ($10-100M)",
        TRUE ~ "Very Large (>$100M)"
      )
    )
  
  investment_analysis <- df_investment %>%
    group_by(investment_size_category) %>%
    filter(n() >= 20) %>%
    summarise(
      n = n(),
      mean_discount = round(mean(discount_rate_application_sophistication), 2),
      mean_risk = round(mean(risk_assessment_methodology_score), 2),
      correlation = round(cor(discount_rate_application_sophistication, 
                             risk_assessment_methodology_score), 3),
      .groups = "drop"
    )
  
  print("Analysis by investment size (n ≥ 20):")
  print(investment_analysis)
}

# High sophistication threshold analysis
high_sophistication <- df_categories %>%
  filter(risk_methodology_level == "Advanced Methodology") %>%
  group_by(discount_level) %>%
  summarise(
    n = n(),
    percentage = round(n / nrow(df_categories %>% 
                               filter(discount_level == unique(discount_level))) * 100, 1),
    .groups = "drop"
  )

print("Advanced risk methodology by discount rate application level:")
print(high_sophistication)

# Standardized regression coefficient (beta)
df_standardized <- df %>%
  mutate(
    discount_z = scale(discount_rate_application_sophistication)[,1],
    risk_z = scale(risk_assessment_methodology_score)[,1]
  )

lm_standardized <- lm(risk_z ~ discount_z, data = df_standardized)
beta_coefficient <- coef(lm_standardized)[2]
print(paste("Standardized β =", round(beta_coefficient, 3)))

# Financial modeling experience stratified analysis
if(sum(!is.na(df$financial_modeling_experience_years)) > 100) {
  df_exp_strata <- df %>%
    filter(!is.na(financial_modeling_experience_years)) %>%
    mutate(
      experience_level = case_when(
        financial_modeling_experience_years <= 3 ~ "Low Experience (≤3 years)",
        financial_modeling_experience_years <= 8 ~ "Moderate Experience (4-8 years)",
        financial_modeling_experience_years > 8 ~ "High Experience (>8 years)"
      )
    )
  
  exp_correlations <- df_exp_strata %>%
    group_by(experience_level) %>%
    filter(n() >= 30) %>%
    summarise(
      n = n(),
      correlation = round(cor(discount_rate_application_sophistication, 
                             risk_assessment_methodology_score), 3),
      .groups = "drop"
    )
  
  print("Discount-Risk correlation by financial modeling experience:")
  print(exp_correlations)
}

# Advanced vs Basic methodology comparison
advanced_basic_comparison <- df_categories %>%
  filter(risk_methodology_level %in% c("Basic Methodology", "Advanced Methodology")) %>%
  group_by(risk_methodology_level) %>%
  summarise(
    n = n(),
    mean_discount = round(mean(discount_rate_application_sophistication), 2),
    sd_discount = round(sd(discount_rate_application_sophistication), 2),
    .groups = "drop"
  )

print("Discount rate application: Advanced vs Basic risk methodology:")
print(advanced_basic_comparison)

# T-test for advanced vs basic
if(nrow(advanced_basic_comparison) == 2) {
  adv_basic_data <- df_categories %>%
    filter(risk_methodology_level %in% c("Basic Methodology", "Advanced Methodology"))
  
  adv_basic_test <- t.test(discount_rate_application_sophistication ~ risk_methodology_level, 
                          data = adv_basic_data)
  
  print("Advanced vs Basic methodology t-test:")
  print(paste("t =", round(adv_basic_test$statistic, 2)))
  print(paste("p =", format(adv_basic_test$p.value, scientific = TRUE)))
  
  # Effect size
  adv_basic_cohens_d <- cohens_d(discount_rate_application_sophistication ~ risk_methodology_level, 
                                 data = adv_basic_data)
  print(paste("Cohen's d =", round(adv_basic_cohens_d$Cohens_d, 3)))
}

# Expected: r = .276, p < .001