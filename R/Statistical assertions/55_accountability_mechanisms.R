# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 55: "Accountability mechanisms effectiveness correlated with governance structure complexity (r = .318, p < .001)"
# Purpose: Correlation analysis between accountability mechanisms effectiveness and organizational governance structure complexity

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_ACCOUNTABILITY_EFFECTIVENESS <- "accountability_mechanisms_effectiveness_rating"
COL_GOVERNANCE_COMPLEXITY <- "governance_structure_complexity_score"
COL_STAKEHOLDER <- "stakeholder"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_BOARD_SIZE <- "board_size_numeric"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    accountability_mechanisms_effectiveness_rating = as.numeric(accountability_mechanisms_effectiveness_rating),
    governance_structure_complexity_score = as.numeric(governance_structure_complexity_score),
    stakeholder = factor(stakeholder),
    organization_size_category = factor(organization_size_category),
    board_size_numeric = as.numeric(board_size_numeric)
  ) %>%
  filter(!is.na(accountability_mechanisms_effectiveness_rating), 
         !is.na(governance_structure_complexity_score)) %>%
  # Ensure variables are within expected ranges
  filter(accountability_mechanisms_effectiveness_rating >= 1, accountability_mechanisms_effectiveness_rating <= 10,
         governance_structure_complexity_score >= 1, governance_structure_complexity_score <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    accountability_mean = round(mean(accountability_mechanisms_effectiveness_rating), 2),
    accountability_sd = round(sd(accountability_mechanisms_effectiveness_rating), 2),
    governance_mean = round(mean(governance_structure_complexity_score), 2),
    governance_sd = round(sd(governance_structure_complexity_score), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$accountability_mechanisms_effectiveness_rating, 
                      df$governance_structure_complexity_score,
                      method = "pearson", conf.level = 0.95)

print("Accountability effectiveness × Governance complexity correlation:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$accountability_mechanisms_effectiveness_rating, d$governance_structure_complexity_score,
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
    correlation = cor.test(.$accountability_mechanisms_effectiveness_rating, .$governance_structure_complexity_score)
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

# Correlation by organization size
if(!all(is.na(df$organization_size_category))) {
  cor_by_size <- df %>%
    filter(!is.na(organization_size_category)) %>%
    group_by(organization_size_category) %>%
    filter(n() >= 20) %>%
    do(
      correlation = cor.test(.$accountability_mechanisms_effectiveness_rating, .$governance_structure_complexity_score)
    ) %>%
    mutate(
      n = df %>% filter(organization_size_category == .$organization_size_category[1]) %>% nrow(),
      r = map_dbl(correlation, ~ round(.x$estimate, 3)),
      p_value = map_dbl(correlation, ~ .x$p.value)
    ) %>%
    select(-correlation)
  
  print("Correlation by organization size (n ≥ 20):")
  print(cor_by_size)
}

# Partial correlation controlling for board size (if available)
if(sum(!is.na(df$board_size_numeric)) > 50) {
  df_board <- df %>%
    filter(!is.na(board_size_numeric))
  
  partial_cor <- pcor.test(df_board$accountability_mechanisms_effectiveness_rating,
                          df_board$governance_structure_complexity_score,
                          df_board$board_size_numeric)
  
  print("Partial correlation (controlling for board size):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness
spearman_result <- cor.test(df$accountability_mechanisms_effectiveness_rating,
                           df$governance_structure_complexity_score,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(accountability_mechanisms_effectiveness_rating ~ governance_structure_complexity_score, 
               data = df)
lm_summary <- summary(lm_model)

print("Linear regression: Accountability ~ Governance complexity:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))

# Multiple regression with control variables
if(!all(is.na(df$organization_size_category))) {
  lm_multiple <- lm(accountability_mechanisms_effectiveness_rating ~ 
                   governance_structure_complexity_score + stakeholder + 
                   organization_size_category, 
                   data = df %>% filter(!is.na(organization_size_category)))
  
  multiple_summary <- summary(lm_multiple)
  print("Multiple regression with controls:")
  
  # Extract governance coefficient
  governance_coef <- multiple_summary$coefficients["governance_structure_complexity_score", ]
  print("Governance complexity coefficient (controlled):")
  print(paste("β =", round(governance_coef[1], 3)))
  print(paste("SE =", round(governance_coef[2], 3)))
  print(paste("p =", format(governance_coef[4], scientific = TRUE)))
}

# Cross-tabulation for categorical analysis
df_categories <- df %>%
  mutate(
    accountability_level = case_when(
      accountability_mechanisms_effectiveness_rating <= 4 ~ "Low Effectiveness",
      accountability_mechanisms_effectiveness_rating <= 7 ~ "Moderate Effectiveness",
      accountability_mechanisms_effectiveness_rating <= 10 ~ "High Effectiveness"
    ),
    governance_level = case_when(
      governance_structure_complexity_score <= 4 ~ "Low Complexity",
      governance_structure_complexity_score <= 7 ~ "Moderate Complexity",
      governance_structure_complexity_score <= 10 ~ "High Complexity"
    )
  )

cross_tab <- table(df_categories$governance_level, df_categories$accountability_level)
print("Cross-tabulation of governance complexity and accountability effectiveness levels:")
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

# Quartile analysis for governance complexity
df_quartiles <- df %>%
  mutate(
    governance_quartile = ntile(governance_structure_complexity_score, 4),
    governance_quartile_label = case_when(
      governance_quartile == 1 ~ "Q1 (Least Complex)",
      governance_quartile == 2 ~ "Q2 (Low-Moderate)",
      governance_quartile == 3 ~ "Q3 (Moderate-High)",
      governance_quartile == 4 ~ "Q4 (Most Complex)"
    )
  )

quartile_analysis <- df_quartiles %>%
  group_by(governance_quartile_label) %>%
  summarise(
    n = n(),
    governance_range = paste(round(min(governance_structure_complexity_score), 1), "-",
                            round(max(governance_structure_complexity_score), 1)),
    mean_accountability = round(mean(accountability_mechanisms_effectiveness_rating), 2),
    sd_accountability = round(sd(accountability_mechanisms_effectiveness_rating), 2),
    .groups = "drop"
  )

print("Accountability effectiveness by governance complexity quartiles:")
print(quartile_analysis)

# ANOVA across quartiles
anova_quartiles <- lm(accountability_mechanisms_effectiveness_rating ~ factor(governance_quartile), 
                     data = df_quartiles)
anova_summary <- summary(anova_quartiles)

print("ANOVA across governance complexity quartiles:")
print(paste("F(", anova_summary$df[1], ",", anova_summary$df[2], ") =",
            round(anova_summary$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_summary$fstatistic[1], 
                                 anova_summary$df[1], 
                                 anova_summary$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Board size analysis (if available)
if(sum(!is.na(df$board_size_numeric)) > 50) {
  df_board_analysis <- df %>%
    filter(!is.na(board_size_numeric)) %>%
    mutate(
      board_size_category = case_when(
        board_size_numeric <= 5 ~ "Small Board (≤5)",
        board_size_numeric <= 9 ~ "Medium Board (6-9)",
        board_size_numeric >= 10 ~ "Large Board (≥10)"
      )
    )
  
  board_analysis <- df_board_analysis %>%
    group_by(board_size_category) %>%
    summarise(
      n = n(),
      mean_accountability = round(mean(accountability_mechanisms_effectiveness_rating), 2),
      mean_governance = round(mean(governance_structure_complexity_score), 2),
      correlation = round(cor(accountability_mechanisms_effectiveness_rating, 
                             governance_structure_complexity_score), 3),
      .groups = "drop"
    )
  
  print("Analysis by board size:")
  print(board_analysis)
}

# High accountability threshold analysis (ratings ≥ 8)
high_accountability <- df_categories %>%
  filter(accountability_level == "High Effectiveness") %>%
  group_by(governance_level) %>%
  summarise(
    n = n(),
    percentage = round(n / nrow(df_categories %>% 
                               filter(governance_level == unique(governance_level))) * 100, 1),
    .groups = "drop"
  )

print("High accountability effectiveness by governance complexity level:")
print(high_accountability)

# Standardized regression coefficient (beta)
df_standardized <- df %>%
  mutate(
    accountability_z = scale(accountability_mechanisms_effectiveness_rating)[,1],
    governance_z = scale(governance_structure_complexity_score)[,1]
  )

lm_standardized <- lm(accountability_z ~ governance_z, data = df_standardized)
beta_coefficient <- coef(lm_standardized)[2]
print(paste("Standardized β =", round(beta_coefficient, 3)))

# Curvilinear relationship test
lm_quadratic <- lm(accountability_mechanisms_effectiveness_rating ~ 
                   governance_structure_complexity_score + 
                   I(governance_structure_complexity_score^2), 
                   data = df)

quadratic_summary <- summary(lm_quadratic)
print("Quadratic relationship test:")
print(paste("R-squared (quadratic):", round(quadratic_summary$r.squared, 3)))
print(paste("R-squared improvement:", round(quadratic_summary$r.squared - lm_summary$r.squared, 4)))

# Model comparison
anova_linear_quad <- anova(lm_model, lm_quadratic)
print("Linear vs Quadratic model comparison:")
print(anova_linear_quad)

# Expected: r = .318, p < .001