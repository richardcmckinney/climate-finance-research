# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 64: "Scenario planning utilization averaged 6.1 (SD = 2.3) and correlated with strategic planning horizon (r = .298, p < .001)"
# Purpose: Descriptive analysis of scenario planning utilization and correlation with strategic planning horizon

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SCENARIO_PLANNING <- "scenario_planning_utilization_rating"
COL_STRATEGIC_HORIZON <- "strategic_planning_horizon_years"
COL_STAKEHOLDER <- "stakeholder"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_UNCERTAINTY_TOLERANCE <- "uncertainty_tolerance_rating"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    scenario_planning_utilization_rating = as.numeric(scenario_planning_utilization_rating),
    strategic_planning_horizon_years = as.numeric(strategic_planning_horizon_years),
    stakeholder = factor(stakeholder),
    organization_size_category = factor(organization_size_category),
    uncertainty_tolerance_rating = as.numeric(uncertainty_tolerance_rating)
  ) %>%
  filter(!is.na(scenario_planning_utilization_rating)) %>%
  # Ensure scenario planning ratings are within expected scale
  filter(scenario_planning_utilization_rating >= 1, scenario_planning_utilization_rating <= 10)

# Overall descriptive statistics for scenario planning
overall_stats <- df %>%
  summarise(
    n = n(),
    mean_scenario = round(mean(scenario_planning_utilization_rating), 2),
    sd_scenario = round(sd(scenario_planning_utilization_rating), 2),
    median_scenario = round(median(scenario_planning_utilization_rating), 2),
    q1 = round(quantile(scenario_planning_utilization_rating, 0.25), 2),
    q3 = round(quantile(scenario_planning_utilization_rating, 0.75), 2),
    min_rating = min(scenario_planning_utilization_rating),
    max_rating = max(scenario_planning_utilization_rating),
    .groups = "drop"
  )

print("Overall scenario planning utilization ratings:")
print(overall_stats)

# 95% Confidence interval for the mean
ci_mean <- t.test(df$scenario_planning_utilization_rating)$conf.int
print(paste("95% CI for mean: [", round(ci_mean[1], 2), ", ", 
            round(ci_mean[2], 2), "]", sep=""))

# Distribution analysis
print("Scenario planning utilization rating distribution:")
rating_distribution <- table(df$scenario_planning_utilization_rating)
prop_distribution <- round(prop.table(rating_distribution) * 100, 1)
print(rating_distribution)
print("Proportions (%):")
print(prop_distribution)

# Correlation with strategic planning horizon
df_horizon <- df %>%
  filter(!is.na(strategic_planning_horizon_years)) %>%
  # Remove extreme outliers (planning horizons > 20 years)
  filter(strategic_planning_horizon_years >= 1, strategic_planning_horizon_years <= 20)

if(nrow(df_horizon) > 50) {
  print("Descriptive statistics for strategic planning horizon:")
  horizon_stats <- df_horizon %>%
    summarise(
      n = n(),
      mean_horizon = round(mean(strategic_planning_horizon_years), 2),
      sd_horizon = round(sd(strategic_planning_horizon_years), 2),
      median_horizon = round(median(strategic_planning_horizon_years), 2),
      .groups = "drop"
    )
  print(horizon_stats)
  
  # Primary correlation analysis
  cor_result <- cor.test(df_horizon$scenario_planning_utilization_rating, 
                        df_horizon$strategic_planning_horizon_years,
                        method = "pearson", conf.level = 0.95)
  
  print("Scenario planning × Strategic planning horizon correlation:")
  print(paste("r =", round(cor_result$estimate, 3)))
  print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
              round(cor_result$conf.int[2], 3), "]", sep=""))
  print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
  print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))
  
  # Bootstrap confidence interval
  boot_cor <- function(data, indices) {
    d <- data[indices, ]
    return(cor(d$scenario_planning_utilization_rating, d$strategic_planning_horizon_years,
               use = "complete.obs"))
  }
  
  set.seed(123)
  boot_result <- boot(df_horizon, boot_cor, R = 10000)
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
}

# Breakdown by stakeholder
stakeholder_stats <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_scenario = round(mean(scenario_planning_utilization_rating), 2),
    sd_scenario = round(sd(scenario_planning_utilization_rating), 2),
    median_scenario = round(median(scenario_planning_utilization_rating), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_scenario))

print("Scenario planning utilization by stakeholder:")
print(stakeholder_stats)

# ANOVA by stakeholder
anova_stakeholder <- lm(scenario_planning_utilization_rating ~ stakeholder, data = df)
anova_stakeholder_result <- Anova(anova_stakeholder, type = "III")

print("ANOVA: Scenario planning by stakeholder:")
print(anova_stakeholder_result)

# Analysis by organization size
if(!all(is.na(df$organization_size_category))) {
  size_stats <- df %>%
    filter(!is.na(organization_size_category)) %>%
    group_by(organization_size_category) %>%
    summarise(
      n = n(),
      mean_scenario = round(mean(scenario_planning_utilization_rating), 2),
      sd_scenario = round(sd(scenario_planning_utilization_rating), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(mean_scenario))
  
  print("Scenario planning utilization by organization size (n ≥ 20):")
  print(size_stats)
  
  # ANOVA by organization size
  if(nrow(size_stats) > 1) {
    df_size_filtered <- df %>%
      filter(!is.na(organization_size_category)) %>%
      group_by(organization_size_category) %>%
      filter(n() >= 20) %>%
      ungroup() %>%
      mutate(organization_size_category = droplevels(organization_size_category))
    
    anova_size <- lm(scenario_planning_utilization_rating ~ organization_size_category, 
                    data = df_size_filtered)
    anova_size_result <- Anova(anova_size, type = "III")
    
    print("ANOVA: Scenario planning by organization size:")
    print(anova_size_result)
  }
}

# Correlation analysis by stakeholder group
if(nrow(df_horizon) > 100) {
  cor_by_stakeholder <- df_horizon %>%
    group_by(stakeholder) %>%
    filter(n() >= 30) %>%
    do(
      correlation = cor.test(.$scenario_planning_utilization_rating, .$strategic_planning_horizon_years)
    ) %>%
    mutate(
      n = df_horizon %>% filter(stakeholder == .$stakeholder[1]) %>% nrow(),
      r = map_dbl(correlation, ~ round(.x$estimate, 3)),
      ci_lower = map_dbl(correlation, ~ round(.x$conf.int[1], 3)),
      ci_upper = map_dbl(correlation, ~ round(.x$conf.int[2], 3)),
      p_value = map_dbl(correlation, ~ .x$p.value)
    ) %>%
    select(-correlation)
  
  print("Scenario planning × Strategic horizon correlation by stakeholder (n ≥ 30):")
  print(cor_by_stakeholder)
}

# Linear regression analysis
if(nrow(df_horizon) > 50) {
  lm_model <- lm(scenario_planning_utilization_rating ~ strategic_planning_horizon_years, 
                 data = df_horizon)
  lm_summary <- summary(lm_model)
  
  print("Linear regression: Scenario planning ~ Strategic horizon:")
  print(paste("R-squared:", round(lm_summary$r.squared, 3)))
  print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
  print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
  print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
  print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))
}

# Analysis by uncertainty tolerance (if available)
if(sum(!is.na(df$uncertainty_tolerance_rating)) > 50) {
  df_uncertainty <- df %>%
    filter(!is.na(uncertainty_tolerance_rating))
  
  # Correlation with uncertainty tolerance
  cor_uncertainty <- cor.test(df_uncertainty$scenario_planning_utilization_rating, 
                             df_uncertainty$uncertainty_tolerance_rating)
  
  print("Scenario planning × Uncertainty tolerance correlation:")
  print(paste("r =", round(cor_uncertainty$estimate, 3)))
  print(paste("95% CI: [", round(cor_uncertainty$conf.int[1], 3), ", ", 
              round(cor_uncertainty$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_uncertainty$p.value, scientific = TRUE)))
  
  # Uncertainty tolerance by scenario planning level
  uncertainty_analysis <- df_uncertainty %>%
    mutate(
      scenario_level = case_when(
        scenario_planning_utilization_rating <= 4 ~ "Low Usage",
        scenario_planning_utilization_rating <= 7 ~ "Moderate Usage",
        scenario_planning_utilization_rating >= 8 ~ "High Usage"
      )
    ) %>%
    group_by(scenario_level) %>%
    summarise(
      n = n(),
      mean_uncertainty_tolerance = round(mean(uncertainty_tolerance_rating), 2),
      sd_uncertainty_tolerance = round(sd(uncertainty_tolerance_rating), 2),
      .groups = "drop"
    )
  
  print("Uncertainty tolerance by scenario planning usage level:")
  print(uncertainty_analysis)
}

# High utilization threshold analysis (ratings ≥ 7)
high_utilization_analysis <- df %>%
  mutate(high_utilization = as.integer(scenario_planning_utilization_rating >= 7)) %>%
  summarise(
    n_total = n(),
    n_high = sum(high_utilization),
    prop_high = round(mean(high_utilization) * 100, 1)
  )

print("High scenario planning utilization (ratings ≥ 7):")
print(paste(high_utilization_analysis$prop_high, "% of respondents (",
            high_utilization_analysis$n_high, "/", high_utilization_analysis$n_total, ")"))

# High utilization by stakeholder
high_util_stakeholder <- df %>%
  mutate(high_utilization = as.integer(scenario_planning_utilization_rating >= 7)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    n_high = sum(high_utilization),
    prop_high = round(mean(high_utilization) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(prop_high))

print("High scenario planning utilization by stakeholder:")
print(high_util_stakeholder)

# Strategic planning horizon quartiles analysis
if(nrow(df_horizon) > 100) {
  df_horizon_quartiles <- df_horizon %>%
    mutate(
      horizon_quartile = ntile(strategic_planning_horizon_years, 4),
      horizon_quartile_label = case_when(
        horizon_quartile == 1 ~ "Q1 (Shortest Horizon)",
        horizon_quartile == 2 ~ "Q2 (Short-Medium)",
        horizon_quartile == 3 ~ "Q3 (Medium-Long)",
        horizon_quartile == 4 ~ "Q4 (Longest Horizon)"
      )
    )
  
  quartile_analysis <- df_horizon_quartiles %>%
    group_by(horizon_quartile_label) %>%
    summarise(
      n = n(),
      horizon_range = paste(round(min(strategic_planning_horizon_years), 1), "-",
                           round(max(strategic_planning_horizon_years), 1), "years"),
      mean_scenario = round(mean(scenario_planning_utilization_rating), 2),
      sd_scenario = round(sd(scenario_planning_utilization_rating), 2),
      .groups = "drop"
    )
  
  print("Scenario planning by strategic horizon quartiles:")
  print(quartile_analysis)
  
  # ANOVA across quartiles
  anova_quartiles <- lm(scenario_planning_utilization_rating ~ factor(horizon_quartile), 
                       data = df_horizon_quartiles)
  anova_quartile_summary <- summary(anova_quartiles)
  
  print("ANOVA across strategic horizon quartiles:")
  print(paste("F(", anova_quartile_summary$df[1], ",", anova_quartile_summary$df[2], ") =",
              round(anova_quartile_summary$fstatistic[1], 2)))
  print(paste("p-value:", format(pf(anova_quartile_summary$fstatistic[1], 
                                   anova_quartile_summary$df[1], 
                                   anova_quartile_summary$df[2], 
                                   lower.tail = FALSE), scientific = TRUE)))
}

# Normality assessment
shapiro_test <- shapiro.test(sample(df$scenario_planning_utilization_rating, 
                                   min(5000, nrow(df))))
print(paste("Shapiro-Wilk normality test: W =", round(shapiro_test$statistic, 3),
            ", p =", format(shapiro_test$p.value, scientific = TRUE)))

# Skewness and kurtosis
skew_kurt <- describe(df$scenario_planning_utilization_rating)
print(paste("Skewness:", round(skew_kurt$skew, 3)))
print(paste("Kurtosis:", round(skew_kurt$kurtosis, 3)))

# Mode calculation
mode_val <- as.numeric(names(sort(table(df$scenario_planning_utilization_rating), 
                                 decreasing = TRUE))[1])
print(paste("Mode:", mode_val))

# Spearman correlation for robustness (if horizon data available)
if(nrow(df_horizon) > 50) {
  spearman_result <- cor.test(df_horizon$scenario_planning_utilization_rating,
                             df_horizon$strategic_planning_horizon_years,
                             method = "spearman")
  print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
              "(robustness check)"))
}

# Multiple regression with control variables (if horizon data available)
if(nrow(df_horizon) > 100) {
  df_multiple <- df_horizon %>%
    filter(!is.na(stakeholder), !is.na(organization_size_category))
  
  if(nrow(df_multiple) > 50) {
    lm_multiple <- lm(scenario_planning_utilization_rating ~ 
                     strategic_planning_horizon_years + stakeholder + 
                     organization_size_category, 
                     data = df_multiple)
    
    multiple_summary <- summary(lm_multiple)
    print("Multiple regression with controls:")
    
    # Extract horizon coefficient
    horizon_coef <- multiple_summary$coefficients["strategic_planning_horizon_years", ]
    print("Strategic horizon coefficient (controlled):")
    print(paste("β =", round(horizon_coef[1], 3)))
    print(paste("SE =", round(horizon_coef[2], 3)))
    print(paste("p =", format(horizon_coef[4], scientific = TRUE)))
  }
}

# Scenario planning categories analysis
scenario_categories <- df %>%
  mutate(
    scenario_category = case_when(
      scenario_planning_utilization_rating <= 3 ~ "Low Utilization (1-3)",
      scenario_planning_utilization_rating <= 6 ~ "Moderate Utilization (4-6)",
      scenario_planning_utilization_rating <= 10 ~ "High Utilization (7-10)"
    )
  ) %>%
  count(scenario_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("Scenario planning utilization categories:")
print(scenario_categories)

# Expected: Mean = 6.1, SD = 2.3, correlation with strategic horizon r = .298, p < .001