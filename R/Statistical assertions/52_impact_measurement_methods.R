# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 52: "Impact measurement methods usage correlated with reporting requirements (r = .412, p < .001)"
# Purpose: Correlation analysis between impact measurement methods usage and regulatory/voluntary reporting requirements

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_IMPACT_MEASUREMENT <- "impact_measurement_methods_usage_score"
COL_REPORTING_REQUIREMENTS <- "reporting_requirements_compliance_score"
COL_STAKEHOLDER <- "stakeholder"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_MANDATORY_REPORTING <- "mandatory_reporting_obligations"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    impact_measurement_methods_usage_score = as.numeric(impact_measurement_methods_usage_score),
    reporting_requirements_compliance_score = as.numeric(reporting_requirements_compliance_score),
    stakeholder = factor(stakeholder),
    organization_size_category = factor(organization_size_category),
    mandatory_reporting_obligations = as.numeric(mandatory_reporting_obligations)
  ) %>%
  filter(!is.na(impact_measurement_methods_usage_score), 
         !is.na(reporting_requirements_compliance_score)) %>%
  # Ensure scores are within expected ranges
  filter(impact_measurement_methods_usage_score >= 1, impact_measurement_methods_usage_score <= 10,
         reporting_requirements_compliance_score >= 1, reporting_requirements_compliance_score <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    impact_mean = round(mean(impact_measurement_methods_usage_score), 2),
    impact_sd = round(sd(impact_measurement_methods_usage_score), 2),
    reporting_mean = round(mean(reporting_requirements_compliance_score), 2),
    reporting_sd = round(sd(reporting_requirements_compliance_score), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$impact_measurement_methods_usage_score, 
                      df$reporting_requirements_compliance_score,
                      method = "pearson", conf.level = 0.95)

print("Impact measurement × Reporting requirements correlation:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$impact_measurement_methods_usage_score, d$reporting_requirements_compliance_score,
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
    correlation = cor.test(.$impact_measurement_methods_usage_score, .$reporting_requirements_compliance_score)
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
      correlation = cor.test(.$impact_measurement_methods_usage_score, .$reporting_requirements_compliance_score)
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

# Partial correlation controlling for mandatory reporting obligations
if(sum(!is.na(df$mandatory_reporting_obligations)) > 50) {
  df_mandatory <- df %>%
    filter(!is.na(mandatory_reporting_obligations))
  
  partial_cor <- pcor.test(df_mandatory$impact_measurement_methods_usage_score,
                          df_mandatory$reporting_requirements_compliance_score,
                          df_mandatory$mandatory_reporting_obligations)
  
  print("Partial correlation (controlling for mandatory reporting):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness
spearman_result <- cor.test(df$impact_measurement_methods_usage_score,
                           df$reporting_requirements_compliance_score,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(impact_measurement_methods_usage_score ~ reporting_requirements_compliance_score, 
               data = df)
lm_summary <- summary(lm_model)

print("Linear regression: Impact measurement ~ Reporting requirements:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))

# Multiple regression with control variables
lm_multiple <- lm(impact_measurement_methods_usage_score ~ 
                 reporting_requirements_compliance_score + stakeholder + 
                 organization_size_category, 
                 data = df %>% filter(!is.na(organization_size_category)))

multiple_summary <- summary(lm_multiple)
print("Multiple regression with controls:")

# Extract reporting coefficient
reporting_coef <- multiple_summary$coefficients["reporting_requirements_compliance_score", ]
print("Reporting requirements coefficient (controlled):")
print(paste("β =", round(reporting_coef[1], 3)))
print(paste("SE =", round(reporting_coef[2], 3)))
print(paste("p =", format(reporting_coef[4], scientific = TRUE)))

# Cross-tabulation for categorical analysis
df_categories <- df %>%
  mutate(
    impact_level = case_when(
      impact_measurement_methods_usage_score <= 4 ~ "Low Usage",
      impact_measurement_methods_usage_score <= 7 ~ "Moderate Usage",
      impact_measurement_methods_usage_score <= 10 ~ "High Usage"
    ),
    reporting_level = case_when(
      reporting_requirements_compliance_score <= 4 ~ "Low Compliance",
      reporting_requirements_compliance_score <= 7 ~ "Moderate Compliance",
      reporting_requirements_compliance_score <= 10 ~ "High Compliance"
    )
  )

cross_tab <- table(df_categories$impact_level, df_categories$reporting_level)
print("Cross-tabulation of impact measurement and reporting compliance levels:")
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

# Analysis by mandatory vs voluntary reporting
if(sum(!is.na(df$mandatory_reporting_obligations)) > 50) {
  df_reporting_type <- df %>%
    filter(!is.na(mandatory_reporting_obligations)) %>%
    mutate(
      reporting_type = ifelse(mandatory_reporting_obligations >= 5, 
                             "High Mandatory Requirements", 
                             "Low/Voluntary Requirements")
    )
  
  reporting_type_analysis <- df_reporting_type %>%
    group_by(reporting_type) %>%
    summarise(
      n = n(),
      impact_mean = round(mean(impact_measurement_methods_usage_score), 2),
      impact_sd = round(sd(impact_measurement_methods_usage_score), 2),
      reporting_mean = round(mean(reporting_requirements_compliance_score), 2),
      reporting_sd = round(sd(reporting_requirements_compliance_score), 2),
      correlation = round(cor(impact_measurement_methods_usage_score, 
                             reporting_requirements_compliance_score), 3),
      .groups = "drop"
    )
  
  print("Analysis by mandatory reporting obligations:")
  print(reporting_type_analysis)
  
  # T-test comparing impact measurement usage by reporting type
  impact_by_reporting <- t.test(impact_measurement_methods_usage_score ~ reporting_type, 
                               data = df_reporting_type)
  print("Impact measurement usage by reporting obligation type:")
  print(paste("t =", round(impact_by_reporting$statistic, 2)))
  print(paste("p =", format(impact_by_reporting$p.value, scientific = TRUE)))
}

# Quartile analysis
df_quartiles <- df %>%
  mutate(
    reporting_quartile = ntile(reporting_requirements_compliance_score, 4),
    reporting_quartile_label = case_when(
      reporting_quartile == 1 ~ "Q1 (Lowest Compliance)",
      reporting_quartile == 2 ~ "Q2 (Low-Moderate)",
      reporting_quartile == 3 ~ "Q3 (Moderate-High)",
      reporting_quartile == 4 ~ "Q4 (Highest Compliance)"
    )
  )

quartile_analysis <- df_quartiles %>%
  group_by(reporting_quartile_label) %>%
  summarise(
    n = n(),
    reporting_range = paste(round(min(reporting_requirements_compliance_score), 1), "-",
                           round(max(reporting_requirements_compliance_score), 1)),
    mean_impact = round(mean(impact_measurement_methods_usage_score), 2),
    sd_impact = round(sd(impact_measurement_methods_usage_score), 2),
    .groups = "drop"
  )

print("Impact measurement usage by reporting compliance quartiles:")
print(quartile_analysis)

# ANOVA across quartiles
anova_quartiles <- lm(impact_measurement_methods_usage_score ~ factor(reporting_quartile), 
                     data = df_quartiles)
anova_summary <- summary(anova_quartiles)

print("ANOVA across reporting compliance quartiles:")
print(paste("F(", anova_summary$df[1], ",", anova_summary$df[2], ") =",
            round(anova_summary$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_summary$fstatistic[1], 
                                 anova_summary$df[1], 
                                 anova_summary$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Standardized regression coefficient (beta)
df_standardized <- df %>%
  mutate(
    impact_z = scale(impact_measurement_methods_usage_score)[,1],
    reporting_z = scale(reporting_requirements_compliance_score)[,1]
  )

lm_standardized <- lm(impact_z ~ reporting_z, data = df_standardized)
beta_coefficient <- coef(lm_standardized)[2]
print(paste("Standardized β =", round(beta_coefficient, 3)))

# Expected: r = .412, p < .001