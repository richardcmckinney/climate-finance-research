# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 57: "Climate data availability scores correlated with geographic location (r = .267, p < .001) and regulatory requirements"
# Purpose: Correlation analysis between climate data availability, geographic location, and regulatory requirements

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_CLIMATE_DATA_AVAILABILITY <- "climate_data_availability_score"
COL_GEOGRAPHIC_LOCATION <- "geographic_location_score"
COL_REGULATORY_REQUIREMENTS <- "regulatory_requirements_stringency"
COL_GEOGRAPHIC_REGION <- "geographic_region"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    climate_data_availability_score = as.numeric(climate_data_availability_score),
    geographic_location_score = as.numeric(geographic_location_score),
    regulatory_requirements_stringency = as.numeric(regulatory_requirements_stringency),
    geographic_region = factor(geographic_region),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(climate_data_availability_score), !is.na(geographic_location_score)) %>%
  # Ensure variables are within expected ranges
  filter(climate_data_availability_score >= 1, climate_data_availability_score <= 10,
         geographic_location_score >= 1, geographic_location_score <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    climate_data_mean = round(mean(climate_data_availability_score), 2),
    climate_data_sd = round(sd(climate_data_availability_score), 2),
    location_mean = round(mean(geographic_location_score), 2),
    location_sd = round(sd(geographic_location_score), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation: Climate data availability × Geographic location
cor_location <- cor.test(df$climate_data_availability_score, 
                        df$geographic_location_score,
                        method = "pearson", conf.level = 0.95)

print("Climate data availability × Geographic location correlation:")
print(paste("r =", round(cor_location$estimate, 3)))
print(paste("95% CI: [", round(cor_location$conf.int[1], 3), ", ", 
            round(cor_location$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_location$parameter, ") =", round(cor_location$statistic, 2)))
print(paste("p-value:", format(cor_location$p.value, scientific = TRUE)))

# Bootstrap confidence interval for primary correlation
boot_cor_location <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$climate_data_availability_score, d$geographic_location_score,
             use = "complete.obs"))
}

set.seed(123)
boot_result_location <- boot(df, boot_cor_location, R = 10000)
boot_ci_location <- boot.ci(boot_result_location, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation (Climate data × Location):")
print(paste("[", round(boot_ci_location$percent[4], 3), ", ", round(boot_ci_location$percent[5], 3), "]", sep=""))

# Secondary correlation: Climate data availability × Regulatory requirements
df_regulatory <- df %>%
  filter(!is.na(regulatory_requirements_stringency))

if(nrow(df_regulatory) > 50) {
  cor_regulatory <- cor.test(df_regulatory$climate_data_availability_score, 
                            df_regulatory$regulatory_requirements_stringency,
                            method = "pearson", conf.level = 0.95)
  
  print("Climate data availability × Regulatory requirements correlation:")
  print(paste("r =", round(cor_regulatory$estimate, 3)))
  print(paste("95% CI: [", round(cor_regulatory$conf.int[1], 3), ", ", 
              round(cor_regulatory$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_regulatory$p.value, scientific = TRUE)))
  print(paste("Sample size:", nrow(df_regulatory)))
  
  # Bootstrap CI for regulatory correlation
  boot_cor_regulatory <- function(data, indices) {
    d <- data[indices, ]
    return(cor(d$climate_data_availability_score, d$regulatory_requirements_stringency,
               use = "complete.obs"))
  }
  
  boot_result_regulatory <- boot(df_regulatory, boot_cor_regulatory, R = 10000)
  boot_ci_regulatory <- boot.ci(boot_result_regulatory, type = "perc", conf = 0.95)
  
  print("Bootstrap 95% CI (Climate data × Regulatory):")
  print(paste("[", round(boot_ci_regulatory$percent[4], 3), ", ", round(boot_ci_regulatory$percent[5], 3), "]", sep=""))
}

# Correlation matrix for all three variables
if(nrow(df_regulatory) > 50) {
  cor_matrix <- cor(df_regulatory %>% 
                   select(climate_data_availability_score, 
                         geographic_location_score,
                         regulatory_requirements_stringency),
                   use = "complete.obs")
  
  print("Correlation matrix:")
  print(round(cor_matrix, 3))
  
  # Partial correlations
  partial_climate_location <- pcor.test(df_regulatory$climate_data_availability_score,
                                       df_regulatory$geographic_location_score,
                                       df_regulatory$regulatory_requirements_stringency)
  
  partial_climate_regulatory <- pcor.test(df_regulatory$climate_data_availability_score,
                                         df_regulatory$regulatory_requirements_stringency,
                                         df_regulatory$geographic_location_score)
  
  print("Partial correlations:")
  print(paste("Climate data × Location (controlling for regulatory): r =", 
              round(partial_climate_location$estimate, 3),
              ", p =", format(partial_climate_location$p.value, scientific = TRUE)))
  print(paste("Climate data × Regulatory (controlling for location): r =", 
              round(partial_climate_regulatory$estimate, 3),
              ", p =", format(partial_climate_regulatory$p.value, scientific = TRUE)))
}

# Analysis by geographic region
if(!all(is.na(df$geographic_region))) {
  region_analysis <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    filter(n() >= 20) %>%
    summarise(
      n = n(),
      climate_data_mean = round(mean(climate_data_availability_score), 2),
      climate_data_sd = round(sd(climate_data_availability_score), 2),
      location_mean = round(mean(geographic_location_score), 2),
      location_sd = round(sd(geographic_location_score), 2),
      correlation = round(cor(climate_data_availability_score, 
                             geographic_location_score, 
                             use = "complete.obs"), 3),
      .groups = "drop"
    ) %>%
    arrange(desc(climate_data_mean))
  
  print("Climate data availability by geographic region (n ≥ 20):")
  print(region_analysis)
  
  # ANOVA: Climate data availability across regions
  df_regions <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    filter(n() >= 20) %>%
    ungroup() %>%
    mutate(geographic_region = droplevels(geographic_region))
  
  if(length(levels(df_regions$geographic_region)) > 1) {
    anova_regions <- lm(climate_data_availability_score ~ geographic_region, 
                       data = df_regions)
    anova_summary <- summary(anova_regions)
    
    print("ANOVA: Climate data availability by geographic region:")
    print(paste("F(", anova_summary$df[1], ",", anova_summary$df[2], ") =",
                round(anova_summary$fstatistic[1], 2)))
    print(paste("p-value:", format(pf(anova_summary$fstatistic[1], 
                                     anova_summary$df[1], 
                                     anova_summary$df[2], 
                                     lower.tail = FALSE), scientific = TRUE)))
    print(paste("R-squared:", round(anova_summary$r.squared, 3)))
  }
}

# Correlation by stakeholder group
cor_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%
  do(
    correlation = cor.test(.$climate_data_availability_score, .$geographic_location_score)
  ) %>%
  mutate(
    n = df %>% filter(stakeholder == .$stakeholder[1]) %>% nrow(),
    r = map_dbl(correlation, ~ round(.x$estimate, 3)),
    ci_lower = map_dbl(correlation, ~ round(.x$conf.int[1], 3)),
    ci_upper = map_dbl(correlation, ~ round(.x$conf.int[2], 3)),
    p_value = map_dbl(correlation, ~ .x$p.value)
  ) %>%
  select(-correlation)

print("Climate data × Location correlation by stakeholder (n ≥ 30):")
print(cor_by_stakeholder)

# Spearman correlations for robustness
spearman_location <- cor.test(df$climate_data_availability_score,
                             df$geographic_location_score,
                             method = "spearman")
print(paste("Spearman's ρ (Climate data × Location) =", round(spearman_location$estimate, 3)))

if(nrow(df_regulatory) > 50) {
  spearman_regulatory <- cor.test(df_regulatory$climate_data_availability_score,
                                 df_regulatory$regulatory_requirements_stringency,
                                 method = "spearman")
  print(paste("Spearman's ρ (Climate data × Regulatory) =", round(spearman_regulatory$estimate, 3)))
}

# Linear regression analysis
lm_location <- lm(climate_data_availability_score ~ geographic_location_score, data = df)
lm_summary <- summary(lm_location)

print("Linear regression: Climate data ~ Geographic location:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Slope (β):", round(coef(lm_location)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))
print(paste("p-value:", format(lm_summary$coefficients[2,4], scientific = TRUE)))

# Multiple regression with both predictors
if(nrow(df_regulatory) > 100) {
  lm_multiple <- lm(climate_data_availability_score ~ 
                   geographic_location_score + regulatory_requirements_stringency,
                   data = df_regulatory)
  
  multiple_summary <- summary(lm_multiple)
  print("Multiple regression: Climate data ~ Location + Regulatory:")
  print(multiple_summary)
  
  # Extract coefficients
  location_coef <- multiple_summary$coefficients["geographic_location_score", ]
  regulatory_coef <- multiple_summary$coefficients["regulatory_requirements_stringency", ]
  
  print("Geographic location coefficient:")
  print(paste("β =", round(location_coef[1], 3), ", SE =", round(location_coef[2], 3),
              ", p =", format(location_coef[4], scientific = TRUE)))
  
  print("Regulatory requirements coefficient:")
  print(paste("β =", round(regulatory_coef[1], 3), ", SE =", round(regulatory_coef[2], 3),
              ", p =", format(regulatory_coef[4], scientific = TRUE)))
}

# Categorized analysis
df_categories <- df %>%
  mutate(
    climate_data_level = case_when(
      climate_data_availability_score <= 4 ~ "Low Availability",
      climate_data_availability_score <= 7 ~ "Moderate Availability",
      climate_data_availability_score <= 10 ~ "High Availability"
    ),
    location_category = case_when(
      geographic_location_score <= 4 ~ "Remote/Rural",
      geographic_location_score <= 7 ~ "Suburban/Regional",
      geographic_location_score <= 10 ~ "Urban/Central"
    )
  )

cross_tab <- table(df_categories$location_category, df_categories$climate_data_level)
print("Cross-tabulation: Geographic location × Climate data availability:")
print(cross_tab)
print("Row percentages:")
print(round(prop.table(cross_tab, 1) * 100, 1))

# Chi-square test for categorical association
chi_test <- chisq.test(cross_tab)
print(paste("Chi-square test: χ² =", round(chi_test$statistic, 2),
            ", df =", chi_test$parameter,
            ", p =", format(chi_test$p.value, scientific = TRUE)))

# Cramér's V
cramers_v <- sqrt(chi_test$statistic / (sum(cross_tab) * (min(dim(cross_tab)) - 1)))
print(paste("Cramér's V:", round(cramers_v, 3)))

# Effect size interpretation for primary correlation
cor_magnitude <- abs(cor_location$estimate)
effect_size_interpretation <- case_when(
  cor_magnitude < 0.1 ~ "Negligible",
  cor_magnitude < 0.3 ~ "Small", 
  cor_magnitude < 0.5 ~ "Medium",
  cor_magnitude < 0.7 ~ "Large",
  TRUE ~ "Very Large"
)

print(paste("Effect size interpretation (Climate data × Location):", effect_size_interpretation))

# Climate data availability quintiles analysis
df_quintiles <- df %>%
  mutate(
    location_quintile = ntile(geographic_location_score, 5),
    location_quintile_label = case_when(
      location_quintile == 1 ~ "Q1 (Most Remote)",
      location_quintile == 2 ~ "Q2",
      location_quintile == 3 ~ "Q3 (Moderate)",
      location_quintile == 4 ~ "Q4",
      location_quintile == 5 ~ "Q5 (Most Central)"
    )
  )

quintile_analysis <- df_quintiles %>%
  group_by(location_quintile_label) %>%
  summarise(
    n = n(),
    location_range = paste(round(min(geographic_location_score), 1), "-",
                          round(max(geographic_location_score), 1)),
    mean_climate_data = round(mean(climate_data_availability_score), 2),
    sd_climate_data = round(sd(climate_data_availability_score), 2),
    .groups = "drop"
  )

print("Climate data availability by geographic location quintiles:")
print(quintile_analysis)

# ANOVA across quintiles
anova_quintiles <- lm(climate_data_availability_score ~ factor(location_quintile), 
                     data = df_quintiles)
anova_quintile_summary <- summary(anova_quintiles)

print("ANOVA across location quintiles:")
print(paste("F(", anova_quintile_summary$df[1], ",", anova_quintile_summary$df[2], ") =",
            round(anova_quintile_summary$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_quintile_summary$fstatistic[1], 
                                 anova_quintile_summary$df[1], 
                                 anova_quintile_summary$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Standardized regression coefficients
df_standardized <- df %>%
  mutate(
    climate_data_z = scale(climate_data_availability_score)[,1],
    location_z = scale(geographic_location_score)[,1]
  )

lm_standardized <- lm(climate_data_z ~ location_z, data = df_standardized)
beta_coefficient <- coef(lm_standardized)[2]
print(paste("Standardized β (Location → Climate data) =", round(beta_coefficient, 3)))

# Expected: r = .267, p < .001 (correlation with geographic location)