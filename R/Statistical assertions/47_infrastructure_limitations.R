# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 47: "Infrastructure limitations correlated with geographic location (r = .241, p < .001) and funding accessibility"
# Purpose: Correlation analysis between infrastructure limitations, geographic location, and funding accessibility

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_INFRASTRUCTURE_LIMITATIONS <- "infrastructure_limitation_severity"
COL_GEOGRAPHIC_LOCATION <- "geographic_location_remoteness_score"
COL_FUNDING_ACCESSIBILITY <- "funding_accessibility_score"
COL_REGION <- "geographic_region"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    infrastructure_limitation_severity = as.numeric(infrastructure_limitation_severity),
    geographic_location_remoteness_score = as.numeric(geographic_location_remoteness_score),
    funding_accessibility_score = as.numeric(funding_accessibility_score),
    geographic_region = factor(geographic_region),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(infrastructure_limitation_severity), 
         !is.na(geographic_location_remoteness_score)) %>%
  # Ensure variables are within expected ranges
  filter(infrastructure_limitation_severity >= 1, infrastructure_limitation_severity <= 7,
         geographic_location_remoteness_score >= 1, geographic_location_remoteness_score <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    infrastructure_mean = round(mean(infrastructure_limitation_severity), 2),
    infrastructure_sd = round(sd(infrastructure_limitation_severity), 2),
    location_mean = round(mean(geographic_location_remoteness_score), 2),
    location_sd = round(sd(geographic_location_remoteness_score), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation: Infrastructure limitations × Geographic location
cor_infra_geo <- cor.test(df$infrastructure_limitation_severity, 
                         df$geographic_location_remoteness_score,
                         method = "pearson", conf.level = 0.95)

print("Infrastructure limitations × Geographic location correlation:")
print(paste("r =", round(cor_infra_geo$estimate, 3)))
print(paste("95% CI: [", round(cor_infra_geo$conf.int[1], 3), ", ", 
            round(cor_infra_geo$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_infra_geo$parameter, ") =", round(cor_infra_geo$statistic, 2)))
print(paste("p-value:", format(cor_infra_geo$p.value, scientific = TRUE)))

# Bootstrap confidence interval for infrastructure-geography correlation
boot_cor_geo <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$infrastructure_limitation_severity, d$geographic_location_remoteness_score,
             use = "complete.obs"))
}

set.seed(123)
boot_result_geo <- boot(df, boot_cor_geo, R = 10000)
boot_ci_geo <- boot.ci(boot_result_geo, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation (Infrastructure × Geography):")
print(paste("[", round(boot_ci_geo$percent[4], 3), ", ", round(boot_ci_geo$percent[5], 3), "]", sep=""))

# Secondary correlation: Infrastructure limitations × Funding accessibility
df_funding <- df %>%
  filter(!is.na(funding_accessibility_score)) %>%
  filter(funding_accessibility_score >= 1, funding_accessibility_score <= 10)

if(nrow(df_funding) > 50) {
  cor_infra_funding <- cor.test(df_funding$infrastructure_limitation_severity, 
                               df_funding$funding_accessibility_score,
                               method = "pearson", conf.level = 0.95)
  
  print("Infrastructure limitations × Funding accessibility correlation:")
  print(paste("r =", round(cor_infra_funding$estimate, 3)))
  print(paste("95% CI: [", round(cor_infra_funding$conf.int[1], 3), ", ", 
              round(cor_infra_funding$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_infra_funding$p.value, scientific = TRUE)))
  print(paste("Sample size:", nrow(df_funding)))
  
  # Bootstrap CI for infrastructure-funding correlation
  boot_cor_funding <- function(data, indices) {
    d <- data[indices, ]
    return(cor(d$infrastructure_limitation_severity, d$funding_accessibility_score,
               use = "complete.obs"))
  }
  
  boot_result_funding <- boot(df_funding, boot_cor_funding, R = 10000)
  boot_ci_funding <- boot.ci(boot_result_funding, type = "perc", conf = 0.95)
  
  print("Bootstrap 95% CI (Infrastructure × Funding):")
  print(paste("[", round(boot_ci_funding$percent[4], 3), ", ", round(boot_ci_funding$percent[5], 3), "]", sep=""))
}

# Correlation matrix for all three variables
if(nrow(df_funding) > 50) {
  cor_matrix <- cor(df_funding %>% 
                   select(infrastructure_limitation_severity, 
                         geographic_location_remoteness_score,
                         funding_accessibility_score),
                   use = "complete.obs")
  
  print("Correlation matrix:")
  print(round(cor_matrix, 3))
  
  # Partial correlations
  partial_infra_geo <- pcor.test(df_funding$infrastructure_limitation_severity,
                                df_funding$geographic_location_remoteness_score,
                                df_funding$funding_accessibility_score)
  
  partial_infra_funding <- pcor.test(df_funding$infrastructure_limitation_severity,
                                    df_funding$funding_accessibility_score,
                                    df_funding$geographic_location_remoteness_score)
  
  print("Partial correlations:")
  print(paste("Infrastructure × Geography (controlling for funding): r =", 
              round(partial_infra_geo$estimate, 3),
              ", p =", format(partial_infra_geo$p.value, scientific = TRUE)))
  print(paste("Infrastructure × Funding (controlling for geography): r =", 
              round(partial_infra_funding$estimate, 3),
              ", p =", format(partial_infra_funding$p.value, scientific = TRUE)))
}

# Breakdown by geographic region
if(!all(is.na(df$geographic_region))) {
  region_analysis <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    filter(n() >= 20) %>%
    summarise(
      n = n(),
      infrastructure_mean = round(mean(infrastructure_limitation_severity), 2),
      infrastructure_sd = round(sd(infrastructure_limitation_severity), 2),
      location_mean = round(mean(geographic_location_remoteness_score), 2),
      location_sd = round(sd(geographic_location_remoteness_score), 2),
      correlation = round(cor(infrastructure_limitation_severity, 
                             geographic_location_remoteness_score, 
                             use = "complete.obs"), 3),
      .groups = "drop"
    ) %>%
    arrange(desc(infrastructure_mean))
  
  print("Infrastructure limitations by geographic region (n ≥ 20):")
  print(region_analysis)
}

# ANOVA: Infrastructure limitations across geographic regions
if(!all(is.na(df$geographic_region))) {
  df_regions <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    filter(n() >= 20) %>%
    ungroup() %>%
    mutate(geographic_region = droplevels(geographic_region))
  
  if(length(levels(df_regions$geographic_region)) > 1) {
    anova_regions <- lm(infrastructure_limitation_severity ~ geographic_region, 
                       data = df_regions)
    anova_summary <- summary(anova_regions)
    
    print("ANOVA: Infrastructure limitations by geographic region:")
    print(paste("F(", anova_summary$df[1], ",", anova_summary$df[2], ") =",
                round(anova_summary$fstatistic[1], 2)))
    print(paste("p-value:", format(pf(anova_summary$fstatistic[1], 
                                     anova_summary$df[1], 
                                     anova_summary$df[2], 
                                     lower.tail = FALSE), scientific = TRUE)))
    print(paste("R-squared:", round(anova_summary$r.squared, 3)))
  }
}

# Spearman correlations for robustness
spearman_geo <- cor.test(df$infrastructure_limitation_severity,
                        df$geographic_location_remoteness_score,
                        method = "spearman")
print(paste("Spearman's ρ (Infrastructure × Geography) =", round(spearman_geo$estimate, 3)))

if(nrow(df_funding) > 50) {
  spearman_funding <- cor.test(df_funding$infrastructure_limitation_severity,
                              df_funding$funding_accessibility_score,
                              method = "spearman")
  print(paste("Spearman's ρ (Infrastructure × Funding) =", round(spearman_funding$estimate, 3)))
}

# Categorized analysis
df_categories <- df %>%
  mutate(
    infrastructure_level = case_when(
      infrastructure_limitation_severity <= 3 ~ "Low Limitations",
      infrastructure_limitation_severity <= 5 ~ "Moderate Limitations",
      infrastructure_limitation_severity <= 7 ~ "High Limitations"
    ),
    location_remoteness = case_when(
      geographic_location_remoteness_score <= 3 ~ "Urban/Central",
      geographic_location_remoteness_score <= 7 ~ "Suburban/Regional",
      geographic_location_remoteness_score <= 10 ~ "Rural/Remote"
    )
  )

cross_tab <- table(df_categories$infrastructure_level, df_categories$location_remoteness)
print("Cross-tabulation of infrastructure limitations and location remoteness:")
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

# Multiple regression: Infrastructure predicted by geography and funding
if(nrow(df_funding) > 100) {
  lm_multiple <- lm(infrastructure_limitation_severity ~ 
                   geographic_location_remoteness_score + funding_accessibility_score,
                   data = df_funding)
  
  multiple_summary <- summary(lm_multiple)
  print("Multiple regression: Infrastructure ~ Geography + Funding")
  print(multiple_summary)
  
  # Extract key coefficients
  geo_coef <- multiple_summary$coefficients["geographic_location_remoteness_score", ]
  funding_coef <- multiple_summary$coefficients["funding_accessibility_score", ]
  
  print("Geography coefficient:")
  print(paste("β =", round(geo_coef[1], 3), ", SE =", round(geo_coef[2], 3),
              ", p =", format(geo_coef[4], scientific = TRUE)))
  
  print("Funding coefficient:")
  print(paste("β =", round(funding_coef[1], 3), ", SE =", round(funding_coef[2], 3),
              ", p =", format(funding_coef[4], scientific = TRUE)))
}

# Effect size interpretation for primary correlation
cor_magnitude <- abs(cor_infra_geo$estimate)
effect_size_interpretation <- case_when(
  cor_magnitude < 0.1 ~ "Negligible",
  cor_magnitude < 0.3 ~ "Small", 
  cor_magnitude < 0.5 ~ "Medium",
  cor_magnitude < 0.7 ~ "Large",
  TRUE ~ "Very Large"
)

print(paste("Effect size interpretation (Infrastructure × Geography):", effect_size_interpretation))

# Expected: r = .241, p < .001 (correlation with geographic location)