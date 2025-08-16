# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 94: "Licensing agreement structures: complexity index (M = 3.45, SD = 0.93)"
# Purpose: Analyze licensing agreement complexity index scores

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_LICENSING_COMPLEXITY <- "licensing_agreement_complexity_index"
COL_LICENSE_TYPE <- "primary_license_type"
COL_ROYALTY_STRUCTURE <- "royalty_structure_type"
COL_MILESTONE_PAYMENTS <- "milestone_payment_count"
COL_TERRITORY_SCOPE <- "territory_scope_score"
COL_EXCLUSIVITY <- "exclusivity_level"
COL_SUBLICENSING_RIGHTS <- "sublicensing_allowed"
COL_TECHNOLOGY_FIELD <- "technology_field"
COL_DEAL_VALUE <- "total_deal_value"
COL_NEGOTIATION_DURATION <- "negotiation_duration_days"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    licensing_agreement_complexity_index = as.numeric(licensing_agreement_complexity_index),
    primary_license_type = factor(primary_license_type),
    royalty_structure_type = factor(royalty_structure_type),
    milestone_payment_count = as.numeric(milestone_payment_count),
    territory_scope_score = as.numeric(territory_scope_score),
    exclusivity_level = factor(exclusivity_level,
                             levels = c("Non-exclusive", "Semi-exclusive", "Exclusive"),
                             ordered = TRUE),
    sublicensing_allowed = factor(sublicensing_allowed),
    technology_field = factor(technology_field),
    total_deal_value = as.numeric(total_deal_value),
    negotiation_duration_days = as.numeric(negotiation_duration_days),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(licensing_agreement_complexity_index))

# Primary descriptive statistics
n_total <- nrow(df)
mean_complexity <- mean(df$licensing_agreement_complexity_index, na.rm = TRUE)
sd_complexity <- sd(df$licensing_agreement_complexity_index, na.rm = TRUE)
median_complexity <- median(df$licensing_agreement_complexity_index, na.rm = TRUE)
se_complexity <- sd_complexity / sqrt(n_total)

print("Licensing Agreement Complexity Index - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_complexity, 2)))
print(paste("SD =", round(sd_complexity, 2)))
print(paste("Median =", round(median_complexity, 2)))
print(paste("SE =", round(se_complexity, 3)))

# Confidence intervals
ci_lower <- mean_complexity - qt(0.975, n_total - 1) * se_complexity
ci_upper <- mean_complexity + qt(0.975, n_total - 1) * se_complexity

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$licensing_agreement_complexity_index)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution characteristics
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$licensing_agreement_complexity_index, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$licensing_agreement_complexity_index, na.rm = TRUE), 3)))

# Percentiles
percentiles <- quantile(df$licensing_agreement_complexity_index, 
                       probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95), 
                       na.rm = TRUE)
print("\nPercentiles:")
print(round(percentiles, 2))

# Coefficient of variation
cv <- (sd_complexity / mean_complexity) * 100
print(paste("\nCoefficient of variation:", round(cv, 1), "%"))

# Mode calculation
density_est <- density(df$licensing_agreement_complexity_index)
mode_value <- density_est$x[which.max(density_est$y)]
print(paste("Mode (kernel density estimate):", round(mode_value, 2)))

# Normality tests
shapiro_result <- shapiro.test(df$licensing_agreement_complexity_index[1:min(5000, n_total)])
print("\nShapiro-Wilk normality test:")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint
scale_midpoint <- 3
t_test_midpoint <- t.test(df$licensing_agreement_complexity_index, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_complexity - scale_midpoint, 2)))

# Bootstrap confidence intervals
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$licensing_agreement_complexity_index, 
                       size = n_total, 
                       replace = TRUE)
  boot_means[i] <- mean(boot_sample)
  boot_sds[i] <- sd(boot_sample)
}

boot_ci_mean <- quantile(boot_means, c(0.025, 0.975))
boot_ci_sd <- quantile(boot_sds, c(0.025, 0.975))

print("\nBootstrap 95% CI for mean:")
print(round(boot_ci_mean, 3))
print("Bootstrap 95% CI for SD:")
print(round(boot_ci_sd, 3))

# Analysis by license type
if(!all(is.na(df$primary_license_type))) {
  license_stats <- df %>%
    filter(!is.na(primary_license_type)) %>%
    group_by(primary_license_type) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      sd_complexity = round(sd(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_complexity))
  
  print("\nComplexity by primary license type:")
  print(license_stats)
  
  # ANOVA if multiple types
  if(length(unique(df$primary_license_type)) > 2) {
    anova_license <- aov(licensing_agreement_complexity_index ~ primary_license_type, 
                        data = df)
    print("\nANOVA by license type:")
    print(summary(anova_license))
  }
}

# Royalty structure analysis
if(!all(is.na(df$royalty_structure_type))) {
  royalty_stats <- df %>%
    filter(!is.na(royalty_structure_type)) %>%
    group_by(royalty_structure_type) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      sd_complexity = round(sd(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nComplexity by royalty structure:")
  print(royalty_stats)
}

# Milestone payments correlation
if(sum(!is.na(df$milestone_payment_count)) > 30) {
  cor_milestones <- cor.test(df$licensing_agreement_complexity_index,
                            df$milestone_payment_count,
                            use = "complete.obs")
  
  print("\nCorrelation with milestone payment count:")
  print(paste("r =", round(cor_milestones$estimate, 3)))
  print(paste("p-value:", format(cor_milestones$p.value, scientific = TRUE)))
  
  # Categorize milestone payments
  df <- df %>%
    mutate(milestone_category = case_when(
      milestone_payment_count == 0 ~ "No milestones",
      milestone_payment_count <= 3 ~ "1-3 milestones",
      milestone_payment_count <= 6 ~ "4-6 milestones",
      TRUE ~ "7+ milestones"
    ))
  
  milestone_category_stats <- df %>%
    filter(!is.na(milestone_category)) %>%
    group_by(milestone_category) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nComplexity by milestone payment categories:")
  print(milestone_category_stats)
}

# Territory scope analysis
if(sum(!is.na(df$territory_scope_score)) > 30) {
  cor_territory <- cor.test(df$licensing_agreement_complexity_index,
                           df$territory_scope_score,
                           use = "complete.obs")
  
  print("\nCorrelation with territory scope:")
  print(paste("r =", round(cor_territory$estimate, 3)))
  print(paste("p-value:", format(cor_territory$p.value, scientific = TRUE)))
}

# Exclusivity level analysis
if(!all(is.na(df$exclusivity_level))) {
  exclusivity_stats <- df %>%
    filter(!is.na(exclusivity_level)) %>%
    group_by(exclusivity_level) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      sd_complexity = round(sd(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nComplexity by exclusivity level:")
  print(exclusivity_stats)
  
  # Jonckheere-Terpstra test for ordered factor
  if(nrow(exclusivity_stats) > 2) {
    jt_test <- jonckheere.test(df$licensing_agreement_complexity_index,
                               df$exclusivity_level,
                               alternative = "increasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# Sublicensing rights analysis
if(!all(is.na(df$sublicensing_allowed))) {
  sublicensing_comparison <- df %>%
    filter(!is.na(sublicensing_allowed)) %>%
    group_by(sublicensing_allowed) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      sd_complexity = round(sd(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nComplexity by sublicensing rights:")
  print(sublicensing_comparison)
  
  # t-test if binary
  if(length(unique(df$sublicensing_allowed)) == 2) {
    t_test_sublicense <- t.test(licensing_agreement_complexity_index ~ sublicensing_allowed, 
                                data = df)
    print(paste("t-test: t =", round(t_test_sublicense$statistic, 2)))
    print(paste("p-value:", format(t_test_sublicense$p.value, scientific = TRUE)))
  }
}

# Deal value correlation
if(sum(!is.na(df$total_deal_value)) > 30) {
  # Log transform deal value
  df$log_deal_value <- log10(df$total_deal_value + 1)
  
  cor_deal <- cor.test(df$licensing_agreement_complexity_index,
                      df$log_deal_value,
                      use = "complete.obs")
  
  print("\nCorrelation with log(deal value):")
  print(paste("r =", round(cor_deal$estimate, 3)))
  print(paste("p-value:", format(cor_deal$p.value, scientific = TRUE)))
  
  # Regression analysis
  lm_deal <- lm(licensing_agreement_complexity_index ~ log_deal_value, data = df)
  print("\nRegression - Complexity predicted by log(deal value):")
  print(summary(lm_deal))
}

# Negotiation duration analysis
if(sum(!is.na(df$negotiation_duration_days)) > 30) {
  cor_duration <- cor.test(df$licensing_agreement_complexity_index,
                          df$negotiation_duration_days,
                          use = "complete.obs")
  
  print("\nCorrelation with negotiation duration:")
  print(paste("r =", round(cor_duration$estimate, 3)))
  print(paste("p-value:", format(cor_duration$p.value, scientific = TRUE)))
  
  # Categorize negotiation duration
  df <- df %>%
    mutate(duration_category = case_when(
      negotiation_duration_days <= 30 ~ "Quick (≤30 days)",
      negotiation_duration_days <= 90 ~ "Standard (31-90 days)",
      negotiation_duration_days <= 180 ~ "Extended (91-180 days)",
      TRUE ~ "Prolonged (>180 days)"
    ))
  
  duration_stats <- df %>%
    filter(!is.na(duration_category)) %>%
    group_by(duration_category) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nComplexity by negotiation duration category:")
  print(duration_stats)
}

# Technology field analysis
if(!all(is.na(df$technology_field))) {
  tech_stats <- df %>%
    filter(!is.na(technology_field)) %>%
    group_by(technology_field) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      sd_complexity = round(sd(licensing_agreement_complexity_index, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_complexity))
  
  print("\nTop technology fields by licensing complexity:")
  print(head(tech_stats, 10))
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_complexity = round(mean(licensing_agreement_complexity_index, na.rm = TRUE), 2),
    sd_complexity = round(sd(licensing_agreement_complexity_index, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_complexity))

print("\nLicensing complexity by stakeholder:")
print(stakeholder_stats)

# Multiple regression model
predictors <- c("milestone_payment_count", "territory_scope_score", "log_deal_value")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("licensing_agreement_complexity_index ~", 
                      paste(available_predictors, collapse = " + "))
  
  lm_multiple <- lm(as.formula(formula_str), data = df)
  
  print("\nMultiple regression model:")
  print(summary(lm_multiple))
}

# Outlier analysis
z_scores <- abs(scale(df$licensing_agreement_complexity_index))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

# Robust statistics
mad_value <- mad(df$licensing_agreement_complexity_index, na.rm = TRUE)
trimmed_mean <- mean(df$licensing_agreement_complexity_index, trim = 0.1, na.rm = TRUE)
winsorized_mean <- mean(Winsorize(df$licensing_agreement_complexity_index, probs = c(0.05, 0.95)))

print(paste("\nMedian Absolute Deviation (MAD):", round(mad_value, 3)))
print(paste("10% Trimmed mean:", round(trimmed_mean, 2)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 2)))

# Floor and ceiling effects
floor_effect <- sum(df$licensing_agreement_complexity_index == min(df$licensing_agreement_complexity_index)) / n_total * 100
ceiling_effect <- sum(df$licensing_agreement_complexity_index == max(df$licensing_agreement_complexity_index)) / n_total * 100

print(paste("\nFloor effect:", round(floor_effect, 1), "%"))
print(paste("Ceiling effect:", round(ceiling_effect, 1), "%"))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Licensing agreement complexity index: M =", round(mean_complexity, 2), 
           ", SD =", round(sd_complexity, 2)))
print(paste("N =", n_total))
print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print("The moderate mean suggests typical licensing agreements have medium complexity,")
print("with reasonable variability indicating diverse agreement structures in the sample.")

# Expected: complexity index (M = 3.45, SD = 0.93)