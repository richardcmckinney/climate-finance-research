# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 95: "Technology transfer mechanisms: success rate (62.3%, 95% CI [58.1%, 66.5%])"
# Purpose: Analyze technology transfer success rates and confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)
library(PropCIs)
library(binom)
library(epitools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TECH_TRANSFER_SUCCESS <- "technology_transfer_successful"
COL_TRANSFER_MECHANISM <- "primary_transfer_mechanism"
COL_TRANSFER_VALUE <- "technology_transfer_value"
COL_TIME_TO_TRANSFER <- "time_to_transfer_months"
COL_RECIPIENT_TYPE <- "technology_recipient_type"
COL_TECHNOLOGY_READINESS <- "technology_readiness_level"
COL_IP_PROTECTION <- "ip_protection_strength"
COL_SUPPORT_SERVICES <- "transfer_support_services_used"
COL_BARRIERS_ENCOUNTERED <- "transfer_barriers_count"
COL_STAKEHOLDER <- "stakeholder"
COL_INDUSTRY <- "industry_sector"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    technology_transfer_successful = as.numeric(technology_transfer_successful),
    primary_transfer_mechanism = factor(primary_transfer_mechanism),
    technology_transfer_value = as.numeric(technology_transfer_value),
    time_to_transfer_months = as.numeric(time_to_transfer_months),
    technology_recipient_type = factor(technology_recipient_type),
    technology_readiness_level = as.numeric(technology_readiness_level),
    ip_protection_strength = as.numeric(ip_protection_strength),
    transfer_support_services_used = as.numeric(transfer_support_services_used),
    transfer_barriers_count = as.numeric(transfer_barriers_count),
    stakeholder = factor(stakeholder),
    industry_sector = factor(industry_sector)
  ) %>%
  filter(!is.na(technology_transfer_successful))

# Convert to binary if needed (assuming 1 = success, 0 = failure)
df$transfer_success_binary <- ifelse(df$technology_transfer_successful >= 4, 1, 0)

# Primary success rate calculation
n_total <- nrow(df)
n_success <- sum(df$transfer_success_binary)
n_failure <- n_total - n_success
success_rate <- n_success / n_total

print("Technology Transfer Success Rate Analysis:")
print(paste("Total transfers analyzed:", n_total))
print(paste("Successful transfers:", n_success))
print(paste("Failed transfers:", n_failure))
print(paste("Success rate:", round(success_rate * 100, 1), "%"))

# Exact binomial confidence interval (Clopper-Pearson)
binom_ci <- binom.test(n_success, n_total, conf.level = 0.95)
ci_lower <- binom_ci$conf.int[1] * 100
ci_upper <- binom_ci$conf.int[2] * 100

print(paste("\n95% CI (Exact binomial): [", round(ci_lower, 1), "%,", 
           round(ci_upper, 1), "%]"))

# Wilson score interval (recommended for proportions)
wilson_ci <- prop.test(n_success, n_total, correct = FALSE)
wilson_lower <- wilson_ci$conf.int[1] * 100
wilson_upper <- wilson_ci$conf.int[2] * 100

print(paste("95% CI (Wilson): [", round(wilson_lower, 1), "%,", 
           round(wilson_upper, 1), "%]"))

# Agresti-Coull interval
ac_ci <- binom.confint(n_success, n_total, method = "ac")
print(paste("95% CI (Agresti-Coull): [", round(ac_ci$lower * 100, 1), "%,", 
           round(ac_ci$upper * 100, 1), "%]"))

# Jeffreys interval (Bayesian approach)
jeffreys_ci <- binom.confint(n_success, n_total, method = "bayes")
print(paste("95% CI (Jeffreys): [", round(jeffreys_ci$lower * 100, 1), "%,", 
           round(jeffreys_ci$upper * 100, 1), "%]"))

# Standard error and normal approximation CI
se_proportion <- sqrt(success_rate * (1 - success_rate) / n_total)
normal_ci_lower <- (success_rate - 1.96 * se_proportion) * 100
normal_ci_upper <- (success_rate + 1.96 * se_proportion) * 100

print(paste("\n95% CI (Normal approximation): [", round(normal_ci_lower, 1), "%,", 
           round(normal_ci_upper, 1), "%]"))
print(paste("Standard error:", round(se_proportion * 100, 2), "%"))

# Bootstrap confidence interval
set.seed(123)
n_boot <- 10000
boot_success_rates <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$transfer_success_binary, size = n_total, replace = TRUE)
  boot_success_rates[i] <- mean(boot_sample)
}

boot_ci <- quantile(boot_success_rates, c(0.025, 0.975)) * 100

print(paste("\n95% CI (Bootstrap): [", round(boot_ci[1], 1), "%,", 
           round(boot_ci[2], 1), "%]"))

# Success rate by transfer mechanism
if(!all(is.na(df$primary_transfer_mechanism))) {
  mechanism_stats <- df %>%
    filter(!is.na(primary_transfer_mechanism)) %>%
    group_by(primary_transfer_mechanism) %>%
    summarise(
      n = n(),
      successes = sum(transfer_success_binary),
      success_rate = mean(transfer_success_binary) * 100,
      ci_lower = binom.test(successes, n)$conf.int[1] * 100,
      ci_upper = binom.test(successes, n)$conf.int[2] * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(success_rate))
  
  print("\nSuccess rates by transfer mechanism:")
  print(mechanism_stats)
  
  # Chi-square test for independence
  if(nrow(mechanism_stats) > 1) {
    contingency_table <- table(df$primary_transfer_mechanism, df$transfer_success_binary)
    chi_sq_test <- chisq.test(contingency_table)
    
    print("\nChi-square test (mechanism × success):")
    print(paste("χ² =", round(chi_sq_test$statistic, 2)))
    print(paste("df =", chi_sq_test$parameter))
    print(paste("p-value:", format(chi_sq_test$p.value, scientific = TRUE)))
    
    # Cramér's V effect size
    cramers_v <- sqrt(chi_sq_test$statistic / (n_total * (min(dim(contingency_table)) - 1)))
    print(paste("Cramér's V:", round(cramers_v, 3)))
  }
}

# Success rate by technology readiness level
if(sum(!is.na(df$technology_readiness_level)) > 30) {
  # Create TRL categories
  df <- df %>%
    mutate(trl_category = case_when(
      technology_readiness_level <= 3 ~ "Early (TRL 1-3)",
      technology_readiness_level <= 6 ~ "Middle (TRL 4-6)",
      technology_readiness_level <= 9 ~ "Late (TRL 7-9)",
      TRUE ~ NA_character_
    ))
  
  trl_stats <- df %>%
    filter(!is.na(trl_category)) %>%
    group_by(trl_category) %>%
    summarise(
      n = n(),
      successes = sum(transfer_success_binary),
      success_rate = mean(transfer_success_binary) * 100,
      .groups = "drop"
    )
  
  print("\nSuccess rates by technology readiness level:")
  print(trl_stats)
  
  # Logistic regression with TRL
  logit_trl <- glm(transfer_success_binary ~ technology_readiness_level, 
                   data = df, family = binomial)
  
  print("\nLogistic regression (TRL predicting success):")
  print(summary(logit_trl))
  
  # Odds ratio per unit increase in TRL
  or_trl <- exp(coef(logit_trl)[2])
  or_ci <- exp(confint(logit_trl)[2, ])
  
  print(paste("Odds ratio per TRL unit:", round(or_trl, 2)))
  print(paste("95% CI:", round(or_ci[1], 2), "-", round(or_ci[2], 2)))
}

# Success rate by recipient type
if(!all(is.na(df$technology_recipient_type))) {
  recipient_stats <- df %>%
    filter(!is.na(technology_recipient_type)) %>%
    group_by(technology_recipient_type) %>%
    summarise(
      n = n(),
      successes = sum(transfer_success_binary),
      success_rate = mean(transfer_success_binary) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(success_rate))
  
  print("\nSuccess rates by recipient type:")
  print(recipient_stats)
}

# IP protection strength analysis
if(sum(!is.na(df$ip_protection_strength)) > 30) {
  # Compare success rates by IP protection level
  df <- df %>%
    mutate(ip_protection_category = case_when(
      ip_protection_strength <= 2 ~ "Weak",
      ip_protection_strength <= 4 ~ "Moderate",
      TRUE ~ "Strong"
    ))
  
  ip_stats <- df %>%
    filter(!is.na(ip_protection_category)) %>%
    group_by(ip_protection_category) %>%
    summarise(
      n = n(),
      successes = sum(transfer_success_binary),
      success_rate = mean(transfer_success_binary) * 100,
      .groups = "drop"
    )
  
  print("\nSuccess rates by IP protection strength:")
  print(ip_stats)
  
  # Cochran-Armitage trend test
  if(nrow(ip_stats) > 2) {
    print("\nCochran-Armitage test for trend would be appropriate here")
  }
}

# Time to transfer analysis
if(sum(!is.na(df$time_to_transfer_months)) > 30) {
  time_comparison <- df %>%
    group_by(transfer_success_binary) %>%
    summarise(
      n = n(),
      mean_time = mean(time_to_transfer_months, na.rm = TRUE),
      median_time = median(time_to_transfer_months, na.rm = TRUE),
      sd_time = sd(time_to_transfer_months, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nTime to transfer by success status:")
  print(time_comparison)
  
  # t-test for time difference
  t_test_time <- t.test(time_to_transfer_months ~ transfer_success_binary, data = df)
  print(paste("t-test: t =", round(t_test_time$statistic, 2)))
  print(paste("p-value:", format(t_test_time$p.value, scientific = TRUE)))
}

# Barriers analysis
if(sum(!is.na(df$transfer_barriers_count)) > 30) {
  barriers_comparison <- df %>%
    group_by(transfer_success_binary) %>%
    summarise(
      n = n(),
      mean_barriers = mean(transfer_barriers_count, na.rm = TRUE),
      median_barriers = median(transfer_barriers_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nBarriers encountered by success status:")
  print(barriers_comparison)
  
  # Logistic regression with barriers
  logit_barriers <- glm(transfer_success_binary ~ transfer_barriers_count, 
                        data = df, family = binomial)
  print("\nLogistic regression (barriers predicting success):")
  print(summary(logit_barriers))
}

# Support services analysis
if(sum(!is.na(df$transfer_support_services_used)) > 30) {
  support_comparison <- df %>%
    group_by(transfer_success_binary) %>%
    summarise(
      n = n(),
      mean_support = mean(transfer_support_services_used, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nSupport services used by success status:")
  print(support_comparison)
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    successes = sum(transfer_success_binary),
    success_rate = mean(transfer_success_binary) * 100,
    ci_lower = ifelse(n >= 10, binom.test(successes, n)$conf.int[1] * 100, NA),
    ci_upper = ifelse(n >= 10, binom.test(successes, n)$conf.int[2] * 100, NA),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(desc(success_rate))

print("\nSuccess rates by stakeholder:")
print(stakeholder_stats)

# Industry sector analysis
if(length(unique(df$industry_sector)) > 3) {
  industry_stats <- df %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      successes = sum(transfer_success_binary),
      success_rate = mean(transfer_success_binary) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(success_rate))
  
  print("\nTop industries by transfer success rate:")
  print(head(industry_stats, 10))
}

# Multiple logistic regression
predictors <- c("technology_readiness_level", "ip_protection_strength", 
               "transfer_barriers_count", "transfer_support_services_used")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("transfer_success_binary ~", 
                      paste(available_predictors, collapse = " + "))
  
  logit_full <- glm(as.formula(formula_str), data = df, family = binomial)
  
  print("\nMultiple logistic regression model:")
  print(summary(logit_full))
  
  # Odds ratios for all predictors
  or_all <- exp(coef(logit_full))
  print("\nOdds ratios:")
  print(round(or_all, 2))
  
  # Model fit statistics
  print(paste("\nAIC:", round(AIC(logit_full), 2)))
  print(paste("BIC:", round(BIC(logit_full), 2)))
  
  # Pseudo R-squared (McFadden)
  null_model <- glm(transfer_success_binary ~ 1, data = df, family = binomial)
  mcfadden_r2 <- 1 - (logLik(logit_full) / logLik(null_model))
  print(paste("McFadden's R²:", round(mcfadden_r2, 3)))
}

# Temporal analysis (if time data available)
if("transfer_year" %in% names(df)) {
  yearly_stats <- df %>%
    filter(!is.na(transfer_year)) %>%
    group_by(transfer_year) %>%
    summarise(
      n = n(),
      success_rate = mean(transfer_success_binary) * 100,
      .groups = "drop"
    ) %>%
    arrange(transfer_year)
  
  print("\nSuccess rates by year:")
  print(yearly_stats)
  
  # Trend test
  cor_trend <- cor.test(yearly_stats$transfer_year, yearly_stats$success_rate)
  print(paste("Trend correlation: r =", round(cor_trend$estimate, 3)))
}

# Sample size calculations for future studies
# For detecting 5% difference from current rate
p0 <- success_rate
p1 <- p0 + 0.05
effect_size <- ES.h(p1, p0)

power_calc <- pwr.p.test(h = effect_size, sig.level = 0.05, power = 0.80)
print(paste("\nSample size needed to detect 5% difference (80% power):", 
           ceiling(power_calc$n)))

# Summary statistics for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Technology transfer success rate:", round(success_rate * 100, 1), "%"))
print(paste("95% Confidence Interval: [", round(ci_lower, 1), "%,", 
           round(ci_upper, 1), "%]"))
print(paste("Based on", n_total, "technology transfers"))
print(paste("Number of successful transfers:", n_success))
print(paste("Number of failed transfers:", n_failure))
print("The confidence interval suggests the true population success rate")
print(paste("lies between", round(ci_lower, 1), "% and", round(ci_upper, 1), "%"))

# Expected: success rate (62.3%, 95% CI [58.1%, 66.5%])