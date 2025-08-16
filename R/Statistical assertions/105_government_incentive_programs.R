# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 105: "Government incentive programs: utilization rate (43.7%, 95% CI [39.2%, 48.2%])"
# Purpose: Analyze government incentive program utilization rates

library(tidyverse)
library(janitor)
library(DescTools)
library(PropCIs)
library(binom)
library(epitools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_INCENTIVE_UTILIZATION <- "government_incentive_utilized"
COL_INCENTIVE_TYPE <- "incentive_program_type"
COL_COMPANY_SIZE <- "company_size_category"
COL_INDUSTRY <- "industry_sector"
COL_GEOGRAPHIC_REGION <- "geographic_region"
COL_AWARENESS_LEVEL <- "incentive_awareness_score"
COL_APPLICATION_DIFFICULTY <- "application_difficulty_rating"
COL_BENEFIT_RECEIVED <- "benefit_amount_received"
COL_TIME_TO_APPROVAL <- "approval_time_days"
COL_REAPPLICATION <- "would_reapply"
COL_SUCCESS_IMPACT <- "business_success_impact"
COL_COMPLIANCE_BURDEN <- "compliance_burden_score"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    government_incentive_utilized = as.numeric(government_incentive_utilized),
    incentive_program_type = factor(incentive_program_type),
    company_size_category = factor(company_size_category,
                                  levels = c("Micro", "Small", "Medium", "Large"),
                                  ordered = TRUE),
    industry_sector = factor(industry_sector),
    geographic_region = factor(geographic_region),
    incentive_awareness_score = as.numeric(incentive_awareness_score),
    application_difficulty_rating = as.numeric(application_difficulty_rating),
    benefit_amount_received = as.numeric(benefit_amount_received),
    approval_time_days = as.numeric(approval_time_days),
    would_reapply = factor(would_reapply),
    business_success_impact = as.numeric(business_success_impact),
    compliance_burden_score = as.numeric(compliance_burden_score),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(government_incentive_utilized))

# Convert to binary (1 = utilized, 0 = not utilized)
df$utilized <- ifelse(df$government_incentive_utilized >= 1, 1, 0)

# Primary utilization rate calculation
n_total <- nrow(df)
n_utilized <- sum(df$utilized)
n_not_utilized <- n_total - n_utilized
utilization_rate <- n_utilized / n_total

print("Government Incentive Programs Utilization Rate Analysis:")
print(paste("Total eligible entities:", n_total))
print(paste("Utilized incentives:", n_utilized))
print(paste("Did not utilize:", n_not_utilized))
print(paste("Utilization rate:", round(utilization_rate * 100, 1), "%"))

# Multiple confidence interval methods
print("\n=== CONFIDENCE INTERVALS ===")

# 1. Wilson score interval (recommended)
wilson_ci <- prop.test(n_utilized, n_total, correct = FALSE)
ci_wilson_lower <- wilson_ci$conf.int[1] * 100
ci_wilson_upper <- wilson_ci$conf.int[2] * 100

print(paste("95% CI (Wilson): [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))

# 2. Exact binomial (Clopper-Pearson)
binom_ci <- binom.test(n_utilized, n_total, conf.level = 0.95)
ci_exact_lower <- binom_ci$conf.int[1] * 100
ci_exact_upper <- binom_ci$conf.int[2] * 100

print(paste("95% CI (Exact/Clopper-Pearson): [", round(ci_exact_lower, 1), "%,", 
           round(ci_exact_upper, 1), "%]"))

# 3. Agresti-Coull interval
ac_ci <- binom.confint(n_utilized, n_total, method = "ac")
print(paste("95% CI (Agresti-Coull): [", round(ac_ci$lower * 100, 1), "%,", 
           round(ac_ci$upper * 100, 1), "%]"))

# 4. Jeffreys interval
jeffreys_ci <- binom.confint(n_utilized, n_total, method = "bayes")
print(paste("95% CI (Jeffreys/Bayesian): [", round(jeffreys_ci$lower * 100, 1), "%,", 
           round(jeffreys_ci$upper * 100, 1), "%]"))

# 5. Normal approximation
se_proportion <- sqrt(utilization_rate * (1 - utilization_rate) / n_total)
ci_normal_lower <- (utilization_rate - 1.96 * se_proportion) * 100
ci_normal_upper <- (utilization_rate + 1.96 * se_proportion) * 100

print(paste("95% CI (Normal approximation): [", round(ci_normal_lower, 1), "%,", 
           round(ci_normal_upper, 1), "%]"))
print(paste("Standard error:", round(se_proportion * 100, 2), "%"))

# Bootstrap confidence interval
set.seed(123)
n_boot <- 10000
boot_rates <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$utilized, size = n_total, replace = TRUE)
  boot_rates[i] <- mean(boot_sample)
}

boot_ci <- quantile(boot_rates, c(0.025, 0.975)) * 100

print(paste("\n95% CI (Bootstrap): [", round(boot_ci[1], 1), "%,", 
           round(boot_ci[2], 1), "%]"))

# Utilization by incentive type
if(!all(is.na(df$incentive_program_type))) {
  type_stats <- df %>%
    filter(!is.na(incentive_program_type)) %>%
    group_by(incentive_program_type) %>%
    summarise(
      n = n(),
      utilized_count = sum(utilized),
      utilization_rate = mean(utilized) * 100,
      ci_lower = ifelse(n >= 10, 
                       binom.test(utilized_count, n)$conf.int[1] * 100, NA),
      ci_upper = ifelse(n >= 10, 
                       binom.test(utilized_count, n)$conf.int[2] * 100, NA),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(utilization_rate))
  
  print("\nUtilization rates by incentive type:")
  print(type_stats)
  
  # Chi-square test
  if(nrow(type_stats) > 1) {
    contingency <- table(df$incentive_program_type, df$utilized)
    chi_test <- chisq.test(contingency)
    
    print("\nChi-square test (type × utilization):")
    print(paste("χ² =", round(chi_test$statistic, 2)))
    print(paste("p-value:", format(chi_test$p.value, scientific = TRUE)))
  }
}

# Company size analysis
if(!all(is.na(df$company_size_category))) {
  size_stats <- df %>%
    filter(!is.na(company_size_category)) %>%
    group_by(company_size_category) %>%
    summarise(
      n = n(),
      utilization_rate = mean(utilized) * 100,
      .groups = "drop"
    )
  
  print("\nUtilization rate by company size:")
  print(size_stats)
  
  # Cochran-Armitage trend test
  if(nrow(size_stats) > 2) {
    print("\nCochran-Armitage test for trend would be appropriate")
  }
}

# Industry analysis
if(length(unique(df$industry_sector)) > 3) {
  industry_stats <- df %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      utilization_rate = mean(utilized) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(utilization_rate))
  
  print("\nTop industries by utilization rate:")
  print(head(industry_stats, 10))
}

# Geographic region analysis
if(!all(is.na(df$geographic_region))) {
  region_stats <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    summarise(
      n = n(),
      utilization_rate = mean(utilized) * 100,
      ci_lower = ifelse(n >= 20, 
                       binom.test(sum(utilized), n)$conf.int[1] * 100, NA),
      ci_upper = ifelse(n >= 20, 
                       binom.test(sum(utilized), n)$conf.int[2] * 100, NA),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(utilization_rate))
  
  print("\nUtilization by geographic region:")
  print(region_stats)
}

# Awareness level analysis
if(sum(!is.na(df$incentive_awareness_score)) > 50) {
  awareness_comparison <- df %>%
    group_by(utilized) %>%
    summarise(
      n = n(),
      mean_awareness = mean(incentive_awareness_score, na.rm = TRUE),
      sd_awareness = sd(incentive_awareness_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nAwareness score by utilization status:")
  print(awareness_comparison)
  
  # Logistic regression
  logit_awareness <- glm(utilized ~ incentive_awareness_score, 
                        data = df, family = binomial)
  print("\nLogistic regression (awareness predicting utilization):")
  print(summary(logit_awareness))
  
  # Odds ratio
  or_awareness <- exp(coef(logit_awareness)[2])
  print(paste("Odds ratio per unit increase in awareness:", round(or_awareness, 3)))
}

# Application difficulty analysis
if(sum(!is.na(df$application_difficulty_rating)) > 50) {
  difficulty_stats <- df %>%
    filter(utilized == 1, !is.na(application_difficulty_rating)) %>%
    summarise(
      n = n(),
      mean_difficulty = mean(application_difficulty_rating, na.rm = TRUE),
      sd_difficulty = sd(application_difficulty_rating, na.rm = TRUE),
      median_difficulty = median(application_difficulty_rating, na.rm = TRUE)
    )
  
  print("\nApplication difficulty (among utilizers):")
  print(difficulty_stats)
  
  # Categorize difficulty
  df <- df %>%
    mutate(difficulty_category = case_when(
      application_difficulty_rating <= 2 ~ "Easy",
      application_difficulty_rating <= 4 ~ "Moderate",
      TRUE ~ "Difficult"
    ))
  
  difficulty_utilization <- df %>%
    filter(!is.na(difficulty_category)) %>%
    group_by(difficulty_category) %>%
    summarise(
      n = n(),
      utilization_rate = mean(utilized) * 100,
      .groups = "drop"
    )
  
  print("\nUtilization by perceived difficulty:")
  print(difficulty_utilization)
}

# Benefit amount analysis
if(sum(!is.na(df$benefit_amount_received)) > 30) {
  benefit_summary <- df %>%
    filter(utilized == 1, !is.na(benefit_amount_received)) %>%
    summarise(
      n = n(),
      mean_benefit = mean(benefit_amount_received, na.rm = TRUE),
      median_benefit = median(benefit_amount_received, na.rm = TRUE),
      total_benefits = sum(benefit_amount_received, na.rm = TRUE)
    )
  
  print("\nBenefit amounts received:")
  print(benefit_summary)
}

# Time to approval analysis
if(sum(!is.na(df$approval_time_days)) > 30) {
  approval_time_summary <- df %>%
    filter(utilized == 1, !is.na(approval_time_days)) %>%
    summarise(
      n = n(),
      mean_days = mean(approval_time_days, na.rm = TRUE),
      median_days = median(approval_time_days, na.rm = TRUE),
      min_days = min(approval_time_days, na.rm = TRUE),
      max_days = max(approval_time_days, na.rm = TRUE)
    )
  
  print("\nApproval time statistics:")
  print(approval_time_summary)
}

# Reapplication willingness
if(!all(is.na(df$would_reapply))) {
  reapply_stats <- df %>%
    filter(utilized == 1, !is.na(would_reapply)) %>%
    count(would_reapply) %>%
    mutate(percentage = n / sum(n) * 100)
  
  print("\nWould reapply (among utilizers):")
  print(reapply_stats)
}

# Business impact analysis
if(sum(!is.na(df$business_success_impact)) > 30) {
  impact_comparison <- df %>%
    group_by(utilized) %>%
    summarise(
      n = n(),
      mean_impact = mean(business_success_impact, na.rm = TRUE),
      sd_impact = sd(business_success_impact, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nBusiness success impact by utilization:")
  print(impact_comparison)
  
  # t-test
  t_test_impact <- t.test(business_success_impact ~ utilized, data = df)
  print(paste("t-test: t =", round(t_test_impact$statistic, 2)))
  print(paste("p-value:", format(t_test_impact$p.value, scientific = TRUE)))
}

# Compliance burden analysis
if(sum(!is.na(df$compliance_burden_score)) > 30) {
  compliance_stats <- df %>%
    filter(utilized == 1, !is.na(compliance_burden_score)) %>%
    summarise(
      n = n(),
      mean_burden = mean(compliance_burden_score, na.rm = TRUE),
      sd_burden = sd(compliance_burden_score, na.rm = TRUE)
    )
  
  print("\nCompliance burden (among utilizers):")
  print(compliance_stats)
}

# Multiple logistic regression
predictors <- c("incentive_awareness_score", "application_difficulty_rating",
               "compliance_burden_score")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("utilized ~", paste(available_predictors, collapse = " + "))
  
  logit_full <- glm(as.formula(formula_str), data = df, family = binomial)
  
  print("\nMultiple logistic regression model:")
  print(summary(logit_full))
  
  # Odds ratios
  or_all <- exp(coef(logit_full))
  print("\nOdds ratios:")
  print(round(or_all, 3))
  
  # Model fit
  print(paste("\nAIC:", round(AIC(logit_full), 2)))
  
  # Pseudo R-squared
  null_model <- glm(utilized ~ 1, data = df, family = binomial)
  mcfadden_r2 <- 1 - (logLik(logit_full) / logLik(null_model))
  print(paste("McFadden's R²:", round(mcfadden_r2, 3)))
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    utilization_rate = mean(utilized) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(utilization_rate))

print("\nUtilization rate by stakeholder:")
print(stakeholder_stats)

# Power analysis
library(pwr)
# For detecting 5% difference from observed rate
p0 <- utilization_rate
p1 <- p0 + 0.05
effect_size <- ES.h(p1, p0)

power_calc <- pwr.p.test(h = effect_size, sig.level = 0.05, power = 0.80)
print(paste("\nSample size for detecting 5% difference (80% power):", 
           ceiling(power_calc$n)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Government incentive program utilization rate:", 
           round(utilization_rate * 100, 1), "%"))
print(paste("95% Confidence Interval: [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))
print(paste("Based on", n_total, "eligible entities"))
print(paste("Number utilizing programs:", n_utilized))
print(paste("Number not utilizing:", n_not_utilized))
print("The confidence interval suggests the true population utilization rate")
print(paste("is between", round(ci_wilson_lower, 1), "% and", 
           round(ci_wilson_upper, 1), "%"))

# Expected: utilization rate (43.7%, 95% CI [39.2%, 48.2%])