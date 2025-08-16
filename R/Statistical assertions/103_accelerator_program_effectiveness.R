# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 103: "Accelerator program effectiveness: graduation rate (71.2%, 95% CI [67.3%, 75.1%])"
# Purpose: Analyze accelerator program graduation rates and effectiveness

library(tidyverse)
library(janitor)
library(DescTools)
library(PropCIs)
library(binom)
library(epitools)
library(pwr)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_GRADUATION_STATUS <- "accelerator_graduation_status"
COL_PROGRAM_TYPE <- "accelerator_program_type"
COL_COHORT_SIZE <- "cohort_size"
COL_PROGRAM_DURATION <- "program_duration_weeks"
COL_MENTORSHIP_HOURS <- "mentorship_hours_total"
COL_FUNDING_PROVIDED <- "funding_amount_provided"
COL_EQUITY_TAKEN <- "equity_percentage_taken"
COL_DEMO_DAY_PARTICIPATION <- "demo_day_participated"
COL_POST_PROGRAM_FUNDING <- "post_program_funding_raised"
COL_SURVIVAL_12_MONTHS <- "company_survived_12_months"
COL_INDUSTRY_VERTICAL <- "industry_vertical"
COL_FOUNDER_EXPERIENCE <- "founder_experience_level"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    accelerator_graduation_status = as.numeric(accelerator_graduation_status),
    accelerator_program_type = factor(accelerator_program_type),
    cohort_size = as.numeric(cohort_size),
    program_duration_weeks = as.numeric(program_duration_weeks),
    mentorship_hours_total = as.numeric(mentorship_hours_total),
    funding_amount_provided = as.numeric(funding_amount_provided),
    equity_percentage_taken = as.numeric(equity_percentage_taken),
    demo_day_participated = factor(demo_day_participated),
    post_program_funding_raised = as.numeric(post_program_funding_raised),
    company_survived_12_months = factor(company_survived_12_months),
    industry_vertical = factor(industry_vertical),
    founder_experience_level = factor(founder_experience_level,
                                     levels = c("First-time", "Serial", "Experienced"),
                                     ordered = TRUE),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(accelerator_graduation_status))

# Convert to binary (1 = graduated, 0 = did not graduate)
df$graduated <- ifelse(df$accelerator_graduation_status >= 1, 1, 0)

# Primary graduation rate calculation
n_total <- nrow(df)
n_graduated <- sum(df$graduated)
n_not_graduated <- n_total - n_graduated
graduation_rate <- n_graduated / n_total

print("Accelerator Program Graduation Rate Analysis:")
print(paste("Total participants:", n_total))
print(paste("Graduated:", n_graduated))
print(paste("Did not graduate:", n_not_graduated))
print(paste("Graduation rate:", round(graduation_rate * 100, 1), "%"))

# Multiple confidence interval methods
print("\n=== CONFIDENCE INTERVALS ===")

# 1. Exact binomial (Clopper-Pearson)
binom_ci <- binom.test(n_graduated, n_total, conf.level = 0.95)
ci_exact_lower <- binom_ci$conf.int[1] * 100
ci_exact_upper <- binom_ci$conf.int[2] * 100

print(paste("95% CI (Exact/Clopper-Pearson): [", round(ci_exact_lower, 1), "%,", 
           round(ci_exact_upper, 1), "%]"))

# 2. Wilson score interval
wilson_ci <- prop.test(n_graduated, n_total, correct = FALSE)
ci_wilson_lower <- wilson_ci$conf.int[1] * 100
ci_wilson_upper <- wilson_ci$conf.int[2] * 100

print(paste("95% CI (Wilson): [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))

# 3. Agresti-Coull interval
ac_ci <- binom.confint(n_graduated, n_total, method = "ac")
print(paste("95% CI (Agresti-Coull): [", round(ac_ci$lower * 100, 1), "%,", 
           round(ac_ci$upper * 100, 1), "%]"))

# 4. Jeffreys interval
jeffreys_ci <- binom.confint(n_graduated, n_total, method = "bayes")
print(paste("95% CI (Jeffreys/Bayesian): [", round(jeffreys_ci$lower * 100, 1), "%,", 
           round(jeffreys_ci$upper * 100, 1), "%]"))

# 5. Normal approximation
se_proportion <- sqrt(graduation_rate * (1 - graduation_rate) / n_total)
ci_normal_lower <- (graduation_rate - 1.96 * se_proportion) * 100
ci_normal_upper <- (graduation_rate + 1.96 * se_proportion) * 100

print(paste("95% CI (Normal approximation): [", round(ci_normal_lower, 1), "%,", 
           round(ci_normal_upper, 1), "%]"))
print(paste("Standard error:", round(se_proportion * 100, 2), "%"))

# Bootstrap confidence interval
set.seed(123)
n_boot <- 10000
boot_rates <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$graduated, size = n_total, replace = TRUE)
  boot_rates[i] <- mean(boot_sample)
}

boot_ci <- quantile(boot_rates, c(0.025, 0.975)) * 100

print(paste("\n95% CI (Bootstrap): [", round(boot_ci[1], 1), "%,", 
           round(boot_ci[2], 1), "%]"))

# Graduation rate by program type
if(!all(is.na(df$accelerator_program_type))) {
  program_stats <- df %>%
    filter(!is.na(accelerator_program_type)) %>%
    group_by(accelerator_program_type) %>%
    summarise(
      n = n(),
      graduates = sum(graduated),
      graduation_rate = mean(graduated) * 100,
      ci_lower = ifelse(n >= 10, 
                       binom.test(graduates, n)$conf.int[1] * 100, NA),
      ci_upper = ifelse(n >= 10, 
                       binom.test(graduates, n)$conf.int[2] * 100, NA),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(graduation_rate))
  
  print("\nGraduation rates by program type:")
  print(program_stats)
  
  # Chi-square test
  if(nrow(program_stats) > 1) {
    contingency <- table(df$accelerator_program_type, df$graduated)
    chi_test <- chisq.test(contingency)
    
    print("\nChi-square test (program type × graduation):")
    print(paste("χ² =", round(chi_test$statistic, 2)))
    print(paste("p-value:", format(chi_test$p.value, scientific = TRUE)))
  }
}

# Cohort size analysis
if(sum(!is.na(df$cohort_size)) > 50) {
  # Categorize cohort size
  df <- df %>%
    mutate(cohort_category = case_when(
      cohort_size <= 10 ~ "Small (≤10)",
      cohort_size <= 20 ~ "Medium (11-20)",
      cohort_size <= 30 ~ "Large (21-30)",
      TRUE ~ "Very Large (>30)"
    ))
  
  cohort_stats <- df %>%
    filter(!is.na(cohort_category)) %>%
    group_by(cohort_category) %>%
    summarise(
      n = n(),
      graduation_rate = mean(graduated) * 100,
      .groups = "drop"
    )
  
  print("\nGraduation rate by cohort size:")
  print(cohort_stats)
  
  # Logistic regression with cohort size
  logit_cohort <- glm(graduated ~ cohort_size, data = df, family = binomial)
  print("\nLogistic regression (cohort size):")
  print(summary(logit_cohort))
}

# Program duration analysis
if(sum(!is.na(df$program_duration_weeks)) > 50) {
  duration_comparison <- df %>%
    group_by(graduated) %>%
    summarise(
      n = n(),
      mean_duration = mean(program_duration_weeks, na.rm = TRUE),
      sd_duration = sd(program_duration_weeks, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nProgram duration by graduation status:")
  print(duration_comparison)
  
  # t-test
  t_test_duration <- t.test(program_duration_weeks ~ graduated, data = df)
  print(paste("t-test: t =", round(t_test_duration$statistic, 2)))
  print(paste("p-value:", format(t_test_duration$p.value, scientific = TRUE)))
}

# Mentorship hours analysis
if(sum(!is.na(df$mentorship_hours_total)) > 50) {
  mentorship_comparison <- df %>%
    group_by(graduated) %>%
    summarise(
      n = n(),
      mean_mentorship = mean(mentorship_hours_total, na.rm = TRUE),
      median_mentorship = median(mentorship_hours_total, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nMentorship hours by graduation status:")
  print(mentorship_comparison)
  
  # Logistic regression
  logit_mentorship <- glm(graduated ~ mentorship_hours_total, 
                          data = df, family = binomial)
  print("\nLogistic regression (mentorship hours):")
  print(summary(logit_mentorship))
  
  # Odds ratio
  or_mentorship <- exp(coef(logit_mentorship)[2])
  print(paste("Odds ratio per hour:", round(or_mentorship, 4)))
}

# Funding provided analysis
if(sum(!is.na(df$funding_amount_provided)) > 50) {
  # Log transform funding
  df$log_funding <- log10(df$funding_amount_provided + 1)
  
  funding_comparison <- df %>%
    group_by(graduated) %>%
    summarise(
      n = n(),
      mean_funding = mean(funding_amount_provided, na.rm = TRUE),
      median_funding = median(funding_amount_provided, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nFunding provided by graduation status:")
  print(funding_comparison)
}

# Demo day participation
if(!all(is.na(df$demo_day_participated))) {
  demo_day_table <- table(df$demo_day_participated, df$graduated)
  
  print("\nGraduation by demo day participation:")
  print(prop.table(demo_day_table, 1))
  
  # Odds ratio
  if(nrow(demo_day_table) == 2 && ncol(demo_day_table) == 2) {
    or_demo <- (demo_day_table[2,2] * demo_day_table[1,1]) / 
               (demo_day_table[2,1] * demo_day_table[1,2])
    print(paste("Odds ratio (demo day):", round(or_demo, 2)))
  }
}

# Post-program success metrics
if(!all(is.na(df$company_survived_12_months))) {
  survival_by_graduation <- table(df$graduated, df$company_survived_12_months)
  
  print("\n12-month survival by graduation status:")
  print(prop.table(survival_by_graduation, 1))
  
  # Chi-square test
  chi_survival <- chisq.test(survival_by_graduation)
  print(paste("χ² =", round(chi_survival$statistic, 2)))
  print(paste("p-value:", format(chi_survival$p.value, scientific = TRUE)))
}

# Founder experience analysis
if(!all(is.na(df$founder_experience_level))) {
  experience_stats <- df %>%
    filter(!is.na(founder_experience_level)) %>%
    group_by(founder_experience_level) %>%
    summarise(
      n = n(),
      graduation_rate = mean(graduated) * 100,
      .groups = "drop"
    )
  
  print("\nGraduation rate by founder experience:")
  print(experience_stats)
  
  # Cochran-Armitage trend test
  if(nrow(experience_stats) > 2) {
    print("\nCochran-Armitage test for trend would be appropriate here")
  }
}

# Industry vertical analysis
if(length(unique(df$industry_vertical)) > 3) {
  industry_stats <- df %>%
    filter(!is.na(industry_vertical)) %>%
    group_by(industry_vertical) %>%
    summarise(
      n = n(),
      graduation_rate = mean(graduated) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(graduation_rate))
  
  print("\nTop industries by graduation rate:")
  print(head(industry_stats, 10))
}

# Multiple logistic regression
predictors <- c("cohort_size", "program_duration_weeks", "mentorship_hours_total",
               "log_funding")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("graduated ~", paste(available_predictors, collapse = " + "))
  
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
  null_model <- glm(graduated ~ 1, data = df, family = binomial)
  mcfadden_r2 <- 1 - (logLik(logit_full) / logLik(null_model))
  print(paste("McFadden's R²:", round(mcfadden_r2, 3)))
}

# Temporal analysis (if cohort data available)
if("cohort_year" %in% names(df)) {
  yearly_rates <- df %>%
    filter(!is.na(cohort_year)) %>%
    group_by(cohort_year) %>%
    summarise(
      n = n(),
      graduation_rate = mean(graduated) * 100,
      .groups = "drop"
    ) %>%
    arrange(cohort_year)
  
  print("\nGraduation rates by year:")
  print(yearly_rates)
  
  # Trend test
  cor_trend <- cor.test(yearly_rates$cohort_year, yearly_rates$graduation_rate)
  print(paste("Trend correlation: r =", round(cor_trend$estimate, 3)))
}

# Power analysis
# For detecting 5% difference from observed rate
p0 <- graduation_rate
p1 <- p0 + 0.05
effect_size <- ES.h(p1, p0)

power_calc <- pwr.p.test(h = effect_size, sig.level = 0.05, power = 0.80)
print(paste("\nSample size for detecting 5% difference (80% power):", 
           ceiling(power_calc$n)))

# Comparison to benchmark
benchmark_rate <- 0.70  # 70% benchmark
test_benchmark <- binom.test(n_graduated, n_total, p = benchmark_rate)

print(paste("\nTest against 70% benchmark:"))
print(paste("p-value:", format(test_benchmark$p.value, scientific = TRUE)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Accelerator program graduation rate:", round(graduation_rate * 100, 1), "%"))
print(paste("95% Confidence Interval: [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))
print(paste("Based on", n_total, "program participants"))
print(paste("Number graduated:", n_graduated))
print(paste("Number not graduated:", n_not_graduated))
print("The confidence interval indicates the true population graduation rate")
print(paste("is likely between", round(ci_wilson_lower, 1), "% and", 
           round(ci_wilson_upper, 1), "%"))

# Expected: graduation rate (71.2%, 95% CI [67.3%, 75.1%])