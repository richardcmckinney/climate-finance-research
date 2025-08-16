# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 107: "Grant funding accessibility: application success rate (28.4%, 95% CI [24.7%, 32.1%])"
# Purpose: Analyze grant funding application success rates

library(tidyverse)
library(janitor)
library(DescTools)
library(PropCIs)
library(binom)
library(epitools)
library(pwr)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_GRANT_SUCCESS <- "grant_application_successful"
COL_GRANT_TYPE <- "grant_program_type"
COL_GRANT_AMOUNT <- "grant_amount_requested"
COL_ORGANIZATION_TYPE <- "organization_type"
COL_PREVIOUS_GRANTS <- "previous_grant_count"
COL_APPLICATION_QUALITY <- "application_quality_score"
COL_REVIEWER_FEEDBACK <- "reviewer_feedback_score"
COL_PREPARATION_TIME <- "application_preparation_hours"
COL_EXTERNAL_HELP <- "external_consultant_used"
COL_RESUBMISSION <- "is_resubmission"
COL_COMPETITION_LEVEL <- "competition_level_score"
COL_ALIGNMENT_SCORE <- "program_alignment_score"
COL_INDUSTRY <- "industry_sector"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    grant_application_successful = as.numeric(grant_application_successful),
    grant_program_type = factor(grant_program_type),
    grant_amount_requested = as.numeric(grant_amount_requested),
    organization_type = factor(organization_type),
    previous_grant_count = as.numeric(previous_grant_count),
    application_quality_score = as.numeric(application_quality_score),
    reviewer_feedback_score = as.numeric(reviewer_feedback_score),
    application_preparation_hours = as.numeric(application_preparation_hours),
    external_consultant_used = factor(external_consultant_used),
    is_resubmission = factor(is_resubmission),
    competition_level_score = as.numeric(competition_level_score),
    program_alignment_score = as.numeric(program_alignment_score),
    industry_sector = factor(industry_sector),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(grant_application_successful))

# Convert to binary (1 = successful, 0 = unsuccessful)
df$success <- ifelse(df$grant_application_successful >= 1, 1, 0)

# Primary success rate calculation
n_total <- nrow(df)
n_success <- sum(df$success)
n_failure <- n_total - n_success
success_rate <- n_success / n_total

print("Grant Funding Application Success Rate Analysis:")
print(paste("Total applications:", n_total))
print(paste("Successful applications:", n_success))
print(paste("Unsuccessful applications:", n_failure))
print(paste("Success rate:", round(success_rate * 100, 1), "%"))

# Multiple confidence interval methods
print("\n=== CONFIDENCE INTERVALS ===")

# 1. Wilson score interval (recommended)
wilson_ci <- prop.test(n_success, n_total, correct = FALSE)
ci_wilson_lower <- wilson_ci$conf.int[1] * 100
ci_wilson_upper <- wilson_ci$conf.int[2] * 100

print(paste("95% CI (Wilson): [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))

# 2. Exact binomial (Clopper-Pearson)
binom_ci <- binom.test(n_success, n_total, conf.level = 0.95)
ci_exact_lower <- binom_ci$conf.int[1] * 100
ci_exact_upper <- binom_ci$conf.int[2] * 100

print(paste("95% CI (Exact/Clopper-Pearson): [", round(ci_exact_lower, 1), "%,", 
           round(ci_exact_upper, 1), "%]"))

# 3. Agresti-Coull interval
ac_ci <- binom.confint(n_success, n_total, method = "ac")
print(paste("95% CI (Agresti-Coull): [", round(ac_ci$lower * 100, 1), "%,", 
           round(ac_ci$upper * 100, 1), "%]"))

# 4. Jeffreys interval
jeffreys_ci <- binom.confint(n_success, n_total, method = "bayes")
print(paste("95% CI (Jeffreys/Bayesian): [", round(jeffreys_ci$lower * 100, 1), "%,", 
           round(jeffreys_ci$upper * 100, 1), "%]"))

# Standard error
se_proportion <- sqrt(success_rate * (1 - success_rate) / n_total)
print(paste("\nStandard error:", round(se_proportion * 100, 2), "%"))

# Bootstrap confidence interval
set.seed(123)
n_boot <- 10000
boot_rates <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$success, size = n_total, replace = TRUE)
  boot_rates[i] <- mean(boot_sample)
}

boot_ci <- quantile(boot_rates, c(0.025, 0.975)) * 100

print(paste("95% CI (Bootstrap): [", round(boot_ci[1], 1), "%,", 
           round(boot_ci[2], 1), "%]"))

# Success rate by grant type
if(!all(is.na(df$grant_program_type))) {
  type_stats <- df %>%
    filter(!is.na(grant_program_type)) %>%
    group_by(grant_program_type) %>%
    summarise(
      n = n(),
      successes = sum(success),
      success_rate = mean(success) * 100,
      ci_lower = ifelse(n >= 10, 
                       binom.test(successes, n)$conf.int[1] * 100, NA),
      ci_upper = ifelse(n >= 10, 
                       binom.test(successes, n)$conf.int[2] * 100, NA),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(success_rate))
  
  print("\nSuccess rates by grant program type:")
  print(type_stats)
  
  # Chi-square test
  if(nrow(type_stats) > 1) {
    contingency <- table(df$grant_program_type, df$success)
    chi_test <- chisq.test(contingency)
    
    print("\nChi-square test (type × success):")
    print(paste("χ² =", round(chi_test$statistic, 2)))
    print(paste("p-value:", format(chi_test$p.value, scientific = TRUE)))
  }
}

# Grant amount analysis
if(sum(!is.na(df$grant_amount_requested)) > 50) {
  # Log transform grant amount
  df$log_amount <- log10(df$grant_amount_requested + 1)
  
  amount_comparison <- df %>%
    group_by(success) %>%
    summarise(
      n = n(),
      mean_amount = mean(grant_amount_requested, na.rm = TRUE),
      median_amount = median(grant_amount_requested, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nGrant amount by success status:")
  print(amount_comparison)
  
  # Logistic regression
  logit_amount <- glm(success ~ log_amount, data = df, family = binomial)
  print("\nLogistic regression (log amount):")
  print(summary(logit_amount))
}

# Previous grants analysis
if(sum(!is.na(df$previous_grant_count)) > 50) {
  # Categorize previous grants
  df <- df %>%
    mutate(experience_category = case_when(
      previous_grant_count == 0 ~ "First-time",
      previous_grant_count <= 2 ~ "Some experience (1-2)",
      previous_grant_count <= 5 ~ "Experienced (3-5)",
      TRUE ~ "Very experienced (6+)"
    ))
  
  experience_stats <- df %>%
    filter(!is.na(experience_category)) %>%
    group_by(experience_category) %>%
    summarise(
      n = n(),
      success_rate = mean(success) * 100,
      .groups = "drop"
    )
  
  print("\nSuccess rate by grant experience:")
  print(experience_stats)
  
  # Logistic regression
  logit_experience <- glm(success ~ previous_grant_count, data = df, family = binomial)
  print("\nLogistic regression (previous grants):")
  print(summary(logit_experience))
  
  # Odds ratio
  or_experience <- exp(coef(logit_experience)[2])
  print(paste("Odds ratio per additional grant:", round(or_experience, 3)))
}

# Application quality analysis
if(sum(!is.na(df$application_quality_score)) > 50) {
  quality_comparison <- df %>%
    group_by(success) %>%
    summarise(
      n = n(),
      mean_quality = mean(application_quality_score, na.rm = TRUE),
      sd_quality = sd(application_quality_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nApplication quality by success status:")
  print(quality_comparison)
  
  # Logistic regression
  logit_quality <- glm(success ~ application_quality_score, data = df, family = binomial)
  print("\nLogistic regression (application quality):")
  print(summary(logit_quality))
}

# Preparation time analysis
if(sum(!is.na(df$application_preparation_hours)) > 50) {
  prep_comparison <- df %>%
    group_by(success) %>%
    summarise(
      n = n(),
      mean_hours = mean(application_preparation_hours, na.rm = TRUE),
      median_hours = median(application_preparation_hours, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nPreparation time by success status:")
  print(prep_comparison)
  
  # T-test
  t_test_prep <- t.test(application_preparation_hours ~ success, data = df)
  print(paste("t-test: t =", round(t_test_prep$statistic, 2)))
  print(paste("p-value:", format(t_test_prep$p.value, scientific = TRUE)))
}

# External consultant analysis
if(!all(is.na(df$external_consultant_used))) {
  consultant_table <- table(df$external_consultant_used, df$success)
  
  print("\nSuccess rate by consultant use:")
  print(prop.table(consultant_table, 1))
  
  # Odds ratio
  if(nrow(consultant_table) == 2 && ncol(consultant_table) == 2) {
    or_consultant <- (consultant_table[2,2] * consultant_table[1,1]) / 
                    (consultant_table[2,1] * consultant_table[1,2])
    print(paste("Odds ratio (consultant use):", round(or_consultant, 2)))
    
    # Chi-square test
    chi_consultant <- chisq.test(consultant_table)
    print(paste("χ² =", round(chi_consultant$statistic, 2)))
    print(paste("p-value:", format(chi_consultant$p.value, scientific = TRUE)))
  }
}

# Resubmission analysis
if(!all(is.na(df$is_resubmission))) {
  resubmission_stats <- df %>%
    filter(!is.na(is_resubmission)) %>%
    group_by(is_resubmission) %>%
    summarise(
      n = n(),
      success_rate = mean(success) * 100,
      .groups = "drop"
    )
  
  print("\nSuccess rate by submission status:")
  print(resubmission_stats)
}

# Competition level analysis
if(sum(!is.na(df$competition_level_score)) > 50) {
  cor_competition <- cor.test(df$success, df$competition_level_score,
                             use = "complete.obs")
  
  print("\nCorrelation with competition level:")
  print(paste("r =", round(cor_competition$estimate, 3)))
  print(paste("p-value:", format(cor_competition$p.value, scientific = TRUE)))
}

# Program alignment analysis
if(sum(!is.na(df$program_alignment_score)) > 50) {
  alignment_comparison <- df %>%
    group_by(success) %>%
    summarise(
      n = n(),
      mean_alignment = mean(program_alignment_score, na.rm = TRUE),
      sd_alignment = sd(program_alignment_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nProgram alignment by success status:")
  print(alignment_comparison)
  
  # Logistic regression
  logit_alignment <- glm(success ~ program_alignment_score, data = df, family = binomial)
  print("\nLogistic regression (program alignment):")
  print(summary(logit_alignment))
}

# Multiple logistic regression
predictors <- c("application_quality_score", "previous_grant_count",
               "program_alignment_score", "application_preparation_hours")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("success ~", paste(available_predictors, collapse = " + "))
  
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
  null_model <- glm(success ~ 1, data = df, family = binomial)
  mcfadden_r2 <- 1 - (logLik(logit_full) / logLik(null_model))
  print(paste("McFadden's R²:", round(mcfadden_r2, 3)))
}

# Organization type analysis
if(!all(is.na(df$organization_type))) {
  org_stats <- df %>%
    filter(!is.na(organization_type)) %>%
    group_by(organization_type) %>%
    summarise(
      n = n(),
      success_rate = mean(success) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(success_rate))
  
  print("\nSuccess rate by organization type:")
  print(org_stats)
}

# Industry analysis
if(length(unique(df$industry_sector)) > 3) {
  industry_stats <- df %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      success_rate = mean(success) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(success_rate))
  
  print("\nTop industries by success rate:")
  print(head(industry_stats, 10))
}

# Power analysis
# For detecting 5% difference from observed rate
p0 <- success_rate
p1 <- p0 + 0.05
effect_size <- ES.h(p1, p0)

power_calc <- pwr.p.test(h = effect_size, sig.level = 0.05, power = 0.80)
print(paste("\nSample size for detecting 5% difference (80% power):", 
           ceiling(power_calc$n)))

# Comparison to benchmark
benchmark_rate <- 0.25  # 25% benchmark
test_benchmark <- binom.test(n_success, n_total, p = benchmark_rate)

print(paste("\nTest against 25% benchmark:"))
print(paste("p-value:", format(test_benchmark$p.value, scientific = TRUE)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Grant funding application success rate:", round(success_rate * 100, 1), "%"))
print(paste("95% Confidence Interval: [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))
print(paste("Based on", n_total, "grant applications"))
print(paste("Successful applications:", n_success))
print(paste("Unsuccessful applications:", n_failure))
print("The confidence interval indicates the true population success rate")
print(paste("is between", round(ci_wilson_lower, 1), "% and", 
           round(ci_wilson_upper, 1), "%"))

# Expected: application success rate (28.4%, 95% CI [24.7%, 32.1%])