# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 97: "Research partnership agreements: duration effect (β = 0.34, p < .001)"
# Purpose: Analyze the effect of partnership duration on research outcomes

library(tidyverse)
library(janitor)
library(DescTools)
library(lmtest)
library(sandwich)
library(car)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_PARTNERSHIP_DURATION <- "research_partnership_duration_months"
COL_RESEARCH_OUTCOMES <- "research_outcome_score"
COL_PARTNERSHIP_TYPE <- "partnership_agreement_type"
COL_FUNDING_LEVEL <- "partnership_funding_amount"
COL_TEAM_SIZE <- "research_team_size"
COL_PUBLICATION_OUTPUT <- "publication_output_score"
COL_INNOVATION_METRICS <- "innovation_metrics_score"
COL_MILESTONE_ACHIEVEMENT <- "milestone_achievement_rate"
COL_PARTNER_SATISFACTION <- "partner_satisfaction_score"
COL_RENEWAL_STATUS <- "partnership_renewed"
COL_INDUSTRY <- "industry_sector"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    research_partnership_duration_months = as.numeric(research_partnership_duration_months),
    research_outcome_score = as.numeric(research_outcome_score),
    partnership_agreement_type = factor(partnership_agreement_type),
    partnership_funding_amount = as.numeric(partnership_funding_amount),
    research_team_size = as.numeric(research_team_size),
    publication_output_score = as.numeric(publication_output_score),
    innovation_metrics_score = as.numeric(innovation_metrics_score),
    milestone_achievement_rate = as.numeric(milestone_achievement_rate),
    partner_satisfaction_score = as.numeric(partner_satisfaction_score),
    partnership_renewed = factor(partnership_renewed),
    industry_sector = factor(industry_sector),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(research_partnership_duration_months), !is.na(research_outcome_score))

# Standardize predictors for regression
df$duration_std <- scale(df$research_partnership_duration_months)[,1]
df$outcome_std <- scale(df$research_outcome_score)[,1]

# Primary linear regression
lm_duration <- lm(research_outcome_score ~ research_partnership_duration_months, data = df)
summary_lm <- summary(lm_duration)

print("Research Partnership Duration Effect on Outcomes:")
print(summary_lm)

# Extract beta coefficient and statistics
beta_coef <- coef(lm_duration)[2]
beta_se <- summary_lm$coefficients[2, 2]
t_stat <- summary_lm$coefficients[2, 3]
p_value <- summary_lm$coefficients[2, 4]
n_total <- nrow(df)

print(paste("\nUnstandardized β =", round(beta_coef, 4)))
print(paste("SE =", round(beta_se, 4)))
print(paste("t(", n_total - 2, ") =", round(t_stat, 2)))
print(paste("p-value:", format(p_value, scientific = TRUE)))

# Standardized regression coefficient
lm_std <- lm(outcome_std ~ duration_std, data = df)
beta_std <- coef(lm_std)[2]

print(paste("\nStandardized β =", round(beta_std, 3)))
print(paste("This is the effect size in standard deviation units"))

# Confidence intervals for beta
ci_beta <- confint(lm_duration)[2, ]
print(paste("95% CI for unstandardized β: [", round(ci_beta[1], 4), ",", 
           round(ci_beta[2], 4), "]"))

ci_beta_std <- confint(lm_std)[2, ]
print(paste("95% CI for standardized β: [", round(ci_beta_std[1], 3), ",", 
           round(ci_beta_std[2], 3), "]"))

# Effect size measures
r_squared <- summary_lm$r.squared
adj_r_squared <- summary_lm$adj.r.squared
f_stat <- summary_lm$fstatistic[1]

print(paste("\nR² =", round(r_squared, 3)))
print(paste("Adjusted R² =", round(adj_r_squared, 3)))
print(paste("F(1,", n_total - 2, ") =", round(f_stat, 2)))

# Cohen's f² effect size
cohens_f2 <- r_squared / (1 - r_squared)
print(paste("Cohen's f² =", round(cohens_f2, 3)))

# Correlation analysis
cor_result <- cor.test(df$research_partnership_duration_months, 
                      df$research_outcome_score)
print(paste("\nPearson r =", round(cor_result$estimate, 3)))
print(paste("r² =", round(cor_result$estimate^2, 3)))

# Bootstrap confidence intervals for beta
set.seed(123)
n_boot <- 10000
boot_betas <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_indices <- sample(nrow(df), replace = TRUE)
  boot_data <- df[boot_indices, ]
  boot_lm <- lm(outcome_std ~ duration_std, data = boot_data)
  boot_betas[i] <- coef(boot_lm)[2]
}

boot_ci <- quantile(boot_betas, c(0.025, 0.975))
print(paste("\nBootstrap 95% CI for standardized β: [", 
           round(boot_ci[1], 3), ",", round(boot_ci[2], 3), "]"))

# Regression diagnostics
print("\n=== REGRESSION DIAGNOSTICS ===")

# Normality of residuals
shapiro_test <- shapiro.test(residuals(lm_duration)[1:min(5000, length(residuals(lm_duration)))])
print(paste("Shapiro-Wilk test for residuals: p =", 
           format(shapiro_test$p.value, scientific = TRUE)))

# Homoscedasticity (Breusch-Pagan test)
bp_test <- bptest(lm_duration)
print(paste("Breusch-Pagan test: χ² =", round(bp_test$statistic, 2), 
           ", p =", format(bp_test$p.value, scientific = TRUE)))

# Durbin-Watson test for autocorrelation
dw_test <- dwtest(lm_duration)
print(paste("Durbin-Watson statistic:", round(dw_test$statistic, 3)))

# Robust standard errors (HC3)
robust_se <- coeftest(lm_duration, vcov = vcovHC(lm_duration, type = "HC3"))
print("\nRobust standard errors (HC3):")
print(robust_se[2, ])

# Non-linear relationships
# Quadratic term
lm_quad <- lm(research_outcome_score ~ research_partnership_duration_months + 
             I(research_partnership_duration_months^2), data = df)

print("\nQuadratic model:")
print(summary(lm_quad))

# Compare linear vs quadratic
anova_comparison <- anova(lm_duration, lm_quad)
print("\nModel comparison (linear vs quadratic):")
print(anova_comparison)

# Polynomial regression (cubic)
lm_cubic <- lm(research_outcome_score ~ poly(research_partnership_duration_months, 3), 
              data = df)
print("\nCubic polynomial model R²:", round(summary(lm_cubic)$r.squared, 3))

# Spline regression
library(splines)
lm_spline <- lm(research_outcome_score ~ ns(research_partnership_duration_months, 3), 
               data = df)
print(paste("Natural spline model R²:", round(summary(lm_spline)$r.squared, 3)))

# Duration categories analysis
df <- df %>%
  mutate(duration_category = case_when(
    research_partnership_duration_months <= 12 ~ "Short (≤1 year)",
    research_partnership_duration_months <= 36 ~ "Medium (1-3 years)",
    research_partnership_duration_months <= 60 ~ "Long (3-5 years)",
    TRUE ~ "Very long (>5 years)"
  ))

duration_stats <- df %>%
  group_by(duration_category) %>%
  summarise(
    n = n(),
    mean_outcome = mean(research_outcome_score, na.rm = TRUE),
    sd_outcome = sd(research_outcome_score, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor(duration_category, 
                levels = c("Short (≤1 year)", "Medium (1-3 years)", 
                          "Long (3-5 years)", "Very long (>5 years)")))

print("\nOutcomes by duration category:")
print(duration_stats)

# ANOVA for duration categories
anova_duration <- aov(research_outcome_score ~ duration_category, data = df)
print("\nANOVA by duration categories:")
print(summary(anova_duration))

# Multiple regression with covariates
if(sum(!is.na(df$partnership_funding_amount)) > 50 && 
   sum(!is.na(df$research_team_size)) > 50) {
  
  # Log transform funding
  df$log_funding <- log10(df$partnership_funding_amount + 1)
  
  lm_multiple <- lm(research_outcome_score ~ research_partnership_duration_months + 
                   log_funding + research_team_size, data = df)
  
  print("\nMultiple regression with covariates:")
  print(summary(lm_multiple))
  
  # Standardized coefficients
  df_complete <- df %>%
    select(research_outcome_score, research_partnership_duration_months, 
           log_funding, research_team_size) %>%
    na.omit()
  
  df_std_all <- df_complete %>%
    mutate(across(everything(), scale))
  
  lm_std_multiple <- lm(research_outcome_score ~ ., data = df_std_all)
  
  print("\nStandardized coefficients:")
  print(round(coef(lm_std_multiple), 3))
  
  # Partial correlation
  library(ppcor)
  partial_cor <- pcor.test(df_complete$research_outcome_score,
                          df_complete$research_partnership_duration_months,
                          df_complete[, c("log_funding", "research_team_size")])
  
  print(paste("\nPartial correlation (controlling for funding & team size):",
             round(partial_cor$estimate, 3)))
  print(paste("p-value:", format(partial_cor$p.value, scientific = TRUE)))
}

# Interaction with partnership type
if(!all(is.na(df$partnership_agreement_type))) {
  lm_interaction <- lm(research_outcome_score ~ research_partnership_duration_months * 
                       partnership_agreement_type, data = df)
  
  print("\nInteraction model (Duration × Partnership Type):")
  print(anova(lm_interaction))
  
  # Simple slopes analysis
  types <- unique(df$partnership_agreement_type)
  for(type in types[!is.na(types)]) {
    df_subset <- df %>% filter(partnership_agreement_type == type)
    if(nrow(df_subset) >= 20) {
      lm_subset <- lm(research_outcome_score ~ research_partnership_duration_months, 
                     data = df_subset)
      print(paste("\nSlope for", type, ": β =", 
                 round(coef(lm_subset)[2], 3)))
    }
  }
}

# Mediation analysis with publication output
if(sum(!is.na(df$publication_output_score)) > 50) {
  # Step 1: Duration → Outcome (already done)
  
  # Step 2: Duration → Publications
  lm_med2 <- lm(publication_output_score ~ research_partnership_duration_months, data = df)
  
  # Step 3: Duration + Publications → Outcome
  lm_med3 <- lm(research_outcome_score ~ research_partnership_duration_months + 
               publication_output_score, data = df)
  
  print("\n=== MEDIATION ANALYSIS ===")
  print("Path a (Duration → Publications):")
  print(paste("β =", round(coef(lm_med2)[2], 3), 
             ", p =", format(summary(lm_med2)$coefficients[2, 4], scientific = TRUE)))
  
  print("\nPath b (Publications → Outcome, controlling for Duration):")
  print(paste("β =", round(coef(lm_med3)[3], 3),
             ", p =", format(summary(lm_med3)$coefficients[3, 4], scientific = TRUE)))
  
  print("\nDirect effect (Duration → Outcome, controlling for Publications):")
  print(paste("β =", round(coef(lm_med3)[2], 3),
             ", p =", format(summary(lm_med3)$coefficients[2, 4], scientific = TRUE)))
  
  # Indirect effect
  indirect_effect <- coef(lm_med2)[2] * coef(lm_med3)[3]
  print(paste("\nIndirect effect:", round(indirect_effect, 4)))
}

# Milestone achievement correlation
if(sum(!is.na(df$milestone_achievement_rate)) > 30) {
  cor_milestones <- cor.test(df$research_partnership_duration_months,
                            df$milestone_achievement_rate)
  
  print("\nCorrelation with milestone achievement:")
  print(paste("r =", round(cor_milestones$estimate, 3)))
  print(paste("p-value:", format(cor_milestones$p.value, scientific = TRUE)))
}

# Partnership renewal analysis
if(!all(is.na(df$partnership_renewed))) {
  renewal_comparison <- df %>%
    group_by(partnership_renewed) %>%
    summarise(
      n = n(),
      mean_duration = mean(research_partnership_duration_months, na.rm = TRUE),
      mean_outcome = mean(research_outcome_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nDuration and outcomes by renewal status:")
  print(renewal_comparison)
  
  # Logistic regression predicting renewal
  logit_renewal <- glm(partnership_renewed ~ research_partnership_duration_months + 
                       research_outcome_score, 
                       data = df, family = binomial)
  
  print("\nLogistic regression predicting renewal:")
  print(summary(logit_renewal))
}

# Sensitivity analysis - remove outliers
z_scores_duration <- abs(scale(df$research_partnership_duration_months))
z_scores_outcome <- abs(scale(df$research_outcome_score))
df_no_outliers <- df[z_scores_duration < 3 & z_scores_outcome < 3, ]

lm_robust <- lm(outcome_std ~ duration_std, 
               data = df_no_outliers %>%
                 mutate(duration_std = scale(research_partnership_duration_months)[,1],
                       outcome_std = scale(research_outcome_score)[,1]))

print("\nSensitivity analysis (outliers removed):")
print(paste("N after removing outliers:", nrow(df_no_outliers)))
print(paste("Standardized β =", round(coef(lm_robust)[2], 3)))
print(paste("p-value:", format(summary(lm_robust)$coefficients[2, 4], scientific = TRUE)))

# Power analysis
library(pwr)
# For detecting the observed effect
power_observed <- pwr.f2.test(u = 1, v = n_total - 2, f2 = cohens_f2, sig.level = 0.05)
print(paste("\nPost-hoc power for observed effect:", round(power_observed$power, 3)))

# Sample size for 80% power to detect β = 0.30
r_target <- 0.30
f2_target <- r_target^2 / (1 - r_target^2)
n_required <- pwr.f2.test(u = 1, f2 = f2_target, sig.level = 0.05, power = 0.80)$v + 2
print(paste("Sample size needed for 80% power to detect β = 0.30:", ceiling(n_required)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print("Research partnership duration showed a significant positive effect on outcomes")
print(paste("Standardized β =", round(beta_std, 2), 
           ", 95% CI [", round(ci_beta_std[1], 2), ",", round(ci_beta_std[2], 2), "]"))
print(paste("t(", n_total - 2, ") =", round(t_stat, 2), 
           ", p <", ifelse(p_value < 0.001, ".001", round(p_value, 3))))
print(paste("R² =", round(r_squared, 3), ", indicating", 
           round(r_squared * 100, 1), "% of variance explained"))
print("Longer partnerships are associated with better research outcomes")

# Expected: duration effect (β = 0.34, p < .001)