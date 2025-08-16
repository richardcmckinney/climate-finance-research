# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 109: "Public private partnerships: success factors (R² = 0.51, p < .001)"
# Purpose: Analyze success factors in public-private partnerships

library(tidyverse)
library(janitor)
library(DescTools)
library(car)
library(leaps)
library(caret)
library(lavaan)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_PPP_SUCCESS <- "ppp_success_score"
COL_STAKEHOLDER_ALIGNMENT <- "stakeholder_alignment_score"
COL_RISK_SHARING <- "risk_sharing_effectiveness"
COL_CONTRACT_CLARITY <- "contract_clarity_score"
COL_GOVERNANCE_STRUCTURE <- "governance_quality_score"
COL_FINANCIAL_VIABILITY <- "financial_viability_score"
COL_POLITICAL_SUPPORT <- "political_support_level"
COL_PUBLIC_ACCEPTANCE <- "public_acceptance_score"
COL_TECHNICAL_CAPABILITY <- "technical_capability_score"
COL_REGULATORY_FRAMEWORK <- "regulatory_framework_quality"
COL_TRANSPARENCY_LEVEL <- "transparency_score"
COL_PERFORMANCE_MONITORING <- "performance_monitoring_score"
COL_DISPUTE_RESOLUTION <- "dispute_resolution_effectiveness"
COL_VALUE_FOR_MONEY <- "value_for_money_score"
COL_PPP_TYPE <- "ppp_model_type"
COL_SECTOR <- "ppp_sector"
COL_PROJECT_SIZE <- "project_size_category"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    ppp_success_score = as.numeric(ppp_success_score),
    stakeholder_alignment_score = as.numeric(stakeholder_alignment_score),
    risk_sharing_effectiveness = as.numeric(risk_sharing_effectiveness),
    contract_clarity_score = as.numeric(contract_clarity_score),
    governance_quality_score = as.numeric(governance_quality_score),
    financial_viability_score = as.numeric(financial_viability_score),
    political_support_level = as.numeric(political_support_level),
    public_acceptance_score = as.numeric(public_acceptance_score),
    technical_capability_score = as.numeric(technical_capability_score),
    regulatory_framework_quality = as.numeric(regulatory_framework_quality),
    transparency_score = as.numeric(transparency_score),
    performance_monitoring_score = as.numeric(performance_monitoring_score),
    dispute_resolution_effectiveness = as.numeric(dispute_resolution_effectiveness),
    value_for_money_score = as.numeric(value_for_money_score),
    ppp_model_type = factor(ppp_model_type),
    ppp_sector = factor(ppp_sector),
    project_size_category = factor(project_size_category,
                                  levels = c("Small", "Medium", "Large", "Mega"),
                                  ordered = TRUE),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(ppp_success_score))

# Identify available predictors
predictors <- c("stakeholder_alignment_score", "risk_sharing_effectiveness",
               "contract_clarity_score", "governance_quality_score",
               "financial_viability_score", "political_support_level",
               "public_acceptance_score", "technical_capability_score",
               "regulatory_framework_quality", "transparency_score",
               "performance_monitoring_score", "dispute_resolution_effectiveness",
               "value_for_money_score")

available_predictors <- predictors[predictors %in% names(df)]
available_predictors <- available_predictors[sapply(df[available_predictors], 
                                                   function(x) sum(!is.na(x)) > 30)]

print(paste("Available predictors:", length(available_predictors)))
print(available_predictors)

# Create complete cases dataset
df_complete <- df %>%
  select(ppp_success_score, all_of(available_predictors)) %>%
  na.omit()

n_total <- nrow(df_complete)
n_predictors <- length(available_predictors)

print(paste("\nComplete cases:", n_total))
print(paste("Number of predictors:", n_predictors))
print(paste("Cases per predictor:", round(n_total / n_predictors, 1)))

# Full multiple regression model
formula_full <- paste("ppp_success_score ~", 
                     paste(available_predictors, collapse = " + "))
lm_full <- lm(as.formula(formula_full), data = df_complete)

print("\n=== FULL REGRESSION MODEL ===")
print(summary(lm_full))

# Extract key statistics
r_squared <- summary(lm_full)$r.squared
adj_r_squared <- summary(lm_full)$adj.r.squared
f_stat <- summary(lm_full)$fstatistic[1]
f_df1 <- summary(lm_full)$fstatistic[2]
f_df2 <- summary(lm_full)$fstatistic[3]
p_value <- pf(f_stat, f_df1, f_df2, lower.tail = FALSE)

print(paste("\nR² =", round(r_squared, 3)))
print(paste("Adjusted R² =", round(adj_r_squared, 3)))
print(paste("F(", f_df1, ",", f_df2, ") =", round(f_stat, 2)))
print(paste("p-value:", format(p_value, scientific = TRUE)))

# Cohen's f² effect size
cohens_f2 <- r_squared / (1 - r_squared)
print(paste("Cohen's f² =", round(cohens_f2, 3)))
print(paste("Effect size interpretation:", 
           ifelse(cohens_f2 < 0.15, "small",
                 ifelse(cohens_f2 < 0.35, "medium", "large"))))

# Standardized coefficients
df_std <- df_complete %>%
  mutate(across(everything(), scale))

lm_std <- lm(as.formula(formula_full), data = df_std)
std_coefs <- coef(lm_std)[-1]  # Remove intercept

print("\nStandardized coefficients (beta weights):")
std_coef_df <- data.frame(
  Predictor = names(std_coefs),
  Beta = round(std_coefs, 3),
  p_value = round(summary(lm_std)$coefficients[-1, 4], 4)
) %>%
  arrange(desc(abs(Beta)))

print(std_coef_df)

# Top 5 predictors
top_5_predictors <- std_coef_df %>%
  filter(p_value < 0.05) %>%
  head(5)

print("\n=== TOP 5 SIGNIFICANT PREDICTORS ===")
print(top_5_predictors)

# Model diagnostics
print("\n=== MODEL DIAGNOSTICS ===")

# VIF for multicollinearity
vif_values <- vif(lm_full)
print("Variance Inflation Factors:")
print(round(vif_values, 2))
print(paste("Maximum VIF:", round(max(vif_values), 2)))

if(max(vif_values) > 10) {
  print("WARNING: Severe multicollinearity detected (VIF > 10)")
} else if(max(vif_values) > 5) {
  print("WARNING: Moderate multicollinearity detected (VIF > 5)")
}

# Normality of residuals
shapiro_test <- shapiro.test(residuals(lm_full)[1:min(5000, length(residuals(lm_full)))])
print(paste("\nShapiro-Wilk test for residuals: p =", 
           format(shapiro_test$p.value, scientific = TRUE)))

# Homoscedasticity
library(lmtest)
bp_test <- bptest(lm_full)
print(paste("Breusch-Pagan test: χ² =", round(bp_test$statistic, 2),
           ", p =", format(bp_test$p.value, scientific = TRUE)))

# Durbin-Watson
dw_test <- durbinWatsonTest(lm_full)
print(paste("Durbin-Watson statistic:", round(dw_test, 3)))

# Hierarchical regression analysis
print("\n=== HIERARCHICAL REGRESSION ===")

# Step 1: Core governance factors
core_predictors <- c("stakeholder_alignment_score", "governance_quality_score", 
                    "contract_clarity_score")
core_available <- core_predictors[core_predictors %in% available_predictors]

if(length(core_available) >= 2) {
  formula_step1 <- paste("ppp_success_score ~", 
                        paste(core_available, collapse = " + "))
  lm_step1 <- lm(as.formula(formula_step1), data = df_complete)
  
  print("Step 1 - Core governance factors:")
  print(paste("R² =", round(summary(lm_step1)$r.squared, 3)))
}

# Step 2: Add financial factors
financial_predictors <- c("financial_viability_score", "value_for_money_score",
                         "risk_sharing_effectiveness")
financial_available <- financial_predictors[financial_predictors %in% available_predictors]

if(length(c(core_available, financial_available)) >= 4) {
  formula_step2 <- paste("ppp_success_score ~", 
                        paste(c(core_available, financial_available), collapse = " + "))
  lm_step2 <- lm(as.formula(formula_step2), data = df_complete)
  
  print("\nStep 2 - Add financial factors:")
  print(paste("R² =", round(summary(lm_step2)$r.squared, 3)))
  print(paste("ΔR² =", round(summary(lm_step2)$r.squared - 
                            summary(lm_step1)$r.squared, 3)))
  
  # F-test for R² change
  anova_hier <- anova(lm_step1, lm_step2)
  print("\nF-test for R² change:")
  print(anova_hier)
}

# Step 3: Add stakeholder factors
stakeholder_predictors <- c("political_support_level", "public_acceptance_score")
stakeholder_available <- stakeholder_predictors[stakeholder_predictors %in% available_predictors]

if(length(c(core_available, financial_available, stakeholder_available)) >= 5) {
  formula_step3 <- paste("ppp_success_score ~", 
                        paste(c(core_available, financial_available, 
                               stakeholder_available), collapse = " + "))
  lm_step3 <- lm(as.formula(formula_step3), data = df_complete)
  
  print("\nStep 3 - Add stakeholder factors:")
  print(paste("R² =", round(summary(lm_step3)$r.squared, 3)))
  print(paste("ΔR² =", round(summary(lm_step3)$r.squared - 
                            summary(lm_step2)$r.squared, 3)))
}

# Stepwise selection
print("\n=== STEPWISE SELECTION ===")

# Backward elimination
lm_backward <- step(lm_full, direction = "backward", trace = 0)
print("Backward elimination - Final model:")
print(paste("Variables retained:", 
           paste(names(coef(lm_backward))[-1], collapse = ", ")))
print(paste("R² =", round(summary(lm_backward)$r.squared, 3)))

# Best subsets regression
if(n_predictors <= 15) {
  library(leaps)
  regsubsets_full <- regsubsets(as.formula(formula_full), 
                                data = df_complete, 
                                nvmax = n_predictors,
                                method = "exhaustive")
  reg_summary <- summary(regsubsets_full)
  
  print("\nBest subsets - Adjusted R² by model size:")
  best_adjr2 <- data.frame(
    n_vars = 1:length(reg_summary$adjr2),
    adj_r2 = round(reg_summary$adjr2, 3),
    bic = round(reg_summary$bic, 1)
  )
  print(best_adjr2)
  
  # Best model by BIC
  best_model_bic <- which.min(reg_summary$bic)
  print(paste("\nBest model by BIC:", best_model_bic, "predictors"))
}

# Cross-validation
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(as.formula(formula_full), 
                 data = df_complete,
                 method = "lm",
                 trControl = train_control)

print("\n=== 10-FOLD CROSS-VALIDATION ===")
print(paste("Cross-validated R²:", round(cv_model$results$Rsquared, 3)))
print(paste("Cross-validated RMSE:", round(cv_model$results$RMSE, 3)))
print(paste("Cross-validated MAE:", round(cv_model$results$MAE, 3)))

# Relative importance analysis
if(length(available_predictors) >= 3) {
  library(relaimpo)
  relimp <- calc.relimp(lm_full, type = "lmg")
  
  importance_df <- data.frame(
    Predictor = names(relimp$lmg),
    Relative_Importance = round(relimp$lmg * 100, 1)
  ) %>%
    arrange(desc(Relative_Importance))
  
  print("\nRelative importance (% of R²):")
  print(head(importance_df, 10))
}

# PPP type analysis
if(!all(is.na(df$ppp_model_type))) {
  type_stats <- df %>%
    filter(!is.na(ppp_model_type)) %>%
    group_by(ppp_model_type) %>%
    summarise(
      n = n(),
      mean_success = round(mean(ppp_success_score, na.rm = TRUE), 2),
      sd_success = round(sd(ppp_success_score, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_success))
  
  print("\nSuccess by PPP model type:")
  print(type_stats)
}

# Sector analysis
if(length(unique(df$ppp_sector)) > 3) {
  sector_stats <- df %>%
    filter(!is.na(ppp_sector)) %>%
    group_by(ppp_sector) %>%
    summarise(
      n = n(),
      mean_success = round(mean(ppp_success_score, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(mean_success))
  
  print("\nSuccess by PPP sector:")
  print(head(sector_stats, 10))
}

# Project size analysis
if(!all(is.na(df$project_size_category))) {
  size_stats <- df %>%
    filter(!is.na(project_size_category)) %>%
    group_by(project_size_category) %>%
    summarise(
      n = n(),
      mean_success = round(mean(ppp_success_score, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nSuccess by project size:")
  print(size_stats)
}

# Bootstrap R² confidence interval
set.seed(123)
n_boot <- 1000
boot_r2 <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_indices <- sample(nrow(df_complete), replace = TRUE)
  boot_data <- df_complete[boot_indices, ]
  boot_lm <- lm(as.formula(formula_full), data = boot_data)
  boot_r2[i] <- summary(boot_lm)$r.squared
}

boot_ci_r2 <- quantile(boot_r2, c(0.025, 0.975))

print("\nBootstrap 95% CI for R²:")
print(round(boot_ci_r2, 3))

# Power analysis
library(pwr)
f2 <- cohens_f2
power_observed <- pwr.f2.test(u = n_predictors, 
                              v = n_total - n_predictors - 1, 
                              f2 = f2, 
                              sig.level = 0.05)
print(paste("\nPost-hoc power:", round(power_observed$power, 3)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print("Public-private partnership success can be predicted by multiple factors")
print(paste("Full model: R² =", round(r_squared, 2), 
           ", Adjusted R² =", round(adj_r_squared, 2)))
print(paste("F(", f_df1, ",", f_df2, ") =", round(f_stat, 2),
           ", p <", ifelse(p_value < 0.001, ".001", round(p_value, 3))))
print(paste("The model explains", round(r_squared * 100), 
           "% of variance in PPP success"))
print("\nKey success factors identified (top 3):")
for(i in 1:min(3, nrow(top_5_predictors))) {
  print(paste(i, ".", top_5_predictors$Predictor[i], 
             "(β =", top_5_predictors$Beta[i], ")"))
}

# Expected: success factors (R² = 0.51, p < .001)