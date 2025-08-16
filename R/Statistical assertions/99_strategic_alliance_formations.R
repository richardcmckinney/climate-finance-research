# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 99: "Strategic alliance formations: success predictors (R² = 0.42, p < .001)"
# Purpose: Analyze predictors of strategic alliance formation success

library(tidyverse)
library(janitor)
library(DescTools)
library(car)
library(leaps)
library(glmnet)
library(caret)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_ALLIANCE_SUCCESS <- "strategic_alliance_success_score"
COL_PARTNER_COMPATIBILITY <- "partner_compatibility_score"
COL_STRATEGIC_FIT <- "strategic_fit_score"
COL_RESOURCE_COMPLEMENTARITY <- "resource_complementarity_score"
COL_TRUST_LEVEL <- "trust_level_score"
COL_COMMUNICATION_QUALITY <- "communication_quality_score"
COL_GOVERNANCE_STRUCTURE <- "governance_structure_score"
COL_CULTURAL_ALIGNMENT <- "cultural_alignment_score"
COL_PRIOR_RELATIONSHIP <- "prior_relationship_exists"
COL_ALLIANCE_EXPERIENCE <- "alliance_experience_years"
COL_MARKET_OVERLAP <- "market_overlap_percentage"
COL_TECHNOLOGY_SIMILARITY <- "technology_similarity_score"
COL_FINANCIAL_STRENGTH <- "combined_financial_strength"
COL_INDUSTRY <- "industry_sector"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    strategic_alliance_success_score = as.numeric(strategic_alliance_success_score),
    partner_compatibility_score = as.numeric(partner_compatibility_score),
    strategic_fit_score = as.numeric(strategic_fit_score),
    resource_complementarity_score = as.numeric(resource_complementarity_score),
    trust_level_score = as.numeric(trust_level_score),
    communication_quality_score = as.numeric(communication_quality_score),
    governance_structure_score = as.numeric(governance_structure_score),
    cultural_alignment_score = as.numeric(cultural_alignment_score),
    prior_relationship_exists = as.numeric(prior_relationship_exists),
    alliance_experience_years = as.numeric(alliance_experience_years),
    market_overlap_percentage = as.numeric(market_overlap_percentage),
    technology_similarity_score = as.numeric(technology_similarity_score),
    combined_financial_strength = as.numeric(combined_financial_strength),
    industry_sector = factor(industry_sector),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(strategic_alliance_success_score))

# Identify available predictors
predictors <- c("partner_compatibility_score", "strategic_fit_score", 
               "resource_complementarity_score", "trust_level_score",
               "communication_quality_score", "governance_structure_score",
               "cultural_alignment_score", "prior_relationship_exists",
               "alliance_experience_years", "market_overlap_percentage",
               "technology_similarity_score", "combined_financial_strength")

available_predictors <- predictors[predictors %in% names(df)]
available_predictors <- available_predictors[sapply(df[available_predictors], 
                                                   function(x) sum(!is.na(x)) > 30)]

print(paste("Available predictors:", length(available_predictors)))
print(available_predictors)

# Create complete cases dataset
df_complete <- df %>%
  select(strategic_alliance_success_score, all_of(available_predictors)) %>%
  na.omit()

n_total <- nrow(df_complete)
n_predictors <- length(available_predictors)

print(paste("\nComplete cases:", n_total))
print(paste("Number of predictors:", n_predictors))
print(paste("Cases per predictor:", round(n_total / n_predictors, 1)))

# Full multiple regression model
formula_full <- paste("strategic_alliance_success_score ~", 
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

# Confidence intervals for coefficients
ci_coefs <- confint(lm_full)
print("\n95% Confidence intervals for unstandardized coefficients:")
print(round(ci_coefs, 4))

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
bp_test <- bptest(lm_full)
print(paste("Breusch-Pagan test: χ² =", round(bp_test$statistic, 2),
           ", p =", format(bp_test$p.value, scientific = TRUE)))

# Durbin-Watson for autocorrelation
dw_test <- durbinWatsonTest(lm_full)
print(paste("Durbin-Watson statistic:", round(dw_test, 3)))

# Cook's distance for influential observations
cooks_d <- cooks.distance(lm_full)
influential <- which(cooks_d > 4/n_total)
print(paste("\nInfluential observations (Cook's D > 4/n):", length(influential)))

# Stepwise selection
print("\n=== STEPWISE SELECTION ===")

# Backward elimination
lm_backward <- step(lm_full, direction = "backward", trace = 0)
print("Backward elimination model:")
print(summary(lm_backward))

# Forward selection
lm_null <- lm(strategic_alliance_success_score ~ 1, data = df_complete)
lm_forward <- step(lm_null, 
                  scope = list(lower = lm_null, upper = lm_full),
                  direction = "forward", trace = 0)
print("\nForward selection model:")
print(summary(lm_forward))

# Best subsets regression
if(n_predictors <= 15) {
  regsubsets_full <- regsubsets(as.formula(formula_full), 
                                data = df_complete, 
                                nvmax = n_predictors)
  reg_summary <- summary(regsubsets_full)
  
  print("\nBest subsets - Adjusted R² by model size:")
  best_adjr2 <- data.frame(
    n_vars = 1:length(reg_summary$adjr2),
    adj_r2 = round(reg_summary$adjr2, 3)
  )
  print(best_adjr2)
  
  # Best model by adjusted R²
  best_model_size <- which.max(reg_summary$adjr2)
  print(paste("\nBest model size by adjusted R²:", best_model_size, "predictors"))
  print("Variables in best model:")
  print(names(coef(regsubsets_full, best_model_size))[-1])
}

# LASSO regression
if(n_predictors >= 5) {
  set.seed(123)
  X <- as.matrix(df_complete[, available_predictors])
  y <- df_complete$strategic_alliance_success_score
  
  # Cross-validation for lambda
  cv_lasso <- cv.glmnet(X, y, alpha = 1, nfolds = 10)
  
  print("\n=== LASSO REGRESSION ===")
  print(paste("Lambda (min):", round(cv_lasso$lambda.min, 4)))
  print(paste("Lambda (1se):", round(cv_lasso$lambda.1se, 4)))
  
  # Coefficients at optimal lambda
  lasso_model <- glmnet(X, y, alpha = 1, lambda = cv_lasso$lambda.min)
  lasso_coefs <- coef(lasso_model)
  
  non_zero_coefs <- lasso_coefs[lasso_coefs[,1] != 0, ]
  print("\nNon-zero LASSO coefficients:")
  print(round(non_zero_coefs, 4))
  
  # R² for LASSO model
  lasso_pred <- predict(lasso_model, X)
  lasso_r2 <- cor(lasso_pred, y)^2
  print(paste("LASSO R²:", round(lasso_r2, 3)))
}

# Ridge regression
if(n_predictors >= 5) {
  cv_ridge <- cv.glmnet(X, y, alpha = 0, nfolds = 10)
  ridge_model <- glmnet(X, y, alpha = 0, lambda = cv_ridge$lambda.min)
  ridge_pred <- predict(ridge_model, X)
  ridge_r2 <- cor(ridge_pred, y)^2
  print(paste("\nRidge R²:", round(ridge_r2, 3)))
}

# Cross-validation of full model
set.seed(123)
train_control <- trainControl(method = "cv", number = 10)
cv_model <- train(as.formula(formula_full), 
                 data = df_complete,
                 method = "lm",
                 trControl = train_control)

print("\n=== 10-FOLD CROSS-VALIDATION ===")
print(paste("Cross-validated R²:", round(cv_model$results$Rsquared, 3)))
print(paste("Cross-validated RMSE:", round(cv_model$results$RMSE, 3)))

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
  print(importance_df)
}

# Hierarchical regression (if logical groups exist)
if(all(c("partner_compatibility_score", "strategic_fit_score", 
        "trust_level_score") %in% available_predictors)) {
  
  # Step 1: Relationship factors
  lm_step1 <- lm(strategic_alliance_success_score ~ partner_compatibility_score + 
                trust_level_score, data = df_complete)
  
  # Step 2: Add strategic factors
  lm_step2 <- lm(strategic_alliance_success_score ~ partner_compatibility_score + 
                trust_level_score + strategic_fit_score + 
                resource_complementarity_score, data = df_complete)
  
  print("\n=== HIERARCHICAL REGRESSION ===")
  print("Step 1 - Relationship factors:")
  print(paste("R² =", round(summary(lm_step1)$r.squared, 3)))
  
  print("\nStep 2 - Add strategic factors:")
  print(paste("R² =", round(summary(lm_step2)$r.squared, 3)))
  print(paste("ΔR² =", round(summary(lm_step2)$r.squared - 
                            summary(lm_step1)$r.squared, 3)))
  
  # F-test for R² change
  anova_hier <- anova(lm_step1, lm_step2)
  print("\nF-test for R² change:")
  print(anova_hier)
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
# Post-hoc power
f2 <- cohens_f2
power_observed <- pwr.f2.test(u = n_predictors, v = n_total - n_predictors - 1, 
                              f2 = f2, sig.level = 0.05)
print(paste("\nPost-hoc power:", round(power_observed$power, 3)))

# Sample size for 80% power
n_required <- pwr.f2.test(u = n_predictors, f2 = f2, 
                         sig.level = 0.05, power = 0.80)$v + n_predictors + 1
print(paste("Sample size needed for 80% power:", ceiling(n_required)))

# Top 3 predictors summary
top_3_predictors <- std_coef_df %>%
  filter(p_value < 0.05) %>%
  head(3)

print("\n=== TOP 3 SIGNIFICANT PREDICTORS ===")
print(top_3_predictors)

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print("Strategic alliance success can be predicted by multiple factors")
print(paste("Full model: R² =", round(r_squared, 2), 
           ", Adjusted R² =", round(adj_r_squared, 2)))
print(paste("F(", f_df1, ",", f_df2, ") =", round(f_stat, 2),
           ", p <", ifelse(p_value < 0.001, ".001", round(p_value, 3))))
print(paste("The model explains", round(r_squared * 100, 1), 
           "% of variance in alliance success"))
print(paste("Cross-validated R² =", round(cv_model$results$Rsquared, 3),
           "suggests good generalizability"))

# Expected: success predictors (R² = 0.42, p < .001)