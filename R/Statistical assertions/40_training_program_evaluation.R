# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 40: "Training program participation predicted improved assessment scores (β = .183, SE = .042, p < .001, R² = .034)"
# Purpose: Linear regression analysis predicting assessment score improvements from training program participation

library(tidyverse)
library(janitor)
library(broom)
library(car)
library(lmtest)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TRAINING_PARTICIPATION <- "training_program_participation"
COL_ASSESSMENT_IMPROVEMENT <- "assessment_score_improvement"
COL_STAKEHOLDER <- "stakeholder"
COL_EXPERIENCE <- "experience_years"
COL_BASELINE_SCORE <- "baseline_assessment_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    training_program_participation = as.numeric(training_program_participation),
    assessment_score_improvement = as.numeric(assessment_score_improvement),
    stakeholder = factor(stakeholder),
    experience_years = as.numeric(experience_years),
    baseline_assessment_score = as.numeric(baseline_assessment_score)
  ) %>%
  filter(!is.na(training_program_participation), 
         !is.na(assessment_score_improvement)) %>%
  # Remove extreme outliers
  filter(assessment_score_improvement >= -10, assessment_score_improvement <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    training_mean = round(mean(training_program_participation), 2),
    training_sd = round(sd(training_program_participation), 2),
    improvement_mean = round(mean(assessment_score_improvement), 2),
    improvement_sd = round(sd(assessment_score_improvement), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Training participation breakdown
training_breakdown <- df %>%
  mutate(
    participation_level = case_when(
      training_program_participation == 0 ~ "No Participation",
      training_program_participation <= 2 ~ "Low Participation",
      training_program_participation <= 4 ~ "Moderate Participation",
      training_program_participation <= 6 ~ "High Participation"
    )
  ) %>%
  group_by(participation_level) %>%
  summarise(
    n = n(),
    mean_improvement = round(mean(assessment_score_improvement), 2),
    sd_improvement = round(sd(assessment_score_improvement), 2),
    .groups = "drop"
  )

print("Assessment improvement by training participation level:")
print(training_breakdown)

# Simple linear regression
lm_model <- lm(assessment_score_improvement ~ training_program_participation, 
               data = df)
lm_summary <- summary(lm_model)

print("Simple linear regression results:")
print(lm_summary)

# Extract key statistics
beta_coefficient <- coef(lm_model)[2]
beta_se <- lm_summary$coefficients[2, 2]
beta_t <- lm_summary$coefficients[2, 3]
beta_p <- lm_summary$coefficients[2, 4]
r_squared <- lm_summary$r.squared
adj_r_squared <- lm_summary$adj.r.squared

print("Key regression statistics:")
print(paste("β =", round(beta_coefficient, 3)))
print(paste("SE =", round(beta_se, 3)))
print(paste("t =", round(beta_t, 2)))
print(paste("p =", format(beta_p, scientific = TRUE)))
print(paste("R² =", round(r_squared, 3)))
print(paste("Adjusted R² =", round(adj_r_squared, 3)))

# 95% Confidence interval for beta coefficient
beta_ci <- confint(lm_model)[2, ]
print(paste("95% CI for β: [", round(beta_ci[1], 3), ", ", round(beta_ci[2], 3), "]", sep=""))

# Model diagnostics
print("Model diagnostics:")

# Check assumptions
# 1. Linearity - residuals vs fitted
fitted_values <- fitted(lm_model)
residuals <- residuals(lm_model)

# 2. Normality of residuals - Shapiro-Wilk test
shapiro_residuals <- shapiro.test(sample(residuals, min(5000, length(residuals))))
print(paste("Shapiro-Wilk test of residuals: W =", round(shapiro_residuals$statistic, 3),
            ", p =", format(shapiro_residuals$p.value, scientific = TRUE)))

# 3. Homoscedasticity - Breusch-Pagan test
bp_test <- bptest(lm_model)
print(paste("Breusch-Pagan test: BP =", round(bp_test$statistic, 3),
            ", df =", bp_test$parameter,
            ", p =", format(bp_test$p.value, scientific = TRUE)))

# 4. Independence - Durbin-Watson test
dw_test <- durbinWatsonTest(lm_model)
print(paste("Durbin-Watson test: DW =", round(dw_test$dw, 3),
            ", p =", format(dw_test$p, scientific = TRUE)))

# Multiple regression with control variables
if(sum(!is.na(df$experience_years)) > 100 && sum(!is.na(df$baseline_assessment_score)) > 100) {
  df_complete <- df %>%
    filter(!is.na(experience_years), !is.na(baseline_assessment_score), !is.na(stakeholder))
  
  lm_multiple <- lm(assessment_score_improvement ~ training_program_participation + 
                   experience_years + baseline_assessment_score + stakeholder, 
                   data = df_complete)
  
  multiple_summary <- summary(lm_multiple)
  print("Multiple regression with controls:")
  print(multiple_summary)
  
  # Extract training coefficient from multiple regression
  training_coef_multiple <- multiple_summary$coefficients["training_program_participation", ]
  print("Training participation coefficient (controlled):")
  print(paste("β =", round(training_coef_multiple[1], 3)))
  print(paste("SE =", round(training_coef_multiple[2], 3)))
  print(paste("p =", format(training_coef_multiple[4], scientific = TRUE)))
}

# Effect size interpretation
# Cohen's conventions for R²: 0.01 = small, 0.09 = medium, 0.25 = large
r_squared_interpretation <- case_when(
  r_squared < 0.01 ~ "Negligible",
  r_squared < 0.09 ~ "Small",
  r_squared < 0.25 ~ "Medium",
  TRUE ~ "Large"
)
print(paste("R² effect size interpretation:", r_squared_interpretation))

# Standardized beta coefficient
df_standardized <- df %>%
  mutate(
    training_z = scale(training_program_participation)[,1],
    improvement_z = scale(assessment_score_improvement)[,1]
  )

lm_standardized <- lm(improvement_z ~ training_z, data = df_standardized)
standardized_beta <- coef(lm_standardized)[2]
print(paste("Standardized β =", round(standardized_beta, 3)))

# Correlation for comparison
cor_result <- cor.test(df$training_program_participation, df$assessment_score_improvement)
print(paste("Pearson correlation: r =", round(cor_result$estimate, 3),
            ", p =", format(cor_result$p.value, scientific = TRUE)))

# Breakdown by stakeholder group
stakeholder_regression <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%
  do(model = lm(assessment_score_improvement ~ training_program_participation, data = .)) %>%
  mutate(
    n = df %>% filter(stakeholder == .$stakeholder[1]) %>% nrow(),
    beta = map_dbl(model, ~ coef(.x)[2]),
    r_squared = map_dbl(model, ~ summary(.x)$r.squared),
    p_value = map_dbl(model, ~ summary(.x)$coefficients[2, 4])
  ) %>%
  select(-model)

print("Regression results by stakeholder group (n ≥ 30):")
print(stakeholder_regression)

# Prediction intervals for new observations
new_data <- data.frame(training_program_participation = c(0, 1, 2, 3, 4, 5))
predictions <- predict(lm_model, newdata = new_data, interval = "prediction")
prediction_table <- cbind(new_data, round(predictions, 2))
print("Predicted assessment improvements by training level:")
print(prediction_table)

# Expected: β = .183, SE = .042, p < .001, R² = .034