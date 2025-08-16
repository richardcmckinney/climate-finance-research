# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 106: "Tax credit utilization: effectiveness rating (M = 3.73, SD = 0.91)"
# Purpose: 

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TAX_CREDIT_EFFECTIVENESS <- "tax_credit_effectiveness_rating"
COL_CREDIT_TYPE <- "primary_tax_credit_type"
COL_CREDIT_AMOUNT <- "annual_tax_credit_amount"
COL_UTILIZATION_RATE <- "tax_credit_utilization_percentage"
COL_APPLICATION_COMPLEXITY <- "application_complexity_score"
COL_DOCUMENTATION_BURDEN <- "documentation_burden_rating"
COL_APPROVAL_TIME <- "approval_processing_time_days"
COL_COMPLIANCE_COST <- "compliance_cost_percentage"
COL_CASH_FLOW_IMPACT <- "cash_flow_impact_score"
COL_REINVESTMENT_RATE <- "credit_reinvestment_percentage"
COL_COMPANY_SIZE <- "company_size_category"
COL_INDUSTRY <- "industry_sector"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    tax_credit_effectiveness_rating = as.numeric(tax_credit_effectiveness_rating),
    primary_tax_credit_type = factor(primary_tax_credit_type),
    annual_tax_credit_amount = as.numeric(annual_tax_credit_amount),
    tax_credit_utilization_percentage = as.numeric(tax_credit_utilization_percentage),
    application_complexity_score = as.numeric(application_complexity_score),
    documentation_burden_rating = as.numeric(documentation_burden_rating),
    approval_processing_time_days = as.numeric(approval_processing_time_days),
    compliance_cost_percentage = as.numeric(compliance_cost_percentage),
    cash_flow_impact_score = as.numeric(cash_flow_impact_score),
    credit_reinvestment_percentage = as.numeric(credit_reinvestment_percentage),
    company_size_category = factor(company_size_category,
                                  levels = c("Small", "Medium", "Large", "Enterprise"),
                                  ordered = TRUE),
    industry_sector = factor(industry_sector),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(tax_credit_effectiveness_rating))

# Primary descriptive statistics
n_total <- nrow(df)
mean_effectiveness <- mean(df$tax_credit_effectiveness_rating, na.rm = TRUE)
sd_effectiveness <- sd(df$tax_credit_effectiveness_rating, na.rm = TRUE)
median_effectiveness <- median(df$tax_credit_effectiveness_rating, na.rm = TRUE)
se_effectiveness <- sd_effectiveness / sqrt(n_total)

print("Tax Credit Utilization Effectiveness - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_effectiveness, 2)))
print(paste("SD =", round(sd_effectiveness, 2)))
print(paste("Median =", round(median_effectiveness, 2)))
print(paste("SE =", round(se_effectiveness, 3)))

# Confidence intervals
ci_lower <- mean_effectiveness - qt(0.975, n_total - 1) * se_effectiveness
ci_upper <- mean_effectiveness + qt(0.975, n_total - 1) * se_effectiveness

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$tax_credit_effectiveness_rating)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution analysis
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$tax_credit_effectiveness_rating, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$tax_credit_effectiveness_rating, na.rm = TRUE), 3)))

# Percentiles
percentiles <- quantile(df$tax_credit_effectiveness_rating, 
                       probs = seq(0, 1, 0.1), 
                       na.rm = TRUE)
print("\nPercentiles:")
print(round(percentiles, 2))

# Coefficient of variation
cv <- (sd_effectiveness / mean_effectiveness) * 100
print(paste("\nCoefficient of variation:", round(cv, 1), "%"))

# Normality tests
shapiro_result <- shapiro.test(df$tax_credit_effectiveness_rating[1:min(5000, n_total)])
print("\nShapiro-Wilk normality test:")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint
scale_midpoint <- 3
t_test_midpoint <- t.test(df$tax_credit_effectiveness_rating, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_effectiveness - scale_midpoint, 2)))

# Effect size
cohens_d_one <- cohens_d(df$tax_credit_effectiveness_rating, mu = scale_midpoint)
print(paste("Cohen's d:", round(cohens_d_one$Cohens_d, 3)))

# Bootstrap confidence intervals
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$tax_credit_effectiveness_rating, 
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

# Analysis by tax credit type
if(!all(is.na(df$primary_tax_credit_type))) {
  type_stats <- df %>%
    filter(!is.na(primary_tax_credit_type)) %>%
    group_by(primary_tax_credit_type) %>%
    summarise(
      n = n(),
      mean_effectiveness = round(mean(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      sd_effectiveness = round(sd(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_effectiveness))
  
  print("\nEffectiveness by tax credit type:")
  print(type_stats)
  
  # ANOVA
  if(nrow(type_stats) > 2) {
    anova_type <- aov(tax_credit_effectiveness_rating ~ primary_tax_credit_type, 
                     data = df %>% filter(primary_tax_credit_type %in% type_stats$primary_tax_credit_type))
    print("\nANOVA by tax credit type:")
    print(summary(anova_type))
  }
}

# Credit amount analysis
if(sum(!is.na(df$annual_tax_credit_amount)) > 30) {
  # Log transform credit amount
  df$log_credit_amount <- log10(df$annual_tax_credit_amount + 1)
  
  cor_amount <- cor.test(df$tax_credit_effectiveness_rating,
                        df$log_credit_amount,
                        use = "complete.obs")
  
  print("\nCorrelation with log(credit amount):")
  print(paste("r =", round(cor_amount$estimate, 3)))
  print(paste("p-value:", format(cor_amount$p.value, scientific = TRUE)))
  
  # Categorize credit amounts
  df <- df %>%
    mutate(amount_category = case_when(
      annual_tax_credit_amount <= 10000 ~ "Small (<$10K)",
      annual_tax_credit_amount <= 50000 ~ "Medium ($10-50K)",
      annual_tax_credit_amount <= 250000 ~ "Large ($50-250K)",
      TRUE ~ "Very Large (>$250K)"
    ))
  
  amount_stats <- df %>%
    filter(!is.na(amount_category)) %>%
    group_by(amount_category) %>%
    summarise(
      n = n(),
      mean_effectiveness = round(mean(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nEffectiveness by credit amount category:")
  print(amount_stats)
}

# Utilization rate analysis
if(sum(!is.na(df$tax_credit_utilization_percentage)) > 30) {
  cor_utilization <- cor.test(df$tax_credit_effectiveness_rating,
                             df$tax_credit_utilization_percentage,
                             use = "complete.obs")
  
  print("\nCorrelation with utilization percentage:")
  print(paste("r =", round(cor_utilization$estimate, 3)))
  print(paste("p-value:", format(cor_utilization$p.value, scientific = TRUE)))
  
  # Regression
  lm_utilization <- lm(tax_credit_effectiveness_rating ~ tax_credit_utilization_percentage, 
                       data = df)
  print("\nRegression - Effectiveness predicted by utilization:")
  print(summary(lm_utilization))
}

# Application complexity analysis
if(sum(!is.na(df$application_complexity_score)) > 30) {
  cor_complexity <- cor.test(df$tax_credit_effectiveness_rating,
                            df$application_complexity_score,
                            use = "complete.obs")
  
  print("\nCorrelation with application complexity:")
  print(paste("r =", round(cor_complexity$estimate, 3)))
  print(paste("p-value:", format(cor_complexity$p.value, scientific = TRUE)))
}

# Documentation burden analysis
if(sum(!is.na(df$documentation_burden_rating)) > 30) {
  cor_documentation <- cor.test(df$tax_credit_effectiveness_rating,
                               df$documentation_burden_rating,
                               use = "complete.obs")
  
  print("\nCorrelation with documentation burden:")
  print(paste("r =", round(cor_documentation$estimate, 3)))
  print(paste("p-value:", format(cor_documentation$p.value, scientific = TRUE)))
}

# Approval time analysis
if(sum(!is.na(df$approval_processing_time_days)) > 30) {
  # Categorize approval time
  df <- df %>%
    mutate(approval_category = case_when(
      approval_processing_time_days <= 30 ~ "Fast (≤30 days)",
      approval_processing_time_days <= 90 ~ "Moderate (31-90 days)",
      approval_processing_time_days <= 180 ~ "Slow (91-180 days)",
      TRUE ~ "Very Slow (>180 days)"
    ))
  
  approval_stats <- df %>%
    filter(!is.na(approval_category)) %>%
    group_by(approval_category) %>%
    summarise(
      n = n(),
      mean_effectiveness = round(mean(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nEffectiveness by approval time:")
  print(approval_stats)
}

# Cash flow impact analysis
if(sum(!is.na(df$cash_flow_impact_score)) > 30) {
  cor_cashflow <- cor.test(df$tax_credit_effectiveness_rating,
                          df$cash_flow_impact_score,
                          use = "complete.obs")
  
  print("\nCorrelation with cash flow impact:")
  print(paste("r =", round(cor_cashflow$estimate, 3)))
  print(paste("p-value:", format(cor_cashflow$p.value, scientific = TRUE)))
}

# Company size analysis
if(!all(is.na(df$company_size_category))) {
  size_stats <- df %>%
    filter(!is.na(company_size_category)) %>%
    group_by(company_size_category) %>%
    summarise(
      n = n(),
      mean_effectiveness = round(mean(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      sd_effectiveness = round(sd(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nEffectiveness by company size:")
  print(size_stats)
  
  # Jonckheere-Terpstra test for trend
  if(nrow(size_stats) > 2) {
    jt_test <- jonckheere.test(df$tax_credit_effectiveness_rating,
                               df$company_size_category,
                               alternative = "increasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# Industry analysis
if(length(unique(df$industry_sector)) > 3) {
  industry_stats <- df %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      mean_effectiveness = round(mean(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      sd_effectiveness = round(sd(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_effectiveness))
  
  print("\nTop industries by effectiveness:")
  print(head(industry_stats, 10))
}

# Multiple regression model
predictors <- c("tax_credit_utilization_percentage", "application_complexity_score",
               "documentation_burden_rating", "cash_flow_impact_score")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("tax_credit_effectiveness_rating ~", 
                      paste(available_predictors, collapse = " + "))
  
  lm_multiple <- lm(as.formula(formula_str), data = df)
  
  print("\nMultiple regression model:")
  print(summary(lm_multiple))
  
  # Standardized coefficients
  df_std <- df %>%
    select(tax_credit_effectiveness_rating, all_of(available_predictors)) %>%
    na.omit() %>%
    mutate(across(everything(), scale))
  
  lm_std <- lm(as.formula(formula_str), data = df_std)
  
  print("\nStandardized coefficients:")
  print(round(coef(lm_std), 3))
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_effectiveness = round(mean(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
    sd_effectiveness = round(sd(tax_credit_effectiveness_rating, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_effectiveness))

print("\nEffectiveness by stakeholder:")
print(stakeholder_stats)

# Outlier analysis
z_scores <- abs(scale(df$tax_credit_effectiveness_rating))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

# Robust statistics
mad_value <- mad(df$tax_credit_effectiveness_rating, na.rm = TRUE)
trimmed_mean <- mean(df$tax_credit_effectiveness_rating, trim = 0.1, na.rm = TRUE)
winsorized_mean <- mean(Winsorize(df$tax_credit_effectiveness_rating, probs = c(0.05, 0.95)))

print(paste("\nMedian Absolute Deviation (MAD):", round(mad_value, 3)))
print(paste("10% Trimmed mean:", round(trimmed_mean, 2)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 2)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Tax credit utilization effectiveness: M =", round(mean_effectiveness, 2), 
           ", SD =", round(sd_effectiveness, 2)))
print(paste("N =", n_total))
print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("The mean rating of", round(mean_effectiveness, 2), 
           "indicates moderate to high perceived effectiveness"))
print(paste("with moderate variability (SD =", round(sd_effectiveness, 2), ")"))

# Expected: effectiveness rating (M = 3.73, SD = 0.91)