# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 104: "Incubator support mechanisms: satisfaction score (M = 3.89, SD = 0.82)"
# Purpose: Analyze satisfaction scores for incubator support mechanisms

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)
library(lavaan)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SUPPORT_SATISFACTION <- "incubator_support_satisfaction"
COL_SUPPORT_TYPE <- "primary_support_type"
COL_OFFICE_SPACE <- "office_space_quality"
COL_MENTORSHIP_QUALITY <- "mentorship_quality_score"
COL_NETWORKING_OPPORTUNITIES <- "networking_opportunities_score"
COL_BUSINESS_SERVICES <- "business_services_score"
COL_FUNDING_ACCESS <- "funding_access_facilitation"
COL_TECHNICAL_RESOURCES <- "technical_resources_score"
COL_EDUCATIONAL_PROGRAMS <- "educational_program_quality"
COL_PEER_SUPPORT <- "peer_support_score"
COL_INCUBATOR_STAGE <- "incubator_stage"
COL_TIME_IN_INCUBATOR <- "time_in_incubator_months"
COL_COMPANY_GROWTH <- "company_growth_rate"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    incubator_support_satisfaction = as.numeric(incubator_support_satisfaction),
    primary_support_type = factor(primary_support_type),
    office_space_quality = as.numeric(office_space_quality),
    mentorship_quality_score = as.numeric(mentorship_quality_score),
    networking_opportunities_score = as.numeric(networking_opportunities_score),
    business_services_score = as.numeric(business_services_score),
    funding_access_facilitation = as.numeric(funding_access_facilitation),
    technical_resources_score = as.numeric(technical_resources_score),
    educational_program_quality = as.numeric(educational_program_quality),
    peer_support_score = as.numeric(peer_support_score),
    incubator_stage = factor(incubator_stage,
                            levels = c("Pre-incubation", "Incubation", "Post-incubation"),
                            ordered = TRUE),
    time_in_incubator_months = as.numeric(time_in_incubator_months),
    company_growth_rate = as.numeric(company_growth_rate),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(incubator_support_satisfaction))

# Primary descriptive statistics
n_total <- nrow(df)
mean_satisfaction <- mean(df$incubator_support_satisfaction, na.rm = TRUE)
sd_satisfaction <- sd(df$incubator_support_satisfaction, na.rm = TRUE)
median_satisfaction <- median(df$incubator_support_satisfaction, na.rm = TRUE)
se_satisfaction <- sd_satisfaction / sqrt(n_total)

print("Incubator Support Mechanisms Satisfaction - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_satisfaction, 2)))
print(paste("SD =", round(sd_satisfaction, 2)))
print(paste("Median =", round(median_satisfaction, 2)))
print(paste("SE =", round(se_satisfaction, 3)))

# Confidence intervals
ci_lower <- mean_satisfaction - qt(0.975, n_total - 1) * se_satisfaction
ci_upper <- mean_satisfaction + qt(0.975, n_total - 1) * se_satisfaction

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$incubator_support_satisfaction)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution analysis
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$incubator_support_satisfaction, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$incubator_support_satisfaction, na.rm = TRUE), 3)))

# Percentiles
percentiles <- quantile(df$incubator_support_satisfaction, 
                       probs = seq(0, 1, 0.1), 
                       na.rm = TRUE)
print("\nPercentiles:")
print(round(percentiles, 2))

# Coefficient of variation
cv <- (sd_satisfaction / mean_satisfaction) * 100
print(paste("\nCoefficient of variation:", round(cv, 1), "%"))

# Normality tests
shapiro_result <- shapiro.test(df$incubator_support_satisfaction[1:min(5000, n_total)])
print("\nShapiro-Wilk normality test:")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint
scale_midpoint <- 3
t_test_midpoint <- t.test(df$incubator_support_satisfaction, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_satisfaction - scale_midpoint, 2)))

# Bootstrap confidence intervals
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$incubator_support_satisfaction, 
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

# Support type analysis
if(!all(is.na(df$primary_support_type))) {
  support_type_stats <- df %>%
    filter(!is.na(primary_support_type)) %>%
    group_by(primary_support_type) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(incubator_support_satisfaction, na.rm = TRUE), 2),
      sd_satisfaction = round(sd(incubator_support_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_satisfaction))
  
  print("\nSatisfaction by primary support type:")
  print(support_type_stats)
  
  # ANOVA
  if(nrow(support_type_stats) > 2) {
    anova_support <- aov(incubator_support_satisfaction ~ primary_support_type, 
                        data = df %>% filter(primary_support_type %in% support_type_stats$primary_support_type))
    print("\nANOVA by support type:")
    print(summary(anova_support))
  }
}

# Individual support mechanism correlations
support_mechanisms <- c("office_space_quality", "mentorship_quality_score",
                       "networking_opportunities_score", "business_services_score",
                       "funding_access_facilitation", "technical_resources_score",
                       "educational_program_quality", "peer_support_score")

correlation_results <- data.frame(
  Mechanism = character(),
  Correlation = numeric(),
  P_value = numeric(),
  N = integer()
)

for(mechanism in support_mechanisms) {
  if(mechanism %in% names(df) && sum(!is.na(df[[mechanism]])) > 30) {
    cor_test <- cor.test(df$incubator_support_satisfaction,
                        df[[mechanism]],
                        use = "complete.obs")
    
    correlation_results <- rbind(correlation_results,
                                data.frame(
                                  Mechanism = mechanism,
                                  Correlation = cor_test$estimate,
                                  P_value = cor_test$p.value,
                                  N = sum(complete.cases(df$incubator_support_satisfaction, 
                                                        df[[mechanism]]))
                                ))
  }
}

print("\nCorrelations with individual support mechanisms:")
print(correlation_results %>% arrange(desc(abs(Correlation))))

# Factor analysis of support mechanisms
available_mechanisms <- support_mechanisms[support_mechanisms %in% names(df)]
if(length(available_mechanisms) >= 3) {
  df_fa <- df %>%
    select(all_of(available_mechanisms)) %>%
    na.omit()
  
  if(nrow(df_fa) > 100) {
    # KMO test
    kmo_result <- KMO(df_fa)
    print("\nKaiser-Meyer-Olkin (KMO) measure of sampling adequacy:")
    print(paste("Overall MSA:", round(kmo_result$MSA, 3)))
    
    # Factor analysis
    fa_result <- fa(df_fa, nfactors = 2, rotate = "varimax")
    print("\nFactor analysis of support mechanisms:")
    print(fa_result$loadings)
    
    # Variance explained
    print("\nVariance explained by factors:")
    print(round(fa_result$Vaccounted, 3))
  }
}

# Incubator stage analysis
if(!all(is.na(df$incubator_stage))) {
  stage_stats <- df %>%
    filter(!is.na(incubator_stage)) %>%
    group_by(incubator_stage) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(incubator_support_satisfaction, na.rm = TRUE), 2),
      sd_satisfaction = round(sd(incubator_support_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nSatisfaction by incubator stage:")
  print(stage_stats)
  
  # Jonckheere-Terpstra test for trend
  if(nrow(stage_stats) > 2) {
    jt_test <- jonckheere.test(df$incubator_support_satisfaction,
                               df$incubator_stage,
                               alternative = "increasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# Time in incubator analysis
if(sum(!is.na(df$time_in_incubator_months)) > 30) {
  # Categorize time
  df <- df %>%
    mutate(time_category = case_when(
      time_in_incubator_months <= 6 ~ "0-6 months",
      time_in_incubator_months <= 12 ~ "7-12 months",
      time_in_incubator_months <= 24 ~ "13-24 months",
      TRUE ~ ">24 months"
    ))
  
  time_stats <- df %>%
    filter(!is.na(time_category)) %>%
    group_by(time_category) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(incubator_support_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nSatisfaction by time in incubator:")
  print(time_stats)
  
  # Correlation with continuous time
  cor_time <- cor.test(df$incubator_support_satisfaction,
                      df$time_in_incubator_months,
                      use = "complete.obs")
  
  print("\nCorrelation with time in incubator:")
  print(paste("r =", round(cor_time$estimate, 3)))
  print(paste("p-value:", format(cor_time$p.value, scientific = TRUE)))
}

# Company growth analysis
if(sum(!is.na(df$company_growth_rate)) > 30) {
  cor_growth <- cor.test(df$incubator_support_satisfaction,
                        df$company_growth_rate,
                        use = "complete.obs")
  
  print("\nCorrelation with company growth rate:")
  print(paste("r =", round(cor_growth$estimate, 3)))
  print(paste("p-value:", format(cor_growth$p.value, scientific = TRUE)))
  
  # Regression
  lm_growth <- lm(company_growth_rate ~ incubator_support_satisfaction, data = df)
  print("\nRegression - Growth predicted by satisfaction:")
  print(summary(lm_growth))
}

# Multiple regression model
predictors <- c("mentorship_quality_score", "networking_opportunities_score",
               "business_services_score", "funding_access_facilitation",
               "technical_resources_score")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 3) {
  formula_str <- paste("incubator_support_satisfaction ~", 
                      paste(available_predictors, collapse = " + "))
  
  lm_multiple <- lm(as.formula(formula_str), data = df)
  
  print("\nMultiple regression model:")
  print(summary(lm_multiple))
  
  # Standardized coefficients
  df_std <- df %>%
    select(incubator_support_satisfaction, all_of(available_predictors)) %>%
    na.omit() %>%
    mutate(across(everything(), scale))
  
  lm_std <- lm(as.formula(formula_str), data = df_std)
  
  print("\nStandardized coefficients:")
  print(round(coef(lm_std), 3))
  
  # VIF for multicollinearity
  library(car)
  vif_values <- vif(lm_multiple)
  print("\nVariance Inflation Factors:")
  print(round(vif_values, 2))
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_satisfaction = round(mean(incubator_support_satisfaction, na.rm = TRUE), 2),
    sd_satisfaction = round(sd(incubator_support_satisfaction, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_satisfaction))

print("\nSatisfaction by stakeholder:")
print(stakeholder_stats)

# Outlier analysis
z_scores <- abs(scale(df$incubator_support_satisfaction))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

# Robust statistics
mad_value <- mad(df$incubator_support_satisfaction, na.rm = TRUE)
trimmed_mean <- mean(df$incubator_support_satisfaction, trim = 0.1, na.rm = TRUE)
winsorized_mean <- mean(Winsorize(df$incubator_support_satisfaction, probs = c(0.05, 0.95)))

print(paste("\nMedian Absolute Deviation (MAD):", round(mad_value, 3)))
print(paste("10% Trimmed mean:", round(trimmed_mean, 2)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 2)))

# Response pattern analysis
freq_table <- table(round(df$incubator_support_satisfaction, 1))
print("\nFrequency distribution:")
print(freq_table)
print("\nProportions:")
print(round(prop.table(freq_table), 3))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Incubator support mechanisms satisfaction: M =", round(mean_satisfaction, 2), 
           ", SD =", round(sd_satisfaction, 2)))
print(paste("N =", n_total))
print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("The mean of", round(mean_satisfaction, 2), 
           "indicates moderately high satisfaction"))
print(paste("with incubator support mechanisms (SD =", round(sd_satisfaction, 2), ")"))

# Expected: satisfaction score (M = 3.89, SD = 0.82)