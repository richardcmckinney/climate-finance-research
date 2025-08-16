# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 100: "Merger acquisition considerations: due diligence score (M = 4.12, SD = 0.76)"
# Purpose: Analyze due diligence scores in merger and acquisition considerations

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_DUE_DILIGENCE_SCORE <- "ma_due_diligence_score"
COL_DEAL_SIZE <- "deal_size_category"
COL_TARGET_INDUSTRY <- "target_industry_sector"
COL_ACQUIRER_TYPE <- "acquirer_type"
COL_STRATEGIC_RATIONALE <- "strategic_rationale_score"
COL_FINANCIAL_HEALTH <- "target_financial_health_score"
COL_CULTURAL_FIT <- "cultural_fit_assessment"
COL_SYNERGY_POTENTIAL <- "synergy_potential_score"
COL_INTEGRATION_COMPLEXITY <- "integration_complexity_rating"
COL_REGULATORY_HURDLES <- "regulatory_hurdle_count"
COL_DEAL_SUCCESS <- "deal_success_indicator"
COL_TIME_TO_CLOSE <- "time_to_close_days"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    ma_due_diligence_score = as.numeric(ma_due_diligence_score),
    deal_size_category = factor(deal_size_category,
                               levels = c("Small", "Medium", "Large", "Mega"),
                               ordered = TRUE),
    target_industry_sector = factor(target_industry_sector),
    acquirer_type = factor(acquirer_type),
    strategic_rationale_score = as.numeric(strategic_rationale_score),
    target_financial_health_score = as.numeric(target_financial_health_score),
    cultural_fit_assessment = as.numeric(cultural_fit_assessment),
    synergy_potential_score = as.numeric(synergy_potential_score),
    integration_complexity_rating = as.numeric(integration_complexity_rating),
    regulatory_hurdle_count = as.numeric(regulatory_hurdle_count),
    deal_success_indicator = factor(deal_success_indicator),
    time_to_close_days = as.numeric(time_to_close_days),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(ma_due_diligence_score))

# Primary descriptive statistics
n_total <- nrow(df)
mean_score <- mean(df$ma_due_diligence_score, na.rm = TRUE)
sd_score <- sd(df$ma_due_diligence_score, na.rm = TRUE)
median_score <- median(df$ma_due_diligence_score, na.rm = TRUE)
se_score <- sd_score / sqrt(n_total)

print("M&A Due Diligence Score - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_score, 2)))
print(paste("SD =", round(sd_score, 2)))
print(paste("Median =", round(median_score, 2)))
print(paste("SE =", round(se_score, 3)))

# Confidence intervals
ci_lower <- mean_score - qt(0.975, n_total - 1) * se_score
ci_upper <- mean_score + qt(0.975, n_total - 1) * se_score

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$ma_due_diligence_score)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution analysis
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$ma_due_diligence_score, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$ma_due_diligence_score, na.rm = TRUE), 3)))

# Percentiles
percentiles <- quantile(df$ma_due_diligence_score, 
                       probs = seq(0, 1, 0.1), 
                       na.rm = TRUE)
print("\nPercentiles:")
print(round(percentiles, 2))

# Coefficient of variation
cv <- (sd_score / mean_score) * 100
print(paste("\nCoefficient of variation:", round(cv, 1), "%"))

# Normality tests
shapiro_result <- shapiro.test(df$ma_due_diligence_score[1:min(5000, n_total)])
print("\nShapiro-Wilk normality test:")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint (assuming 1-5 scale)
scale_midpoint <- 3
t_test_midpoint <- t.test(df$ma_due_diligence_score, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_score - scale_midpoint, 2)))

# Bootstrap confidence intervals
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$ma_due_diligence_score, 
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

# Analysis by deal size
if(!all(is.na(df$deal_size_category))) {
  size_stats <- df %>%
    filter(!is.na(deal_size_category)) %>%
    group_by(deal_size_category) %>%
    summarise(
      n = n(),
      mean_dd = round(mean(ma_due_diligence_score, na.rm = TRUE), 2),
      sd_dd = round(sd(ma_due_diligence_score, na.rm = TRUE), 2),
      median_dd = round(median(ma_due_diligence_score, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nDue diligence score by deal size:")
  print(size_stats)
  
  # Jonckheere-Terpstra test for trend
  if(nrow(size_stats) > 2) {
    jt_test <- jonckheere.test(df$ma_due_diligence_score, 
                               df$deal_size_category,
                               alternative = "increasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# Analysis by acquirer type
if(!all(is.na(df$acquirer_type))) {
  acquirer_stats <- df %>%
    filter(!is.na(acquirer_type)) %>%
    group_by(acquirer_type) %>%
    summarise(
      n = n(),
      mean_dd = round(mean(ma_due_diligence_score, na.rm = TRUE), 2),
      sd_dd = round(sd(ma_due_diligence_score, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_dd))
  
  print("\nDue diligence score by acquirer type:")
  print(acquirer_stats)
  
  # ANOVA if multiple types
  if(nrow(acquirer_stats) > 2) {
    anova_acquirer <- aov(ma_due_diligence_score ~ acquirer_type, 
                         data = df %>% filter(acquirer_type %in% acquirer_stats$acquirer_type))
    print("\nANOVA by acquirer type:")
    print(summary(anova_acquirer))
  }
}

# Correlation with strategic rationale
if(sum(!is.na(df$strategic_rationale_score)) > 30) {
  cor_strategic <- cor.test(df$ma_due_diligence_score,
                           df$strategic_rationale_score,
                           use = "complete.obs")
  
  print("\nCorrelation with strategic rationale:")
  print(paste("r =", round(cor_strategic$estimate, 3)))
  print(paste("p-value:", format(cor_strategic$p.value, scientific = TRUE)))
}

# Correlation with financial health
if(sum(!is.na(df$target_financial_health_score)) > 30) {
  cor_financial <- cor.test(df$ma_due_diligence_score,
                           df$target_financial_health_score,
                           use = "complete.obs")
  
  print("\nCorrelation with target financial health:")
  print(paste("r =", round(cor_financial$estimate, 3)))
  print(paste("p-value:", format(cor_financial$p.value, scientific = TRUE)))
}

# Cultural fit analysis
if(sum(!is.na(df$cultural_fit_assessment)) > 30) {
  cor_culture <- cor.test(df$ma_due_diligence_score,
                         df$cultural_fit_assessment,
                         use = "complete.obs")
  
  print("\nCorrelation with cultural fit assessment:")
  print(paste("r =", round(cor_culture$estimate, 3)))
  print(paste("p-value:", format(cor_culture$p.value, scientific = TRUE)))
  
  # Regression analysis
  lm_culture <- lm(ma_due_diligence_score ~ cultural_fit_assessment, data = df)
  print("\nRegression - DD score predicted by cultural fit:")
  print(summary(lm_culture))
}

# Synergy potential analysis
if(sum(!is.na(df$synergy_potential_score)) > 30) {
  cor_synergy <- cor.test(df$ma_due_diligence_score,
                         df$synergy_potential_score,
                         use = "complete.obs")
  
  print("\nCorrelation with synergy potential:")
  print(paste("r =", round(cor_synergy$estimate, 3)))
  print(paste("p-value:", format(cor_synergy$p.value, scientific = TRUE)))
}

# Integration complexity analysis
if(sum(!is.na(df$integration_complexity_rating)) > 30) {
  cor_complexity <- cor.test(df$ma_due_diligence_score,
                            df$integration_complexity_rating,
                            use = "complete.obs")
  
  print("\nCorrelation with integration complexity:")
  print(paste("r =", round(cor_complexity$estimate, 3)))
  print(paste("p-value:", format(cor_complexity$p.value, scientific = TRUE)))
}

# Regulatory hurdles analysis
if(sum(!is.na(df$regulatory_hurdle_count)) > 30) {
  # Categorize regulatory hurdles
  df <- df %>%
    mutate(regulatory_category = case_when(
      regulatory_hurdle_count == 0 ~ "No hurdles",
      regulatory_hurdle_count <= 2 ~ "Low (1-2)",
      regulatory_hurdle_count <= 5 ~ "Medium (3-5)",
      TRUE ~ "High (6+)"
    ))
  
  regulatory_stats <- df %>%
    filter(!is.na(regulatory_category)) %>%
    group_by(regulatory_category) %>%
    summarise(
      n = n(),
      mean_dd = round(mean(ma_due_diligence_score, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nDue diligence score by regulatory hurdles:")
  print(regulatory_stats)
}

# Deal success analysis
if(!all(is.na(df$deal_success_indicator))) {
  success_comparison <- df %>%
    filter(!is.na(deal_success_indicator)) %>%
    group_by(deal_success_indicator) %>%
    summarise(
      n = n(),
      mean_dd = round(mean(ma_due_diligence_score, na.rm = TRUE), 2),
      sd_dd = round(sd(ma_due_diligence_score, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nDue diligence score by deal success:")
  print(success_comparison)
  
  # t-test if binary
  if(length(unique(df$deal_success_indicator)) == 2) {
    t_test_success <- t.test(ma_due_diligence_score ~ deal_success_indicator, 
                            data = df)
    print("\nt-test by deal success:")
    print(paste("t =", round(t_test_success$statistic, 2)))
    print(paste("p-value:", format(t_test_success$p.value, scientific = TRUE)))
  }
}

# Time to close analysis
if(sum(!is.na(df$time_to_close_days)) > 30) {
  cor_time <- cor.test(df$ma_due_diligence_score,
                      df$time_to_close_days,
                      use = "complete.obs",
                      method = "spearman")
  
  print("\nSpearman correlation with time to close:")
  print(paste("rho =", round(cor_time$estimate, 3)))
  print(paste("p-value:", format(cor_time$p.value, scientific = TRUE)))
  
  # Categorize time to close
  df <- df %>%
    mutate(time_category = case_when(
      time_to_close_days <= 90 ~ "Fast (≤90 days)",
      time_to_close_days <= 180 ~ "Standard (91-180 days)",
      time_to_close_days <= 365 ~ "Extended (181-365 days)",
      TRUE ~ "Prolonged (>365 days)"
    ))
  
  time_stats <- df %>%
    filter(!is.na(time_category)) %>%
    group_by(time_category) %>%
    summarise(
      n = n(),
      mean_dd = round(mean(ma_due_diligence_score, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nDue diligence score by time to close:")
  print(time_stats)
}

# Industry analysis
if(length(unique(df$target_industry_sector)) > 3) {
  industry_stats <- df %>%
    filter(!is.na(target_industry_sector)) %>%
    group_by(target_industry_sector) %>%
    summarise(
      n = n(),
      mean_dd = round(mean(ma_due_diligence_score, na.rm = TRUE), 2),
      sd_dd = round(sd(ma_due_diligence_score, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_dd))
  
  print("\nTop industries by due diligence score:")
  print(head(industry_stats, 10))
}

# Multiple regression model
predictors <- c("strategic_rationale_score", "target_financial_health_score",
               "cultural_fit_assessment", "synergy_potential_score",
               "integration_complexity_rating")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("ma_due_diligence_score ~", 
                      paste(available_predictors, collapse = " + "))
  
  lm_multiple <- lm(as.formula(formula_str), data = df)
  
  print("\nMultiple regression model:")
  print(summary(lm_multiple))
  
  # Standardized coefficients
  df_std <- df %>%
    select(ma_due_diligence_score, all_of(available_predictors)) %>%
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
    mean_dd = round(mean(ma_due_diligence_score, na.rm = TRUE), 2),
    sd_dd = round(sd(ma_due_diligence_score, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_dd))

print("\nDue diligence score by stakeholder:")
print(stakeholder_stats)

# Outlier analysis
z_scores <- abs(scale(df$ma_due_diligence_score))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

# Robust statistics
mad_value <- mad(df$ma_due_diligence_score, na.rm = TRUE)
trimmed_mean <- mean(df$ma_due_diligence_score, trim = 0.1, na.rm = TRUE)
winsorized_mean <- mean(Winsorize(df$ma_due_diligence_score, probs = c(0.05, 0.95)))

print(paste("\nMedian Absolute Deviation (MAD):", round(mad_value, 3)))
print(paste("10% Trimmed mean:", round(trimmed_mean, 2)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 2)))

# Response distribution
freq_table <- table(round(df$ma_due_diligence_score, 1))
print("\nFrequency distribution (rounded to 0.1):")
print(freq_table)

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("M&A due diligence score: M =", round(mean_score, 2), 
           ", SD =", round(sd_score, 2)))
print(paste("N =", n_total))
print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("The high mean of", round(mean_score, 2), 
           "indicates thorough due diligence practices"))
print(paste("with relatively low variability (SD =", round(sd_score, 2), ")"))

# Expected: due diligence score (M = 4.12, SD = 0.76)