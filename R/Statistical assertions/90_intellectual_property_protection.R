# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 90: "Intellectual property protection: importance rating (M = 4.23, SD = 0.71)"
# Purpose: Analyze intellectual property protection importance ratings

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_IP_IMPORTANCE <- "ip_protection_importance"
COL_IP_STRATEGY <- "ip_strategy_sophistication"
COL_PATENT_COUNT <- "patent_portfolio_size"
COL_TRADEMARK_COUNT <- "trademark_count"
COL_TRADE_SECRET <- "trade_secret_reliance"
COL_COPYRIGHT <- "copyright_importance"
COL_IP_BUDGET <- "ip_budget_percentage"
COL_IP_LITIGATION <- "ip_litigation_experience"
COL_INDUSTRY <- "industry_sector"
COL_COMPANY_STAGE <- "company_stage"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    ip_protection_importance = as.numeric(ip_protection_importance),
    ip_strategy_sophistication = as.numeric(ip_strategy_sophistication),
    patent_portfolio_size = as.numeric(patent_portfolio_size),
    trademark_count = as.numeric(trademark_count),
    trade_secret_reliance = as.numeric(trade_secret_reliance),
    copyright_importance = as.numeric(copyright_importance),
    ip_budget_percentage = as.numeric(ip_budget_percentage),
    ip_litigation_experience = factor(ip_litigation_experience),
    industry_sector = factor(industry_sector),
    company_stage = factor(company_stage,
                           levels = c("Pre-seed", "Seed", "Series A", "Series B", 
                                    "Series C+", "IPO/Public"),
                           ordered = TRUE),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(ip_protection_importance))

# Primary descriptive statistics
n_total <- nrow(df)
mean_importance <- mean(df$ip_protection_importance, na.rm = TRUE)
sd_importance <- sd(df$ip_protection_importance, na.rm = TRUE)
median_importance <- median(df$ip_protection_importance, na.rm = TRUE)
se_importance <- sd_importance / sqrt(n_total)

print("Intellectual Property Protection Importance - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_importance, 2)))
print(paste("SD =", round(sd_importance, 2)))
print(paste("Median =", round(median_importance, 2)))
print(paste("SE =", round(se_importance, 3)))

# Confidence intervals
ci_lower <- mean_importance - qt(0.975, n_total - 1) * se_importance
ci_upper <- mean_importance + qt(0.975, n_total - 1) * se_importance

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$ip_protection_importance)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution analysis
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$ip_protection_importance, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$ip_protection_importance, na.rm = TRUE), 3)))

# Quartiles and percentiles
percentiles <- quantile(df$ip_protection_importance, 
                       probs = seq(0, 1, 0.1), 
                       na.rm = TRUE)
print("\nPercentiles:")
print(round(percentiles, 2))

# Mode and frequency distribution
freq_table <- table(df$ip_protection_importance)
mode_value <- as.numeric(names(freq_table)[which.max(freq_table)])
print(paste("\nMode:", mode_value))
print("\nFrequency distribution:")
print(freq_table)
print("\nProportions:")
print(round(prop.table(freq_table), 3))

# Coefficient of variation
cv <- (sd_importance / mean_importance) * 100
print(paste("\nCoefficient of variation:", round(cv, 1), "%"))

# Normality tests
shapiro_result <- shapiro.test(df$ip_protection_importance[1:min(5000, n_total)])
print("\nNormality test (Shapiro-Wilk):")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint (assuming 1-5 scale)
scale_midpoint <- 3
t_test_midpoint <- t.test(df$ip_protection_importance, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_importance - scale_midpoint, 2)))

# Effect size for one-sample t-test
cohens_d_one <- cohens_d(df$ip_protection_importance, mu = scale_midpoint)
print(paste("Cohen's d:", round(cohens_d_one$Cohens_d, 3)))

# Bootstrap confidence intervals
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$ip_protection_importance, 
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

# Correlation with IP strategy sophistication
if(sum(!is.na(df$ip_strategy_sophistication)) > 30) {
  cor_strategy <- cor.test(df$ip_protection_importance,
                          df$ip_strategy_sophistication,
                          use = "complete.obs")
  
  print("\nCorrelation with IP strategy sophistication:")
  print(paste("r =", round(cor_strategy$estimate, 3)))
  print(paste("95% CI: [", round(cor_strategy$conf.int[1], 3), ",", 
             round(cor_strategy$conf.int[2], 3), "]"))
  print(paste("p-value:", format(cor_strategy$p.value, scientific = TRUE)))
}

# Analysis by patent portfolio size
if(sum(!is.na(df$patent_portfolio_size)) > 30) {
  # Create patent portfolio categories
  df <- df %>%
    mutate(patent_category = case_when(
      patent_portfolio_size == 0 ~ "No patents",
      patent_portfolio_size <= 5 ~ "1-5 patents",
      patent_portfolio_size <= 20 ~ "6-20 patents",
      patent_portfolio_size > 20 ~ "20+ patents",
      TRUE ~ NA_character_
    ))
  
  patent_stats <- df %>%
    filter(!is.na(patent_category)) %>%
    group_by(patent_category) %>%
    summarise(
      n = n(),
      mean_importance = round(mean(ip_protection_importance, na.rm = TRUE), 2),
      sd_importance = round(sd(ip_protection_importance, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nIP importance by patent portfolio size:")
  print(patent_stats)
  
  # Correlation with continuous patent count
  cor_patents <- cor.test(df$ip_protection_importance,
                          df$patent_portfolio_size,
                          use = "complete.obs",
                          method = "spearman")
  
  print("\nSpearman correlation with patent count:")
  print(paste("rho =", round(cor_patents$estimate, 3)))
  print(paste("p-value:", format(cor_patents$p.value, scientific = TRUE)))
}

# Analysis by industry sector
if(!all(is.na(df$industry_sector))) {
  industry_stats <- df %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      mean_importance = round(mean(ip_protection_importance, na.rm = TRUE), 2),
      sd_importance = round(sd(ip_protection_importance, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_importance))
  
  print("\nTop industries by IP protection importance:")
  print(head(industry_stats, 10))
  
  # ANOVA by industry
  if(nrow(industry_stats) > 2) {
    anova_industry <- aov(ip_protection_importance ~ industry_sector, 
                         data = df %>% filter(industry_sector %in% industry_stats$industry_sector))
    print("\nANOVA by industry sector:")
    print(summary(anova_industry))
    
    # Effect size
    eta_sq <- eta_squared(anova_industry)
    print(paste("Eta-squared:", round(eta_sq$Eta2, 3)))
  }
}

# Analysis by company stage
if(!all(is.na(df$company_stage))) {
  stage_stats <- df %>%
    filter(!is.na(company_stage)) %>%
    group_by(company_stage) %>%
    summarise(
      n = n(),
      mean_importance = round(mean(ip_protection_importance, na.rm = TRUE), 2),
      sd_importance = round(sd(ip_protection_importance, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nIP importance by company stage:")
  print(stage_stats)
  
  # Jonckheere-Terpstra test for ordered trend
  if(length(unique(df$company_stage)) > 2) {
    jt_test <- jonckheere.test(df$ip_protection_importance, 
                               df$company_stage, 
                               alternative = "increasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT statistic =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# Trade secret reliance analysis
if(sum(!is.na(df$trade_secret_reliance)) > 30) {
  cor_trade_secret <- cor.test(df$ip_protection_importance,
                               df$trade_secret_reliance,
                               use = "complete.obs")
  
  print("\nCorrelation with trade secret reliance:")
  print(paste("r =", round(cor_trade_secret$estimate, 3)))
  print(paste("p-value:", format(cor_trade_secret$p.value, scientific = TRUE)))
}

# IP budget analysis
if(sum(!is.na(df$ip_budget_percentage)) > 30) {
  cor_budget <- cor.test(df$ip_protection_importance,
                        df$ip_budget_percentage,
                        use = "complete.obs")
  
  print("\nCorrelation with IP budget percentage:")
  print(paste("r =", round(cor_budget$estimate, 3)))
  print(paste("p-value:", format(cor_budget$p.value, scientific = TRUE)))
  
  # Regression analysis
  lm_budget <- lm(ip_budget_percentage ~ ip_protection_importance, data = df)
  print("\nRegression - IP budget predicted by importance:")
  print(summary(lm_budget))
}

# IP litigation experience analysis
if(!all(is.na(df$ip_litigation_experience))) {
  litigation_stats <- df %>%
    filter(!is.na(ip_litigation_experience)) %>%
    group_by(ip_litigation_experience) %>%
    summarise(
      n = n(),
      mean_importance = round(mean(ip_protection_importance, na.rm = TRUE), 2),
      sd_importance = round(sd(ip_protection_importance, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nIP importance by litigation experience:")
  print(litigation_stats)
  
  # t-test if binary
  if(length(unique(df$ip_litigation_experience)) == 2) {
    t_test_litigation <- t.test(ip_protection_importance ~ ip_litigation_experience, 
                                data = df)
    print("\nt-test by litigation experience:")
    print(paste("t =", round(t_test_litigation$statistic, 2)))
    print(paste("p-value:", format(t_test_litigation$p.value, scientific = TRUE)))
  }
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_importance = round(mean(ip_protection_importance, na.rm = TRUE), 2),
    sd_importance = round(sd(ip_protection_importance, na.rm = TRUE), 2),
    se_importance = round(sd_importance / sqrt(n), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_importance))

print("\nIP protection importance by stakeholder:")
print(stakeholder_stats)

# ANOVA by stakeholder
if(length(unique(df$stakeholder)) > 2) {
  anova_stakeholder <- aov(ip_protection_importance ~ stakeholder, data = df)
  print("\nANOVA by stakeholder:")
  print(summary(anova_stakeholder))
  
  # Post-hoc tests if significant
  if(summary(anova_stakeholder)[[1]][["Pr(>F)"]][1] < 0.05) {
    tukey_result <- TukeyHSD(anova_stakeholder)
    print("\nTukey HSD post-hoc test:")
    print(tukey_result)
  }
}

# Outlier analysis
z_scores <- abs(scale(df$ip_protection_importance))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

# Robust statistics
mad_value <- mad(df$ip_protection_importance, na.rm = TRUE)
print(paste("Median Absolute Deviation (MAD):", round(mad_value, 3)))

# Trimmed mean (10% trimming)
trimmed_mean <- mean(df$ip_protection_importance, trim = 0.1, na.rm = TRUE)
print(paste("10% Trimmed mean:", round(trimmed_mean, 2)))

# Winsorized statistics
winsorized_mean <- mean(Winsorize(df$ip_protection_importance, probs = c(0.05, 0.95)))
winsorized_sd <- sd(Winsorize(df$ip_protection_importance, probs = c(0.05, 0.95)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 2)))
print(paste("5% Winsorized SD:", round(winsorized_sd, 2)))

# Response pattern analysis
response_pattern <- df %>%
  count(ip_protection_importance) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("\nResponse distribution:")
print(response_pattern)

# Ceiling/floor effects
ceiling_threshold <- max(df$ip_protection_importance, na.rm = TRUE)
floor_threshold <- min(df$ip_protection_importance, na.rm = TRUE)

ceiling_effect <- sum(df$ip_protection_importance == ceiling_threshold, na.rm = TRUE) / n_total * 100
floor_effect <- sum(df$ip_protection_importance == floor_threshold, na.rm = TRUE) / n_total * 100

print(paste("\nCeiling effect (% at maximum):", round(ceiling_effect, 1), "%"))
print(paste("Floor effect (% at minimum):", round(floor_effect, 1), "%"))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Intellectual property protection importance: M =", round(mean_importance, 2), 
           ", SD =", round(sd_importance, 2)))
print(paste("N =", n_total))
print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("The high mean rating of", round(mean_importance, 2), 
           "indicates strong perceived importance of IP protection"))
print(paste("Low SD of", round(sd_importance, 2), "suggests consensus across respondents"))

# Expected: importance rating (M = 4.23, SD = 0.71)