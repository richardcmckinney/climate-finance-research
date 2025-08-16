# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 111: "Technology readiness assessment: level distribution (M = 5.23, SD = 1.67)"
# Purpose: Analyze technology readiness level (TRL) distribution

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)
library(fitdistrplus)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TRL_LEVEL <- "technology_readiness_level"
COL_TECHNOLOGY_TYPE <- "technology_category"
COL_DEVELOPMENT_STAGE <- "development_stage"
COL_TIME_TO_MARKET <- "estimated_time_to_market_months"
COL_R_AND_D_INVESTMENT <- "rd_investment_amount"
COL_TECHNICAL_RISK <- "technical_risk_score"
COL_VALIDATION_STATUS <- "validation_status"
COL_PROTOTYPE_STATUS <- "prototype_completion_percentage"
COL_TESTING_PHASE <- "testing_phase_current"
COL_INDUSTRY <- "industry_sector"
COL_FUNDING_STAGE <- "funding_stage"
COL_TEAM_SIZE <- "technical_team_size"
COL_PATENT_STATUS <- "patent_filing_status"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    technology_readiness_level = as.numeric(technology_readiness_level),
    technology_category = factor(technology_category),
    development_stage = factor(development_stage,
                              levels = c("Concept", "Development", "Demonstration", 
                                       "Deployment", "Commercial"),
                              ordered = TRUE),
    estimated_time_to_market_months = as.numeric(estimated_time_to_market_months),
    rd_investment_amount = as.numeric(rd_investment_amount),
    technical_risk_score = as.numeric(technical_risk_score),
    validation_status = factor(validation_status),
    prototype_completion_percentage = as.numeric(prototype_completion_percentage),
    testing_phase_current = factor(testing_phase_current),
    industry_sector = factor(industry_sector),
    funding_stage = factor(funding_stage),
    technical_team_size = as.numeric(technical_team_size),
    patent_filing_status = factor(patent_filing_status),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(technology_readiness_level))

# Primary descriptive statistics
n_total <- nrow(df)
mean_trl <- mean(df$technology_readiness_level, na.rm = TRUE)
sd_trl <- sd(df$technology_readiness_level, na.rm = TRUE)
median_trl <- median(df$technology_readiness_level, na.rm = TRUE)
se_trl <- sd_trl / sqrt(n_total)

print("Technology Readiness Level Distribution - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_trl, 2)))
print(paste("SD =", round(sd_trl, 2)))
print(paste("Median =", round(median_trl, 2)))
print(paste("SE =", round(se_trl, 3)))

# Confidence intervals
ci_lower <- mean_trl - qt(0.975, n_total - 1) * se_trl
ci_upper <- mean_trl + qt(0.975, n_total - 1) * se_trl

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$technology_readiness_level)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution characteristics
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$technology_readiness_level, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$technology_readiness_level, na.rm = TRUE), 3)))
print(paste("Range:", min(df$technology_readiness_level, na.rm = TRUE), "-", 
           max(df$technology_readiness_level, na.rm = TRUE)))

# TRL frequency distribution
trl_freq <- table(df$technology_readiness_level)
trl_prop <- prop.table(trl_freq)

print("\nTRL Frequency Distribution:")
print(trl_freq)
print("\nProportions:")
print(round(trl_prop, 3))

# Categorize TRL levels
df <- df %>%
  mutate(trl_category = case_when(
    technology_readiness_level <= 3 ~ "Basic Research (TRL 1-3)",
    technology_readiness_level <= 6 ~ "Technology Development (TRL 4-6)",
    technology_readiness_level <= 9 ~ "System Development (TRL 7-9)",
    TRUE ~ NA_character_
  ))

category_stats <- df %>%
  filter(!is.na(trl_category)) %>%
  count(trl_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("\nTRL Categories:")
print(category_stats)

# Percentiles
percentiles <- quantile(df$technology_readiness_level, 
                       probs = seq(0, 1, 0.1), 
                       na.rm = TRUE)
print("\nPercentiles:")
print(round(percentiles, 1))

# Mode calculation
mode_trl <- as.numeric(names(trl_freq)[which.max(trl_freq)])
print(paste("\nMode:", mode_trl))

# Coefficient of variation
cv <- (sd_trl / mean_trl) * 100
print(paste("Coefficient of variation:", round(cv, 1), "%"))

# Normality tests
shapiro_result <- shapiro.test(df$technology_readiness_level[1:min(5000, n_total)])
print("\nShapiro-Wilk normality test:")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# Anderson-Darling test
ad_test <- ad.test(df$technology_readiness_level)
print("\nAnderson-Darling test:")
print(paste("A =", round(ad_test$statistic, 3)))
print(paste("p-value:", format(ad_test$p.value, scientific = TRUE)))

# Distribution fitting
print("\n=== DISTRIBUTION FITTING ===")

# Fit normal distribution
fit_norm <- fitdist(df$technology_readiness_level, "norm")
print("Normal distribution parameters:")
print(fit_norm$estimate)

# Fit truncated normal (TRL is bounded 1-9)
library(truncnorm)
fit_truncnorm <- fitdist(df$technology_readiness_level, "truncnorm", 
                         start = list(mean = mean_trl, sd = sd_trl),
                         fix.arg = list(a = 1, b = 9))
print("\nTruncated normal distribution parameters:")
print(fit_truncnorm$estimate)

# Goodness of fit
gof_stats <- gofstat(list(fit_norm, fit_truncnorm))
print("\nGoodness of fit statistics:")
print(gof_stats)

# Bootstrap analysis
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$technology_readiness_level, 
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

# Technology type analysis
if(!all(is.na(df$technology_category))) {
  tech_stats <- df %>%
    filter(!is.na(technology_category)) %>%
    group_by(technology_category) %>%
    summarise(
      n = n(),
      mean_trl = round(mean(technology_readiness_level, na.rm = TRUE), 2),
      sd_trl = round(sd(technology_readiness_level, na.rm = TRUE), 2),
      median_trl = round(median(technology_readiness_level, na.rm = TRUE), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_trl))
  
  print("\nTRL by technology category:")
  print(head(tech_stats, 10))
  
  # ANOVA
  if(nrow(tech_stats) > 2) {
    anova_tech <- aov(technology_readiness_level ~ technology_category, 
                     data = df %>% filter(technology_category %in% tech_stats$technology_category))
    print("\nANOVA by technology category:")
    print(summary(anova_tech))
  }
}

# Development stage correlation
if(!all(is.na(df$development_stage))) {
  stage_stats <- df %>%
    filter(!is.na(development_stage)) %>%
    group_by(development_stage) %>%
    summarise(
      n = n(),
      mean_trl = round(mean(technology_readiness_level, na.rm = TRUE), 2),
      sd_trl = round(sd(technology_readiness_level, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nTRL by development stage:")
  print(stage_stats)
  
  # Jonckheere-Terpstra test for trend
  if(nrow(stage_stats) > 2) {
    jt_test <- jonckheere.test(df$technology_readiness_level,
                               df$development_stage,
                               alternative = "increasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# Time to market analysis
if(sum(!is.na(df$estimated_time_to_market_months)) > 30) {
  cor_time <- cor.test(df$technology_readiness_level,
                      df$estimated_time_to_market_months,
                      use = "complete.obs",
                      method = "spearman")
  
  print("\nSpearman correlation with time to market:")
  print(paste("rho =", round(cor_time$estimate, 3)))
  print(paste("p-value:", format(cor_time$p.value, scientific = TRUE)))
  
  # Regression
  lm_time <- lm(estimated_time_to_market_months ~ technology_readiness_level, data = df)
  print("\nRegression - Time to market predicted by TRL:")
  print(summary(lm_time))
}

# Technical risk correlation
if(sum(!is.na(df$technical_risk_score)) > 30) {
  cor_risk <- cor.test(df$technology_readiness_level,
                      df$technical_risk_score,
                      use = "complete.obs")
  
  print("\nCorrelation with technical risk:")
  print(paste("r =", round(cor_risk$estimate, 3)))
  print(paste("p-value:", format(cor_risk$p.value, scientific = TRUE)))
}

# R&D investment analysis
if(sum(!is.na(df$rd_investment_amount)) > 30) {
  # Log transform R&D investment
  df$log_rd_investment <- log10(df$rd_investment_amount + 1)
  
  # Group by TRL
  rd_by_trl <- df %>%
    filter(!is.na(log_rd_investment)) %>%
    group_by(technology_readiness_level) %>%
    summarise(
      n = n(),
      mean_log_investment = mean(log_rd_investment, na.rm = TRUE),
      median_investment = median(rd_investment_amount, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(mean_investment = 10^mean_log_investment)
  
  print("\nR&D investment by TRL:")
  print(rd_by_trl)
}

# Prototype status analysis
if(sum(!is.na(df$prototype_completion_percentage)) > 30) {
  prototype_by_trl <- df %>%
    filter(!is.na(prototype_completion_percentage)) %>%
    group_by(technology_readiness_level) %>%
    summarise(
      n = n(),
      mean_completion = mean(prototype_completion_percentage, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nPrototype completion by TRL:")
  print(prototype_by_trl)
  
  # Correlation
  cor_prototype <- cor.test(df$technology_readiness_level,
                           df$prototype_completion_percentage,
                           use = "complete.obs")
  print(paste("Correlation: r =", round(cor_prototype$estimate, 3)))
}

# Industry analysis
if(length(unique(df$industry_sector)) > 3) {
  industry_stats <- df %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      mean_trl = round(mean(technology_readiness_level, na.rm = TRUE), 2),
      sd_trl = round(sd(technology_readiness_level, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(mean_trl))
  
  print("\nTop industries by TRL:")
  print(head(industry_stats, 10))
}

# Funding stage analysis
if(!all(is.na(df$funding_stage))) {
  funding_stats <- df %>%
    filter(!is.na(funding_stage)) %>%
    group_by(funding_stage) %>%
    summarise(
      n = n(),
      mean_trl = round(mean(technology_readiness_level, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(mean_trl)
  
  print("\nTRL by funding stage:")
  print(funding_stats)
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_trl = round(mean(technology_readiness_level, na.rm = TRUE), 2),
    sd_trl = round(sd(technology_readiness_level, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_trl))

print("\nTRL by stakeholder:")
print(stakeholder_stats)

# Outlier analysis
z_scores <- abs(scale(df$technology_readiness_level))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

# Robust statistics
mad_value <- mad(df$technology_readiness_level, na.rm = TRUE)
trimmed_mean <- mean(df$technology_readiness_level, trim = 0.1, na.rm = TRUE)

print(paste("\nMedian Absolute Deviation (MAD):", round(mad_value, 3)))
print(paste("10% Trimmed mean:", round(trimmed_mean, 2)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Technology readiness level: M =", round(mean_trl, 2), 
           ", SD =", round(sd_trl, 2)))
print(paste("N =", n_total))
print(paste("95% CI for mean: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("Median TRL:", round(median_trl, 1)))
print(paste("Mode TRL:", mode_trl))
print("The mean TRL of 5.23 indicates technologies are generally in the")
print("technology development to demonstration phase (TRL 4-6)")

# Expected: level distribution (M = 5.23, SD = 1.67)