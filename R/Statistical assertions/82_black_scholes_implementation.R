# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 82: "Black-Scholes implementation: volatility estimates σ = 0.45 (SD = 0.12)"
# Purpose: Analyze Black-Scholes model implementation and volatility parameters

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_BLACK_SCHOLES_USED <- "black_scholes_model_used"
COL_VOLATILITY_ESTIMATE <- "volatility_estimate_sigma"
COL_RISK_FREE_RATE <- "risk_free_rate_used"
COL_TIME_TO_MATURITY <- "time_to_maturity_years"
COL_IMPLIED_VS_HISTORICAL <- "volatility_type"
COL_STAKEHOLDER <- "stakeholder"
COL_OPTION_TYPE <- "option_type_valued"
COL_CALIBRATION_FREQUENCY <- "model_calibration_frequency"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    black_scholes_model_used = as.integer(black_scholes_model_used),
    volatility_estimate_sigma = as.numeric(volatility_estimate_sigma),
    risk_free_rate_used = as.numeric(risk_free_rate_used),
    time_to_maturity_years = as.numeric(time_to_maturity_years),
    volatility_type = factor(volatility_type),
    stakeholder = factor(stakeholder),
    option_type_valued = factor(option_type_valued),
    model_calibration_frequency = factor(model_calibration_frequency)
  ) %>%
  filter(!is.na(volatility_estimate_sigma), volatility_estimate_sigma > 0)

# Volatility estimate statistics
n_total <- nrow(df)
volatility_stats <- df %>%
  summarise(
    n = n(),
    mean_sigma = round(mean(volatility_estimate_sigma), 3),
    sd_sigma = round(sd(volatility_estimate_sigma), 3),
    median_sigma = round(median(volatility_estimate_sigma), 3),
    q25_sigma = round(quantile(volatility_estimate_sigma, 0.25), 3),
    q75_sigma = round(quantile(volatility_estimate_sigma, 0.75), 3),
    min_sigma = round(min(volatility_estimate_sigma), 3),
    max_sigma = round(max(volatility_estimate_sigma), 3)
  )

print("Black-Scholes volatility estimates (σ):")
print(volatility_stats)
print(paste("Mean σ:", volatility_stats$mean_sigma))
print(paste("SD of σ:", volatility_stats$sd_sigma))

# 95% CI for mean volatility
ci_mean <- MeanCI(df$volatility_estimate_sigma, conf.level = 0.95)
print(paste("95% CI for mean σ: [", round(ci_mean[2], 3), ", ", 
            round(ci_mean[3], 3), "]", sep=""))

# 95% CI for standard deviation using bootstrap
set.seed(123)
boot_sd <- boot(df$volatility_estimate_sigma, 
                function(x, i) sd(x[i]), 
                R = 1000)
ci_sd <- boot.ci(boot_sd, type = "perc")$percent[4:5]
print(paste("95% Bootstrap CI for SD: [", round(ci_sd[1], 3), ", ", 
            round(ci_sd[2], 3), "]", sep=""))

# Volatility distribution categories
volatility_categories <- df %>%
  mutate(
    volatility_category = case_when(
      volatility_estimate_sigma < 0.2 ~ "Very Low (<0.2)",
      volatility_estimate_sigma < 0.35 ~ "Low (0.2-0.35)",
      volatility_estimate_sigma < 0.5 ~ "Moderate (0.35-0.5)",
      volatility_estimate_sigma < 0.65 ~ "High (0.5-0.65)",
      volatility_estimate_sigma >= 0.65 ~ "Very High (≥0.65)"
    )
  ) %>%
  group_by(volatility_category) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    mean_sigma = round(mean(volatility_estimate_sigma), 3),
    .groups = "drop"
  ) %>%
  mutate(
    volatility_category = factor(volatility_category,
                                levels = c("Very Low (<0.2)", "Low (0.2-0.35)",
                                         "Moderate (0.35-0.5)", "High (0.5-0.65)",
                                         "Very High (≥0.65)"))
  ) %>%
  arrange(volatility_category)

print("\nVolatility distribution categories:")
print(volatility_categories)

# Black-Scholes usage analysis
if(!all(is.na(df$black_scholes_model_used))) {
  bs_usage <- df %>%
    filter(!is.na(black_scholes_model_used)) %>%
    summarise(
      n = n(),
      using_bs = sum(black_scholes_model_used),
      usage_pct = round(mean(black_scholes_model_used) * 100, 1)
    )
  
  print("\nBlack-Scholes model usage:")
  print(bs_usage)
  
  # Compare volatility estimates between BS users and non-users
  volatility_by_usage <- df %>%
    filter(!is.na(black_scholes_model_used)) %>%
    mutate(
      bs_status = ifelse(black_scholes_model_used == 1,
                        "Uses Black-Scholes", "Other Methods")
    ) %>%
    group_by(bs_status) %>%
    summarise(
      n = n(),
      mean_sigma = round(mean(volatility_estimate_sigma), 3),
      sd_sigma = round(sd(volatility_estimate_sigma), 3),
      median_sigma = round(median(volatility_estimate_sigma), 3),
      .groups = "drop"
    )
  
  print("\nVolatility by Black-Scholes usage:")
  print(volatility_by_usage)
  
  # t-test for differences
  if(nrow(volatility_by_usage) == 2) {
    bs_volatility <- df$volatility_estimate_sigma[df$black_scholes_model_used == 1 & 
                                                  !is.na(df$black_scholes_model_used)]
    other_volatility <- df$volatility_estimate_sigma[df$black_scholes_model_used == 0 & 
                                                    !is.na(df$black_scholes_model_used)]
    
    if(length(bs_volatility) >= 20 && length(other_volatility) >= 20) {
      t_test <- t.test(bs_volatility, other_volatility)
      
      print("Volatility comparison (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 3)))
    }
  }
}

# Risk-free rate analysis
if(sum(!is.na(df$risk_free_rate_used)) > 50) {
  rf_stats <- df %>%
    filter(!is.na(risk_free_rate_used)) %>%
    summarise(
      n = n(),
      mean_rf = round(mean(risk_free_rate_used), 3),
      sd_rf = round(sd(risk_free_rate_used), 3),
      median_rf = round(median(risk_free_rate_used), 3),
      q25_rf = round(quantile(risk_free_rate_used, 0.25), 3),
      q75_rf = round(quantile(risk_free_rate_used, 0.75), 3)
    )
  
  print("\nRisk-free rate statistics:")
  print(rf_stats)
  
  # Correlation between volatility and risk-free rate
  cor_rf <- cor.test(df$volatility_estimate_sigma[!is.na(df$risk_free_rate_used)],
                     df$risk_free_rate_used[!is.na(df$risk_free_rate_used)])
  
  print("Volatility × Risk-free rate correlation:")
  print(paste("r =", round(cor_rf$estimate, 3)))
  print(paste("95% CI: [", round(cor_rf$conf.int[1], 3), ", ", 
              round(cor_rf$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_rf$p.value, scientific = TRUE)))
}

# Time to maturity analysis
if(sum(!is.na(df$time_to_maturity_years)) > 50) {
  maturity_stats <- df %>%
    filter(!is.na(time_to_maturity_years)) %>%
    summarise(
      n = n(),
      mean_maturity = round(mean(time_to_maturity_years), 1),
      sd_maturity = round(sd(time_to_maturity_years), 1),
      median_maturity = round(median(time_to_maturity_years), 1)
    )
  
  print("\nTime to maturity statistics:")
  print(maturity_stats)
  
  # Correlation between volatility and time to maturity
  cor_maturity <- cor.test(df$volatility_estimate_sigma[!is.na(df$time_to_maturity_years)],
                           df$time_to_maturity_years[!is.na(df$time_to_maturity_years)])
  
  print("Volatility × Time to maturity correlation:")
  print(paste("r =", round(cor_maturity$estimate, 3)))
  print(paste("p-value:", format(cor_maturity$p.value, scientific = TRUE)))
}

# Implied vs Historical volatility
if(!all(is.na(df$volatility_type))) {
  volatility_type_analysis <- df %>%
    filter(!is.na(volatility_type)) %>%
    group_by(volatility_type) %>%
    summarise(
      n = n(),
      mean_sigma = round(mean(volatility_estimate_sigma), 3),
      sd_sigma = round(sd(volatility_estimate_sigma), 3),
      percentage = round(n() / sum(!is.na(df$volatility_type)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_sigma))
  
  print("\nVolatility by type (implied vs historical):")
  print(volatility_type_analysis)
  
  # t-test if two types
  if(nrow(volatility_type_analysis) == 2) {
    types <- unique(df$volatility_type[!is.na(df$volatility_type)])
    type1_vol <- df$volatility_estimate_sigma[df$volatility_type == types[1] & 
                                             !is.na(df$volatility_type)]
    type2_vol <- df$volatility_estimate_sigma[df$volatility_type == types[2] & 
                                             !is.na(df$volatility_type)]
    
    if(length(type1_vol) >= 20 && length(type2_vol) >= 20) {
      t_test_type <- t.test(type1_vol, type2_vol)
      print("Volatility type comparison (t-test):")
      print(paste("t =", round(t_test_type$statistic, 2)))
      print(paste("p-value:", format(t_test_type$p.value, scientific = TRUE)))
    }
  }
}

# Analysis by stakeholder
stakeholder_volatility <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_sigma = round(mean(volatility_estimate_sigma), 3),
    sd_sigma = round(sd(volatility_estimate_sigma), 3),
    median_sigma = round(median(volatility_estimate_sigma), 3),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(mean_sigma))

print("\nVolatility estimates by stakeholder (n ≥ 20):")
print(stakeholder_volatility)

# ANOVA for stakeholder differences
if(length(unique(df$stakeholder)) > 2) {
  anova_stakeholder <- aov(volatility_estimate_sigma ~ stakeholder, data = df)
  anova_summary <- summary(anova_stakeholder)
  
  print("\nANOVA - Volatility by stakeholder:")
  print(anova_summary)
  
  # Post-hoc if significant
  if(anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
    tukey_test <- TukeyHSD(anova_stakeholder)
    print("Tukey's HSD post-hoc comparisons:")
    print(tukey_test)
    
    # Effect size
    ss_total <- sum(anova_summary[[1]][["Sum Sq"]])
    ss_effect <- anova_summary[[1]][["Sum Sq"]][1]
    eta_squared <- ss_effect / ss_total
    print(paste("Eta-squared:", round(eta_squared, 3)))
  }
}

# Option type analysis
if(!all(is.na(df$option_type_valued))) {
  option_type_volatility <- df %>%
    filter(!is.na(option_type_valued)) %>%
    group_by(option_type_valued) %>%
    summarise(
      n = n(),
      mean_sigma = round(mean(volatility_estimate_sigma), 3),
      sd_sigma = round(sd(volatility_estimate_sigma), 3),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_sigma))
  
  print("\nVolatility by option type (n ≥ 10):")
  print(option_type_volatility)
}

# Calibration frequency analysis
if(!all(is.na(df$model_calibration_frequency))) {
  calibration_analysis <- df %>%
    filter(!is.na(model_calibration_frequency)) %>%
    group_by(model_calibration_frequency) %>%
    summarise(
      n = n(),
      mean_sigma = round(mean(volatility_estimate_sigma), 3),
      sd_sigma = round(sd(volatility_estimate_sigma), 3),
      .groups = "drop"
    ) %>%
    arrange(model_calibration_frequency)
  
  print("\nVolatility by calibration frequency:")
  print(calibration_analysis)
}

# Multiple regression model
if(nrow(df) > 150) {
  df_regression <- df
  
  # Build model with available predictors
  predictors <- c()
  
  if(sum(!is.na(df_regression$risk_free_rate_used)) > 100) {
    predictors <- c(predictors, "risk_free_rate_used")
  }
  
  if(sum(!is.na(df_regression$time_to_maturity_years)) > 100) {
    predictors <- c(predictors, "time_to_maturity_years")
  }
  
  if(length(unique(df_regression$stakeholder)) > 1) {
    predictors <- c(predictors, "stakeholder")
  }
  
  if(length(predictors) > 0 && nrow(df_regression) > 100) {
    formula_str <- paste("volatility_estimate_sigma ~", 
                        paste(predictors, collapse = " + "))
    lm_model <- lm(as.formula(formula_str), data = df_regression)
    
    lm_summary <- summary(lm_model)
    print("\nMultiple regression: Volatility estimate predictors")
    print(lm_summary)
    
    # Confidence intervals for coefficients
    coef_ci <- confint(lm_model)
    print("\n95% CIs for coefficients:")
    print(coef_ci)
    
    # Model diagnostics
    print("\nModel diagnostics:")
    print(paste("R-squared:", round(lm_summary$r.squared, 3)))
    print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
  }
}

# Distribution tests
# Shapiro-Wilk test for normality
if(nrow(df) <= 5000) {
  shapiro_test <- shapiro.test(df$volatility_estimate_sigma)
  print("\nShapiro-Wilk normality test for volatility:")
  print(paste("W =", round(shapiro_test$statistic, 4)))
  print(paste("p-value:", format(shapiro_test$p.value, scientific = TRUE)))
}

# Skewness and kurtosis
skewness_val <- e1071::skewness(df$volatility_estimate_sigma)
kurtosis_val <- e1071::kurtosis(df$volatility_estimate_sigma)

print("\nDistribution shape for volatility:")
print(paste("Skewness:", round(skewness_val, 2)))
print(paste("Kurtosis:", round(kurtosis_val, 2)))

# Outlier analysis
outliers <- boxplot.stats(df$volatility_estimate_sigma)$out
n_outliers <- length(outliers)
pct_outliers <- round(n_outliers / nrow(df) * 100, 1)

print("\nOutlier analysis for volatility:")
print(paste("Number of outliers:", n_outliers))
print(paste("Percentage of outliers:", pct_outliers, "%"))
if(n_outliers > 0 && n_outliers <= 10) {
  print(paste("Outlier values:", paste(round(sort(outliers), 3), collapse = ", ")))
}

# Robust statistics
trimmed_mean <- mean(df$volatility_estimate_sigma, trim = 0.1)
winsorized_mean <- mean(Winsorize(df$volatility_estimate_sigma, probs = c(0.05, 0.95)))

print("\nRobust volatility measures:")
print(paste("10% Trimmed mean:", round(trimmed_mean, 3)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 3)))
print(paste("Median (robust):", volatility_stats$median_sigma))

# Time trend analysis (if date data available)
if("response_date" %in% names(df) && sum(!is.na(df$response_date)) > 100) {
  df_time <- df %>%
    filter(!is.na(response_date)) %>%
    mutate(
      response_quarter = paste0(year(as.Date(response_date)), "-Q",
                               quarter(as.Date(response_date)))
    ) %>%
    group_by(response_quarter) %>%
    summarise(
      n = n(),
      mean_sigma = round(mean(volatility_estimate_sigma), 3),
      median_sigma = round(median(volatility_estimate_sigma), 3),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nVolatility trends over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$mean_sigma, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of Black-Scholes volatility implementation:")
print(paste("Mean volatility (σ):", volatility_stats$mean_sigma))
print(paste("SD of volatility:", volatility_stats$sd_sigma))
print(paste("95% CI for mean: [", round(ci_mean[2], 3), ", ", 
            round(ci_mean[3], 3), "]", sep=""))
print(paste("Median volatility:", volatility_stats$median_sigma))

# Expected: volatility estimates σ = 0.45 (SD = 0.12)