# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 78: "Discounted cash flow usage: terminal value represents 65% of total value (95% CI [62.3%, 67.7%])"
# Purpose: Analyze DCF methodology and terminal value contribution

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TERMINAL_VALUE_PERCENTAGE <- "terminal_value_percentage"
COL_DCF_DISCOUNT_RATE <- "dcf_discount_rate"
COL_FORECAST_PERIOD_YEARS <- "dcf_forecast_period_years"
COL_TERMINAL_GROWTH_RATE <- "terminal_growth_rate"
COL_DCF_METHOD_TYPE <- "dcf_method_type"
COL_STAKEHOLDER <- "stakeholder"
COL_SECTOR_FOCUS <- "sector_focus"
COL_INVESTMENT_STAGE <- "investment_stage_focus"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    terminal_value_percentage = as.numeric(terminal_value_percentage),
    dcf_discount_rate = as.numeric(dcf_discount_rate),
    dcf_forecast_period_years = as.numeric(dcf_forecast_period_years),
    terminal_growth_rate = as.numeric(terminal_growth_rate),
    dcf_method_type = factor(dcf_method_type),
    stakeholder = factor(stakeholder),
    sector_focus = factor(sector_focus),
    investment_stage_focus = factor(investment_stage_focus)
  ) %>%
  filter(!is.na(terminal_value_percentage))

# Terminal value percentage statistics
n_total <- nrow(df)
terminal_stats <- df %>%
  summarise(
    n = n(),
    mean = round(mean(terminal_value_percentage), 1),
    sd = round(sd(terminal_value_percentage), 1),
    median = round(median(terminal_value_percentage), 1),
    q25 = round(quantile(terminal_value_percentage, 0.25), 1),
    q75 = round(quantile(terminal_value_percentage, 0.75), 1),
    min = round(min(terminal_value_percentage), 1),
    max = round(max(terminal_value_percentage), 1)
  )

print("Terminal value as percentage of total DCF value:")
print(terminal_stats)
print(paste("Mean:", terminal_stats$mean, "%"))

# 95% CI for mean
ci_mean <- MeanCI(df$terminal_value_percentage, conf.level = 0.95)
print(paste("95% CI for mean: [", round(ci_mean[2], 1), "%, ", 
            round(ci_mean[3], 1), "%]", sep=""))

# Bootstrap CI for median
set.seed(123)
boot_median <- boot(df$terminal_value_percentage, 
                    function(x, i) median(x[i]), 
                    R = 1000)
ci_median <- boot.ci(boot_median, type = "perc")$percent[4:5]
print(paste("Median:", terminal_stats$median, "%"))
print(paste("95% Bootstrap CI for median: [", round(ci_median[1], 1), "%, ", 
            round(ci_median[2], 1), "%]", sep=""))

# Terminal value categories
terminal_categories <- df %>%
  mutate(
    terminal_category = case_when(
      terminal_value_percentage < 40 ~ "Low (<40%)",
      terminal_value_percentage < 55 ~ "Moderate (40-54%)",
      terminal_value_percentage < 70 ~ "High (55-69%)",
      terminal_value_percentage < 85 ~ "Very High (70-84%)",
      terminal_value_percentage >= 85 ~ "Extreme (≥85%)"
    )
  ) %>%
  group_by(terminal_category) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    terminal_category = factor(terminal_category,
                              levels = c("Low (<40%)", "Moderate (40-54%)",
                                       "High (55-69%)", "Very High (70-84%)",
                                       "Extreme (≥85%)"))
  ) %>%
  arrange(terminal_category)

print("\nTerminal value contribution categories:")
print(terminal_categories)

# Discount rate analysis
if(sum(!is.na(df$dcf_discount_rate)) > 50) {
  discount_stats <- df %>%
    filter(!is.na(dcf_discount_rate)) %>%
    summarise(
      n = n(),
      mean_rate = round(mean(dcf_discount_rate), 1),
      sd_rate = round(sd(dcf_discount_rate), 1),
      median_rate = round(median(dcf_discount_rate), 1),
      q25_rate = round(quantile(dcf_discount_rate, 0.25), 1),
      q75_rate = round(quantile(dcf_discount_rate, 0.75), 1)
    )
  
  print("\nDCF discount rate statistics:")
  print(discount_stats)
  
  # Correlation with terminal value
  cor_discount <- cor.test(df$terminal_value_percentage[!is.na(df$dcf_discount_rate)],
                           df$dcf_discount_rate[!is.na(df$dcf_discount_rate)])
  
  print("Terminal value % × Discount rate correlation:")
  print(paste("r =", round(cor_discount$estimate, 3)))
  print(paste("95% CI: [", round(cor_discount$conf.int[1], 3), ", ", 
              round(cor_discount$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_discount$p.value, scientific = TRUE)))
}

# Forecast period analysis
if(sum(!is.na(df$dcf_forecast_period_years)) > 50) {
  forecast_stats <- df %>%
    filter(!is.na(dcf_forecast_period_years)) %>%
    summarise(
      n = n(),
      mean_period = round(mean(dcf_forecast_period_years), 1),
      sd_period = round(sd(dcf_forecast_period_years), 1),
      median_period = median(dcf_forecast_period_years),
      mode_period = as.numeric(names(sort(table(dcf_forecast_period_years), 
                                          decreasing = TRUE)[1]))
    )
  
  print("\nDCF forecast period statistics:")
  print(forecast_stats)
  
  # Forecast period distribution
  period_dist <- df %>%
    filter(!is.na(dcf_forecast_period_years)) %>%
    group_by(dcf_forecast_period_years) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$dcf_forecast_period_years)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(dcf_forecast_period_years)
  
  print("\nForecast period distribution:")
  print(period_dist)
  
  # Correlation with terminal value
  cor_period <- cor.test(df$terminal_value_percentage[!is.na(df$dcf_forecast_period_years)],
                         df$dcf_forecast_period_years[!is.na(df$dcf_forecast_period_years)])
  
  print("Terminal value % × Forecast period correlation:")
  print(paste("r =", round(cor_period$estimate, 3)))
  print(paste("p-value:", format(cor_period$p.value, scientific = TRUE)))
}

# Terminal growth rate analysis
if(sum(!is.na(df$terminal_growth_rate)) > 50) {
  growth_stats <- df %>%
    filter(!is.na(terminal_growth_rate)) %>%
    summarise(
      n = n(),
      mean_growth = round(mean(terminal_growth_rate), 2),
      sd_growth = round(sd(terminal_growth_rate), 2),
      median_growth = round(median(terminal_growth_rate), 2),
      q25_growth = round(quantile(terminal_growth_rate, 0.25), 2),
      q75_growth = round(quantile(terminal_growth_rate, 0.75), 2)
    )
  
  print("\nTerminal growth rate statistics:")
  print(growth_stats)
  
  # Correlation with terminal value
  cor_growth <- cor.test(df$terminal_value_percentage[!is.na(df$terminal_growth_rate)],
                         df$terminal_growth_rate[!is.na(df$terminal_growth_rate)])
  
  print("Terminal value % × Terminal growth rate correlation:")
  print(paste("r =", round(cor_growth$estimate, 3)))
  print(paste("p-value:", format(cor_growth$p.value, scientific = TRUE)))
}

# DCF method type analysis
if(!all(is.na(df$dcf_method_type))) {
  method_terminal <- df %>%
    filter(!is.na(dcf_method_type)) %>%
    group_by(dcf_method_type) %>%
    summarise(
      n = n(),
      mean_terminal = round(mean(terminal_value_percentage), 1),
      sd_terminal = round(sd(terminal_value_percentage), 1),
      median_terminal = round(median(terminal_value_percentage), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_terminal))
  
  print("\nTerminal value by DCF method type:")
  print(method_terminal)
  
  # ANOVA for method differences
  if(length(unique(df$dcf_method_type[!is.na(df$dcf_method_type)])) > 2) {
    anova_method <- aov(terminal_value_percentage ~ dcf_method_type, 
                       data = df %>% filter(!is.na(dcf_method_type)))
    print("\nANOVA - Terminal value by DCF method:")
    print(summary(anova_method))
  }
}

# Analysis by stakeholder
stakeholder_terminal <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_terminal = round(mean(terminal_value_percentage), 1),
    sd_terminal = round(sd(terminal_value_percentage), 1),
    median_terminal = round(median(terminal_value_percentage), 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(mean_terminal))

print("\nTerminal value by stakeholder (n ≥ 20):")
print(stakeholder_terminal)

# Calculate 95% CIs for each stakeholder
stakeholder_ci <- stakeholder_terminal %>%
  rowwise() %>%
  mutate(
    terminal_values = list(df$terminal_value_percentage[df$stakeholder == stakeholder]),
    ci_data = list(MeanCI(unlist(terminal_values), conf.level = 0.95)),
    ci_lower = round(ci_data[2], 1),
    ci_upper = round(ci_data[3], 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-terminal_values, -ci_data)

print("\nStakeholder means with 95% CIs:")
print(stakeholder_ci %>% select(stakeholder, n, mean_terminal, ci_text))

# ANOVA for stakeholder differences
if(length(unique(df$stakeholder)) > 2) {
  anova_stakeholder <- aov(terminal_value_percentage ~ stakeholder, data = df)
  anova_summary <- summary(anova_stakeholder)
  
  print("\nANOVA - Terminal value by stakeholder:")
  print(anova_summary)
  
  # Effect size
  if(anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
    ss_total <- sum(anova_summary[[1]][["Sum Sq"]])
    ss_effect <- anova_summary[[1]][["Sum Sq"]][1]
    eta_squared <- ss_effect / ss_total
    print(paste("Eta-squared:", round(eta_squared, 3)))
  }
}

# Sector focus analysis
if(!all(is.na(df$sector_focus))) {
  sector_terminal <- df %>%
    filter(!is.na(sector_focus)) %>%
    group_by(sector_focus) %>%
    summarise(
      n = n(),
      mean_terminal = round(mean(terminal_value_percentage), 1),
      median_terminal = round(median(terminal_value_percentage), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(mean_terminal)) %>%
    head(10)
  
  print("\nTop 10 sectors by terminal value % (n ≥ 15):")
  print(sector_terminal)
}

# Investment stage analysis
if(!all(is.na(df$investment_stage_focus))) {
  stage_terminal <- df %>%
    filter(!is.na(investment_stage_focus)) %>%
    group_by(investment_stage_focus) %>%
    summarise(
      n = n(),
      mean_terminal = round(mean(terminal_value_percentage), 1),
      sd_terminal = round(sd(terminal_value_percentage), 1),
      median_terminal = round(median(terminal_value_percentage), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(investment_stage_focus)
  
  print("\nTerminal value by investment stage (n ≥ 15):")
  print(stage_terminal)
  
  # Test for trend across stages
  if(nrow(stage_terminal) > 2) {
    # Assuming stages are ordered
    df_stage_cor <- df %>%
      filter(!is.na(investment_stage_focus)) %>%
      mutate(
        stage_numeric = as.numeric(factor(investment_stage_focus))
      )
    
    cor_stage <- cor.test(df_stage_cor$terminal_value_percentage,
                          df_stage_cor$stage_numeric,
                          method = "spearman")
    
    print("\nInvestment stage × Terminal value correlation (Spearman):")
    print(paste("ρ =", round(cor_stage$estimate, 3)))
    print(paste("p-value:", format(cor_stage$p.value, scientific = TRUE)))
  }
}

# Multiple regression model
if(nrow(df) > 150) {
  df_regression <- df
  
  # Build model with available predictors
  predictors <- c()
  
  if(sum(!is.na(df_regression$dcf_discount_rate)) > 100) {
    predictors <- c(predictors, "dcf_discount_rate")
  }
  
  if(sum(!is.na(df_regression$dcf_forecast_period_years)) > 100) {
    predictors <- c(predictors, "dcf_forecast_period_years")
  }
  
  if(sum(!is.na(df_regression$terminal_growth_rate)) > 100) {
    predictors <- c(predictors, "terminal_growth_rate")
  }
  
  if(length(predictors) > 0 && nrow(df_regression) > 100) {
    formula_str <- paste("terminal_value_percentage ~", 
                        paste(predictors, collapse = " + "))
    lm_model <- lm(as.formula(formula_str), data = df_regression)
    
    lm_summary <- summary(lm_model)
    print("\nMultiple regression: Terminal value predictors")
    print(lm_summary)
    
    # Confidence intervals for coefficients
    coef_ci <- confint(lm_model)
    print("\n95% CIs for coefficients:")
    print(coef_ci)
    
    # Model diagnostics
    print("\nModel diagnostics:")
    print(paste("R-squared:", round(lm_summary$r.squared, 3)))
    print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
    
    # VIF for multicollinearity
    if(length(predictors) > 1) {
      vif_values <- car::vif(lm_model)
      print("\nVariance Inflation Factors:")
      print(round(vif_values, 2))
    }
  }
}

# Sensitivity analysis
if(sum(!is.na(df$dcf_discount_rate)) > 50 && sum(!is.na(df$terminal_growth_rate)) > 50) {
  # Create categories for sensitivity analysis
  sensitivity_analysis <- df %>%
    filter(!is.na(dcf_discount_rate), !is.na(terminal_growth_rate)) %>%
    mutate(
      discount_category = cut(dcf_discount_rate, 
                             breaks = quantile(dcf_discount_rate, c(0, 0.33, 0.67, 1)),
                             labels = c("Low", "Medium", "High")),
      growth_category = cut(terminal_growth_rate,
                           breaks = quantile(terminal_growth_rate, c(0, 0.33, 0.67, 1)),
                           labels = c("Low", "Medium", "High"))
    ) %>%
    group_by(discount_category, growth_category) %>%
    summarise(
      n = n(),
      mean_terminal = round(mean(terminal_value_percentage), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 5)
  
  print("\nSensitivity: Terminal value by discount rate and growth rate:")
  print(sensitivity_analysis)
}

# Distribution tests
# Shapiro-Wilk test for normality
if(nrow(df) <= 5000) {
  shapiro_test <- shapiro.test(df$terminal_value_percentage)
  print("\nShapiro-Wilk normality test:")
  print(paste("W =", round(shapiro_test$statistic, 4)))
  print(paste("p-value:", format(shapiro_test$p.value, scientific = TRUE)))
}

# Skewness and kurtosis
skewness_val <- e1071::skewness(df$terminal_value_percentage)
kurtosis_val <- e1071::kurtosis(df$terminal_value_percentage)

print("\nDistribution shape:")
print(paste("Skewness:", round(skewness_val, 2)))
print(paste("Kurtosis:", round(kurtosis_val, 2)))

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
      mean_terminal = round(mean(terminal_value_percentage), 1),
      median_terminal = round(median(terminal_value_percentage), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nTerminal value trends over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$mean_terminal, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of DCF terminal value analysis:")
print(paste("Mean terminal value:", terminal_stats$mean, "%"))
print(paste("95% CI: [", round(ci_mean[2], 1), "%, ", 
            round(ci_mean[3], 1), "%]", sep=""))
print(paste("Median:", terminal_stats$median, "%"))
print(paste("IQR: [", terminal_stats$q25, "%, ", terminal_stats$q75, "%]"))

# Expected: terminal value represents 65% of total value (95% CI [62.3%, 67.7%])