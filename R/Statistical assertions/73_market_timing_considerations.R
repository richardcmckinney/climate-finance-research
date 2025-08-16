# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 73: "Market timing considerations: actively considered by 38% (95% CI [34.7%, 41.3%])"
# Purpose: Analyze market timing strategy adoption with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MARKET_TIMING_ACTIVE <- "market_timing_active_consideration"
COL_MARKET_TIMING_STRATEGY <- "market_timing_strategy_type"
COL_TIMING_SUCCESS_RATE <- "timing_success_rate_percentage"
COL_MARKET_INDICATORS_USED <- "market_indicators_monitored"
COL_STAKEHOLDER <- "stakeholder"
COL_EXPERIENCE_YEARS <- "experience_years"
COL_PORTFOLIO_PERFORMANCE <- "portfolio_performance"
COL_MARKET_VOLATILITY_TOLERANCE <- "market_volatility_tolerance"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    market_timing_active_consideration = as.integer(market_timing_active_consideration),
    market_timing_strategy_type = factor(market_timing_strategy_type),
    timing_success_rate_percentage = as.numeric(timing_success_rate_percentage),
    market_indicators_monitored = as.character(market_indicators_monitored),
    stakeholder = factor(stakeholder),
    experience_years = as.numeric(experience_years),
    portfolio_performance = as.numeric(portfolio_performance),
    market_volatility_tolerance = as.numeric(market_volatility_tolerance)
  ) %>%
  filter(!is.na(market_timing_active_consideration))

# Overall proportion actively considering market timing
n_total <- nrow(df)
n_active <- sum(df$market_timing_active_consideration, na.rm = TRUE)
prop_active <- n_active / n_total

print("Market timing active consideration:")
print(paste("Total respondents:", n_total))
print(paste("Actively considering:", n_active))
print(paste("Proportion:", round(prop_active * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_active, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence intervals
ci_exact <- BinomCI(n_active, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_active, n_total, conf.level = 0.95, method = "agresti-coull")
ci_wald <- BinomCI(n_active, n_total, conf.level = 0.95, method = "wald")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Wald CI: [", round(ci_wald[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wald[,"upr.ci"] * 100, 1), "%]", sep=""))

# Market timing strategy types
if(!all(is.na(df$market_timing_strategy_type))) {
  strategy_breakdown <- df %>%
    filter(!is.na(market_timing_strategy_type), 
           market_timing_active_consideration == 1) %>%
    group_by(market_timing_strategy_type) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(df$market_timing_active_consideration == 1) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("Market timing strategies among active practitioners:")
  print(strategy_breakdown)
}

# Success rate analysis
if(sum(!is.na(df$timing_success_rate_percentage)) > 30) {
  success_stats <- df %>%
    filter(!is.na(timing_success_rate_percentage),
           market_timing_active_consideration == 1) %>%
    summarise(
      n = n(),
      mean_success = round(mean(timing_success_rate_percentage), 1),
      sd_success = round(sd(timing_success_rate_percentage), 1),
      median_success = round(median(timing_success_rate_percentage), 1),
      q25_success = round(quantile(timing_success_rate_percentage, 0.25), 1),
      q75_success = round(quantile(timing_success_rate_percentage, 0.75), 1)
    )
  
  print("Timing success rates among active practitioners:")
  print(success_stats)
  
  # Compare success rates: active vs non-active
  success_comparison <- df %>%
    filter(!is.na(timing_success_rate_percentage)) %>%
    mutate(
      timing_status = ifelse(market_timing_active_consideration == 1,
                           "Active Timing", "No Active Timing")
    ) %>%
    group_by(timing_status) %>%
    summarise(
      n = n(),
      mean_success = round(mean(timing_success_rate_percentage), 1),
      sd_success = round(sd(timing_success_rate_percentage), 1),
      .groups = "drop"
    )
  
  print("Success rate comparison:")
  print(success_comparison)
  
  # t-test if both groups have sufficient data
  if(nrow(success_comparison) == 2) {
    active_success <- df$timing_success_rate_percentage[
      df$market_timing_active_consideration == 1 & 
      !is.na(df$timing_success_rate_percentage)]
    inactive_success <- df$timing_success_rate_percentage[
      df$market_timing_active_consideration == 0 & 
      !is.na(df$timing_success_rate_percentage)]
    
    if(length(active_success) >= 15 && length(inactive_success) >= 15) {
      t_test <- t.test(active_success, inactive_success)
      print("Success rate comparison (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 1), "%"))
    }
  }
}

# Analysis by stakeholder
stakeholder_timing <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    active_count = sum(market_timing_active_consideration),
    active_pct = round(mean(market_timing_active_consideration) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(active_pct))

print("Market timing by stakeholder (n ≥ 20):")
print(stakeholder_timing)

# Calculate Wilson CIs for each stakeholder
stakeholder_ci <- stakeholder_timing %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(active_count, n, conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("Stakeholder proportions with 95% Wilson CIs:")
print(stakeholder_ci %>% select(stakeholder, n, active_pct, ci_text))

# Chi-square test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder, df$market_timing_active_consideration)
  chi_stakeholder <- chisq.test(stakeholder_table)
  print("Chi-square test for stakeholder differences:")
  print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
  print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
  
  # Cramér's V
  cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * 
                                                 (min(dim(stakeholder_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Experience years relationship
if(sum(!is.na(df$experience_years)) > 50) {
  experience_timing <- df %>%
    filter(!is.na(experience_years)) %>%
    mutate(
      experience_category = case_when(
        experience_years <= 5 ~ "Junior (≤5 years)",
        experience_years <= 10 ~ "Mid-level (6-10 years)",
        experience_years <= 15 ~ "Senior (11-15 years)",
        experience_years > 15 ~ "Executive (>15 years)"
      )
    ) %>%
    group_by(experience_category) %>%
    summarise(
      n = n(),
      active_count = sum(market_timing_active_consideration),
      active_pct = round(mean(market_timing_active_consideration) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(experience_category)
  
  print("Market timing by experience level (n ≥ 15):")
  print(experience_timing)
  
  # Point-biserial correlation
  cor_experience <- cor.test(df$market_timing_active_consideration[!is.na(df$experience_years)],
                             df$experience_years[!is.na(df$experience_years)])
  
  print("Experience × Market timing correlation:")
  print(paste("r =", round(cor_experience$estimate, 3)))
  print(paste("95% CI: [", round(cor_experience$conf.int[1], 3), ", ", 
              round(cor_experience$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_experience$p.value, scientific = TRUE)))
}

# Market indicators analysis
if(!all(is.na(df$market_indicators_monitored))) {
  # Parse comma-separated indicators
  indicators_df <- df %>%
    filter(!is.na(market_indicators_monitored), 
           market_timing_active_consideration == 1) %>%
    mutate(indicators_list = strsplit(market_indicators_monitored, ",")) %>%
    unnest(indicators_list) %>%
    mutate(indicators_list = trimws(indicators_list))
  
  if(nrow(indicators_df) > 0) {
    indicator_counts <- indicators_df %>%
      group_by(indicators_list) %>%
      summarise(
        n = n(),
        percentage = round(n() / sum(df$market_timing_active_consideration == 1) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(n)) %>%
      head(15)
    
    print("Top 15 market indicators monitored by active timers:")
    print(indicator_counts)
    
    # Number of indicators per respondent
    indicators_per_person <- df %>%
      filter(!is.na(market_indicators_monitored), 
             market_timing_active_consideration == 1) %>%
      mutate(
        n_indicators = str_count(market_indicators_monitored, ",") + 1
      ) %>%
      summarise(
        mean_indicators = round(mean(n_indicators), 1),
        sd_indicators = round(sd(n_indicators), 1),
        median_indicators = median(n_indicators)
      )
    
    print("Number of indicators per active timer:")
    print(indicators_per_person)
  }
}

# Portfolio performance relationship
if(sum(!is.na(df$portfolio_performance)) > 50) {
  performance_comparison <- df %>%
    filter(!is.na(portfolio_performance)) %>%
    mutate(
      timing_status = ifelse(market_timing_active_consideration == 1,
                           "Active Market Timing", "No Active Timing")
    ) %>%
    group_by(timing_status) %>%
    summarise(
      n = n(),
      mean_performance = round(mean(portfolio_performance), 2),
      sd_performance = round(sd(portfolio_performance), 2),
      median_performance = round(median(portfolio_performance), 2),
      .groups = "drop"
    )
  
  print("Portfolio performance by market timing approach:")
  print(performance_comparison)
  
  # t-test for performance differences
  if(nrow(performance_comparison) == 2) {
    active_perf <- df$portfolio_performance[df$market_timing_active_consideration == 1 & 
                                           !is.na(df$portfolio_performance)]
    inactive_perf <- df$portfolio_performance[df$market_timing_active_consideration == 0 & 
                                             !is.na(df$portfolio_performance)]
    
    if(length(active_perf) >= 20 && length(inactive_perf) >= 20) {
      t_test_perf <- t.test(active_perf, inactive_perf)
      
      print("Performance comparison (t-test):")
      print(paste("t =", round(t_test_perf$statistic, 2)))
      print(paste("df =", round(t_test_perf$parameter, 1)))
      print(paste("p-value:", format(t_test_perf$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test_perf$estimate), 2)))
      
      # Cohen's d
      pooled_sd <- sqrt(((length(active_perf) - 1) * var(active_perf) + 
                        (length(inactive_perf) - 1) * var(inactive_perf)) / 
                       (length(active_perf) + length(inactive_perf) - 2))
      cohens_d <- (mean(active_perf) - mean(inactive_perf)) / pooled_sd
      print(paste("Cohen's d =", round(cohens_d, 3)))
    }
  }
}

# Market volatility tolerance relationship
if(sum(!is.na(df$market_volatility_tolerance)) > 50) {
  volatility_comparison <- df %>%
    filter(!is.na(market_volatility_tolerance)) %>%
    mutate(
      timing_status = ifelse(market_timing_active_consideration == 1,
                           "Active Timing", "No Active Timing")
    ) %>%
    group_by(timing_status) %>%
    summarise(
      n = n(),
      mean_tolerance = round(mean(market_volatility_tolerance), 2),
      sd_tolerance = round(sd(market_volatility_tolerance), 2),
      .groups = "drop"
    )
  
  print("Market volatility tolerance by timing approach:")
  print(volatility_comparison)
  
  # Correlation test
  cor_volatility <- cor.test(df$market_timing_active_consideration[!is.na(df$market_volatility_tolerance)],
                             df$market_volatility_tolerance[!is.na(df$market_volatility_tolerance)])
  
  print("Market timing × Volatility tolerance correlation:")
  print(paste("r =", round(cor_volatility$estimate, 3)))
  print(paste("p-value:", format(cor_volatility$p.value, scientific = TRUE)))
}

# Logistic regression model
if(nrow(df) > 150) {
  df_logistic <- df %>%
    filter(!is.na(stakeholder))
  
  # Build model with available predictors
  predictors <- c("stakeholder")
  
  if(sum(!is.na(df_logistic$experience_years)) > 100) {
    predictors <- c(predictors, "experience_years")
  }
  
  if(sum(!is.na(df_logistic$market_volatility_tolerance)) > 100) {
    predictors <- c(predictors, "market_volatility_tolerance")
  }
  
  if(sum(!is.na(df_logistic$portfolio_performance)) > 100) {
    predictors <- c(predictors, "portfolio_performance")
  }
  
  if(length(predictors) > 1 && nrow(df_logistic) > 100) {
    formula_str <- paste("market_timing_active_consideration ~", 
                        paste(predictors, collapse = " + "))
    logit_model <- glm(as.formula(formula_str), data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: Market timing predictors")
    print(logit_summary)
    
    # Odds ratios
    odds_ratios <- exp(coef(logit_model))
    odds_ci <- exp(confint(logit_model))
    
    print("Odds ratios with 95% CIs:")
    for(i in 1:length(odds_ratios)) {
      print(paste(names(odds_ratios)[i], ": OR =", round(odds_ratios[i], 3),
                  ", 95% CI [", round(odds_ci[i, 1], 3), ", ", 
                  round(odds_ci[i, 2], 3), "]", sep=""))
    }
    
    # Model fit
    print("Model fit statistics:")
    print(paste("AIC:", round(AIC(logit_model), 1)))
    print(paste("BIC:", round(BIC(logit_model), 1)))
    
    # McFadden's pseudo R²
    null_model <- glm(market_timing_active_consideration ~ 1, 
                     data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

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
      active_count = sum(market_timing_active_consideration),
      active_pct = round(mean(market_timing_active_consideration) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("Market timing adoption over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$active_pct, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary statistics for active vs inactive groups
summary_comparison <- df %>%
  mutate(
    timing_group = ifelse(market_timing_active_consideration == 1,
                         "Active Market Timers", "Non-Market Timers")
  ) %>%
  group_by(timing_group) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  )

print("Overall distribution:")
print(summary_comparison)

# Summary of key findings
print("Summary of market timing considerations:")
print(paste("Actively considered by:", round(prop_active * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: actively considered by 38% (95% CI [34.7%, 41.3%])