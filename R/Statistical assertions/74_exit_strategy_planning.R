# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 74: "Exit strategy planning: documented plans for 81% of investments (95% CI [78.2%, 83.7%])"
# Purpose: Analyze exit strategy documentation practices with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_EXIT_STRATEGY_DOCUMENTED <- "exit_strategy_documented_percentage"
COL_EXIT_PLANNING_STAGE <- "exit_planning_stage"
COL_EXIT_TIMELINE_YEARS <- "typical_exit_timeline_years"
COL_EXIT_MULTIPLE_TARGET <- "target_exit_multiple"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size"
COL_INVESTMENT_STAGE_FOCUS <- "investment_stage_focus"
COL_EXIT_SUCCESS_RATE <- "historical_exit_success_rate"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    exit_strategy_documented_percentage = as.numeric(exit_strategy_documented_percentage),
    exit_planning_stage = factor(exit_planning_stage),
    typical_exit_timeline_years = as.numeric(typical_exit_timeline_years),
    target_exit_multiple = as.numeric(target_exit_multiple),
    stakeholder = factor(stakeholder),
    portfolio_size = as.numeric(portfolio_size),
    investment_stage_focus = factor(investment_stage_focus),
    historical_exit_success_rate = as.numeric(historical_exit_success_rate)
  ) %>%
  filter(!is.na(exit_strategy_documented_percentage))

# Calculate proportion with documented exit strategies
n_total <- nrow(df)
mean_documented <- mean(df$exit_strategy_documented_percentage)
sd_documented <- sd(df$exit_strategy_documented_percentage)

print("Exit strategy documentation rates:")
print(paste("Total respondents:", n_total))
print(paste("Mean documentation rate:", round(mean_documented, 1), "%"))
print(paste("SD:", round(sd_documented, 1), "%"))

# Calculate 95% CI for the mean
ci_mean <- MeanCI(df$exit_strategy_documented_percentage, conf.level = 0.95)
print(paste("95% CI for mean: [", round(ci_mean[2], 1), "%, ", 
            round(ci_mean[3], 1), "%]", sep=""))

# For proportion with >80% documented
n_above_80 <- sum(df$exit_strategy_documented_percentage > 80)
prop_above_80 <- n_above_80 / n_total

# Wilson confidence interval for proportion >80%
ci_wilson_80 <- BinomCI(n_above_80, n_total, conf.level = 0.95, method = "wilson")
print(paste("Proportion with >80% documented:", round(prop_above_80 * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson_80[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson_80[,"upr.ci"] * 100, 1), "%]", sep=""))

# Distribution statistics
documentation_stats <- df %>%
  summarise(
    n = n(),
    mean = round(mean(exit_strategy_documented_percentage), 1),
    sd = round(sd(exit_strategy_documented_percentage), 1),
    median = round(median(exit_strategy_documented_percentage), 1),
    q25 = round(quantile(exit_strategy_documented_percentage, 0.25), 1),
    q75 = round(quantile(exit_strategy_documented_percentage, 0.75), 1),
    min = round(min(exit_strategy_documented_percentage), 1),
    max = round(max(exit_strategy_documented_percentage), 1)
  )

print("Documentation rate distribution:")
print(documentation_stats)

# Categorized documentation levels
documentation_categories <- df %>%
  mutate(
    documentation_level = case_when(
      exit_strategy_documented_percentage >= 90 ~ "Comprehensive (≥90%)",
      exit_strategy_documented_percentage >= 75 ~ "High (75-89%)",
      exit_strategy_documented_percentage >= 50 ~ "Moderate (50-74%)",
      exit_strategy_documented_percentage >= 25 ~ "Low (25-49%)",
      exit_strategy_documented_percentage < 25 ~ "Very Low (<25%)"
    )
  ) %>%
  group_by(documentation_level) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    documentation_level = factor(documentation_level,
                                levels = c("Comprehensive (≥90%)", "High (75-89%)",
                                         "Moderate (50-74%)", "Low (25-49%)",
                                         "Very Low (<25%)"))
  ) %>%
  arrange(documentation_level)

print("Documentation level distribution:")
print(documentation_categories)

# Exit planning stage analysis
if(!all(is.na(df$exit_planning_stage))) {
  planning_stage_analysis <- df %>%
    filter(!is.na(exit_planning_stage)) %>%
    group_by(exit_planning_stage) %>%
    summarise(
      n = n(),
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      sd_documented = round(sd(exit_strategy_documented_percentage), 1),
      median_documented = round(median(exit_strategy_documented_percentage), 1),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_documented))
  
  print("Documentation by exit planning stage:")
  print(planning_stage_analysis)
  
  # ANOVA for planning stage differences
  if(length(unique(df$exit_planning_stage[!is.na(df$exit_planning_stage)])) > 2) {
    anova_stage <- aov(exit_strategy_documented_percentage ~ exit_planning_stage, 
                      data = df %>% filter(!is.na(exit_planning_stage)))
    print("ANOVA - Documentation by planning stage:")
    print(summary(anova_stage))
    
    # Post-hoc if significant
    if(summary(anova_stage)[[1]][["Pr(>F)"]][1] < 0.05) {
      tukey_test <- TukeyHSD(anova_stage)
      print("Tukey's HSD post-hoc comparisons:")
      print(tukey_test)
    }
  }
}

# Analysis by stakeholder
stakeholder_documentation <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_documented = round(mean(exit_strategy_documented_percentage), 1),
    sd_documented = round(sd(exit_strategy_documented_percentage), 1),
    pct_above_80 = round(mean(exit_strategy_documented_percentage > 80) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(mean_documented))

print("Exit documentation by stakeholder (n ≥ 20):")
print(stakeholder_documentation)

# ANOVA for stakeholder differences
if(length(unique(df$stakeholder)) > 2) {
  anova_stakeholder <- aov(exit_strategy_documented_percentage ~ stakeholder, data = df)
  anova_summary <- summary(anova_stakeholder)
  
  print("ANOVA - Documentation by stakeholder:")
  print(anova_summary)
  
  # Effect size (eta-squared)
  if(anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
    ss_total <- sum(anova_summary[[1]][["Sum Sq"]])
    ss_effect <- anova_summary[[1]][["Sum Sq"]][1]
    eta_squared <- ss_effect / ss_total
    print(paste("Eta-squared:", round(eta_squared, 3)))
  }
}

# Exit timeline analysis
if(sum(!is.na(df$typical_exit_timeline_years)) > 50) {
  timeline_correlation <- cor.test(df$exit_strategy_documented_percentage[!is.na(df$typical_exit_timeline_years)],
                                  df$typical_exit_timeline_years[!is.na(df$typical_exit_timeline_years)])
  
  print("Documentation × Exit timeline correlation:")
  print(paste("r =", round(timeline_correlation$estimate, 3)))
  print(paste("95% CI: [", round(timeline_correlation$conf.int[1], 3), ", ", 
              round(timeline_correlation$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(timeline_correlation$p.value, scientific = TRUE)))
  
  # Categorized timeline analysis
  timeline_categories <- df %>%
    filter(!is.na(typical_exit_timeline_years)) %>%
    mutate(
      timeline_category = case_when(
        typical_exit_timeline_years <= 3 ~ "Short (≤3 years)",
        typical_exit_timeline_years <= 5 ~ "Medium (4-5 years)",
        typical_exit_timeline_years <= 7 ~ "Long (6-7 years)",
        typical_exit_timeline_years > 7 ~ "Very Long (>7 years)"
      )
    ) %>%
    group_by(timeline_category) %>%
    summarise(
      n = n(),
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      sd_documented = round(sd(exit_strategy_documented_percentage), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Documentation by exit timeline (n ≥ 10):")
  print(timeline_categories)
}

# Target exit multiple analysis
if(sum(!is.na(df$target_exit_multiple)) > 50) {
  multiple_correlation <- cor.test(df$exit_strategy_documented_percentage[!is.na(df$target_exit_multiple)],
                                  df$target_exit_multiple[!is.na(df$target_exit_multiple)])
  
  print("Documentation × Target exit multiple correlation:")
  print(paste("r =", round(multiple_correlation$estimate, 3)))
  print(paste("p-value:", format(multiple_correlation$p.value, scientific = TRUE)))
  
  # Categorized multiple analysis
  multiple_categories <- df %>%
    filter(!is.na(target_exit_multiple)) %>%
    mutate(
      multiple_category = case_when(
        target_exit_multiple <= 2 ~ "Conservative (<2x)",
        target_exit_multiple <= 5 ~ "Moderate (2-5x)",
        target_exit_multiple <= 10 ~ "Aggressive (5-10x)",
        target_exit_multiple > 10 ~ "Very Aggressive (>10x)"
      )
    ) %>%
    group_by(multiple_category) %>%
    summarise(
      n = n(),
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Documentation by target exit multiple (n ≥ 10):")
  print(multiple_categories)
}

# Portfolio size relationship
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_documentation <- df %>%
    filter(!is.na(portfolio_size)) %>%
    mutate(
      portfolio_category = case_when(
        portfolio_size <= 10 ~ "Small (≤10)",
        portfolio_size <= 30 ~ "Medium (11-30)",
        portfolio_size <= 100 ~ "Large (31-100)",
        portfolio_size > 100 ~ "Very Large (>100)"
      )
    ) %>%
    group_by(portfolio_category) %>%
    summarise(
      n = n(),
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      sd_documented = round(sd(exit_strategy_documented_percentage), 1),
      pct_above_80 = round(mean(exit_strategy_documented_percentage > 80) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Documentation by portfolio size (n ≥ 10):")
  print(portfolio_documentation)
  
  # Correlation with continuous portfolio size
  cor_portfolio <- cor.test(df$exit_strategy_documented_percentage[!is.na(df$portfolio_size)],
                            df$portfolio_size[!is.na(df$portfolio_size)])
  
  print("Portfolio size × Documentation correlation:")
  print(paste("r =", round(cor_portfolio$estimate, 3)))
  print(paste("95% CI: [", round(cor_portfolio$conf.int[1], 3), ", ", 
              round(cor_portfolio$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_portfolio$p.value, scientific = TRUE)))
}

# Investment stage focus analysis
if(!all(is.na(df$investment_stage_focus))) {
  stage_documentation <- df %>%
    filter(!is.na(investment_stage_focus)) %>%
    group_by(investment_stage_focus) %>%
    summarise(
      n = n(),
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      pct_above_80 = round(mean(exit_strategy_documented_percentage > 80) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(mean_documented))
  
  print("Documentation by investment stage focus (n ≥ 15):")
  print(stage_documentation)
}

# Historical exit success rate relationship
if(sum(!is.na(df$historical_exit_success_rate)) > 50) {
  success_correlation <- cor.test(df$exit_strategy_documented_percentage[!is.na(df$historical_exit_success_rate)],
                                 df$historical_exit_success_rate[!is.na(df$historical_exit_success_rate)])
  
  print("Documentation × Historical exit success correlation:")
  print(paste("r =", round(success_correlation$estimate, 3)))
  print(paste("95% CI: [", round(success_correlation$conf.int[1], 3), ", ", 
              round(success_correlation$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(success_correlation$p.value, scientific = TRUE)))
  
  # Categorized success rate analysis
  success_categories <- df %>%
    filter(!is.na(historical_exit_success_rate)) %>%
    mutate(
      success_category = case_when(
        historical_exit_success_rate >= 75 ~ "High Success (≥75%)",
        historical_exit_success_rate >= 50 ~ "Moderate Success (50-74%)",
        historical_exit_success_rate >= 25 ~ "Low Success (25-49%)",
        historical_exit_success_rate < 25 ~ "Very Low Success (<25%)"
      )
    ) %>%
    group_by(success_category) %>%
    summarise(
      n = n(),
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      sd_documented = round(sd(exit_strategy_documented_percentage), 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Documentation by historical exit success (n ≥ 10):")
  print(success_categories)
}

# Linear regression model
if(nrow(df) > 150) {
  df_regression <- df %>%
    filter(!is.na(portfolio_size))
  
  # Build model with available predictors
  predictors <- c("portfolio_size")
  
  if(sum(!is.na(df_regression$typical_exit_timeline_years)) > 100) {
    predictors <- c(predictors, "typical_exit_timeline_years")
  }
  
  if(sum(!is.na(df_regression$target_exit_multiple)) > 100) {
    predictors <- c(predictors, "target_exit_multiple")
  }
  
  if(sum(!is.na(df_regression$historical_exit_success_rate)) > 100) {
    predictors <- c(predictors, "historical_exit_success_rate")
  }
  
  if(length(predictors) > 1 && nrow(df_regression) > 100) {
    formula_str <- paste("exit_strategy_documented_percentage ~", 
                        paste(predictors, collapse = " + "))
    lm_model <- lm(as.formula(formula_str), data = df_regression)
    
    lm_summary <- summary(lm_model)
    print("Linear regression: Exit documentation predictors")
    print(lm_summary)
    
    # Confidence intervals for coefficients
    coef_ci <- confint(lm_model)
    print("95% CIs for coefficients:")
    print(coef_ci)
    
    # Model diagnostics
    print("Model diagnostics:")
    print(paste("R-squared:", round(lm_summary$r.squared, 3)))
    print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
    print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
    print(paste("p-value:", format(pf(lm_summary$fstatistic[1], 
                                      lm_summary$fstatistic[2], 
                                      lm_summary$fstatistic[3], 
                                      lower.tail = FALSE), scientific = TRUE)))
  }
}

# Comparison of high vs low documentation groups
high_low_comparison <- df %>%
  mutate(
    documentation_group = ifelse(exit_strategy_documented_percentage >= 80,
                                "High Documentation (≥80%)",
                                "Low Documentation (<80%)")
  ) %>%
  group_by(documentation_group) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  )

print("High vs Low documentation groups:")
print(high_low_comparison)

# Additional metrics for high documentation group
if(sum(df$exit_strategy_documented_percentage >= 80) > 30) {
  high_doc_metrics <- df %>%
    filter(exit_strategy_documented_percentage >= 80) %>%
    summarise(
      n = n(),
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      median_timeline = round(median(typical_exit_timeline_years, na.rm = TRUE), 1),
      mean_portfolio_size = round(mean(portfolio_size, na.rm = TRUE), 1),
      mean_success_rate = round(mean(historical_exit_success_rate, na.rm = TRUE), 1)
    )
  
  print("Metrics for high documentation group (≥80%):")
  print(high_doc_metrics)
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
      mean_documented = round(mean(exit_strategy_documented_percentage), 1),
      pct_above_80 = round(mean(exit_strategy_documented_percentage > 80) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("Documentation trends over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$mean_documented, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("Summary of exit strategy planning:")
print(paste("Mean documentation rate:", round(mean_documented, 1), "%"))
print(paste("95% CI for mean: [", round(ci_mean[2], 1), "%, ", 
            round(ci_mean[3], 1), "%]", sep=""))
print(paste("Proportion with >80% documented:", round(prop_above_80 * 100, 1), "%"))

# Expected: documented plans for 81% of investments (95% CI [78.2%, 83.7%])