# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 71: "Concentration risk limits: single investment ≤25% for 78% of respondents (95% CI [75.0%, 80.9%])"
# Purpose: Analyze concentration risk limit practices with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_CONCENTRATION_LIMIT <- "max_single_investment_percentage"
COL_HAS_CONCENTRATION_POLICY <- "has_concentration_risk_policy"
COL_CONCENTRATION_THRESHOLD <- "concentration_risk_threshold"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size"
COL_INVESTMENT_STAGE_FOCUS <- "investment_stage_focus"
COL_GEOGRAPHIC_FOCUS <- "geographic_focus"
COL_SECTOR_CONCENTRATION <- "sector_concentration_percentage"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    max_single_investment_percentage = as.numeric(max_single_investment_percentage),
    has_concentration_risk_policy = as.integer(has_concentration_risk_policy),
    concentration_risk_threshold = as.numeric(concentration_risk_threshold),
    stakeholder = factor(stakeholder),
    portfolio_size = as.numeric(portfolio_size),
    investment_stage_focus = factor(investment_stage_focus),
    geographic_focus = factor(geographic_focus),
    sector_concentration_percentage = as.numeric(sector_concentration_percentage)
  ) %>%
  filter(!is.na(max_single_investment_percentage))

# Calculate proportion with ≤25% limit
n_total <- nrow(df)
n_limit_25 <- sum(df$max_single_investment_percentage <= 25, na.rm = TRUE)
prop_limit_25 <- n_limit_25 / n_total

print("Concentration risk limits (single investment ≤25%):")
print(paste("Total respondents:", n_total))
print(paste("With ≤25% limit:", n_limit_25))
print(paste("Proportion:", round(prop_limit_25 * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_limit_25, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Additional confidence intervals
ci_exact <- BinomCI(n_limit_25, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_limit_25, n_total, conf.level = 0.95, method = "agresti-coull")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))

# Distribution of concentration limits
limit_stats <- df %>%
  summarise(
    n = n(),
    mean_limit = round(mean(max_single_investment_percentage), 1),
    sd_limit = round(sd(max_single_investment_percentage), 1),
    median_limit = round(median(max_single_investment_percentage), 1),
    q25_limit = round(quantile(max_single_investment_percentage, 0.25), 1),
    q75_limit = round(quantile(max_single_investment_percentage, 0.75), 1),
    min_limit = round(min(max_single_investment_percentage), 1),
    max_limit = round(max(max_single_investment_percentage), 1)
  )

print("Concentration limit distribution statistics:")
print(limit_stats)

# Categorized concentration limits
limit_categories <- df %>%
  mutate(
    limit_category = case_when(
      max_single_investment_percentage <= 10 ~ "Very Conservative (≤10%)",
      max_single_investment_percentage <= 20 ~ "Conservative (11-20%)",
      max_single_investment_percentage <= 25 ~ "Moderate (21-25%)",
      max_single_investment_percentage <= 35 ~ "Aggressive (26-35%)",
      max_single_investment_percentage > 35 ~ "Very Aggressive (>35%)"
    )
  ) %>%
  group_by(limit_category) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(limit_category)

print("Distribution of concentration limit categories:")
print(limit_categories)

# Calculate cumulative distribution
cumulative_limits <- df %>%
  arrange(max_single_investment_percentage) %>%
  mutate(
    cumulative_n = row_number(),
    cumulative_pct = round(cumulative_n / n() * 100, 1)
  ) %>%
  filter(max_single_investment_percentage %in% c(10, 15, 20, 25, 30, 35, 40, 50)) %>%
  select(max_single_investment_percentage, cumulative_pct)

print("Cumulative distribution at key thresholds:")
print(cumulative_limits)

# Analysis by stakeholder
stakeholder_limits <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_limit = round(mean(max_single_investment_percentage), 1),
    median_limit = round(median(max_single_investment_percentage), 1),
    pct_under_25 = round(mean(max_single_investment_percentage <= 25) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(pct_under_25)

print("Concentration limits by stakeholder (n ≥ 20):")
print(stakeholder_limits)

# ANOVA for stakeholder differences
if(length(unique(df$stakeholder)) > 2) {
  anova_stakeholder <- aov(max_single_investment_percentage ~ stakeholder, data = df)
  anova_summary <- summary(anova_stakeholder)
  
  print("ANOVA - Concentration limits by stakeholder:")
  print(anova_summary)
  
  # Post-hoc tests if significant
  if(anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
    tukey_test <- TukeyHSD(anova_stakeholder)
    print("Tukey's HSD post-hoc comparisons:")
    print(tukey_test)
    
    # Effect size (eta-squared)
    ss_total <- sum(anova_summary[[1]][["Sum Sq"]])
    ss_effect <- anova_summary[[1]][["Sum Sq"]][1]
    eta_squared <- ss_effect / ss_total
    print(paste("Eta-squared:", round(eta_squared, 3)))
  }
}

# Concentration policy analysis
if(sum(!is.na(df$has_concentration_risk_policy)) > 50) {
  policy_analysis <- df %>%
    filter(!is.na(has_concentration_risk_policy)) %>%
    mutate(
      policy_status = ifelse(has_concentration_risk_policy == 1,
                           "Has Policy", "No Policy")
    ) %>%
    group_by(policy_status) %>%
    summarise(
      n = n(),
      mean_limit = round(mean(max_single_investment_percentage), 1),
      sd_limit = round(sd(max_single_investment_percentage), 1),
      pct_under_25 = round(mean(max_single_investment_percentage <= 25) * 100, 1),
      .groups = "drop"
    )
  
  print("Concentration limits by policy status:")
  print(policy_analysis)
  
  # t-test for policy differences
  if(nrow(policy_analysis) == 2) {
    with_policy <- df$max_single_investment_percentage[df$has_concentration_risk_policy == 1 & 
                                                       !is.na(df$has_concentration_risk_policy)]
    without_policy <- df$max_single_investment_percentage[df$has_concentration_risk_policy == 0 & 
                                                          !is.na(df$has_concentration_risk_policy)]
    
    if(length(with_policy) >= 20 && length(without_policy) >= 20) {
      t_test <- t.test(with_policy, without_policy)
      
      print("Policy impact on limits (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 1), "%"))
      
      # Cohen's d
      pooled_sd <- sqrt(((length(with_policy) - 1) * var(with_policy) + 
                        (length(without_policy) - 1) * var(without_policy)) / 
                       (length(with_policy) + length(without_policy) - 2))
      cohens_d <- (mean(with_policy) - mean(without_policy)) / pooled_sd
      print(paste("Cohen's d =", round(cohens_d, 3)))
    }
  }
}

# Portfolio size relationship
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_limits <- df %>%
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
      mean_limit = round(mean(max_single_investment_percentage), 1),
      median_limit = round(median(max_single_investment_percentage), 1),
      pct_under_25 = round(mean(max_single_investment_percentage <= 25) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Concentration limits by portfolio size (n ≥ 10):")
  print(portfolio_limits)
  
  # Correlation with continuous portfolio size
  cor_portfolio <- cor.test(df$max_single_investment_percentage[!is.na(df$portfolio_size)],
                            df$portfolio_size[!is.na(df$portfolio_size)])
  
  print("Portfolio size × Concentration limit correlation:")
  print(paste("r =", round(cor_portfolio$estimate, 3)))
  print(paste("95% CI: [", round(cor_portfolio$conf.int[1], 3), ", ", 
              round(cor_portfolio$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_portfolio$p.value, scientific = TRUE)))
}

# Investment stage focus relationship
if(!all(is.na(df$investment_stage_focus))) {
  stage_limits <- df %>%
    filter(!is.na(investment_stage_focus)) %>%
    group_by(investment_stage_focus) %>%
    summarise(
      n = n(),
      mean_limit = round(mean(max_single_investment_percentage), 1),
      pct_under_25 = round(mean(max_single_investment_percentage <= 25) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(mean_limit)
  
  print("Concentration limits by investment stage (n ≥ 15):")
  print(stage_limits)
  
  # ANOVA for stage differences
  if(nrow(stage_limits) > 2) {
    stage_data <- df %>%
      filter(!is.na(investment_stage_focus)) %>%
      group_by(investment_stage_focus) %>%
      filter(n() >= 15) %>%
      ungroup()
    
    if(nrow(stage_data) > 50) {
      anova_stage <- aov(max_single_investment_percentage ~ investment_stage_focus, 
                        data = stage_data)
      print("ANOVA - Limits by investment stage:")
      print(summary(anova_stage))
    }
  }
}

# Geographic focus analysis
if(!all(is.na(df$geographic_focus))) {
  geo_limits <- df %>%
    filter(!is.na(geographic_focus)) %>%
    group_by(geographic_focus) %>%
    summarise(
      n = n(),
      mean_limit = round(mean(max_single_investment_percentage), 1),
      pct_under_25 = round(mean(max_single_investment_percentage <= 25) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(mean_limit)
  
  print("Concentration limits by geographic focus (n ≥ 15):")
  print(geo_limits)
}

# Sector concentration relationship
if(sum(!is.na(df$sector_concentration_percentage)) > 50) {
  # Correlation between single investment and sector concentration
  cor_sector <- cor.test(df$max_single_investment_percentage[!is.na(df$sector_concentration_percentage)],
                         df$sector_concentration_percentage[!is.na(df$sector_concentration_percentage)])
  
  print("Single investment × Sector concentration correlation:")
  print(paste("r =", round(cor_sector$estimate, 3)))
  print(paste("95% CI: [", round(cor_sector$conf.int[1], 3), ", ", 
              round(cor_sector$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_sector$p.value, scientific = TRUE)))
  
  # Categorized analysis
  sector_analysis <- df %>%
    filter(!is.na(sector_concentration_percentage)) %>%
    mutate(
      sector_concentration_level = case_when(
        sector_concentration_percentage <= 30 ~ "Low Sector Concentration",
        sector_concentration_percentage <= 60 ~ "Moderate Sector Concentration",
        sector_concentration_percentage > 60 ~ "High Sector Concentration"
      )
    ) %>%
    group_by(sector_concentration_level) %>%
    summarise(
      n = n(),
      mean_single_limit = round(mean(max_single_investment_percentage), 1),
      pct_under_25 = round(mean(max_single_investment_percentage <= 25) * 100, 1),
      .groups = "drop"
    )
  
  print("Single investment limits by sector concentration:")
  print(sector_analysis)
}

# Risk threshold analysis
if(sum(!is.na(df$concentration_risk_threshold)) > 50) {
  threshold_comparison <- df %>%
    filter(!is.na(concentration_risk_threshold)) %>%
    mutate(
      meets_25_limit = ifelse(max_single_investment_percentage <= 25,
                             "Meets ≤25% Limit", "Exceeds 25% Limit")
    ) %>%
    group_by(meets_25_limit) %>%
    summarise(
      n = n(),
      mean_threshold = round(mean(concentration_risk_threshold), 2),
      sd_threshold = round(sd(concentration_risk_threshold), 2),
      .groups = "drop"
    )
  
  print("Risk thresholds by concentration limit compliance:")
  print(threshold_comparison)
}

# Logistic regression for ≤25% limit
if(nrow(df) > 150) {
  df$limit_25_binary <- as.integer(df$max_single_investment_percentage <= 25)
  
  df_logistic <- df %>%
    filter(!is.na(portfolio_size), !is.na(stakeholder))
  
  if(nrow(df_logistic) > 100) {
    # Include policy status if available
    if(sum(!is.na(df_logistic$has_concentration_risk_policy)) > 80) {
      logit_model <- glm(limit_25_binary ~ portfolio_size + stakeholder + 
                        has_concentration_risk_policy,
                        data = df_logistic, family = binomial)
    } else {
      logit_model <- glm(limit_25_binary ~ portfolio_size + stakeholder,
                        data = df_logistic, family = binomial)
    }
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: Factors associated with ≤25% limit")
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
  }
}

# Distribution visualization data
limit_distribution <- df %>%
  group_by(max_single_investment_percentage) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 2),
    .groups = "drop"
  ) %>%
  arrange(max_single_investment_percentage)

print("Detailed limit distribution:")
print(head(limit_distribution, 20))

# Key percentiles
percentiles <- quantile(df$max_single_investment_percentage, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
print("Concentration limit percentiles:")
for(i in 1:length(percentiles)) {
  print(paste(names(percentiles)[i], ":", round(percentiles[i], 1), "%"))
}

# Summary of key findings
print("Summary of concentration risk limits:")
print(paste("Proportion with ≤25% limit:", round(prop_limit_25 * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("Mean limit:", round(mean(df$max_single_investment_percentage), 1), "%"))
print(paste("Median limit:", round(median(df$max_single_investment_percentage), 1), "%"))

# Expected: single investment ≤25% for 78% of respondents (95% CI [75.0%, 80.9%])