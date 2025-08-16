# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 70: "Correlation risk management: systematic assessment by 44% (95% CI [40.6%, 47.4%])"
# Purpose: Analyze correlation risk management practices with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)
library(corrplot)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_CORRELATION_RISK_MGMT <- "correlation_risk_management"
COL_SYSTEMATIC_ASSESSMENT <- "systematic_correlation_assessment"
COL_CORRELATION_MONITORING_FREQ <- "correlation_monitoring_frequency"
COL_CORRELATION_THRESHOLD <- "correlation_threshold"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size"
COL_RISK_EVENTS_EXPERIENCED <- "risk_events_experienced"
COL_PORTFOLIO_VOLATILITY <- "portfolio_volatility"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    correlation_risk_management = as.integer(correlation_risk_management),
    systematic_correlation_assessment = as.integer(systematic_correlation_assessment),
    correlation_monitoring_frequency = factor(correlation_monitoring_frequency),
    correlation_threshold = as.numeric(correlation_threshold),
    stakeholder = factor(stakeholder),
    portfolio_size = as.numeric(portfolio_size),
    risk_events_experienced = as.integer(risk_events_experienced),
    portfolio_volatility = as.numeric(portfolio_volatility)
  ) %>%
  filter(!is.na(systematic_correlation_assessment))

# Overall proportion with systematic assessment
n_total <- nrow(df)
n_systematic <- sum(df$systematic_correlation_assessment, na.rm = TRUE)
prop_systematic <- n_systematic / n_total

print("Systematic correlation risk assessment:")
print(paste("Total respondents:", n_total))
print(paste("Using systematic assessment:", n_systematic))
print(paste("Proportion:", round(prop_systematic * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_systematic, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence intervals
ci_exact <- BinomCI(n_systematic, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_systematic, n_total, conf.level = 0.95, method = "agresti-coull")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))

# Monitoring frequency among those with systematic assessment
if(sum(!is.na(df$correlation_monitoring_frequency)) > 30) {
  monitoring_freq <- df %>%
    filter(systematic_correlation_assessment == 1,
           !is.na(correlation_monitoring_frequency)) %>%
    group_by(correlation_monitoring_frequency) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(df$systematic_correlation_assessment == 1) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(correlation_monitoring_frequency)
  
  print("Monitoring frequency among systematic assessors:")
  print(monitoring_freq)
}

# Correlation thresholds analysis
if(sum(!is.na(df$correlation_threshold)) > 30) {
  threshold_stats <- df %>%
    filter(!is.na(correlation_threshold)) %>%
    summarise(
      n = n(),
      mean_threshold = round(mean(correlation_threshold), 3),
      sd_threshold = round(sd(correlation_threshold), 3),
      median_threshold = round(median(correlation_threshold), 3),
      q25_threshold = round(quantile(correlation_threshold, 0.25), 3),
      q75_threshold = round(quantile(correlation_threshold, 0.75), 3)
    )
  
  print("Correlation threshold statistics:")
  print(threshold_stats)
  
  # Compare thresholds between systematic vs non-systematic
  threshold_comparison <- df %>%
    filter(!is.na(correlation_threshold)) %>%
    mutate(
      assessment_type = ifelse(systematic_correlation_assessment == 1,
                              "Systematic", "Non-systematic")
    ) %>%
    group_by(assessment_type) %>%
    summarise(
      n = n(),
      mean_threshold = round(mean(correlation_threshold), 3),
      sd_threshold = round(sd(correlation_threshold), 3),
      .groups = "drop"
    )
  
  print("Threshold comparison by assessment type:")
  print(threshold_comparison)
  
  # t-test for threshold differences
  if(nrow(threshold_comparison) == 2) {
    systematic_thresholds <- df$correlation_threshold[df$systematic_correlation_assessment == 1 & 
                                                     !is.na(df$correlation_threshold)]
    nonsystematic_thresholds <- df$correlation_threshold[df$systematic_correlation_assessment == 0 & 
                                                        !is.na(df$correlation_threshold)]
    
    if(length(systematic_thresholds) >= 10 && length(nonsystematic_thresholds) >= 10) {
      t_test <- t.test(systematic_thresholds, nonsystematic_thresholds)
      print("Threshold comparison t-test:")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 3)))
    }
  }
}

# Analysis by stakeholder
stakeholder_analysis <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    systematic_count = sum(systematic_correlation_assessment),
    proportion = round(mean(systematic_correlation_assessment) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(proportion))

print("Systematic assessment by stakeholder (n ≥ 20):")
print(stakeholder_analysis)

# Calculate Wilson CIs for each stakeholder
stakeholder_ci <- stakeholder_analysis %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(systematic_count, n, conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("Stakeholder proportions with 95% Wilson CIs:")
print(stakeholder_ci %>% select(stakeholder, n, proportion, ci_text))

# Chi-square test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder, df$systematic_correlation_assessment)
  chi_stakeholder <- chisq.test(stakeholder_table)
  print("Chi-square test for stakeholder differences:")
  print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
  print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
  
  # Cramér's V
  cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * (min(dim(stakeholder_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Portfolio size relationship
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_analysis <- df %>%
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
      systematic_count = sum(systematic_correlation_assessment),
      proportion = round(mean(systematic_correlation_assessment) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(portfolio_category)
  
  print("Systematic assessment by portfolio size (n ≥ 10):")
  print(portfolio_analysis)
  
  # Point-biserial correlation
  cor_portfolio <- cor.test(df$systematic_correlation_assessment[!is.na(df$portfolio_size)],
                            df$portfolio_size[!is.na(df$portfolio_size)])
  
  print("Portfolio size × Systematic assessment correlation:")
  print(paste("r =", round(cor_portfolio$estimate, 3)))
  print(paste("95% CI: [", round(cor_portfolio$conf.int[1], 3), ", ", 
              round(cor_portfolio$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_portfolio$p.value, scientific = TRUE)))
}

# Risk events relationship
if(sum(!is.na(df$risk_events_experienced)) > 50) {
  risk_events_analysis <- df %>%
    filter(!is.na(risk_events_experienced)) %>%
    mutate(
      risk_events_category = ifelse(risk_events_experienced == 1,
                                   "Experienced Risk Events",
                                   "No Risk Events")
    ) %>%
    group_by(risk_events_category) %>%
    summarise(
      n = n(),
      systematic_count = sum(systematic_correlation_assessment),
      proportion = round(mean(systematic_correlation_assessment) * 100, 1),
      .groups = "drop"
    )
  
  print("Systematic assessment by risk event experience:")
  print(risk_events_analysis)
  
  # Fisher's exact test
  if(nrow(risk_events_analysis) == 2) {
    risk_table <- table(df$risk_events_experienced[!is.na(df$risk_events_experienced)],
                       df$systematic_correlation_assessment[!is.na(df$risk_events_experienced)])
    
    fisher_test <- fisher.test(risk_table)
    print("Risk events association (Fisher's exact test):")
    print(paste("p-value:", format(fisher_test$p.value, scientific = TRUE)))
    print(paste("Odds ratio:", round(fisher_test$estimate, 2)))
    print(paste("95% CI for OR: [", round(fisher_test$conf.int[1], 2), ", ",
                round(fisher_test$conf.int[2], 2), "]", sep=""))
  }
}

# Portfolio volatility relationship
if(sum(!is.na(df$portfolio_volatility)) > 50) {
  volatility_comparison <- df %>%
    filter(!is.na(portfolio_volatility)) %>%
    mutate(
      assessment_type = ifelse(systematic_correlation_assessment == 1,
                              "Systematic Assessment",
                              "No Systematic Assessment")
    ) %>%
    group_by(assessment_type) %>%
    summarise(
      n = n(),
      mean_volatility = round(mean(portfolio_volatility), 3),
      sd_volatility = round(sd(portfolio_volatility), 3),
      median_volatility = round(median(portfolio_volatility), 3),
      .groups = "drop"
    )
  
  print("Portfolio volatility by assessment type:")
  print(volatility_comparison)
  
  # t-test for volatility differences
  if(nrow(volatility_comparison) == 2) {
    systematic_vol <- df$portfolio_volatility[df$systematic_correlation_assessment == 1 & 
                                             !is.na(df$portfolio_volatility)]
    nonsystematic_vol <- df$portfolio_volatility[df$systematic_correlation_assessment == 0 & 
                                                !is.na(df$portfolio_volatility)]
    
    if(length(systematic_vol) >= 20 && length(nonsystematic_vol) >= 20) {
      t_test_vol <- t.test(systematic_vol, nonsystematic_vol)
      
      print("Portfolio volatility comparison (t-test):")
      print(paste("t =", round(t_test_vol$statistic, 2)))
      print(paste("df =", round(t_test_vol$parameter, 1)))
      print(paste("p-value:", format(t_test_vol$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test_vol$estimate), 3)))
      
      # Cohen's d
      pooled_sd <- sqrt(((length(systematic_vol) - 1) * var(systematic_vol) + 
                        (length(nonsystematic_vol) - 1) * var(nonsystematic_vol)) / 
                       (length(systematic_vol) + length(nonsystematic_vol) - 2))
      cohens_d <- (mean(systematic_vol) - mean(nonsystematic_vol)) / pooled_sd
      print(paste("Cohen's d =", round(cohens_d, 3)))
    }
  }
}

# General correlation risk management practices
if(sum(!is.na(df$correlation_risk_management)) > 50) {
  general_mgmt <- df %>%
    filter(!is.na(correlation_risk_management)) %>%
    summarise(
      n = n(),
      any_mgmt_count = sum(correlation_risk_management),
      any_mgmt_pct = round(mean(correlation_risk_management) * 100, 1)
    )
  
  print("General correlation risk management:")
  print(general_mgmt)
  
  # Relationship between general and systematic
  mgmt_comparison <- df %>%
    filter(!is.na(correlation_risk_management)) %>%
    group_by(correlation_risk_management) %>%
    summarise(
      n = n(),
      systematic_count = sum(systematic_correlation_assessment),
      systematic_pct = round(mean(systematic_correlation_assessment) * 100, 1),
      .groups = "drop"
    )
  
  print("Systematic assessment by general management practice:")
  print(mgmt_comparison)
  
  # Association test
  mgmt_table <- table(df$correlation_risk_management[!is.na(df$correlation_risk_management)],
                     df$systematic_correlation_assessment[!is.na(df$correlation_risk_management)])
  
  if(min(dim(mgmt_table)) == 2) {
    phi_coef <- sqrt(chisq.test(mgmt_table)$statistic / sum(mgmt_table))
    print(paste("Association (φ coefficient):", round(phi_coef, 3)))
  }
}

# Logistic regression model
if(nrow(df) > 150) {
  df_logistic <- df %>%
    filter(!is.na(portfolio_size), !is.na(stakeholder))
  
  if(nrow(df_logistic) > 100) {
    # Add risk events if available
    if(sum(!is.na(df_logistic$risk_events_experienced)) > 80) {
      logit_model <- glm(systematic_correlation_assessment ~ portfolio_size + 
                        stakeholder + risk_events_experienced,
                        data = df_logistic, family = binomial)
    } else {
      logit_model <- glm(systematic_correlation_assessment ~ portfolio_size + stakeholder,
                        data = df_logistic, family = binomial)
    }
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: Systematic assessment predictors")
    print(logit_summary)
    
    # Odds ratios with confidence intervals
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
    null_model <- glm(systematic_correlation_assessment ~ 1, 
                     data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

# Time series of adoption (if date data available)
if("response_date" %in% names(df) && sum(!is.na(df$response_date)) > 100) {
  df_time <- df %>%
    filter(!is.na(response_date)) %>%
    mutate(
      response_month = format(as.Date(response_date), "%Y-%m")
    ) %>%
    group_by(response_month) %>%
    summarise(
      n = n(),
      systematic_count = sum(systematic_correlation_assessment),
      proportion = round(mean(systematic_correlation_assessment) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(response_month)
  
  if(nrow(df_time) > 3) {
    print("Systematic assessment adoption over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(month_num = row_number())
    
    trend_cor <- cor.test(df_time$month_num, df_time$proportion, method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("Summary of correlation risk management:")
print(paste("Systematic assessment rate:", round(prop_systematic * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: systematic assessment by 44% (95% CI [40.6%, 47.4%])