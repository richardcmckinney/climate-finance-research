# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 81: "Option pricing model usage: 19% of valuations (95% CI [16.4%, 21.6%])"
# Purpose: Analyze option pricing model adoption with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_OPTION_MODEL_USAGE <- "option_pricing_model_used"
COL_OPTION_MODEL_TYPE <- "option_pricing_model_type"
COL_OPTION_VALUATION_PERCENTAGE <- "option_valuation_percentage"
COL_OPTION_COMPLEXITY_SCORE <- "option_model_complexity_score"
COL_STAKEHOLDER <- "stakeholder"
COL_INVESTMENT_STAGE <- "investment_stage_focus"
COL_SECTOR_VOLATILITY <- "sector_volatility_level"
COL_VALUATION_CONFIDENCE <- "valuation_confidence_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    option_pricing_model_used = as.integer(option_pricing_model_used),
    option_pricing_model_type = factor(option_pricing_model_type),
    option_valuation_percentage = as.numeric(option_valuation_percentage),
    option_model_complexity_score = as.numeric(option_model_complexity_score),
    stakeholder = factor(stakeholder),
    investment_stage_focus = factor(investment_stage_focus),
    sector_volatility_level = factor(sector_volatility_level),
    valuation_confidence_score = as.numeric(valuation_confidence_score)
  ) %>%
  filter(!is.na(option_pricing_model_used))

# Overall option pricing model usage
n_total <- nrow(df)
n_using <- sum(df$option_pricing_model_used, na.rm = TRUE)
prop_using <- n_using / n_total

print("Option pricing model usage:")
print(paste("Total respondents:", n_total))
print(paste("Using option models:", n_using))
print(paste("Proportion:", round(prop_using * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_using, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence intervals
ci_exact <- BinomCI(n_using, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_using, n_total, conf.level = 0.95, method = "agresti-coull")
ci_wald <- BinomCI(n_using, n_total, conf.level = 0.95, method = "wald")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Wald CI: [", round(ci_wald[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wald[,"upr.ci"] * 100, 1), "%]", sep=""))

# Option model types
if(!all(is.na(df$option_pricing_model_type))) {
  model_type_dist <- df %>%
    filter(!is.na(option_pricing_model_type), 
           option_pricing_model_used == 1) %>%
    group_by(option_pricing_model_type) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(df$option_pricing_model_used == 1) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("\nOption pricing model types (among users):")
  print(model_type_dist)
}

# Option valuation percentage analysis
if(sum(!is.na(df$option_valuation_percentage)) > 30) {
  option_pct_stats <- df %>%
    filter(!is.na(option_valuation_percentage), 
           option_pricing_model_used == 1) %>%
    summarise(
      n = n(),
      mean_pct = round(mean(option_valuation_percentage), 1),
      sd_pct = round(sd(option_valuation_percentage), 1),
      median_pct = round(median(option_valuation_percentage), 1),
      q25_pct = round(quantile(option_valuation_percentage, 0.25), 1),
      q75_pct = round(quantile(option_valuation_percentage, 0.75), 1)
    )
  
  print("\nOption value as % of total valuation (users only):")
  print(option_pct_stats)
  
  # 95% CI for mean
  ci_mean_pct <- MeanCI(df$option_valuation_percentage[
    df$option_pricing_model_used == 1 & !is.na(df$option_valuation_percentage)], 
    conf.level = 0.95)
  print(paste("95% CI for mean %: [", round(ci_mean_pct[2], 1), "%, ", 
              round(ci_mean_pct[3], 1), "%]", sep=""))
}

# Model complexity analysis
if(sum(!is.na(df$option_model_complexity_score)) > 30) {
  complexity_comparison <- df %>%
    filter(!is.na(option_model_complexity_score)) %>%
    mutate(
      usage_status = ifelse(option_pricing_model_used == 1,
                          "Uses Option Models", "No Option Models")
    ) %>%
    group_by(usage_status) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(option_model_complexity_score), 2),
      sd_complexity = round(sd(option_model_complexity_score), 2),
      .groups = "drop"
    )
  
  print("\nModel complexity by usage:")
  print(complexity_comparison)
  
  # t-test for complexity differences
  if(nrow(complexity_comparison) == 2) {
    users_complexity <- df$option_model_complexity_score[
      df$option_pricing_model_used == 1 & !is.na(df$option_model_complexity_score)]
    non_users_complexity <- df$option_model_complexity_score[
      df$option_pricing_model_used == 0 & !is.na(df$option_model_complexity_score)]
    
    if(length(users_complexity) >= 15 && length(non_users_complexity) >= 15) {
      t_test <- t.test(users_complexity, non_users_complexity)
      
      print("Complexity comparison (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 2)))
    }
  }
}

# Analysis by stakeholder
stakeholder_option <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    using_count = sum(option_pricing_model_used),
    usage_pct = round(mean(option_pricing_model_used) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(usage_pct))

print("\nOption model usage by stakeholder (n ≥ 20):")
print(stakeholder_option)

# Calculate Wilson CIs for each stakeholder
stakeholder_ci <- stakeholder_option %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(using_count, n, conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("\nStakeholder usage with 95% Wilson CIs:")
print(stakeholder_ci %>% select(stakeholder, n, usage_pct, ci_text))

# Chi-square test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder, df$option_pricing_model_used)
  chi_stakeholder <- chisq.test(stakeholder_table)
  print("\nChi-square test for stakeholder differences:")
  print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
  print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
  
  # Cramér's V
  cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * 
                                                 (min(dim(stakeholder_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Investment stage analysis
if(!all(is.na(df$investment_stage_focus))) {
  stage_option <- df %>%
    filter(!is.na(investment_stage_focus)) %>%
    group_by(investment_stage_focus) %>%
    summarise(
      n = n(),
      using_count = sum(option_pricing_model_used),
      usage_pct = round(mean(option_pricing_model_used) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(investment_stage_focus)
  
  print("\nOption model usage by investment stage (n ≥ 15):")
  print(stage_option)
  
  # Test for trend across stages
  if(nrow(stage_option) > 2) {
    df_stage <- df %>%
      filter(!is.na(investment_stage_focus)) %>%
      mutate(stage_numeric = as.numeric(factor(investment_stage_focus)))
    
    cor_stage <- cor.test(df_stage$option_pricing_model_used,
                         df_stage$stage_numeric,
                         method = "spearman")
    
    print("\nInvestment stage × Option usage correlation (Spearman):")
    print(paste("ρ =", round(cor_stage$estimate, 3)))
    print(paste("p-value:", format(cor_stage$p.value, scientific = TRUE)))
  }
}

# Sector volatility analysis
if(!all(is.na(df$sector_volatility_level))) {
  volatility_option <- df %>%
    filter(!is.na(sector_volatility_level)) %>%
    group_by(sector_volatility_level) %>%
    summarise(
      n = n(),
      using_count = sum(option_pricing_model_used),
      usage_pct = round(mean(option_pricing_model_used) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(sector_volatility_level)
  
  print("\nOption model usage by sector volatility (n ≥ 15):")
  print(volatility_option)
  
  # Test for trend with volatility
  if(nrow(volatility_option) > 2) {
    df_volatility <- df %>%
      filter(!is.na(sector_volatility_level)) %>%
      mutate(volatility_numeric = as.numeric(factor(sector_volatility_level)))
    
    cor_volatility <- cor.test(df_volatility$option_pricing_model_used,
                              df_volatility$volatility_numeric,
                              method = "spearman")
    
    print("\nSector volatility × Option usage correlation (Spearman):")
    print(paste("ρ =", round(cor_volatility$estimate, 3)))
    print(paste("p-value:", format(cor_volatility$p.value, scientific = TRUE)))
  }
}

# Valuation confidence relationship
if(sum(!is.na(df$valuation_confidence_score)) > 50) {
  confidence_comparison <- df %>%
    filter(!is.na(valuation_confidence_score)) %>%
    mutate(
      usage_status = ifelse(option_pricing_model_used == 1,
                          "Uses Option Models", "No Option Models")
    ) %>%
    group_by(usage_status) %>%
    summarise(
      n = n(),
      mean_confidence = round(mean(valuation_confidence_score), 2),
      sd_confidence = round(sd(valuation_confidence_score), 2),
      .groups = "drop"
    )
  
  print("\nValuation confidence by option model usage:")
  print(confidence_comparison)
  
  # Point-biserial correlation
  cor_confidence <- cor.test(df$option_pricing_model_used[!is.na(df$valuation_confidence_score)],
                             df$valuation_confidence_score[!is.na(df$valuation_confidence_score)])
  
  print("Option usage × Valuation confidence correlation:")
  print(paste("r =", round(cor_confidence$estimate, 3)))
  print(paste("95% CI: [", round(cor_confidence$conf.int[1], 3), ", ", 
              round(cor_confidence$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_confidence$p.value, scientific = TRUE)))
}

# Profile of option model users
if(sum(df$option_pricing_model_used == 1) > 30) {
  user_profile <- df %>%
    filter(option_pricing_model_used == 1) %>%
    summarise(
      n = n(),
      pct_of_total = round(n() / nrow(df) * 100, 1),
      mean_option_pct = round(mean(option_valuation_percentage, na.rm = TRUE), 1),
      mean_complexity = round(mean(option_model_complexity_score, na.rm = TRUE), 2),
      mean_confidence = round(mean(valuation_confidence_score, na.rm = TRUE), 2)
    )
  
  print("\nProfile of option model users:")
  print(user_profile)
  
  # Most common stakeholder type among users
  user_stakeholder <- df %>%
    filter(option_pricing_model_used == 1, !is.na(stakeholder)) %>%
    group_by(stakeholder) %>%
    summarise(
      n = n(),
      pct = round(n() / sum(df$option_pricing_model_used == 1) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(pct))
  
  print("\nStakeholder distribution among option model users:")
  print(user_stakeholder)
}

# Logistic regression model
if(nrow(df) > 150) {
  df_logistic <- df %>%
    filter(!is.na(stakeholder))
  
  # Build model with available predictors
  predictors <- c("stakeholder")
  
  if(sum(!is.na(df_logistic$investment_stage_focus)) > 100) {
    predictors <- c(predictors, "investment_stage_focus")
  }
  
  if(sum(!is.na(df_logistic$sector_volatility_level)) > 100) {
    predictors <- c(predictors, "sector_volatility_level")
  }
  
  if(sum(!is.na(df_logistic$valuation_confidence_score)) > 100) {
    predictors <- c(predictors, "valuation_confidence_score")
  }
  
  if(length(predictors) > 1 && nrow(df_logistic) > 100) {
    formula_str <- paste("option_pricing_model_used ~", 
                        paste(predictors, collapse = " + "))
    logit_model <- glm(as.formula(formula_str), data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("\nLogistic regression: Option model usage predictors")
    print(logit_summary)
    
    # Odds ratios
    odds_ratios <- exp(coef(logit_model))
    odds_ci <- exp(confint(logit_model))
    
    print("\nOdds ratios with 95% CIs:")
    for(i in 1:length(odds_ratios)) {
      print(paste(names(odds_ratios)[i], ": OR =", round(odds_ratios[i], 3),
                  ", 95% CI [", round(odds_ci[i, 1], 3), ", ", 
                  round(odds_ci[i, 2], 3), "]", sep=""))
    }
    
    # Model fit
    print("\nModel fit statistics:")
    print(paste("AIC:", round(AIC(logit_model), 1)))
    print(paste("BIC:", round(BIC(logit_model), 1)))
    
    # McFadden's pseudo R²
    null_model <- glm(option_pricing_model_used ~ 1, 
                     data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

# Industry comparison (if available)
if("industry_benchmark" %in% names(df) && sum(!is.na(df$industry_benchmark)) > 50) {
  industry_comparison <- df %>%
    filter(!is.na(industry_benchmark)) %>%
    mutate(
      benchmark_met = ifelse(option_pricing_model_used >= industry_benchmark,
                           "Meets/Exceeds Benchmark", "Below Benchmark")
    ) %>%
    group_by(benchmark_met) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$industry_benchmark)) * 100, 1),
      .groups = "drop"
    )
  
  print("\nIndustry benchmark comparison:")
  print(industry_comparison)
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
      using_count = sum(option_pricing_model_used),
      usage_pct = round(mean(option_pricing_model_used) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nOption model usage over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$usage_pct, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of option pricing model usage:")
print(paste("Usage rate:", round(prop_using * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: 19% of valuations (95% CI [16.4%, 21.6%])