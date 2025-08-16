# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 72: "Liquidity risk assessment: quarterly reviews by 62% (95% CI [58.6%, 65.3%])"
# Purpose: Analyze liquidity risk assessment frequency with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_LIQUIDITY_REVIEW_FREQUENCY <- "liquidity_review_frequency"
COL_LIQUIDITY_RISK_SCORE <- "liquidity_risk_score"
COL_LIQUIDITY_BUFFER_PCT <- "liquidity_buffer_percentage"
COL_CASH_RESERVES_MONTHS <- "cash_reserves_months"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size"
COL_FUND_SIZE <- "fund_size_millions"
COL_EXIT_HORIZON <- "average_exit_horizon_years"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    liquidity_review_frequency = factor(liquidity_review_frequency),
    liquidity_risk_score = as.numeric(liquidity_risk_score),
    liquidity_buffer_percentage = as.numeric(liquidity_buffer_percentage),
    cash_reserves_months = as.numeric(cash_reserves_months),
    stakeholder = factor(stakeholder),
    portfolio_size = as.numeric(portfolio_size),
    fund_size_millions = as.numeric(fund_size_millions),
    average_exit_horizon_years = as.numeric(average_exit_horizon_years)
  )

# Create binary variable for quarterly or more frequent reviews
df <- df %>%
  mutate(
    quarterly_or_more = as.integer(liquidity_review_frequency %in% 
                                  c("Monthly", "Quarterly", "Bi-monthly"))
  )

# Calculate proportion with quarterly reviews
n_total <- sum(!is.na(df$quarterly_or_more))
n_quarterly <- sum(df$quarterly_or_more, na.rm = TRUE)
prop_quarterly <- n_quarterly / n_total

print("Liquidity risk assessment - Quarterly or more frequent reviews:")
print(paste("Total respondents:", n_total))
print(paste("With quarterly+ reviews:", n_quarterly))
print(paste("Proportion:", round(prop_quarterly * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_quarterly, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence intervals
ci_exact <- BinomCI(n_quarterly, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_quarterly, n_total, conf.level = 0.95, method = "agresti-coull")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))

# Detailed frequency breakdown
frequency_breakdown <- df %>%
  filter(!is.na(liquidity_review_frequency)) %>%
  group_by(liquidity_review_frequency) %>%
  summarise(
    n = n(),
    percentage = round(n() / sum(!is.na(df$liquidity_review_frequency)) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    liquidity_review_frequency = factor(liquidity_review_frequency,
                                      levels = c("Daily", "Weekly", "Bi-weekly", 
                                               "Monthly", "Quarterly", "Semi-annually", 
                                               "Annually", "As needed"))
  ) %>%
  arrange(liquidity_review_frequency)

print("Detailed liquidity review frequency distribution:")
print(frequency_breakdown)

# Calculate Wilson CIs for each frequency
frequency_ci <- frequency_breakdown %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(n, sum(frequency_breakdown$n), 
                          conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("Frequency distribution with 95% Wilson CIs:")
print(frequency_ci %>% select(liquidity_review_frequency, n, percentage, ci_text))

# Analysis by stakeholder
stakeholder_liquidity <- df %>%
  filter(!is.na(stakeholder), !is.na(quarterly_or_more)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    quarterly_count = sum(quarterly_or_more),
    quarterly_pct = round(mean(quarterly_or_more) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(quarterly_pct))

print("Quarterly+ reviews by stakeholder (n ≥ 20):")
print(stakeholder_liquidity)

# Chi-square test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder[!is.na(df$quarterly_or_more)],
                            df$quarterly_or_more[!is.na(df$quarterly_or_more)])
  
  if(min(dim(stakeholder_table)) > 1) {
    chi_stakeholder <- chisq.test(stakeholder_table)
    print("Chi-square test for stakeholder differences:")
    print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
    print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
    
    # Cramér's V
    cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * 
                                                   (min(dim(stakeholder_table)) - 1)))
    print(paste("Cramér's V =", round(cramers_v, 3)))
  }
}

# Liquidity risk score analysis
if(sum(!is.na(df$liquidity_risk_score)) > 50) {
  risk_score_stats <- df %>%
    filter(!is.na(liquidity_risk_score)) %>%
    summarise(
      n = n(),
      mean_score = round(mean(liquidity_risk_score), 2),
      sd_score = round(sd(liquidity_risk_score), 2),
      median_score = round(median(liquidity_risk_score), 2),
      q25_score = round(quantile(liquidity_risk_score, 0.25), 2),
      q75_score = round(quantile(liquidity_risk_score, 0.75), 2)
    )
  
  print("Liquidity risk score statistics:")
  print(risk_score_stats)
  
  # Comparison by review frequency
  score_by_frequency <- df %>%
    filter(!is.na(liquidity_risk_score), !is.na(quarterly_or_more)) %>%
    mutate(
      review_category = ifelse(quarterly_or_more == 1,
                              "Quarterly or More", "Less than Quarterly")
    ) %>%
    group_by(review_category) %>%
    summarise(
      n = n(),
      mean_score = round(mean(liquidity_risk_score), 2),
      sd_score = round(sd(liquidity_risk_score), 2),
      median_score = round(median(liquidity_risk_score), 2),
      .groups = "drop"
    )
  
  print("Liquidity risk scores by review frequency:")
  print(score_by_frequency)
  
  # t-test for score differences
  if(nrow(score_by_frequency) == 2) {
    quarterly_scores <- df$liquidity_risk_score[df$quarterly_or_more == 1 & 
                                                !is.na(df$liquidity_risk_score)]
    less_frequent_scores <- df$liquidity_risk_score[df$quarterly_or_more == 0 & 
                                                   !is.na(df$liquidity_risk_score)]
    
    if(length(quarterly_scores) >= 20 && length(less_frequent_scores) >= 20) {
      t_test <- t.test(quarterly_scores, less_frequent_scores)
      
      print("Risk score comparison (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 2)))
      
      # Cohen's d
      pooled_sd <- sqrt(((length(quarterly_scores) - 1) * var(quarterly_scores) + 
                        (length(less_frequent_scores) - 1) * var(less_frequent_scores)) / 
                       (length(quarterly_scores) + length(less_frequent_scores) - 2))
      cohens_d <- (mean(quarterly_scores) - mean(less_frequent_scores)) / pooled_sd
      print(paste("Cohen's d =", round(cohens_d, 3)))
    }
  }
}

# Liquidity buffer analysis
if(sum(!is.na(df$liquidity_buffer_percentage)) > 50) {
  buffer_comparison <- df %>%
    filter(!is.na(liquidity_buffer_percentage), !is.na(quarterly_or_more)) %>%
    mutate(
      review_category = ifelse(quarterly_or_more == 1,
                              "Quarterly or More", "Less than Quarterly")
    ) %>%
    group_by(review_category) %>%
    summarise(
      n = n(),
      mean_buffer = round(mean(liquidity_buffer_percentage), 1),
      sd_buffer = round(sd(liquidity_buffer_percentage), 1),
      median_buffer = round(median(liquidity_buffer_percentage), 1),
      .groups = "drop"
    )
  
  print("Liquidity buffer by review frequency:")
  print(buffer_comparison)
  
  # Correlation between buffer and review frequency
  cor_buffer <- cor.test(df$quarterly_or_more[!is.na(df$liquidity_buffer_percentage)],
                         df$liquidity_buffer_percentage[!is.na(df$liquidity_buffer_percentage)])
  
  print("Review frequency × Liquidity buffer correlation:")
  print(paste("r =", round(cor_buffer$estimate, 3)))
  print(paste("p-value:", format(cor_buffer$p.value, scientific = TRUE)))
}

# Cash reserves analysis
if(sum(!is.na(df$cash_reserves_months)) > 50) {
  reserves_comparison <- df %>%
    filter(!is.na(cash_reserves_months), !is.na(quarterly_or_more)) %>%
    mutate(
      review_category = ifelse(quarterly_or_more == 1,
                              "Quarterly or More", "Less than Quarterly")
    ) %>%
    group_by(review_category) %>%
    summarise(
      n = n(),
      mean_reserves = round(mean(cash_reserves_months), 1),
      sd_reserves = round(sd(cash_reserves_months), 1),
      median_reserves = round(median(cash_reserves_months), 1),
      .groups = "drop"
    )
  
  print("Cash reserves (months) by review frequency:")
  print(reserves_comparison)
}

# Fund size relationship
if(sum(!is.na(df$fund_size_millions)) > 50) {
  fund_size_analysis <- df %>%
    filter(!is.na(fund_size_millions), !is.na(quarterly_or_more)) %>%
    mutate(
      fund_category = case_when(
        fund_size_millions <= 50 ~ "Small (<$50M)",
        fund_size_millions <= 250 ~ "Medium ($50-250M)",
        fund_size_millions <= 1000 ~ "Large ($250M-1B)",
        fund_size_millions > 1000 ~ "Very Large (>$1B)"
      )
    ) %>%
    group_by(fund_category) %>%
    summarise(
      n = n(),
      quarterly_count = sum(quarterly_or_more),
      quarterly_pct = round(mean(quarterly_or_more) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Quarterly+ reviews by fund size (n ≥ 10):")
  print(fund_size_analysis)
  
  # Correlation with continuous fund size
  cor_fund <- cor.test(df$quarterly_or_more[!is.na(df$fund_size_millions)],
                       log10(df$fund_size_millions[!is.na(df$fund_size_millions)] + 1))
  
  print("Fund size (log) × Quarterly review correlation:")
  print(paste("r =", round(cor_fund$estimate, 3)))
  print(paste("p-value:", format(cor_fund$p.value, scientific = TRUE)))
}

# Portfolio size relationship
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_liquidity <- df %>%
    filter(!is.na(portfolio_size), !is.na(quarterly_or_more)) %>%
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
      quarterly_count = sum(quarterly_or_more),
      quarterly_pct = round(mean(quarterly_or_more) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Quarterly+ reviews by portfolio size (n ≥ 10):")
  print(portfolio_liquidity)
  
  # Point-biserial correlation
  cor_portfolio <- cor.test(df$quarterly_or_more[!is.na(df$portfolio_size)],
                            df$portfolio_size[!is.na(df$portfolio_size)])
  
  print("Portfolio size × Quarterly review correlation:")
  print(paste("r =", round(cor_portfolio$estimate, 3)))
  print(paste("95% CI: [", round(cor_portfolio$conf.int[1], 3), ", ", 
              round(cor_portfolio$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_portfolio$p.value, scientific = TRUE)))
}

# Exit horizon relationship
if(sum(!is.na(df$average_exit_horizon_years)) > 50) {
  exit_horizon_analysis <- df %>%
    filter(!is.na(average_exit_horizon_years), !is.na(quarterly_or_more)) %>%
    mutate(
      horizon_category = case_when(
        average_exit_horizon_years <= 3 ~ "Short (≤3 years)",
        average_exit_horizon_years <= 5 ~ "Medium (4-5 years)",
        average_exit_horizon_years <= 7 ~ "Long (6-7 years)",
        average_exit_horizon_years > 7 ~ "Very Long (>7 years)"
      )
    ) %>%
    group_by(horizon_category) %>%
    summarise(
      n = n(),
      quarterly_count = sum(quarterly_or_more),
      quarterly_pct = round(mean(quarterly_or_more) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Quarterly+ reviews by exit horizon (n ≥ 10):")
  print(exit_horizon_analysis)
  
  # Correlation test
  cor_horizon <- cor.test(df$quarterly_or_more[!is.na(df$average_exit_horizon_years)],
                          df$average_exit_horizon_years[!is.na(df$average_exit_horizon_years)])
  
  print("Exit horizon × Quarterly review correlation:")
  print(paste("r =", round(cor_horizon$estimate, 3)))
  print(paste("p-value:", format(cor_horizon$p.value, scientific = TRUE)))
}

# Logistic regression model
if(nrow(df) > 150) {
  df_logistic <- df %>%
    filter(!is.na(quarterly_or_more), !is.na(portfolio_size), !is.na(stakeholder))
  
  if(nrow(df_logistic) > 100) {
    # Build model with available predictors
    predictors <- c("portfolio_size", "stakeholder")
    
    if(sum(!is.na(df_logistic$fund_size_millions)) > 80) {
      df_logistic$log_fund_size <- log10(df_logistic$fund_size_millions + 1)
      predictors <- c(predictors, "log_fund_size")
    }
    
    if(sum(!is.na(df_logistic$liquidity_risk_score)) > 80) {
      predictors <- c(predictors, "liquidity_risk_score")
    }
    
    formula_str <- paste("quarterly_or_more ~", paste(predictors, collapse = " + "))
    logit_model <- glm(as.formula(formula_str), data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: Predictors of quarterly+ reviews")
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
    null_model <- glm(quarterly_or_more ~ 1, data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

# Review frequency combinations
if(!all(is.na(df$liquidity_review_frequency))) {
  # Group into broader categories
  frequency_groups <- df %>%
    filter(!is.na(liquidity_review_frequency)) %>%
    mutate(
      frequency_group = case_when(
        liquidity_review_frequency %in% c("Daily", "Weekly") ~ "Very Frequent",
        liquidity_review_frequency %in% c("Bi-weekly", "Monthly") ~ "Frequent",
        liquidity_review_frequency == "Quarterly" ~ "Quarterly",
        liquidity_review_frequency %in% c("Semi-annually", "Annually") ~ "Infrequent",
        TRUE ~ "Ad hoc"
      )
    ) %>%
    group_by(frequency_group) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$liquidity_review_frequency)) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      frequency_group = factor(frequency_group,
                              levels = c("Very Frequent", "Frequent", "Quarterly",
                                       "Infrequent", "Ad hoc"))
    ) %>%
    arrange(frequency_group)
  
  print("Grouped review frequency distribution:")
  print(frequency_groups)
}

# Time trend analysis (if date data available)
if("response_date" %in% names(df) && sum(!is.na(df$response_date)) > 100) {
  df_time <- df %>%
    filter(!is.na(response_date), !is.na(quarterly_or_more)) %>%
    mutate(
      response_quarter = paste0(year(as.Date(response_date)), "-Q",
                               quarter(as.Date(response_date)))
    ) %>%
    group_by(response_quarter) %>%
    summarise(
      n = n(),
      quarterly_count = sum(quarterly_or_more),
      quarterly_pct = round(mean(quarterly_or_more) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("Quarterly+ review adoption over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$quarterly_pct, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("Summary of liquidity risk assessment:")
print(paste("Quarterly or more frequent reviews:", round(prop_quarterly * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: quarterly reviews by 62% (95% CI [58.6%, 65.3%])