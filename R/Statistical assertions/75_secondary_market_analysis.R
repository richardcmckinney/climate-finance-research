# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 75: "Secondary market analysis: participation by 29% (95% CI [25.9%, 32.1%])"
# Purpose: Analyze secondary market participation with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SECONDARY_MARKET_PARTICIPATION <- "secondary_market_participation"
COL_SECONDARY_TRANSACTION_COUNT <- "secondary_transactions_count"
COL_SECONDARY_VOLUME_PERCENTAGE <- "secondary_volume_percentage"
COL_SECONDARY_MARKET_STRATEGY <- "secondary_market_strategy"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size"
COL_FUND_VINTAGE <- "fund_vintage_year"
COL_LIQUIDITY_NEEDS <- "liquidity_needs_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    secondary_market_participation = as.integer(secondary_market_participation),
    secondary_transactions_count = as.numeric(secondary_transactions_count),
    secondary_volume_percentage = as.numeric(secondary_volume_percentage),
    secondary_market_strategy = factor(secondary_market_strategy),
    stakeholder = factor(stakeholder),
    portfolio_size = as.numeric(portfolio_size),
    fund_vintage_year = as.numeric(fund_vintage_year),
    liquidity_needs_score = as.numeric(liquidity_needs_score)
  ) %>%
  filter(!is.na(secondary_market_participation))

# Overall secondary market participation
n_total <- nrow(df)
n_participating <- sum(df$secondary_market_participation, na.rm = TRUE)
prop_participating <- n_participating / n_total

print("Secondary market participation:")
print(paste("Total respondents:", n_total))
print(paste("Participating:", n_participating))
print(paste("Proportion:", round(prop_participating * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_participating, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence intervals
ci_exact <- BinomCI(n_participating, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_participating, n_total, conf.level = 0.95, method = "agresti-coull")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))

# Transaction count analysis
if(sum(!is.na(df$secondary_transactions_count)) > 30) {
  transaction_stats <- df %>%
    filter(!is.na(secondary_transactions_count), 
           secondary_market_participation == 1) %>%
    summarise(
      n = n(),
      mean_transactions = round(mean(secondary_transactions_count), 1),
      sd_transactions = round(sd(secondary_transactions_count), 1),
      median_transactions = round(median(secondary_transactions_count), 0),
      max_transactions = max(secondary_transactions_count)
    )
  
  print("Secondary transaction counts (participants only):")
  print(transaction_stats)
  
  # Transaction count categories
  transaction_categories <- df %>%
    filter(!is.na(secondary_transactions_count), 
           secondary_market_participation == 1) %>%
    mutate(
      transaction_category = case_when(
        secondary_transactions_count == 0 ~ "None",
        secondary_transactions_count <= 2 ~ "Low (1-2)",
        secondary_transactions_count <= 5 ~ "Moderate (3-5)",
        secondary_transactions_count <= 10 ~ "High (6-10)",
        secondary_transactions_count > 10 ~ "Very High (>10)"
      )
    ) %>%
    group_by(transaction_category) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(df$secondary_market_participation == 1) * 100, 1),
      .groups = "drop"
    )
  
  print("Transaction frequency distribution:")
  print(transaction_categories)
}

# Secondary volume analysis
if(sum(!is.na(df$secondary_volume_percentage)) > 30) {
  volume_stats <- df %>%
    filter(!is.na(secondary_volume_percentage), 
           secondary_market_participation == 1) %>%
    summarise(
      n = n(),
      mean_volume_pct = round(mean(secondary_volume_percentage), 1),
      sd_volume_pct = round(sd(secondary_volume_percentage), 1),
      median_volume_pct = round(median(secondary_volume_percentage), 1),
      q25_volume_pct = round(quantile(secondary_volume_percentage, 0.25), 1),
      q75_volume_pct = round(quantile(secondary_volume_percentage, 0.75), 1)
    )
  
  print("Secondary market volume as % of portfolio (participants only):")
  print(volume_stats)
}

# Secondary market strategy analysis
if(!all(is.na(df$secondary_market_strategy))) {
  strategy_breakdown <- df %>%
    filter(!is.na(secondary_market_strategy), 
           secondary_market_participation == 1) %>%
    group_by(secondary_market_strategy) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(df$secondary_market_participation == 1) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("Secondary market strategies (participants only):")
  print(strategy_breakdown)
}

# Analysis by stakeholder
stakeholder_secondary <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    participating_count = sum(secondary_market_participation),
    participation_rate = round(mean(secondary_market_participation) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(participation_rate))

print("Secondary market participation by stakeholder (n ≥ 20):")
print(stakeholder_secondary)

# Calculate Wilson CIs for each stakeholder
stakeholder_ci <- stakeholder_secondary %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(participating_count, n, conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("Stakeholder participation with 95% Wilson CIs:")
print(stakeholder_ci %>% select(stakeholder, n, participation_rate, ci_text))

# Chi-square test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder, df$secondary_market_participation)
  chi_stakeholder <- chisq.test(stakeholder_table)
  print("Chi-square test for stakeholder differences:")
  print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
  print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
  
  # Cramér's V
  cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * 
                                                 (min(dim(stakeholder_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Portfolio size relationship
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_secondary <- df %>%
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
      participating_count = sum(secondary_market_participation),
      participation_rate = round(mean(secondary_market_participation) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Secondary participation by portfolio size (n ≥ 10):")
  print(portfolio_secondary)
  
  # Point-biserial correlation
  cor_portfolio <- cor.test(df$secondary_market_participation[!is.na(df$portfolio_size)],
                            df$portfolio_size[!is.na(df$portfolio_size)])
  
  print("Portfolio size × Secondary participation correlation:")
  print(paste("r =", round(cor_portfolio$estimate, 3)))
  print(paste("95% CI: [", round(cor_portfolio$conf.int[1], 3), ", ", 
              round(cor_portfolio$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_portfolio$p.value, scientific = TRUE)))
}

# Fund vintage analysis
if(sum(!is.na(df$fund_vintage_year)) > 50) {
  # Calculate fund age
  current_year <- as.numeric(format(Sys.Date(), "%Y"))
  df_vintage <- df %>%
    filter(!is.na(fund_vintage_year)) %>%
    mutate(fund_age = current_year - fund_vintage_year)
  
  vintage_secondary <- df_vintage %>%
    mutate(
      vintage_category = case_when(
        fund_age <= 3 ~ "New (≤3 years)",
        fund_age <= 6 ~ "Developing (4-6 years)",
        fund_age <= 10 ~ "Mature (7-10 years)",
        fund_age > 10 ~ "Very Mature (>10 years)"
      )
    ) %>%
    group_by(vintage_category) %>%
    summarise(
      n = n(),
      participating_count = sum(secondary_market_participation),
      participation_rate = round(mean(secondary_market_participation) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Secondary participation by fund age (n ≥ 10):")
  print(vintage_secondary)
  
  # Correlation with fund age
  cor_age <- cor.test(df_vintage$secondary_market_participation, df_vintage$fund_age)
  
  print("Fund age × Secondary participation correlation:")
  print(paste("r =", round(cor_age$estimate, 3)))
  print(paste("p-value:", format(cor_age$p.value, scientific = TRUE)))
}

# Liquidity needs relationship
if(sum(!is.na(df$liquidity_needs_score)) > 50) {
  liquidity_comparison <- df %>%
    filter(!is.na(liquidity_needs_score)) %>%
    mutate(
      participation_status = ifelse(secondary_market_participation == 1,
                                  "Secondary Participant", "Non-Participant")
    ) %>%
    group_by(participation_status) %>%
    summarise(
      n = n(),
      mean_liquidity_needs = round(mean(liquidity_needs_score), 2),
      sd_liquidity_needs = round(sd(liquidity_needs_score), 2),
      .groups = "drop"
    )
  
  print("Liquidity needs by secondary participation:")
  print(liquidity_comparison)
  
  # t-test for liquidity needs
  if(nrow(liquidity_comparison) == 2) {
    participant_liquidity <- df$liquidity_needs_score[df$secondary_market_participation == 1 & 
                                                     !is.na(df$liquidity_needs_score)]
    non_participant_liquidity <- df$liquidity_needs_score[df$secondary_market_participation == 0 & 
                                                         !is.na(df$liquidity_needs_score)]
    
    if(length(participant_liquidity) >= 15 && length(non_participant_liquidity) >= 15) {
      t_test <- t.test(participant_liquidity, non_participant_liquidity)
      
      print("Liquidity needs comparison (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 2)))
      
      # Cohen's d
      pooled_sd <- sqrt(((length(participant_liquidity) - 1) * var(participant_liquidity) + 
                        (length(non_participant_liquidity) - 1) * var(non_participant_liquidity)) / 
                       (length(participant_liquidity) + length(non_participant_liquidity) - 2))
      cohens_d <- (mean(participant_liquidity) - mean(non_participant_liquidity)) / pooled_sd
      print(paste("Cohen's d =", round(cohens_d, 3)))
    }
  }
}

# Combined analysis of participants
if(sum(df$secondary_market_participation == 1) > 50) {
  participant_profile <- df %>%
    filter(secondary_market_participation == 1) %>%
    summarise(
      n = n(),
      mean_portfolio_size = round(mean(portfolio_size, na.rm = TRUE), 1),
      median_transactions = round(median(secondary_transactions_count, na.rm = TRUE), 0),
      mean_volume_pct = round(mean(secondary_volume_percentage, na.rm = TRUE), 1),
      mean_liquidity_needs = round(mean(liquidity_needs_score, na.rm = TRUE), 2)
    )
  
  print("Profile of secondary market participants:")
  print(participant_profile)
}

# Logistic regression model
if(nrow(df) > 150) {
  df_logistic <- df %>%
    filter(!is.na(portfolio_size), !is.na(stakeholder))
  
  if(nrow(df_logistic) > 100) {
    # Build model with available predictors
    predictors <- c("portfolio_size", "stakeholder")
    
    if(sum(!is.na(df_logistic$liquidity_needs_score)) > 80) {
      predictors <- c(predictors, "liquidity_needs_score")
    }
    
    if(sum(!is.na(df_logistic$fund_vintage_year)) > 80) {
      df_logistic$fund_age <- current_year - df_logistic$fund_vintage_year
      predictors <- c(predictors, "fund_age")
    }
    
    formula_str <- paste("secondary_market_participation ~", 
                        paste(predictors, collapse = " + "))
    logit_model <- glm(as.formula(formula_str), data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: Secondary market participation predictors")
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
    null_model <- glm(secondary_market_participation ~ 1, 
                     data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

# Activity level analysis
activity_levels <- df %>%
  mutate(
    activity_level = case_when(
      secondary_market_participation == 0 ~ "No Participation",
      !is.na(secondary_transactions_count) & secondary_transactions_count == 0 ~ "Registered but Inactive",
      !is.na(secondary_transactions_count) & secondary_transactions_count <= 3 ~ "Low Activity",
      !is.na(secondary_transactions_count) & secondary_transactions_count <= 10 ~ "Moderate Activity",
      !is.na(secondary_transactions_count) & secondary_transactions_count > 10 ~ "High Activity",
      TRUE ~ "Participation Unknown"
    )
  ) %>%
  group_by(activity_level) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  )

print("Secondary market activity levels:")
print(activity_levels)

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
      participating_count = sum(secondary_market_participation),
      participation_rate = round(mean(secondary_market_participation) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("Secondary market participation over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$participation_rate, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("Summary of secondary market analysis:")
print(paste("Participation rate:", round(prop_participating * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: participation by 29% (95% CI [25.9%, 32.1%])