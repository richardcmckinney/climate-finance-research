# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 68: "Modern portfolio theory applied by 67% (95% CI [63.7%, 70.2%])"
# Purpose: Proportion analysis with Wilson confidence intervals for MPT application

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MPT_APPLICATION <- "modern_portfolio_theory_applied"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size"
COL_EXPERIENCE_YEARS <- "experience_years"
COL_OPTIMIZATION_METHOD <- "optimization_method"
COL_RISK_ADJUSTED_RETURNS <- "risk_adjusted_returns"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    modern_portfolio_theory_applied = as.integer(modern_portfolio_theory_applied),
    stakeholder = factor(stakeholder),
    portfolio_size = as.numeric(portfolio_size),
    experience_years = as.numeric(experience_years),
    optimization_method = factor(optimization_method),
    risk_adjusted_returns = as.numeric(risk_adjusted_returns)
  ) %>%
  filter(!is.na(modern_portfolio_theory_applied))

# Overall proportion analysis
n_total <- nrow(df)
n_mpt <- sum(df$modern_portfolio_theory_applied, na.rm = TRUE)
prop_mpt <- n_mpt / n_total

print("Overall Modern Portfolio Theory application:")
print(paste("Total respondents:", n_total))
print(paste("Applying MPT:", n_mpt))
print(paste("Proportion:", round(prop_mpt * 100, 1), "%"))

# Wilson confidence interval for overall proportion
ci_wilson <- BinomCI(n_mpt, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Additional confidence interval methods for robustness
ci_exact <- BinomCI(n_mpt, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_wald <- BinomCI(n_mpt, n_total, conf.level = 0.95, method = "wald")
ci_agresti <- BinomCI(n_mpt, n_total, conf.level = 0.95, method = "agresti-coull")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Wald CI: [", round(ci_wald[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wald[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))

# Breakdown by stakeholder
stakeholder_breakdown <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mpt_count = sum(modern_portfolio_theory_applied, na.rm = TRUE),
    proportion = round(mean(modern_portfolio_theory_applied, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(proportion))

print("MPT application by stakeholder:")
print(stakeholder_breakdown)

# Statistical test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder, df$modern_portfolio_theory_applied)
  chi_stakeholder <- chisq.test(stakeholder_table)
  print("Chi-square test for stakeholder variation:")
  print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
  print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
  
  # Cramér's V for effect size
  cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * (min(dim(stakeholder_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Analysis by portfolio size
if(sum(!is.na(df$portfolio_size)) > 50) {
  df_portfolio <- df %>%
    filter(!is.na(portfolio_size))
  
  # Categorize portfolio sizes
  portfolio_analysis <- df_portfolio %>%
    mutate(
      portfolio_category = case_when(
        portfolio_size <= 10 ~ "Small (≤10)",
        portfolio_size <= 50 ~ "Medium (11-50)",
        portfolio_size > 50 ~ "Large (>50)"
      )
    ) %>%
    group_by(portfolio_category) %>%
    summarise(
      n = n(),
      mpt_count = sum(modern_portfolio_theory_applied),
      proportion = round(mean(modern_portfolio_theory_applied) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(proportion))
  
  print("MPT application by portfolio size:")
  print(portfolio_analysis)
  
  # Calculate Wilson CIs for each portfolio size
  portfolio_ci <- portfolio_analysis %>%
    rowwise() %>%
    mutate(
      ci_data = list(BinomCI(mpt_count, n, conf.level = 0.95, method = "wilson")),
      ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
      ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
      ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
    ) %>%
    select(-ci_data)
  
  print("Portfolio size proportions with 95% Wilson CIs:")
  print(portfolio_ci %>% select(portfolio_category, n, proportion, ci_text))
  
  # Point-biserial correlation with portfolio size
  cor_portfolio <- cor.test(df_portfolio$modern_portfolio_theory_applied, 
                            df_portfolio$portfolio_size)
  
  print("MPT application × Portfolio size correlation:")
  print(paste("r =", round(cor_portfolio$estimate, 3)))
  print(paste("95% CI: [", round(cor_portfolio$conf.int[1], 3), ", ", 
              round(cor_portfolio$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_portfolio$p.value, scientific = TRUE)))
}

# Analysis by experience years
if(sum(!is.na(df$experience_years)) > 50) {
  df_experience <- df %>%
    filter(!is.na(experience_years))
  
  experience_analysis <- df_experience %>%
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
      mpt_count = sum(modern_portfolio_theory_applied),
      proportion = round(mean(modern_portfolio_theory_applied) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(proportion))
  
  print("MPT application by experience level (n ≥ 10):")
  print(experience_analysis)
  
  # Test for experience level differences
  if(nrow(experience_analysis) > 1) {
    experience_table <- df_experience %>%
      mutate(
        experience_category = case_when(
          experience_years <= 5 ~ "Junior",
          experience_years <= 10 ~ "Mid-level",
          experience_years <= 15 ~ "Senior",
          experience_years > 15 ~ "Executive"
        )
      ) %>%
      filter(!is.na(experience_category)) %>%
      group_by(experience_category) %>%
      filter(n() >= 10) %>%
      ungroup() %>%
      select(experience_category, modern_portfolio_theory_applied) %>%
      table()
    
    if(min(dim(experience_table)) > 1) {
      chi_experience <- chisq.test(experience_table)
      print("Chi-square test for experience level differences:")
      print(paste("χ² =", round(chi_experience$statistic, 2),
                  ", p =", format(chi_experience$p.value, scientific = TRUE)))
    }
  }
  
  # Correlation with continuous experience
  cor_experience <- cor.test(df_experience$modern_portfolio_theory_applied, 
                             df_experience$experience_years)
  
  print("MPT application × Experience years correlation:")
  print(paste("r =", round(cor_experience$estimate, 3)))
  print(paste("p-value:", format(cor_experience$p.value, scientific = TRUE)))
}

# Optimization method breakdown
if(!all(is.na(df$optimization_method))) {
  optimization_breakdown <- df %>%
    filter(!is.na(optimization_method), modern_portfolio_theory_applied == 1) %>%
    group_by(optimization_method) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(df$modern_portfolio_theory_applied == 1, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 5) %>%
    arrange(desc(percentage))
  
  print("Optimization methods among MPT users (n ≥ 5):")
  print(optimization_breakdown)
}

# Risk-adjusted returns analysis
if(sum(!is.na(df$risk_adjusted_returns)) > 50) {
  returns_comparison <- df %>%
    filter(!is.na(risk_adjusted_returns)) %>%
    mutate(
      mpt_status = ifelse(modern_portfolio_theory_applied == 1, 
                         "MPT Applied", "MPT Not Applied")
    ) %>%
    group_by(mpt_status) %>%
    summarise(
      n = n(),
      mean_returns = round(mean(risk_adjusted_returns), 2),
      sd_returns = round(sd(risk_adjusted_returns), 2),
      median_returns = round(median(risk_adjusted_returns), 2),
      .groups = "drop"
    )
  
  print("Risk-adjusted returns by MPT application:")
  print(returns_comparison)
  
  # t-test for returns difference
  if(nrow(returns_comparison) == 2) {
    mpt_returns <- df$risk_adjusted_returns[df$modern_portfolio_theory_applied == 1 & 
                                           !is.na(df$risk_adjusted_returns)]
    no_mpt_returns <- df$risk_adjusted_returns[df$modern_portfolio_theory_applied == 0 & 
                                              !is.na(df$risk_adjusted_returns)]
    
    if(length(mpt_returns) >= 20 && length(no_mpt_returns) >= 20) {
      t_test_returns <- t.test(mpt_returns, no_mpt_returns)
      
      print("Risk-adjusted returns comparison (t-test):")
      print(paste("t =", round(t_test_returns$statistic, 2)))
      print(paste("df =", round(t_test_returns$parameter, 1)))
      print(paste("p-value:", format(t_test_returns$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test_returns$estimate), 2)))
      print(paste("95% CI: [", round(t_test_returns$conf.int[1], 2), ", ",
                  round(t_test_returns$conf.int[2], 2), "]", sep=""))
      
      # Cohen's d for effect size
      pooled_sd <- sqrt(((length(mpt_returns) - 1) * var(mpt_returns) + 
                        (length(no_mpt_returns) - 1) * var(no_mpt_returns)) / 
                       (length(mpt_returns) + length(no_mpt_returns) - 2))
      cohens_d <- (mean(mpt_returns) - mean(no_mpt_returns)) / pooled_sd
      print(paste("Cohen's d =", round(cohens_d, 3)))
    }
  }
}

# Combined analysis: Stakeholder and portfolio size
if(sum(!is.na(df$portfolio_size)) > 100) {
  combined_analysis <- df %>%
    filter(!is.na(portfolio_size), !is.na(stakeholder)) %>%
    mutate(
      portfolio_category = case_when(
        portfolio_size <= 10 ~ "Small",
        portfolio_size <= 50 ~ "Medium",
        portfolio_size > 50 ~ "Large"
      )
    ) %>%
    group_by(stakeholder, portfolio_category) %>%
    summarise(
      n = n(),
      mpt_count = sum(modern_portfolio_theory_applied),
      proportion = round(mean(modern_portfolio_theory_applied) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(stakeholder, desc(proportion))
  
  if(nrow(combined_analysis) > 0) {
    print("MPT application by stakeholder and portfolio size (n ≥ 10):")
    print(combined_analysis)
  }
}

# Logistic regression analysis
if(sum(!is.na(df$portfolio_size)) > 100 && sum(!is.na(df$experience_years)) > 100) {
  df_logistic <- df %>%
    filter(!is.na(portfolio_size), !is.na(experience_years), !is.na(stakeholder))
  
  if(nrow(df_logistic) > 150) {
    logit_model <- glm(modern_portfolio_theory_applied ~ portfolio_size + experience_years + stakeholder,
                       data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: MPT application ~ Portfolio size + Experience + Stakeholder")
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
    
    # Model fit statistics
    print("Model fit statistics:")
    print(paste("AIC:", round(AIC(logit_model), 1)))
    print(paste("BIC:", round(BIC(logit_model), 1)))
    
    # Pseudo R-squared (McFadden's)
    null_model <- glm(modern_portfolio_theory_applied ~ 1, 
                     data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

# Trend analysis over portfolio size ranges
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_trend <- df %>%
    filter(!is.na(portfolio_size)) %>%
    mutate(portfolio_decile = ntile(portfolio_size, 10)) %>%
    group_by(portfolio_decile) %>%
    summarise(
      n = n(),
      mean_portfolio_size = round(mean(portfolio_size), 1),
      mpt_rate = round(mean(modern_portfolio_theory_applied) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 5) %>%
    arrange(portfolio_decile)
  
  print("MPT application trend by portfolio size deciles:")
  print(portfolio_trend)
  
  # Trend test (Cochran-Armitage)
  if(nrow(portfolio_trend) >= 3) {
    trend_test <- cor.test(portfolio_trend$portfolio_decile, 
                          portfolio_trend$mpt_rate,
                          method = "spearman")
    
    print("Portfolio size trend test (Spearman):")
    print(paste("ρ =", round(trend_test$estimate, 3)))
    print(paste("p-value:", format(trend_test$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("Summary of Modern Portfolio Theory application:")
print(paste("Overall application rate:", round(prop_mpt * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: 67% (95% CI [63.7%, 70.2%])