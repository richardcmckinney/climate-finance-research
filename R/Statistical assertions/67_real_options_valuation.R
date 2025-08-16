# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 67: "Real options valuation usage was limited to 23% of respondents (95% CI [20.1%, 25.9%]) but showed strong association with advanced financial training"
# Purpose: Proportion analysis with Wilson confidence intervals and association testing with advanced financial training

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_REAL_OPTIONS_USAGE <- "real_options_valuation_usage"
COL_ADVANCED_FINANCIAL_TRAINING <- "advanced_financial_training_level"
COL_STAKEHOLDER <- "stakeholder"
COL_EDUCATION_LEVEL <- "education_level"
COL_FINANCE_CERTIFICATION <- "finance_professional_certification"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    real_options_valuation_usage = as.integer(real_options_valuation_usage),
    advanced_financial_training_level = as.numeric(advanced_financial_training_level),
    stakeholder = factor(stakeholder),
    education_level = factor(education_level),
    finance_professional_certification = as.integer(finance_professional_certification)
  ) %>%
  filter(!is.na(real_options_valuation_usage))

# Overall proportion analysis
n_total <- nrow(df)
n_usage <- sum(df$real_options_valuation_usage, na.rm = TRUE)
prop_usage <- n_usage / n_total

print("Overall real options valuation usage:")
print(paste("Total respondents:", n_total))
print(paste("Using real options:", n_usage))
print(paste("Proportion:", round(prop_usage * 100, 1), "%"))

# Wilson confidence interval for overall proportion
ci_wilson <- BinomCI(n_usage, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Additional confidence interval methods for comparison
ci_exact <- BinomCI(n_usage, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_wald <- BinomCI(n_usage, n_total, conf.level = 0.95, method = "wald")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Wald CI: [", round(ci_wald[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wald[,"upr.ci"] * 100, 1), "%]", sep=""))

# Association with advanced financial training
if(sum(!is.na(df$advanced_financial_training_level)) > 50) {
  df_training <- df %>%
    filter(!is.na(advanced_financial_training_level))
  
  print("Descriptive statistics for advanced financial training:")
  training_stats <- df_training %>%
    summarise(
      n = n(),
      mean_training = round(mean(advanced_financial_training_level), 2),
      sd_training = round(sd(advanced_financial_training_level), 2),
      .groups = "drop"
    )
  print(training_stats)
  
  # Point-biserial correlation
  cor_training <- cor.test(df_training$real_options_valuation_usage, 
                          df_training$advanced_financial_training_level)
  
  print("Real options usage × Advanced financial training correlation:")
  print(paste("r =", round(cor_training$estimate, 3)))
  print(paste("95% CI: [", round(cor_training$conf.int[1], 3), ", ", 
              round(cor_training$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_training$p.value, scientific = TRUE)))
  
  # Categorize training levels for detailed analysis
  training_analysis <- df_training %>%
    mutate(
      training_level = case_when(
        advanced_financial_training_level <= 3 ~ "Basic Training",
        advanced_financial_training_level <= 7 ~ "Moderate Training",
        advanced_financial_training_level >= 8 ~ "Advanced Training"
      )
    ) %>%
    group_by(training_level) %>%
    summarise(
      n = n(),
      usage_count = sum(real_options_valuation_usage),
      usage_percentage = round(mean(real_options_valuation_usage) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(usage_percentage))
  
  print("Real options usage by financial training level:")
  print(training_analysis)
  
  # Calculate Wilson CIs for each training level
  training_analysis_ci <- training_analysis %>%
    rowwise() %>%
    mutate(
      ci_data = list(BinomCI(usage_count, n, conf.level = 0.95, method = "wilson")),
      ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
      ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
      ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
    ) %>%
    select(-ci_data)
  
  print("Training level proportions with 95% Wilson CIs:")
  print(training_analysis_ci %>% select(training_level, n, usage_percentage, ci_text))
  
  # Chi-square test for training level differences
  if(nrow(training_analysis) > 1) {
    training_table <- df_training %>%
      mutate(
        training_level = case_when(
          advanced_financial_training_level <= 3 ~ "Basic",
          advanced_financial_training_level <= 7 ~ "Moderate",
          advanced_financial_training_level >= 8 ~ "Advanced"
        )
      ) %>%
      filter(!is.na(training_level)) %>%
      select(training_level, real_options_valuation_usage) %>%
      table()
    
    chi_training <- chisq.test(training_table)
    print("Chi-square test for training level differences:")
    print(paste("χ² =", round(chi_training$statistic, 2),
                ", p =", format(chi_training$p.value, scientific = TRUE)))
    
    # Cramér's V
    cramers_v <- sqrt(chi_training$statistic / (sum(training_table) * (min(dim(training_table)) - 1)))
    print(paste("Cramér's V =", round(cramers_v, 3)))
  }
  
  # Advanced vs Basic training comparison
  advanced_vs_basic <- df_training %>%
    mutate(
      training_category = ifelse(advanced_financial_training_level >= 8, 
                                "Advanced Training", "Basic/Moderate Training")
    ) %>%
    group_by(training_category) %>%
    summarise(
      n = n(),
      usage_count = sum(real_options_valuation_usage),
      usage_percentage = round(mean(real_options_valuation_usage) * 100, 1),
      .groups = "drop"
    )
  
  print("Advanced vs Basic/Moderate financial training:")
  print(advanced_vs_basic)
  
  # Fisher's exact test for advanced vs basic
  if(nrow(advanced_vs_basic) == 2) {
    advanced_basic_table <- df_training %>%
      mutate(
        training_category = ifelse(advanced_financial_training_level >= 8, 
                                  "Advanced", "Basic_Moderate")
      ) %>%
      select(training_category, real_options_valuation_usage) %>%
      table()
    
    fisher_test <- fisher.test(advanced_basic_table)
    print("Advanced vs Basic/Moderate training (Fisher's exact test):")
    print(paste("p-value:", format(fisher_test$p.value, scientific = TRUE)))
    print(paste("Odds ratio:", round(fisher_test$estimate, 2)))
    print(paste("95% CI for OR: [", round(fisher_test$conf.int[1], 2), ", ",
                round(fisher_test$conf.int[2], 2), "]", sep=""))
  }
}

# Breakdown by stakeholder
stakeholder_breakdown <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    usage_count = sum(real_options_valuation_usage, na.rm = TRUE),
    proportion = round(mean(real_options_valuation_usage, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(proportion))

print("Real options usage by stakeholder:")
print(stakeholder_breakdown)

# Statistical test for stakeholder differences
stakeholder_table <- table(df$stakeholder, df$real_options_valuation_usage)
chi_stakeholder <- chisq.test(stakeholder_table)
print("Chi-square test for stakeholder variation:")
print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))

# Analysis by education level
if(!all(is.na(df$education_level))) {
  education_breakdown <- df %>%
    filter(!is.na(education_level)) %>%
    group_by(education_level) %>%
    summarise(
      n = n(),
      usage_count = sum(real_options_valuation_usage),
      proportion = round(mean(real_options_valuation_usage) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%  # Only education levels with sufficient sample size
    arrange(desc(proportion))
  
  print("Real options usage by education level (n ≥ 20):")
  print(education_breakdown)
  
  # Test for education level differences
  if(nrow(education_breakdown) > 1) {
    education_table <- df %>%
      filter(!is.na(education_level)) %>%
      group_by(education_level) %>%
      filter(n() >= 20) %>%
      ungroup() %>%
      mutate(education_level = droplevels(education_level)) %>%
      select(education_level, real_options_valuation_usage) %>%
      table()
    
    chi_education <- chisq.test(education_table)
    print("Chi-square test for education level differences:")
    print(paste("χ² =", round(chi_education$statistic, 2),
                ", p =", format(chi_education$p.value, scientific = TRUE)))
  }
}

# Finance certification analysis
if(sum(!is.na(df$finance_professional_certification)) > 50) {
  cert_comparison <- df %>%
    filter(!is.na(finance_professional_certification)) %>%
    mutate(
      certification_status = ifelse(finance_professional_certification == 1, 
                                   "Has Finance Certification", "No Finance Certification")
    ) %>%
    group_by(certification_status) %>%
    summarise(
      n = n(),
      usage_count = sum(real_options_valuation_usage),
      proportion = round(mean(real_options_valuation_usage) * 100, 1),
      .groups = "drop"
    )
  
  print("Real options usage by finance certification status:")
  print(cert_comparison)
  
  # Fisher's exact test for certification comparison
  if(nrow(cert_comparison) == 2) {
    cert_table <- df %>%
      filter(!is.na(finance_professional_certification)) %>%
      mutate(
        cert_status = ifelse(finance_professional_certification == 1, "Certified", "Not_Certified")
      ) %>%
      select(cert_status, real_options_valuation_usage) %>%
      table()
    
    fisher_cert <- fisher.test(cert_table)
    print("Finance certification comparison (Fisher's exact test):")
    print(paste("p-value:", format(fisher_cert$p.value, scientific = TRUE)))
    print(paste("Odds ratio:", round(fisher_cert$estimate, 2)))
    print(paste("95% CI for OR: [", round(fisher_cert$conf.int[1], 2), ", ",
                round(fisher_cert$conf.int[2], 2), "]", sep=""))
  }
}

# Combined analysis: Training and stakeholder
if(sum(!is.na(df$advanced_financial_training_level)) > 100) {
  combined_analysis <- df %>%
    filter(!is.na(advanced_financial_training_level), !is.na(stakeholder)) %>%
    mutate(
      training_level = case_when(
        advanced_financial_training_level <= 3 ~ "Basic",
        advanced_financial_training_level <= 7 ~ "Moderate",
        advanced_financial_training_level >= 8 ~ "Advanced"
      )
    ) %>%
    group_by(stakeholder, training_level) %>%
    summarise(
      n = n(),
      usage_count = sum(real_options_valuation_usage),
      proportion = round(mean(real_options_valuation_usage) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(stakeholder, desc(proportion))
  
  if(nrow(combined_analysis) > 0) {
    print("Real options usage by stakeholder and training level (n ≥ 10):")
    print(combined_analysis)
  }
}

# Logistic regression analysis
if(sum(!is.na(df$advanced_financial_training_level)) > 50) {
  df_logistic <- df %>%
    filter(!is.na(advanced_financial_training_level), !is.na(stakeholder))
  
  if(nrow(df_logistic) > 100) {
    logit_model <- glm(real_options_valuation_usage ~ advanced_financial_training_level + stakeholder,
                       data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: Real options usage ~ Training + Stakeholder")
    print(logit_summary)
    
    # Odds ratios
    odds_ratios <- exp(coef(logit_model))
    print("Odds ratios:")
    print(round(odds_ratios, 3))
    
    # Training level coefficient
    training_coef <- logit_summary$coefficients["advanced_financial_training_level", ]
    print("Advanced financial training coefficient:")
    print(paste("β =", round(training_coef[1], 3), ", SE =", round(training_coef[2], 3),
                ", p =", format(training_coef[4], scientific = TRUE)))
    print(paste("Odds ratio =", round(exp(training_coef[1]), 3)))
  }
}

# Usage trend by training score
if(sum(!is.na(df$advanced_financial_training_level)) > 50) {
  training_trend <- df %>%
    filter(!is.na(advanced_financial_training_level)) %>%
    group_by(advanced_financial_training_level) %>%
    summarise(
      n = n(),
      usage_rate = round(mean(real_options_valuation_usage) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 5) %>%
    arrange(advanced_financial_training_level)
  
  print("Real options usage trend by training score:")
  print(training_trend)
  
  # Trend test (Spearman correlation)
  trend_test <- cor.test(df$advanced_financial_training_level[!is.na(df$advanced_financial_training_level)],
                        df$real_options_valuation_usage[!is.na(df$advanced_financial_training_level)],
                        method = "spearman")
  
  print("Training score trend test (Spearman):")
  print(paste("ρ =", round(trend_test$estimate, 3)))
  print(paste("p-value:", format(trend_test$p.value, scientific = TRUE)))
}

# Non-usage analysis
non_usage_rate <- (1 - prop_usage) * 100
print(paste("Non-usage rate:", round(non_usage_rate, 1), "%"))

# Summary of key findings
print("Summary of real options valuation usage:")
print(paste("Overall usage rate:", round(prop_usage * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: 23% of respondents (95% CI [20.1%, 25.9%]) with strong association with advanced financial training