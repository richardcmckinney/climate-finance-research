# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 42: "Partnership collaboration effectiveness correlated with portfolio performance (r = .298, p < .001)"
# Purpose: Correlation analysis between partnership collaboration effectiveness and portfolio performance metrics

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_PARTNERSHIP_EFFECTIVENESS <- "partnership_collaboration_effectiveness"
COL_PORTFOLIO_PERFORMANCE <- "portfolio_performance_rating"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size_numeric"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    partnership_collaboration_effectiveness = as.numeric(partnership_collaboration_effectiveness),
    portfolio_performance_rating = as.numeric(portfolio_performance_rating),
    stakeholder = factor(stakeholder),
    portfolio_size_numeric = as.numeric(portfolio_size_numeric)
  ) %>%
  filter(!is.na(partnership_collaboration_effectiveness), 
         !is.na(portfolio_performance_rating)) %>%
  # Ensure variables are within expected ranges
  filter(partnership_collaboration_effectiveness >= 1, partnership_collaboration_effectiveness <= 7,
         portfolio_performance_rating >= 1, portfolio_performance_rating <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    partnership_mean = round(mean(partnership_collaboration_effectiveness), 2),
    partnership_sd = round(sd(partnership_collaboration_effectiveness), 2),
    performance_mean = round(mean(portfolio_performance_rating), 2),
    performance_sd = round(sd(portfolio_performance_rating), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$partnership_collaboration_effectiveness, 
                      df$portfolio_performance_rating,
                      method = "pearson", conf.level = 0.95)

print("Pearson correlation results:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$partnership_collaboration_effectiveness, d$portfolio_performance_rating,
             use = "complete.obs"))
}

set.seed(123)
boot_result <- boot(df, boot_cor, R = 10000)
boot_ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation:")
print(paste("[", round(boot_ci$percent[4], 3), ", ", round(boot_ci$percent[5], 3), "]", sep=""))

# Effect size interpretation
cor_magnitude <- abs(cor_result$estimate)
effect_size_interpretation <- case_when(
  cor_magnitude < 0.1 ~ "Negligible",
  cor_magnitude < 0.3 ~ "Small", 
  cor_magnitude < 0.5 ~ "Medium",
  cor_magnitude < 0.7 ~ "Large",
  TRUE ~ "Very Large"
)

print(paste("Effect size interpretation:", effect_size_interpretation))

# Correlation by stakeholder group
cor_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%
  do(
    correlation = cor.test(.$partnership_collaboration_effectiveness, .$portfolio_performance_rating)
  ) %>%
  mutate(
    n = df %>% filter(stakeholder == .$stakeholder[1]) %>% nrow(),
    r = map_dbl(correlation, ~ round(.x$estimate, 3)),
    ci_lower = map_dbl(correlation, ~ round(.x$conf.int[1], 3)),
    ci_upper = map_dbl(correlation, ~ round(.x$conf.int[2], 3)),
    p_value = map_dbl(correlation, ~ .x$p.value)
  ) %>%
  select(-correlation)

print("Correlation by stakeholder group (n ≥ 30):")
print(cor_by_stakeholder)

# Partial correlation controlling for portfolio size
if(sum(!is.na(df$portfolio_size_numeric)) > 50) {
  df_portfolio <- df %>%
    filter(!is.na(portfolio_size_numeric)) %>%
    # Log transform portfolio size if heavily skewed
    mutate(portfolio_size_log = log(portfolio_size_numeric + 1))
  
  partial_cor <- pcor.test(df_portfolio$partnership_collaboration_effectiveness,
                          df_portfolio$portfolio_performance_rating,
                          df_portfolio$portfolio_size_log)
  
  print("Partial correlation (controlling for portfolio size):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness
spearman_result <- cor.test(df$partnership_collaboration_effectiveness,
                           df$portfolio_performance_rating,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(portfolio_performance_rating ~ partnership_collaboration_effectiveness, 
               data = df)
lm_summary <- summary(lm_model)

print("Linear regression results:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("F-statistic:", round(lm_summary$fstatistic[1], 2)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))

# Cross-tabulation for categorical analysis
df_categories <- df %>%
  mutate(
    partnership_level = case_when(
      partnership_collaboration_effectiveness <= 3 ~ "Low Effectiveness",
      partnership_collaboration_effectiveness <= 5 ~ "Moderate Effectiveness", 
      partnership_collaboration_effectiveness <= 7 ~ "High Effectiveness"
    ),
    performance_level = case_when(
      portfolio_performance_rating <= 4 ~ "Below Average",
      portfolio_performance_rating <= 7 ~ "Average",
      portfolio_performance_rating <= 10 ~ "Above Average"
    )
  )

cross_tab <- table(df_categories$partnership_level, df_categories$performance_level)
print("Cross-tabulation of partnership effectiveness and portfolio performance levels:")
print(cross_tab)
print("Row percentages:")
print(round(prop.table(cross_tab, 1) * 100, 1))

# Chi-square test for categorical association
chi_test <- chisq.test(cross_tab)
print(paste("Chi-square test: χ² =", round(chi_test$statistic, 2),
            ", df =", chi_test$parameter,
            ", p =", format(chi_test$p.value, scientific = TRUE)))

# Cramér's V for effect size
cramers_v <- sqrt(chi_test$statistic / (sum(cross_tab) * (min(dim(cross_tab)) - 1)))
print(paste("Cramér's V:", round(cramers_v, 3)))

# Detailed analysis by partnership effectiveness quartiles
df_quartiles <- df %>%
  mutate(
    partnership_quartile = ntile(partnership_collaboration_effectiveness, 4),
    partnership_quartile_label = case_when(
      partnership_quartile == 1 ~ "Q1 (Lowest)",
      partnership_quartile == 2 ~ "Q2 (Low-Moderate)",
      partnership_quartile == 3 ~ "Q3 (Moderate-High)",
      partnership_quartile == 4 ~ "Q4 (Highest)"
    )
  )

quartile_analysis <- df_quartiles %>%
  group_by(partnership_quartile_label) %>%
  summarise(
    n = n(),
    partnership_range = paste(round(min(partnership_collaboration_effectiveness), 1), "-",
                             round(max(partnership_collaboration_effectiveness), 1)),
    mean_performance = round(mean(portfolio_performance_rating), 2),
    sd_performance = round(sd(portfolio_performance_rating), 2),
    median_performance = round(median(portfolio_performance_rating), 2),
    .groups = "drop"
  )

print("Portfolio performance by partnership effectiveness quartiles:")
print(quartile_analysis)

# ANOVA across quartiles
anova_model <- lm(portfolio_performance_rating ~ factor(partnership_quartile), 
                  data = df_quartiles)
anova_summary <- summary(anova_model)

print("ANOVA across partnership effectiveness quartiles:")
print(paste("F(", anova_summary$df[1], ",", anova_summary$df[2], ") =",
            round(anova_summary$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_summary$fstatistic[1], 
                                 anova_summary$df[1], 
                                 anova_summary$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Multiple regression with additional predictors
if(sum(!is.na(df$portfolio_size_numeric)) > 100) {
  df_multiple <- df %>%
    filter(!is.na(portfolio_size_numeric), !is.na(stakeholder)) %>%
    mutate(portfolio_size_log = log(portfolio_size_numeric + 1))
  
  lm_multiple <- lm(portfolio_performance_rating ~ partnership_collaboration_effectiveness + 
                   portfolio_size_log + stakeholder, 
                   data = df_multiple)
  
  multiple_summary <- summary(lm_multiple)
  print("Multiple regression with controls:")
  
  # Extract partnership coefficient
  partnership_coef <- multiple_summary$coefficients["partnership_collaboration_effectiveness", ]
  print("Partnership effectiveness coefficient (controlled):")
  print(paste("β =", round(partnership_coef[1], 3)))
  print(paste("SE =", round(partnership_coef[2], 3)))
  print(paste("p =", format(partnership_coef[4], scientific = TRUE)))
}

# Expected: r = .298, p < .001