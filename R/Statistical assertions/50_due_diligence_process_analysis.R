# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 50: "Due diligence process duration correlated with investment amount (r = .356, p < .001) and stakeholder type"
# Purpose: Correlation analysis between due diligence duration, investment amount, and analysis by stakeholder type

library(tidyverse)
library(janitor)
library(boot)
library(car)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_DUE_DILIGENCE_DURATION <- "due_diligence_duration_weeks"
COL_INVESTMENT_AMOUNT <- "investment_amount_numeric"
COL_STAKEHOLDER <- "stakeholder"
COL_DEAL_COMPLEXITY <- "deal_complexity_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    due_diligence_duration_weeks = as.numeric(due_diligence_duration_weeks),
    investment_amount_numeric = as.numeric(investment_amount_numeric),
    stakeholder = factor(stakeholder),
    deal_complexity_score = as.numeric(deal_complexity_score)
  ) %>%
  filter(!is.na(due_diligence_duration_weeks), !is.na(investment_amount_numeric)) %>%
  # Remove extreme outliers
  filter(due_diligence_duration_weeks >= 1, due_diligence_duration_weeks <= 52,
         investment_amount_numeric > 0, investment_amount_numeric <= 1e9) %>%
  # Create log-transformed investment amount for analysis
  mutate(investment_amount_log = log(investment_amount_numeric))

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    dd_duration_mean = round(mean(due_diligence_duration_weeks), 2),
    dd_duration_sd = round(sd(due_diligence_duration_weeks), 2),
    dd_duration_median = round(median(due_diligence_duration_weeks), 2),
    investment_mean = round(mean(investment_amount_numeric), 0),
    investment_median = round(median(investment_amount_numeric), 0),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation: Due diligence duration × Investment amount
cor_result <- cor.test(df$due_diligence_duration_weeks, 
                      df$investment_amount_numeric,
                      method = "pearson", conf.level = 0.95)

print("Due diligence duration × Investment amount correlation:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Correlation with log-transformed investment amount
cor_log <- cor.test(df$due_diligence_duration_weeks, 
                   df$investment_amount_log,
                   method = "pearson", conf.level = 0.95)

print("Due diligence duration × Log(Investment amount) correlation:")
print(paste("r =", round(cor_log$estimate, 3)))
print(paste("95% CI: [", round(cor_log$conf.int[1], 3), ", ", 
            round(cor_log$conf.int[2], 3), "]", sep=""))
print(paste("p-value:", format(cor_log$p.value, scientific = TRUE)))

# Bootstrap confidence interval
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$due_diligence_duration_weeks, d$investment_amount_log,
             use = "complete.obs"))
}

set.seed(123)
boot_result <- boot(df, boot_cor, R = 10000)
boot_ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation (with log investment):")
print(paste("[", round(boot_ci$percent[4], 3), ", ", round(boot_ci$percent[5], 3), "]", sep=""))

# Analysis by stakeholder type
stakeholder_analysis <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    dd_duration_mean = round(mean(due_diligence_duration_weeks), 2),
    dd_duration_sd = round(sd(due_diligence_duration_weeks), 2),
    dd_duration_median = round(median(due_diligence_duration_weeks), 2),
    investment_mean = round(mean(investment_amount_numeric) / 1e6, 2), # in millions
    investment_median = round(median(investment_amount_numeric) / 1e6, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(dd_duration_mean))

print("Due diligence duration and investment amounts by stakeholder:")
print(stakeholder_analysis)

# ANOVA: Due diligence duration by stakeholder
anova_dd <- lm(due_diligence_duration_weeks ~ stakeholder, data = df)
anova_result <- Anova(anova_dd, type = "III")

print("ANOVA: Due diligence duration by stakeholder:")
print(anova_result)

# Effect size for ANOVA
eta_squared_result <- eta_squared(anova_result)
print(paste("η² =", round(eta_squared_result$Eta2_partial[1], 3)))

# Post-hoc comparisons for stakeholder differences
if(anova_result$`Pr(>F)`[1] < 0.05) {
  emmeans_result <- emmeans(anova_dd, ~ stakeholder)
  pairwise_comp <- pairs(emmeans_result, adjust = "bonferroni")
  
  print("Post-hoc pairwise comparisons (due diligence duration):")
  print(pairwise_comp)
}

# Correlation by stakeholder group
cor_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%
  do(
    correlation = cor.test(.$due_diligence_duration_weeks, .$investment_amount_log)
  ) %>%
  mutate(
    n = df %>% filter(stakeholder == .$stakeholder[1]) %>% nrow(),
    r = map_dbl(correlation, ~ round(.x$estimate, 3)),
    ci_lower = map_dbl(correlation, ~ round(.x$conf.int[1], 3)),
    ci_upper = map_dbl(correlation, ~ round(.x$conf.int[2], 3)),
    p_value = map_dbl(correlation, ~ .x$p.value)
  ) %>%
  select(-correlation)

print("Duration-Investment correlation by stakeholder (n ≥ 30):")
print(cor_by_stakeholder)

# Spearman correlation for robustness
spearman_result <- cor.test(df$due_diligence_duration_weeks,
                           df$investment_amount_numeric,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(due_diligence_duration_weeks ~ investment_amount_log, data = df)
lm_summary <- summary(lm_model)

print("Linear regression: Duration ~ Log(Investment):")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))
print(paste("p-value:", format(lm_summary$coefficients[2,4], scientific = TRUE)))

# Multiple regression with stakeholder type
lm_multiple <- lm(due_diligence_duration_weeks ~ investment_amount_log + stakeholder, 
                 data = df)
multiple_summary <- summary(lm_multiple)

print("Multiple regression: Duration ~ Log(Investment) + Stakeholder:")
print(multiple_summary)

# Extract investment coefficient from multiple regression
investment_coef <- multiple_summary$coefficients["investment_amount_log", ]
print("Investment amount coefficient (controlling for stakeholder):")
print(paste("β =", round(investment_coef[1], 3)))
print(paste("SE =", round(investment_coef[2], 3)))
print(paste("p =", format(investment_coef[4], scientific = TRUE)))

# Deal complexity analysis (if available)
if(sum(!is.na(df$deal_complexity_score)) > 50) {
  df_complexity <- df %>%
    filter(!is.na(deal_complexity_score))
  
  # Correlation with deal complexity
  cor_complexity <- cor.test(df_complexity$due_diligence_duration_weeks, 
                            df_complexity$deal_complexity_score)
  
  print("Due diligence duration × Deal complexity correlation:")
  print(paste("r =", round(cor_complexity$estimate, 3)))
  print(paste("p =", format(cor_complexity$p.value, scientific = TRUE)))
  
  # Multiple regression with complexity
  lm_complexity <- lm(due_diligence_duration_weeks ~ investment_amount_log + 
                     deal_complexity_score + stakeholder, 
                     data = df_complexity)
  
  complexity_summary <- summary(lm_complexity)
  print("Multiple regression with deal complexity:")
  print(paste("R-squared:", round(complexity_summary$r.squared, 3)))
}

# Categorized analysis
df_categories <- df %>%
  mutate(
    investment_category = case_when(
      investment_amount_numeric <= 1e6 ~ "Small (<$1M)",
      investment_amount_numeric <= 1e7 ~ "Medium ($1-10M)",
      investment_amount_numeric <= 1e8 ~ "Large ($10-100M)",
      TRUE ~ "Very Large (>$100M)"
    ),
    dd_duration_category = case_when(
      due_diligence_duration_weeks <= 4 ~ "Short (≤4 weeks)",
      due_diligence_duration_weeks <= 12 ~ "Medium (5-12 weeks)",
      due_diligence_duration_weeks <= 24 ~ "Long (13-24 weeks)",
      TRUE ~ "Very Long (>24 weeks)"
    )
  )

# Cross-tabulation
cross_tab <- table(df_categories$investment_category, df_categories$dd_duration_category)
print("Cross-tabulation: Investment size × Due diligence duration:")
print(cross_tab)
print("Row percentages:")
print(round(prop.table(cross_tab, 1) * 100, 1))

# Chi-square test for categorical association
chi_test <- chisq.test(cross_tab)
print(paste("Chi-square test: χ² =", round(chi_test$statistic, 2),
            ", df =", chi_test$parameter,
            ", p =", format(chi_test$p.value, scientific = TRUE)))

# Cramér's V
cramers_v <- sqrt(chi_test$statistic / (sum(cross_tab) * (min(dim(cross_tab)) - 1)))
print(paste("Cramér's V:", round(cramers_v, 3)))

# Investment size quartiles analysis
df_quartiles <- df %>%
  mutate(
    investment_quartile = ntile(investment_amount_numeric, 4),
    investment_quartile_label = case_when(
      investment_quartile == 1 ~ "Q1 (Smallest)",
      investment_quartile == 2 ~ "Q2 (Small-Medium)",
      investment_quartile == 3 ~ "Q3 (Medium-Large)",
      investment_quartile == 4 ~ "Q4 (Largest)"
    )
  )

quartile_analysis <- df_quartiles %>%
  group_by(investment_quartile_label) %>%
  summarise(
    n = n(),
    investment_range = paste("$", round(min(investment_amount_numeric)/1e6, 1), "-",
                            round(max(investment_amount_numeric)/1e6, 1), "M"),
    mean_dd_duration = round(mean(due_diligence_duration_weeks), 2),
    sd_dd_duration = round(sd(due_diligence_duration_weeks), 2),
    .groups = "drop"
  )

print("Due diligence duration by investment amount quartiles:")
print(quartile_analysis)

# Effect size interpretation
cor_magnitude <- abs(cor_log$estimate)
effect_size_interpretation <- case_when(
  cor_magnitude < 0.1 ~ "Negligible",
  cor_magnitude < 0.3 ~ "Small", 
  cor_magnitude < 0.5 ~ "Medium",
  cor_magnitude < 0.7 ~ "Large",
  TRUE ~ "Very Large"
)

print(paste("Effect size interpretation:", effect_size_interpretation))

# Expected: r = .356, p < .001 (correlation with investment amount)