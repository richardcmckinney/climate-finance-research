# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 66: "Decision tree implementation correlated with decision complexity (r = .312, p < .001) and organizational hierarchy levels"
# Purpose: Correlation analysis between decision tree implementation and decision complexity, plus analysis by organizational hierarchy

library(tidyverse)
library(janitor)
library(boot)
library(psych)
library(car)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_DECISION_TREE_IMPLEMENTATION <- "decision_tree_implementation_usage"
COL_DECISION_COMPLEXITY <- "decision_complexity_rating"
COL_ORGANIZATIONAL_HIERARCHY <- "organizational_hierarchy_levels"
COL_STAKEHOLDER <- "stakeholder"
COL_DECISION_MAKING_AUTHORITY <- "decision_making_authority_level"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    decision_tree_implementation_usage = as.numeric(decision_tree_implementation_usage),
    decision_complexity_rating = as.numeric(decision_complexity_rating),
    organizational_hierarchy_levels = as.numeric(organizational_hierarchy_levels),
    stakeholder = factor(stakeholder),
    decision_making_authority_level = as.numeric(decision_making_authority_level)
  ) %>%
  filter(!is.na(decision_tree_implementation_usage), !is.na(decision_complexity_rating)) %>%
  # Ensure variables are within expected ranges
  filter(decision_tree_implementation_usage >= 1, decision_tree_implementation_usage <= 10,
         decision_complexity_rating >= 1, decision_complexity_rating <= 10)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    decision_tree_mean = round(mean(decision_tree_implementation_usage), 2),
    decision_tree_sd = round(sd(decision_tree_implementation_usage), 2),
    complexity_mean = round(mean(decision_complexity_rating), 2),
    complexity_sd = round(sd(decision_complexity_rating), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation: Decision tree implementation × Decision complexity
cor_complexity <- cor.test(df$decision_tree_implementation_usage, 
                          df$decision_complexity_rating,
                          method = "pearson", conf.level = 0.95)

print("Decision tree implementation × Decision complexity correlation:")
print(paste("r =", round(cor_complexity$estimate, 3)))
print(paste("95% CI: [", round(cor_complexity$conf.int[1], 3), ", ", 
            round(cor_complexity$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_complexity$parameter, ") =", round(cor_complexity$statistic, 2)))
print(paste("p-value:", format(cor_complexity$p.value, scientific = TRUE)))

# Bootstrap confidence interval for primary correlation
boot_cor_complexity <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$decision_tree_implementation_usage, d$decision_complexity_rating,
             use = "complete.obs"))
}

set.seed(123)
boot_result_complexity <- boot(df, boot_cor_complexity, R = 10000)
boot_ci_complexity <- boot.ci(boot_result_complexity, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation (Decision tree × Complexity):")
print(paste("[", round(boot_ci_complexity$percent[4], 3), ", ", round(boot_ci_complexity$percent[5], 3), "]", sep=""))

# Effect size interpretation for primary correlation
cor_magnitude <- abs(cor_complexity$estimate)
effect_size_interpretation <- case_when(
  cor_magnitude < 0.1 ~ "Negligible",
  cor_magnitude < 0.3 ~ "Small", 
  cor_magnitude < 0.5 ~ "Medium",
  cor_magnitude < 0.7 ~ "Large",
  TRUE ~ "Very Large"
)

print(paste("Effect size interpretation:", effect_size_interpretation))

# Analysis by organizational hierarchy levels
df_hierarchy <- df %>%
  filter(!is.na(organizational_hierarchy_levels)) %>%
  filter(organizational_hierarchy_levels >= 2, organizational_hierarchy_levels <= 10)  # Reasonable range

if(nrow(df_hierarchy) > 50) {
  print("Descriptive statistics for organizational hierarchy:")
  hierarchy_stats <- df_hierarchy %>%
    summarise(
      n = n(),
      mean_hierarchy = round(mean(organizational_hierarchy_levels), 2),
      sd_hierarchy = round(sd(organizational_hierarchy_levels), 2),
      median_hierarchy = round(median(organizational_hierarchy_levels), 2),
      .groups = "drop"
    )
  print(hierarchy_stats)
  
  # Correlation: Decision tree implementation × Organizational hierarchy
  cor_hierarchy <- cor.test(df_hierarchy$decision_tree_implementation_usage, 
                           df_hierarchy$organizational_hierarchy_levels,
                           method = "pearson", conf.level = 0.95)
  
  print("Decision tree implementation × Organizational hierarchy correlation:")
  print(paste("r =", round(cor_hierarchy$estimate, 3)))
  print(paste("95% CI: [", round(cor_hierarchy$conf.int[1], 3), ", ", 
              round(cor_hierarchy$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_hierarchy$p.value, scientific = TRUE)))
  
  # Bootstrap CI for hierarchy correlation
  boot_cor_hierarchy <- function(data, indices) {
    d <- data[indices, ]
    return(cor(d$decision_tree_implementation_usage, d$organizational_hierarchy_levels,
               use = "complete.obs"))
  }
  
  boot_result_hierarchy <- boot(df_hierarchy, boot_cor_hierarchy, R = 10000)
  boot_ci_hierarchy <- boot.ci(boot_result_hierarchy, type = "perc", conf = 0.95)
  
  print("Bootstrap 95% CI (Decision tree × Hierarchy):")
  print(paste("[", round(boot_ci_hierarchy$percent[4], 3), ", ", round(boot_ci_hierarchy$percent[5], 3), "]", sep=""))
}

# Correlation by stakeholder group
cor_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%
  do(
    correlation = cor.test(.$decision_tree_implementation_usage, .$decision_complexity_rating)
  ) %>%
  mutate(
    n = df %>% filter(stakeholder == .$stakeholder[1]) %>% nrow(),
    r = map_dbl(correlation, ~ round(.x$estimate, 3)),
    ci_lower = map_dbl(correlation, ~ round(.x$conf.int[1], 3)),
    ci_upper = map_dbl(correlation, ~ round(.x$conf.int[2], 3)),
    p_value = map_dbl(correlation, ~ .x$p.value)
  ) %>%
  select(-correlation)

print("Decision tree × Complexity correlation by stakeholder (n ≥ 30):")
print(cor_by_stakeholder)

# Three-way correlation matrix (if hierarchy data available)
if(nrow(df_hierarchy) > 50) {
  # Select complete cases for three-way analysis
  df_threeway <- df_hierarchy %>%
    select(decision_tree_implementation_usage, decision_complexity_rating, 
           organizational_hierarchy_levels) %>%
    filter(complete.cases(.))
  
  if(nrow(df_threeway) > 50) {
    cor_matrix <- cor(df_threeway, use = "complete.obs")
    
    print("Three-way correlation matrix:")
    print(round(cor_matrix, 3))
    
    # Partial correlations
    partial_tree_complexity <- pcor.test(df_threeway$decision_tree_implementation_usage,
                                        df_threeway$decision_complexity_rating,
                                        df_threeway$organizational_hierarchy_levels)
    
    partial_tree_hierarchy <- pcor.test(df_threeway$decision_tree_implementation_usage,
                                       df_threeway$organizational_hierarchy_levels,
                                       df_threeway$decision_complexity_rating)
    
    print("Partial correlations:")
    print(paste("Decision tree × Complexity (controlling for hierarchy): r =", 
                round(partial_tree_complexity$estimate, 3),
                ", p =", format(partial_tree_complexity$p.value, scientific = TRUE)))
    print(paste("Decision tree × Hierarchy (controlling for complexity): r =", 
                round(partial_tree_hierarchy$estimate, 3),
                ", p =", format(partial_tree_hierarchy$p.value, scientific = TRUE)))
  }
}

# Spearman correlations for robustness
spearman_complexity <- cor.test(df$decision_tree_implementation_usage,
                               df$decision_complexity_rating,
                               method = "spearman")
print(paste("Spearman's ρ (Decision tree × Complexity) =", round(spearman_complexity$estimate, 3)))

if(nrow(df_hierarchy) > 50) {
  spearman_hierarchy <- cor.test(df_hierarchy$decision_tree_implementation_usage,
                                df_hierarchy$organizational_hierarchy_levels,
                                method = "spearman")
  print(paste("Spearman's ρ (Decision tree × Hierarchy) =", round(spearman_hierarchy$estimate, 3)))
}

# Linear regression analysis
lm_complexity <- lm(decision_tree_implementation_usage ~ decision_complexity_rating, data = df)
lm_summary <- summary(lm_complexity)

print("Linear regression: Decision tree ~ Complexity:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Slope (β):", round(coef(lm_complexity)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))
print(paste("p-value:", format(lm_summary$coefficients[2,4], scientific = TRUE)))

# Multiple regression with hierarchy (if available)
if(nrow(df_hierarchy) > 100) {
  df_multiple <- df_hierarchy %>%
    filter(!is.na(stakeholder))
  
  if(nrow(df_multiple) > 50) {
    lm_multiple <- lm(decision_tree_implementation_usage ~ 
                     decision_complexity_rating + organizational_hierarchy_levels + stakeholder,
                     data = df_multiple)
    
    multiple_summary <- summary(lm_multiple)
    print("Multiple regression with hierarchy and stakeholder controls:")
    print(multiple_summary)
    
    # Extract key coefficients
    complexity_coef <- multiple_summary$coefficients["decision_complexity_rating", ]
    hierarchy_coef <- multiple_summary$coefficients["organizational_hierarchy_levels", ]
    
    print("Decision complexity coefficient (controlled):")
    print(paste("β =", round(complexity_coef[1], 3), ", SE =", round(complexity_coef[2], 3),
                ", p =", format(complexity_coef[4], scientific = TRUE)))
    
    print("Organizational hierarchy coefficient (controlled):")
    print(paste("β =", round(hierarchy_coef[1], 3), ", SE =", round(hierarchy_coef[2], 3),
                ", p =", format(hierarchy_coef[4], scientific = TRUE)))
  }
}

# Categorical analysis
df_categories <- df %>%
  mutate(
    decision_tree_level = case_when(
      decision_tree_implementation_usage <= 4 ~ "Low Implementation",
      decision_tree_implementation_usage <= 7 ~ "Moderate Implementation",
      decision_tree_implementation_usage <= 10 ~ "High Implementation"
    ),
    complexity_level = case_when(
      decision_complexity_rating <= 4 ~ "Low Complexity",
      decision_complexity_rating <= 7 ~ "Moderate Complexity",
      decision_complexity_rating <= 10 ~ "High Complexity"
    )
  )

cross_tab <- table(df_categories$complexity_level, df_categories$decision_tree_level)
print("Cross-tabulation: Decision complexity × Decision tree implementation:")
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

# Hierarchy level analysis (if available)
if(nrow(df_hierarchy) > 50) {
  hierarchy_categories <- df_hierarchy %>%
    mutate(
      hierarchy_level = case_when(
        organizational_hierarchy_levels <= 3 ~ "Flat (≤3 levels)",
        organizational_hierarchy_levels <= 5 ~ "Moderate (4-5 levels)",
        organizational_hierarchy_levels >= 6 ~ "Complex (≥6 levels)"
      )
    ) %>%
    group_by(hierarchy_level) %>%
    summarise(
      n = n(),
      mean_decision_tree = round(mean(decision_tree_implementation_usage), 2),
      sd_decision_tree = round(sd(decision_tree_implementation_usage), 2),
      mean_complexity = round(mean(decision_complexity_rating), 2),
      .groups = "drop"
    )
  
  print("Decision tree implementation by organizational hierarchy level:")
  print(hierarchy_categories)
  
  # ANOVA by hierarchy level
  if(nrow(hierarchy_categories) > 1) {
    df_hierarchy_categorized <- df_hierarchy %>%
      mutate(
        hierarchy_level = case_when(
          organizational_hierarchy_levels <= 3 ~ "Flat",
          organizational_hierarchy_levels <= 5 ~ "Moderate",
          organizational_hierarchy_levels >= 6 ~ "Complex"
        )
      ) %>%
      filter(!is.na(hierarchy_level))
    
    anova_hierarchy <- lm(decision_tree_implementation_usage ~ hierarchy_level, 
                         data = df_hierarchy_categorized)
    anova_hierarchy_result <- Anova(anova_hierarchy, type = "III")
    
    print("ANOVA: Decision tree implementation by hierarchy level:")
    print(anova_hierarchy_result)
  }
}

# Decision making authority analysis (if available)
if(sum(!is.na(df$decision_making_authority_level)) > 50) {
  df_authority <- df %>%
    filter(!is.na(decision_making_authority_level))
  
  cor_authority <- cor.test(df_authority$decision_tree_implementation_usage,
                           df_authority$decision_making_authority_level)
  
  print("Decision tree implementation × Decision making authority correlation:")
  print(paste("r =", round(cor_authority$estimate, 3)))
  print(paste("p-value:", format(cor_authority$p.value, scientific = TRUE)))
}

# Quartile analysis for decision complexity
df_quartiles <- df %>%
  mutate(
    complexity_quartile = ntile(decision_complexity_rating, 4),
    complexity_quartile_label = case_when(
      complexity_quartile == 1 ~ "Q1 (Lowest Complexity)",
      complexity_quartile == 2 ~ "Q2 (Low-Moderate)",
      complexity_quartile == 3 ~ "Q3 (Moderate-High)",
      complexity_quartile == 4 ~ "Q4 (Highest Complexity)"
    )
  )

quartile_analysis <- df_quartiles %>%
  group_by(complexity_quartile_label) %>%
  summarise(
    n = n(),
    complexity_range = paste(round(min(decision_complexity_rating), 1), "-",
                            round(max(decision_complexity_rating), 1)),
    mean_decision_tree = round(mean(decision_tree_implementation_usage), 2),
    sd_decision_tree = round(sd(decision_tree_implementation_usage), 2),
    .groups = "drop"
  )

print("Decision tree implementation by decision complexity quartiles:")
print(quartile_analysis)

# ANOVA across complexity quartiles
anova_quartiles <- lm(decision_tree_implementation_usage ~ factor(complexity_quartile), 
                     data = df_quartiles)
anova_quartile_summary <- summary(anova_quartiles)

print("ANOVA across decision complexity quartiles:")
print(paste("F(", anova_quartile_summary$df[1], ",", anova_quartile_summary$df[2], ") =",
            round(anova_quartile_summary$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_quartile_summary$fstatistic[1], 
                                 anova_quartile_summary$df[1], 
                                 anova_quartile_summary$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Expected: r = .312, p < .001 (correlation with decision complexity) and relationship with organizational hierarchy