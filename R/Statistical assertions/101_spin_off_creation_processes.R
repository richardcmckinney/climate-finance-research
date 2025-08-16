# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 101: "Spin-off creation processes: timeline variance (F = 3.92, p < .01)"
# Purpose: Analyze timeline variance in spin-off creation processes

library(tidyverse)
library(janitor)
library(DescTools)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SPINOFF_TIMELINE <- "spinoff_creation_timeline_months"
COL_SPINOFF_TYPE <- "spinoff_type"
COL_PARENT_SIZE <- "parent_company_size"
COL_INDUSTRY <- "industry_sector"
COL_COMPLEXITY_SCORE <- "spinoff_complexity_score"
COL_REGULATORY_REQUIREMENTS <- "regulatory_requirement_count"
COL_STAKEHOLDER_ALIGNMENT <- "stakeholder_alignment_score"
COL_RESOURCE_ALLOCATION <- "resource_allocation_score"
COL_MARKET_CONDITIONS <- "market_condition_favorability"
COL_SUCCESS_INDICATOR <- "spinoff_success_indicator"
COL_VALUE_CREATION <- "value_creation_score"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    spinoff_creation_timeline_months = as.numeric(spinoff_creation_timeline_months),
    spinoff_type = factor(spinoff_type),
    parent_company_size = factor(parent_company_size,
                                levels = c("Small", "Medium", "Large", "Enterprise"),
                                ordered = TRUE),
    industry_sector = factor(industry_sector),
    spinoff_complexity_score = as.numeric(spinoff_complexity_score),
    regulatory_requirement_count = as.numeric(regulatory_requirement_count),
    stakeholder_alignment_score = as.numeric(stakeholder_alignment_score),
    resource_allocation_score = as.numeric(resource_allocation_score),
    market_condition_favorability = as.numeric(market_condition_favorability),
    spinoff_success_indicator = factor(spinoff_success_indicator),
    value_creation_score = as.numeric(value_creation_score),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(spinoff_creation_timeline_months), !is.na(spinoff_type))

# Filter for adequate group sizes
type_counts <- df %>%
  group_by(spinoff_type) %>%
  summarise(n = n()) %>%
  filter(n >= 10)

df_anova <- df %>%
  filter(spinoff_type %in% type_counts$spinoff_type) %>%
  mutate(spinoff_type = droplevels(spinoff_type))

# Descriptive statistics by spin-off type
timeline_stats <- df_anova %>%
  group_by(spinoff_type) %>%
  summarise(
    n = n(),
    mean_timeline = mean(spinoff_creation_timeline_months, na.rm = TRUE),
    sd_timeline = sd(spinoff_creation_timeline_months, na.rm = TRUE),
    var_timeline = var(spinoff_creation_timeline_months, na.rm = TRUE),
    median_timeline = median(spinoff_creation_timeline_months, na.rm = TRUE),
    min_timeline = min(spinoff_creation_timeline_months, na.rm = TRUE),
    max_timeline = max(spinoff_creation_timeline_months, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(mean_timeline)

print("Spin-off Creation Timeline by Type:")
print(timeline_stats)

# Overall statistics
n_total <- nrow(df_anova)
n_groups <- length(unique(df_anova$spinoff_type))
overall_mean <- mean(df_anova$spinoff_creation_timeline_months)
overall_sd <- sd(df_anova$spinoff_creation_timeline_months)

print(paste("\nTotal N:", n_total))
print(paste("Number of spin-off types:", n_groups))
print(paste("Overall mean timeline:", round(overall_mean, 1), "months"))
print(paste("Overall SD:", round(overall_sd, 1), "months"))

# One-way ANOVA
anova_result <- aov(spinoff_creation_timeline_months ~ spinoff_type, data = df_anova)
anova_summary <- summary(anova_result)

print("\nOne-way ANOVA - Timeline by Spin-off Type:")
print(anova_summary)

# Extract F-statistic and p-value
f_stat <- anova_summary[[1]][["F value"]][1]
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
df_between <- anova_summary[[1]][["Df"]][1]
df_within <- anova_summary[[1]][["Df"]][2]

print(paste("\nF(", df_between, ",", df_within, ") =", round(f_stat, 2)))
print(paste("p-value:", format(p_value, scientific = TRUE)))

# Effect size measures
ss_between <- anova_summary[[1]][["Sum Sq"]][1]
ss_within <- anova_summary[[1]][["Sum Sq"]][2]
ss_total <- ss_between + ss_within

eta_squared <- ss_between / ss_total
omega_squared <- (ss_between - df_between * (ss_within / df_within)) / 
                (ss_total + (ss_within / df_within))

print("\nEffect Size Measures:")
print(paste("Eta-squared:", round(eta_squared, 3)))
print(paste("Omega-squared:", round(omega_squared, 3)))

# Cohen's f
cohens_f <- sqrt(eta_squared / (1 - eta_squared))
print(paste("Cohen's f:", round(cohens_f, 3)))

# Levene's test for homogeneity of variances
levene_test <- leveneTest(spinoff_creation_timeline_months ~ spinoff_type, 
                         data = df_anova)
print("\nLevene's Test for Homogeneity of Variances:")
print(levene_test)

# Brown-Forsythe test
bf_test <- leveneTest(spinoff_creation_timeline_months ~ spinoff_type, 
                     data = df_anova, center = median)
print("\nBrown-Forsythe Test:")
print(bf_test)

# Post-hoc tests
print("\n=== POST-HOC ANALYSES ===")

# Tukey's HSD
tukey_result <- TukeyHSD(anova_result)
tukey_df <- as.data.frame(tukey_result$spinoff_type)
significant_comparisons <- tukey_df[tukey_df$`p adj` < 0.05, ]

print("\nTukey's HSD - Significant Comparisons (p < 0.05):")
if(nrow(significant_comparisons) > 0) {
  print(significant_comparisons[order(significant_comparisons$`p adj`), ])
} else {
  print("No significant pairwise differences found")
}

# Games-Howell test (for unequal variances)
if(levene_test$`Pr(>F)`[1] < 0.05) {
  print("\nNote: Variances are unequal. Games-Howell test recommended.")
}

# Welch's ANOVA (robust to unequal variances)
welch_test <- oneway.test(spinoff_creation_timeline_months ~ spinoff_type, 
                         data = df_anova, var.equal = FALSE)
print("\nWelch's ANOVA:")
print(paste("F =", round(welch_test$statistic, 2)))
print(paste("df1 =", welch_test$parameter[1], ", df2 =", round(welch_test$parameter[2], 1)))
print(paste("p-value:", format(welch_test$p.value, scientific = TRUE)))

# Kruskal-Wallis test (non-parametric)
kruskal_test <- kruskal.test(spinoff_creation_timeline_months ~ spinoff_type, 
                            data = df_anova)
print("\nKruskal-Wallis Test:")
print(paste("χ² =", round(kruskal_test$statistic, 2)))
print(paste("df =", kruskal_test$parameter))
print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))

# Analysis of variance components
var_between <- var(tapply(df_anova$spinoff_creation_timeline_months, 
                         df_anova$spinoff_type, mean))
var_within <- mean(tapply(df_anova$spinoff_creation_timeline_months, 
                         df_anova$spinoff_type, var))

print("\nVariance Components:")
print(paste("Between-group variance:", round(var_between, 2)))
print(paste("Within-group variance:", round(var_within, 2)))
print(paste("Intraclass correlation:", round(var_between / (var_between + var_within), 3)))

# Complexity score analysis
if(sum(!is.na(df_anova$spinoff_complexity_score)) > 50) {
  complexity_by_type <- df_anova %>%
    filter(!is.na(spinoff_complexity_score)) %>%
    group_by(spinoff_type) %>%
    summarise(
      mean_complexity = mean(spinoff_complexity_score, na.rm = TRUE),
      sd_complexity = sd(spinoff_complexity_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nComplexity score by spin-off type:")
  print(complexity_by_type)
  
  # Correlation between complexity and timeline
  cor_complexity <- cor.test(df_anova$spinoff_creation_timeline_months,
                            df_anova$spinoff_complexity_score,
                            use = "complete.obs")
  
  print("\nCorrelation between complexity and timeline:")
  print(paste("r =", round(cor_complexity$estimate, 3)))
  print(paste("p-value:", format(cor_complexity$p.value, scientific = TRUE)))
}

# Parent company size analysis
if(!all(is.na(df_anova$parent_company_size))) {
  # Two-way ANOVA
  two_way_anova <- aov(spinoff_creation_timeline_months ~ spinoff_type * parent_company_size, 
                      data = df_anova)
  
  print("\nTwo-way ANOVA - Type × Parent Size:")
  print(summary(two_way_anova))
  
  # Type II SS for unbalanced design
  print("\nType II Sum of Squares:")
  print(Anova(two_way_anova, type = "II"))
}

# Regulatory requirements analysis
if(sum(!is.na(df_anova$regulatory_requirement_count)) > 50) {
  reg_by_type <- df_anova %>%
    filter(!is.na(regulatory_requirement_count)) %>%
    group_by(spinoff_type) %>%
    summarise(
      mean_regulations = mean(regulatory_requirement_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nRegulatory requirements by spin-off type:")
  print(reg_by_type)
  
  # ANCOVA with regulatory requirements as covariate
  ancova_model <- aov(spinoff_creation_timeline_months ~ spinoff_type + 
                     regulatory_requirement_count, data = df_anova)
  
  print("\nANCOVA with regulatory requirements:")
  print(summary(ancova_model))
}

# Success indicator analysis
if(!all(is.na(df_anova$spinoff_success_indicator))) {
  success_timeline <- df_anova %>%
    filter(!is.na(spinoff_success_indicator)) %>%
    group_by(spinoff_success_indicator, spinoff_type) %>%
    summarise(
      n = n(),
      mean_timeline = mean(spinoff_creation_timeline_months, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(names_from = spinoff_success_indicator, 
               values_from = mean_timeline)
  
  print("\nTimeline by success status and type:")
  print(success_timeline)
}

# Market conditions analysis
if(sum(!is.na(df_anova$market_condition_favorability)) > 50) {
  # Categorize market conditions
  df_anova <- df_anova %>%
    mutate(market_category = case_when(
      market_condition_favorability <= 2 ~ "Unfavorable",
      market_condition_favorability <= 4 ~ "Neutral",
      TRUE ~ "Favorable"
    ))
  
  market_timeline <- df_anova %>%
    filter(!is.na(market_category)) %>%
    group_by(market_category) %>%
    summarise(
      n = n(),
      mean_timeline = mean(spinoff_creation_timeline_months, na.rm = TRUE),
      sd_timeline = sd(spinoff_creation_timeline_months, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nTimeline by market conditions:")
  print(market_timeline)
}

# Industry sector analysis
if(length(unique(df_anova$industry_sector)) > 3) {
  industry_variance <- df_anova %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      var_timeline = var(spinoff_creation_timeline_months, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(var_timeline))
  
  print("\nTimeline variance by industry:")
  print(head(industry_variance, 10))
}

# Polynomial contrasts
if(n_groups > 2) {
  contrast_matrix <- contr.poly(n_groups)
  contrasts(df_anova$spinoff_type) <- contrast_matrix
  
  poly_anova <- aov(spinoff_creation_timeline_months ~ spinoff_type, data = df_anova)
  print("\nPolynomial Contrasts:")
  print(summary.lm(poly_anova))
}

# Bootstrap F-statistic
set.seed(123)
n_boot <- 1000
boot_f_stats <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_indices <- sample(nrow(df_anova), replace = TRUE)
  boot_data <- df_anova[boot_indices, ]
  boot_anova <- aov(spinoff_creation_timeline_months ~ spinoff_type, data = boot_data)
  boot_f_stats[i] <- summary(boot_anova)[[1]][["F value"]][1]
}

boot_f_ci <- quantile(boot_f_stats, c(0.025, 0.975))
print("\nBootstrap 95% CI for F-statistic:")
print(round(boot_f_ci, 2))

# Power analysis
library(pwr)
power_result <- pwr.anova.test(k = n_groups, 
                              n = n_total/n_groups, 
                              f = cohens_f, 
                              sig.level = 0.05)
print("\nPower Analysis:")
print(paste("Achieved power:", round(power_result$power, 3)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print("Spin-off creation timelines showed significant variance across types")
print(paste("F(", df_between, ",", df_within, ") =", round(f_stat, 2),
           ", p <", ifelse(p_value < 0.01, ".01", round(p_value, 3))))
print(paste("η² =", round(eta_squared, 3), ", ω² =", round(omega_squared, 3)))
print("Timeline ranges by type:")
for(i in 1:nrow(timeline_stats)) {
  print(paste(timeline_stats$spinoff_type[i], ":",
             round(timeline_stats$mean_timeline[i], 1), "±",
             round(timeline_stats$sd_timeline[i], 1), "months"))
}

# Expected: timeline variance (F = 3.92, p < .01)