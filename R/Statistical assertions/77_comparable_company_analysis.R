# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 77: "Comparable company analysis: median of 8 comparables used (IQR [5, 12])"
# Purpose: Analyze comparable company analysis practices with distribution statistics

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_NUM_COMPARABLES <- "number_of_comparables_used"
COL_COMPARABLES_SOURCE <- "comparables_data_source"
COL_COMPARABLES_CRITERIA <- "comparables_selection_criteria"
COL_COMPARABLES_FREQUENCY <- "comparables_update_frequency"
COL_STAKEHOLDER <- "stakeholder"
COL_SECTOR_FOCUS <- "sector_focus"
COL_DEAL_SIZE_CATEGORY <- "typical_deal_size_category"
COL_VALUATION_ACCURACY <- "valuation_accuracy_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    number_of_comparables_used = as.numeric(number_of_comparables_used),
    comparables_data_source = factor(comparables_data_source),
    comparables_selection_criteria = as.character(comparables_selection_criteria),
    comparables_update_frequency = factor(comparables_update_frequency),
    stakeholder = factor(stakeholder),
    sector_focus = factor(sector_focus),
    typical_deal_size_category = factor(typical_deal_size_category),
    valuation_accuracy_score = as.numeric(valuation_accuracy_score)
  ) %>%
  filter(!is.na(number_of_comparables_used), number_of_comparables_used > 0)

# Distribution statistics for number of comparables
n_total <- nrow(df)
comparables_stats <- df %>%
  summarise(
    n = n(),
    mean = round(mean(number_of_comparables_used), 1),
    sd = round(sd(number_of_comparables_used), 1),
    median = median(number_of_comparables_used),
    q25 = quantile(number_of_comparables_used, 0.25),
    q75 = quantile(number_of_comparables_used, 0.75),
    iqr = q75 - q25,
    min = min(number_of_comparables_used),
    max = max(number_of_comparables_used)
  )

print("Number of comparables used - Distribution statistics:")
print(comparables_stats)
print(paste("Median:", comparables_stats$median, "comparables"))
print(paste("IQR: [", comparables_stats$q25, ", ", comparables_stats$q75, "]", sep=""))

# 95% CI for median using bootstrap
set.seed(123)
boot_median <- boot(df$number_of_comparables_used, 
                    function(x, i) median(x[i]), 
                    R = 1000)
ci_median <- boot.ci(boot_median, type = "perc")$percent[4:5]

print(paste("95% Bootstrap CI for median: [", round(ci_median[1], 1), ", ", 
            round(ci_median[2], 1), "]", sep=""))

# 95% CI for mean
ci_mean <- MeanCI(df$number_of_comparables_used, conf.level = 0.95)
print(paste("95% CI for mean: [", round(ci_mean[2], 1), ", ", 
            round(ci_mean[3], 1), "]", sep=""))

# Distribution categories
comparables_categories <- df %>%
  mutate(
    comparables_category = case_when(
      number_of_comparables_used <= 3 ~ "Very Few (≤3)",
      number_of_comparables_used <= 5 ~ "Few (4-5)",
      number_of_comparables_used <= 8 ~ "Moderate (6-8)",
      number_of_comparables_used <= 12 ~ "Many (9-12)",
      number_of_comparables_used <= 20 ~ "Very Many (13-20)",
      number_of_comparables_used > 20 ~ "Extensive (>20)"
    )
  ) %>%
  group_by(comparables_category) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  ) %>%
  mutate(
    comparables_category = factor(comparables_category,
                                 levels = c("Very Few (≤3)", "Few (4-5)", 
                                          "Moderate (6-8)", "Many (9-12)",
                                          "Very Many (13-20)", "Extensive (>20)"))
  ) %>%
  arrange(comparables_category)

print("\nDistribution of comparable count categories:")
print(comparables_categories)

# Percentile distribution
percentiles <- quantile(df$number_of_comparables_used, 
                        probs = c(0.1, 0.25, 0.5, 0.75, 0.9, 0.95))
print("\nPercentile distribution:")
for(i in 1:length(percentiles)) {
  print(paste(names(percentiles)[i], ":", percentiles[i], "comparables"))
}

# Data source analysis
if(!all(is.na(df$comparables_data_source))) {
  source_breakdown <- df %>%
    filter(!is.na(comparables_data_source)) %>%
    group_by(comparables_data_source) %>%
    summarise(
      n = n(),
      mean_comparables = round(mean(number_of_comparables_used), 1),
      median_comparables = median(number_of_comparables_used),
      percentage = round(n() / sum(!is.na(df$comparables_data_source)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
  
  print("\nComparables by data source:")
  print(source_breakdown)
  
  # ANOVA for source differences
  if(length(unique(df$comparables_data_source[!is.na(df$comparables_data_source)])) > 2) {
    anova_source <- aov(number_of_comparables_used ~ comparables_data_source, 
                       data = df %>% filter(!is.na(comparables_data_source)))
    print("\nANOVA - Number of comparables by data source:")
    print(summary(anova_source))
  }
}

# Selection criteria analysis
if(!all(is.na(df$comparables_selection_criteria))) {
  # Parse comma-separated criteria
  criteria_df <- df %>%
    filter(!is.na(comparables_selection_criteria)) %>%
    mutate(criteria_list = strsplit(comparables_selection_criteria, ",")) %>%
    unnest(criteria_list) %>%
    mutate(criteria_list = trimws(criteria_list))
  
  if(nrow(criteria_df) > 0) {
    criteria_counts <- criteria_df %>%
      group_by(criteria_list) %>%
      summarise(
        n = n(),
        mean_comparables = round(mean(number_of_comparables_used), 1),
        percentage = round(n() / nrow(df) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(n)) %>%
      head(15)
    
    print("\nTop 15 selection criteria used:")
    print(criteria_counts)
  }
}

# Update frequency analysis
if(!all(is.na(df$comparables_update_frequency))) {
  frequency_analysis <- df %>%
    filter(!is.na(comparables_update_frequency)) %>%
    group_by(comparables_update_frequency) %>%
    summarise(
      n = n(),
      mean_comparables = round(mean(number_of_comparables_used), 1),
      median_comparables = median(number_of_comparables_used),
      percentage = round(n() / sum(!is.na(df$comparables_update_frequency)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(comparables_update_frequency)
  
  print("\nComparables by update frequency:")
  print(frequency_analysis)
}

# Analysis by stakeholder
stakeholder_comparables <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_comparables = round(mean(number_of_comparables_used), 1),
    sd_comparables = round(sd(number_of_comparables_used), 1),
    median_comparables = median(number_of_comparables_used),
    q25 = quantile(number_of_comparables_used, 0.25),
    q75 = quantile(number_of_comparables_used, 0.75),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(median_comparables))

print("\nComparables by stakeholder (n ≥ 20):")
print(stakeholder_comparables)

# Kruskal-Wallis test for stakeholder differences
if(length(unique(df$stakeholder)) > 2) {
  kruskal_test <- kruskal.test(number_of_comparables_used ~ stakeholder, data = df)
  print("\nKruskal-Wallis test for stakeholder differences:")
  print(paste("χ² =", round(kruskal_test$statistic, 2)))
  print(paste("df =", kruskal_test$parameter))
  print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))
  
  # Post-hoc pairwise comparisons if significant
  if(kruskal_test$p.value < 0.05) {
    pairwise_test <- pairwise.wilcox.test(df$number_of_comparables_used, 
                                          df$stakeholder,
                                          p.adjust.method = "bonferroni")
    print("\nPairwise Wilcoxon tests (Bonferroni adjusted):")
    print(round(pairwise_test$p.value, 4))
  }
}

# Sector focus analysis
if(!all(is.na(df$sector_focus))) {
  sector_comparables <- df %>%
    filter(!is.na(sector_focus)) %>%
    group_by(sector_focus) %>%
    summarise(
      n = n(),
      mean_comparables = round(mean(number_of_comparables_used), 1),
      median_comparables = median(number_of_comparables_used),
      iqr = IQR(number_of_comparables_used),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(median_comparables)) %>%
    head(10)
  
  print("\nTop 10 sectors by median comparables (n ≥ 15):")
  print(sector_comparables)
}

# Deal size category analysis
if(!all(is.na(df$typical_deal_size_category))) {
  deal_size_comparables <- df %>%
    filter(!is.na(typical_deal_size_category)) %>%
    group_by(typical_deal_size_category) %>%
    summarise(
      n = n(),
      mean_comparables = round(mean(number_of_comparables_used), 1),
      median_comparables = median(number_of_comparables_used),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(typical_deal_size_category)
  
  print("\nComparables by deal size category (n ≥ 10):")
  print(deal_size_comparables)
  
  # Correlation test (assuming ordinal deal sizes)
  if(nrow(deal_size_comparables) > 2) {
    # Convert to numeric for correlation
    df_deal_cor <- df %>%
      filter(!is.na(typical_deal_size_category)) %>%
      mutate(
        deal_size_numeric = as.numeric(factor(typical_deal_size_category))
      )
    
    cor_deal <- cor.test(df_deal_cor$number_of_comparables_used,
                         df_deal_cor$deal_size_numeric,
                         method = "spearman")
    
    print("\nDeal size × Number of comparables correlation (Spearman):")
    print(paste("ρ =", round(cor_deal$estimate, 3)))
    print(paste("p-value:", format(cor_deal$p.value, scientific = TRUE)))
  }
}

# Valuation accuracy relationship
if(sum(!is.na(df$valuation_accuracy_score)) > 50) {
  # Correlation with accuracy
  cor_accuracy <- cor.test(df$number_of_comparables_used[!is.na(df$valuation_accuracy_score)],
                           df$valuation_accuracy_score[!is.na(df$valuation_accuracy_score)])
  
  print("\nNumber of comparables × Valuation accuracy correlation:")
  print(paste("r =", round(cor_accuracy$estimate, 3)))
  print(paste("95% CI: [", round(cor_accuracy$conf.int[1], 3), ", ", 
              round(cor_accuracy$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_accuracy$p.value, scientific = TRUE)))
  
  # Categorized accuracy analysis
  accuracy_by_comparables <- df %>%
    filter(!is.na(valuation_accuracy_score)) %>%
    mutate(
      comparables_group = case_when(
        number_of_comparables_used <= 5 ~ "Few (≤5)",
        number_of_comparables_used <= 10 ~ "Moderate (6-10)",
        number_of_comparables_used <= 15 ~ "Many (11-15)",
        number_of_comparables_used > 15 ~ "Extensive (>15)"
      )
    ) %>%
    group_by(comparables_group) %>%
    summarise(
      n = n(),
      mean_accuracy = round(mean(valuation_accuracy_score), 2),
      sd_accuracy = round(sd(valuation_accuracy_score), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("\nValuation accuracy by number of comparables (n ≥ 10):")
  print(accuracy_by_comparables)
}

# Linear regression model
if(nrow(df) > 150) {
  df_regression <- df
  
  # Build model with available predictors
  predictors <- c()
  
  if(sum(!is.na(df_regression$valuation_accuracy_score)) > 100) {
    predictors <- c(predictors, "valuation_accuracy_score")
  }
  
  if(length(unique(df_regression$stakeholder)) > 1) {
    predictors <- c(predictors, "stakeholder")
  }
  
  if(sum(!is.na(df_regression$sector_focus)) > 100) {
    predictors <- c(predictors, "sector_focus")
  }
  
  if(length(predictors) > 0 && nrow(df_regression) > 100) {
    formula_str <- paste("number_of_comparables_used ~", 
                        paste(predictors, collapse = " + "))
    lm_model <- lm(as.formula(formula_str), data = df_regression)
    
    lm_summary <- summary(lm_model)
    print("\nLinear regression: Predictors of comparables count")
    print(lm_summary)
    
    # Model diagnostics
    print("\nModel diagnostics:")
    print(paste("R-squared:", round(lm_summary$r.squared, 3)))
    print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
  }
}

# Outlier analysis
outliers <- boxplot.stats(df$number_of_comparables_used)$out
n_outliers <- length(outliers)
pct_outliers <- round(n_outliers / nrow(df) * 100, 1)

print("\nOutlier analysis:")
print(paste("Number of outliers:", n_outliers))
print(paste("Percentage of outliers:", pct_outliers, "%"))
if(n_outliers > 0 && n_outliers <= 20) {
  print(paste("Outlier values:", paste(sort(outliers), collapse = ", ")))
}

# Robust statistics (trimmed mean and Winsorized mean)
trimmed_mean <- mean(df$number_of_comparables_used, trim = 0.1)
winsorized_mean <- mean(Winsorize(df$number_of_comparables_used, probs = c(0.05, 0.95)))

print("\nRobust location measures:")
print(paste("10% Trimmed mean:", round(trimmed_mean, 1)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 1)))
print(paste("Median (robust):", comparables_stats$median))

# Distribution shape
skewness_val <- e1071::skewness(df$number_of_comparables_used)
kurtosis_val <- e1071::kurtosis(df$number_of_comparables_used)

print("\nDistribution shape:")
print(paste("Skewness:", round(skewness_val, 2)))
print(paste("Kurtosis:", round(kurtosis_val, 2)))

# Shapiro-Wilk test for normality (if n <= 5000)
if(nrow(df) <= 5000) {
  shapiro_test <- shapiro.test(df$number_of_comparables_used)
  print(paste("Shapiro-Wilk test: W =", round(shapiro_test$statistic, 4),
              ", p =", format(shapiro_test$p.value, scientific = TRUE)))
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
      mean_comparables = round(mean(number_of_comparables_used), 1),
      median_comparables = median(number_of_comparables_used),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nComparables trend over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$median_comparables, 
                          method = "spearman")
    print("Temporal trend for median (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of comparable company analysis:")
print(paste("Median:", comparables_stats$median, "comparables"))
print(paste("IQR: [", comparables_stats$q25, ", ", comparables_stats$q75, "]", sep=""))
print(paste("Mean:", comparables_stats$mean, "comparables (SD:", comparables_stats$sd, ")"))
print(paste("Range:", comparables_stats$min, "-", comparables_stats$max))

# Expected: median of 8 comparables used (IQR [5, 12])