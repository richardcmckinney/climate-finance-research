# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 83: "Binomial model applications: average 50 time steps (IQR [30, 100])"
# Purpose: Analyze binomial option pricing model implementation parameters

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_BINOMIAL_MODEL_USED <- "binomial_model_used"
COL_TIME_STEPS <- "binomial_time_steps"
COL_AMERICAN_VS_EUROPEAN <- "option_style"
COL_EARLY_EXERCISE_PREMIUM <- "early_exercise_premium_pct"
COL_CONVERGENCE_THRESHOLD <- "convergence_threshold"
COL_STAKEHOLDER <- "stakeholder"
COL_COMPUTATIONAL_TIME <- "computation_time_seconds"
COL_MODEL_ACCURACY_SCORE <- "model_accuracy_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    binomial_model_used = as.integer(binomial_model_used),
    binomial_time_steps = as.numeric(binomial_time_steps),
    option_style = factor(option_style),
    early_exercise_premium_pct = as.numeric(early_exercise_premium_pct),
    convergence_threshold = as.numeric(convergence_threshold),
    stakeholder = factor(stakeholder),
    computation_time_seconds = as.numeric(computation_time_seconds),
    model_accuracy_score = as.numeric(model_accuracy_score)
  ) %>%
  filter(!is.na(binomial_time_steps), binomial_time_steps > 0)

# Time steps distribution statistics
n_total <- nrow(df)
time_steps_stats <- df %>%
  summarise(
    n = n(),
    mean = round(mean(binomial_time_steps), 1),
    sd = round(sd(binomial_time_steps), 1),
    median = median(binomial_time_steps),
    q25 = quantile(binomial_time_steps, 0.25),
    q75 = quantile(binomial_time_steps, 0.75),
    iqr = q75 - q25,
    min = min(binomial_time_steps),
    max = max(binomial_time_steps)
  )

print("Binomial model time steps distribution:")
print(time_steps_stats)
print(paste("Average (mean):", time_steps_stats$mean, "time steps"))
print(paste("Median:", time_steps_stats$median, "time steps"))
print(paste("IQR: [", time_steps_stats$q25, ", ", time_steps_stats$q75, "]", sep=""))

# 95% CI for mean
ci_mean <- MeanCI(df$binomial_time_steps, conf.level = 0.95)
print(paste("95% CI for mean: [", round(ci_mean[2], 1), ", ", 
            round(ci_mean[3], 1), "]", sep=""))

# Bootstrap CI for median
set.seed(123)
boot_median <- boot(df$binomial_time_steps, 
                    function(x, i) median(x[i]), 
                    R = 1000)
ci_median <- boot.ci(boot_median, type = "perc")$percent[4:5]
print(paste("95% Bootstrap CI for median: [", round(ci_median[1], 1), ", ", 
            round(ci_median[2], 1), "]", sep=""))

# Time steps categories
time_steps_categories <- df %>%
  mutate(
    steps_category = case_when(
      binomial_time_steps <= 20 ~ "Very Few (≤20)",
      binomial_time_steps <= 30 ~ "Few (21-30)",
      binomial_time_steps <= 50 ~ "Moderate (31-50)",
      binomial_time_steps <= 100 ~ "Many (51-100)",
      binomial_time_steps <= 200 ~ "Very Many (101-200)",
      binomial_time_steps > 200 ~ "Extensive (>200)"
    )
  ) %>%
  group_by(steps_category) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    mean_steps = round(mean(binomial_time_steps), 1),
    .groups = "drop"
  ) %>%
  mutate(
    steps_category = factor(steps_category,
                           levels = c("Very Few (≤20)", "Few (21-30)",
                                    "Moderate (31-50)", "Many (51-100)",
                                    "Very Many (101-200)", "Extensive (>200)"))
  ) %>%
  arrange(steps_category)

print("\nTime steps distribution categories:")
print(time_steps_categories)

# Binomial model usage
if(!all(is.na(df$binomial_model_used))) {
  binomial_usage <- df %>%
    filter(!is.na(binomial_model_used)) %>%
    summarise(
      n = n(),
      using_binomial = sum(binomial_model_used),
      usage_pct = round(mean(binomial_model_used) * 100, 1)
    )
  
  print("\nBinomial model usage:")
  print(binomial_usage)
  
  # Time steps comparison by usage
  steps_by_usage <- df %>%
    filter(!is.na(binomial_model_used)) %>%
    mutate(
      model_status = ifelse(binomial_model_used == 1,
                          "Uses Binomial", "Other Methods")
    ) %>%
    group_by(model_status) %>%
    summarise(
      n = n(),
      mean_steps = round(mean(binomial_time_steps), 1),
      median_steps = median(binomial_time_steps),
      iqr_steps = IQR(binomial_time_steps),
      .groups = "drop"
    )
  
  print("\nTime steps by model usage:")
  print(steps_by_usage)
}

# Option style analysis (American vs European)
if(!all(is.na(df$option_style))) {
  style_analysis <- df %>%
    filter(!is.na(option_style)) %>%
    group_by(option_style) %>%
    summarise(
      n = n(),
      mean_steps = round(mean(binomial_time_steps), 1),
      sd_steps = round(sd(binomial_time_steps), 1),
      median_steps = median(binomial_time_steps),
      percentage = round(n() / sum(!is.na(df$option_style)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_steps))
  
  print("\nTime steps by option style:")
  print(style_analysis)
  
  # t-test for American vs European if both present
  if("American" %in% df$option_style && "European" %in% df$option_style) {
    american_steps <- df$binomial_time_steps[df$option_style == "American" & 
                                            !is.na(df$option_style)]
    european_steps <- df$binomial_time_steps[df$option_style == "European" & 
                                            !is.na(df$option_style)]
    
    if(length(american_steps) >= 20 && length(european_steps) >= 20) {
      t_test <- t.test(american_steps, european_steps)
      
      print("American vs European time steps (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 1)))
    }
  }
}

# Early exercise premium analysis
if(sum(!is.na(df$early_exercise_premium_pct)) > 30) {
  exercise_stats <- df %>%
    filter(!is.na(early_exercise_premium_pct)) %>%
    summarise(
      n = n(),
      mean_premium = round(mean(early_exercise_premium_pct), 2),
      sd_premium = round(sd(early_exercise_premium_pct), 2),
      median_premium = round(median(early_exercise_premium_pct), 2)
    )
  
  print("\nEarly exercise premium statistics:")
  print(exercise_stats)
  
  # Correlation with time steps
  cor_premium <- cor.test(df$binomial_time_steps[!is.na(df$early_exercise_premium_pct)],
                          df$early_exercise_premium_pct[!is.na(df$early_exercise_premium_pct)])
  
  print("Time steps × Early exercise premium correlation:")
  print(paste("r =", round(cor_premium$estimate, 3)))
  print(paste("p-value:", format(cor_premium$p.value, scientific = TRUE)))
}

# Convergence threshold analysis
if(sum(!is.na(df$convergence_threshold)) > 30) {
  convergence_stats <- df %>%
    filter(!is.na(convergence_threshold)) %>%
    summarise(
      n = n(),
      mean_threshold = round(mean(convergence_threshold), 4),
      sd_threshold = round(sd(convergence_threshold), 4),
      median_threshold = round(median(convergence_threshold), 4)
    )
  
  print("\nConvergence threshold statistics:")
  print(convergence_stats)
  
  # Relationship with time steps
  cor_convergence <- cor.test(df$binomial_time_steps[!is.na(df$convergence_threshold)],
                              df$convergence_threshold[!is.na(df$convergence_threshold)])
  
  print("Time steps × Convergence threshold correlation:")
  print(paste("r =", round(cor_convergence$estimate, 3)))
  print(paste("p-value:", format(cor_convergence$p.value, scientific = TRUE)))
}

# Computational time analysis
if(sum(!is.na(df$computation_time_seconds)) > 50) {
  comp_time_stats <- df %>%
    filter(!is.na(computation_time_seconds)) %>%
    summarise(
      n = n(),
      mean_time = round(mean(computation_time_seconds), 2),
      sd_time = round(sd(computation_time_seconds), 2),
      median_time = round(median(computation_time_seconds), 2)
    )
  
  print("\nComputational time statistics:")
  print(comp_time_stats)
  
  # Correlation with time steps
  cor_time <- cor.test(df$binomial_time_steps[!is.na(df$computation_time_seconds)],
                       df$computation_time_seconds[!is.na(df$computation_time_seconds)])
  
  print("Time steps × Computation time correlation:")
  print(paste("r =", round(cor_time$estimate, 3)))
  print(paste("95% CI: [", round(cor_time$conf.int[1], 3), ", ", 
              round(cor_time$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_time$p.value, scientific = TRUE)))
  
  # Time efficiency categories
  time_efficiency <- df %>%
    filter(!is.na(computation_time_seconds)) %>%
    mutate(
      steps_per_second = binomial_time_steps / computation_time_seconds,
      efficiency_category = case_when(
        steps_per_second < 10 ~ "Low Efficiency",
        steps_per_second < 50 ~ "Moderate Efficiency",
        steps_per_second < 100 ~ "High Efficiency",
        steps_per_second >= 100 ~ "Very High Efficiency"
      )
    ) %>%
    group_by(efficiency_category) %>%
    summarise(
      n = n(),
      mean_steps = round(mean(binomial_time_steps), 1),
      mean_time = round(mean(computation_time_seconds), 2),
      .groups = "drop"
    )
  
  print("\nComputational efficiency analysis:")
  print(time_efficiency)
}

# Model accuracy analysis
if(sum(!is.na(df$model_accuracy_score)) > 50) {
  accuracy_analysis <- df %>%
    filter(!is.na(model_accuracy_score)) %>%
    mutate(
      steps_category = cut(binomial_time_steps,
                          breaks = c(0, 30, 50, 100, Inf),
                          labels = c("≤30", "31-50", "51-100", ">100"))
    ) %>%
    group_by(steps_category) %>%
    summarise(
      n = n(),
      mean_accuracy = round(mean(model_accuracy_score), 2),
      sd_accuracy = round(sd(model_accuracy_score), 2),
      .groups = "drop"
    )
  
  print("\nModel accuracy by time steps:")
  print(accuracy_analysis)
  
  # Correlation with accuracy
  cor_accuracy <- cor.test(df$binomial_time_steps[!is.na(df$model_accuracy_score)],
                           df$model_accuracy_score[!is.na(df$model_accuracy_score)])
  
  print("Time steps × Model accuracy correlation:")
  print(paste("r =", round(cor_accuracy$estimate, 3)))
  print(paste("p-value:", format(cor_accuracy$p.value, scientific = TRUE)))
}

# Analysis by stakeholder
stakeholder_steps <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_steps = round(mean(binomial_time_steps), 1),
    sd_steps = round(sd(binomial_time_steps), 1),
    median_steps = median(binomial_time_steps),
    q25 = quantile(binomial_time_steps, 0.25),
    q75 = quantile(binomial_time_steps, 0.75),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(median_steps))

print("\nTime steps by stakeholder (n ≥ 20):")
print(stakeholder_steps)

# Kruskal-Wallis test for stakeholder differences
if(length(unique(df$stakeholder)) > 2) {
  kruskal_test <- kruskal.test(binomial_time_steps ~ stakeholder, data = df)
  print("\nKruskal-Wallis test for stakeholder differences:")
  print(paste("χ² =", round(kruskal_test$statistic, 2)))
  print(paste("df =", kruskal_test$parameter))
  print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))
}

# Linear regression model
if(nrow(df) > 150) {
  df_regression <- df
  
  # Build model with available predictors
  predictors <- c()
  
  if(sum(!is.na(df_regression$model_accuracy_score)) > 100) {
    predictors <- c(predictors, "model_accuracy_score")
  }
  
  if(sum(!is.na(df_regression$computation_time_seconds)) > 100) {
    predictors <- c(predictors, "log(computation_time_seconds + 1)")
  }
  
  if(sum(!is.na(df_regression$convergence_threshold)) > 100) {
    predictors <- c(predictors, "convergence_threshold")
  }
  
  if(length(predictors) > 0 && nrow(df_regression) > 100) {
    formula_str <- paste("binomial_time_steps ~", 
                        paste(predictors, collapse = " + "))
    lm_model <- lm(as.formula(formula_str), data = df_regression)
    
    lm_summary <- summary(lm_model)
    print("\nLinear regression: Time steps predictors")
    print(lm_summary)
    
    # Model diagnostics
    print("\nModel diagnostics:")
    print(paste("R-squared:", round(lm_summary$r.squared, 3)))
    print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
  }
}

# Optimal time steps analysis
optimal_analysis <- df %>%
  mutate(
    within_optimal = ifelse(binomial_time_steps >= 30 & binomial_time_steps <= 100,
                           "Within Optimal Range", "Outside Optimal Range")
  ) %>%
  group_by(within_optimal) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    mean_accuracy = round(mean(model_accuracy_score, na.rm = TRUE), 2),
    mean_time = round(mean(computation_time_seconds, na.rm = TRUE), 2),
    .groups = "drop"
  )

print("\nOptimal range analysis (30-100 steps):")
print(optimal_analysis)

# Distribution shape
skewness_val <- e1071::skewness(df$binomial_time_steps)
kurtosis_val <- e1071::kurtosis(df$binomial_time_steps)

print("\nDistribution shape:")
print(paste("Skewness:", round(skewness_val, 2)))
print(paste("Kurtosis:", round(kurtosis_val, 2)))

# Log transformation analysis (common for count data)
df$log_steps <- log10(df$binomial_time_steps + 1)
log_stats <- df %>%
  summarise(
    mean_log = round(mean(log_steps), 3),
    sd_log = round(sd(log_steps), 3),
    median_log = round(median(log_steps), 3)
  )

print("\nLog-transformed time steps:")
print(log_stats)
print(paste("Back-transformed mean:", round(10^log_stats$mean_log - 1, 1)))

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
      mean_steps = round(mean(binomial_time_steps), 1),
      median_steps = median(binomial_time_steps),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nTime steps trends over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$median_steps, 
                          method = "spearman")
    print("Temporal trend for median (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of binomial model applications:")
print(paste("Mean time steps:", time_steps_stats$mean))
print(paste("Median time steps:", time_steps_stats$median))
print(paste("IQR: [", time_steps_stats$q25, ", ", time_steps_stats$q75, "]", sep=""))
print(paste("Range:", time_steps_stats$min, "-", time_steps_stats$max))

# Expected: average 50 time steps (IQR [30, 100])