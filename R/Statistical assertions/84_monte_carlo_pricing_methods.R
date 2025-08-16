# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 84: "Monte Carlo pricing methods: 10,000 simulations median (95% CI [8,000, 15,000])"
# Purpose: Analyze Monte Carlo simulation parameters and implementation

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MONTE_CARLO_USED <- "monte_carlo_method_used"
COL_NUM_SIMULATIONS <- "monte_carlo_simulations"
COL_RANDOM_NUMBER_GENERATOR <- "random_number_generator_type"
COL_VARIANCE_REDUCTION <- "variance_reduction_technique"
COL_CONVERGENCE_METRIC <- "convergence_metric_used"
COL_STAKEHOLDER <- "stakeholder"
COL_COMPUTATION_TIME <- "monte_carlo_computation_time"
COL_CONFIDENCE_INTERVAL_WIDTH <- "confidence_interval_width"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    monte_carlo_method_used = as.integer(monte_carlo_method_used),
    monte_carlo_simulations = as.numeric(monte_carlo_simulations),
    random_number_generator_type = factor(random_number_generator_type),
    variance_reduction_technique = factor(variance_reduction_technique),
    convergence_metric_used = factor(convergence_metric_used),
    stakeholder = factor(stakeholder),
    monte_carlo_computation_time = as.numeric(monte_carlo_computation_time),
    confidence_interval_width = as.numeric(confidence_interval_width)
  ) %>%
  filter(!is.na(monte_carlo_simulations), monte_carlo_simulations > 0)

# Simulation count statistics
n_total <- nrow(df)
simulation_stats <- df %>%
  summarise(
    n = n(),
    mean = round(mean(monte_carlo_simulations), 0),
    sd = round(sd(monte_carlo_simulations), 0),
    median = median(monte_carlo_simulations),
    q25 = quantile(monte_carlo_simulations, 0.25),
    q75 = quantile(monte_carlo_simulations, 0.75),
    iqr = q75 - q25,
    min = min(monte_carlo_simulations),
    max = max(monte_carlo_simulations)
  )

print("Monte Carlo simulations distribution:")
print(simulation_stats)
print(paste("Median:", format(simulation_stats$median, scientific = FALSE), "simulations"))
print(paste("IQR: [", format(simulation_stats$q25, scientific = FALSE), ", ", 
            format(simulation_stats$q75, scientific = FALSE), "]", sep=""))

# 95% CI for median using bootstrap
set.seed(123)
boot_median <- boot(df$monte_carlo_simulations, 
                    function(x, i) median(x[i]), 
                    R = 1000)
ci_median <- boot.ci(boot_median, type = "perc")$percent[4:5]
print(paste("95% Bootstrap CI for median: [", 
            format(ci_median[1], scientific = FALSE), ", ", 
            format(ci_median[2], scientific = FALSE), "]", sep=""))

# 95% CI for mean
ci_mean <- MeanCI(df$monte_carlo_simulations, conf.level = 0.95)
print(paste("Mean:", format(simulation_stats$mean, scientific = FALSE)))
print(paste("95% CI for mean: [", format(ci_mean[2], scientific = FALSE), ", ", 
            format(ci_mean[3], scientific = FALSE), "]", sep=""))

# Simulation count categories
simulation_categories <- df %>%
  mutate(
    simulation_category = case_when(
      monte_carlo_simulations < 1000 ~ "<1,000",
      monte_carlo_simulations < 5000 ~ "1,000-4,999",
      monte_carlo_simulations < 10000 ~ "5,000-9,999",
      monte_carlo_simulations < 20000 ~ "10,000-19,999",
      monte_carlo_simulations < 50000 ~ "20,000-49,999",
      monte_carlo_simulations >= 50000 ~ "≥50,000"
    )
  ) %>%
  group_by(simulation_category) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    mean_sims = round(mean(monte_carlo_simulations), 0),
    .groups = "drop"
  ) %>%
  mutate(
    simulation_category = factor(simulation_category,
                                levels = c("<1,000", "1,000-4,999", "5,000-9,999",
                                         "10,000-19,999", "20,000-49,999", "≥50,000"))
  ) %>%
  arrange(simulation_category)

print("\nSimulation count distribution:")
print(simulation_categories)

# Monte Carlo usage analysis
if(!all(is.na(df$monte_carlo_method_used))) {
  mc_usage <- df %>%
    filter(!is.na(monte_carlo_method_used)) %>%
    summarise(
      n = n(),
      using_mc = sum(monte_carlo_method_used),
      usage_pct = round(mean(monte_carlo_method_used) * 100, 1)
    )
  
  print("\nMonte Carlo method usage:")
  print(mc_usage)
  
  # Simulation count by usage
  sims_by_usage <- df %>%
    filter(!is.na(monte_carlo_method_used)) %>%
    mutate(
      mc_status = ifelse(monte_carlo_method_used == 1,
                        "Uses Monte Carlo", "Other Methods")
    ) %>%
    group_by(mc_status) %>%
    summarise(
      n = n(),
      mean_sims = round(mean(monte_carlo_simulations), 0),
      median_sims = median(monte_carlo_simulations),
      .groups = "drop"
    )
  
  print("\nSimulations by Monte Carlo usage:")
  print(sims_by_usage)
}

# Random number generator analysis
if(!all(is.na(df$random_number_generator_type))) {
  rng_analysis <- df %>%
    filter(!is.na(random_number_generator_type)) %>%
    group_by(random_number_generator_type) %>%
    summarise(
      n = n(),
      mean_sims = round(mean(monte_carlo_simulations), 0),
      median_sims = median(monte_carlo_simulations),
      percentage = round(n() / sum(!is.na(df$random_number_generator_type)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
  
  print("\nSimulations by RNG type:")
  print(rng_analysis)
}

# Variance reduction techniques
if(!all(is.na(df$variance_reduction_technique))) {
  variance_reduction <- df %>%
    filter(!is.na(variance_reduction_technique)) %>%
    group_by(variance_reduction_technique) %>%
    summarise(
      n = n(),
      mean_sims = round(mean(monte_carlo_simulations), 0),
      median_sims = median(monte_carlo_simulations),
      percentage = round(n() / sum(!is.na(df$variance_reduction_technique)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("\nVariance reduction techniques used:")
  print(variance_reduction)
  
  # ANOVA for technique differences
  if(length(unique(df$variance_reduction_technique[!is.na(df$variance_reduction_technique)])) > 2) {
    anova_technique <- aov(log10(monte_carlo_simulations + 1) ~ variance_reduction_technique, 
                          data = df %>% filter(!is.na(variance_reduction_technique)))
    print("\nANOVA - Simulations by variance reduction (log scale):")
    print(summary(anova_technique))
  }
}

# Computation time analysis
if(sum(!is.na(df$monte_carlo_computation_time)) > 50) {
  comp_time_analysis <- df %>%
    filter(!is.na(monte_carlo_computation_time)) %>%
    summarise(
      n = n(),
      mean_time = round(mean(monte_carlo_computation_time), 2),
      sd_time = round(sd(monte_carlo_computation_time), 2),
      median_time = round(median(monte_carlo_computation_time), 2)
    )
  
  print("\nComputation time statistics (seconds):")
  print(comp_time_analysis)
  
  # Correlation with simulation count
  cor_time <- cor.test(df$monte_carlo_simulations[!is.na(df$monte_carlo_computation_time)],
                       df$monte_carlo_computation_time[!is.na(df$monte_carlo_computation_time)])
  
  print("Simulations × Computation time correlation:")
  print(paste("r =", round(cor_time$estimate, 3)))
  print(paste("95% CI: [", round(cor_time$conf.int[1], 3), ", ", 
              round(cor_time$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_time$p.value, scientific = TRUE)))
  
  # Efficiency analysis
  efficiency_analysis <- df %>%
    filter(!is.na(monte_carlo_computation_time), monte_carlo_computation_time > 0) %>%
    mutate(
      sims_per_second = monte_carlo_simulations / monte_carlo_computation_time,
      efficiency_category = cut(sims_per_second,
                               breaks = c(0, 100, 500, 1000, 5000, Inf),
                               labels = c("Very Low", "Low", "Moderate", "High", "Very High"))
    ) %>%
    group_by(efficiency_category) %>%
    summarise(
      n = n(),
      mean_sims = round(mean(monte_carlo_simulations), 0),
      mean_time = round(mean(monte_carlo_computation_time), 2),
      .groups = "drop"
    )
  
  print("\nComputational efficiency:")
  print(efficiency_analysis)
}

# Confidence interval width analysis
if(sum(!is.na(df$confidence_interval_width)) > 50) {
  ci_width_analysis <- df %>%
    filter(!is.na(confidence_interval_width)) %>%
    summarise(
      n = n(),
      mean_width = round(mean(confidence_interval_width), 3),
      sd_width = round(sd(confidence_interval_width), 3),
      median_width = round(median(confidence_interval_width), 3)
    )
  
  print("\nConfidence interval width statistics:")
  print(ci_width_analysis)
  
  # Relationship with simulation count
  cor_width <- cor.test(df$monte_carlo_simulations[!is.na(df$confidence_interval_width)],
                        df$confidence_interval_width[!is.na(df$confidence_interval_width)])
  
  print("Simulations × CI width correlation:")
  print(paste("r =", round(cor_width$estimate, 3)))
  print(paste("p-value:", format(cor_width$p.value, scientific = TRUE)))
  
  # Expected inverse relationship
  df_ci <- df %>%
    filter(!is.na(confidence_interval_width)) %>%
    mutate(
      sim_category = cut(monte_carlo_simulations,
                        breaks = c(0, 5000, 10000, 20000, Inf),
                        labels = c("<5K", "5-10K", "10-20K", ">20K"))
    ) %>%
    group_by(sim_category) %>%
    summarise(
      n = n(),
      mean_width = round(mean(confidence_interval_width), 3),
      sd_width = round(sd(confidence_interval_width), 3),
      .groups = "drop"
    )
  
  print("\nCI width by simulation count:")
  print(df_ci)
}

# Convergence metric analysis
if(!all(is.na(df$convergence_metric_used))) {
  convergence_analysis <- df %>%
    filter(!is.na(convergence_metric_used)) %>%
    group_by(convergence_metric_used) %>%
    summarise(
      n = n(),
      mean_sims = round(mean(monte_carlo_simulations), 0),
      median_sims = median(monte_carlo_simulations),
      percentage = round(n() / sum(!is.na(df$convergence_metric_used)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("\nConvergence metrics used:")
  print(convergence_analysis)
}

# Analysis by stakeholder
stakeholder_mc <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_sims = round(mean(monte_carlo_simulations), 0),
    median_sims = median(monte_carlo_simulations),
    q25 = quantile(monte_carlo_simulations, 0.25),
    q75 = quantile(monte_carlo_simulations, 0.75),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(median_sims))

print("\nSimulations by stakeholder (n ≥ 20):")
print(stakeholder_mc)

# Kruskal-Wallis test for stakeholder differences
if(length(unique(df$stakeholder)) > 2) {
  kruskal_test <- kruskal.test(monte_carlo_simulations ~ stakeholder, data = df)
  print("\nKruskal-Wallis test for stakeholder differences:")
  print(paste("χ² =", round(kruskal_test$statistic, 2)))
  print(paste("df =", kruskal_test$parameter))
  print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))
}

# Optimal simulation range analysis
optimal_range <- df %>%
  mutate(
    in_optimal = ifelse(monte_carlo_simulations >= 8000 & 
                       monte_carlo_simulations <= 15000,
                       "Within Optimal Range", "Outside Optimal Range")
  ) %>%
  group_by(in_optimal) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    mean_ci_width = round(mean(confidence_interval_width, na.rm = TRUE), 3),
    mean_time = round(mean(monte_carlo_computation_time, na.rm = TRUE), 2),
    .groups = "drop"
  )

print("\nOptimal range analysis (8,000-15,000):")
print(optimal_range)

# Linear regression model (log scale)
if(nrow(df) > 150) {
  df_regression <- df %>%
    mutate(log_sims = log10(monte_carlo_simulations + 1))
  
  # Build model with available predictors
  predictors <- c()
  
  if(sum(!is.na(df_regression$monte_carlo_computation_time)) > 100) {
    df_regression$log_time <- log10(df_regression$monte_carlo_computation_time + 1)
    predictors <- c(predictors, "log_time")
  }
  
  if(sum(!is.na(df_regression$confidence_interval_width)) > 100) {
    predictors <- c(predictors, "confidence_interval_width")
  }
  
  if(length(predictors) > 0 && nrow(df_regression) > 100) {
    formula_str <- paste("log_sims ~", paste(predictors, collapse = " + "))
    lm_model <- lm(as.formula(formula_str), data = df_regression)
    
    lm_summary <- summary(lm_model)
    print("\nLinear regression: Log(simulations) predictors")
    print(lm_summary)
    
    # Model diagnostics
    print("\nModel diagnostics:")
    print(paste("R-squared:", round(lm_summary$r.squared, 3)))
    print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
  }
}

# Distribution shape
skewness_val <- e1071::skewness(df$monte_carlo_simulations)
kurtosis_val <- e1071::kurtosis(df$monte_carlo_simulations)

print("\nDistribution shape:")
print(paste("Skewness:", round(skewness_val, 2)))
print(paste("Kurtosis:", round(kurtosis_val, 2)))

# Log transformation statistics
df$log_sims <- log10(df$monte_carlo_simulations + 1)
log_stats <- df %>%
  summarise(
    mean_log = round(mean(log_sims), 3),
    sd_log = round(sd(log_sims), 3),
    median_log = round(median(log_sims), 3)
  )

print("\nLog-transformed simulations:")
print(log_stats)
print(paste("Back-transformed median:", round(10^log_stats$median_log - 1, 0)))

# Common simulation counts
common_counts <- df %>%
  group_by(monte_carlo_simulations) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 5) %>%
  arrange(desc(n)) %>%
  head(10)

print("\nMost common simulation counts (n ≥ 5):")
print(common_counts)

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
      mean_sims = round(mean(monte_carlo_simulations), 0),
      median_sims = median(monte_carlo_simulations),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nSimulation trends over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$median_sims, 
                          method = "spearman")
    print("Temporal trend for median (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of Monte Carlo pricing methods:")
print(paste("Median simulations:", format(simulation_stats$median, scientific = FALSE)))
print(paste("95% CI for median: [", format(ci_median[1], scientific = FALSE), ", ", 
            format(ci_median[2], scientific = FALSE), "]", sep=""))
print(paste("IQR: [", format(simulation_stats$q25, scientific = FALSE), ", ", 
            format(simulation_stats$q75, scientific = FALSE), "]", sep=""))

# Expected: 10,000 simulations median (95% CI [8,000, 15,000])