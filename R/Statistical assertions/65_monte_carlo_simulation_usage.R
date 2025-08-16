# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 65: "Monte Carlo simulation usage was reported by 42% of respondents (95% CI [38.7%, 45.3%]) with higher adoption among quantitative analysts"
# Purpose: Proportion analysis with Wilson confidence intervals and comparison across analyst types

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MONTE_CARLO_USAGE <- "monte_carlo_simulation_usage"
COL_ANALYST_TYPE <- "analyst_type_category"
COL_STAKEHOLDER <- "stakeholder"
COL_QUANTITATIVE_BACKGROUND <- "quantitative_analysis_background"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    monte_carlo_simulation_usage = as.integer(monte_carlo_simulation_usage),
    analyst_type_category = factor(analyst_type_category),
    stakeholder = factor(stakeholder),
    quantitative_analysis_background = as.integer(quantitative_analysis_background)
  ) %>%
  filter(!is.na(monte_carlo_simulation_usage))

# Overall proportion analysis
n_total <- nrow(df)
n_usage <- sum(df$monte_carlo_simulation_usage, na.rm = TRUE)
prop_usage <- n_usage / n_total

print("Overall Monte Carlo simulation usage:")
print(paste("Total respondents:", n_total))
print(paste("Using Monte Carlo:", n_usage))
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

# Breakdown by analyst type
if(!all(is.na(df$analyst_type_category))) {
  analyst_breakdown <- df %>%
    filter(!is.na(analyst_type_category)) %>%
    group_by(analyst_type_category) %>%
    summarise(
      n = n(),
      usage_count = sum(monte_carlo_simulation_usage, na.rm = TRUE),
      proportion = round(mean(monte_carlo_simulation_usage, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%  # Only analyst types with sufficient sample size
    arrange(desc(proportion))
  
  print("Monte Carlo usage by analyst type (n ≥ 20):")
  print(analyst_breakdown)
  
  # Calculate Wilson CIs for each analyst type
  analyst_breakdown_ci <- analyst_breakdown %>%
    rowwise() %>%
    mutate(
      ci_data = list(BinomCI(usage_count, n, conf.level = 0.95, method = "wilson")),
      ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
      ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
      ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
    ) %>%
    select(-ci_data)
  
  print("Analyst type proportions with 95% Wilson CIs:")
  print(analyst_breakdown_ci %>% select(analyst_type_category, n, proportion, ci_text))
  
  # Test for significant variation by analyst type
  analyst_table <- table(df$analyst_type_category[!is.na(df$analyst_type_category)], 
                         df$monte_carlo_simulation_usage[!is.na(df$analyst_type_category)])
  
  chi_test <- chisq.test(analyst_table)
  print("Chi-square test for analyst type variation:")
  print(paste("χ²(", chi_test$parameter, ") =", round(chi_test$statistic, 2)))
  print(paste("p-value:", format(chi_test$p.value, scientific = TRUE)))
  
  # Cramér's V for effect size
  cramers_v <- sqrt(chi_test$statistic / (sum(analyst_table) * (min(dim(analyst_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Quantitative vs Non-quantitative background comparison
if(!all(is.na(df$quantitative_analysis_background))) {
  quant_comparison <- df %>%
    filter(!is.na(quantitative_analysis_background)) %>%
    mutate(
      background_type = ifelse(quantitative_analysis_background == 1, 
                              "Quantitative Background", "Non-Quantitative Background")
    ) %>%
    group_by(background_type) %>%
    summarise(
      n = n(),
      usage_count = sum(monte_carlo_simulation_usage, na.rm = TRUE),
      proportion = round(mean(monte_carlo_simulation_usage, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    )
  
  print("Monte Carlo usage by quantitative background:")
  print(quant_comparison)
  
  # Calculate Wilson CIs for background comparison
  quant_comparison_ci <- quant_comparison %>%
    rowwise() %>%
    mutate(
      ci_data = list(BinomCI(usage_count, n, conf.level = 0.95, method = "wilson")),
      ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
      ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1)
    ) %>%
    select(-ci_data)
  
  print("Background comparison with 95% Wilson CIs:")
  print(quant_comparison_ci)
  
  # Fisher's exact test for background comparison
  if(nrow(quant_comparison) == 2) {
    background_table <- df %>%
      filter(!is.na(quantitative_analysis_background)) %>%
      mutate(background_type = ifelse(quantitative_analysis_background == 1, 
                                    "Quantitative", "Non-Quantitative")) %>%
      select(background_type, monte_carlo_simulation_usage) %>%
      table()
    
    fisher_test <- fisher.test(background_table)
    print("Fisher's exact test (Quantitative vs Non-Quantitative):")
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
    usage_count = sum(monte_carlo_simulation_usage, na.rm = TRUE),
    proportion = round(mean(monte_carlo_simulation_usage, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(proportion))

print("Monte Carlo usage by stakeholder:")
print(stakeholder_breakdown)

# Statistical comparison across stakeholders
stakeholder_table <- table(df$stakeholder, df$monte_carlo_simulation_usage)
chi_stakeholder <- chisq.test(stakeholder_table)
print("Chi-square test for stakeholder variation:")
print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))

# Specific focus on quantitative analysts
if(!all(is.na(df$analyst_type_category))) {
  # Identify quantitative analyst categories
  quant_analyst_categories <- c("Quantitative Analyst", "Risk Analyst", "Financial Modeler", 
                               "Data Scientist", "Statistician")
  
  df_quant_focus <- df %>%
    filter(!is.na(analyst_type_category)) %>%
    mutate(
      is_quantitative_analyst = ifelse(analyst_type_category %in% quant_analyst_categories, 1, 0)
    )
  
  quant_analyst_comparison <- df_quant_focus %>%
    group_by(is_quantitative_analyst) %>%
    summarise(
      n = n(),
      usage_count = sum(monte_carlo_simulation_usage, na.rm = TRUE),
      proportion = round(mean(monte_carlo_simulation_usage, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      analyst_group = ifelse(is_quantitative_analyst == 1, "Quantitative Analysts", "Other Analysts")
    ) %>%
    select(-is_quantitative_analyst)
  
  print("Monte Carlo usage: Quantitative analysts vs Others:")
  print(quant_analyst_comparison)
  
  # Test for difference
  if(nrow(quant_analyst_comparison) == 2) {
    quant_other_table <- table(df_quant_focus$is_quantitative_analyst, 
                              df_quant_focus$monte_carlo_simulation_usage)
    
    fisher_quant <- fisher.test(quant_other_table)
    print("Quantitative analysts vs Others comparison:")
    print(paste("Fisher's exact test p-value:", format(fisher_quant$p.value, scientific = TRUE)))
    print(paste("Odds ratio:", round(fisher_quant$estimate, 2)))
  }
}

# Combined analyst type and stakeholder analysis
if(!all(is.na(df$analyst_type_category))) {
  combined_breakdown <- df %>%
    filter(!is.na(analyst_type_category), !is.na(stakeholder)) %>%
    group_by(stakeholder, analyst_type_category) %>%
    summarise(
      n = n(),
      usage_count = sum(monte_carlo_simulation_usage, na.rm = TRUE),
      proportion = round(mean(monte_carlo_simulation_usage, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(stakeholder, desc(proportion))
  
  if(nrow(combined_breakdown) > 0) {
    print("Monte Carlo usage by stakeholder and analyst type (n ≥ 10):")
    print(combined_breakdown)
  }
}

# Usage pattern analysis
usage_patterns <- df %>%
  mutate(
    usage_status = ifelse(monte_carlo_simulation_usage == 1, "Uses Monte Carlo", "Does Not Use")
  ) %>%
  count(usage_status) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("Monte Carlo simulation usage patterns:")
print(usage_patterns)

# Post-hoc pairwise comparisons for analyst types (if significant)
if(!all(is.na(df$analyst_type_category)) && exists("chi_test") && chi_test$p.value < 0.05) {
  print("Performing post-hoc pairwise comparisons for analyst types...")
  
  analyst_levels <- levels(df$analyst_type_category[!is.na(df$analyst_type_category)])
  analyst_levels <- analyst_levels[analyst_levels %in% analyst_breakdown$analyst_type_category]
  
  if(length(analyst_levels) > 1) {
    n_comparisons <- choose(length(analyst_levels), 2)
    alpha_corrected <- 0.05 / n_comparisons
    
    print(paste("Bonferroni corrected alpha:", round(alpha_corrected, 4)))
    
    for(i in 1:(length(analyst_levels)-1)) {
      for(j in (i+1):length(analyst_levels)) {
        subset_data <- df %>%
          filter(analyst_type_category %in% c(analyst_levels[i], analyst_levels[j]),
                 !is.na(analyst_type_category))
        
        if(nrow(subset_data) > 0) {
          subset_table <- table(subset_data$analyst_type_category, 
                               subset_data$monte_carlo_simulation_usage)
          
          if(all(dim(subset_table) == c(2, 2))) {
            fisher_result <- fisher.test(subset_table)
            
            prop1 <- mean(subset_data$monte_carlo_simulation_usage[subset_data$analyst_type_category == analyst_levels[i]])
            prop2 <- mean(subset_data$monte_carlo_simulation_usage[subset_data$analyst_type_category == analyst_levels[j]])
            
            print(paste(analyst_levels[i], "vs", analyst_levels[j], ":"))
            print(paste("  ", analyst_levels[i], ": ", round(prop1*100, 1), "%", sep=""))
            print(paste("  ", analyst_levels[j], ": ", round(prop2*100, 1), "%", sep=""))
            print(paste("  p =", format(fisher_result$p.value, scientific = TRUE),
                        ifelse(fisher_result$p.value < alpha_corrected, " *", "")))
          }
        }
      }
    }
  }
}

# Identify highest and lowest usage groups
if(!all(is.na(df$analyst_type_category)) && nrow(analyst_breakdown_ci) > 0) {
  highest_usage <- analyst_breakdown_ci %>% slice_max(proportion, n = 1)
  lowest_usage <- analyst_breakdown_ci %>% slice_min(proportion, n = 1)
  
  print(paste("Highest Monte Carlo usage:", highest_usage$analyst_type_category,
              "(", highest_usage$proportion, "%, ", highest_usage$ci_text, ")"))
  print(paste("Lowest Monte Carlo usage:", lowest_usage$analyst_type_category,
              "(", lowest_usage$proportion, "%, ", lowest_usage$ci_text, ")"))
}

# Range analysis
if(!all(is.na(df$analyst_type_category)) && nrow(analyst_breakdown) > 1) {
  usage_range <- analyst_breakdown %>%
    summarise(
      min_usage = min(proportion),
      max_usage = max(proportion),
      range = max_usage - min_usage
    )
  
  print(paste("Usage range across analyst types:", usage_range$min_usage, "% to", 
              usage_range$max_usage, "% (range =", round(usage_range$range, 1), "percentage points)"))
}

# Expected: 42% of respondents (95% CI [38.7%, 45.3%]) with higher adoption among quantitative analysts