# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 63: "Sensitivity analysis practices were more prevalent among larger organizations (χ²(3) = 18.9, p < .001, φ = .121)"
# Purpose: Chi-square test examining the relationship between sensitivity analysis practices and organization size

library(tidyverse)
library(janitor)
library(vcd)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SENSITIVITY_ANALYSIS <- "sensitivity_analysis_practices_used"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_STAKEHOLDER <- "stakeholder"
COL_FINANCIAL_SOPHISTICATION <- "financial_analysis_sophistication"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    sensitivity_analysis_practices_used = as.integer(sensitivity_analysis_practices_used),
    organization_size_category = factor(organization_size_category, 
                                      levels = c("Small", "Medium", "Large", "Very Large")),
    stakeholder = factor(stakeholder),
    financial_analysis_sophistication = as.numeric(financial_analysis_sophistication)
  ) %>%
  filter(!is.na(sensitivity_analysis_practices_used), !is.na(organization_size_category))

# Sample sizes
total_n <- nrow(df)
print(paste("Total sample size:", total_n))

# Organization size distribution
size_counts <- df %>%
  count(organization_size_category, sort = TRUE)
print("Sample sizes by organization size:")
print(size_counts)

# Sensitivity analysis usage distribution
sensitivity_counts <- df %>%
  count(sensitivity_analysis_practices_used, sort = TRUE)
print("Sensitivity analysis usage distribution:")
print(c("Not Used" = sensitivity_counts$n[sensitivity_counts$sensitivity_analysis_practices_used == 0],
        "Used" = sensitivity_counts$n[sensitivity_counts$sensitivity_analysis_practices_used == 1]))

# Create contingency table
contingency_table <- table(df$organization_size_category, df$sensitivity_analysis_practices_used)
colnames(contingency_table) <- c("Not Used", "Used")
print("Contingency table: Organization Size × Sensitivity Analysis Usage")
print(contingency_table)

# Row percentages (within organization size)
row_percentages <- round(prop.table(contingency_table, 1) * 100, 1)
print("Row percentages (within organization size):")
print(row_percentages)

# Column percentages (within sensitivity analysis usage)
col_percentages <- round(prop.table(contingency_table, 2) * 100, 1)
print("Column percentages (within sensitivity analysis usage):")
print(col_percentages)

# Chi-square test of independence
chi_test <- chisq.test(contingency_table)
print("Chi-square test of independence:")
print(chi_test)

# Extract key statistics
chi_statistic <- chi_test$statistic
df_chi <- chi_test$parameter
p_value <- chi_test$p.value

print("Chi-square test summary:")
print(paste("χ²(", df_chi, ") =", round(chi_statistic, 2)))
print(paste("p-value:", format(p_value, scientific = TRUE)))

# Calculate phi coefficient (effect size for 2x4 table, use Cramér's V)
n_total <- sum(contingency_table)
cramers_v <- sqrt(chi_statistic / (n_total * (min(dim(contingency_table)) - 1)))
phi_coefficient <- sqrt(chi_statistic / n_total)

print(paste("Phi coefficient =", round(phi_coefficient, 3)))
print(paste("Cramér's V =", round(cramers_v, 3)))

# Interpret effect size
phi_interpretation <- case_when(
  phi_coefficient < 0.1 ~ "Negligible",
  phi_coefficient < 0.3 ~ "Small",
  phi_coefficient < 0.5 ~ "Medium",
  TRUE ~ "Large"
)
print(paste("Phi coefficient interpretation:", phi_interpretation))

# Check assumptions for chi-square test
expected_frequencies <- chi_test$expected
print("Expected frequencies:")
print(round(expected_frequencies, 1))

# Check for cells with expected frequency < 5
low_freq_cells <- sum(expected_frequencies < 5)
total_cells <- length(expected_frequencies)
percent_low_freq <- round(low_freq_cells / total_cells * 100, 1)

print(paste("Cells with expected frequency < 5:", low_freq_cells, "out of", total_cells, 
            "(", percent_low_freq, "%)"))

# Standardized residuals
std_residuals <- (contingency_table - expected_frequencies) / sqrt(expected_frequencies)
print("Standardized residuals (|z| > 2 indicates significant contribution):")
print(round(std_residuals, 2))

# Highlight significant cells
significant_cells <- abs(std_residuals) > 2
if(any(significant_cells)) {
  print("Cells contributing significantly to chi-square:")
  sig_positions <- which(significant_cells, arr.ind = TRUE)
  for(i in 1:nrow(sig_positions)) {
    row_idx <- sig_positions[i, 1]
    col_idx <- sig_positions[i, 2]
    print(paste(rownames(contingency_table)[row_idx], "×", 
                colnames(contingency_table)[col_idx], ":",
                "observed =", contingency_table[row_idx, col_idx],
                ", expected =", round(expected_frequencies[row_idx, col_idx], 1),
                ", std.resid =", round(std_residuals[row_idx, col_idx], 2)))
  }
}

# Trend analysis (treating organization size as ordinal)
df_trend <- df %>%
  mutate(size_numeric = as.numeric(organization_size_category))

# Linear trend test
trend_test <- cor.test(df_trend$size_numeric, df_trend$sensitivity_analysis_practices_used, 
                      method = "spearman")

print("Trend analysis (Spearman correlation with ordinal size):")
print(paste("ρ =", round(trend_test$estimate, 3)))
print(paste("p-value:", format(trend_test$p.value, scientific = TRUE)))

# Cochran-Armitage trend test
size_levels <- levels(df$organization_size_category)
prop_by_size <- df %>%
  group_by(organization_size_category) %>%
  summarise(
    n = n(),
    usage_count = sum(sensitivity_analysis_practices_used),
    usage_prop = round(mean(sensitivity_analysis_practices_used) * 100, 1),
    .groups = "drop"
  )

print("Sensitivity analysis usage by organization size:")
print(prop_by_size)

# Pairwise comparisons with Bonferroni correction
if(p_value < 0.05) {
  print("Performing post-hoc pairwise comparisons...")
  
  size_levels <- levels(df$organization_size_category)
  n_comparisons <- choose(length(size_levels), 2)
  alpha_corrected <- 0.05 / n_comparisons
  
  print(paste("Bonferroni corrected alpha:", round(alpha_corrected, 4)))
  
  pairwise_results <- tibble()
  for(i in 1:(length(size_levels)-1)) {
    for(j in (i+1):length(size_levels)) {
      subset_data <- df %>%
        filter(organization_size_category %in% c(size_levels[i], size_levels[j]))
      
      if(nrow(subset_data) > 0) {
        subset_table <- table(subset_data$organization_size_category, 
                             subset_data$sensitivity_analysis_practices_used)
        
        # Use Fisher's exact test for 2x2 tables
        fisher_result <- fisher.test(subset_table)
        
        pairwise_results <- bind_rows(pairwise_results, tibble(
          comparison = paste(size_levels[i], "vs", size_levels[j]),
          p_value = fisher_result$p.value,
          odds_ratio = round(fisher_result$estimate, 2),
          significant = fisher_result$p.value < alpha_corrected
        ))
      }
    }
  }
  
  print("Pairwise comparisons (Bonferroni corrected):")
  print(pairwise_results)
}

# Analysis by stakeholder type
stakeholder_sensitivity <- df %>%
  group_by(stakeholder, organization_size_category) %>%
  summarise(
    n = n(),
    usage_count = sum(sensitivity_analysis_practices_used),
    usage_prop = round(mean(sensitivity_analysis_practices_used) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(stakeholder, organization_size_category)

if(nrow(stakeholder_sensitivity) > 0) {
  print("Sensitivity analysis usage by stakeholder and size (n ≥ 10):")
  print(stakeholder_sensitivity)
}

# Three-way analysis: Size × Stakeholder × Sensitivity
threeway_table <- table(df$organization_size_category, df$stakeholder, 
                       df$sensitivity_analysis_practices_used)

# Log-linear model for three-way interaction
threeway_data <- as.data.frame(threeway_table)
names(threeway_data) <- c("Size", "Stakeholder", "Sensitivity", "Freq")

if(sum(threeway_data$Freq > 0) / nrow(threeway_data) > 0.5) {  # At least 50% non-zero cells
  # Test for independence
  model_independence <- glm(Freq ~ Size + Stakeholder + Sensitivity, 
                          data = threeway_data, family = poisson)
  model_saturated <- glm(Freq ~ Size * Stakeholder * Sensitivity, 
                        data = threeway_data, family = poisson)
  
  independence_test <- anova(model_independence, model_saturated, test = "Chisq")
  print("Three-way independence test:")
  print(independence_test)
}

# Odds ratios for adjacent size categories
odds_ratios <- tibble()
for(i in 1:(length(size_levels)-1)) {
  size1 <- size_levels[i]
  size2 <- size_levels[i+1]
  
  data_pair <- df %>%
    filter(organization_size_category %in% c(size1, size2))
  
  if(nrow(data_pair) > 0) {
    table_pair <- table(data_pair$organization_size_category, 
                       data_pair$sensitivity_analysis_practices_used)
    
    if(all(dim(table_pair) == c(2, 2))) {
      fisher_result <- fisher.test(table_pair)
      
      odds_ratios <- bind_rows(odds_ratios, tibble(
        comparison = paste(size2, "vs", size1),
        odds_ratio = round(fisher_result$estimate, 2),
        ci_lower = round(fisher_result$conf.int[1], 2),
        ci_upper = round(fisher_result$conf.int[2], 2),
        p_value = round(fisher_result$p.value, 4)
      ))
    }
  }
}

if(nrow(odds_ratios) > 0) {
  print("Odds ratios for adjacent size categories:")
  print(odds_ratios)
}

# Financial sophistication correlation
if(sum(!is.na(df$financial_analysis_sophistication)) > 50) {
  df_financial <- df %>%
    filter(!is.na(financial_analysis_sophistication))
  
  # Point-biserial correlation
  cor_financial <- cor.test(df_financial$sensitivity_analysis_practices_used, 
                           df_financial$financial_analysis_sophistication)
  
  print("Sensitivity analysis usage × Financial sophistication correlation:")
  print(paste("r =", round(cor_financial$estimate, 3)))
  print(paste("p-value:", format(cor_financial$p.value, scientific = TRUE)))
  
  # Financial sophistication by organization size
  financial_by_size <- df_financial %>%
    group_by(organization_size_category) %>%
    summarise(
      n = n(),
      mean_financial = round(mean(financial_analysis_sophistication), 2),
      usage_prop = round(mean(sensitivity_analysis_practices_used) * 100, 1),
      .groups = "drop"
    )
  
  print("Financial sophistication and sensitivity analysis usage by size:")
  print(financial_by_size)
}

# Small vs Large organization contrast
small_large_data <- df %>%
  filter(organization_size_category %in% c("Small", "Large"))

if(nrow(small_large_data) > 0) {
  small_large_table <- table(small_large_data$organization_size_category, 
                            small_large_data$sensitivity_analysis_practices_used)
  
  if(all(dim(small_large_table) == c(2, 2))) {
    fisher_small_large <- fisher.test(small_large_table)
    
    print("Small vs Large organization comparison:")
    print(small_large_table)
    print(paste("Odds ratio:", round(fisher_small_large$estimate, 2)))
    print(paste("95% CI: [", round(fisher_small_large$conf.int[1], 2), ", ",
                round(fisher_small_large$conf.int[2], 2), "]", sep=""))
    print(paste("p-value:", format(fisher_small_large$p.value, scientific = TRUE)))
  }
}

# Usage rates summary
print("Summary of sensitivity analysis usage rates:")
usage_summary <- prop_by_size %>%
  summarise(
    min_usage = min(usage_prop),
    max_usage = max(usage_prop),
    mean_usage = round(mean(usage_prop), 1),
    range = max_usage - min_usage
  )
print(usage_summary)

# Expected: χ²(3) = 18.9, p < .001, φ = .121