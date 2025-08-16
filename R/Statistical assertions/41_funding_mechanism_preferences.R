# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 41: "Funding mechanism preferences differed significantly by stakeholder (χ²(12) = 34.7, p < .001, Cramér's V = .162)"
# Purpose: Chi-square test of independence examining funding mechanism preferences across stakeholder groups

library(tidyverse)
library(janitor)
library(vcd)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_FUNDING_MECHANISM <- "preferred_funding_mechanism"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    preferred_funding_mechanism = factor(preferred_funding_mechanism),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(preferred_funding_mechanism), !is.na(stakeholder))

# Sample sizes
total_n <- nrow(df)
print(paste("Total sample size:", total_n))

# Stakeholder distribution
stakeholder_counts <- df %>%
  count(stakeholder, sort = TRUE)
print("Sample sizes by stakeholder:")
print(stakeholder_counts)

# Funding mechanism distribution
mechanism_counts <- df %>%
  count(preferred_funding_mechanism, sort = TRUE)
print("Sample sizes by funding mechanism:")
print(mechanism_counts)

# Create contingency table
contingency_table <- table(df$stakeholder, df$preferred_funding_mechanism)
print("Contingency table: Stakeholder × Funding Mechanism")
print(contingency_table)

# Row percentages (within stakeholder)
row_percentages <- round(prop.table(contingency_table, 1) * 100, 1)
print("Row percentages (within stakeholder):")
print(row_percentages)

# Column percentages (within funding mechanism)
col_percentages <- round(prop.table(contingency_table, 2) * 100, 1)
print("Column percentages (within funding mechanism):")
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

# Calculate Cramér's V
n_total <- sum(contingency_table)
min_dimension <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
cramers_v <- sqrt(chi_statistic / (n_total * min_dimension))

print(paste("Cramér's V =", round(cramers_v, 3)))

# Interpret Cramér's V effect size
cramers_interpretation <- case_when(
  cramers_v < 0.1 ~ "Negligible",
  cramers_v < 0.3 ~ "Small",
  cramers_v < 0.5 ~ "Medium",
  TRUE ~ "Large"
)
print(paste("Cramér's V interpretation:", cramers_interpretation))

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

if(percent_low_freq > 20) {
  print("Warning: More than 20% of cells have expected frequency < 5")
  print("Consider Fisher's exact test or collapsing categories")
}

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

# Post-hoc pairwise comparisons if significant
if(p_value < 0.05) {
  print("Performing post-hoc pairwise comparisons...")
  
  # Pairwise chi-square tests with Bonferroni correction
  stakeholder_levels <- levels(df$stakeholder)
  n_comparisons <- choose(length(stakeholder_levels), 2)
  alpha_corrected <- 0.05 / n_comparisons
  
  print(paste("Bonferroni corrected alpha:", round(alpha_corrected, 4)))
  
  pairwise_results <- tibble()
  for(i in 1:(length(stakeholder_levels)-1)) {
    for(j in (i+1):length(stakeholder_levels)) {
      subset_data <- df %>%
        filter(stakeholder %in% c(stakeholder_levels[i], stakeholder_levels[j])) %>%
        mutate(stakeholder = droplevels(stakeholder))
      
      if(nrow(subset_data) > 0) {
        subset_table <- table(subset_data$stakeholder, subset_data$preferred_funding_mechanism)
        subset_chi <- chisq.test(subset_table)
        
        pairwise_results <- bind_rows(pairwise_results, tibble(
          comparison = paste(stakeholder_levels[i], "vs", stakeholder_levels[j]),
          chi_square = round(subset_chi$statistic, 2),
          df = subset_chi$parameter,
          p_value = subset_chi$p.value,
          significant = subset_chi$p.value < alpha_corrected
        ))
      }
    }
  }
  
  print("Pairwise comparisons (Bonferroni corrected):")
  print(pairwise_results)
}

# Alternative: Fisher's exact test for small samples
if(any(expected_frequencies < 5)) {
  print("Performing Fisher's exact test due to low expected frequencies...")
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
  print(paste("Fisher's exact test p-value:", format(fisher_test$p.value, scientific = TRUE)))
}

# Mosaic plot data preparation
print("Most preferred funding mechanism by stakeholder:")
mode_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  count(preferred_funding_mechanism) %>%
  slice_max(n, with_ties = FALSE) %>%
  select(stakeholder, most_preferred = preferred_funding_mechanism, count = n)

print(mode_by_stakeholder)

# Overall preferences
overall_preferences <- df %>%
  count(preferred_funding_mechanism, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("Overall funding mechanism preferences:")
print(overall_preferences)

# Association strength comparison
phi_coefficient <- sqrt(chi_statistic / n_total)
print(paste("Phi coefficient =", round(phi_coefficient, 3)))

contingency_coefficient <- sqrt(chi_statistic / (chi_statistic + n_total))
print(paste("Contingency coefficient =", round(contingency_coefficient, 3)))

# Expected: χ²(12) = 34.7, p < .001, Cramér's V = .162