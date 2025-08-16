# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 53: "Reporting standard preferences differed significantly across stakeholders (χ²(9) = 28.3, p < .001, Cramér's V = .147)"
# Purpose: Chi-square test of independence examining reporting standard preferences across stakeholder groups

library(tidyverse)
library(janitor)
library(vcd)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_REPORTING_STANDARD <- "preferred_reporting_standard"
COL_STAKEHOLDER <- "stakeholder"
COL_ORGANIZATION_SIZE <- "organization_size_category"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    preferred_reporting_standard = factor(preferred_reporting_standard),
    stakeholder = factor(stakeholder),
    organization_size_category = factor(organization_size_category)
  ) %>%
  filter(!is.na(preferred_reporting_standard), !is.na(stakeholder))

# Sample sizes
total_n <- nrow(df)
print(paste("Total sample size:", total_n))

# Stakeholder distribution
stakeholder_counts <- df %>%
  count(stakeholder, sort = TRUE)
print("Sample sizes by stakeholder:")
print(stakeholder_counts)

# Reporting standard distribution
standard_counts <- df %>%
  count(preferred_reporting_standard, sort = TRUE)
print("Sample sizes by reporting standard:")
print(standard_counts)

# Create contingency table
contingency_table <- table(df$stakeholder, df$preferred_reporting_standard)
print("Contingency table: Stakeholder × Reporting Standard")
print(contingency_table)

# Row percentages (within stakeholder)
row_percentages <- round(prop.table(contingency_table, 1) * 100, 1)
print("Row percentages (within stakeholder):")
print(row_percentages)

# Column percentages (within reporting standard)
col_percentages <- round(prop.table(contingency_table, 2) * 100, 1)
print("Column percentages (within reporting standard):")
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

# Most preferred standard by stakeholder
mode_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  count(preferred_reporting_standard) %>%
  slice_max(n, with_ties = FALSE) %>%
  select(stakeholder, most_preferred = preferred_reporting_standard, count = n)

print("Most preferred reporting standard by stakeholder:")
print(mode_by_stakeholder)

# Overall preferences
overall_preferences <- df %>%
  count(preferred_reporting_standard, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("Overall reporting standard preferences:")
print(overall_preferences)

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
        subset_table <- table(subset_data$stakeholder, subset_data$preferred_reporting_standard)
        
        # Remove columns with all zeros
        subset_table <- subset_table[, colSums(subset_table) > 0]
        
        if(ncol(subset_table) > 1 && nrow(subset_table) > 1) {
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
  }
  
  if(nrow(pairwise_results) > 0) {
    print("Pairwise comparisons (Bonferroni corrected):")
    print(pairwise_results)
  }
}

# Alternative: Fisher's exact test for small samples
if(any(expected_frequencies < 5)) {
  print("Performing Fisher's exact test due to low expected frequencies...")
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
  print(paste("Fisher's exact test p-value:", format(fisher_test$p.value, scientific = TRUE)))
}

# Analysis by organization size (if available)
if(!all(is.na(df$organization_size_category))) {
  size_standard_analysis <- df %>%
    filter(!is.na(organization_size_category)) %>%
    group_by(organization_size_category, preferred_reporting_standard) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(organization_size_category) %>%
    mutate(percentage = round(n / sum(n) * 100, 1)) %>%
    arrange(organization_size_category, desc(percentage))
  
  print("Reporting standard preferences by organization size:")
  print(size_standard_analysis)
  
  # Chi-square test by organization size
  size_table <- table(df$organization_size_category[!is.na(df$organization_size_category)], 
                     df$preferred_reporting_standard[!is.na(df$organization_size_category)])
  
  if(min(dim(size_table)) > 1) {
    chi_size <- chisq.test(size_table)
    print("Chi-square test: Reporting standards by organization size:")
    print(paste("χ² =", round(chi_size$statistic, 2), 
                ", p =", format(chi_size$p.value, scientific = TRUE)))
  }
}

# Identify dominant preferences
# Find standards preferred by >50% in any stakeholder group
dominant_prefs <- row_percentages %>%
  as.data.frame() %>%
  rownames_to_column("stakeholder") %>%
  pivot_longer(-stakeholder, names_to = "standard", values_to = "percentage") %>%
  filter(percentage > 50) %>%
  arrange(desc(percentage))

if(nrow(dominant_prefs) > 0) {
  print("Dominant preferences (>50% within stakeholder):")
  print(dominant_prefs)
}

# Three-way analysis: Stakeholder × Standard × Size (if sufficient data)
if(!all(is.na(df$organization_size_category))) {
  threeway_table <- table(df$stakeholder, df$preferred_reporting_standard, 
                         df$organization_size_category)
  
  # Log-linear model for three-way interaction
  if(sum(threeway_table > 0) / length(threeway_table) > 0.7) {  # At least 70% non-zero cells
    loglin_data <- expand.grid(
      stakeholder = levels(df$stakeholder),
      standard = levels(df$preferred_reporting_standard),
      size = levels(df$organization_size_category)
    )
    loglin_data$count <- as.vector(threeway_table)
    
    # Test for three-way interaction
    model_saturated <- glm(count ~ stakeholder * standard * organization_size_category, 
                          data = loglin_data, family = poisson)
    model_no_threeway <- glm(count ~ stakeholder * standard + stakeholder * organization_size_category + 
                            standard * organization_size_category, 
                            data = loglin_data, family = poisson)
    
    threeway_test <- anova(model_no_threeway, model_saturated, test = "Chisq")
    print("Three-way interaction test (Stakeholder × Standard × Size):")
    print(threeway_test)
  }
}

# Association strength comparison
phi_coefficient <- sqrt(chi_statistic / n_total)
print(paste("Phi coefficient =", round(phi_coefficient, 3)))

contingency_coefficient <- sqrt(chi_statistic / (chi_statistic + n_total))
print(paste("Contingency coefficient =", round(contingency_coefficient, 3)))

# Preference diversity index (entropy) by stakeholder
preference_diversity <- df %>%
  group_by(stakeholder) %>%
  count(preferred_reporting_standard) %>%
  mutate(prop = n / sum(n)) %>%
  summarise(
    diversity_index = -sum(prop * log(prop)),
    .groups = "drop"
  ) %>%
  arrange(desc(diversity_index))

print("Preference diversity (entropy) by stakeholder:")
print(preference_diversity)

# Expected: χ²(9) = 28.3, p < .001, Cramér's V = .147