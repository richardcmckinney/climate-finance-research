# Main Analysis Script for Climate Finance Survey
# Author: Richard McKinney
# Date: 2025-08-06
# Purpose: Reproduce key findings from manuscript

library(tidyverse)
library(here)

# Load anonymized data
cat("Loading anonymized survey data...\n")
survey_data <- read_csv(here("data", "climate_finance_survey_anonymized.csv"))

cat("Loaded", nrow(survey_data), "responses\n\n")

# ============================================================================
# SAMPLE CHARACTERISTICS (Section 3.5.1 of manuscript)
# ============================================================================

# Define stakeholder groups based on Q2.1
stakeholder_mapping <- c(
  "1" = "Venture Capitalist",
  "2" = "Government Funding Agency",
  "3" = "Entrepreneur",
  "4" = "Philanthropic Organization",
  "5" = "Limited Partner",
  "6" = "High Net-Worth Individual",
  "7" = "Family Office",
  "8" = "ESG Investor",
  "12" = "Other"
)

# Count respondents by stakeholder type
stakeholder_distribution <- survey_data %>%
  filter(!is.na(Q2.1)) %>%
  mutate(
    stakeholder_type = stakeholder_mapping[as.character(Q2.1)]
  ) %>%
  filter(!is.na(stakeholder_type)) %>%
  count(stakeholder_type, name = "n_respondents") %>%
  mutate(
    percentage = round(n_respondents / sum(n_respondents) * 100, 1)
  ) %>%
  arrange(desc(n_respondents))

cat("=== STAKEHOLDER DISTRIBUTION ===\n")
print(stakeholder_distribution)
cat("\nTotal categorized respondents:", sum(stakeholder_distribution$n_respondents), "\n")

# Geographic distribution (Q2.2)
geographic_distribution <- survey_data %>%
  filter(!is.na(Q2.2)) %>%
  count(Q2.2) %>%
  mutate(
    region = case_when(
      Q2.2 == "1" ~ "North America",
      Q2.2 == "2" ~ "Europe",
      Q2.2 == "3" ~ "Asia",
      Q2.2 == "4" ~ "Other",
      TRUE ~ "Unspecified"
    )
  ) %>%
  group_by(region) %>%
  summarise(n = sum(n)) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

cat("\n=== GEOGRAPHIC DISTRIBUTION ===\n")
print(geographic_distribution)

# ============================================================================
# KEY FINDINGS - Capital-Opportunity Mismatch
# ============================================================================

# Analyze barriers by stakeholder type (Q3.11 - multiple choice)
# Note: These are placeholder column names - adjust based on actual survey structure

cat("\n=== BARRIER ANALYSIS ===\n")

# Count how many respondents completed barrier questions
barrier_cols <- names(survey_data)[grepl("Q3.11", names(survey_data))]
if(length(barrier_cols) > 0) {
  barrier_completion <- survey_data %>%
    select(all_of(barrier_cols)) %>%
    mutate(any_barrier_selected = rowSums(!is.na(.)) > 0) %>%
    summarise(
      n_with_barriers = sum(any_barrier_selected, na.rm = TRUE),
      pct_with_barriers = round(mean(any_barrier_selected, na.rm = TRUE) * 100, 1)
    )

  cat("Respondents who selected at least one barrier:",
      barrier_completion$n_with_barriers,
      "(", barrier_completion$pct_with_barriers, "%)\n")
}

# Save results
cat("\n=== SAVING RESULTS ===\n")
results <- list(
  stakeholder_distribution = stakeholder_distribution,
  geographic_distribution = geographic_distribution,
  survey_data = survey_data
)

saveRDS(results, here("output", "analysis_results.rds"))
cat("Results saved to output/analysis_results.rds\n")

# Generate summary report
cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Total responses analyzed:", nrow(survey_data), "\n")
cat("Date range:", min(survey_data$StartDate_month, na.rm = TRUE),
    "to", max(survey_data$EndDate_month, na.rm = TRUE), "\n")
