# Main Analysis Script - Supports Both Basic and Classified Data
# Author: Richard McKinney
# Date: 2025-08-06

library(tidyverse)
library(here)

# ============================================================================
# DATA LOADING - INTELLIGENT SELECTION
# ============================================================================

cat("=== LOADING SURVEY DATA ===\n")

# Check which data version to use
if(file.exists(here("data_processed", "survey_responses_anonymized_classified.csv"))) {
  cat("Loading fully classified data (with Appendix J classifications)...\n")
  survey_data <- read_csv(here("data_processed", "survey_responses_anonymized_classified.csv"))
  data_version <- "CLASSIFIED"
} else if(file.exists(here("data_processed", "survey_responses_anonymized_preliminary.csv"))) {
  cat("Loading preliminary classified data...\n")
  survey_data <- read_csv(here("data_processed", "survey_responses_anonymized_preliminary.csv"))
  data_version <- "PRELIMINARY"
} else {
  cat("Loading basic anonymized data...\n")
  survey_data <- read_csv(here("data", "survey_responses_anonymized_basic.csv"))
  data_version <- "BASIC"
}

cat("Data version:", data_version, "\n")
cat("Loaded", nrow(survey_data), "responses\n\n")

# ============================================================================
# STAKEHOLDER DISTRIBUTION ANALYSIS
# ============================================================================

if("stakeholder_category" %in% names(survey_data)) {
  # Use Appendix J classifications if available
  cat("=== STAKEHOLDER DISTRIBUTION (Appendix J Classification) ===\n")

  stakeholder_distribution <- survey_data %>%
    filter(!is.na(stakeholder_category)) %>%
    count(stakeholder_category, name = "n_respondents") %>%
    mutate(
      percentage = round(n_respondents / sum(n_respondents) * 100, 1)
    ) %>%
    arrange(desc(n_respondents))

} else {
  # Fallback to basic Q2.1 analysis
  cat("=== STAKEHOLDER DISTRIBUTION (Basic Classification) ===\n")

  stakeholder_distribution <- survey_data %>%
    filter(!is.na(Q2.1) &
             Q2.1 != "{\"ImportId\":\"QID174\"}" &
             Q2.1 != "Which of the following best describes your role? (Please select the most appropriate option) - Selected Choice") %>%
    mutate(
      stakeholder_category = case_when(
        grepl("Venture Capital", Q2.1) ~ "Venture Capital Firm",
        Q2.1 == "Entrepreneur in Climate Technology" ~ "Entrepreneur in Climate Technology",
        Q2.1 == "Government Funding Agency" ~ "Government Funding Agency",
        # ... other mappings
        Q2.1 == "Other (please specify)" ~ "Other/Unclassified",
        TRUE ~ "Other"
      )
    ) %>%
    count(stakeholder_category, name = "n_respondents") %>%
    mutate(
      percentage = round(n_respondents / sum(n_respondents) * 100, 1)
    ) %>%
    arrange(desc(n_respondents))
}

print(stakeholder_distribution)
cat("\nTotal categorized:", sum(stakeholder_distribution$n_respondents), "\n")

# ============================================================================
# KEY MANUSCRIPT GROUPS (Section 3.5.1)
# ============================================================================

# Define manuscript's key groups
manuscript_key_groups <- c(
  "Venture Capital Firm",
  "Government Funding Agency",
  "Entrepreneur in Climate Technology",
  "Philanthropic Organization",
  "Limited Partner",
  "High Net-Worth Individual",
  "Family Office",
  "ESG Investor"
)

manuscript_distribution <- stakeholder_distribution %>%
  filter(stakeholder_category %in% manuscript_key_groups)

cat("\n=== KEY MANUSCRIPT STAKEHOLDER GROUPS ===\n")
print(manuscript_distribution)
cat("Manuscript groups total:", sum(manuscript_distribution$n_respondents), "\n")

# ============================================================================
# SAVE RESULTS
# ============================================================================

results <- list(
  data_version = data_version,
  stakeholder_distribution = stakeholder_distribution,
  manuscript_distribution = manuscript_distribution,
  survey_data = survey_data,
  summary = list(
    total_responses = nrow(survey_data),
    total_categorized = sum(stakeholder_distribution$n_respondents),
    classification_method = ifelse(
      data_version == "CLASSIFIED",
      "Appendix J Manual Classification",
      "Preliminary Automated Classification"
    )
  )
)

saveRDS(results, here("output", "analysis_results.rds"))
cat("\nResults saved to output/analysis_results.rds\n")
