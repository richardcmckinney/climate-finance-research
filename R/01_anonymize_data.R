# Enhanced Data Anonymization Script for Climate Finance Survey
# Author: Richard McKinney
# Date: 2025-08-06
# Purpose: Create anonymized versions of survey data with and without classifications
# Ensure S4 methods are available when running via Rscript
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))  # if you use dplyr/readr/etc.
library(tidyverse)
library(digest)
library(here)

# ============================================================================
# PART 1: BASIC ANONYMIZATION OF RAW DATA
# ============================================================================

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")

# Load raw data
raw_data <- read_csv(
  here("data_raw", "(Strategic Research) Accelerating climate finance_ An analysis of barriers and solutions_August 21, 2024_18.54.csv"),
  col_types = cols(.default = "c")  # Read all as character to avoid type issues
)

cat("Loaded", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")

# Function to create deterministic hash IDs
create_hash_id <- function(x, prefix = "ID") {
  sapply(x, function(i) {
    if(is.na(i) || i == "") return(NA)
    paste0(prefix, "_", substr(digest(as.character(i), algo = "sha256"), 1, 8))
  })
}

# List of PII columns to remove
pii_columns <- c(
  "IPAddress", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
  "ExternalReference", "LocationLatitude", "LocationLongitude", "FULL_NAME",
  "GENDER", "ORGANIZATION_NAME", "hq_address_line_1", "hq_address_line_2",
  "hq_email", "hq_phone", "primary_contact_email", "primary_contact_phone",
  "website", "lp_name", "PRIMARY_POSITION", "description"
)

# Create anonymized version
cat("Anonymizing data (removing PII)...\n")
anonymized_basic <- raw_data %>%
  # Create anonymized IDs
  mutate(
    respondent_id = create_hash_id(ResponseId, "RESP"),
    organization_id = if("ORGANIZATION_ID" %in% names(.))
      create_hash_id(ORGANIZATION_ID, "ORG") else NA,
    firm_id = if("FIRM_ORGANIZATION_ID" %in% names(.))
      create_hash_id(FIRM_ORGANIZATION_ID, "FIRM") else NA
  ) %>%
  # Remove PII columns
  select(-any_of(c(pii_columns, "ResponseId", "ORGANIZATION_ID",
                   "FIRM_ORGANIZATION_ID", "FIRM_ORGANIZATION_ID_NUMBER", "pbid"))) %>%
  # Generalize geographic information
  mutate(
    hq_country_region = case_when(
      hq_country %in% c("United States", "Canada", "USA", "CA") ~ "North America",
      hq_country %in% c("United Kingdom", "Germany", "France", "Netherlands",
                        "Switzerland", "Sweden", "Denmark", "Norway", "Spain",
                        "Italy", "Belgium", "Austria", "Finland", "UK", "DE", "FR") ~ "Europe",
      hq_country %in% c("China", "Japan", "Singapore", "India", "South Korea",
                        "Hong Kong", "Taiwan", "CN", "JP", "SG", "IN", "KR") ~ "Asia",
      !is.na(hq_country) ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-any_of(c("hq_state_province", "hq_city", "hq_zip_code", "hq_country"))) %>%
  rename(hq_country = hq_country_region) %>%
  # Generalize dates
  mutate(
    across(c(StartDate, EndDate, RecordedDate),
           ~if_else(!is.na(.), format(as.Date(., format = "%m/%d/%Y %H:%M", tz = "UTC"), "%Y-%m"), NA_character_),
           .names = "{.col}_month")
  ) %>%
  select(-c(StartDate, EndDate, RecordedDate))

# Save basic anonymized data
cat("Saving basic anonymized data...\n")
write_csv(anonymized_basic, here("data", "survey_responses_anonymized_basic.csv"))

# ============================================================================
# PART 2: PREPARE FOR APPENDIX J CLASSIFICATIONS
# ============================================================================

cat("\n=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

# Extract responses that need classification
responses_to_classify <- anonymized_basic %>%
  select(respondent_id, Q2.1, Q2.1_12_TEXT) %>%
  filter(!is.na(Q2.1)) %>%
  filter(Q2.1 != "{\"ImportId\":\"QID174\"}" &
           Q2.1 != "Which of the following best describes your role? (Please select the most appropriate option) - Selected Choice")

# Create classification template
cat("Creating classification template for manual coding...\n")
classification_template <- responses_to_classify %>%
  mutate(
    original_response = Q2.1,
    other_text = Q2.1_12_TEXT,
    needs_harmonization = Q2.1 == "Other (please specify)",
    preliminary_category = case_when(
      grepl("Venture Capital Firm", Q2.1) ~ "Venture Capital Firm",
      Q2.1 == "Entrepreneur in Climate Technology" ~ "Entrepreneur in Climate Technology",
      Q2.1 == "Government Funding Agency" ~ "Government Funding Agency",
      Q2.1 == "Philanthropic Organization" ~ "Philanthropic Organization",
      Q2.1 == "Limited Partner" ~ "Limited Partner",
      Q2.1 == "High Net-Worth Individual" ~ "High Net-Worth Individual",
      grepl("Family Office", Q2.1) ~ "Family Office",
      grepl("ESG Investor", Q2.1) ~ "ESG Investor",
      Q2.1 == "Private Equity Firm" ~ "Private Equity Firm",
      Q2.1 == "Corporate Venture Arm" ~ "Corporate Venture Arm",
      Q2.1 == "Angel Investor" ~ "Angel Investor",
      Q2.1 == "Nonprofit Organization" ~ "Nonprofit Organization",
      Q2.1 == "Academic or Research Institution" ~ "Academic or Research Institution",
      Q2.1 == "Other (please specify)" ~ "NEEDS_CLASSIFICATION",
      TRUE ~ "Other"
    ),
    final_category_appendix_j = NA_character_  # To be filled in manually
  )

# Save classification template
write_csv(classification_template, here("docs", "appendix_j_classification_template.csv"))
cat("Classification template saved to docs/appendix_j_classification_template.csv\n")
cat("This file needs manual completion based on Appendix J methodology\n")

# ============================================================================
# PART 3: CREATE PLACEHOLDER FOR CLASSIFIED DATA
# ============================================================================

cat("\n=== PART 3: PLACEHOLDER FOR CLASSIFIED DATA ===\n")

# Check if classifications exist
classification_file <- here("docs", "appendix_j_classifications_complete.csv")

if(file.exists(classification_file)) {
  cat("Loading completed classifications...\n")
  classifications <- read_csv(classification_file)

  # Merge classifications with anonymized data
  anonymized_classified <- anonymized_basic %>%
    left_join(
      classifications %>% select(respondent_id, final_category_appendix_j),
      by = "respondent_id"
    ) %>%
    mutate(
      stakeholder_category = coalesce(final_category_appendix_j, preliminary_category)
    )

  # Save classified version
  write_csv(anonymized_classified, here("data_processed", "survey_responses_anonymized_classified.csv"))
  cat("Classified data saved to data_processed/survey_responses_anonymized_classified.csv\n")

} else {
  cat("Classification file not found. Creating placeholder with preliminary classifications only...\n")

  # Create preliminary classified version
  anonymized_classified <- anonymized_basic %>%
    left_join(
      classification_template %>% select(respondent_id, preliminary_category),
      by = "respondent_id"
    ) %>%
    mutate(stakeholder_category = preliminary_category)

  write_csv(anonymized_classified, here("data_processed", "survey_responses_anonymized_preliminary.csv"))
  cat("Preliminary classified data saved to data_processed/survey_responses_anonymized_preliminary.csv\n")
}

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== ANONYMIZATION SUMMARY ===\n")
cat("Original columns:", ncol(raw_data), "\n")
cat("Anonymized columns:", ncol(anonymized_basic), "\n")
cat("Columns removed:", ncol(raw_data) - ncol(anonymized_basic), "\n")
cat("Rows preserved:", nrow(anonymized_basic), "\n")

# Create data dictionary
data_dict <- tibble(
  variable_name = names(anonymized_basic),
  type = map_chr(anonymized_basic, ~class(.)[1]),
  n_missing = map_int(anonymized_basic, ~sum(is.na(.))),
  n_non_missing = nrow(anonymized_basic) - n_missing,
  completeness_pct = round(n_non_missing / nrow(anonymized_basic) * 100, 1)
)

write_csv(data_dict, here("data", "data_dictionary.csv"))
cat("Data dictionary saved to data/data_dictionary.csv\n")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next steps:\n")
cat("1. Review classification template in docs/appendix_j_classification_template.csv\n")
cat("2. Complete manual classifications following Appendix J methodology\n")
cat("3. Save as docs/appendix_j_classifications_complete.csv\n")
cat("4. Re-run this script to generate fully classified dataset\n")
