# Data Anonymization Script for Climate Finance Survey
# Author: Richard McKinney
# Date: 2025-08-06
# Purpose: Remove PII and create shareable dataset

# Load required libraries
library(tidyverse)
library(digest)
library(here)

# Load raw data
cat("Loading raw data...\n")
raw_data <- read_csv(here("data_raw", "(Strategic Research) Accelerating climate finance_ An analysis of barriers and solutions_August 21, 2024_18.54.csv"))

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
  "IPAddress",
  "RecipientLastName",
  "RecipientFirstName",
  "RecipientEmail",
  "ExternalReference",
  "LocationLatitude",
  "LocationLongitude",
  "FULL_NAME",
  "GENDER",
  "ORGANIZATION_NAME",
  "hq_address_line_1",
  "hq_address_line_2",
  "hq_email",
  "hq_phone",
  "primary_contact_email",
  "primary_contact_phone",
  "website",
  "lp_name",
  "PRIMARY_POSITION",
  "description"
)

# Anonymize the data
cat("Anonymizing data...\n")
anonymized_data <- raw_data %>%
  # Create anonymized IDs before removing originals
  mutate(
    respondent_id = create_hash_id(ResponseId, "RESP"),
    organization_id = if("ORGANIZATION_ID" %in% names(.)) create_hash_id(ORGANIZATION_ID, "ORG") else NA,
    firm_id = if("FIRM_ORGANIZATION_ID" %in% names(.)) create_hash_id(FIRM_ORGANIZATION_ID, "FIRM") else NA
  ) %>%
  # Remove all PII columns that exist in the data
  select(-any_of(c(pii_columns, "ResponseId", "ORGANIZATION_ID",
                   "FIRM_ORGANIZATION_ID", "FIRM_ORGANIZATION_ID_NUMBER", "pbid"))) %>%
  # Generalize geographic information
  mutate(
    # Generalize country to regions
    hq_country_region = case_when(
      hq_country %in% c("United States", "Canada", "USA", "CA") ~ "North America",
      hq_country %in% c("United Kingdom", "Germany", "France", "Netherlands",
                        "Switzerland", "Sweden", "Denmark", "Norway", "Spain",
                        "Italy", "Belgium", "Austria", "Finland", "UK", "DE", "FR") ~ "Europe",
      hq_country %in% c("China", "Japan", "Singapore", "India", "South Korea",
                        "Hong Kong", "Taiwan", "CN", "JP", "SG", "IN", "KR") ~ "Asia",
      !is.na(hq_country) ~ "Other",
      TRUE ~ NA_character_
    ),
    # Remove specific city/state info
    hq_state_province = NULL,
    hq_city = NULL,
    hq_zip_code = NULL,
    hq_country = hq_country_region  # Replace with generalized region
  ) %>%
  select(-hq_country_region) %>%  # Clean up temp column
  # Generalize dates to preserve privacy
  mutate(
    across(c(StartDate, EndDate, RecordedDate),
           ~if_else(!is.na(.), format(as.Date(., format = "%m/%d/%Y %H:%M", tz = "UTC"), "%Y-%m"), NA_character_),
           .names = "{.col}_month")
  ) %>%
  select(-c(StartDate, EndDate, RecordedDate))  # Remove original date columns

# Summary statistics
cat("\n=== Anonymization Summary ===\n")
cat("Original columns:", ncol(raw_data), "\n")
cat("Anonymized columns:", ncol(anonymized_data), "\n")
cat("Columns removed:", ncol(raw_data) - ncol(anonymized_data), "\n")
cat("Rows preserved:", nrow(anonymized_data), "\n")
cat("Survey responses by status:\n")
print(table(anonymized_data$Status))

# Save anonymized data
cat("\nSaving anonymized data...\n")
write_csv(anonymized_data, here("data", "climate_finance_survey_anonymized.csv"))
cat("✓ Anonymized data saved to: data/climate_finance_survey_anonymized.csv\n")

# Create data dictionary
cat("\nCreating data dictionary...\n")
data_dict <- tibble(
  variable_name = names(anonymized_data),
  type = map_chr(anonymized_data, ~class(.)[1]),
  n_missing = map_int(anonymized_data, ~sum(is.na(.))),
  n_non_missing = nrow(anonymized_data) - n_missing,
  completeness_pct = round(n_non_missing / nrow(anonymized_data) * 100, 1)
) %>%
  arrange(desc(completeness_pct))

write_csv(data_dict, here("data", "data_dictionary.csv"))
cat("✓ Data dictionary saved to: data/data_dictionary.csv\n")

# Final check - ensure no PII remains
remaining_cols <- names(anonymized_data)
potential_pii <- grep("name|email|phone|address|ip|location", remaining_cols, ignore.case = TRUE, value = TRUE)
if(length(potential_pii) > 0) {
  cat("\n⚠️  WARNING: Check these columns for potential PII:\n")
  print(potential_pii)
} else {
  cat("\n✓ No obvious PII column names detected in anonymized data\n")
}

cat("\n=== Anonymization Complete ===\n")
