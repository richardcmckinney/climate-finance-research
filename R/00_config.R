# R/00_config.R - Central configuration for all paths
# This file must be sourced by all other scripts

# Define canonical paths
PATHS <- list(
  # Stage 1: Anonymization outputs
  basic_anon = "data/survey_responses_anonymized_basic.csv",
  dictionary = "data/data_dictionary.csv",
  
  # Stage 2: Classification outputs  
  classification_template = "docs/appendix_j_classification_template.csv",
  preliminary_classified = "data/survey_responses_anonymized_preliminary.csv",
  classification_audit = "output/classification_audit.csv",
  
  # Stage 3: Quota-matching outputs
  final_1307 = "data/climate_finance_survey_final_1307.csv",
  reassignment_log = "output/reassignment_log.csv",
  verification = "output/final_distribution_verification.csv",
  
  # Stage 4: Analysis outputs
  hypothesis_summary = "output/hypothesis_testing_summary.csv",
  three_factor_model = "output/tables/three_factor_summary.csv"
)

# Define deprecated paths that should NEVER be used
DEPRECATED_PATHS <- c(
  "data/climate_finance_survey_classified.csv",  # Legacy, remove all references
  "data/climate_finance_survey_anonymized.csv"   # Old naming convention
)

# Function to check for deprecated paths
check_deprecated <- function() {
  for (path in DEPRECATED_PATHS) {
    if (file.exists(path)) {
      warning(paste(
        "DEPRECATED FILE FOUND:", path,
        "\nThis file uses old naming conventions.",
        "\nPlease delete and re-run pipeline."
      ))
    }
  }
}

# Validate paths exist at expected stages
validate_stage <- function(stage) {
  required <- switch(stage,
    "anonymize" = character(0),  # No requirements for first stage
    "classify" = c(PATHS$basic_anon),
    "quota" = c(PATHS$preliminary_classified),
    "analyze" = c(PATHS$final_1307),
    stop("Invalid stage specified")
  )
  
  missing <- required[!file.exists(required)]
  if (length(missing) > 0) {
    stop(paste(
      "Missing required files for stage '", stage, "':",
      paste("\n  -", missing, collapse = ""),
      "\nRun previous pipeline stages first.",
      sep = ""
    ))
  }
}