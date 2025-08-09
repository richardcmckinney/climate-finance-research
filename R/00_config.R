# R/00_config.R - Central configuration for all paths and standards
# This file must be sourced by all other scripts
# Version: 2.0
# Date: 2025-08-08

# =============================================================================
# PATH CONFIGURATION
# =============================================================================

# Define canonical paths for all pipeline stages
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
  quota_stats = "output/quota_matching_statistics.csv",
  quality_report = "output/quality_control_report.csv",
  
  # Stage 4: Analysis outputs
  hypothesis_summary = "output/hypothesis_testing_summary.csv",
  three_factor_model = "output/tables/three_factor_summary.csv",
  stakeholder_dist = "output/stakeholder_distribution_final.csv",
  barrier_analysis = "output/barrier_presence_by_stakeholder.csv",
  geographic_dist = "output/geographic_distribution_final.csv",
  
  # Verification outputs
  checksums = "docs/checksums.txt",
  verification_report = "docs/verification_report.md",
  error_log = "docs/error_log.txt",
  quality_assurance = "output/quality_assurance_report.csv"
)

# Define deprecated paths that should NEVER be used
DEPRECATED_PATHS <- c(
  "data/climate_finance_survey_classified.csv",  # Legacy, remove all references
  "data/climate_finance_survey_anonymized.csv"   # Old naming convention
)

# =============================================================================
# COLUMN NAME STANDARDS
# =============================================================================

# Define canonical column names used throughout pipeline
STANDARD_COLUMNS <- list(
  # Core identifiers
  respondent_id = "respondent_id",
  response_id = "ResponseId",
  
  # Role/category columns
  role = "Final_Role_Category",
  role_raw = "Role_Raw", 
  role_text = "Role_Other_Text",
  
  # Progress and metadata
  progress = "Progress",
  consent = "Consent",
  
  # Geographic (anonymized only)
  geography = "region",  # Must be anonymized to regions
  
  # Classification metadata
  classification_stage = "Classification_Stage",
  classification_version = "Classification_Version",
  classification_date = "Classification_Date"
)

# Privacy-sensitive columns that must NEVER appear in outputs
PRIVACY_COLUMNS <- c(
  "Q2.2",           # Raw geographic data
  "email",          # Email addresses
  "address",        # Physical addresses  
  "phone",          # Phone numbers
  "organization",   # Organization names
  "company",        # Company names
  "name",           # Personal names
  "ip_address",     # IP addresses
  "latitude",       # GPS coordinates
  "longitude"       # GPS coordinates
)

# =============================================================================
# VALIDATION FUNCTIONS
# =============================================================================

# Function to check for deprecated paths
check_deprecated <- function(verbose = TRUE) {
  deprecated_found <- character()
  
  for (path in DEPRECATED_PATHS) {
    if (file.exists(path)) {
      deprecated_found <- c(deprecated_found, path)
      if (verbose) {
        warning(paste(
          "DEPRECATED FILE FOUND:", path,
          "\nThis file uses old naming conventions.",
          "\nPlease delete and re-run pipeline.",
          sep = "\n"
        ))
      }
    }
  }
  
  return(deprecated_found)
}

# Validate paths exist at expected stages
validate_stage <- function(stage) {
  required <- switch(stage,
    "anonymize" = character(0),  # No requirements for first stage
    "classify" = c(PATHS$basic_anon),
    "quota" = c(PATHS$preliminary_classified),
    "analyze" = c(PATHS$final_1307),
    "verify" = c(PATHS$final_1307, PATHS$preliminary_classified),
    stop("Invalid stage specified. Valid stages: anonymize, classify, quota, analyze, verify")
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
  
  invisible(TRUE)
}

# Validate column names in a dataframe
validate_columns <- function(df, required_cols) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    stop(paste(
      "Required columns missing from dataframe:",
      paste("\n  -", missing, collapse = ""),
      "\nExpected column names per STANDARD_COLUMNS configuration.",
      sep = ""
    ))
  }
  invisible(TRUE)
}

# Check for privacy violations in a dataframe
check_privacy_violations <- function(df, stop_on_violation = TRUE) {
  violations <- character()
  
  # Check column names
  for (col in PRIVACY_COLUMNS) {
    if (col %in% names(df)) {
      violations <- c(violations, paste("Column found:", col))
    }
    # Also check for partial matches
    pattern_matches <- grep(col, names(df), ignore.case = TRUE, value = TRUE)
    if (length(pattern_matches) > 0) {
      violations <- c(violations, 
                     paste("Pattern match for", col, ":", 
                           paste(pattern_matches, collapse = ", ")))
    }
  }
  
  if (length(violations) > 0) {
    msg <- paste(
      "PRIVACY VIOLATIONS DETECTED:",
      paste("\n  -", violations, collapse = ""),
      "\nThese columns/patterns must not appear in output data.",
      sep = "\n"
    )
    
    if (stop_on_violation) {
      stop(msg)
    } else {
      warning(msg)
    }
  }
  
  return(length(violations) == 0)
}

# Standardization function for role columns
standardize_role_column <- function(df, verbose = TRUE) {
  # Possible role column names in order of preference
  role_variants <- c(
    "Final_Role_Category",
    "final_category_appendix_j", 
    "stakeholder_category",
    "Category",
    "role_category"
  )
  
  role_col <- intersect(role_variants, names(df))[1]
  
  if (is.na(role_col)) {
    stop("No role category column found. Checked: ", 
         paste(role_variants, collapse = ", "))
  }
  
  if (role_col != "Final_Role_Category") {
    if (verbose) {
      message("Standardizing role column: ", role_col, " -> Final_Role_Category")
    }
    df <- df %>% dplyr::rename(Final_Role_Category = !!sym(role_col))
  }
  
  return(df)
}

# Get a safe file path (creates directory if needed)
safe_path <- function(path) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  return(normalizePath(path, mustWork = FALSE))
}

# =============================================================================
# PIPELINE STAGE TRACKING
# =============================================================================

# Track current pipeline stage (for error reporting)
CURRENT_STAGE <- NULL

set_stage <- function(stage) {
  assign("CURRENT_STAGE", stage, envir = .GlobalEnv)
  message(paste("\n=== Pipeline Stage:", stage, "==="))
}

get_stage <- function() {
  if (exists("CURRENT_STAGE", envir = .GlobalEnv)) {
    return(get("CURRENT_STAGE", envir = .GlobalEnv))
  }
  return("Unknown")
}

# =============================================================================
# QUALITY CONTROL PARAMETERS
# =============================================================================

QUALITY_PARAMS <- list(
  min_progress = 10,              # Minimum progress for inclusion
  min_group_size = 10,            # Minimum group size for analysis
  max_missing_pct = 20,           # Maximum missing data percentage
  target_n = 1307,                # Target sample size
  confidence_level = 0.95,        # Confidence level for intervals
  significance_level = 0.05       # Significance level for tests
)

# =============================================================================
# INITIALIZE ON LOAD
# =============================================================================

# Check for deprecated files on load
.onLoad <- function() {
  deprecated <- check_deprecated(verbose = FALSE)
  if (length(deprecated) > 0) {
    message("Note: ", length(deprecated), " deprecated file(s) found. Run check_deprecated() for details.")
  }
}

# Run initialization if being sourced
if (interactive() || !exists(".config_initialized")) {
  deprecated <- check_deprecated(verbose = FALSE)
  if (length(deprecated) > 0) {
    message("Configuration loaded. Warning: ", length(deprecated), 
            " deprecated file(s) found.")
  } else {
    message("Configuration loaded successfully. No deprecated files found.")
  }
  .config_initialized <- TRUE
}

# =============================================================================
# EXPORT MESSAGE
# =============================================================================

# List available functions for user reference
if (interactive()) {
  message("\nAvailable configuration functions:")
  message("  - check_deprecated(): Check for deprecated files")
  message("  - validate_stage(stage): Validate required files for a pipeline stage")
  message("  - validate_columns(df, cols): Check required columns exist")
  message("  - check_privacy_violations(df): Check for PII in dataframe")
  message("  - standardize_role_column(df): Standardize role column names")
  message("  - safe_path(path): Get safe file path with directory creation")
  message("\nAccess paths with: PATHS$<name>")
  message("Access standard columns with: STANDARD_COLUMNS$<name>")
}