# R/00_config.R - Central configuration for all paths and standards
# This file must be sourced by all other scripts
# Version: 3.0
# Date: 2025-08-09
# 
# IMPORTANT: This is a pure configuration file with NO external dependencies.
# All functions requiring packages should be implemented in their respective modules.

# =============================================================================
# ENVIRONMENT SETUP (Set deterministic behavior)
# =============================================================================
options(stringsAsFactors = FALSE)
Sys.setenv(TZ = "UTC")
set.seed(20240101)

# =============================================================================
# PATH CONFIGURATION (Single source of truth)
# =============================================================================

# Define canonical paths for all pipeline stages
PATHS <- list(
  # Stage 1: Anonymization outputs
  basic_anon = "data/survey_responses_anonymized_basic.csv",
  dictionary = "data/data_dictionary.csv",
  
  # Stage 2: Classification outputs  
  classification_template = "docs/appendix_j_classification_template.csv",
  appendix_j_template = "docs/appendix_j_classification_template.csv",  # Alias for compatibility
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
  
  # Role/category columns - standardized name
  role = "Final_Role_Category",
  
  # Alternative role column names (for detection, not standardization)
  # Note: Business logic for handling these belongs in processing scripts
  role_alternatives = c(
    "Final_Role_Category",
    "final_category_appendix_j",
    "stakeholder_category",
    "Category",
    "role_category"
  ),
  
  # Raw role data columns
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
# Using exact column names to avoid false positives from pattern matching
PRIVACY_COLUMNS <- list(
  # Exact column names
  exact = c(
    "Q2.2",           # Raw geographic data
    "email",          # Email addresses
    "Email",          # Alternative case
    "E-mail",         # Alternative spelling
    "address",        # Physical addresses
    "Address",        # Alternative case
    "phone",          # Phone numbers
    "Phone",          # Alternative case
    "organization",   # Organization names
    "Organization",   # Alternative case
    "company",        # Company names
    "Company",        # Alternative case
    "name",           # Personal names (exact match only)
    "Name",           # Alternative case
    "ip_address",     # IP addresses
    "IP_Address",     # Alternative case
    "IPAddress",      # Alternative format
    "latitude",       # GPS coordinates
    "Latitude",       # Alternative case
    "longitude",      # GPS coordinates
    "Longitude"       # Alternative case
  ),
  
  # Patterns for partial matching (use with caution)
  # These require word boundaries or specific context to avoid false positives
  patterns = list(
    email = "\\b[Ee][-_]?mail\\b",
    phone = "\\b[Pp]hone\\b",
    address = "\\b[Aa]ddress\\b",
    ip = "\\bIP[-_]?[Aa]ddress\\b",  # More specific pattern for IP
    coords = "\\b(lat|lon|latitude|longitude)\\b"
  )
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
# Fixed to use more precise matching to avoid false positives
check_privacy_violations <- function(df, stop_on_violation = TRUE) {
  violations <- character()
  
  # Check for exact column name matches
  exact_matches <- intersect(PRIVACY_COLUMNS$exact, names(df))
  if (length(exact_matches) > 0) {
    for (col in exact_matches) {
      violations <- c(violations, paste("Exact match found:", col))
    }
  }
  
  # Check for pattern matches with more precision
  # Only check patterns if explicitly needed and with word boundaries
  if (length(PRIVACY_COLUMNS$patterns) > 0) {
    for (pattern_name in names(PRIVACY_COLUMNS$patterns)) {
      pattern <- PRIVACY_COLUMNS$patterns[[pattern_name]]
      pattern_matches <- grep(pattern, names(df), perl = TRUE, value = TRUE)
      
      # Filter out false positives based on context
      # For example, "description" should not match "ip" pattern
      if (pattern_name == "ip" && length(pattern_matches) > 0) {
        # Only keep matches that actually relate to IP addresses
        pattern_matches <- pattern_matches[grepl("\\bIP", pattern_matches, ignore.case = TRUE)]
      }
      
      if (length(pattern_matches) > 0) {
        violations <- c(violations, 
                       paste("Pattern match for", pattern_name, ":", 
                             paste(pattern_matches, collapse = ", ")))
      }
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

# Get a safe file path (creates directory if needed)
safe_path <- function(path) {
  dir_path <- dirname(path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }
  return(normalizePath(path, mustWork = FALSE))
}

# Helper function to detect role column in a dataframe
# Returns the column name if found, NULL otherwise
# Note: This is configuration/detection only - standardization belongs in processing scripts
detect_role_column <- function(df) {
  if (!is.data.frame(df)) {
    stop("Input must be a data frame")
  }
  
  # Check for standard role column alternatives
  role_cols <- intersect(STANDARD_COLUMNS$role_alternatives, names(df))
  
  if (length(role_cols) > 0) {
    # Return the first match based on priority order
    return(role_cols[1])
  }
  
  # No role column found
  return(NULL)
}

# =============================================================================
# PIPELINE STAGE TRACKING
# =============================================================================

# Track current pipeline stage (for error reporting)
# Note: Using a list in .GlobalEnv to avoid namespace issues
if (!exists(".PIPELINE_STATE", envir = .GlobalEnv)) {
  assign(".PIPELINE_STATE", list(stage = NULL), envir = .GlobalEnv)
}

set_stage <- function(stage) {
  .PIPELINE_STATE <- get(".PIPELINE_STATE", envir = .GlobalEnv)
  .PIPELINE_STATE$stage <- stage
  assign(".PIPELINE_STATE", .PIPELINE_STATE, envir = .GlobalEnv)
  message(paste("\n=== Pipeline Stage:", stage, "==="))
}

get_stage <- function() {
  if (exists(".PIPELINE_STATE", envir = .GlobalEnv)) {
    state <- get(".PIPELINE_STATE", envir = .GlobalEnv)
    if (!is.null(state$stage)) {
      return(state$stage)
    }
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
  # REMOVED: target_n = 1307,     # Now only in appendix_j_config.R
  confidence_level = 0.95,        # Confidence level for intervals
  significance_level = 0.05       # Significance level for tests
)

# Note: target_n is now exclusively defined in appendix_j_config.R as APPENDIX_J_CONFIG$target_n
# This ensures a single source of truth for the target sample size

# =============================================================================
# ERROR HANDLING CONFIGURATION
# =============================================================================

# Standard error messages for common issues
ERROR_MESSAGES <- list(
  missing_file = "Required file not found: %s",
  invalid_stage = "Invalid pipeline stage: %s",
  privacy_violation = "Privacy-sensitive data detected in: %s",
  deprecated_file = "Deprecated file detected: %s",
  column_missing = "Required column missing: %s",
  data_corruption = "Data integrity check failed for: %s",
  quota_mismatch = "Quota requirements not met for category: %s"
)

# =============================================================================
# INITIALIZATION CHECK
# =============================================================================

# Simple initialization check and status message
# Using a unique variable name to avoid conflicts
if (!exists(".config_00_initialized", envir = .GlobalEnv)) {
  deprecated <- check_deprecated(verbose = FALSE)
  
  # Create necessary directories
  required_dirs <- unique(dirname(unlist(PATHS)))
  for (dir in required_dirs) {
    if (!dir.exists(dir)) {
      dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    }
  }
  
  # Status message
  if (length(deprecated) > 0) {
    message(sprintf(
      "Configuration loaded with warnings. %d deprecated file(s) found.",
      length(deprecated)
    ))
    message("Run check_deprecated() for details.")
  } else {
    message("Master configuration (00_config.R) loaded successfully.")
    # Reference from appendix_j_config.R instead
if (exists("APPENDIX_J_CONFIG")) {
  message(sprintf("  Target N: %d", APPENDIX_J_CONFIG$target_n))
} else {
  message("  Target N: (defined in appendix_j_config.R)")
}
    message(sprintf("  Pipeline paths configured: %d", length(PATHS)))
    message(sprintf("  Quality parameters set: %d", length(QUALITY_PARAMS)))
  }
  
  # Mark as initialized
  assign(".config_00_initialized", TRUE, envir = .GlobalEnv)
}

# EOF