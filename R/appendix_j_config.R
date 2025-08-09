#!/usr/bin/env Rscript
# appendix_j_config.R - Centralized Appendix J Target Distribution
# Purpose: Single source of truth for target distribution quotas
# Author: Richard McKinney
# Date: 2025-08-08
# Version: 1.0

# This file defines the target distribution (Appendix J) used by multiple scripts
# Sourcing this file ensures consistency across all quota-matching operations

# ========================= TARGET DISTRIBUTION =========================
# Define target distribution as a function to ensure clean namespace
get_appendix_j_target <- function() {
  # Ensure tibble is available
  if (!requireNamespace("tibble", quietly = TRUE)) {
    stop("Package 'tibble' is required but not installed. Please install it with: install.packages('tibble')")
  }
  
  # Using tibble::tribble for clarity and maintainability
  # Total must equal 1,307 for publication requirements
  target <- tibble::tribble(
    ~Category,                              ~Target,
    "Entrepreneur in Climate Technology",      159,
    "Venture Capital Firm",                    117,
    "Investment and Financial Services",       109,
    "Private Equity Firm",                      88,
    "Business Consulting and Advisory",         79,
    "Nonprofit Organization",                   73,
    "High Net-Worth Individual",                66,
    "Government Funding Agency",                53,
    "Academic or Research Institution",         52,
    "Limited Partner",                          49,
    "Family Office",                            48,
    "Corporate Venture Arm",                    47,
    "Angel Investor",                           43,
    "ESG Investor",                             38,
    "Legal Services",                           38,
    "Corporate Entities",                       35,
    "Manufacturing and Industrial",             25,
    "Energy and Infrastructure",                24,
    "Real Estate and Property",                 20,
    "Philanthropic Organization",               19,
    "Technology and Software",                  19,
    "Media and Communication",                   7,
    "Miscellaneous and Individual Respondents", 151
  )
  
  # Validate total
  target_sum <- sum(target$Target)
  if (target_sum != 1307) {
    warning(sprintf(
      "Target distribution sums to %d, not 1,307! Check configuration.",
      target_sum
    ))
  }
  
  return(target)
}

# ========================= METADATA =========================
# Additional configuration parameters related to Appendix J
APPENDIX_J_CONFIG <- list(
  target_n = 1307,
  n_categories = 23,
  version = "2025-08-08",
  description = "Climate Finance Survey - Appendix J Distribution",
  
  # Role column candidates (in priority order)
  role_column_candidates = c(
    "Final_Role_Category",
    "final_category_appendix_j", 
    "stakeholder_category",
    "Final_Category",
    "Category",
    "Role_Category",
    "role_category",
    "StakeholderType"
  ),
  
  # Progress column candidates (in priority order)
  progress_column_candidates = c(
    "Progress",
    "progress",
    "Progress_pct",
    "completion",
    "Completion",
    "PercentComplete",
    "percent_complete",
    "pct_complete"
  ),
  
  # Default thresholds
  min_progress = 10,  # Minimum progress % for inclusion
  
  # Quality control parameters
  quality_exclusions = c(
    "R_bBAyiwWo1sotqM6"  # Documented straight-line respondent
  )
)

# ========================= UTILITY FUNCTIONS =========================
# Helper function to find the appropriate role column in a dataframe
find_role_column <- function(df, candidates = APPENDIX_J_CONFIG$role_column_candidates) {
  role_col <- intersect(candidates, names(df))
  
  if (length(role_col) == 0) {
    # Try pattern matching as fallback
    role_pattern <- grep("role|category|stakeholder", names(df), 
                        ignore.case = TRUE, value = TRUE)
    if (length(role_pattern) > 0) {
      return(role_pattern[1])
    } else {
      stop("No role category column found. Looked for: ", 
           paste(candidates, collapse = ", "),
           "\nAvailable columns: ", paste(names(df), collapse = ", "))
    }
  }
  
  return(role_col[1])
}

# Helper function to find the appropriate progress column in a dataframe
find_progress_column <- function(df, candidates = APPENDIX_J_CONFIG$progress_column_candidates) {
  progress_col <- intersect(candidates, names(df))
  
  if (length(progress_col) == 0) {
    # Try pattern matching as fallback
    prog_pattern <- grep("prog|compl|pct|percent", names(df), 
                        ignore.case = TRUE, value = TRUE)
    if (length(prog_pattern) > 0) {
      return(prog_pattern[1])
    } else {
      return(NULL)  # Return NULL if not found, let caller handle
    }
  }
  
  return(progress_col[1])
}

# Helper function to validate categories against target
validate_categories <- function(data_categories, target = get_appendix_j_target()) {
  target_cats <- target$Category
  data_cats <- unique(data_categories[!is.na(data_categories) & data_categories != ""])
  
  missing_in_data <- setdiff(target_cats, data_cats)
  extra_in_data <- setdiff(data_cats, target_cats)
  
  list(
    missing = missing_in_data,
    extra = extra_in_data,
    n_matched = length(intersect(target_cats, data_cats)),
    n_target = length(target_cats),
    match_rate = length(intersect(target_cats, data_cats)) / length(target_cats)
  )
}

# ========================= EXPORT MESSAGE =========================
# Only print if sourced interactively
if (interactive() && !exists(".appendix_j_config_silent")) {
  cat("Appendix J configuration loaded successfully.\n")
  cat("  Target N: ", APPENDIX_J_CONFIG$target_n, "\n")
  cat("  Categories: ", APPENDIX_J_CONFIG$n_categories, "\n")
  cat("  Version: ", APPENDIX_J_CONFIG$version, "\n")
  cat("Functions available:\n")
  cat("  - get_appendix_j_target(): Returns target distribution tibble\n")
  cat("  - find_role_column(df): Finds role category column\n")
  cat("  - find_progress_column(df): Finds progress column\n")
  cat("  - validate_categories(data_categories): Validates against target\n")
}