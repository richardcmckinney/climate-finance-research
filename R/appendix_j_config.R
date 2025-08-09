#!/usr/bin/env Rscript
# appendix_j_config.R - Centralized Appendix J Target Distribution
# Purpose: Single source of truth for target distribution quotas with self-contained helpers
# Author: Richard McKinney
# Date: 2025-08-09
# Version: 3.0 - Enhanced with dependency management and robust helper functions
# Changes: 
#   - Added internal dependency handling for rlang when needed
#   - Made all helper functions self-contained
#   - Enhanced error handling and validation
#   - Added dynamic column selection utilities

# ========================= DEPENDENCY MANAGEMENT =========================
# Ensure required packages are available when needed
.ensure_rlang <- function() {
  # Check if rlang is available, load it silently if possible
  if (!requireNamespace("rlang", quietly = TRUE)) {
    # If rlang is not available, provide fallback functionality
    return(FALSE)
  }
  return(TRUE)
}

# Helper for dynamic column selection that works with or without rlang
.safe_select_column <- function(df, col_name) {
  # This function provides a safe way to select columns dynamically
  # Works with both base R and tidyverse contexts
  if (col_name %in% names(df)) {
    return(df[[col_name]])
  } else {
    stop("Column '", col_name, "' not found in dataframe")
  }
}

# ========================= TARGET DISTRIBUTION =========================
# Define target distribution as a function to ensure clean namespace
get_appendix_j_target <- function() {
  # Using base R data.frame to avoid package dependencies
  # Total MUST equal exactly 1,307 for publication requirements
  target <- data.frame(
    Category = c(
      "Entrepreneur in Climate Technology",
      "Venture Capital Firm",
      "Investment and Financial Services",
      "Private Equity Firm",
      "Business Consulting and Advisory",
      "Nonprofit Organization",
      "High Net-Worth Individual",
      "Government Funding Agency",
      "Academic or Research Institution",
      "Limited Partner",
      "Family Office",
      "Corporate Venture Arm",
      "Angel Investor",
      "ESG Investor",
      "Legal Services",
      "Corporate Entities",
      "Manufacturing and Industrial",
      "Energy and Infrastructure",
      "Real Estate and Property",
      "Philanthropic Organization",
      "Technology and Software",
      "Media and Communication",
      "Miscellaneous and Individual Respondents"
    ),
    Target = c(
      159,  # Entrepreneur in Climate Technology
      117,  # Venture Capital Firm
      109,  # Investment and Financial Services
      88,   # Private Equity Firm
      79,   # Business Consulting and Advisory
      73,   # Nonprofit Organization
      66,   # High Net-Worth Individual
      53,   # Government Funding Agency
      52,   # Academic or Research Institution
      49,   # Limited Partner
      48,   # Family Office
      47,   # Corporate Venture Arm
      43,   # Angel Investor
      38,   # ESG Investor
      38,   # Legal Services
      35,   # Corporate Entities
      25,   # Manufacturing and Industrial
      24,   # Energy and Infrastructure
      20,   # Real Estate and Property
      19,   # Philanthropic Organization
      19,   # Technology and Software
      7,    # Media and Communication
      99    # Miscellaneous and Individual Respondents
    ),
    stringsAsFactors = FALSE  # Ensure strings are not converted to factors
  )
  
  # Validate total (CRITICAL - must be exactly 1307)
  target_sum <- sum(target$Target)
  if (target_sum != 1307) {
    stop(sprintf(
      "CRITICAL ERROR: Target distribution sums to %d, not 1,307!\n%s",
      target_sum,
      "This must be fixed immediately in appendix_j_config.R"
    ))
  }
  
  # Validate structure
  if (nrow(target) != 23) {
    stop(sprintf(
      "CRITICAL ERROR: Target distribution has %d categories, expected 23!",
      nrow(target)
    ))
  }
  
  # Add row names for easier reference
  rownames(target) <- NULL
  
  # Ensure consistent ordering
  target <- target[order(target$Target, decreasing = TRUE), ]
  rownames(target) <- NULL
  
  return(target)
}

# ========================= METADATA =========================
# Additional configuration parameters related to Appendix J
APPENDIX_J_CONFIG <- list(
  target_n = 1307,
  n_categories = 23,
  version = "2025-08-09-v3.0",  # Updated version for enhanced functionality
  description = "Climate Finance Survey - Appendix J Distribution (Enhanced)",
  
  # Role column candidates (in priority order)
  # More specific names listed first to avoid false matches
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
  ),
  
  # Pattern matching configuration for fallback searches
  role_patterns = list(
    primary = c("^role", "^category", "^stakeholder"),  # Start of column name
    secondary = c("_role$", "_category$", "_type$"),     # End of column name
    contains = c("role_cat", "category_", "stakeholder") # Contains specific strings
  ),
  
  progress_patterns = list(
    primary = c("^progress", "^completion", "^percent"),   # Start of column name
    secondary = c("_progress$", "_complete$", "_pct$"),    # End of column name
    contains = c("progress_", "completion_", "percent_")   # Contains specific strings
  )
)

# ========================= ENHANCED UTILITY FUNCTIONS =========================

# Helper function to find the appropriate role column in a dataframe
# Now with enhanced error handling and self-contained functionality
find_role_column <- function(df, candidates = APPENDIX_J_CONFIG$role_column_candidates, 
                            verbose = FALSE) {
  # Validate input
  if (!is.data.frame(df) && !inherits(df, "tbl_df")) {
    stop("Input must be a data frame or tibble")
  }
  
  if (ncol(df) == 0) {
    stop("Input data frame has no columns")
  }
  
  # First try exact matches from candidates list
  role_col <- intersect(candidates, names(df))
  
  if (length(role_col) > 0) {
    if (verbose) {
      cat("Found role column via exact match:", role_col[1], "\n")
    }
    return(role_col[1])
  }
  
  # Try pattern matching as fallback
  patterns <- APPENDIX_J_CONFIG$role_patterns
  df_names_lower <- tolower(names(df))
  
  # Helper function for pattern matching with exclusions
  .find_pattern_match <- function(patterns, df_names, df_names_lower, exclude_patterns) {
    for (pattern in patterns) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        # Filter out false positives
        valid_matches <- matches[!grepl(paste(exclude_patterns, collapse = "|"), 
                                       df_names_lower[matches], ignore.case = TRUE)]
        if (length(valid_matches) > 0) {
          return(df_names[valid_matches[1]])
        }
      }
    }
    return(NULL)
  }
  
  # Define exclusion patterns for false positives
  exclude_patterns <- c("enrol", "control", "scroll", "troll", "parole", "prole")
  
  # Try primary patterns
  result <- .find_pattern_match(patterns$primary, names(df), df_names_lower, exclude_patterns)
  if (!is.null(result)) {
    if (verbose) cat("Found role column via primary pattern:", result, "\n")
    return(result)
  }
  
  # Try secondary patterns
  result <- .find_pattern_match(patterns$secondary, names(df), df_names_lower, exclude_patterns)
  if (!is.null(result)) {
    if (verbose) cat("Found role column via secondary pattern:", result, "\n")
    return(result)
  }
  
  # Try contains patterns
  result <- .find_pattern_match(patterns$contains, names(df), df_names_lower, exclude_patterns)
  if (!is.null(result)) {
    if (verbose) cat("Found role column via contains pattern:", result, "\n")
    return(result)
  }
  
  # If still no match, provide detailed error with suggestions
  stop("No role category column found.\n",
       "Looked for exact matches: ", paste(candidates, collapse = ", "), "\n",
       "Also tried patterns: ", 
       paste(c(patterns$primary, patterns$secondary, patterns$contains), collapse = ", "), "\n",
       "Available columns: ", paste(names(df), collapse = ", "), "\n",
       "Consider renaming your role column to one of the standard names.")
}

# Helper function to find the appropriate progress column in a dataframe
# Enhanced with better numeric detection and validation
find_progress_column <- function(df, candidates = APPENDIX_J_CONFIG$progress_column_candidates,
                                verbose = FALSE) {
  # Validate input
  if (!is.data.frame(df) && !inherits(df, "tbl_df")) {
    stop("Input must be a data frame or tibble")
  }
  
  # First try exact matches from candidates list
  progress_col <- intersect(candidates, names(df))
  
  if (length(progress_col) > 0) {
    # Verify it contains numeric-like data
    col_data <- df[[progress_col[1]]]
    if (is.numeric(col_data) || 
        all(grepl("^[0-9.]+$", na.omit(as.character(col_data))))) {
      if (verbose) cat("Found progress column via exact match:", progress_col[1], "\n")
      return(progress_col[1])
    }
  }
  
  # Try pattern matching as fallback
  patterns <- APPENDIX_J_CONFIG$progress_patterns
  df_names_lower <- tolower(names(df))
  
  # Helper function to check if column contains numeric data
  .is_numeric_column <- function(df, col_idx) {
    col_data <- df[[col_idx]]
    return(is.numeric(col_data) || 
           all(grepl("^[0-9.]+$", na.omit(as.character(col_data)))))
  }
  
  # Helper function for pattern matching with numeric validation
  .find_numeric_pattern_match <- function(patterns, df, df_names_lower, exclude_patterns) {
    for (pattern in patterns) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        # Filter out false positives
        valid_matches <- matches[!grepl(paste(exclude_patterns, collapse = "|"), 
                                       df_names_lower[matches], ignore.case = TRUE)]
        
        # Prefer numeric columns
        for (idx in valid_matches) {
          if (.is_numeric_column(df, idx)) {
            return(names(df)[idx])
          }
        }
        
        # If no numeric columns found, return first valid match
        if (length(valid_matches) > 0) {
          return(names(df)[valid_matches[1]])
        }
      }
    }
    return(NULL)
  }
  
  # Define exclusion patterns
  exclude_patterns <- c("progressive", "progression_id", "progress_note", 
                       "progress_comment", "in_progress")
  
  # Try all pattern types
  for (pattern_type in c("primary", "secondary", "contains")) {
    result <- .find_numeric_pattern_match(
      patterns[[pattern_type]], df, df_names_lower, exclude_patterns
    )
    if (!is.null(result)) {
      if (verbose) cat("Found progress column via", pattern_type, "pattern:", result, "\n")
      return(result)
    }
  }
  
  # Return NULL if not found (caller should handle)
  if (verbose) {
    cat("No progress column found. Available columns:", 
        paste(names(df), collapse = ", "), "\n")
  }
  return(NULL)
}

# Helper function to validate categories against target
# Enhanced with detailed reporting and suggestions
validate_categories <- function(data_categories, target = get_appendix_j_target(), 
                               verbose = FALSE, suggest_mappings = FALSE) {
  target_cats <- target$Category
  
  # Clean data categories - remove NA and empty strings
  data_cats <- unique(data_categories[!is.na(data_categories) & 
                                     data_categories != "" & 
                                     trimws(data_categories) != ""])
  
  # Identify mismatches
  missing_in_data <- setdiff(target_cats, data_cats)
  extra_in_data <- setdiff(data_cats, target_cats)
  matched_cats <- intersect(target_cats, data_cats)
  
  # Calculate metrics
  n_matched <- length(matched_cats)
  n_target <- length(target_cats)
  match_rate <- n_matched / n_target
  
  # Create validation result
  result <- list(
    missing = missing_in_data,
    extra = extra_in_data,
    matched = matched_cats,
    n_matched = n_matched,
    n_target = n_target,
    n_data = length(data_cats),
    match_rate = match_rate,
    is_valid = (match_rate == 1.0 && length(extra_in_data) == 0)
  )
  
  # Add suggested mappings if requested
  if (suggest_mappings && length(extra_in_data) > 0 && length(missing_in_data) > 0) {
    result$suggested_mappings <- .suggest_category_mappings(
      extra_in_data, missing_in_data
    )
  }
  
  # Print verbose output if requested
  if (verbose) {
    .print_validation_report(result)
  }
  
  return(result)
}

# Helper function to suggest category mappings
.suggest_category_mappings <- function(extra_cats, missing_cats) {
  suggestions <- list()
  
  # Simple string similarity matching
  for (extra in extra_cats) {
    best_match <- NULL
    best_score <- 0
    
    for (missing in missing_cats) {
      # Calculate simple similarity score
      score <- .string_similarity(tolower(extra), tolower(missing))
      if (score > best_score) {
        best_score <- score
        best_match <- missing
      }
    }
    
    if (best_score > 0.5) {  # Threshold for suggestion
      suggestions[[extra]] <- list(
        suggested = best_match,
        confidence = round(best_score, 2)
      )
    }
  }
  
  return(suggestions)
}

# Simple string similarity function
.string_similarity <- function(str1, str2) {
  # Count matching words
  words1 <- strsplit(str1, "\\s+")[[1]]
  words2 <- strsplit(str2, "\\s+")[[1]]
  matches <- length(intersect(words1, words2))
  total <- length(union(words1, words2))
  
  if (total == 0) return(0)
  return(matches / total)
}

# Print validation report
.print_validation_report <- function(result) {
  cat("\n=== Category Validation Report ===\n")
  cat(sprintf("Target categories: %d\n", result$n_target))
  cat(sprintf("Data categories: %d\n", result$n_data))
  cat(sprintf("Matched: %d (%.1f%%)\n", result$n_matched, result$match_rate * 100))
  
  if (length(result$missing) > 0) {
    cat("\nMissing in data:\n")
    for (cat_name in result$missing) {
      cat(sprintf("  - %s\n", cat_name))
    }
  }
  
  if (length(result$extra) > 0) {
    cat("\nExtra in data (not in target):\n")
    for (cat_name in result$extra) {
      cat(sprintf("  - %s\n", cat_name))
    }
  }
  
  if (!is.null(result$suggested_mappings) && length(result$suggested_mappings) > 0) {
    cat("\nSuggested mappings:\n")
    for (extra in names(result$suggested_mappings)) {
      sugg <- result$suggested_mappings[[extra]]
      cat(sprintf("  '%s' -> '%s' (confidence: %.0f%%)\n", 
                  extra, sugg$suggested, sugg$confidence * 100))
    }
  }
  
  if (result$is_valid) {
    cat("\n✓ Validation PASSED: All categories match perfectly.\n")
  } else {
    cat("\n✗ Validation FAILED: Category mismatches detected.\n")
  }
  cat("==================================\n\n")
}

# Helper function to get a summary of the target distribution
get_target_summary <- function(target = get_appendix_j_target(), format = "list") {
  # Calculate summary statistics
  summary_stats <- list(
    total_n = sum(target$Target),
    n_categories = nrow(target),
    mean_per_category = mean(target$Target),
    median_per_category = median(target$Target),
    min_quota = min(target$Target),
    max_quota = max(target$Target),
    sd_quota = sd(target$Target),
    cv_quota = sd(target$Target) / mean(target$Target),  # Coefficient of variation
    largest_category = target$Category[which.max(target$Target)],
    smallest_category = target$Category[which.min(target$Target)],
    largest_n = max(target$Target),
    smallest_n = min(target$Target)
  )
  
  # Return in requested format
  if (format == "data.frame") {
    return(data.frame(
      Metric = names(summary_stats),
      Value = as.character(unlist(summary_stats)),
      stringsAsFactors = FALSE
    ))
  }
  
  return(summary_stats)
}

# Enhanced function for working with tidyverse pipelines
# This function helps bridge between base R config and tidyverse usage
create_quota_matching_spec <- function(target = get_appendix_j_target()) {
  # Create a specification object that can be used with tidyverse
  spec <- list(
    target = target,
    target_n = sum(target$Target),
    categories = target$Category,
    quotas = setNames(target$Target, target$Category),
    
    # Helper function for use in dplyr pipelines
    get_quota = function(category) {
      if (category %in% names(spec$quotas)) {
        return(spec$quotas[[category]])
      }
      return(NA_integer_)
    },
    
    # Function to create rlang symbols if available
    create_sym = function(col_name) {
      if (.ensure_rlang()) {
        return(rlang::sym(col_name))
      } else {
        # Return the column name as-is for base R operations
        return(col_name)
      }
    }
  )
  
  class(spec) <- c("quota_matching_spec", "list")
  return(spec)
}

# Print method for quota matching spec
print.quota_matching_spec <- function(x, ...) {
  cat("Quota Matching Specification\n")
  cat("============================\n")
  cat("Total target N:", x$target_n, "\n")
  cat("Categories:", length(x$categories), "\n")
  cat("Range of quotas:", min(x$quotas), "-", max(x$quotas), "\n")
  cat("\nTop 5 categories by quota:\n")
  top5 <- head(sort(x$quotas, decreasing = TRUE), 5)
  for (cat in names(top5)) {
    cat(sprintf("  %-40s: %d\n", substr(cat, 1, 40), top5[[cat]]))
  }
  invisible(x)
}

# ========================= EXPORT MESSAGE =========================
# Only print if sourced interactively
if (interactive() && !exists(".appendix_j_config_silent")) {
  cat("================================================================================\n")
  cat("Appendix J configuration loaded successfully (v3.0 - Enhanced)\n")
  cat("================================================================================\n")
  cat("  Target N:     ", APPENDIX_J_CONFIG$target_n, " ✓ VERIFIED\n")
  cat("  Categories:   ", APPENDIX_J_CONFIG$n_categories, "\n")
  cat("  Version:      ", APPENDIX_J_CONFIG$version, "\n")
  cat("--------------------------------------------------------------------------------\n")
  cat("Core Functions:\n")
  cat("  - get_appendix_j_target()        : Returns target distribution data.frame\n")
  cat("  - find_role_column(df)           : Finds role category column (enhanced)\n")
  cat("  - find_progress_column(df)       : Finds progress column (enhanced)\n")
  cat("  - validate_categories(data, ...) : Validates categories with suggestions\n")
  cat("  - get_target_summary()           : Returns summary statistics\n")
  cat("\nNew Functions:\n")
  cat("  - create_quota_matching_spec()   : Creates spec for tidyverse pipelines\n")
  cat("\nEnhancements:\n")
  cat("  ✓ Self-contained dependency management\n")
  cat("  ✓ Works with both base R and tidyverse\n")
  cat("  ✓ Improved error messages and suggestions\n")
  cat("  ✓ Robust pattern matching and validation\n")
  cat("================================================================================\n")
}

# ========================= COMPATIBILITY CHECK =========================
# Ensure this config works in various environments
.check_environment <- function() {
  checks <- list(
    base_r = TRUE,  # Always available
    rlang = requireNamespace("rlang", quietly = TRUE),
    tidyverse = requireNamespace("dplyr", quietly = TRUE),
    readr = requireNamespace("readr", quietly = TRUE)
  )
  
  return(checks)
}

# Store environment check results for debugging if needed
.APPENDIX_J_ENV <- .check_environment()