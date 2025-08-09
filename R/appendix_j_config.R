#!/usr/bin/env Rscript
# appendix_j_config.R - Centralized Appendix J Target Distribution
# Purpose: Single source of truth for target distribution quotas
# Author: Richard McKinney
# Date: 2025-08-09
# Version: 2.0
# Changes: Removed package dependencies, improved pattern matching specificity

# This file defines the target distribution (Appendix J) used by multiple scripts
# Sourcing this file ensures consistency across all quota-matching operations

# ========================= TARGET DISTRIBUTION =========================
# Define target distribution as a function to ensure clean namespace
get_appendix_j_target <- function() {
  # Using base R data.frame to avoid package dependencies
  # Total must equal 1,307 for publication requirements
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
      151   # Miscellaneous and Individual Respondents
    ),
    stringsAsFactors = FALSE  # Ensure strings are not converted to factors
  )
  
  # Validate total
  target_sum <- sum(target$Target)
  if (target_sum != 1307) {
    warning(sprintf(
      "Target distribution sums to %d, not 1,307! Check configuration.",
      target_sum
    ))
  }
  
  # Validate structure
  if (nrow(target) != 23) {
    warning(sprintf(
      "Target distribution has %d categories, expected 23! Check configuration.",
      nrow(target)
    ))
  }
  
  return(target)
}

# ========================= METADATA =========================
# Additional configuration parameters related to Appendix J
APPENDIX_J_CONFIG <- list(
  target_n = 1307,
  n_categories = 23,
  version = "2025-08-09",
  description = "Climate Finance Survey - Appendix J Distribution",
  
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
  # More specific names listed first to avoid false matches
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

# ========================= UTILITY FUNCTIONS =========================
# Helper function to find the appropriate role column in a dataframe
find_role_column <- function(df, candidates = APPENDIX_J_CONFIG$role_column_candidates) {
  # First try exact matches from candidates list
  role_col <- intersect(candidates, names(df))
  
  if (length(role_col) == 0) {
    # Try more specific pattern matching as fallback
    patterns <- APPENDIX_J_CONFIG$role_patterns
    df_names_lower <- tolower(names(df))
    
    # Try primary patterns (start of column name)
    for (pattern in patterns$primary) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        # Filter to avoid columns that might contain role but aren't category columns
        # Exclude columns with words like 'enrolled', 'controller', etc.
        exclude_patterns <- c("enrol", "control", "scroll", "troll")
        valid_matches <- matches[!grepl(paste(exclude_patterns, collapse = "|"), 
                                       df_names_lower[matches], ignore.case = TRUE)]
        if (length(valid_matches) > 0) {
          return(names(df)[valid_matches[1]])
        }
      }
    }
    
    # Try secondary patterns (end of column name)
    for (pattern in patterns$secondary) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        return(names(df)[matches[1]])
      }
    }
    
    # Try contains patterns
    for (pattern in patterns$contains) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        return(names(df)[matches[1]])
      }
    }
    
    # If still no match, provide detailed error
    stop("No role category column found. Looked for exact matches: ", 
         paste(candidates, collapse = ", "),
         "\nAlso tried patterns: ", 
         paste(c(patterns$primary, patterns$secondary, patterns$contains), collapse = ", "),
         "\nAvailable columns: ", paste(names(df), collapse = ", "))
  }
  
  return(role_col[1])
}

# Helper function to find the appropriate progress column in a dataframe
find_progress_column <- function(df, candidates = APPENDIX_J_CONFIG$progress_column_candidates) {
  # First try exact matches from candidates list
  progress_col <- intersect(candidates, names(df))
  
  if (length(progress_col) == 0) {
    # Try more specific pattern matching as fallback
    patterns <- APPENDIX_J_CONFIG$progress_patterns
    df_names_lower <- tolower(names(df))
    
    # Try primary patterns (start of column name)
    for (pattern in patterns$primary) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        # Filter to avoid columns that might contain progress but aren't progress columns
        # Exclude columns with words like 'progressive', 'progression_id', etc.
        exclude_patterns <- c("progressive", "progression_id", "progress_note", "progress_comment")
        valid_matches <- matches[!grepl(paste(exclude_patterns, collapse = "|"), 
                                       df_names_lower[matches], ignore.case = TRUE)]
        if (length(valid_matches) > 0) {
          # Prefer numeric columns if multiple matches
          numeric_matches <- valid_matches[sapply(valid_matches, function(i) {
            is.numeric(df[[i]]) || all(grepl("^[0-9.]+$", na.omit(as.character(df[[i]]))))
          })]
          if (length(numeric_matches) > 0) {
            return(names(df)[numeric_matches[1]])
          }
          return(names(df)[valid_matches[1]])
        }
      }
    }
    
    # Try secondary patterns (end of column name)
    for (pattern in patterns$secondary) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        # Prefer numeric columns if multiple matches
        numeric_matches <- matches[sapply(matches, function(i) {
          is.numeric(df[[i]]) || all(grepl("^[0-9.]+$", na.omit(as.character(df[[i]]))))
        })]
        if (length(numeric_matches) > 0) {
          return(names(df)[numeric_matches[1]])
        }
        return(names(df)[matches[1]])
      }
    }
    
    # Try contains patterns
    for (pattern in patterns$contains) {
      matches <- grep(pattern, df_names_lower, ignore.case = TRUE, value = FALSE)
      if (length(matches) > 0) {
        # Prefer numeric columns if multiple matches
        numeric_matches <- matches[sapply(matches, function(i) {
          is.numeric(df[[i]]) || all(grepl("^[0-9.]+$", na.omit(as.character(df[[i]]))))
        })]
        if (length(numeric_matches) > 0) {
          return(names(df)[numeric_matches[1]])
        }
        return(names(df)[matches[1]])
      }
    }
    
    # Return NULL if not found, let caller handle
    return(NULL)
  }
  
  return(progress_col[1])
}

# Helper function to validate categories against target
validate_categories <- function(data_categories, target = get_appendix_j_target(), 
                               verbose = FALSE) {
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
  
  # Print verbose output if requested
  if (verbose) {
    cat("\n=== Category Validation Report ===\n")
    cat(sprintf("Target categories: %d\n", n_target))
    cat(sprintf("Data categories: %d\n", length(data_cats)))
    cat(sprintf("Matched: %d (%.1f%%)\n", n_matched, match_rate * 100))
    
    if (length(missing_in_data) > 0) {
      cat("\nMissing in data:\n")
      for (cat_name in missing_in_data) {
        cat(sprintf("  - %s\n", cat_name))
      }
    }
    
    if (length(extra_in_data) > 0) {
      cat("\nExtra in data (not in target):\n")
      for (cat_name in extra_in_data) {
        cat(sprintf("  - %s\n", cat_name))
      }
    }
    
    if (result$is_valid) {
      cat("\n✓ Validation PASSED: All categories match perfectly.\n")
    } else {
      cat("\n✗ Validation FAILED: Category mismatches detected.\n")
    }
    cat("==================================\n\n")
  }
  
  return(result)
}

# Helper function to get a summary of the target distribution
get_target_summary <- function(target = get_appendix_j_target()) {
  # Calculate summary statistics
  summary_stats <- list(
    total_n = sum(target$Target),
    n_categories = nrow(target),
    mean_per_category = mean(target$Target),
    median_per_category = median(target$Target),
    min_quota = min(target$Target),
    max_quota = max(target$Target),
    sd_quota = sd(target$Target),
    largest_category = target$Category[which.max(target$Target)],
    smallest_category = target$Category[which.min(target$Target)]
  )
  
  return(summary_stats)
}

# ========================= EXPORT MESSAGE =========================
# Only print if sourced interactively
if (interactive() && !exists(".appendix_j_config_silent")) {
  cat("================================================================================\n")
  cat("Appendix J configuration loaded successfully (v2.0 - dependency-free)\n")
  cat("================================================================================\n")
  cat("  Target N:    ", APPENDIX_J_CONFIG$target_n, "\n")
  cat("  Categories:  ", APPENDIX_J_CONFIG$n_categories, "\n")
  cat("  Version:     ", APPENDIX_J_CONFIG$version, "\n")
  cat("--------------------------------------------------------------------------------\n")
  cat("Functions available:\n")
  cat("  - get_appendix_j_target()       : Returns target distribution data.frame\n")
  cat("  - find_role_column(df)          : Finds role category column with improved matching\n")
  cat("  - find_progress_column(df)      : Finds progress column with improved matching\n")
  cat("  - validate_categories(data, ...) : Validates categories against target\n")
  cat("  - get_target_summary()          : Returns summary statistics of target distribution\n")
  cat("================================================================================\n")
}