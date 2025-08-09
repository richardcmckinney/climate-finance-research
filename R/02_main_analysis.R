#!/usr/bin/env Rscript
# 02_main_analysis.R
# Purpose: Compatibility wrapper to run core analysis on FINAL quota-matched dataset
# Version: 3.0 - CRITICAL FIX: Use quota_target_category for N=1,307 analysis
# Date: 2025-08-09
# Author: Richard McKinney
#
# CRITICAL CHANGE LOG:
# - v3.0: Fixed to use quota_target_category (not Final_Role_Category) for analysis
# - v3.0: Removed hardcoded paths, fully integrated with centralized config
# - v3.0: Standardized file reading with readr throughout
# - v3.0: Added comprehensive validation of quota-matched data structure

# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)

# Load required packages
suppressPackageStartupMessages({
  library(tidyverse)  # Includes readr for consistent file I/O
})

# ========================= LOAD CENTRAL CONFIGURATION =========================
# Source centralized configuration for paths and functions
config_paths <- c(
  "R/00_config.R",
  "00_config.R",
  "../R/00_config.R"
)

config_loaded <- FALSE
for (config_path in config_paths) {
  if (file.exists(config_path)) {
    source(config_path)
    config_loaded <- TRUE
    cat("✓ Loaded central configuration from:", config_path, "\n")
    break
  }
}

if (!config_loaded) {
  stop("Could not find 00_config.R. This file is required for centralized paths and utilities.")
}

# ========================= VALIDATE AND LOAD DATA =========================
# Validate that required stage has been completed
validate_stage("analyze")

# Use centralized path - no hardcoding
if (!file.exists(PATHS$final_1307)) {
  stop("Final dataset not found at: ", PATHS$final_1307,
       "\nRun get_exact_1307.R first to generate the quota-matched dataset.")
}

# Load data using readr for consistency (not base read.csv)
df <- tryCatch({
  readr::read_csv(PATHS$final_1307, show_col_types = FALSE, progress = FALSE)
}, error = function(e) {
  stop("Failed to read final dataset: ", e$message)
})

# ========================= CRITICAL: VALIDATE QUOTA-MATCHED STRUCTURE =========================
cat("\n=== DATASET LOADED FOR ANALYSIS ===\n")
cat("File: ", basename(PATHS$final_1307), "\n")
cat("Rows: ", nrow(df), "\n")
cat("Columns: ", ncol(df), "\n")

# CRITICAL FIX: For N=1,307 analysis, we MUST use quota_target_category
# This reflects the actual composition after quota matching
required_cols <- c("quota_target_category", "Original_Classification", "Was_Reassigned")
missing_cols <- setdiff(required_cols, names(df))

if (length(missing_cols) > 0) {
  warning("Quota-matched columns missing: ", paste(missing_cols, collapse = ", "),
          "\nThis may indicate the dataset was not properly quota-matched.")
}

# Identify the correct analysis column based on dataset type
if ("quota_target_category" %in% names(df)) {
  # CRITICAL: This is a quota-matched dataset - use quota_target_category for analysis
  analysis_role_col <- "quota_target_category"
  cat("\n✓ QUOTA-MATCHED DATASET DETECTED\n")
  cat("  Using 'quota_target_category' for all analyses (N=1,307 composition)\n")
} else if ("Final_Role_Category" %in% names(df)) {
  # Fallback for non-quota datasets
  analysis_role_col <- "Final_Role_Category"
  cat("\n⚠ Standard dataset detected (not quota-matched)\n")
  cat("  Using 'Final_Role_Category' for analysis\n")
} else {
  # Use centralized detection function
  analysis_role_col <- detect_role_column(df)
  if (is.null(analysis_role_col)) {
    stop("No role category column found in dataset")
  }
  cat("\n⚠ Using detected role column: ", analysis_role_col, "\n")
}

# Report stakeholder distribution using the correct column
cat("\nStakeholder categories (", analysis_role_col, "):\n", sep = "")
cat_counts <- table(df[[analysis_role_col]])
for (i in seq_along(cat_counts)) {
  cat("  ", names(cat_counts)[i], ": ", cat_counts[i], "\n", sep = "")
}

# Report on quota matching details if available
if (all(c("Original_Classification", "quota_target_category", "Was_Reassigned") %in% names(df))) {
  cat("\n=== QUOTA MATCHING VALIDATION ===\n")
  cat("✓ Non-destructive quota matching confirmed\n")
  cat("  Original classifications preserved in: Original_Classification\n")
  cat("  Quota-matched assignments in: quota_target_category\n")
  cat("  Final analysis will use: quota_target_category\n")
  
  # Report on reassignments
  n_reassigned <- sum(df$Was_Reassigned, na.rm = TRUE)
  cat("  Reassigned responses: ", n_reassigned, " (", 
      round(100 * n_reassigned / nrow(df), 1), "%)\n", sep = "")
  
  # Validate quota targets
  quota_summary <- df %>%
    count(quota_target_category, name = "n") %>%
    arrange(desc(n))
  
  cat("\nQuota distribution achieved:\n")
  for (i in seq_len(nrow(quota_summary))) {
    cat("  ", quota_summary$quota_target_category[i], ": ", 
        quota_summary$n[i], "\n", sep = "")
  }
  
  # Check if targets were met
  expected_targets <- list(
    "Asset Managers/Investors" = 500,
    "Project Developers" = 335,
    "Banks/Financial Institutions" = 297,
    "Consultants/Advisors" = 91,
    "Corporates" = 57,
    "Public/Multilateral Organizations" = 27
  )
  
  cat("\nQuota target validation:\n")
  for (cat_name in names(expected_targets)) {
    actual <- sum(df$quota_target_category == cat_name, na.rm = TRUE)
    expected <- expected_targets[[cat_name]]
    status <- if (actual == expected) "✓" else "⚠"
    cat("  ", status, " ", cat_name, ": ", actual, "/", expected, "\n", sep = "")
  }
}

# ========================= EXPORT ANALYSIS CONFIGURATION =========================
# Export the determined analysis column for use by 03_main_analysis.R
ANALYSIS_ROLE_COLUMN <- analysis_role_col
cat("\n✓ Analysis configuration set: ANALYSIS_ROLE_COLUMN =", ANALYSIS_ROLE_COLUMN, "\n")

# ========================= DELEGATE TO MAIN ANALYSIS =========================
cat("\n=== DELEGATING TO MAIN ANALYSIS SCRIPT ===\n")

# Check if main analysis script exists
analysis_script <- "R/03_main_analysis.R"
if (!file.exists(analysis_script)) {
  # Try alternative location
  analysis_script <- "03_main_analysis.R"
  if (!file.exists(analysis_script)) {
    stop("Main analysis script not found. Looked for:\n",
         "  - R/03_main_analysis.R\n",
         "  - 03_main_analysis.R")
  }
}

cat("Running: ", analysis_script, "\n")
cat("Dataset: ", basename(PATHS$final_1307), "\n")
cat("Analysis column: ", ANALYSIS_ROLE_COLUMN, "\n\n")

# Source the main analysis with the correct configuration
source(analysis_script)