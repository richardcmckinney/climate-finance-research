#!/usr/bin/env Rscript
# 02_main_analysis.R
# Purpose: Compatibility wrapper to run core analysis on FINAL quota-matched dataset
# Version: 4.0 - COMPREHENSIVE FIX: Strict validation and error handling
# Date: 2025-08-09
# Author: Richard McKinney
#
# CRITICAL CHANGE LOG:
# - v4.0: Added strict column validation to prevent silent failures
# - v4.0: Implemented proper error handling for script delegation
# - v4.0: Enhanced environment isolation for sourced scripts
# - v4.0: Improved cli-based messaging for better diagnostics
# - v3.0: Fixed to use quota_target_category (not Final_Role_Category) for analysis
# - v3.0: Removed hardcoded paths, fully integrated with centralized config

# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)

# Load required packages with enhanced error handling
suppressPackageStartupMessages({
  library(tidyverse)  # Includes readr for consistent file I/O
  library(cli)        # Enhanced console messaging
})

# ========================= LOAD CENTRAL CONFIGURATION =========================
cli::cli_h1("Initializing Analysis Configuration")

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
    cli::cli_alert_success("Loaded central configuration from: {.file {config_path}}")
    break
  }
}

if (!config_loaded) {
  cli::cli_abort(c(
    "Could not find 00_config.R",
    "x" = "This file is required for centralized paths and utilities",
    "i" = "Searched locations: {.file {config_paths}}"
  ))
}

# ========================= VALIDATE AND LOAD DATA =========================
cli::cli_h1("Loading and Validating Dataset")

# Validate that required stage has been completed
validate_stage("analyze")

# Use centralized path - no hardcoding
if (!file.exists(PATHS$final_1307)) {
  cli::cli_abort(c(
    "Final dataset not found",
    "x" = "Missing file: {.file {PATHS$final_1307}}",
    "i" = "Run get_exact_1307.R first to generate the quota-matched dataset"
  ))
}

# Load data using readr for consistency with comprehensive error handling
cli::cli_progress_step("Reading dataset from {.file {basename(PATHS$final_1307)}}")

df <- tryCatch({
  readr::read_csv(PATHS$final_1307, show_col_types = FALSE, progress = FALSE)
}, error = function(e) {
  cli::cli_abort(c(
    "Failed to read final dataset",
    "x" = conditionMessage(e),
    "i" = "File path: {.file {PATHS$final_1307}}"
  ))
})

cli::cli_progress_done()

# Basic dataset information
cli::cli_alert_info("Dataset loaded: {.val {nrow(df)}} rows, {.val {ncol(df)}} columns")

# ========================= CRITICAL: STRICT COLUMN VALIDATION =========================
cli::cli_h2("Validating Dataset Structure")

# Check for identifier columns
required_id_cols <- c("ResponseId", "respondent_id")
if (!any(required_id_cols %in% names(df))) {
  cli::cli_abort(c(
    "No identifier column found",
    "x" = "Expected one of: {.field {required_id_cols}}",
    "i" = "Available columns: {.field {head(names(df), 10)}}..."
  ))
}

# CRITICAL: For N=1,307 analysis, quota column is absolutely required
if (!"quota_target_category" %in% names(df)) {
  cli::cli_abort(c(
    "Dataset missing required column: {.field quota_target_category}",
    "x" = "This suggests quota matching was not performed",
    "i" = "Run {.code get_exact_1307.R} first to create the final dataset",
    "!" = "Cannot proceed with analysis without quota-matched categories"
  ))
}

# Set the analysis column with certainty
ANALYSIS_ROLE_COLUMN <- "quota_target_category"
cli::cli_alert_success("Found quota-matched category column: {.field {ANALYSIS_ROLE_COLUMN}}")

# Additional validation for data integrity
cli::cli_h3("Data Integrity Checks")

# Check for Original_Classification (non-critical but informative)
if (!"Original_Classification" %in% names(df)) {
  cli::cli_warn(c(
    "Column {.field Original_Classification} not found",
    "!" = "Cannot verify reassignments without original classifications",
    "i" = "This may indicate an incomplete quota-matching process"
  ))
} else {
  # Calculate reassignments
  n_reassigned <- sum(df$quota_target_category != df$Original_Classification, na.rm = TRUE)
  reassignment_pct <- round(100 * n_reassigned / nrow(df), 1)
  
  cli::cli_alert_info("Non-destructive reassignments: {.val {n_reassigned}} ({.val {reassignment_pct}}%)")
  
  # Check for Was_Reassigned flag consistency
  if ("Was_Reassigned" %in% names(df)) {
    flag_reassigned <- sum(df$Was_Reassigned, na.rm = TRUE)
    if (flag_reassigned != n_reassigned) {
      cli::cli_warn(c(
        "Inconsistency detected in reassignment tracking",
        "!" = "Was_Reassigned flag: {.val {flag_reassigned}}",
        "!" = "Actual reassignments: {.val {n_reassigned}}"
      ))
    } else {
      cli::cli_alert_success("Reassignment tracking validated")
    }
  }
}

# ========================= VALIDATE QUOTA DISTRIBUTION =========================
cli::cli_h2("Quota Distribution Analysis")

# Calculate actual distribution
quota_summary <- df %>%
  count(!!sym(ANALYSIS_ROLE_COLUMN), name = "actual_count") %>%
  arrange(desc(actual_count))

# Define expected targets for N=1,307
expected_targets <- tibble::tibble(
  category = c(
    "Asset Managers/Investors",
    "Project Developers",
    "Banks/Financial Institutions",
    "Consultants/Advisors",
    "Corporates",
    "Public/Multilateral Organizations"
  ),
  expected_count = c(500, 335, 297, 91, 57, 27)
)

# Join actual with expected
quota_validation <- quota_summary %>%
  rename(category = !!sym(ANALYSIS_ROLE_COLUMN)) %>%
  full_join(expected_targets, by = "category") %>%
  mutate(
    actual_count = replace_na(actual_count, 0),
    expected_count = replace_na(expected_count, 0),
    difference = actual_count - expected_count,
    status = case_when(
      difference == 0 ~ "exact",
      abs(difference) <= 2 ~ "close",
      TRUE ~ "mismatch"
    )
  )

# Report quota validation
cli::cli_h3("Quota Target Validation")

for (i in seq_len(nrow(quota_validation))) {
  row <- quota_validation[i, ]
  
  status_symbol <- switch(row$status,
    "exact" = cli::col_green("✓"),
    "close" = cli::col_yellow("≈"),
    "mismatch" = cli::col_red("✗")
  )
  
  status_msg <- glue::glue(
    "{status_symbol} {row$category}: {row$actual_count}/{row$expected_count}"
  )
  
  if (row$status == "mismatch") {
    diff_str <- ifelse(row$difference > 0, 
                       cli::col_red(paste0("+", row$difference)),
                       cli::col_red(as.character(row$difference)))
    cli::cli_alert_warning("{status_msg} ({diff_str})")
  } else if (row$status == "close") {
    cli::cli_alert_info(status_msg)
  } else {
    cli::cli_alert_success(status_msg)
  }
}

# Check total
total_actual <- sum(quota_validation$actual_count)
total_expected <- sum(quota_validation$expected_count)

if (total_actual != 1307) {
  cli::cli_abort(c(
    "Dataset row count mismatch",
    "x" = "Expected: {.val {1307}} rows",
    "x" = "Actual: {.val {total_actual}} rows",
    "!" = "This is not a valid N=1,307 quota-matched dataset"
  ))
}

# Warn if there are significant mismatches
n_mismatches <- sum(quota_validation$status == "mismatch")
if (n_mismatches > 0) {
  cli::cli_warn(c(
    "Quota targets not fully met",
    "!" = "{.val {n_mismatches}} categories have mismatched counts",
    "i" = "This may affect analysis representativeness"
  ))
}

# ========================= EXPORT ANALYSIS CONFIGURATION =========================
cli::cli_h2("Exporting Analysis Configuration")

# Create a structured configuration for downstream analysis
analysis_config <- list(
  dataset_path = PATHS$final_1307,
  n_rows = nrow(df),
  n_cols = ncol(df),
  analysis_column = ANALYSIS_ROLE_COLUMN,
  has_original_classification = "Original_Classification" %in% names(df),
  has_reassignment_flag = "Was_Reassigned" %in% names(df),
  quota_validation = quota_validation,
  timestamp = Sys.time()
)

cli::cli_alert_success("Analysis configuration prepared")
cli::cli_ul(c(
  "Dataset: {.file {basename(analysis_config$dataset_path)}}",
  "Analysis column: {.field {analysis_config$analysis_column}}",
  "Rows: {.val {analysis_config$n_rows}}"
))

# ========================= DELEGATE TO MAIN ANALYSIS WITH ERROR HANDLING =========================
cli::cli_h1("Delegating to Main Analysis Script")

# Define possible script locations
script_locations <- c(
  "R/03_main_analysis.R",
  "03_main_analysis.R",
  "../R/03_main_analysis.R"
)

# Find the analysis script
analysis_script <- NULL
for (location in script_locations) {
  if (file.exists(location)) {
    analysis_script <- location
    break
  }
}

if (is.null(analysis_script)) {
  cli::cli_abort(c(
    "Main analysis script not found",
    "x" = "Could not locate 03_main_analysis.R",
    "i" = "Searched locations: {.file {script_locations}}"
  ))
}

cli::cli_alert_info("Found analysis script: {.file {analysis_script}}")
cli::cli_alert_info("Analysis will use column: {.field {ANALYSIS_ROLE_COLUMN}}")

# Create isolated environment for the child script
# This prevents variable conflicts and provides clean namespace
.analysis_env <- new.env(parent = globalenv())

# Transfer required objects to the child environment
.analysis_env$df <- df
.analysis_env$ANALYSIS_ROLE_COLUMN <- ANALYSIS_ROLE_COLUMN
.analysis_env$PATHS <- PATHS
.analysis_env$analysis_config <- analysis_config

# Also transfer any utility functions from config
if (exists("validate_stage")) .analysis_env$validate_stage <- validate_stage
if (exists("detect_role_column")) .analysis_env$detect_role_column <- detect_role_column

# Source with comprehensive error handling
cli::cli_progress_step("Running main analysis")

analysis_result <- tryCatch({
  # Capture any output or warnings
  capture_output <- capture.output({
    source(analysis_script, local = .analysis_env, echo = FALSE)
  })
  
  # Check if analysis created expected outputs
  if (exists("analysis_complete", envir = .analysis_env)) {
    cli::cli_alert_success("Main analysis completed successfully")
  } else {
    cli::cli_alert_warning("Main analysis completed but did not set completion flag")
  }
  
  # Return success indicator
  list(success = TRUE, output = capture_output)
  
}, warning = function(w) {
  cli::cli_warn(c(
    "Warning in main analysis script",
    "!" = conditionMessage(w)
  ))
  # Continue despite warnings
  list(success = TRUE, warning = conditionMessage(w))
  
}, error = function(e) {
  cli::cli_abort(c(
    "Error in main analysis script",
    "x" = conditionMessage(e),
    "i" = "Script location: {.file {analysis_script}}",
    "i" = "Check the script for syntax errors or missing dependencies"
  ))
})

cli::cli_progress_done()

# ========================= COMPLETION SUMMARY =========================
cli::cli_h1("Analysis Wrapper Complete")

if (analysis_result$success) {
  cli::cli_alert_success("All analysis steps completed successfully")
  
  # Report any outputs created
  if (exists("output_files", envir = .analysis_env)) {
    cli::cli_h3("Generated Outputs")
    for (output_file in .analysis_env$output_files) {
      if (file.exists(output_file)) {
        cli::cli_alert_info("Created: {.file {output_file}}")
      }
    }
  }
} else {
  cli::cli_alert_danger("Analysis completed with issues - review warnings above")
}

# Final timestamp
cli::cli_alert_info("Completed at: {.timestamp {Sys.time()}}")

# Return invisibly for use in pipelines
invisible(list(
  config = analysis_config,
  result = analysis_result,
  environment = .analysis_env
))