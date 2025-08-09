#!/usr/bin/env Rscript
# 02_main_analysis.R
# Purpose: Compatibility wrapper to run core outputs off the FINAL dataset
# Version: 2.0 - Updated to use centralized configuration
# Date: 2025-08-09

# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)

# Load required packages
suppressPackageStartupMessages(library(tidyverse))

# ========================= LOAD CENTRAL CONFIGURATION =========================
# Source centralized configuration for paths
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
    cat("Loaded central configuration from:", config_path, "\n")
    break
  }
}

if (!config_loaded) {
  stop("Could not find 00_config.R. This file is required for centralized paths.")
}

# ========================= VALIDATE AND LOAD DATA =========================
# Validate that required stage has been completed
validate_stage("analyze")

# Use centralized path instead of hardcoded path
if (!file.exists(PATHS$final_1307)) {
  stop("Final dataset not found at: ", PATHS$final_1307,
       "\nRun get_exact_1307.R first to generate the quota-matched dataset.")
}

# Load data using centralized path
df <- tryCatch({
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_csv(PATHS$final_1307, show_col_types = FALSE)
  } else {
    read.csv(PATHS$final_1307, stringsAsFactors = FALSE)
  }
}, error = function(e) {
  stop("Failed to read final dataset: ", e$message)
})

# ========================= DATA VALIDATION =========================
cat("\n=== DATASET LOADED FOR ANALYSIS ===\n")
cat("File: ", basename(PATHS$final_1307), "\n")
cat("Rows: ", nrow(df), "\n")
cat("Columns: ", ncol(df), "\n")

# Check for expected role column
role_col <- if ("Final_Role_Category" %in% names(df)) {
  "Final_Role_Category"
} else {
  detect_role_column(df)
}

if (!is.null(role_col)) {
  cat("\nStakeholder categories (", role_col, "):\n", sep = "")
  cat_counts <- table(df[[role_col]])
  for (i in seq_along(cat_counts)) {
    cat("  ", names(cat_counts)[i], ": ", cat_counts[i], "\n", sep = "")
  }
} else {
  warning("No role category column found in dataset")
}

# Check for non-destructive quota matching columns
if (all(c("Original_Classification", "Final_Role_Category") %in% names(df))) {
  cat("\n✓ Non-destructive quota matching detected\n")
  cat("  Original classifications preserved in: Original_Classification\n")
  cat("  Quota-matched assignments in: Final_Role_Category\n")
  
  # Report on reassignments
  if ("Was_Reassigned" %in% names(df)) {
    n_reassigned <- sum(df$Was_Reassigned, na.rm = TRUE)
    cat("  Reassigned responses: ", n_reassigned, " (", 
        round(100 * n_reassigned / nrow(df), 1), "%)\n", sep = "")
  }
}

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

cat("Running: ", analysis_script, "\n\n")
source(analysis_script)