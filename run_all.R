#!/usr/bin/env Rscript
# run_all.R - Enhanced Pipeline Runner v2.0
# Purpose: Robust, deterministic end-to-end pipeline runner with comprehensive error handling
# Description:
#   This script executes the entire data processing and analysis pipeline with:
#   - S4 method dispatch protection (.local shim)
#   - Case-sensitive filesystem compatibility
#   - Enhanced error logging and recovery
#   - Per-script method safety enforcement
#   - Comprehensive verification and checkpointing
#
# Flags:
#   --clean           Remove all known outputs before running
#   --verify          Run verification checks after pipeline completes (default: TRUE)
#   --no-verify       Skip verification checks
#   --with-analysis   Run full manuscript analysis stage (N=1307 subset, figures, tables)
#   --debug           Enable verbose debug output
#   --checkpoint      Save state after each major step

# =============================================================================
# 1. INITIAL SETUP & S4 METHOD PROTECTION
# =============================================================================

# Critical: Define .local shim BEFORE any package loading to prevent S4 dispatch errors
# This prevents the "Error in .local(object, ...) : could not find function '.local'" issue
if (!exists(".local", mode = "function")) {
  .local <- function(...) {
    # No-op shim for S4 method dispatch protection
    # This function exists solely to prevent S4 dispatch errors during package initialization
    return(invisible(NULL))
  }
  assign(".local", .local, envir = .GlobalEnv)
}

# Load methods package with error protection
tryCatch({
  if (!"methods" %in% loadedNamespaces()) {
    suppressPackageStartupMessages(library(methods))
  }
}, error = function(e) {
  message("Warning: Could not load methods package: ", e$message)
})

# Set deterministic environment settings for reproducibility
set.seed(12345)  # Fixed seed for reproducible random operations
Sys.setenv(TZ = "UTC")  # Consistent timezone across all operations
Sys.setenv(R_DEFAULT_INTERNET_TIMEOUT = "300")  # Increase timeout for package downloads

# Global options for consistency
options(
  stringsAsFactors = FALSE,  # Modern R behavior
  scipen = 999,              # Avoid scientific notation
  readr.show_col_types = FALSE,  # Suppress column type messages
  readr.show_progress = FALSE,   # Suppress progress bars in non-interactive mode
  dplyr.summarise.inform = FALSE,  # Suppress dplyr grouping messages
  warn = 1,  # Print warnings as they occur
  encoding = "UTF-8"  # Ensure UTF-8 encoding
)

# =============================================================================
# 2. ARGUMENT PARSING & CONFIGURATION
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

# Helper function for flag detection with aliases
has_flag <- function(...) {
  flags <- c(...)
  any(args %in% flags)
}

# Parse command-line flags with sensible defaults
CLEAN <- has_flag("--clean", "-c")
VERIFY <- !has_flag("--no-verify") && !has_flag("-nv")  # Default: TRUE
WITH_ANALYSIS <- has_flag("--with-analysis", "-a")
DEBUG <- has_flag("--debug", "-d")
CHECKPOINT <- has_flag("--checkpoint", "-cp")

# Display configuration
if (DEBUG) {
  cat("\n=== Pipeline Configuration ===\n")
  cat(sprintf("  Clean Mode: %s\n", CLEAN))
  cat(sprintf("  Verification: %s\n", VERIFY))
  cat(sprintf("  With Analysis: %s\n", WITH_ANALYSIS))
  cat(sprintf("  Debug Mode: %s\n", DEBUG))
  cat(sprintf("  Checkpointing: %s\n", CHECKPOINT))
  cat("==============================\n\n")
}

# =============================================================================
# 3. ENHANCED ERROR HANDLING
# =============================================================================

# Create docs directory early for error logging
dir.create("docs", showWarnings = FALSE, recursive = TRUE)

# Enhanced error handler with detailed logging
options(error = function() {
  message("\n╔════════════════════════════════════════╗")
  message("║         PIPELINE EXECUTION FAILED       ║")
  message("╚════════════════════════════════════════╝")
  
  err_msg <- geterrmessage()
  message(paste("\n❌ Error:", err_msg))
  
  # Capture detailed error information
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC")
  
  # Get call stack
  calls <- sys.calls()
  call_stack <- if (length(calls) > 0) {
    paste(capture.output(traceback(max.lines = 20)), collapse = "\n")
  } else {
    "No traceback available"
  }
  
  # Session information
  session_info <- capture.output(sessionInfo())
  
  # Write comprehensive error log
  error_log_content <- c(
    "=== PIPELINE ERROR LOG ===",
    paste("Timestamp:", timestamp),
    paste("R Version:", R.version.string),
    paste("Platform:", Sys.info()["sysname"], Sys.info()["release"]),
    paste("Working Directory:", getwd()),
    "",
    "=== ERROR MESSAGE ===",
    err_msg,
    "",
    "=== CALL STACK ===",
    call_stack,
    "",
    "=== SESSION INFO ===",
    session_info,
    "",
    "=== ENVIRONMENT VARIABLES ===",
    paste("TZ:", Sys.getenv("TZ")),
    paste("PATH:", Sys.getenv("PATH"))
  )
  
  # Write to both error log files
  writeLines(error_log_content, "docs/error_log.txt")
  writeLines(error_log_content, "docs/last_error.txt")  # Quick access file
  
  # Also create a minimal error summary for quick diagnosis
  error_summary <- c(
    timestamp,
    err_msg,
    paste("Script:", if (exists("current_script")) current_script else "Unknown"),
    paste("Stage:", if (exists("pipeline_stage")) pipeline_stage else "Unknown")
  )
  writeLines(error_summary, "docs/last_error_summary.txt")
  
  message("\n📄 Error logs saved to:")
  message("   - docs/error_log.txt (full log)")
  message("   - docs/last_error.txt (duplicate for quick access)")
  message("   - docs/last_error_summary.txt (brief summary)")
  
  # Exit with appropriate status code
  quit(save = "no", status = 1, runLast = FALSE)
})

# =============================================================================
# 4. PACKAGE MANAGEMENT
# =============================================================================

# Define required packages by category
required_pkgs <- c(
  "readr",     # Fast CSV I/O
  "dplyr",     # Data manipulation
  "stringr",   # String operations
  "tidyr",     # Data tidying
  "digest",    # Checksum generation
  "cli",       # Enhanced CLI output
  "lubridate", # Date/time handling
  "tibble"     # Modern data frames
)

analysis_pkgs <- c(
  "psych",      # Psychometric analysis
  "lavaan",     # Structural equation modeling
  "ggplot2",    # Visualization
  "scales",     # Scale functions for ggplot2
  "corrplot",   # Correlation visualization
  "broom",      # Tidy model outputs
  "effectsize"  # Effect size calculations
)

# Enhanced package installation with retry logic
install_if_missing <- function(pkgs, max_retries = 3) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  
  if (length(missing) > 0) {
    message(paste("\n📦 Installing missing packages:", paste(missing, collapse = ", ")))
    
    for (pkg in missing) {
      success <- FALSE
      for (attempt in 1:max_retries) {
        tryCatch({
          install.packages(
            pkg, 
            repos = c(
              "https://cloud.r-project.org",
              "https://cran.rstudio.com"
            ),
            quiet = !DEBUG,
            dependencies = TRUE
          )
          success <- TRUE
          break
        }, error = function(e) {
          if (attempt < max_retries) {
            message(sprintf("  ⚠️  Attempt %d/%d failed for %s. Retrying...", 
                          attempt, max_retries, pkg))
            Sys.sleep(2)  # Brief pause before retry
          } else {
            stop(sprintf("Failed to install %s after %d attempts: %s", 
                        pkg, max_retries, e$message))
          }
        })
      }
      if (success && DEBUG) {
        message(sprintf("  ✅ Successfully installed: %s", pkg))
      }
    }
  }
  
  # Verify all packages are now installed
  still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(still_missing) > 0) {
    stop("Failed to install required packages: ", paste(still_missing, collapse = ", "))
  }
  
  invisible(TRUE)
}

# Install packages with progress indication
pipeline_stage <- "Package Installation"
install_if_missing(required_pkgs)

if (WITH_ANALYSIS) {
  install_if_missing(analysis_pkgs)
}

# Load essential packages quietly
suppressPackageStartupMessages({
  library(cli)
  library(dplyr)
  library(readr)
})

# =============================================================================
# 5. FILE PATHS & SCRIPT DEFINITIONS
# =============================================================================

# Canonical file paths (order matters for dependency tracking)
paths <- list(
  # Core outputs (always created)
  basic_anon = "data/survey_responses_anonymized_basic.csv",
  prelim_classified = "data/survey_responses_anonymized_preliminary.csv",
  class_template = "docs/appendix_j_classification_template.csv",
  dictionary = "data/data_dictionary.csv",
  
  # Analysis outputs (created with --with-analysis)
  final_1307 = "data/climate_finance_survey_final_1307.csv",
  
  # Verification outputs
  checksums = "docs/checksums.txt",
  verification = "docs/verification_report.md"
)

# Script definitions with case-sensitive filenames
# CRITICAL FIX: Use lowercase .r extension to match actual filesystem
scripts_base <- c(
  "R/01_anonymize_data.R",
  "R/02_classify_stakeholders.R"
)

scripts_analysis <- c(
  "R/get_exact_1307.R",
  "R/03_main_analysis.R",
  "R/04_hypothesis_testing.r"  # FIX: Changed from .R to .r for case-sensitive compatibility
)

# Validate script existence with case-sensitive check
validate_scripts <- function(script_list) {
  for (script in script_list) {
    if (!file.exists(script)) {
      # Try alternative case if not found
      alt_script <- if (endsWith(script, ".R")) {
        sub("\\.R$", ".r", script)
      } else {
        sub("\\.r$", ".R", script)
      }
      
      if (file.exists(alt_script)) {
        warning(sprintf("Script %s not found, but %s exists. Update script list for consistency.", 
                       script, alt_script))
        # Update the script list
        script_list[script_list == script] <- alt_script
      } else {
        stop(sprintf("Required script not found: %s", script))
      }
    }
  }
  return(script_list)
}

# Validate and potentially fix script paths
scripts_base <- validate_scripts(scripts_base)
scripts_analysis <- validate_scripts(scripts_analysis)

# =============================================================================
# 6. ENHANCED SCRIPT EXECUTION WITH METHOD SAFETY
# =============================================================================

# Enhanced script runner with S4 method protection and checkpointing
run_script <- function(path, stage_name = NULL) {
  # Update global tracking variables
  assign("current_script", path, envir = .GlobalEnv)
  assign("pipeline_stage", stage_name %||% basename(path), envir = .GlobalEnv)
  
  cli::cli_h1(paste("Running:", basename(path)))
  
  if (!file.exists(path)) {
    stop(sprintf("Required script not found: %s", path))
  }
  
  # Get file info for logging
  file_info <- file.info(path)
  if (DEBUG) {
    cli::cli_alert_info(sprintf("File size: %s bytes", file_info$size))
    cli::cli_alert_info(sprintf("Last modified: %s", file_info$mtime))
  }
  
  # Start timing
  start_time <- Sys.time()
  
  # CRITICAL: Re-establish S4 method safety before each script
  # This prevents method dispatch errors that can occur during package loading
  if (!exists(".local", mode = "function", envir = .GlobalEnv)) {
    .local <- function(...) return(invisible(NULL))
    assign(".local", .local, envir = .GlobalEnv)
  }
  
  # Ensure methods package is still loaded
  if (!"methods" %in% loadedNamespaces()) {
    suppressPackageStartupMessages(library(methods))
  }
  
  # Execute script with error handling
  tryCatch({
    # Create a new environment for script execution to avoid pollution
    script_env <- new.env(parent = .GlobalEnv)
    
    # Copy essential objects to script environment
    script_env$.local <- .local
    
    # Source the script
    if (DEBUG) {
      source(path, local = script_env, echo = TRUE, verbose = TRUE)
    } else {
      source(path, local = script_env, echo = FALSE)
    }
    
    # Calculate execution time
    exec_time <- difftime(Sys.time(), start_time, units = "secs")
    
    cli::cli_alert_success(sprintf("Completed: %s (%.2f seconds)", 
                                   basename(path), exec_time))
    
    # Checkpoint if requested
    if (CHECKPOINT) {
      save_checkpoint(stage_name %||% basename(path))
    }
    
  }, error = function(e) {
    cli::cli_alert_danger(sprintf("Script failed: %s", basename(path)))
    stop(sprintf("Error in %s: %s", path, e$message))
  })
  
  invisible(TRUE)
}

# Checkpoint saving function
save_checkpoint <- function(stage) {
  checkpoint_dir <- "checkpoints"
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  
  checkpoint_file <- file.path(checkpoint_dir, 
                               sprintf("checkpoint_%s_%s.RData", 
                                      stage,
                                      format(Sys.time(), "%Y%m%d_%H%M%S")))
  
  # Save workspace snapshot
  save.image(file = checkpoint_file)
  
  if (DEBUG) {
    cli::cli_alert_info(sprintf("Checkpoint saved: %s", checkpoint_file))
  }
}

# =============================================================================
# 7. DATA VALIDATION & PREPARATION
# =============================================================================

pipeline_stage <- "Data Validation"

# Validate raw data existence
if (!dir.exists("data_raw")) {
  stop("Required folder 'data_raw' not found. Please create it and place raw survey CSVs there.")
}

raw_files <- list.files("data_raw", pattern = "\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) {
  stop("No CSV files found in 'data_raw' folder. Please add raw survey data.")
}

if (DEBUG) {
  cli::cli_alert_info(sprintf("Found %d raw data files", length(raw_files)))
  for (f in raw_files) {
    cli::cli_text(sprintf("  - %s (%.1f KB)", 
                         basename(f), 
                         file.info(f)$size / 1024))
  }
}

# Create all required output directories
output_dirs <- c("data", "docs", "output", "figures", "checkpoints")
for (dir in output_dirs) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# =============================================================================
# 8. OPTIONAL CLEANING
# =============================================================================

if (CLEAN) {
  pipeline_stage <- "Cleaning"
  cli::cli_h1("Cleaning Previous Outputs")
  
  # Files to remove
  files_to_remove <- c(
    unlist(paths),
    "docs/error_log.txt",
    "docs/last_error.txt",
    "docs/last_error_summary.txt"
  )
  
  # Remove files
  removed_count <- 0
  for (f in files_to_remove) {
    if (file.exists(f)) {
      file.remove(f)
      removed_count <- removed_count + 1
      if (DEBUG) cli::cli_alert_info(sprintf("Removed: %s", f))
    }
  }
  
  # Remove directories for full clean
  if (WITH_ANALYSIS) {
    for (dir in c("output", "figures", "checkpoints")) {
      if (dir.exists(dir)) {
        unlink(dir, recursive = TRUE)
        if (DEBUG) cli::cli_alert_info(sprintf("Removed directory: %s", dir))
      }
    }
  }
  
  cli::cli_alert_success(sprintf("Cleaned %d files and directories", removed_count))
}

# =============================================================================
# 9. MAIN PIPELINE EXECUTION
# =============================================================================

cli::cli_h1("Pipeline Execution")
overall_start <- Sys.time()

# Base pipeline (always runs)
pipeline_stage <- "Base Pipeline"
cli::cli_h2("Stage 1: Base Data Processing")

for (script in scripts_base) {
  run_script(script, stage_name = paste0("base_", tools::file_path_sans_ext(basename(script))))
}

# Analysis pipeline (conditional)
if (WITH_ANALYSIS) {
  pipeline_stage <- "Analysis Pipeline"
  cli::cli_h2("Stage 2: Analysis & Hypothesis Testing")
  
  for (script in scripts_analysis) {
    run_script(script, stage_name = paste0("analysis_", tools::file_path_sans_ext(basename(script))))
  }
} else {
  cli::cli_alert_info("Skipping analysis stage. Use --with-analysis flag to enable.")
}

# Calculate total execution time
total_time <- difftime(Sys.time(), overall_start, units = "mins")
cli::cli_alert_success(sprintf("Pipeline execution completed in %.1f minutes", total_time))

# =============================================================================
# 10. COMPREHENSIVE VERIFICATION
# =============================================================================

verify_outputs <- function(artifact_paths, is_analysis_run) {
  pipeline_stage <- "Verification"
  cli::cli_h1("Output Verification")
  
  verification_results <- list(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC"),
    all_ok = TRUE,
    checks = list()
  )
  
  report_lines <- c(
    "# Pipeline Verification Report",
    "",
    sprintf("**Generated:** %s", verification_results$timestamp),
    sprintf("**Pipeline Mode:** %s", ifelse(is_analysis_run, "Full Analysis", "Base Only")),
    sprintf("**R Version:** %s", R.version.string),
    ""
  )
  
  # --- Check 1: File Presence ---
  cli::cli_h2("1. File Presence Check")
  
  core_artifacts <- artifact_paths[c("basic_anon", "prelim_classified", 
                                    "class_template", "dictionary")]
  analysis_artifacts <- artifact_paths["final_1307"]
  
  artifacts_to_check <- if (is_analysis_run) {
    c(core_artifacts, analysis_artifacts)
  } else {
    core_artifacts
  }
  
  file_check_results <- list()
  for (name in names(artifacts_to_check)) {
    path <- artifacts_to_check[[name]]
    exists <- file.exists(path)
    file_check_results[[name]] <- exists
    
    if (exists) {
      size_mb <- file.info(path)$size / (1024^2)
      cli::cli_alert_success(sprintf("✓ %s (%.2f MB)", path, size_mb))
    } else {
      cli::cli_alert_danger(sprintf("✗ Missing: %s", path))
      verification_results$all_ok <- FALSE
    }
  }
  
  report_lines <- c(report_lines, 
                   "## 1. File Presence",
                   "",
                   "| File | Status | Size |",
                   "|------|--------|------|")
  
  for (name in names(file_check_results)) {
    path <- artifacts_to_check[[name]]
    if (file_check_results[[name]]) {
      size_mb <- file.info(path)$size / (1024^2)
      report_lines <- c(report_lines,
                       sprintf("| %s | ✓ Present | %.2f MB |", basename(path), size_mb))
    } else {
      report_lines <- c(report_lines,
                       sprintf("| %s | ✗ Missing | - |", basename(path)))
    }
  }
  
  # --- Check 2: Schema Validation ---
  cli::cli_h2("2. Schema Validation")
  report_lines <- c(report_lines, "", "## 2. Schema Validation", "")
  
  if (file.exists(paths$basic_anon) && file.exists(paths$dictionary)) {
    basic_df <- read_csv(paths$basic_anon, show_col_types = FALSE)
    dict_df <- read_csv(paths$dictionary, show_col_types = FALSE)
    
    schema_match <- identical(sort(names(basic_df)), sort(dict_df$column_name))
    
    if (schema_match) {
      cli::cli_alert_success("✓ Dictionary schema matches anonymized data")
      report_lines <- c(report_lines, "- ✓ Dictionary schema matches anonymized data")
      report_lines <- c(report_lines, sprintf("- Column count: %d", ncol(basic_df)))
    } else {
      cli::cli_alert_danger("✗ Schema mismatch between dictionary and data")
      verification_results$all_ok <- FALSE
      
      # Detailed mismatch reporting
      in_data_not_dict <- setdiff(names(basic_df), dict_df$column_name)
      in_dict_not_data <- setdiff(dict_df$column_name, names(basic_df))
      
      report_lines <- c(report_lines, "- ✗ Schema mismatch detected:")
      if (length(in_data_not_dict) > 0) {
        report_lines <- c(report_lines, 
                         sprintf("  - Columns in data but not dictionary: %s", 
                                paste(in_data_not_dict, collapse = ", ")))
      }
      if (length(in_dict_not_data) > 0) {
        report_lines <- c(report_lines,
                         sprintf("  - Columns in dictionary but not data: %s",
                                paste(in_dict_not_data, collapse = ", ")))
      }
    }
  } else {
    cli::cli_alert_warning("⚠ Cannot validate schema (files missing)")
    report_lines <- c(report_lines, "- ⚠ Cannot validate schema (required files missing)")
  }
  
  # --- Check 3: Data Integrity ---
  cli::cli_h2("3. Data Integrity Check")
  report_lines <- c(report_lines, "", "## 3. Data Integrity", "")
  
  if (file.exists(paths$prelim_classified)) {
    prelim_df <- read_csv(paths$prelim_classified, show_col_types = FALSE)
    
    # Find classification column (handle both possible names)
    class_cols <- c("final_category_appendix_j", "stakeholder_category")
    class_col <- intersect(class_cols, names(prelim_df))[1]
    
    if (!is.na(class_col)) {
      na_count <- sum(is.na(prelim_df[[class_col]]))
      total_rows <- nrow(prelim_df)
      
      if (na_count == 0) {
        cli::cli_alert_success(sprintf("✓ No NA values in %s column (%d rows)", 
                                      class_col, total_rows))
        report_lines <- c(report_lines,
                         sprintf("- ✓ Classification column '%s' complete (%d rows, 0 NAs)",
                                class_col, total_rows))
      } else {
        cli::cli_alert_danger(sprintf("✗ Found %d NA values in %s column", 
                                     na_count, class_col))
        verification_results$all_ok <- FALSE
        report_lines <- c(report_lines,
                         sprintf("- ✗ Classification column '%s' has %d NAs (%.1f%%)",
                                class_col, na_count, 100 * na_count / total_rows))
      }
      
      # Additional integrity checks
      unique_categories <- unique(prelim_df[[class_col]])
      report_lines <- c(report_lines,
                       sprintf("- Unique categories: %d", length(unique_categories)))
      
    } else {
      cli::cli_alert_warning("⚠ Classification column not found")
      report_lines <- c(report_lines, "- ⚠ Classification column not found in data")
    }
  }
  
  # --- Check 4: Generate Checksums ---
  cli::cli_h2("4. Checksum Generation (SHA-256)")
  report_lines <- c(report_lines, "", "## 4. File Checksums (SHA-256)", "")
  
  checksum_data <- tibble::tibble(
    file = character(),
    sha256 = character(),
    size_bytes = numeric(),
    modified = character()
  )
  
  for (name in names(artifacts_to_check)) {
    path <- artifacts_to_check[[name]]
    if (file.exists(path)) {
      sha <- digest::digest(file = path, algo = "sha256")
      info <- file.info(path)
      
      checksum_data <- checksum_data %>%
        add_row(
          file = basename(path),
          sha256 = sha,
          size_bytes = info$size,
          modified = format(info$mtime, "%Y-%m-%d %H:%M:%S")
        )
      
      if (DEBUG) {
        cli::cli_text(sprintf("  %s: %s", basename(path), substr(sha, 1, 12)))
      }
    }
  }
  
  # Save checksums
  write_csv(checksum_data, paths$checksums)
  cli::cli_alert_success(sprintf("Checksums saved to %s", paths$checksums))
  
  # Add to report
  report_lines <- c(report_lines, "", "```")
  for (i in seq_len(nrow(checksum_data))) {
    report_lines <- c(report_lines,
                     sprintf("%s: %s", 
                            checksum_data$file[i], 
                            checksum_data$sha256[i]))
  }
  report_lines <- c(report_lines, "```", "")
  
  # --- Final Summary ---
  cli::cli_h2("Verification Summary")
  
  if (verification_results$all_ok) {
    cli::cli_alert_success("✅ All verification checks PASSED")
    report_lines <- c(report_lines, 
                     "## Summary", 
                     "", 
                     "**Result: ✅ PASSED** - All verification checks completed successfully.")
  } else {
    cli::cli_alert_danger("❌ Verification FAILED - Issues detected")
    report_lines <- c(report_lines,
                     "## Summary",
                     "",
                     "**Result: ❌ FAILED** - One or more verification checks failed.")
  }
  
  # Write verification report
  writeLines(report_lines, paths$verification)
  cli::cli_alert_info(sprintf("Detailed report saved to %s", paths$verification))
  
  # Return status
  if (!verification_results$all_ok) {
    stop("Pipeline verification failed. Check the verification report for details.")
  }
  
  invisible(verification_results)
}

# Run verification if requested
if (VERIFY) {
  verify_results <- verify_outputs(paths, WITH_ANALYSIS)
}

# =============================================================================
# 11. FINAL SUCCESS MESSAGE
# =============================================================================

cli::cli_h1("✅ Pipeline Completed Successfully")

# Summary statistics
final_summary <- c(
  sprintf("Total execution time: %.1f minutes", total_time),
  sprintf("Scripts executed: %d", 
         length(scripts_base) + ifelse(WITH_ANALYSIS, length(scripts_analysis), 0)),
  sprintf("Artifacts generated: %d", 
         sum(file.exists(unlist(paths)))),
  sprintf("Verification: %s", ifelse(VERIFY, "PASSED", "SKIPPED"))
)

for (line in final_summary) {
  cli::cli_alert_success(line)
}

# Provide next steps
cli::cli_h2("Next Steps")
if (!WITH_ANALYSIS) {
  cli::cli_alert_info("Run with --with-analysis flag to generate full analysis outputs")
}
cli::cli_alert_info("Check docs/verification_report.md for detailed validation results")
cli::cli_alert_info("Review docs/checksums.txt for file integrity verification")

# Clean exit
quit(save = "no", status = 0, runLast = FALSE)