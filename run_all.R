#!/usr/bin/env Rscript
# run_all.R - Enhanced Pipeline Runner v3.0
# Purpose: Robust, deterministic end-to-end pipeline runner with comprehensive error handling
# Description:
#   This script executes the entire data processing and analysis pipeline with:
#   - S4 method dispatch protection (.local shim)
#   - Enhanced error logging and recovery
#   - Per-script method safety enforcement
#   - Centralized configuration management
#   - Delegation to dedicated quality checks
#
# NAMING CONVENTION: All R scripts must use uppercase .R extension for consistency
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
# 2. LOAD CENTRAL CONFIGURATION
# =============================================================================

# Source the central configuration file EARLY to get PATHS and other settings
config_file <- "R/00_config.R"
if (!file.exists(config_file)) {
  stop(sprintf("Central configuration file not found: %s\nThis file is required for pipeline execution.", config_file))
}

# Source configuration with error handling
tryCatch({
  source(config_file, local = FALSE)
  message("✓ Central configuration loaded from ", config_file)
}, error = function(e) {
  stop(sprintf("Failed to load central configuration: %s", e$message))
})

# Verify PATHS was loaded
if (!exists("PATHS") || !is.list(PATHS)) {
  stop("PATHS configuration not found after sourcing ", config_file)
}

# =============================================================================
# 3. ARGUMENT PARSING & CONFIGURATION
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
# 4. ENHANCED ERROR HANDLING
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
    paste("PATH:", Sys.getenv("PATH")),
    "",
    "=== PIPELINE STAGE ===",
    paste("Current Stage:", if (exists("pipeline_stage")) pipeline_stage else "Unknown"),
    paste("Current Script:", if (exists("current_script")) current_script else "Unknown")
  )
  
  # Use paths from central config for error logs
  error_log_path <- if (!is.null(PATHS$error_log)) PATHS$error_log else "docs/error_log.txt"
  
  # Write to error log files
  writeLines(error_log_content, error_log_path)
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
  message("   - ", error_log_path, " (full log)")
  message("   - docs/last_error.txt (duplicate for quick access)")
  message("   - docs/last_error_summary.txt (brief summary)")
  
  # Exit with appropriate status code
  quit(save = "no", status = 1, runLast = FALSE)
})

# =============================================================================
# 5. PACKAGE MANAGEMENT
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
# 6. SCRIPT DEFINITIONS
# =============================================================================

# Script definitions - all scripts should use consistent .R extension
scripts_base <- c(
  "R/01_anonymize_data.R",
  "R/02_classify_stakeholders.R"
)

scripts_analysis <- c(
  "R/get_exact_1307.R",
  "R/03_main_analysis.R",
  "R/04_hypothesis_testing.R"  # Enforcing consistent .R extension
)

# Simple validation - fail clearly if files don't exist
validate_scripts <- function(script_list, stage_name) {
  missing_scripts <- character()
  
  for (script in script_list) {
    if (!file.exists(script)) {
      missing_scripts <- c(missing_scripts, script)
    }
  }
  
  if (length(missing_scripts) > 0) {
    stop(sprintf(
      "Required %s script(s) not found:\n  %s\n\nPlease ensure all scripts use consistent .R extension and exist in the R/ directory.",
      stage_name,
      paste(missing_scripts, collapse = "\n  ")
    ))
  }
  
  invisible(TRUE)
}

# Validate script existence
validate_scripts(scripts_base, "base pipeline")
if (WITH_ANALYSIS) {
  validate_scripts(scripts_analysis, "analysis pipeline")
}

# =============================================================================
# 7. ENHANCED SCRIPT EXECUTION WITH METHOD SAFETY
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
    script_env$PATHS <- PATHS  # Pass central configuration
    script_env$STANDARD_COLUMNS <- STANDARD_COLUMNS
    script_env$QUALITY_PARAMS <- QUALITY_PARAMS
    
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
# 8. DATA VALIDATION & PREPARATION
# =============================================================================

pipeline_stage <- "Data Validation"

# Use validate_stage function from config if available
if (exists("validate_stage", mode = "function")) {
  # Validate we can run the base pipeline
  tryCatch({
    validate_stage("anonymize")  # First stage has no requirements
  }, error = function(e) {
    # This is expected for first run
    if (DEBUG) message("Note: First run detected (no prior outputs)")
  })
}

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
# 9. OPTIONAL CLEANING
# =============================================================================

if (CLEAN) {
  pipeline_stage <- "Cleaning"
  cli::cli_h1("Cleaning Previous Outputs")
  
  # Use paths from central configuration
  files_to_remove <- c(
    # Core outputs
    PATHS$basic_anon,
    PATHS$preliminary_classified,
    PATHS$classification_template,
    PATHS$dictionary,
    PATHS$final_1307,
    # Verification outputs
    PATHS$checksums,
    PATHS$verification_report,
    PATHS$error_log,
    # Additional files
    "docs/last_error.txt",
    "docs/last_error_summary.txt"
  )
  
  # Remove NULL paths
  files_to_remove <- files_to_remove[!sapply(files_to_remove, is.null)]
  
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
# 10. CHECK FOR DEPRECATED FILES
# =============================================================================

# Use check_deprecated from config if available
if (exists("check_deprecated", mode = "function")) {
  deprecated <- check_deprecated(verbose = FALSE)
  if (length(deprecated) > 0) {
    cli::cli_alert_warning(sprintf("Found %d deprecated file(s). Consider removing:", length(deprecated)))
    for (f in deprecated) {
      cli::cli_text(sprintf("  - %s", f))
    }
  }
}

# =============================================================================
# 11. MAIN PIPELINE EXECUTION
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
# 12. VERIFICATION USING QUALITY CHECKS SCRIPT
# =============================================================================

if (VERIFY) {
  pipeline_stage <- "Verification"
  cli::cli_h1("Running Quality Verification")
  
  quality_script <- "R/99_quality_checks.R"
  
  if (file.exists(quality_script)) {
    # Execute the quality checks script
    cli::cli_alert_info("Delegating to quality checks script...")
    
    tryCatch({
      # Create environment for quality check execution
      qa_env <- new.env(parent = .GlobalEnv)
      
      # Pass configuration to QA environment
      qa_env$PATHS <- PATHS
      qa_env$STANDARD_COLUMNS <- STANDARD_COLUMNS
      qa_env$QUALITY_PARAMS <- QUALITY_PARAMS
      qa_env$DEPRECATED_PATHS <- if (exists("DEPRECATED_PATHS")) DEPRECATED_PATHS else character(0)
      qa_env$PRIVACY_COLUMNS <- if (exists("PRIVACY_COLUMNS")) PRIVACY_COLUMNS else character(0)
      
      # Source quality checks
      source(quality_script, local = qa_env)
      
      # Run the quality checks function
      if (exists("run_quality_checks", envir = qa_env, mode = "function")) {
        qa_results <- qa_env$run_quality_checks(verbose = !DEBUG, save_report = TRUE)
        
        # Display summary
        if (qa_results$overall_status == "PASSED") {
          cli::cli_alert_success("✅ All quality checks PASSED")
        } else if (qa_results$overall_status == "PARTIAL") {
          cli::cli_alert_warning("⚠️ Quality checks PARTIALLY passed")
          cli::cli_text("See ", PATHS$quality_assurance, " for details")
        } else {
          cli::cli_alert_danger("❌ Quality checks FAILED")
          stop("Pipeline verification failed. Check ", PATHS$quality_assurance, " for details.")
        }
      } else {
        cli::cli_alert_warning("Quality check function not found in script")
      }
      
    }, error = function(e) {
      cli::cli_alert_danger("Quality verification failed")
      stop(sprintf("Error running quality checks: %s", e$message))
    })
    
  } else {
    cli::cli_alert_warning(sprintf("Quality checks script not found: %s", quality_script))
    cli::cli_alert_info("Running basic file existence checks instead...")
    
    # Fallback: Basic file existence check using central config
    core_files <- c(PATHS$basic_anon, PATHS$preliminary_classified, 
                   PATHS$classification_template, PATHS$dictionary)
    
    if (WITH_ANALYSIS) {
      core_files <- c(core_files, PATHS$final_1307)
    }
    
    missing_files <- core_files[!file.exists(core_files)]
    
    if (length(missing_files) > 0) {
      cli::cli_alert_danger(sprintf("Missing %d expected output file(s):", length(missing_files)))
      for (f in missing_files) {
        cli::cli_text(sprintf("  - %s", f))
      }
      stop("Pipeline verification failed due to missing outputs.")
    } else {
      cli::cli_alert_success("Basic file verification passed")
    }
  }
} else {
  cli::cli_alert_info("Verification skipped (use --verify to enable)")
}

# =============================================================================
# 13. FINAL SUCCESS MESSAGE
# =============================================================================

cli::cli_h1("✅ Pipeline Completed Successfully")

# Summary statistics
final_summary <- c(
  sprintf("Total execution time: %.1f minutes", total_time),
  sprintf("Scripts executed: %d", 
         length(scripts_base) + ifelse(WITH_ANALYSIS, length(scripts_analysis), 0)),
  sprintf("Verification: %s", 
         ifelse(VERIFY, 
                ifelse(exists("qa_results"), qa_results$overall_status, "BASIC"), 
                "SKIPPED"))
)

for (line in final_summary) {
  cli::cli_alert_success(line)
}

# Count actual artifacts created
artifact_count <- 0
for (path_name in names(PATHS)) {
  if (!is.null(PATHS[[path_name]]) && file.exists(PATHS[[path_name]])) {
    artifact_count <- artifact_count + 1
  }
}
cli::cli_alert_success(sprintf("Artifacts generated: %d", artifact_count))

# Provide next steps
cli::cli_h2("Next Steps")
if (!WITH_ANALYSIS) {
  cli::cli_alert_info("Run with --with-analysis flag to generate full analysis outputs")
}

# Point to reports using central config paths
if (!is.null(PATHS$verification_report) && file.exists(PATHS$verification_report)) {
  cli::cli_alert_info(paste("Check", PATHS$verification_report, "for detailed validation results"))
}
if (!is.null(PATHS$checksums) && file.exists(PATHS$checksums)) {
  cli::cli_alert_info(paste("Review", PATHS$checksums, "for file integrity verification"))
}
if (!is.null(PATHS$quality_assurance) && file.exists(PATHS$quality_assurance)) {
  cli::cli_alert_info(paste("See", PATHS$quality_assurance, "for comprehensive quality report"))
}

# Clean exit
quit(save = "no", status = 0, runLast = FALSE)