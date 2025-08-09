#!/usr/bin/env Rscript
# run_all.R - Pipeline Runner v4.0
# Purpose: Execute the complete data processing and analysis pipeline
# Description: Orchestrates all pipeline stages with proper error handling and configuration
# Author: Richard McKinney
# Date: 2025-08-08
#
# Flags:
#   --clean           Remove all outputs before running
#   --verify          Run verification checks (default: TRUE)
#   --no-verify       Skip verification checks
#   --with-analysis   Run full manuscript analysis stage (N=1307 subset, figures, tables)
#   --debug           Enable verbose debug output
#   --checkpoint      Save state after each major step
#   --help            Show help message

# =============================================================================
# 1. ENVIRONMENT SETUP & S4 METHOD PROTECTION
# =============================================================================

# Critical: Define .local shim BEFORE any package loading to prevent S4 dispatch errors
if (!exists(".local", mode = "function")) {
  .local <- function(...) {
    # No-op shim for S4 method dispatch protection
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

# Set deterministic environment for reproducibility
set.seed(12345)
Sys.setenv(TZ = "UTC")
Sys.setenv(R_DEFAULT_INTERNET_TIMEOUT = "300")

# Global options for consistency
options(
  stringsAsFactors = FALSE,
  scipen = 999,
  readr.show_col_types = FALSE,
  readr.show_progress = FALSE,
  dplyr.summarise.inform = FALSE,
  warn = 1,
  encoding = "UTF-8"
)

# Define useful operator
`%||%` <- function(a, b) if (is.null(a)) b else a

# =============================================================================
# 2. ARGUMENT PARSING
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)

# Parse command-line flags
HELP <- "--help" %in% args || "-h" %in% args
CLEAN <- "--clean" %in% args || "-c" %in% args
VERIFY <- !("--no-verify" %in% args || "-nv" %in% args)
WITH_ANALYSIS <- "--with-analysis" %in% args || "-a" %in% args
DEBUG <- "--debug" %in% args || "-d" %in% args
CHECKPOINT <- "--checkpoint" %in% args || "-cp" %in% args

# Show help if requested
if (HELP) {
  cat("
Climate Finance Research Pipeline Runner

Usage: Rscript run_all.R [OPTIONS]

Options:
  --clean, -c         Remove all outputs before running
  --with-analysis, -a Include full analysis stage (figures & hypothesis testing)
  --no-verify, -nv    Skip verification checks
  --debug, -d         Enable verbose debug output
  --checkpoint, -cp   Save state after each major step
  --help, -h          Show this help message

Examples:
  Rscript run_all.R                    # Run base pipeline with verification
  Rscript run_all.R --clean            # Clean and run base pipeline
  Rscript run_all.R --with-analysis    # Run full pipeline including analysis
  Rscript run_all.R -c -a --checkpoint # Clean and run full pipeline with checkpoints

Default behavior: Runs base pipeline with verification, no cleaning, no analysis.
")
  quit(save = "no", status = 0)
}

# =============================================================================
# 3. ENHANCED ERROR HANDLING
# =============================================================================

# Create directories early for error logging
dir.create("docs", showWarnings = FALSE, recursive = TRUE)
dir.create("logs", showWarnings = FALSE, recursive = TRUE)

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
    "=== PIPELINE STAGE ===",
    paste("Current Stage:", if (exists("pipeline_stage")) pipeline_stage else "Unknown"),
    paste("Current Script:", if (exists("current_script")) current_script else "Unknown")
  )
  
  # Write to multiple log files
  error_log_path <- "logs/error_log.txt"
  writeLines(error_log_content, error_log_path)
  writeLines(error_log_content, "docs/last_error.txt")
  
  # Minimal error summary
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
  
  quit(save = "no", status = 1, runLast = FALSE)
})

# =============================================================================
# 4. PACKAGE MANAGEMENT WITH RETRY LOGIC
# =============================================================================

# Define required packages
required_pkgs <- c(
  "readr", "dplyr", "stringr", "tidyr", "digest",
  "cli", "lubridate", "tibble", "ggplot2", "purrr"
)

analysis_pkgs <- c(
  "psych", "lavaan", "scales", "corrplot",
  "broom", "effectsize", "car", "nnet", "MASS"
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
            Sys.sleep(2)
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
  
  # Verify installation
  still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(still_missing) > 0) {
    stop("Failed to install required packages: ", paste(still_missing, collapse = ", "))
  }
  
  invisible(TRUE)
}

# Install packages
pipeline_stage <- "Package Installation"
install_if_missing(required_pkgs)

if (WITH_ANALYSIS) {
  install_if_missing(analysis_pkgs)
}

# Load essential packages
suppressPackageStartupMessages({
  library(cli)
  library(dplyr)
  library(readr)
})

# =============================================================================
# 5. LOAD CENTRAL CONFIGURATION
# =============================================================================

cli::cli_h1("Climate Finance Research Pipeline")

# Source configuration files
config_file <- "R/00_config.R"
appendix_config <- "R/appendix_j_config.R"

if (!file.exists(config_file)) {
  stop(sprintf("Central configuration file not found: %s", config_file))
}

tryCatch({
  source(config_file, local = FALSE)
  source(appendix_config, local = FALSE)
  cli::cli_alert_success("Configurations loaded successfully")
}, error = function(e) {
  stop(sprintf("Failed to load configuration: %s", e$message))
})

# Verify PATHS was loaded
if (!exists("PATHS") || !is.list(PATHS)) {
  stop("PATHS configuration not found after sourcing ", config_file)
}

# =============================================================================
# 6. SCRIPT DEFINITIONS
# =============================================================================

# Define pipeline stages
scripts_base <- c(
  "R/01_anonymize_data.R",
  "R/02_classify_stakeholders.R"
)

scripts_analysis <- c(
  "R/get_exact_1307.R",
  "R/03_main_analysis.R",
  "R/04_hypothesis_testing.R"
)

# Validate scripts exist
validate_scripts <- function(script_list, stage_name) {
  missing_scripts <- script_list[!file.exists(script_list)]
  
  if (length(missing_scripts) > 0) {
    stop(sprintf(
      "Required %s script(s) not found:\n  %s",
      stage_name,
      paste(missing_scripts, collapse = "\n  ")
    ))
  }
  
  invisible(TRUE)
}

validate_scripts(scripts_base, "base pipeline")
if (WITH_ANALYSIS) {
  validate_scripts(scripts_analysis, "analysis pipeline")
}

# =============================================================================
# 7. ENHANCED SCRIPT EXECUTION WITH METHOD SAFETY
# =============================================================================

# Checkpoint saving function
save_checkpoint <- function(stage) {
  checkpoint_dir <- "checkpoints"
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  
  checkpoint_file <- file.path(
    checkpoint_dir,
    sprintf("checkpoint_%s_%s.RData",
           stage,
           format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  
  save.image(file = checkpoint_file)
  
  if (DEBUG) {
    cli::cli_alert_info(sprintf("Checkpoint saved: %s", checkpoint_file))
  }
}

# Enhanced script runner with S4 method protection and environment isolation
run_script <- function(path, stage_name = NULL) {
  # Update global tracking variables
  assign("current_script", path, envir = .GlobalEnv)
  assign("pipeline_stage", stage_name %||% basename(path), envir = .GlobalEnv)
  
  cli::cli_h2(paste("Running:", basename(path)))
  
  if (!file.exists(path)) {
    stop(sprintf("Required script not found: %s", path))
  }
  
  # Get file info for logging
  file_info <- file.info(path)
  if (DEBUG) {
    cli::cli_alert_info(sprintf("File size: %s bytes", file_info$size))
    cli::cli_alert_info(sprintf("Last modified: %s", file_info$mtime))
  }
  
  start_time <- Sys.time()
  
  # CRITICAL: Re-establish S4 method safety before each script
  if (!exists(".local", mode = "function", envir = .GlobalEnv)) {
    .local <- function(...) return(invisible(NULL))
    assign(".local", .local, envir = .GlobalEnv)
  }
  
  # Ensure methods package is loaded
  if (!"methods" %in% loadedNamespaces()) {
    suppressPackageStartupMessages(library(methods))
  }
  
  # Execute script with error handling
  tryCatch({
    # Create a new environment for script execution to avoid pollution
    script_env <- new.env(parent = .GlobalEnv)
    
    # Copy essential objects to script environment
    script_env$.local <- .local
    script_env$PATHS <- PATHS
    script_env$STANDARD_COLUMNS <- STANDARD_COLUMNS
    script_env$QUALITY_PARAMS <- QUALITY_PARAMS
    script_env$DEPRECATED_PATHS <- DEPRECATED_PATHS
    script_env$PRIVACY_COLUMNS <- PRIVACY_COLUMNS
    
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
    
    return(TRUE)
    
  }, error = function(e) {
    cli::cli_alert_danger(sprintf("Script failed: %s", basename(path)))
    stop(sprintf("Error in %s: %s", path, e$message))
  })
}

# =============================================================================
# 8. DATA VALIDATION & PREPARATION
# =============================================================================

pipeline_stage <- "Data Validation"

# Validate raw data existence
if (!dir.exists(PATHS$raw_data_dir %||% "data_raw")) {
  stop("Required folder 'data_raw' not found. Please create it and place raw survey CSVs there.")
}

raw_files <- list.files(PATHS$raw_data_dir %||% "data_raw", pattern = "\\.csv$", full.names = TRUE)
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
output_dirs <- c("data", "docs", "output", "figures", "logs", "checkpoints")
for (dir in output_dirs) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# =============================================================================
# 9. OPTIONAL CLEANING
# =============================================================================

if (CLEAN) {
  pipeline_stage <- "Cleaning"
  cli::cli_h2("Cleaning Previous Outputs")
  
  # Use paths from central configuration
  files_to_remove <- c(
    PATHS$basic_anon,
    PATHS$preliminary_classified,
    PATHS$classification_template,
    PATHS$dictionary,
    PATHS$final_1307,
    PATHS$checksums,
    PATHS$verification_report,
    PATHS$error_log,
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

# Check for deprecated files
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
# 10. MAIN PIPELINE EXECUTION
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
# 11. VERIFICATION USING QUALITY CHECKS SCRIPT
# =============================================================================

if (VERIFY) {
  pipeline_stage <- "Verification"
  cli::cli_h2("Running Quality Verification")
  
  quality_script <- "R/99_quality_checks.R"
  
  if (file.exists(quality_script)) {
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
    
    # Fallback: Basic file existence check
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
# 12. FINAL SUCCESS MESSAGE
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