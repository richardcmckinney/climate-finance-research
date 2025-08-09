#!/usr/bin/env Rscript
# run_all.R - Pipeline Runner v5.0
# Purpose: Execute the complete data processing and analysis pipeline
# Description: Orchestrates all pipeline stages with enhanced error handling,
#              reproducible dependency management, and configuration-based settings
# Author: Richard McKinney
# Date: 2025-08-09
#
# Flags:
#   --clean           Remove all outputs before running
#   --verify          Run verification checks (default: TRUE)
#   --no-verify       Skip verification checks
#   --with-analysis   Run full manuscript analysis stage (N=1307 subset, figures, tables)
#   --debug           Enable verbose debug output
#   --checkpoint      Save state after each major step
#   --force           Continue pipeline even after errors
#   --help            Show help message

# =============================================================================
# 1. INITIAL SETUP & S4 METHOD PROTECTION
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
FORCE_CONTINUE <- "--force" %in% args || "-f" %in% args

# Show help if requested
if (HELP) {
  cat("
Climate Finance Research Pipeline Runner v5.0

Usage: Rscript run_all.R [OPTIONS]

Options:
  --clean, -c         Remove all outputs before running
  --with-analysis, -a Include full analysis stage (figures & hypothesis testing)
  --no-verify, -nv    Skip verification checks
  --debug, -d         Enable verbose debug output
  --checkpoint, -cp   Save state after each major step
  --force, -f         Continue pipeline even after errors
  --help, -h          Show this help message

Examples:
  Rscript run_all.R                    # Run base pipeline with verification
  Rscript run_all.R --clean            # Clean and run base pipeline
  Rscript run_all.R --with-analysis    # Run full pipeline including analysis
  Rscript run_all.R -c -a --checkpoint # Clean and run full pipeline with checkpoints
  Rscript run_all.R --force            # Continue on errors (use with caution)

Default behavior: Runs base pipeline with verification, no cleaning, no analysis.

Dependencies: This pipeline uses 'renv' for reproducible package management.
              Run 'renv::restore()' to install all required packages.
")
  quit(save = "no", status = 0)
}

# =============================================================================
# 3. EARLY DIRECTORY CREATION FOR LOGGING
# =============================================================================

essential_dirs <- c("docs", "logs", "output", "data", "figures", "checkpoints")
for (dir in essential_dirs) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# =============================================================================
# 4. DEPENDENCY MANAGEMENT WITH RENV (REPLACES RUNTIME INSTALLATION)
# =============================================================================

# Check for renv
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║          DEPENDENCY MANAGEMENT SYSTEM REQUIRED                  ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("This pipeline uses 'renv' for reproducible package management.\n")
  cat("\n")
  cat("To set up the environment:\n")
  cat("  1. Install renv: install.packages('renv')\n")
  cat("  2. Initialize project: renv::init()\n")
  cat("  3. Install dependencies: renv::restore()\n")
  cat("\n")
  cat("If you have a renv.lock file, just run:\n")
  cat("  install.packages('renv')\n")
  cat("  renv::restore()\n")
  cat("\n")
  quit(save = "no", status = 1)
}

# Restore dependencies from renv.lock
cat("Checking project dependencies...\n")

renv_status <- tryCatch({
  # Check if renv.lock exists
  if (file.exists("renv.lock")) {
    renv::restore(prompt = FALSE, clean = FALSE)
    cat("✓ Dependencies verified from renv.lock\n")
    "restored"
  } else {
    cat("⚠ No renv.lock file found\n")
    "no_lockfile"
  }
}, error = function(e) {
  cat("⚠ Could not restore from renv.lock: ", e$message, "\n")
  "error"
})

# Define required packages
core_packages <- c(
  "cli", "readr", "dplyr", "tidyr", "stringr", 
  "tibble", "purrr", "lubridate", "digest"
)

analysis_packages <- c(
  "ggplot2", "psych", "lavaan", "scales", "corrplot",
  "broom", "effectsize", "car", "nnet", "MASS"
)

# Check for missing core packages
missing_core <- core_packages[!sapply(core_packages, requireNamespace, quietly = TRUE)]

if (length(missing_core) > 0) {
  cat("\n")
  cat("╔════════════════════════════════════════════════════════════════╗\n")
  cat("║                  MISSING REQUIRED PACKAGES                      ║\n")
  cat("╚════════════════════════════════════════════════════════════════╝\n")
  cat("\n")
  cat("The following required packages are not installed:\n")
  cat("  ", paste(missing_core, collapse = ", "), "\n")
  cat("\n")
  cat("To install them:\n")
  cat("  1. Run: install.packages(c(", paste0('"', missing_core, '"', collapse = ", "), "))\n")
  cat("  2. Then: renv::snapshot() # to update the lockfile\n")
  cat("\n")
  cat("Or restore from a complete lockfile:\n")
  cat("  renv::restore()\n")
  cat("\n")
  quit(save = "no", status = 1)
}

# Check for missing analysis packages if needed
if (WITH_ANALYSIS) {
  missing_analysis <- analysis_packages[!sapply(analysis_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_analysis) > 0) {
    cat("\n")
    cat("⚠ Missing analysis packages: ", paste(missing_analysis, collapse = ", "), "\n")
    cat("  Install with: install.packages(c(", paste0('"', missing_analysis, '"', collapse = ", "), "))\n")
    cat("  Then update lockfile: renv::snapshot()\n")
    cat("\n")
    
    if (!FORCE_CONTINUE) {
      quit(save = "no", status = 1)
    } else {
      cat("  Continuing anyway due to --force flag\n")
    }
  }
}

# Load essential packages
suppressPackageStartupMessages({
  library(cli)
  library(dplyr)
  library(readr)
})

# =============================================================================
# 5. LOAD CENTRAL CONFIGURATION & SET SEED
# =============================================================================

cli::cli_h1("Climate Finance Research Pipeline v5.0")

# Source configuration files
config_file <- "R/00_config.R"
appendix_config <- "R/appendix_j_config.R"

if (!file.exists(config_file)) {
  cli::cli_abort("Central configuration file not found: {config_file}")
}

tryCatch({
  source(config_file, local = FALSE)
  if (file.exists(appendix_config)) {
    source(appendix_config, local = FALSE)
  }
  cli::cli_alert_success("Configurations loaded successfully")
}, error = function(e) {
  cli::cli_abort("Failed to load configuration: {e$message}")
})

# Verify PATHS was loaded
if (!exists("PATHS") || !is.list(PATHS)) {
  cli::cli_abort("PATHS configuration not found after sourcing {config_file}")
}

# USE CONFIGURATION-BASED SEED (FIX FOR ISSUE #2)
if (exists("PIPELINE_SEED")) {
  set.seed(PIPELINE_SEED)
  cli::cli_alert_info("Using pipeline seed: {PIPELINE_SEED}")
} else {
  # Fallback if not defined in config
  PIPELINE_SEED <- 42
  set.seed(PIPELINE_SEED)
  cli::cli_alert_warning("PIPELINE_SEED not found in config, using default: {PIPELINE_SEED}")
}

# =============================================================================
# 6. ENHANCED ERROR HANDLING SYSTEM (FIX FOR ISSUE #3)
# =============================================================================

# Global error state tracking
pipeline_errors <- list()
pipeline_warnings <- list()

# Enhanced error handler with recovery
options(error = function() {
  err_msg <- geterrmessage()
  timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S UTC")
  
  # Capture error context
  error_context <- list(
    message = err_msg,
    timestamp = timestamp,
    stage = if (exists("pipeline_stage")) pipeline_stage else "Unknown",
    script = if (exists("current_script")) current_script else "Unknown",
    traceback = capture.output(traceback(max.lines = 20))
  )
  
  # Save error state for recovery
  error_file <- file.path("logs", paste0("error_state_", 
                                         format(Sys.time(), "%Y%m%d_%H%M%S"), 
                                         ".rds"))
  saveRDS(error_context, error_file)
  
  # Display error information
  cli::cli_alert_danger("Pipeline execution failed")
  cli::cli_alert_danger("Error: {err_msg}")
  cli::cli_alert_info("Error state saved to: {error_file}")
  
  # Write detailed error log
  error_log_path <- "logs/pipeline_errors.log"
  error_log_entry <- paste(
    "\n=====================================",
    timestamp,
    paste("Stage:", error_context$stage),
    paste("Script:", error_context$script),
    paste("Error:", err_msg),
    paste(error_context$traceback, collapse = "\n"),
    sep = "\n"
  )
  
  cat(error_log_entry, file = error_log_path, append = TRUE)
  
  if (!FORCE_CONTINUE) {
    cli::cli_alert_info("Use --force flag to continue pipeline after errors")
    quit(save = "no", status = 1, runLast = FALSE)
  } else {
    cli::cli_alert_warning("Continuing due to --force flag (error logged)")
  }
})

# =============================================================================
# 7. ENHANCED SCRIPT RUNNER WITH ERROR RECOVERY
# =============================================================================

# Checkpoint saving function
save_checkpoint <- function(stage, success = TRUE) {
  if (!CHECKPOINT) return(invisible(NULL))
  
  checkpoint_dir <- "checkpoints"
  dir.create(checkpoint_dir, showWarnings = FALSE, recursive = TRUE)
  
  checkpoint_file <- file.path(
    checkpoint_dir,
    sprintf("checkpoint_%s_%s_%s.RData",
           stage,
           ifelse(success, "success", "failed"),
           format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  
  # Save workspace and metadata
  checkpoint_metadata <- list(
    stage = stage,
    success = success,
    timestamp = Sys.time(),
    seed = PIPELINE_SEED,
    errors = pipeline_errors,
    warnings = pipeline_warnings
  )
  
  save(list = ls(envir = .GlobalEnv), 
       file = checkpoint_file, 
       envir = .GlobalEnv)
  
  saveRDS(checkpoint_metadata, 
          sub("\\.RData$", "_metadata.rds", checkpoint_file))
  
  if (DEBUG) {
    cli::cli_alert_info("Checkpoint saved: {checkpoint_file}")
  }
}

# Enhanced script runner with comprehensive error handling
run_script <- function(path, stage_name = NULL) {
  stage_name <- stage_name %||% tools::file_path_sans_ext(basename(path))
  
  # Update global tracking
  assign("current_script", path, envir = .GlobalEnv)
  assign("pipeline_stage", stage_name, envir = .GlobalEnv)
  
  cli::cli_h2("Running: {basename(path)}")
  
  if (!file.exists(path)) {
    cli::cli_alert_danger("Script not found: {path}")
    
    # Record error
    pipeline_errors[[stage_name]] <<- list(
      type = "file_not_found",
      message = paste("Script not found:", path),
      timestamp = Sys.time()
    )
    
    if (FORCE_CONTINUE) {
      cli::cli_alert_warning("Skipping due to missing script (--force mode)")
      return(FALSE)
    } else {
      cli::cli_abort("Required script not found: {path}")
    }
  }
  
  # Get file info
  file_info <- file.info(path)
  if (DEBUG) {
    cli::cli_alert_info("File size: {file_info$size} bytes")
    cli::cli_alert_info("Last modified: {format(file_info$mtime)}")
  }
  
  start_time <- Sys.time()
  
  # Create execution environment
  script_env <- new.env(parent = .GlobalEnv)
  
  # Copy essential objects
  script_env$.local <- if (exists(".local")) .local else function(...) invisible(NULL)
  script_env$PATHS <- PATHS
  script_env$PIPELINE_SEED <- PIPELINE_SEED
  
  # Copy other configurations if they exist
  config_objects <- c("STANDARD_COLUMNS", "QUALITY_PARAMS", 
                     "DEPRECATED_PATHS", "PRIVACY_COLUMNS")
  for (obj in config_objects) {
    if (exists(obj)) {
      script_env[[obj]] <- get(obj)
    }
  }
  
  # Execute with error handling
  execution_result <- tryCatch({
    # Ensure methods package is available
    if (!"methods" %in% loadedNamespaces()) {
      suppressPackageStartupMessages(library(methods))
    }
    
    # Source the script
    if (DEBUG) {
      source(path, local = script_env, echo = TRUE, verbose = TRUE)
    } else {
      suppressMessages(source(path, local = script_env, echo = FALSE))
    }
    
    # Success
    exec_time <- difftime(Sys.time(), start_time, units = "secs")
    cli::cli_alert_success("Completed: {basename(path)} ({round(exec_time, 2)} seconds)")
    
    # Save successful checkpoint
    save_checkpoint(stage_name, success = TRUE)
    
    return(list(success = TRUE, time = exec_time))
    
  }, warning = function(w) {
    # Capture warning but continue
    warning_msg <- conditionMessage(w)
    cli::cli_alert_warning("Warning in {basename(path)}: {warning_msg}")
    
    # Record warning
    pipeline_warnings[[stage_name]] <<- append(
      pipeline_warnings[[stage_name]], 
      list(list(message = warning_msg, timestamp = Sys.time()))
    )
    
    # Continue execution
    invokeRestart("muffleWarning")
    
  }, error = function(e) {
    # Handle error
    error_msg <- conditionMessage(e)
    exec_time <- difftime(Sys.time(), start_time, units = "secs")
    
    cli::cli_alert_danger("Script failed: {basename(path)}")
    cli::cli_alert_danger("Error: {error_msg}")
    cli::cli_alert_info("Execution time before failure: {round(exec_time, 2)} seconds")
    
    # Create detailed error record
    error_record <- list(
      type = "execution_error",
      message = error_msg,
      script = path,
      stage = stage_name,
      timestamp = Sys.time(),
      execution_time = exec_time,
      traceback = capture.output(traceback())
    )
    
    # Save error state
    error_file <- file.path("output", paste0("error_", stage_name, "_",
                                            format(Sys.time(), "%Y%m%d_%H%M%S"),
                                            ".rds"))
    saveRDS(error_record, error_file)
    cli::cli_alert_info("Error details saved to: {error_file}")
    
    # Record in global error list
    pipeline_errors[[stage_name]] <<- error_record
    
    # Save failed checkpoint
    save_checkpoint(stage_name, success = FALSE)
    
    return(list(success = FALSE, time = exec_time, error = error_msg))
  })
  
  return(execution_result)
}

# =============================================================================
# 8. DATA VALIDATION & PREPARATION
# =============================================================================

pipeline_stage <- "Data Validation"
cli::cli_h2("Validating Data Environment")

# Check for raw data
raw_data_dir <- PATHS$raw_data_dir %||% "data_raw"
if (!dir.exists(raw_data_dir)) {
  cli::cli_abort(c(
    "Required folder '{raw_data_dir}' not found",
    "i" = "Please create it and place raw survey CSVs there"
  ))
}

raw_files <- list.files(raw_data_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) {
  cli::cli_abort(c(
    "No CSV files found in '{raw_data_dir}' folder",
    "i" = "Please add raw survey data files"
  ))
}

cli::cli_alert_success("Found {length(raw_files)} raw data file{?s}")

if (DEBUG) {
  for (f in raw_files) {
    size_kb <- round(file.info(f)$size / 1024, 1)
    cli::cli_alert_info("  - {basename(f)} ({size_kb} KB)")
  }
}

# =============================================================================
# 9. OPTIONAL CLEANING
# =============================================================================

if (CLEAN) {
  pipeline_stage <- "Cleaning"
  cli::cli_h2("Cleaning Previous Outputs")
  
  # Define files to remove from configuration
  files_to_remove <- c(
    PATHS$basic_anon,
    PATHS$preliminary_classified,
    PATHS$classification_template,
    PATHS$dictionary,
    PATHS$final_1307,
    PATHS$checksums,
    PATHS$verification_report,
    PATHS$quality_assurance
  )
  
  # Add log files
  log_files <- list.files("logs", full.names = TRUE)
  files_to_remove <- c(files_to_remove, log_files)
  
  # Remove NULL paths
  files_to_remove <- files_to_remove[!sapply(files_to_remove, is.null)]
  
  # Remove files
  removed_count <- 0
  for (f in files_to_remove) {
    if (file.exists(f)) {
      file.remove(f)
      removed_count <- removed_count + 1
      if (DEBUG) cli::cli_alert_info("Removed: {f}")
    }
  }
  
  # Clean directories for full clean
  if (WITH_ANALYSIS) {
    for (dir in c("output", "figures", "checkpoints")) {
      if (dir.exists(dir)) {
        unlink(dir, recursive = TRUE)
        dir.create(dir, showWarnings = FALSE)
        if (DEBUG) cli::cli_alert_info("Cleaned directory: {dir}")
      }
    }
  }
  
  cli::cli_alert_success("Cleaned {removed_count} file{?s} and director{?y/ies}")
}

# Check for deprecated files
if (exists("DEPRECATED_PATHS") && length(DEPRECATED_PATHS) > 0) {
  deprecated <- DEPRECATED_PATHS[file.exists(DEPRECATED_PATHS)]
  if (length(deprecated) > 0) {
    cli::cli_alert_warning("Found {length(deprecated)} deprecated file{?s}:")
    for (f in deprecated) {
      cli::cli_text("  - {f}")
    }
    cli::cli_alert_info("Consider removing deprecated files for cleaner workspace")
  }
}

# =============================================================================
# 10. MAIN PIPELINE EXECUTION WITH ERROR RECOVERY
# =============================================================================

cli::cli_h1("Pipeline Execution")
overall_start <- Sys.time()

# Initialize execution tracking
execution_summary <- list()

# Define pipeline scripts
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
  missing <- script_list[!file.exists(script_list)]
  
  if (length(missing) > 0) {
    cli::cli_alert_warning("Missing {stage_name} script{?s}: {.path {missing}}")
    
    if (!FORCE_CONTINUE) {
      cli::cli_abort(c(
        "Required {stage_name} scripts not found",
        "i" = "Use --force to skip missing scripts"
      ))
    }
  }
  
  return(script_list[file.exists(script_list)])
}

# Validate and filter scripts
scripts_base <- validate_scripts(scripts_base, "base pipeline")
if (WITH_ANALYSIS) {
  scripts_analysis <- validate_scripts(scripts_analysis, "analysis pipeline")
}

# Execute base pipeline
pipeline_stage <- "Base Pipeline"
cli::cli_h2("Stage 1: Base Data Processing")

base_success <- TRUE
for (script in scripts_base) {
  stage_id <- paste0("base_", tools::file_path_sans_ext(basename(script)))
  result <- run_script(script, stage_name = stage_id)
  
  execution_summary[[stage_id]] <- result
  
  if (!result$success) {
    base_success <- FALSE
    if (!FORCE_CONTINUE) {
      cli::cli_abort(c(
        "Pipeline halted due to error in {basename(script)}",
        "i" = "Use --force flag to continue after errors"
      ))
    }
  }
}

if (!base_success) {
  cli::cli_alert_warning("Base pipeline completed with errors")
}

# Execute analysis pipeline (conditional)
if (WITH_ANALYSIS) {
  pipeline_stage <- "Analysis Pipeline"
  cli::cli_h2("Stage 2: Analysis & Hypothesis Testing")
  
  analysis_success <- TRUE
  for (script in scripts_analysis) {
    stage_id <- paste0("analysis_", tools::file_path_sans_ext(basename(script)))
    result <- run_script(script, stage_name = stage_id)
    
    execution_summary[[stage_id]] <- result
    
    if (!result$success) {
      analysis_success <- FALSE
      if (!FORCE_CONTINUE) {
        cli::cli_abort(c(
          "Pipeline halted due to error in {basename(script)}",
          "i" = "Use --force flag to continue after errors"
        ))
      }
    }
  }
  
  if (!analysis_success) {
    cli::cli_alert_warning("Analysis pipeline completed with errors")
  }
} else {
  cli::cli_alert_info("Skipping analysis stage. Use --with-analysis flag to enable.")
}

# Calculate execution metrics
total_time <- difftime(Sys.time(), overall_start, units = "mins")
successful_stages <- sum(sapply(execution_summary, function(x) x$success))
total_stages <- length(execution_summary)

# =============================================================================
# 11. VERIFICATION WITH QUALITY CHECKS
# =============================================================================

if (VERIFY) {
  pipeline_stage <- "Verification"
  cli::cli_h2("Running Quality Verification")
  
  quality_script <- "R/99_quality_checks.R"
  
  if (file.exists(quality_script)) {
    cli::cli_alert_info("Running quality checks...")
    
    qa_result <- run_script(quality_script, "quality_verification")
    
    if (qa_result$success) {
      # Check for quality report
      if (!is.null(PATHS$quality_assurance) && file.exists(PATHS$quality_assurance)) {
        cli::cli_alert_success("Quality report generated: {PATHS$quality_assurance}")
      }
    } else {
      cli::cli_alert_warning("Quality checks encountered issues")
    }
    
  } else {
    cli::cli_alert_warning("Quality checks script not found: {quality_script}")
    
    # Fallback: Basic output verification
    cli::cli_alert_info("Running basic output verification...")
    
    expected_outputs <- c(
      PATHS$basic_anon,
      PATHS$preliminary_classified,
      PATHS$classification_template,
      PATHS$dictionary
    )
    
    if (WITH_ANALYSIS) {
      expected_outputs <- c(expected_outputs, PATHS$final_1307)
    }
    
    # Remove NULLs
    expected_outputs <- expected_outputs[!sapply(expected_outputs, is.null)]
    
    missing_outputs <- expected_outputs[!file.exists(expected_outputs)]
    
    if (length(missing_outputs) > 0) {
      cli::cli_alert_warning("Missing expected outputs:")
      for (f in missing_outputs) {
        cli::cli_text("  - {f}")
      }
    } else {
      cli::cli_alert_success("All expected outputs present")
    }
  }
} else {
  cli::cli_alert_info("Verification skipped (use --verify to enable)")
}

# =============================================================================
# 12. EXECUTION SUMMARY & FINAL REPORT
# =============================================================================

# Determine overall status
overall_success <- (successful_stages == total_stages) && 
                  (length(pipeline_errors) == 0)

if (overall_success) {
  cli::cli_h1("✅ Pipeline Completed Successfully")
} else {
  cli::cli_h1("⚠️ Pipeline Completed with Issues")
}

# Display execution summary
cli::cli_h2("Execution Summary")

cli::cli_alert_info("Total execution time: {round(total_time, 1)} minutes")
cli::cli_alert_info("Stages executed: {successful_stages}/{total_stages} successful")

# Show error summary if any
if (length(pipeline_errors) > 0) {
  cli::cli_h3("Errors Encountered")
  for (stage in names(pipeline_errors)) {
    err <- pipeline_errors[[stage]]
    cli::cli_alert_danger("{stage}: {err$message}")
  }
}

# Show warning summary if any
if (length(pipeline_warnings) > 0) {
  cli::cli_h3("Warnings")
  total_warnings <- sum(sapply(pipeline_warnings, length))
  cli::cli_alert_warning("Total warnings: {total_warnings}")
  if (DEBUG) {
    for (stage in names(pipeline_warnings)) {
      for (warn in pipeline_warnings[[stage]]) {
        cli::cli_text("  {stage}: {warn$message}")
      }
    }
  }
}

# Count artifacts created
artifact_count <- 0
if (exists("PATHS") && is.list(PATHS)) {
  for (path_name in names(PATHS)) {
    if (!is.null(PATHS[[path_name]]) && 
        is.character(PATHS[[path_name]]) && 
        file.exists(PATHS[[path_name]])) {
      artifact_count <- artifact_count + 1
    }
  }
}
cli::cli_alert_success("Artifacts generated: {artifact_count}")

# Save execution report
execution_report <- list(
  timestamp = Sys.time(),
  duration_mins = as.numeric(total_time),
  seed = PIPELINE_SEED,
  successful_stages = successful_stages,
  total_stages = total_stages,
  errors = pipeline_errors,
  warnings = pipeline_warnings,
  artifact_count = artifact_count,
  flags = list(
    clean = CLEAN,
    verify = VERIFY,
    with_analysis = WITH_ANALYSIS,
    debug = DEBUG,
    checkpoint = CHECKPOINT,
    force_continue = FORCE_CONTINUE
  ),
  execution_summary = execution_summary
)

report_file <- file.path("output", paste0("pipeline_report_",
                                         format(Sys.time(), "%Y%m%d_%H%M%S"),
                                         ".rds"))
saveRDS(execution_report, report_file)
cli::cli_alert_info("Execution report saved: {report_file}")

# =============================================================================
# 13. NEXT STEPS & RECOMMENDATIONS
# =============================================================================

cli::cli_h2("Next Steps")

if (!WITH_ANALYSIS) {
  cli::cli_alert_info("Run with --with-analysis flag to generate full analysis outputs")
}

if (length(pipeline_errors) > 0 && !FORCE_CONTINUE) {
  cli::cli_alert_info("Review errors and run with --force to continue past failures")
}

# Point to generated reports
report_files <- c(
  verification = PATHS$verification_report,
  quality = PATHS$quality_assurance,
  checksums = PATHS$checksums
)

for (name in names(report_files)) {
  if (!is.null(report_files[[name]]) && file.exists(report_files[[name]])) {
    cli::cli_alert_info("Review {name} report: {report_files[[name]]}")
  }
}

# Create dependency snapshot reminder
if (renv_status == "no_lockfile" || renv_status == "error") {
  cli::cli_h3("Dependency Management")
  cli::cli_alert_info("Remember to create/update renv.lock with: renv::snapshot()")
}

# Exit with appropriate status
exit_status <- ifelse(overall_success, 0, 1)
quit(save = "no", status = exit_status, runLast = FALSE)