if (interactive() && file.exists("scripts/renv_sync.R")) { source("scripts/renv_sync.R"); try(renv_sync(), silent = TRUE) }
#!/usr/bin/env Rscript
# run_all.R - Pipeline Runner v6.0
# Purpose: Execute the complete data processing and analysis pipeline
# Description: Orchestrates all pipeline stages with enhanced error handling,
#              reproducible dependency management, configuration-based settings,
#              and manuscript reproducibility manifest generation
# Author: Richard McKinney
# Date: 2025-08-09
#
# Flags:
#   --clean           Remove all outputs before running (including output/ directory)
#   --verify          Run verification checks (default: TRUE)
#   --no-verify       Skip verification checks
#   --with-analysis   Run full manuscript analysis stage (REQUIRES N=1307 subset)
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
Climate Finance Research Pipeline Runner v6.0

Usage: Rscript run_all.R [OPTIONS]

Options:
  --clean, -c         Remove ALL outputs before running (including output/ directory)
  --with-analysis, -a Include full analysis stage (REQUIRES N=1,307 final dataset)
                      Generates manuscript figures, tables, and reproducibility manifest
  --no-verify, -nv    Skip verification checks
  --debug, -d         Enable verbose debug output
  --checkpoint, -cp   Save state after each major step
  --force, -f         Continue pipeline even after errors
  --help, -h          Show this help message

Examples:
  Rscript run_all.R                    # Run base pipeline with verification
  Rscript run_all.R --clean            # Clean ALL outputs and run base pipeline
  Rscript run_all.R --with-analysis    # Run full pipeline including manuscript analysis
  Rscript run_all.R -c -a --checkpoint # Clean all, run full pipeline with checkpoints
  Rscript run_all.R --force            # Continue on errors (use with caution)

Default behavior: Runs base pipeline with verification, no cleaning, no analysis.

NEW IN v6.0:
  - Enhanced --clean flag removes entire output/ directory
  - --with-analysis REQUIRES final_1307 dataset (N=1,307)
  - Generates reproducibility manifest mapping manuscript claims to artifacts
  - Improved validation for manuscript-ready outputs

Dependencies: This pipeline uses 'renv' for reproducible package management.
              Run 'renv::restore()' to install all required packages.
")
  quit(save = "no", status = 0)
}

# =============================================================================
# 3. EARLY DIRECTORY CREATION FOR LOGGING
# =============================================================================

essential_dirs <- c("docs", "logs", "output", "data", "figures", "checkpoints", "output/manifests")
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
  "broom", "effectsize", "car", "nnet", "MASS", "binom"
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
  library(tibble)
})

# =============================================================================
# 5. LOAD CENTRAL CONFIGURATION & SET SEED
# =============================================================================

cli::cli_h1("Climate Finance Research Pipeline v6.0")

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

# USE CONFIGURATION-BASED SEED
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
# 6. ENHANCED ERROR HANDLING SYSTEM
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
run_script <- function(path, stage_name = NULL, require_output = NULL) {
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
                     "DEPRECATED_PATHS", "PRIVACY_COLUMNS", "APPENDIX_J_CONFIG")
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
    
    # Check for required output if specified
    if (!is.null(require_output) && !file.exists(require_output)) {
      cli::cli_alert_danger("Required output not generated: {basename(require_output)}")
      return(list(success = FALSE, time = difftime(Sys.time(), start_time, units = "secs"),
                  error = paste("Missing required output:", require_output)))
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
# 9. ENHANCED CLEANING (v6.0 - Cleans entire output/ directory)
# =============================================================================

if (CLEAN) {
  pipeline_stage <- "Cleaning"
  cli::cli_h2("Cleaning Previous Outputs (ENHANCED)")
  
  # v6.0 Enhancement: Remove entire output directory
  if (dir.exists("output")) {
    cli::cli_alert_warning("Removing entire output/ directory...")
    unlink("output", recursive = TRUE, force = TRUE)
    cli::cli_alert_success("Removed output/ directory")
    
    # Recreate essential subdirectories
    dir.create("output", showWarnings = FALSE)
    dir.create("output/manifests", showWarnings = FALSE, recursive = TRUE)
    dir.create("output/tables", showWarnings = FALSE, recursive = TRUE)
    cli::cli_alert_info("Recreated output structure")
  }
  
  # Also clean figures and checkpoints if doing full clean
  if (WITH_ANALYSIS) {
    for (dir in c("figures", "checkpoints")) {
      if (dir.exists(dir)) {
        unlink(dir, recursive = TRUE)
        dir.create(dir, showWarnings = FALSE)
        cli::cli_alert_info("Cleaned directory: {dir}")
      }
    }
  }
  
  # Clean specific files from data/ and docs/
  files_to_remove <- c(
    PATHS$basic_anon,
    PATHS$preliminary_classified,
    PATHS$classification_template,
    PATHS$dictionary,
    PATHS$final_1307,
    PATHS$checksums,
    PATHS$verification_report
  )
  
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
  
  # Clean log files
  log_files <- list.files("logs", full.names = TRUE)
  if (length(log_files) > 0) {
    unlink(log_files)
    cli::cli_alert_info("Cleaned {length(log_files)} log file{?s}")
  }
  
  cli::cli_alert_success("Cleaning complete: removed {removed_count} file{?s} and all outputs")
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

scripts_quota <- c(
  "R/get_exact_1307.R"
)

scripts_analysis <- c(
  "R/03_main_analysis.R",
  "R/04_hypothesis_testing.R",
  "R/99_quality_checks.R"
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
scripts_quota <- validate_scripts(scripts_quota, "quota matching")

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

# Execute quota matching to generate final_1307
pipeline_stage <- "Quota Matching"
cli::cli_h2("Stage 2: Quota Matching (N=1,307)")

quota_success <- TRUE
for (script in scripts_quota) {
  stage_id <- paste0("quota_", tools::file_path_sans_ext(basename(script)))
  result <- run_script(script, stage_name = stage_id, require_output = PATHS$final_1307)
  
  execution_summary[[stage_id]] <- result
  
  if (!result$success) {
    quota_success <- FALSE
    if (!FORCE_CONTINUE) {
      cli::cli_abort(c(
        "Pipeline halted: Failed to generate final_1307 dataset",
        "i" = "This is required for analysis. Use --force to skip"
      ))
    }
  }
}

# =============================================================================
# 11. ANALYSIS PIPELINE WITH STRICT REQUIREMENTS (v6.0 Enhancement)
# =============================================================================

if (WITH_ANALYSIS) {
  pipeline_stage <- "Analysis Pipeline"
  cli::cli_h2("Stage 3: Manuscript Analysis & Hypothesis Testing")
  
  # v6.0: STRICT REQUIREMENT - final_1307 must exist
  if (!file.exists(PATHS$final_1307)) {
    cli::cli_abort(c(
      "Cannot run analysis: final_1307 dataset not found",
      "x" = "Expected at: {PATHS$final_1307}",
      "i" = "Run quota matching first or remove --with-analysis flag"
    ))
  }
  
  # Verify it's the correct N=1,307 dataset
  df_check <- readr::read_csv(PATHS$final_1307, show_col_types = FALSE, progress = FALSE)
  if (nrow(df_check) != 1307) {
    cli::cli_abort(c(
      "Invalid final dataset: has {nrow(df_check)} rows, expected 1,307",
      "x" = "The manuscript analysis requires exactly N=1,307",
      "i" = "Re-run quota matching or check data pipeline"
    ))
  }
  cli::cli_alert_success("Verified final_1307 dataset (N={nrow(df_check)})")
  
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
  
  # =============================================================================
  # 12. GENERATE REPRODUCIBILITY MANIFEST (v6.0 NEW FEATURE)
  # =============================================================================
  
  cli::cli_h2("Generating Reproducibility Manifest for Manuscript")
  
  # Create comprehensive manifest mapping claims to artifacts
  manifest <- tibble::tibble(
    manuscript_claim = c(
      "N=1,307 final sample size",
      "23 stakeholder categories (Appendix J)",
      "Category-specific sample sizes",
      "Geographic distribution (anonymized)",
      "EFA 3-factor model variance explained",
      "KMO adequacy (>0.5)",
      "Bartlett's test significance",
      "Headline barrier percentages",
      "Wilson 95% confidence intervals",
      "Technology risk perception (H1)",
      "Government agency sensitivity (H2)",
      "Tech-market correlation (H3)",
      "Market readiness barrier (H4)",
      "VC vs Government barriers (H5)",
      "International scalability (H6)",
      "Ecosystem-collaboration (H7)",
      "European regulatory concern (H8)",
      "Impact orientation (H9)",
      "Within-group coherence (H10)",
      "Physical-operational correlation (H11)",
      "Technology solutions (H12)",
      "ANOVA F-statistics",
      "Effect sizes (η², Cramér's V)",
      "Correlation coefficients",
      "Ordinal regression results",
      "Sample size adequacy checks"
    ),
    artifact_path = c(
      PATHS$final_1307 %||% "data/climate_finance_survey_final_1307.csv",
      PATHS$appendix_j_template %||% "docs/appendix_j_classification_template.csv",
      PATHS$category_counts %||% "output/manifests/category_counts.csv",
      PATHS$region_summary %||% "output/region_summary_final_1307.csv",
      PATHS$efa %||% "output/efa_results.csv",
      PATHS$efa %||% "output/efa_results.csv",
      PATHS$efa %||% "output/efa_results.csv",
      PATHS$headline_barriers_by_role %||% "output/headline_barriers_by_role.csv",
      "output/headline_barriers_by_role_wilson.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      "output/hypothesis_testing_summary.csv",
      PATHS$anova %||% "output/anova_tests.csv",
      "output/hypothesis_testing_summary.csv",
      PATHS$correlations %||% "output/correlations.csv",
      "output/tables/ordinal_regression_physical.csv",
      "output/sample_size_adequacy.csv"
    ),
    verification_method = c(
      "Count rows in final_1307.csv",
      "Count unique categories in template",
      "Check category_counts.csv",
      "Review region_summary.csv",
      "Check 'Total Variance Explained' in efa_results.csv",
      "Check 'KMO MSA' value in efa_results.csv",
      "Check 'Bartlett p-value' in efa_results.csv",
      "Review percentages in headline_barriers_by_role.csv",
      "Check ci_lower_pct and ci_upper_pct columns",
      "Check H1 row in hypothesis_testing_summary.csv",
      "Check H2 row in hypothesis_testing_summary.csv",
      "Check H3 row in hypothesis_testing_summary.csv",
      "Check H4 row in hypothesis_testing_summary.csv",
      "Check H5 row in hypothesis_testing_summary.csv",
      "Check H6 row in hypothesis_testing_summary.csv",
      "Check H7 row in hypothesis_testing_summary.csv",
      "Check H8 row in hypothesis_testing_summary.csv",
      "Check H9 row in hypothesis_testing_summary.csv",
      "Check H10 row in hypothesis_testing_summary.csv",
      "Check H11 row in hypothesis_testing_summary.csv",
      "Check H12 row in hypothesis_testing_summary.csv",
      "Review F-statistics in anova_tests.csv",
      "Check Effect_Size column in summary",
      "Review correlation matrix",
      "Check odds ratios in regression output",
      "Review adequacy flags in output"
    ),
    file_exists = sapply(artifact_path, file.exists),
    last_modified = sapply(artifact_path, function(f) {
      if (file.exists(f)) format(file.mtime(f), "%Y-%m-%d %H:%M:%S") else NA_character_
    })
  )
  
  # Add execution metadata
  manifest <- manifest %>%
    mutate(
      pipeline_version = "6.0",
      execution_timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"),
      random_seed = PIPELINE_SEED,
      r_version = R.version.string
    )
  
  # Save manifest
  manifest_dir <- PATHS$manifests %||% "output/manifests"
  dir.create(manifest_dir, showWarnings = FALSE, recursive = TRUE)
  
  manifest_path <- file.path(manifest_dir, "reproducibility_manifest.csv")
  readr::write_csv(manifest, manifest_path, na = "")
  cli::cli_alert_success("Reproducibility manifest saved: {manifest_path}")
  
  # Also save as markdown for easy reading
  manifest_md <- paste0(
    "# Reproducibility Manifest\n\n",
    "Generated: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
    "Pipeline Version: 6.0\n",
    "Random Seed: ", PIPELINE_SEED, "\n",
    "R Version: ", R.version.string, "\n\n",
    "## Manuscript Claims and Supporting Artifacts\n\n",
    "| Claim | Artifact | Exists | Verification |\n",
    "|-------|----------|--------|-------------|\n",
    paste(apply(manifest[1:4], 1, function(row) {
      sprintf("| %s | %s | %s | %s |",
              substr(row[1], 1, 40),
              basename(row[2]),
              ifelse(row[4] == "TRUE", "✓", "✗"),
              substr(row[3], 1, 30))
    }), collapse = "\n"),
    "\n\n## Missing Artifacts\n\n"
  )
  
  missing_artifacts <- manifest[!manifest$file_exists, ]
  if (nrow(missing_artifacts) > 0) {
    manifest_md <- paste0(manifest_md,
      "The following required artifacts are missing:\n\n",
      paste("- ", missing_artifacts$artifact_path, collapse = "\n"),
      "\n\n"
    )
  } else {
    manifest_md <- paste0(manifest_md, "All required artifacts are present.\n\n")
  }
  
  writeLines(manifest_md, file.path(manifest_dir, "reproducibility_manifest.md"))
  cli::cli_alert_success("Markdown manifest saved: reproducibility_manifest.md")
  
} else {
  cli::cli_alert_info("Skipping analysis stage. Use --with-analysis flag to enable.")
  cli::cli_alert_info("Note: --with-analysis requires the final N=1,307 dataset")
}

# Calculate execution metrics
total_time <- difftime(Sys.time(), overall_start, units = "mins")
successful_stages <- sum(sapply(execution_summary, function(x) x$success))
total_stages <- length(execution_summary)

# =============================================================================
# 13. VERIFICATION WITH QUALITY CHECKS
# =============================================================================

if (VERIFY) {
  pipeline_stage <- "Verification"
  cli::cli_h2("Running Quality Verification")
  
  quality_script <- "R/99_quality_checks.R"
  
  if (file.exists(quality_script) && !WITH_ANALYSIS) {
    # Only run if not already run in analysis stage
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
    
  } else if (!WITH_ANALYSIS) {
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
# 14. EXECUTION SUMMARY & FINAL REPORT
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
  execution_summary = execution_summary,
  version = "6.0"
)

report_file <- file.path("output", paste0("pipeline_report_",
                                         format(Sys.time(), "%Y%m%d_%H%M%S"),
                                         ".rds"))
saveRDS(execution_report, report_file)
cli::cli_alert_info("Execution report saved: {report_file}")

# =============================================================================
# 15. NEXT STEPS & RECOMMENDATIONS
# =============================================================================

cli::cli_h2("Next Steps")

if (!WITH_ANALYSIS) {
  cli::cli_alert_info("Run with --with-analysis flag to generate:")
  cli::cli_alert_info("  • Manuscript figures and tables")
  cli::cli_alert_info("  • Hypothesis testing results")
  cli::cli_alert_info("  • Reproducibility manifest")
  cli::cli_alert_info("  Note: Requires final N=1,307 dataset")
}

if (WITH_ANALYSIS && exists("manifest")) {
  missing_count <- sum(!manifest$file_exists)
  if (missing_count > 0) {
    cli::cli_alert_warning("{missing_count} manuscript artifacts missing")
    cli::cli_alert_info("Check reproducibility_manifest.csv for details")
  } else {
    cli::cli_alert_success("All manuscript artifacts generated successfully!")
  }
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

if (WITH_ANALYSIS) {
  report_files$manifest <- file.path(PATHS$manifests %||% "output/manifests", 
                                     "reproducibility_manifest.csv")
}

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

# v6.0 Enhancement notes
cli::cli_h3("Version 6.0 Enhancements")
cli::cli_alert_info("✓ Enhanced --clean removes entire output/ directory")
cli::cli_alert_info("✓ --with-analysis strictly requires N=1,307 dataset")
cli::cli_alert_info("✓ Reproducibility manifest maps claims to artifacts")
cli::cli_alert_info("✓ Improved validation for manuscript-ready outputs")

# Exit with appropriate status
exit_status <- ifelse(overall_success, 0, 1)
quit(save = "no", status = exit_status, runLast = FALSE)