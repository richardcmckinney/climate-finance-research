#!/usr/bin/env Rscript
# run_all.R
# Purpose: One-command, deterministic end-to-end pipeline runner + verifier.
# Description:
#   This script executes the entire data processing and analysis pipeline.
#   It ensures that all artifacts are regenerated from the raw data in a
#   deterministic manner.
#
# Flags:
#   --clean           Remove known outputs before running.
#   --verify          Run verification checks after the pipeline completes (default).
#   --with-analysis   Run the full manuscript analysis stage (N=1307 subset, figures, tables).

# --- 1. Configuration & Setup ---
# Safety for Rscript/S4 compatibility
if (!"methods" %in% loadedNamespaces()) library(methods)

# Set deterministic environment settings
set.seed(12345)
Sys.setenv(TZ = "UTC")
options(stringsAsFactors = FALSE, scipen = 999, readr.show_col_types = FALSE)

# --- 2. Argument Parsing ---
args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) any(grepl(paste0("^", flag, "$"), args))

CLEAN <- has_flag("--clean")
VERIFY <- has_flag("--verify") || !any(grepl("^--(no-)?verify$", args))
WITH_ANALYSIS <- has_flag("--with-analysis")

# --- 3. Error Handling ---
options(error = function() {
  message("\n--- SCRIPT FAILED ---")
  err_msg <- geterrmessage()
  message(paste("Error:", err_msg))
  
  # Write a concise error log
  dir.create("docs", showWarnings = FALSE, recursive = TRUE)
  log_content <- c(
    paste("Timestamp (UTC):", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste("Error Message:", err_msg),
    "\nTraceback:",
    paste(capture.output(traceback(2)), collapse = "\n")
  )
  writeLines(log_content, "docs/error_log.txt")
  message("A concise error log has been saved to docs/error_log.txt")
  
  # Exit with a non-zero status code
  quit(save = "no", status = 1, runLast = FALSE)
})

# --- 4. Package Management ---
required_pkgs <- c("readr", "dplyr", "stringr", "tidyr", "digest", "cli", "lubridate")
analysis_pkgs <- c("psych", "lavaan", "ggplot2", "scales", "corrplot", "broom", "effectsize")

install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) > 0) {
    message(paste("Installing missing packages:", paste(missing, collapse = ", ")))
    install.packages(missing, repos = "https://cloud.r-project.org")
  }
  # Verify installation
  still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(still_missing) > 0) {
    stop("Failed to install required packages: ", paste(still_missing, collapse = ", "))
  }
}

install_if_missing(required_pkgs)
if (WITH_ANALYSIS) {
  install_if_missing(analysis_pkgs)
}

# --- 5. Define File Paths & Scripts ---
# Canonical file paths used throughout the pipeline
paths <- list(
  # Public artifacts (always created)
  basic_anon = "data/survey_responses_anonymized_basic.csv",
  prelim_classified = "data/survey_responses_anonymized_preliminary.csv",
  class_template = "docs/appendix_j_classification_template.csv",
  dictionary = "data/data_dictionary.csv",
  # Analysis artifacts (created with --with-analysis)
  final_1307 = "data/climate_finance_survey_final_1307.csv"
)

# Scripts to be executed in order
scripts_base <- c(
  "R/01_anonymize_data.R",
  "R/02_classify_stakeholders.R"
)
scripts_analysis <- c(
  "R/get_exact_1307.R",
  "R/03_main_analysis.R",
  "R/04_hypothesis_testing.R"
)

# --- 6. Pipeline Execution ---
run_script <- function(path) {
  cli::cli_h1(paste("Running:", path))
  if (!file.exists(path)) {
    stop("Required script not found: ", path)
  }
  # Source the script in the global environment.
  # The `local = TRUE` argument would create a new environment, but for this
  # simple, linear pipeline, sourcing to the global environment is clean
  # and straightforward, assuming scripts are self-contained.
  source(path, echo = FALSE)
  cli::cli_alert_success(paste("Completed:", path))
}

# Ensure raw data exists
if (!dir.exists("data_raw") || length(list.files("data_raw", pattern = "\\.csv$")) == 0) {
  stop("Required folder `data_raw` not found or is empty. Place raw survey CSVs there.")
}

# Create output directories
for (dir in c("data", "docs", "output", "figures")) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
}

# Optional clean step
if (CLEAN) {
  cli::cli_alert_info("Cleaning previous outputs...")
  files_to_remove <- c(unlist(paths), "docs/checksums.txt", "docs/verification_report.md")
  file.remove(files_to_remove[file.exists(files_to_remove)])
  # Also remove output directories for a full clean
  if (WITH_ANALYSIS) {
    unlink("output", recursive = TRUE)
    unlink("figures", recursive = TRUE)
    cli::cli_alert_info("Removed 'output/' and 'figures/' directories.")
  }
}

# --- Main Execution Block ---
# Base pipeline (always runs)
for (s in scripts_base) {
  run_script(s)
}

# Analysis pipeline (optional)
if (WITH_ANALYSIS) {
  for (s in scripts_analysis) {
    run_script(s)
  }
} else {
  cli::cli_alert_info("Skipping analysis stage. To run it, use the --with-analysis flag.")
}

# --- 7. Verification ---
verify_outputs <- function(artifact_paths, is_analysis_run) {
  cli::cli_h1("Verification")
  all_ok <- TRUE
  report_lines <- c(
    "# Verification Report",
    paste0("- Timestamp (UTC): ", format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )

  # 1. Check for file presence
  cli::cli_h2("1. File Presence")
  core_artifacts <- artifact_paths[c("basic_anon", "prelim_classified", "class_template", "dictionary")]
  analysis_artifacts <- artifact_paths[c("final_1307")]
  
  artifacts_to_check <- if (is_analysis_run) unlist(artifact_paths) else unlist(core_artifacts)
  
  for (path in artifacts_to_check) {
    if (file.exists(path)) {
      cli::cli_alert_success(paste("Found:", path))
    } else {
      cli::cli_alert_danger(paste("Missing:", path))
      all_ok <- FALSE
    }
  }

  # 2. Schema check: dictionary vs. basic anonymized data
  cli::cli_h2("2. Schema Validation")
  if (file.exists(paths$basic_anon) && file.exists(paths$dictionary)) {
    basic_df <- readr::read_csv(paths$basic_anon)
    dict_df <- readr::read_csv(paths$dictionary)
    if (identical(names(basic_df), dict_df$column_name)) {
      cli::cli_alert_success("Dictionary schema matches anonymized data.")
    } else {
      cli::cli_alert_danger("Schema mismatch between dictionary and anonymized data.")
      all_ok <- FALSE
    }
  }

  # 3. Check for NAs in final classification
  cli::cli_h2("3. Data Integrity")
  if (file.exists(paths$prelim_classified)) {
    prelim_df <- readr::read_csv(paths$prelim_classified)
    if ("final_category_appendix_j" %in% names(prelim_df)) {
      if (any(is.na(prelim_df$final_category_appendix_j))) {
        cli::cli_alert_danger("Found NA values in the final classification column.")
        all_ok <- FALSE
      } else {
        cli::cli_alert_success("No NA values found in classification column.")
      }
    }
  }

  # 4. Generate Checksums
  cli::cli_h2("4. Checksums (SHA-256)")
  checksum_df <- tibble::tibble(file = character(), sha256 = character())
  for (path in artifacts_to_check) {
    if (file.exists(path)) {
      sha <- digest::digest(file = path, algo = "sha256")
      checksum_df <- checksum_df %>% add_row(file = basename(path), sha256 = sha)
      cli::cli_text(paste0("{.file ", basename(path), "}: ", sha))
    }
  }
  readr::write_csv(checksum_df, "docs/checksums.txt")
  cli::cli_alert_success("Checksums written to docs/checksums.txt")
  
  # Write verification report
  report_lines <- c(report_lines, "\n## Checksums\n", paste0("- ", checksum_df$file, ": ", checksum_df$sha256))
  writeLines(report_lines, "docs/verification_report.md")

  if (all_ok) {
    cli::cli_h1("✓ Verification Successful")
  } else {
    cli::cli_h1("✗ Verification Failed")
    stop("One or more verification checks failed.")
  }
}

if (VERIFY) {
  verify_outputs(paths, WITH_ANALYSIS)
}

cli::cli_alert_success("✅ Pipeline completed successfully.")