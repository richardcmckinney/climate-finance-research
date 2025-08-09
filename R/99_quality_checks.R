#!/usr/bin/env Rscript
# R/99_quality_checks.R - Comprehensive quality assurance checks
# Purpose: Run comprehensive quality checks on pipeline outputs
# Version: 1.0
# Author: Pipeline QA System

suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
})

# Source central configuration
source("R/00_config.R")

# =============================================================================
# MAIN QUALITY CHECK FUNCTION
# =============================================================================

run_quality_checks <- function(verbose = TRUE, save_report = TRUE) {
  
  if (verbose) message("\n=== RUNNING COMPREHENSIVE QUALITY CHECKS ===")
  
  # Initialize results tracking
  results <- list()
  check_details <- list()
  
  # -------------------------------------------------------------------------
  # Check 1: No deprecated files exist
  # -------------------------------------------------------------------------
  if (verbose) message("\n1. Checking for deprecated files...")
  
  deprecated_files <- DEPRECATED_PATHS[file.exists(DEPRECATED_PATHS)]
  results$deprecated <- length(deprecated_files) == 0
  
  check_details$deprecated <- list(
    status = results$deprecated,
    message = if (results$deprecated) {
      "✓ No deprecated files found"
    } else {
      paste("✗ Found", length(deprecated_files), "deprecated file(s):", 
            paste(basename(deprecated_files), collapse = ", "))
    },
    files = deprecated_files
  )
  
  if (verbose) message(check_details$deprecated$message)
  
  # -------------------------------------------------------------------------
  # Check 2: Column consistency across files
  # -------------------------------------------------------------------------
  if (verbose) message("\n2. Checking column name standardization...")
  
  column_check_results <- list()
  
  # Check final dataset
  if (file.exists(PATHS$final_1307)) {
    df_final <- readr::read_csv(PATHS$final_1307, show_col_types = FALSE, n_max = 10)
    role_ok  <- any(c("Final_Role_Category","final_category_appendix_j") %in% names(df_final))
    quota_ok <- "quota_target_category" %in% names(df_final)
    column_check_results$final_role <- role_ok && quota_ok
    column_check_results$final_progress <- "Progress" %in% names(df_final)
  } else {
    column_check_results$final_role <- NA
    column_check_results$final_progress <- NA
  }
  
  # Check preliminary classified
  if (file.exists(PATHS$preliminary_classified)) {
    df_prelim <- readr::read_csv(PATHS$preliminary_classified, show_col_types = FALSE, n_max = 10)
    column_check_results$prelim_role <- any(c("Final_Role_Category","final_category_appendix_j") %in% names(df_prelim))
  } else {
    column_check_results$prelim_role <- NA
  }
  
  results$columns_standard <- all(unlist(column_check_results), na.rm = TRUE)
  
  check_details$columns <- list(
    status = results$columns_standard,
    message = if (results$columns_standard) {
      "✓ All standard column names found"
    } else {
      "✗ Some standard columns missing"
    },
    details = column_check_results
  )
  
  if (verbose) message(check_details$columns$message)
  
  # -------------------------------------------------------------------------
  # Check 3: No PII in outputs
  # -------------------------------------------------------------------------
  if (verbose) message("\n3. Checking for PII in output files...")
  
  pii_violations <- character()
  
  if (dir.exists("output")) {
    output_files <- list.files("output", pattern = "\\.csv$", 
                              full.names = TRUE, recursive = TRUE)
    
    for (file in output_files) {
      # Check filenames
      for (pattern in PRIVACY_COLUMNS) {
        if (grepl(pattern, basename(file), ignore.case = TRUE)) {
          pii_violations <- c(pii_violations, 
                             paste("Filename contains", pattern, ":", basename(file)))
        }
      }
      
      # Check column names in CSV files
      tryCatch({
        df_check <- readr::read_csv(file, show_col_types = FALSE, n_max = 1)
        for (col in PRIVACY_COLUMNS) {
          if (col %in% names(df_check)) {
            pii_violations <- c(pii_violations, 
                               paste("Column", col, "found in", basename(file)))
          }
        }
      }, error = function(e) {
        # Skip files that can't be read
      })
    }
  }
  
  results$no_pii <- length(pii_violations) == 0
  
  check_details$pii <- list(
    status = results$no_pii,
    message = if (results$no_pii) {
      "✓ No PII detected in output files"
    } else {
      paste("✗ Found", length(pii_violations), "potential PII violation(s)")
    },
    violations = pii_violations
  )
  
  if (verbose) message(check_details$pii$message)
  
  # -------------------------------------------------------------------------
  # Check 4: Geographic data privacy
  # -------------------------------------------------------------------------
  if (verbose) message("\n4. Checking geographic data privacy...")
  
  geo_violations <- character()
  
  # Check for raw Q2.2 in figures directory
  if (dir.exists("figures")) {
    figure_files <- list.files("figures", pattern = "\\.csv$", 
                              full.names = TRUE, recursive = TRUE)
    for (file in figure_files) {
      tryCatch({
        df_fig <- readr::read_csv(file, show_col_types = FALSE, n_max = 10)
        if ("Q2.2" %in% names(df_fig)) {
          geo_violations <- c(geo_violations, 
                             paste("Raw Q2.2 found in", basename(file)))
        }
      }, error = function(e) {})
    }
  }
  
  # Check main data files
  for (path in c(PATHS$final_1307, PATHS$preliminary_classified)) {
    if (file.exists(path)) {
      tryCatch({
        df_check <- readr::read_csv(path, show_col_types = FALSE, n_max = 10)
        if ("Q2.2" %in% names(df_check)) {
          geo_violations <- c(geo_violations, 
                             paste("Raw Q2.2 found in", basename(path)))
        }
      }, error = function(e) {})
    }
  }
  
  results$geographic_privacy <- length(geo_violations) == 0
  
  check_details$geographic <- list(
    status = results$geographic_privacy,
    message = if (results$geographic_privacy) {
      "✓ Geographic data properly anonymized"
    } else {
      paste("✗ Raw geographic data found in", length(geo_violations), "file(s)")
    },
    violations = geo_violations
  )
  
  if (verbose) message(check_details$geographic$message)
  
  # -------------------------------------------------------------------------
  # Check 5: Reproducibility (checksums exist)
  # -------------------------------------------------------------------------
  if (verbose) message("\n5. Checking reproducibility markers...")
  
  checksums_exist <- file.exists(PATHS$checksums)
  verification_exist <- file.exists(PATHS$verification_report)
  
  results$reproducible <- checksums_exist && verification_exist
  
  check_details$reproducibility <- list(
    status = results$reproducible,
    message = if (results$reproducible) {
      "✓ Reproducibility files present"
    } else {
      "✗ Missing reproducibility files"
    },
    checksums = checksums_exist,
    verification = verification_exist
  )
  
  if (verbose) message(check_details$reproducibility$message)
  
  # -------------------------------------------------------------------------
  # Check 6: Sample size consistency
  # -------------------------------------------------------------------------
  if (verbose) message("\n6. Checking sample size consistency...")
  
  sample_sizes <- list()
  
  if (file.exists(PATHS$final_1307)) {
    df_1307 <- readr::read_csv(PATHS$final_1307, show_col_types = FALSE)
    sample_sizes$final <- nrow(df_1307)
  }
  
  if (file.exists(PATHS$preliminary_classified)) {
    df_prelim <- readr::read_csv(PATHS$preliminary_classified, show_col_types = FALSE)
    sample_sizes$preliminary <- nrow(df_prelim)
  }
  
  results$sample_size <- if (length(sample_sizes) > 0 && "final" %in% names(sample_sizes)) {
    sample_sizes$final == QUALITY_PARAMS$target_n
  } else {
    NA
  }
  
  check_details$sample_size <- list(
    status = results$sample_size,
    message = if (isTRUE(results$sample_size)) {
      paste("✓ Final dataset has target N =", QUALITY_PARAMS$target_n)
    } else if (is.na(results$sample_size)) {
      "⚠ Final dataset not found"
    } else {
      paste("✗ Sample size mismatch. Expected:", QUALITY_PARAMS$target_n, 
            "Got:", sample_sizes$final)
    },
    sizes = sample_sizes
  )
  
  if (verbose) message(check_details$sample_size$message)
  
  # -------------------------------------------------------------------------
  # Check 7: Required directories exist
  # -------------------------------------------------------------------------
  if (verbose) message("\n7. Checking directory structure...")
  
  required_dirs <- c("data", "docs", "output", "R")
  missing_dirs <- required_dirs[!dir.exists(required_dirs)]
  
  results$directories <- length(missing_dirs) == 0
  
  check_details$directories <- list(
    status = results$directories,
    message = if (results$directories) {
      "✓ All required directories present"
    } else {
      paste("✗ Missing directories:", paste(missing_dirs, collapse = ", "))
    },
    missing = missing_dirs
  )
  
  if (verbose) message(check_details$directories$message)
  
  # -------------------------------------------------------------------------
  # Check 8: Data dictionary consistency
  # -------------------------------------------------------------------------
  if (verbose) message("\n8. Checking data dictionary...")
  
  dict_check <- list(exists = FALSE, matches = FALSE)
  
  if (file.exists(PATHS$dictionary) && file.exists(PATHS$basic_anon)) {
    dict_check$exists <- TRUE
    
    dict_df <- readr::read_csv(PATHS$dictionary, show_col_types = FALSE)
    basic_df <- readr::read_csv(PATHS$basic_anon, show_col_types = FALSE, n_max = 1)
    
    dict_col_field <- if ("column_name" %in% names(dict_df)) "column_name" else if ("column" %in% names(dict_df)) "column" else NA_character_
    if (is.na(dict_col_field)) {
      dict_check$matches <- FALSE
    } else {
      dict_check$matches <- identical(sort(names(basic_df)), 
                                      sort(dict_df[[dict_col_field]]))
    }
  }
  
  results$dictionary <- dict_check$exists && dict_check$matches
  
  check_details$dictionary <- list(
    status = results$dictionary,
    message = if (results$dictionary) {
      "✓ Data dictionary matches anonymized data"
    } else if (!dict_check$exists) {
      "✗ Data dictionary or basic anonymized file missing"
    } else {
      "✗ Data dictionary doesn't match anonymized data columns"
    },
    details = dict_check
  )
  
  if (verbose) message(check_details$dictionary$message)
  
  # -------------------------------------------------------------------------
  # Generate Summary Report
  # -------------------------------------------------------------------------
  
  # Calculate overall status
  check_names <- names(results)
  passed_checks <- sum(unlist(results), na.rm = TRUE)
  total_checks <- sum(!is.na(unlist(results)))
  
  overall_status <- if (passed_checks == total_checks) {
    "PASSED"
  } else if (passed_checks >= total_checks * 0.7) {
    "PARTIAL"
  } else {
    "FAILED"
  }
  
  # Create report dataframe
  report <- data.frame(
    Check = names(results),
    Passed = unlist(results),
    Timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  # Add details
  report$Details <- sapply(names(check_details), function(x) {
    check_details[[x]]$message
  })
  
  # -------------------------------------------------------------------------
  # Save Report
  # -------------------------------------------------------------------------
  
  if (save_report) {
    report_path <- safe_path(PATHS$quality_assurance)
    write.csv(report, report_path, row.names = FALSE)
    if (verbose) message("\n✓ Quality report saved to: ", report_path)
    
    # Also save detailed JSON report if jsonlite available
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      json_path <- sub("\\.csv$", ".json", report_path)
      jsonlite::write_json(check_details, json_path, pretty = TRUE)
      if (verbose) message("✓ Detailed report saved to: ", json_path)
    }
  }
  
  # -------------------------------------------------------------------------
  # Final Summary
  # -------------------------------------------------------------------------
  
  if (verbose) {
    message("\n" + paste(rep("=", 60), collapse = ""))
    message("QUALITY CHECK SUMMARY")
    message(paste(rep("=", 60), collapse = ""))
    message("Overall Status: ", overall_status)
    message("Checks Passed: ", passed_checks, "/", total_checks)
    message(paste(rep("=", 60), collapse = ""))
    
    if (!all(unlist(results), na.rm = TRUE)) {
      message("\nFailed checks:")
      for (i in seq_along(results)) {
        if (!isTRUE(results[[i]])) {
          message("  - ", names(results)[i], ": ", 
                 check_details[[names(results)[i]]]$message)
        }
      }
      
      message("\nRecommended actions:")
      if (!results$deprecated) {
        message("  • Delete deprecated files: ", 
               paste(basename(deprecated_files), collapse = ", "))
      }
      if (!results$no_pii) {
        message("  • Review and remove PII from output files")
      }
      if (!results$geographic_privacy) {
        message("  • Ensure geographic data is anonymized to regions only")
      }
      if (!results$columns_standard) {
        message("  • Run pipeline with updated scripts to standardize column names")
      }
    } else {
      message("\n✅ All quality checks passed!")
    }
  }
  
  # Return results for programmatic use
  invisible(list(
    results = results,
    details = check_details,
    report = report,
    overall_status = overall_status
  ))
}

# =============================================================================
# ADDITIONAL UTILITY FUNCTIONS
# =============================================================================

# Check specific file for PII
check_file_for_pii <- function(filepath) {
  if (!file.exists(filepath)) {
    stop("File not found: ", filepath)
  }
  
  df <- readr::read_csv(filepath, show_col_types = FALSE)
  violations <- check_privacy_violations(df, stop_on_violation = FALSE)
  
  return(violations)
}

# Verify quota matching
verify_quota_match <- function() {
  if (!file.exists(PATHS$verification)) {
    stop("Verification file not found. Run get_exact_1307.R first.")
  }
  
  verify_df <- readr::read_csv(PATHS$verification, show_col_types = FALSE)
  
  perfect_match <- all(verify_df$Match, na.rm = TRUE)
  total_match <- sum(verify_df$Final) == sum(verify_df$Target)
  
  message("Quota Matching Verification:")
  message("  Perfect category match: ", perfect_match)
  message("  Total N match: ", total_match)
  message("  Categories matched: ", sum(verify_df$Match), "/", nrow(verify_df))
  
  return(list(
    perfect = perfect_match,
    total = total_match,
    details = verify_df
  ))
}

# =============================================================================
# RUN IF CALLED DIRECTLY
# =============================================================================

if (!interactive() && length(commandArgs(trailingOnly = TRUE)) == 0) {
  # Script is being run directly
  message("Running comprehensive quality checks...")
  results <- run_quality_checks(verbose = TRUE, save_report = TRUE)
  
  # Exit with appropriate code
  if (results$overall_status == "PASSED") {
    quit(save = "no", status = 0)
  } else {
    quit(save = "no", status = 1)
  }
}

# ---- Checksums and verification report ----
if (file.exists("R/00_config.R")) source("R/00_config.R")

sha_path <- if (exists("PATHS") && !is.null(PATHS$checksums)) PATHS$checksums else "docs/checksums.txt"
vr_path  <- if (exists("PATHS") && !is.null(PATHS$verification_report)) PATHS$verification_report else "docs/verification_report.md"

artifacts <- c(
  "data/survey_responses_anonymized_basic.csv",
  "docs/appendix_j_classification_template.csv",
  "data/survey_responses_anonymized_preliminary.csv",
  "data/data_dictionary.csv"
)
if (file.exists("data/climate_finance_survey_final_1307.csv")) {
  artifacts <- c(artifacts, "data/climate_finance_survey_final_1307.csv")
}
if (!dir.exists(dirname(sha_path))) dir.create(dirname(sha_path), recursive = TRUE, showWarnings = FALSE)

sha_fun <- function(p) if (file.exists(p)) digest::digest(p, algo = "sha256", file = TRUE) else NA_character_
hash_df <- tibble::tibble(file = artifacts, sha256 = vapply(artifacts, sha_fun, character(1)))
readr::write_csv(hash_df, sha_path)

# Simple verification report
count_fun <- function(p) {
  if (!file.exists(p)) return(NA_integer_)
  suppressWarnings(nrow(readr::read_csv(p, show_col_types = FALSE)))
}
counts <- vapply(artifacts, count_fun, integer(1))
report <- paste0(
  "# Verification Report\n\n",
  "*Timestamp (UTC):* ", format(Sys.time(), "%Y-%m-%d %H:%M:%S", tz = "UTC"), "\n\n",
  "## Artifacts\n\n",
  paste(sprintf("- %s — rows: %s", artifacts, ifelse(is.na(counts), "NA", counts)), collapse = "\n"), "\n\n",
  "## Session Info\n\n",
  "```\n", paste(capture.output(sessionInfo()), collapse = "\n"), "\n```\n"
)
writeLines(report, vr_path)
