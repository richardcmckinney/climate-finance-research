#!/usr/bin/env Rscript
# R/99_quality_checks.R - Comprehensive quality assurance checks
# Purpose: Run comprehensive quality checks on pipeline outputs with enhanced diagnostics
# Version: 2.1
# Author: Pipeline QA System
# 
# ENHANCEMENTS:
# - Added cli package for better formatted output
# - Created report_check function for consistent diagnostic reporting
# - Enhanced all checks with detailed diagnostic information
# - Fixed write_csv to include na = "" parameter
# - Added comprehensive sample size and distribution checks
# - Improved error handling and reporting throughout
# - Fixed docstring syntax error (replaced Python-style with R comments)

suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
  library(cli)  # Added for better output formatting
})

# Source central configuration
source("R/00_config.R")

# Source Appendix J configuration for role column names
source("R/appendix_j_config.R")

# =============================================================================
# ENHANCED REPORTING FUNCTION
# =============================================================================

report_check <- function(name, status, message_ok, message_fail, details = NULL, verbose = TRUE) {
  # Enhanced reporting function for consistent diagnostic output
  # 
  # Args:
  #   name: Name of the check
  #   status: Boolean result of the check
  #   message_ok: Message to display on success
  #   message_fail: Message to display on failure
  #   details: Additional details to display (optional)
  #   verbose: Whether to print output
  # 
  # Returns:
  #   List with check results and diagnostic information
  
  if (verbose) {
    if (status) {
      cli_alert_success("{name}: {message_ok}")
    } else {
      cli_alert_danger("{name}: {message_fail}")
      if (!is.null(details)) {
        if (is.list(details)) {
          for (key in names(details)) {
            cli_alert_info("  {key}: {details[[key]]}")
          }
        } else {
          cli_alert_info("  Details: {details}")
        }
      }
    }
  }
  
  return(list(
    name = name,
    status = status,
    message = if(status) message_ok else message_fail,
    details = details,
    timestamp = Sys.time()
  ))
}

# =============================================================================
# MAIN QUALITY CHECK FUNCTION WITH ENHANCED DIAGNOSTICS
# =============================================================================

run_quality_checks <- function(verbose = TRUE, save_report = TRUE) {
  
  if (verbose) {
    cli_h1("RUNNING COMPREHENSIVE QUALITY CHECKS")
    cli_alert_info("Starting at {Sys.time()}")
  }
  
  # Initialize results tracking
  results <- list()
  check_details <- list()
  
  # -------------------------------------------------------------------------
  # Check 1: No deprecated files exist
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("1. Checking for deprecated files")
  
  deprecated_files <- DEPRECATED_PATHS[file.exists(DEPRECATED_PATHS)]
  results$deprecated <- length(deprecated_files) == 0
  
  check_details$deprecated <- report_check(
    name = "Deprecated Files",
    status = results$deprecated,
    message_ok = "No deprecated files found",
    message_fail = paste("Found", length(deprecated_files), "deprecated file(s)"),
    details = if (!results$deprecated) {
      list(
        count = length(deprecated_files),
        files = paste(basename(deprecated_files), collapse = ", "),
        full_paths = deprecated_files
      )
    } else NULL,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 2: Column consistency across files with enhanced diagnostics
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("2. Checking column name standardization")
  
  column_check_results <- list()
  column_diagnostics <- list()
  
  # Check final dataset for standard role column using configuration
  if (file.exists(PATHS$final_1307)) {
    df_final <- readr::read_csv(PATHS$final_1307, show_col_types = FALSE, n_max = 10)
    
    # Check for role columns
    role_ok <- any(APPENDIX_J_CONFIG$role_column_candidates %in% names(df_final))
    column_check_results$final_role <- role_ok
    
    # Enhanced diagnostics for role column
    if (role_ok) {
      found_role_cols <- APPENDIX_J_CONFIG$role_column_candidates[
        APPENDIX_J_CONFIG$role_column_candidates %in% names(df_final)
      ]
      column_diagnostics$final_role <- list(
        status = "found",
        columns = found_role_cols,
        expected = APPENDIX_J_CONFIG$role_column_candidates
      )
    } else {
      column_diagnostics$final_role <- list(
        status = "missing",
        expected = APPENDIX_J_CONFIG$role_column_candidates,
        actual = names(df_final)[1:min(10, length(names(df_final)))]
      )
    }
    
    # Check for Progress column
    column_check_results$final_progress <- "Progress" %in% names(df_final)
    column_diagnostics$final_progress <- list(
      status = if (column_check_results$final_progress) "found" else "missing",
      all_columns = length(names(df_final))
    )
  } else {
    column_check_results$final_role <- NA
    column_check_results$final_progress <- NA
    column_diagnostics$final_missing <- "File not found"
  }
  
  # Check preliminary classified for standard role column
  if (file.exists(PATHS$preliminary_classified)) {
    df_prelim <- readr::read_csv(PATHS$preliminary_classified, show_col_types = FALSE, n_max = 10)
    column_check_results$prelim_role <- any(APPENDIX_J_CONFIG$role_column_candidates %in% names(df_prelim))
    
    if (column_check_results$prelim_role) {
      found_prelim_cols <- APPENDIX_J_CONFIG$role_column_candidates[
        APPENDIX_J_CONFIG$role_column_candidates %in% names(df_prelim)
      ]
      column_diagnostics$prelim_role <- list(
        status = "found",
        columns = found_prelim_cols
      )
    } else {
      column_diagnostics$prelim_role <- list(
        status = "missing",
        expected = APPENDIX_J_CONFIG$role_column_candidates,
        actual_sample = names(df_prelim)[1:min(5, length(names(df_prelim)))]
      )
    }
  } else {
    column_check_results$prelim_role <- NA
    column_diagnostics$prelim_missing <- "File not found"
  }
  
  # Overall column standardization status
  check_results_bool <- column_check_results[!grepl("_column$", names(column_check_results))]
  results$columns_standard <- all(unlist(check_results_bool), na.rm = TRUE)
  
  check_details$columns <- report_check(
    name = "Column Standardization",
    status = results$columns_standard,
    message_ok = "All standard column names found",
    message_fail = "Some standard columns missing",
    details = column_diagnostics,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 3: No PII in outputs with enhanced reporting
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("3. Checking for PII in output files")
  
  pii_violations <- list()
  pii_summary <- list(
    files_checked = 0,
    violations_found = 0,
    violation_types = list()
  )
  
  if (dir.exists("output")) {
    output_files <- list.files("output", pattern = "\\.csv$", 
                              full.names = TRUE, recursive = TRUE)
    pii_summary$files_checked <- length(output_files)
    
    for (file in output_files) {
      # Check filenames
      for (pattern in PRIVACY_COLUMNS) {
        if (grepl(pattern, basename(file), ignore.case = TRUE)) {
          pii_violations[[length(pii_violations) + 1]] <- list(
            type = "filename",
            pattern = pattern,
            file = basename(file),
            full_path = file
          )
          pii_summary$violations_found <- pii_summary$violations_found + 1
          pii_summary$violation_types$filename <- (pii_summary$violation_types$filename %||% 0) + 1
        }
      }
      
      # Check column names in CSV files
      tryCatch({
        df_check <- readr::read_csv(file, show_col_types = FALSE, n_max = 1)
        for (col in PRIVACY_COLUMNS) {
          if (col %in% names(df_check)) {
            pii_violations[[length(pii_violations) + 1]] <- list(
              type = "column",
              column = col,
              file = basename(file),
              full_path = file
            )
            pii_summary$violations_found <- pii_summary$violations_found + 1
            pii_summary$violation_types$column <- (pii_summary$violation_types$column %||% 0) + 1
          }
        }
      }, error = function(e) {
        # Log file read errors
        pii_summary$read_errors <- c(pii_summary$read_errors, basename(file))
      })
    }
  }
  
  results$no_pii <- length(pii_violations) == 0
  
  check_details$pii <- report_check(
    name = "PII Privacy Check",
    status = results$no_pii,
    message_ok = paste("No PII detected in", pii_summary$files_checked, "output files"),
    message_fail = paste("Found", pii_summary$violations_found, "potential PII violation(s)"),
    details = if (!results$no_pii) pii_summary else NULL,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 4: Geographic data privacy with detailed diagnostics
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("4. Checking geographic data privacy")
  
  geo_violations <- list()
  geo_summary <- list(
    directories_checked = 0,
    files_checked = 0,
    violations = 0
  )
  
  # Check for raw Q2.2 in figures directory
  if (dir.exists("figures")) {
    geo_summary$directories_checked <- geo_summary$directories_checked + 1
    figure_files <- list.files("figures", pattern = "\\.csv$", 
                              full.names = TRUE, recursive = TRUE)
    geo_summary$files_checked <- geo_summary$files_checked + length(figure_files)
    
    for (file in figure_files) {
      tryCatch({
        df_fig <- readr::read_csv(file, show_col_types = FALSE, n_max = 10)
        if ("Q2.2" %in% names(df_fig)) {
          geo_violations[[length(geo_violations) + 1]] <- list(
            location = "figures",
            file = basename(file),
            column = "Q2.2",
            full_path = file
          )
          geo_summary$violations <- geo_summary$violations + 1
        }
      }, error = function(e) {})
    }
  }
  
  # Check main data files
  for (path in c(PATHS$final_1307, PATHS$preliminary_classified)) {
    if (file.exists(path)) {
      geo_summary$files_checked <- geo_summary$files_checked + 1
      tryCatch({
        df_check <- readr::read_csv(path, show_col_types = FALSE, n_max = 10)
        if ("Q2.2" %in% names(df_check)) {
          geo_violations[[length(geo_violations) + 1]] <- list(
            location = "main_data",
            file = basename(path),
            column = "Q2.2",
            full_path = path
          )
          geo_summary$violations <- geo_summary$violations + 1
        }
      }, error = function(e) {})
    }
  }
  
  results$geographic_privacy <- length(geo_violations) == 0
  
  check_details$geographic <- report_check(
    name = "Geographic Privacy",
    status = results$geographic_privacy,
    message_ok = paste("Geographic data properly anonymized across", geo_summary$files_checked, "files"),
    message_fail = paste("Raw geographic data found in", geo_summary$violations, "file(s)"),
    details = if (!results$geographic_privacy) geo_summary else NULL,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 5: Reproducibility with enhanced tracking
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("5. Checking reproducibility markers")
  
  checksums_exist <- file.exists(PATHS$checksums)
  verification_exist <- file.exists(PATHS$verification_report)
  
  reproducibility_details <- list(
    checksums = list(
      exists = checksums_exist,
      path = PATHS$checksums,
      modified = if (checksums_exist) file.mtime(PATHS$checksums) else NA
    ),
    verification = list(
      exists = verification_exist,
      path = PATHS$verification_report,
      modified = if (verification_exist) file.mtime(PATHS$verification_report) else NA
    )
  )
  
  results$reproducible <- checksums_exist && verification_exist
  
  check_details$reproducibility <- report_check(
    name = "Reproducibility Files",
    status = results$reproducible,
    message_ok = "All reproducibility files present",
    message_fail = paste("Missing:", 
                        if (!checksums_exist) "checksums" else NULL,
                        if (!verification_exist) "verification" else NULL),
    details = reproducibility_details,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 6: Enhanced sample size consistency with distribution check
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("6. Checking sample size consistency and quota distribution")
  
  sample_diagnostics <- list()
  
  if (file.exists(PATHS$final_1307)) {
    df_final <- readr::read_csv(PATHS$final_1307, show_col_types = FALSE)
    n_final <- nrow(df_final)
    
    # Check exact count
    sample_diagnostics$final_count <- list(
      actual = n_final,
      expected = QUALITY_PARAMS$target_n,
      difference = n_final - QUALITY_PARAMS$target_n
    )
    
    results$sample_size <- n_final == QUALITY_PARAMS$target_n
    
    # Check quota distribution if quota_target_category exists
    if ("quota_target_category" %in% names(df_final)) {
      dist_check <- df_final %>%
        count(quota_target_category, name = "actual") %>%
        full_join(
          get_appendix_j_target() %>% 
            rename(quota_target_category = Category, expected = Target),
          by = "quota_target_category"
        ) %>%
        mutate(
          difference = actual - expected,
          pct_diff = round((actual - expected) / expected * 100, 2)
        )
      
      all_exact <- all(dist_check$difference == 0, na.rm = TRUE)
      total_diff <- sum(abs(dist_check$difference), na.rm = TRUE)
      
      sample_diagnostics$distribution <- list(
        all_exact = all_exact,
        total_discrepancy = total_diff,
        categories_off = sum(dist_check$difference != 0, na.rm = TRUE),
        max_deviation = max(abs(dist_check$difference), na.rm = TRUE),
        details = dist_check %>% 
          filter(difference != 0) %>%
          select(quota_target_category, actual, expected, difference, pct_diff)
      )
      
      results$distribution_match <- all_exact
    } else {
      sample_diagnostics$distribution <- list(
        status = "quota_target_category column not found",
        columns_available = names(df_final)[1:min(10, length(names(df_final)))]
      )
      results$distribution_match <- NA
    }
  } else {
    results$sample_size <- NA
    results$distribution_match <- NA
    sample_diagnostics$error <- "Final dataset not found"
  }
  
  # Check preliminary file sample size
  if (file.exists(PATHS$preliminary_classified)) {
    df_prelim <- readr::read_csv(PATHS$preliminary_classified, show_col_types = FALSE)
    sample_diagnostics$preliminary_count <- nrow(df_prelim)
  }
  
  check_details$sample_size <- report_check(
    name = "Sample Size & Distribution",
    status = isTRUE(results$sample_size) && isTRUE(results$distribution_match),
    message_ok = paste("Final dataset has exactly N =", QUALITY_PARAMS$target_n, "with perfect quota match"),
    message_fail = if (is.na(results$sample_size)) {
      "Final dataset not found"
    } else if (!results$sample_size) {
      paste("Sample size mismatch (difference:", sample_diagnostics$final_count$difference, ")")
    } else if (!isTRUE(results$distribution_match)) {
      paste("Quota distribution mismatch (", 
            sample_diagnostics$distribution$categories_off, "categories off)")
    } else "Unknown issue",
    details = sample_diagnostics,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 7: Required directories with permissions check
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("7. Checking directory structure and permissions")
  
  required_dirs <- c("data", "docs", "output", "R")
  dir_diagnostics <- list()
  
  for (dir in required_dirs) {
    if (dir.exists(dir)) {
      dir_diagnostics[[dir]] <- list(
        exists = TRUE,
        writable = file.access(dir, 2) == 0,
        files = length(list.files(dir, recursive = TRUE))
      )
    } else {
      dir_diagnostics[[dir]] <- list(
        exists = FALSE,
        writable = NA,
        files = 0
      )
    }
  }
  
  missing_dirs <- names(dir_diagnostics)[!sapply(dir_diagnostics, function(x) x$exists)]
  results$directories <- length(missing_dirs) == 0
  
  check_details$directories <- report_check(
    name = "Directory Structure",
    status = results$directories,
    message_ok = paste("All", length(required_dirs), "required directories present"),
    message_fail = paste("Missing", length(missing_dirs), "directories"),
    details = if (length(missing_dirs) > 0) {
      list(
        missing = missing_dirs,
        all_dirs = dir_diagnostics
      )
    } else NULL,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 8: Data dictionary consistency with detailed comparison
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("8. Checking data dictionary completeness")
  
  dict_diagnostics <- list()
  
  if (file.exists(PATHS$dictionary) && file.exists(PATHS$basic_anon)) {
    dict_diagnostics$files_exist <- TRUE
    
    dict_df <- readr::read_csv(PATHS$dictionary, show_col_types = FALSE)
    basic_df <- readr::read_csv(PATHS$basic_anon, show_col_types = FALSE, n_max = 1)
    
    # Find the column name field in dictionary
    dict_col_field <- if ("column_name" %in% names(dict_df)) {
      "column_name"
    } else if ("column" %in% names(dict_df)) {
      "column"
    } else {
      NA_character_
    }
    
    if (!is.na(dict_col_field)) {
      dict_cols <- sort(dict_df[[dict_col_field]])
      data_cols <- sort(names(basic_df))
      
      dict_diagnostics$match <- identical(dict_cols, data_cols)
      dict_diagnostics$dict_columns <- length(dict_cols)
      dict_diagnostics$data_columns <- length(data_cols)
      
      if (!dict_diagnostics$match) {
        dict_diagnostics$missing_from_dict <- setdiff(data_cols, dict_cols)
        dict_diagnostics$extra_in_dict <- setdiff(dict_cols, data_cols)
      }
    } else {
      dict_diagnostics$match <- FALSE
      dict_diagnostics$error <- "Dictionary column field not found"
    }
  } else {
    dict_diagnostics$files_exist <- FALSE
    dict_diagnostics$match <- FALSE
    dict_diagnostics$missing_files <- c(
      if (!file.exists(PATHS$dictionary)) "dictionary",
      if (!file.exists(PATHS$basic_anon)) "basic_anon"
    )
  }
  
  results$dictionary <- dict_diagnostics$files_exist && dict_diagnostics$match
  
  check_details$dictionary <- report_check(
    name = "Data Dictionary",
    status = results$dictionary,
    message_ok = paste("Data dictionary matches anonymized data (",
                      dict_diagnostics$data_columns, "columns)"),
    message_fail = if (!dict_diagnostics$files_exist) {
      "Required files missing"
    } else if (!dict_diagnostics$match) {
      "Dictionary doesn't match data columns"
    } else "Unknown issue",
    details = dict_diagnostics,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Check 9: Appendix J configuration validity with comprehensive check
  # -------------------------------------------------------------------------
  if (verbose) cli_h2("9. Checking Appendix J configuration integrity")
  
  appendix_j_diagnostics <- list()
  
  appendix_j_diagnostics$config_exists <- exists("APPENDIX_J_CONFIG")
  
  if (appendix_j_diagnostics$config_exists) {
    appendix_j_diagnostics$structure <- list(
      has_role_candidates = !is.null(APPENDIX_J_CONFIG$role_column_candidates),
      candidates_count = length(APPENDIX_J_CONFIG$role_column_candidates),
      has_mappings = !is.null(APPENDIX_J_CONFIG$role_mappings),
      mapping_count = if (!is.null(APPENDIX_J_CONFIG$role_mappings)) {
        length(APPENDIX_J_CONFIG$role_mappings)
      } else 0,
      candidates = APPENDIX_J_CONFIG$role_column_candidates
    )
    
    # Check if target function exists
    appendix_j_diagnostics$target_function <- exists("get_appendix_j_target")
    
    # Validate mappings structure
    if (appendix_j_diagnostics$structure$has_mappings) {
      appendix_j_diagnostics$mappings_valid <- all(
        sapply(APPENDIX_J_CONFIG$role_mappings, function(x) {
          is.character(x) && length(x) > 0
        })
      )
    }
  } else {
    appendix_j_diagnostics$structure <- list(
      error = "APPENDIX_J_CONFIG not found in environment"
    )
  }
  
  results$appendix_j_config <- appendix_j_diagnostics$config_exists && 
                               appendix_j_diagnostics$structure$has_role_candidates && 
                               appendix_j_diagnostics$structure$candidates_count > 0
  
  check_details$appendix_j <- report_check(
    name = "Appendix J Configuration",
    status = results$appendix_j_config,
    message_ok = paste("Configuration valid with", 
                      appendix_j_diagnostics$structure$candidates_count, 
                      "role column candidate(s)"),
    message_fail = if (!appendix_j_diagnostics$config_exists) {
      "APPENDIX_J_CONFIG not found"
    } else {
      "Configuration missing required elements"
    },
    details = appendix_j_diagnostics,
    verbose = verbose
  )
  
  # -------------------------------------------------------------------------
  # Generate Summary Report with Enhanced Metrics
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
  
  # Create enhanced report dataframe
  report <- data.frame(
    Check = names(results),
    Passed = unlist(results),
    Timestamp = Sys.time(),
    stringsAsFactors = FALSE
  )
  
  # Add details and diagnostics
  report$Details <- sapply(names(check_details), function(x) {
    if (!is.null(check_details[[x]]$message)) {
      check_details[[x]]$message
    } else {
      "No details available"
    }
  })
  
  # -------------------------------------------------------------------------
  # Save Report with Enhanced Format
  # -------------------------------------------------------------------------
  
  if (save_report) {
    report_path <- safe_path(PATHS$quality_assurance)
    
    # Save CSV report with na parameter fixed
    readr::write_csv(report, report_path, na = "")
    
    if (verbose) {
      cli_alert_success("Quality report saved to: {report_path}")
    }
    
    # Save detailed JSON report if jsonlite available
    if (requireNamespace("jsonlite", quietly = TRUE)) {
      json_path <- sub("\\.csv$", ".json", report_path)
      
      # Create comprehensive JSON report
      json_report <- list(
        summary = list(
          overall_status = overall_status,
          checks_passed = passed_checks,
          total_checks = total_checks,
          pass_rate = round(passed_checks / total_checks * 100, 2),
          timestamp = Sys.time(),
          r_version = R.version.string
        ),
        results = results,
        details = check_details,
        recommendations = if (overall_status != "PASSED") {
          generate_recommendations(results, check_details)
        } else NULL
      )
      
      jsonlite::write_json(json_report, json_path, pretty = TRUE)
      
      if (verbose) {
        cli_alert_success("Detailed JSON report saved to: {json_path}")
      }
    }
  }
  
  # -------------------------------------------------------------------------
  # Final Summary with Enhanced Output
  # -------------------------------------------------------------------------
  
  if (verbose) {
    cli_rule(left = "QUALITY CHECK SUMMARY", right = overall_status)
    
    # Status indicator with color
    if (overall_status == "PASSED") {
      cli_alert_success("Overall Status: {.strong {overall_status}}")
      cli_alert_success("All {total_checks} checks passed!")
    } else if (overall_status == "PARTIAL") {
      cli_alert_warning("Overall Status: {.strong {overall_status}}")
      cli_alert_info("Checks Passed: {passed_checks}/{total_checks}")
    } else {
      cli_alert_danger("Overall Status: {.strong {overall_status}}")
      cli_alert_danger("Checks Passed: {passed_checks}/{total_checks}")
    }
    
    if (!all(unlist(results), na.rm = TRUE)) {
      cli_h3("Failed Checks")
      
      for (i in seq_along(results)) {
        if (!isTRUE(results[[i]])) {
          check_name <- names(results)[i]
          cli_alert_danger("{check_name}: {check_details[[check_name]]$message}")
        }
      }
      
      # Generate and display recommendations
      recommendations <- generate_recommendations(results, check_details)
      if (length(recommendations) > 0) {
        cli_h3("Recommended Actions")
        for (rec in recommendations) {
          cli_alert_info(rec)
        }
      }
    }
    
    cli_rule()
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
# ENHANCED UTILITY FUNCTIONS
# =============================================================================

# Generate recommendations based on failed checks
generate_recommendations <- function(results, details) {
  recommendations <- c()
  
  if (!isTRUE(results$deprecated)) {
    recommendations <- c(recommendations, 
      "Delete deprecated files from the project")
  }
  
  if (!isTRUE(results$no_pii)) {
    recommendations <- c(recommendations,
      "Review and remove PII columns from output files",
      "Run privacy sanitization on all CSV outputs")
  }
  
  if (!isTRUE(results$geographic_privacy)) {
    recommendations <- c(recommendations,
      "Replace Q2.2 with region_name in all output files",
      "Ensure geographic data is aggregated to region level")
  }
  
  if (!isTRUE(results$columns_standard)) {
    recommendations <- c(recommendations,
      "Re-run pipeline with updated column standardization",
      "Check appendix_j_config.R for correct role column names")
  }
  
  if (!isTRUE(results$sample_size)) {
    recommendations <- c(recommendations,
      "Verify quota sampling in get_exact_1307.R",
      "Check for data filtering issues in pipeline")
  }
  
  if (!isTRUE(results$reproducible)) {
    recommendations <- c(recommendations,
      "Run generate_checksums() to create checksum file",
      "Run generate_verification_report() for documentation")
  }
  
  if (!isTRUE(results$dictionary)) {
    recommendations <- c(recommendations,
      "Update data dictionary to match current data structure",
      "Regenerate dictionary using create_data_dictionary.R")
  }
  
  return(recommendations)
}

# Enhanced PII checking for specific file
check_file_for_pii <- function(filepath, verbose = TRUE) {
  if (!file.exists(filepath)) {
    cli_alert_danger("File not found: {filepath}")
    stop("File not found: ", filepath)
  }
  
  if (verbose) {
    cli_h2("Checking {basename(filepath)} for PII")
  }
  
  df <- readr::read_csv(filepath, show_col_types = FALSE)
  violations <- list()
  
  # Check columns
  for (col in PRIVACY_COLUMNS) {
    if (col %in% names(df)) {
      violations[[length(violations) + 1]] <- list(
        type = "column",
        column = col,
        file = basename(filepath)
      )
    }
  }
  
  # Check for email patterns in string columns
  string_cols <- names(df)[sapply(df, is.character)]
  email_pattern <- "[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\\.[a-zA-Z]{2,}"
  
  for (col in string_cols) {
    if (any(grepl(email_pattern, df[[col]], ignore.case = TRUE))) {
      violations[[length(violations) + 1]] <- list(
        type = "potential_email",
        column = col,
        file = basename(filepath)
      )
    }
  }
  
  if (verbose) {
    if (length(violations) == 0) {
      cli_alert_success("No PII violations found")
    } else {
      cli_alert_danger("Found {length(violations)} potential PII violation(s)")
      for (v in violations) {
        cli_alert_info("  {v$type} in column: {v$column}")
      }
    }
  }
  
  return(violations)
}

# Enhanced quota verification
verify_quota_match <- function(verbose = TRUE) {
  if (!file.exists(PATHS$verification)) {
    cli_alert_danger("Verification file not found. Run get_exact_1307.R first.")
    stop("Verification file not found")
  }
  
  if (verbose) {
    cli_h2("Quota Matching Verification")
  }
  
  verify_df <- readr::read_csv(PATHS$verification, show_col_types = FALSE)
  
  perfect_match <- all(verify_df$Match, na.rm = TRUE)
  total_match <- sum(verify_df$Final) == sum(verify_df$Target)
  categories_matched <- sum(verify_df$Match)
  total_categories <- nrow(verify_df)
  
  # Calculate detailed statistics
  max_deviation <- max(abs(verify_df$Final - verify_df$Target), na.rm = TRUE)
  total_deviation <- sum(abs(verify_df$Final - verify_df$Target), na.rm = TRUE)
  
  if (verbose) {
    cli_alert_info("Perfect category match: {perfect_match}")
    cli_alert_info("Total N match: {total_match}")
    cli_alert_info("Categories matched: {categories_matched}/{total_categories}")
    cli_alert_info("Maximum deviation: {max_deviation}")
    cli_alert_info("Total deviation: {total_deviation}")
    
    if (!perfect_match) {
      cli_h3("Categories with mismatches:")
      mismatches <- verify_df[!verify_df$Match, ]
      for (i in seq_len(nrow(mismatches))) {
        row <- mismatches[i, ]
        cli_alert_warning("  {row$Category}: {row$Final} (target: {row$Target})")
      }
    }
  }
  
  return(list(
    perfect = perfect_match,
    total = total_match,
    categories_matched = categories_matched,
    total_categories = total_categories,
    max_deviation = max_deviation,
    total_deviation = total_deviation,
    details = verify_df
  ))
}

# Enhanced checksum generation with fixed na parameter
generate_checksums <- function(save_to_file = TRUE, verbose = TRUE) {
  if (verbose) {
    cli_h2("Generating checksums for reproducibility")
  }
  
  # Define artifacts to checksum
  artifacts <- c(
    PATHS$basic_anon,
    PATHS$classification_template,
    PATHS$preliminary_classified,
    PATHS$dictionary
  )
  
  # Add final dataset if it exists
  if (file.exists(PATHS$final_1307)) {
    artifacts <- c(artifacts, PATHS$final_1307)
  }
  
  # Calculate checksums with progress
  checksums <- list()
  
  for (i in seq_along(artifacts)) {
    path <- artifacts[i]
    if (verbose) {
      cli_progress_step("Processing {basename(path)}")
    }
    
    if (file.exists(path)) {
      checksums[[i]] <- list(
        file = path,
        sha256 = digest::digest(path, algo = "sha256", file = TRUE),
        size = file.size(path),
        modified = file.mtime(path)
      )
    } else {
      checksums[[i]] <- list(
        file = path,
        sha256 = NA_character_,
        size = NA_real_,
        modified = NA
      )
    }
  }
  
  # Create dataframe
  hash_df <- do.call(rbind, lapply(checksums, as.data.frame))
  
  # Save if requested - WITH na PARAMETER FIX
  if (save_to_file) {
    sha_path <- safe_path(PATHS$checksums)
    readr::write_csv(hash_df, sha_path, na = "")  # FIX APPLIED HERE
    
    if (verbose) {
      cli_alert_success("Checksums saved to: {sha_path}")
      cli_alert_info("Total files checksummed: {sum(!is.na(hash_df$sha256))}")
    }
  }
  
  return(hash_df)
}

# Enhanced verification report generation
generate_verification_report <- function(save_to_file = TRUE, verbose = TRUE) {
  if (verbose) {
    cli_h2("Generating verification report")
  }
  
  # Count rows in each artifact with detailed tracking
  artifact_info <- list()
  
  artifacts <- c(
    "Basic Anonymized" = PATHS$basic_anon,
    "Classification Template" = PATHS$classification_template,
    "Preliminary Classified" = PATHS$preliminary_classified,
    "Data Dictionary" = PATHS$dictionary
  )
  
  if (file.exists(PATHS$final_1307)) {
    artifacts["Final 1307"] <- PATHS$final_1307
  }
  
  for (name in names(artifacts)) {
    path <- artifacts[name]
    if (file.exists(path)) {
      df <- suppressWarnings(readr::read_csv(path, show_col_types = FALSE))
      artifact_info[[name]] <- list(
        rows = nrow(df),
        columns = ncol(df),
        size = format(file.size(path), big.mark = ","),
        modified = format(file.mtime(path), "%Y-%m-%d %H:%M:%S")
      )
    } else {
      artifact_info[[name]] <- list(
        rows = NA,
        columns = NA,
        size = "Not found",
        modified = NA
      )
    }
  }
  
  # Create detailed report content
  report <- paste0(
    "# Pipeline Verification Report\n\n",
    "*Generated:* ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"), "\n",
    "*R Version:* ", R.version.string, "\n\n",
    "## Artifacts Summary\n\n",
    "| Artifact | Rows | Columns | Size | Modified |\n",
    "|----------|------|---------|------|----------|\n",
    paste(sapply(names(artifact_info), function(name) {
      info <- artifact_info[[name]]
      sprintf("| %s | %s | %s | %s | %s |",
              name,
              ifelse(is.na(info$rows), "N/A", format(info$rows, big.mark = ",")),
              ifelse(is.na(info$columns), "N/A", info$columns),
              info$size,
              ifelse(is.na(info$modified), "N/A", info$modified))
    }), collapse = "\n"), "\n\n",
    "## Package Versions\n\n",
    "```\n",
    paste(capture.output(sessionInfo()$otherPkgs), collapse = "\n"), "\n",
    "```\n\n",
    "## System Information\n\n",
    "```\n",
    paste(capture.output(Sys.info()), collapse = "\n"), "\n",
    "```\n"
  )
  
  # Save if requested
  if (save_to_file) {
    vr_path <- safe_path(PATHS$verification_report)
    writeLines(report, vr_path)
    
    if (verbose) {
      cli_alert_success("Verification report saved to: {vr_path}")
    }
  }
  
  return(report)
}

# Enhanced role column consistency check
check_role_column_consistency <- function(verbose = TRUE) {
  if (verbose) {
    cli_h2("Role Column Consistency Check")
    cli_alert_info("Configuration defines {length(APPENDIX_J_CONFIG$role_column_candidates)} candidate column(s):")
    
    for (col in APPENDIX_J_CONFIG$role_column_candidates) {
      cli_alert_info("  • {col}")
    }
  }
  
  files_to_check <- list(
    "Final 1307" = PATHS$final_1307,
    "Preliminary Classified" = PATHS$preliminary_classified,
    "Classification Template" = PATHS$classification_template
  )
  
  results <- list()
  
  for (name in names(files_to_check)) {
    path <- files_to_check[[name]]
    if (file.exists(path)) {
      df <- readr::read_csv(path, show_col_types = FALSE, n_max = 1)
      found_cols <- APPENDIX_J_CONFIG$role_column_candidates[
        APPENDIX_J_CONFIG$role_column_candidates %in% names(df)
      ]
      
      if (length(found_cols) > 0) {
        if (verbose) {
          cli_alert_success("{name} has role column: {paste(found_cols, collapse = ', ')}")
        }
        results[[name]] <- list(
          status = "found",
          columns = found_cols,
          all_columns = length(names(df))
        )
      } else {
        if (verbose) {
          cli_alert_danger("{name} missing role column")
          cli_alert_info("  Available columns (first 5): {paste(names(df)[1:min(5, length(names(df)))], collapse = ', ')}")
        }
        results[[name]] <- list(
          status = "missing",
          columns = character(0),
          available_sample = names(df)[1:min(5, length(names(df)))]
        )
      }
    } else {
      if (verbose) {
        cli_alert_warning("{name} file not found at: {path}")
      }
      results[[name]] <- list(
        status = "file_not_found",
        path = path
      )
    }
  }
  
  return(results)
}

# =============================================================================
# RUN IF CALLED DIRECTLY
# =============================================================================

if (!interactive() && length(commandArgs(trailingOnly = TRUE)) == 0) {
  # Script is being run directly
  cli_h1("Running Pipeline Quality Assurance")
  
  results <- run_quality_checks(verbose = TRUE, save_report = TRUE)
  
  # Generate additional reports
  cli_h2("Generating reproducibility artifacts")
  generate_checksums(save_to_file = TRUE, verbose = TRUE)
  generate_verification_report(save_to_file = TRUE, verbose = TRUE)
  
  # Run role column consistency check
  cli_h2("Checking role column consistency")
  role_consistency <- check_role_column_consistency(verbose = TRUE)
  
  # Final status message
  if (results$overall_status == "PASSED") {
    cli_alert_success("All quality checks passed successfully!")
    quit(save = "no", status = 0)
  } else {
    cli_alert_danger("Quality checks completed with issues. Review report for details.")
    quit(save = "no", status = 1)
  }
}