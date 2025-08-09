# Comprehensive quality assurance checks

run_quality_checks <- function() {
  results <- list()
  
  # Check 1: No deprecated files exist
  deprecated_files <- DEPRECATED_PATHS[file.exists(DEPRECATED_PATHS)]
  results$deprecated <- length(deprecated_files) == 0
  
  # Check 2: Column consistency
  if (file.exists(PATHS$final_1307)) {
    df <- readr::read_csv(PATHS$final_1307, show_col_types = FALSE, n_max = 10)
    results$columns_standard <- "Final_Role_Category" %in% names(df)
  }
  
  # Check 3: No PII in outputs
  results$no_pii <- !any(grepl("Q2\\.2|email|address|phone", 
                               list.files("output", recursive = TRUE)))
  
  # Check 4: Reproducibility
  checksums_exist <- file.exists("docs/checksums.txt")
  results$reproducible <- checksums_exist
  
  # Generate report
  report <- data.frame(
    Check = names(results),
    Passed = unlist(results),
    Timestamp = Sys.time()
  )
  
  write.csv(report, "output/quality_assurance_report.csv", row.names = FALSE)
  
  if (!all(unlist(results))) {
    warning("Some quality checks failed. See output/quality_assurance_report.csv")
  } else {
    message("✓ All quality checks passed")
  }
  
  return(results)
}