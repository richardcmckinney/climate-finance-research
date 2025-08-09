#!/usr/bin/env Rscript
# filter_and_adjust_classifications.R
# Purpose: Report deficits vs Appendix J and identify a donor pool (audit utility)
# Version: 3.1 - Made self-contained with explicit dependencies and robust error handling
# Author: Richard McKinney
# Date: 2025-08-09

# ========================= PACKAGE LOADING =========================
# Explicitly load all required packages with proper error handling
suppressPackageStartupMessages({
  required_packages <- c("tidyverse", "rlang", "cli", "scales")
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(sprintf("Required package '%s' is not installed. Install with: install.packages('%s')", pkg, pkg))
    }
  }
  
  library(tidyverse)
  library(rlang)     # CRITICAL: Explicitly load for sym() and other tidy evaluation
  library(cli)       # For improved user feedback and error messages
  library(scales)    # For percentage formatting
})

# Optional package for better CSV reading
has_readr <- requireNamespace("readr", quietly = TRUE)

# ========================= HELPER FUNCTIONS =========================
# Normalize progress values to 0-100 scale
normalize_progress <- function(progress_values) {
  progress_numeric <- suppressWarnings(as.numeric(progress_values))
  
  # Handle NA values
  if (all(is.na(progress_numeric))) {
    cli_warn("All progress values are NA. Defaulting to 100.")
    return(rep(100, length(progress_values)))
  }
  
  # Check if values are in decimal format (0-1)
  non_na_values <- progress_numeric[!is.na(progress_numeric)]
  if (length(non_na_values) > 0 && max(non_na_values) <= 1) {
    cli_inform("Converting progress from decimal to percentage format")
    progress_numeric <- progress_numeric * 100
  }
  
  # Replace NA with 0
  progress_numeric[is.na(progress_numeric)] <- 0
  
  return(progress_numeric)
}

# ========================= LOAD CENTRAL CONFIGURATION =========================
cli_h1("Loading Configuration")

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
    cli_success("Loaded central configuration from: {config_path}")
    break
  }
}

if (!config_loaded) {
  cli_abort(c(
    "Could not find 00_config.R in any of the expected locations:",
    "x" = "Searched paths: {paste(config_paths, collapse = ', ')}",
    "i" = "This file is required for centralized path configuration."
  ))
}

# ========================= LOAD APPENDIX J CONFIGURATION =========================
appendix_j_paths <- c(
  "R/appendix_j_config.R",
  "appendix_j_config.R",
  "../R/appendix_j_config.R"
)

appendix_j_loaded <- FALSE
for (config_path in appendix_j_paths) {
  if (file.exists(config_path)) {
    .appendix_j_config_silent <- TRUE  # Suppress loading message
    source(config_path)
    appendix_j_loaded <- TRUE
    cli_success("Loaded Appendix J configuration from: {config_path}")
    break
  }
}

if (!appendix_j_loaded) {
  cli_abort(c(
    "Could not find appendix_j_config.R in any of the expected locations:",
    "x" = "Searched paths: {paste(appendix_j_paths, collapse = ', ')}",
    "i" = "This file is required for target distribution configuration."
  ))
}

# Get target distribution from centralized source
target <- get_appendix_j_target()
cli_inform("Loaded target distribution with {nrow(target)} categories")

# ========================= SETUP OUTPUT DIRECTORY =========================
dir.create("output", recursive = TRUE, showWarnings = FALSE)
cli_success("Output directory ready: output/")

# ========================= INPUT FILE SELECTION =========================
cli_h1("Input File Selection")

# Use centralized paths instead of hardcoded strings
input_candidates <- list(
  list(path = PATHS$final_1307, 
       desc = "Final quota-matched dataset"),
  list(path = PATHS$preliminary_classified, 
       desc = "Preliminary classified dataset"),
  list(path = PATHS$basic_anon, 
       desc = "Basic anonymized dataset")
)

# Find first existing file
infile <- NULL
infile_desc <- NULL
for (candidate in input_candidates) {
  if (file.exists(candidate$path)) {
    infile <- candidate$path
    infile_desc <- candidate$desc
    break
  }
}

if (is.null(infile)) {
  cli_abort(c(
    "No input file found.",
    "x" = "Looked for files at:",
    paste0("  - ", sapply(input_candidates, function(x) x$path)),
    "i" = "Run earlier pipeline stages first."
  ))
}

cli_success("Using input: {infile_desc}")
cli_inform("File: {basename(infile)}")

# ========================= DATA LOADING =========================
cli_h1("Loading Data")

df <- tryCatch({
  if (has_readr) {
    readr::read_csv(infile, show_col_types = FALSE, progress = FALSE)
  } else {
    read.csv(infile, stringsAsFactors = FALSE, check.names = FALSE)
  }
}, error = function(e) {
  cli_abort(c(
    "Failed to read input file",
    "x" = "Error: {e$message}",
    "i" = "File: {infile}"
  ))
})

cli_success("Loaded {nrow(df)} rows from {ncol(df)} columns")

# Ensure ResponseId exists
if (!"ResponseId" %in% names(df)) {
  if ("respondent_id" %in% names(df)) {
    df$ResponseId <- df$respondent_id
    cli_inform("Using 'respondent_id' as ResponseId")
  } else {
    df$ResponseId <- seq_len(nrow(df))
    cli_warn("No ResponseId found. Generated sequential IDs.")
  }
}

# ========================= PROGRESS COLUMN DETECTION AND HANDLING =========================
cli_h1("Progress Filtering")

# Use helper function from config with proper error handling
progress_col <- find_progress_column(df)

if (is.null(progress_col)) {
  cli_warn(c(
    "No progress column detected. Assuming all responses are complete.",
    "i" = "Expected columns like: Progress, completion, percent_complete",
    "i" = "Available columns: {paste(head(names(df), 10), collapse=', ')}"
  ))
  df$Progress <- 100
  df_complete <- df
} else {
  cli_inform("Found progress column: '{progress_col}'")
  
  # Copy to standard column name if different
  if (progress_col != "Progress") {
    df$Progress <- df[[progress_col]]
  }
  
  # Normalize progress values
  df$Progress <- normalize_progress(df$Progress)
  
  # Apply minimum progress filter from config
  min_progress <- QUALITY_PARAMS$min_progress
  
  df_complete <- df %>% 
    filter(Progress >= min_progress)
  
  n_excluded <- nrow(df) - nrow(df_complete)
  percentage_kept <- percent(nrow(df_complete) / nrow(df), accuracy = 0.1)
  
  cli_inform(c(
    "Applied progress filter (minimum {min_progress}%)",
    "i" = "Kept {nrow(df_complete)} of {nrow(df)} responses ({percentage_kept})",
    "i" = "Excluded {n_excluded} incomplete responses"
  ))
}

# ========================= ROLE COLUMN DETECTION =========================
cli_h1("Role Classification Analysis")

# Use helper function from config with proper error handling
role_col <- find_role_column(df_complete)

if (is.null(role_col)) {
  cli_abort(c(
    "No role column could be detected in the data.",
    "i" = "Expected one of: {paste(STANDARD_COLUMNS$role_alternatives, collapse=', ')}",
    "x" = "Available columns: {paste(head(names(df_complete), 10), collapse=', ')}",
    "i" = "Check that classification has been run on this dataset."
  ))
}

cli_success("Using role column: '{role_col}'")

# Check for non-destructive quota matching
if (all(c("Original_Classification", "Final_Role_Category") %in% names(df_complete))) {
  cli_inform(c(
    "Non-destructive quota matching detected:",
    "i" = "Analyzing quota-matched assignments (Final_Role_Category)",
    "i" = "Original classifications available in Original_Classification"
  ))
  role_col <- "Final_Role_Category"  # Use quota-matched for deficit analysis
}

# Validate categories
validation <- validate_categories(df_complete[[role_col]], target)

if (length(validation$missing) > 0) {
  cli_warn(c(
    "{length(validation$missing)} target categories not found in data:",
    if (length(validation$missing) <= 5) {
      paste0("  - ", validation$missing)
    } else {
      c(paste0("  - ", head(validation$missing, 5)),
        "  ... and {length(validation$missing) - 5} more")
    }
  ))
}

if (length(validation$unexpected) > 0) {
  cli_inform(c(
    "i" = "{length(validation$unexpected)} categories in data not in target:",
    if (length(validation$unexpected) <= 3) {
      paste0("  - ", validation$unexpected)
    } else {
      c(paste0("  - ", head(validation$unexpected, 3)),
        "  ... and {length(validation$unexpected) - 3} more")
    }
  ))
}

# ========================= DISTRIBUTION ANALYSIS =========================
cli_h1("Computing Distribution Analysis")

# Calculate current distribution using explicit rlang::sym()
curr <- df_complete %>% 
  count(!!rlang::sym(role_col), name = "Current") %>% 
  rename(Category = !!rlang::sym(role_col))

# Compare with target
cmp <- full_join(curr, target, by = "Category") %>%
  mutate(
    across(c(Current, Target), ~replace_na(., 0L)),
    Deficit = Target - Current,
    Surplus = pmax(0, Current - Target),
    Status = case_when(
      Current == Target ~ "Matched",
      Current < Target ~ paste0("Deficit: ", Target - Current),
      Current > Target ~ paste0("Surplus: ", Current - Target)
    ),
    Pct_of_Target = ifelse(Target > 0, round(100 * Current / Target, 1), NA_real_),
    Pct_Difference = ifelse(Target > 0, round(100 * (Current - Target) / Target, 1), NA_real_)
  ) %>%
  arrange(desc(Deficit))

# ========================= SUMMARY STATISTICS =========================
total_current <- sum(cmp$Current)
total_target <- sum(cmp$Target)
total_deficit <- sum(pmax(0, cmp$Deficit))
total_surplus <- sum(pmax(0, cmp$Surplus))
categories_matched <- sum(cmp$Current == cmp$Target)
categories_total <- nrow(cmp)
match_rate <- round(100 * categories_matched / categories_total, 1)

# Create enhanced summary
summary_stats <- data.frame(
  Metric = c(
    "Total Current", 
    "Total Target", 
    "Difference", 
    "Total Deficit", 
    "Total Surplus", 
    "Categories Matched",
    "Categories with Deficit", 
    "Categories with Surplus",
    "Match Rate (%)",
    "Mean Progress (%)",
    "Input File",
    "Role Column Used",
    "Analysis Date",
    "Config Version"
  ),
  Value = c(
    total_current, 
    total_target, 
    total_current - total_target,
    total_deficit, 
    total_surplus, 
    categories_matched,
    sum(cmp$Deficit > 0), 
    sum(cmp$Surplus > 0),
    match_rate,
    round(mean(df_complete$Progress, na.rm = TRUE), 1),
    basename(infile),
    role_col,
    as.character(Sys.Date()),
    APPENDIX_J_CONFIG$version
  )
)

# ========================= DONOR POOL ANALYSIS =========================
cli_h1("Donor Pool Analysis")

# Identify potential donor categories (those with surplus)
donor_pool <- cmp %>%
  filter(Surplus > 0) %>%
  arrange(desc(Surplus)) %>%
  mutate(
    Donor_Priority = row_number(),
    Cumulative_Available = cumsum(Surplus),
    Can_Cover_Deficit = Cumulative_Available >= total_deficit
  )

if (nrow(donor_pool) > 0) {
  cli_success("Identified {nrow(donor_pool)} categories with surplus")
  
  # Find when cumulative surplus covers deficit
  covers_at <- which(donor_pool$Can_Cover_Deficit)[1]
  if (!is.na(covers_at)) {
    cli_inform("Top {covers_at} donor categories can cover total deficit of {total_deficit}")
  }
} else {
  cli_warn("No donor categories available (no surplus found)")
}

# ========================= SAVE OUTPUTS =========================
cli_h1("Saving Results")

output_files <- list(
  deficit_report = "output/classification_deficit_report.csv",
  summary_stats = "output/classification_summary_stats.csv",
  donor_pool = "output/donor_pool_analysis.csv"
)

# Save with error handling
for (name in names(output_files)) {
  filepath <- output_files[[name]]
  data_to_save <- switch(name,
    deficit_report = cmp,
    summary_stats = summary_stats,
    donor_pool = donor_pool
  )
  
  tryCatch({
    write.csv(data_to_save, filepath, row.names = FALSE)
    cli_success("Saved {name}: {filepath}")
  }, error = function(e) {
    cli_warn("Failed to save {name}: {e$message}")
  })
}

# ========================= FINAL REPORT =========================
cli_h1("Classification Deficit Report Summary")

cli_inform(c(
  "Configuration: Appendix J v{APPENDIX_J_CONFIG$version}",
  "Input: {infile_desc} ({basename(infile)})",
  "",
  "Total responses: {total_current} (Target: {total_target})",
  "Difference: {ifelse(total_current >= total_target, '+', '')}{total_current - total_target}",
  "Categories matched: {categories_matched}/{categories_total} ({match_rate}%)",
  "",
  "Total deficit to fill: {cli::col_red(total_deficit)}",
  "Total surplus available: {cli::col_green(total_surplus)}"
))

if (nrow(donor_pool) > 0) {
  cli_h2("Top Donor Categories")
  
  top_donors <- head(donor_pool, 5)
  for (i in seq_len(nrow(top_donors))) {
    cli_inform("  {i}. {top_donors$Category[i]}: {cli::col_green(top_donors$Surplus[i])} available")
  }
}

# ========================= VISUALIZATION (Enhanced) =========================
if (interactive()) {
  cli_h2("Category Status (Top 10 by absolute difference)")
  
  top_differences <- cmp %>%
    mutate(AbsDiff = abs(Current - Target)) %>%
    arrange(desc(AbsDiff)) %>%
    head(10)
  
  # Create formatted table
  cat("\n")
  cat(sprintf("%-45s %8s %8s %8s %12s\n", 
              "Category", "Current", "Target", "Diff", "Status"))
  cat(paste(rep("-", 85), collapse = ""), "\n")
  
  for (i in seq_len(nrow(top_differences))) {
    row <- top_differences[i, ]
    
    # Determine status symbol and color
    if (row$Current == row$Target) {
      status_symbol <- "✓"
      diff_text <- sprintf("%+8d", row$Current - row$Target)
    } else if (row$Current < row$Target) {
      status_symbol <- "↓"
      diff_text <- cli::col_red(sprintf("%+8d", row$Current - row$Target))
    } else {
      status_symbol <- "↑"
      diff_text <- cli::col_green(sprintf("%+8d", row$Current - row$Target))
    }
    
    cat(sprintf("%s %-43s %8d %8d %s %12s\n",
                status_symbol,
                substr(row$Category, 1, 43),
                row$Current,
                row$Target,
                diff_text,
                row$Status))
  }
  cat("\n")
}

# ========================= COMPLETION MESSAGE =========================
cli_alert_success("Analysis complete. All reports saved to output/")

# Return results invisibly for potential downstream use
invisible(list(
  comparison = cmp,
  summary = summary_stats,
  donor_pool = donor_pool,
  config = APPENDIX_J_CONFIG,
  validation = validation,
  input_file = infile,
  role_column = role_col,
  total_deficit = total_deficit,
  total_surplus = total_surplus,
  match_rate = match_rate
))