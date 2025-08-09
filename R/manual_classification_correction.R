#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# manual_classification_correction.R
# Purpose: Create a manual reassignment template for hand corrections with robust validation
# Version: 3.0 - Enhanced with column validation, change tracking, and comprehensive error handling
# Author: Richard McKinney
# Date: 2025-08-09

suppressPackageStartupMessages({
  library(tidyverse)
  library(cli)
})

# =============================================================================
# LOAD CONFIGURATION
# =============================================================================
# Source both configuration files for paths and helper functions
if (!file.exists("R/00_config.R")) {
  cli_abort("Central configuration not found at R/00_config.R")
}
source("R/00_config.R")

if (!file.exists("R/appendix_j_config.R")) {
  cli_abort("Appendix J configuration not found at R/appendix_j_config.R")
}
source("R/appendix_j_config.R")

set_stage("Manual Template Creation")

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

#' Validate Required Columns Exist
#' @param df Data frame to check
#' @param required_cols Vector of required column names
#' @param context Description of the data being checked
#' @return TRUE if all columns exist, stops with error otherwise
validate_columns <- function(df, required_cols, context = "data") {
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0) {
    cli_abort(c(
      "x" = "Missing required columns in {context}:",
      "!" = paste(missing, collapse = ", "),
      "i" = "Available columns: {paste(names(df), collapse = ', ')}"
    ))
  }
  TRUE
}

#' Ensure Column Exists with Default Value
#' @param df Data frame
#' @param col_name Column name to ensure exists
#' @param default_value Default value if column doesn't exist
#' @param warn Whether to warn when creating column
#' @return Modified data frame
ensure_column <- function(df, col_name, default_value = NA_character_, warn = TRUE) {
  if (!col_name %in% names(df)) {
    if (warn) {
      cli_warn("Column '{col_name}' not found, creating with default value")
    }
    df[[col_name]] <- default_value
  }
  df
}

#' Track Changes Between Original and New Values
#' @param df Data frame with both original and new values
#' @param id_col ID column name
#' @param original_col Original value column
#' @param new_col New value column
#' @return Data frame with change tracking information
track_changes <- function(df, id_col, original_col, new_col) {
  changes <- df %>%
    filter(
      (!is.na(.data[[new_col]]) & .data[[new_col]] != "") &
      (.data[[original_col]] != .data[[new_col]] | is.na(.data[[original_col]]))
    ) %>%
    select(
      !!sym(id_col),
      Original = !!sym(original_col),
      New = !!sym(new_col)
    ) %>%
    mutate(
      Change_Type = case_when(
        is.na(Original) | Original == "" ~ "New Classification",
        TRUE ~ "Reclassification"
      ),
      Timestamp = Sys.time()
    )
  
  return(changes)
}

# =============================================================================
# FIND INPUT FILE USING CANONICAL PATHS
# =============================================================================
cli_h1("Manual Classification Correction Template Generator")

# Determine which stage of the pipeline to use for template creation
# Priority: final_1307 > preliminary_classified > basic_anon
input_path <- NULL
input_stage <- NULL

if (file.exists(PATHS$final_1307)) {
  input_path <- PATHS$final_1307
  input_stage <- "Final (N=1,307)"
} else if (file.exists(PATHS$preliminary_classified)) {
  input_path <- PATHS$preliminary_classified
  input_stage <- "Preliminary Classified"
} else if (file.exists(PATHS$basic_anon)) {
  input_path <- PATHS$basic_anon
  input_stage <- "Basic Anonymized"
} else {
  cli_abort(c(
    "x" = "No input data found in the pipeline.",
    "i" = "Run at least the anonymization stage first: Rscript R/01_anonymize_data.R",
    "i" = "Looked for files in this order:",
    " " = "1. {PATHS$final_1307}",
    " " = "2. {PATHS$preliminary_classified}",
    " " = "3. {PATHS$basic_anon}"
  ))
}

cli_alert_info("Using {input_stage} data from: {.path {basename(input_path)}}")

# =============================================================================
# LOAD AND VALIDATE DATA
# =============================================================================
df <- read_csv(input_path, show_col_types = FALSE, progress = FALSE)
cli_alert_success("Loaded {nrow(df)} rows and {ncol(df)} columns")

# Privacy check (warning only for this utility script)
privacy_ok <- check_privacy_violations(df, stop_on_violation = FALSE)
if (!privacy_ok) {
  cli_alert_warning("Privacy-sensitive columns detected. Template will exclude these.")
}

# =============================================================================
# COMPREHENSIVE COLUMN IDENTIFICATION AND VALIDATION
# =============================================================================
cli_h2("Identifying and Validating Key Columns")

# Find ID column (required)
id_candidates <- c(STANDARD_COLUMNS$respondent_id, "ResponseId", "response_id", "_recordId")
id_col <- find_column(df, id_candidates)
if (is.null(id_col)) {
  cli_abort(c(
    "x" = "No ID column found in the dataset.",
    "i" = "Looked for: {paste(id_candidates, collapse = ', ')}"
  ))
}
cli_alert_success("ID column: {.field {id_col}}")

# Validate ID column has unique values
n_duplicates <- sum(duplicated(df[[id_col]]))
if (n_duplicates > 0) {
  cli_warn(c(
    "!" = "Found {n_duplicates} duplicate IDs in column '{id_col}'",
    "i" = "This may cause issues with manual corrections"
  ))
}

# Find role column (required)
role_col <- tryCatch(
  find_role_column(df),
  error = function(e) {
    cli_abort(c(
      "x" = "No role/category column found in the dataset.",
      "i" = e$message
    ))
  }
)
cli_alert_success("Role column: {.field {role_col}}")

# Check for Final_Role_Category column (may be needed for corrections)
final_role_col <- "Final_Role_Category"
if (!final_role_col %in% names(df)) {
  cli_alert_info("'{final_role_col}' column not found, will use '{role_col}' as source")
  df <- ensure_column(df, final_role_col, df[[role_col]], warn = FALSE)
} else {
  cli_alert_success("Found existing '{final_role_col}' column")
}

# Find progress column (optional)
progress_col <- find_progress_column(df)
if (!is.null(progress_col)) {
  cli_alert_success("Progress column: {.field {progress_col}}")
} else {
  cli_alert_info("No progress column found (optional)")
}

# Find raw role columns for review context (optional)
role_raw_candidates <- c(STANDARD_COLUMNS$role_raw, "Role_Raw", "Q2.1", "role_raw", "QID174")
role_raw_col <- find_column(df, role_raw_candidates)
if (!is.null(role_raw_col)) {
  cli_alert_success("Raw role column: {.field {role_raw_col}}")
}

role_text_candidates <- c(STANDARD_COLUMNS$role_text, "Role_Other_Text", "Q2.1_12_TEXT", 
                          "role_other", "QID174_12_TEXT")
role_text_col <- find_column(df, role_text_candidates)
if (!is.null(role_text_col)) {
  cli_alert_success("Other text column: {.field {role_text_col}}")
}

# =============================================================================
# CREATE MANUAL REVIEW TEMPLATE WITH ENHANCED METADATA
# =============================================================================
cli_h2("Building Enhanced Manual Review Template")

# Start with core columns
template <- tibble(
  !!id_col := df[[id_col]],
  Current_Category = df[[role_col]],
  Final_Role_Category = df[[final_role_col]]
)

# Add progress if available
if (!is.null(progress_col)) {
  template$Progress <- df[[progress_col]]
}

# Add raw role data if available (for context during review)
if (!is.null(role_raw_col)) {
  template$Role_Raw <- df[[role_raw_col]]
}

if (!is.null(role_text_col)) {
  template$Role_Other_Text <- df[[role_text_col]]
}

# Add comprehensive review metadata columns
template <- template %>%
  mutate(
    # Determine which rows need review with more granular logic
    Needs_Review = case_when(
      # Has "Other" text that needs classification
      !is.null(role_text_col) & 
        !is.na(Role_Other_Text) & 
        Role_Other_Text != "" ~ "Yes - Other Text",
      
      # In the catch-all category
      Current_Category == "Miscellaneous and Individual Respondents" ~ "Maybe - Miscellaneous",
      
      # Missing classification
      is.na(Current_Category) | Current_Category == "" ~ "Yes - Missing",
      
      # Everything else probably OK
      TRUE ~ "No"
    ),
    
    # Priority scoring for review order
    Review_Priority = case_when(
      grepl("Yes", Needs_Review) ~ 1,
      grepl("Maybe", Needs_Review) ~ 2,
      TRUE ~ 3
    ),
    
    # Columns for manual corrections with validation helpers
    Suggested_New_Category = NA_character_,
    Reassignment_Reason = NA_character_,
    Confidence_Level = NA_character_,  # High/Medium/Low
    Reviewer_Notes = NA_character_,
    Review_Date = NA_character_,
    Reviewed_By = NA_character_,
    
    # Quality control flags
    QC_Flag = NA_character_,
    Requires_Second_Review = NA_character_
  )

# Add existing classification metadata if available
metadata_cols <- c("Classification_Stage", "Classification_Version", 
                   "Classification_Date", "Classification_Method")
for (col in metadata_cols) {
  if (col %in% names(df)) {
    template[[col]] <- df[[col]]
  }
}

# Sort by review priority and ID
template <- template %>%
  arrange(Review_Priority, desc(Needs_Review), !!sym(id_col))

# =============================================================================
# VALIDATION OF TEMPLATE STRUCTURE
# =============================================================================
cli_h2("Validating Template Structure")

# Ensure critical columns for corrections exist
correction_cols <- c(id_col, "Current_Category", "Final_Role_Category", 
                     "Suggested_New_Category")
validate_columns(template, correction_cols, "template")
cli_alert_success("Template structure validated successfully")

# =============================================================================
# SAVE TEMPLATE WITH PROPER NA HANDLING
# =============================================================================
# Define output path (add to PATHS dynamically if needed)
if (!"manual_template" %in% names(PATHS)) {
  PATHS$manual_template <- "output/manual_reassignment_template.csv"
}

output_path <- safe_path(PATHS$manual_template)

# ISSUE FIX 1: Add na = "" parameter to write_csv
write_csv(template, output_path, na = "")
cli_alert_success("Manual review template saved to: {.path {output_path}}")

# Save a backup with timestamp
backup_path <- sub("\\.csv$", paste0("_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"), output_path)
write_csv(template, backup_path, na = "")
cli_alert_info("Backup saved to: {.path {basename(backup_path)}}")

# =============================================================================
# PREPARE CHANGE TRACKING INFRASTRUCTURE
# =============================================================================
cli_h2("Preparing Change Tracking")

# Create empty change log structure for future use
change_log_template <- tibble(
  !!id_col := character(),
  Original_Category = character(),
  New_Category = character(),
  Change_Type = character(),
  Reason = character(),
  Reviewer = character(),
  Review_Date = character(),
  Timestamp = as.POSIXct(character())
)

change_log_path <- "output/manual_correction_change_log_template.csv"
write_csv(change_log_template, change_log_path, na = "")
cli_alert_info("Change log template created: {.path {change_log_path}}")

# =============================================================================
# COMPREHENSIVE SUMMARY STATISTICS
# =============================================================================
cli_h2("Template Summary")

n_total <- nrow(template)
review_summary <- template %>%
  count(Needs_Review) %>%
  mutate(Percentage = sprintf("%.1f%%", 100 * n / sum(n)))

# Enhanced summary box
cli_div(theme = list(span.field = list(color = "blue", "font-weight" = "bold")))
cli_alert_info("Total rows: {.field {n_total}}")
cli_text("")
cli_text("{.strong Review Priority Breakdown:}")
for (i in seq_len(nrow(review_summary))) {
  status <- review_summary$Needs_Review[i]
  count <- review_summary$n[i]
  pct <- review_summary$Percentage[i]
  
  icon <- case_when(
    grepl("Yes", status) ~ "✓",
    grepl("Maybe", status) ~ "?",
    TRUE ~ "○"
  )
  
  cli_text("  {icon} {sprintf('%-25s', status)}: {.field {count}} ({pct})")
}
cli_text("")
cli_text("Data source: {.field {input_stage}}")
cli_end()

# Show category distribution for reference
cli_h3("Current Category Distribution (Top 10)")
category_dist <- df %>%
  count(!!sym(role_col), sort = TRUE, name = "Count") %>%
  mutate(
    Percentage = sprintf("%.1f%%", 100 * Count / sum(Count)),
    Cumulative = sprintf("%.1f%%", 100 * cumsum(Count) / sum(Count))
  ) %>%
  head(10)

# Format as a nice table with cumulative percentages
cli_text("{.strong Rank  Category                                     Count    Pct   Cumul}")
cli_text("{strrep('-', 75)}")
for (i in seq_len(nrow(category_dist))) {
  cli_text(
    sprintf("%3d.  %-40s  %5d  %6s  %6s",
            i,
            substr(category_dist[[1]][i], 1, 40),
            category_dist$Count[i],
            category_dist$Percentage[i],
            category_dist$Cumulative[i])
  )
}

# =============================================================================
# QUALITY CHECKS WITH ENHANCED VALIDATION
# =============================================================================
cli_h2("Quality Checks")

# Check for potential data quality issues
quality_issues <- list()

# Check 1: Missing categories
n_missing <- sum(is.na(template$Current_Category) | template$Current_Category == "")
if (n_missing > 0) {
  quality_issues <- c(quality_issues, 
                      sprintf("%d rows with missing Current_Category", n_missing))
}

# Check 2: Duplicate IDs
n_dup_ids <- sum(duplicated(template[[id_col]]))
if (n_dup_ids > 0) {
  quality_issues <- c(quality_issues, 
                      sprintf("%d duplicate IDs found", n_dup_ids))
}

# Check 3: Inconsistent Final_Role_Category
n_inconsistent <- sum(template$Current_Category != template$Final_Role_Category, na.rm = TRUE)
if (n_inconsistent > 0) {
  quality_issues <- c(quality_issues,
                      sprintf("%d rows where Current != Final category", n_inconsistent))
}

if (length(quality_issues) > 0) {
  cli_alert_warning("Data quality issues detected:")
  for (issue in quality_issues) {
    cli_text("  • {issue}")
  }
} else {
  cli_alert_success("No data quality issues detected")
}

# Check for deprecated files
deprecated_files <- check_deprecated(verbose = FALSE)
if (length(deprecated_files) > 0) {
  cli_alert_warning("Found {length(deprecated_files)} deprecated file(s) that should be removed:")
  for (f in deprecated_files) {
    cli_text("  • {.file {f}}")
  }
} else {
  cli_alert_success("No deprecated files found")
}

# Check context availability
has_raw_role <- !is.null(role_raw_col)
has_text_role <- !is.null(role_text_col)
context_score <- sum(c(has_raw_role, has_text_role))

cli_text("")
cli_text("{.strong Context Availability:}")
cli_bullets(c(
  if (has_raw_role) "v" else "x" = "Raw role data: {if (has_raw_role) 'Available' else 'Not found'}",
  if (has_text_role) "v" else "x" = "Other text data: {if (has_text_role) 'Available' else 'Not found'}",
  "i" = "Context score: {context_score}/2"
))

if (context_score == 0) {
  cli_alert_warning("Limited context for manual review. Consider using earlier pipeline stage.")
}

# =============================================================================
# ENHANCED INSTRUCTIONS FOR USE
# =============================================================================
cli_h2("Instructions for Manual Review")

cli_text("{.strong Step 1:} Open the template file")
cli_bullets(c(
  " " = "File location: {.path {output_path}}",
  "i" = "Backup location: {.path {basename(backup_path)}}",
  ">" = "Use Excel, LibreOffice Calc, or similar spreadsheet software"
))

cli_text("\n{.strong Step 2:} Review classifications by priority")
cli_bullets(c(
  " " = "Rows are sorted by Review_Priority (1=High, 2=Medium, 3=Low)",
  ">" = "Focus on 'Needs_Review' = 'Yes - Other Text' first",
  ">" = "Check 'Role_Other_Text' column for free-text responses",
  ">" = "Compare 'Current_Category' with 'Final_Role_Category'"
))

cli_text("\n{.strong Step 3:} Make corrections")
cli_bullets(c(
  " " = "Enter the correct category in 'Suggested_New_Category'",
  "!" = "Use EXACT category names from Appendix J (see list below)",
  " " = "Document reasoning in 'Reassignment_Reason'",
  " " = "Set 'Confidence_Level' to High/Medium/Low",
  " " = "Add notes in 'Reviewer_Notes' for complex cases",
  " " = "Flag items needing second review with 'Requires_Second_Review' = 'Yes'"
))

cli_text("\n{.strong Step 4:} Complete metadata")
cli_bullets(c(
  " " = "Update 'Review_Date' with today's date (YYYY-MM-DD format)",
  " " = "Enter your name/initials in 'Reviewed_By'",
  " " = "Use 'QC_Flag' for any quality concerns"
))

cli_text("\n{.strong Step 5:} Save and validate")
cli_bullets(c(
  " " = "Save with descriptive name (e.g., manual_reassignment_complete_YYYYMMDD.csv)",
  "!" = "Do NOT overwrite the original template",
  ">" = "Keep NA cells empty (don't write 'NA' as text)",
  "v" = "Run apply_manual_corrections.R to apply changes"
))

# =============================================================================
# VALID CATEGORIES REFERENCE WITH FORMATTING
# =============================================================================
cli_h2("Valid Appendix J Categories")
cli_text("{.strong Use these EXACT category names when making corrections:}")
cli_text("")

target_categories <- get_appendix_j_target()$Category

# Group categories for better display
govt_categories <- grep("Government|Public|Federal|State|Local", target_categories, value = TRUE)
business_categories <- grep("Business|Corporation|Company|Industry", target_categories, value = TRUE)
nonprofit_categories <- grep("Nonprofit|Association|Organization", target_categories, value = TRUE)
other_categories <- setdiff(target_categories, c(govt_categories, business_categories, nonprofit_categories))

# Display grouped categories
if (length(govt_categories) > 0) {
  cli_text("{.strong Government & Public Sector:}")
  for (cat in govt_categories) {
    cli_text("  • {cat}")
  }
  cli_text("")
}

if (length(business_categories) > 0) {
  cli_text("{.strong Business & Industry:}")
  for (cat in business_categories) {
    cli_text("  • {cat}")
  }
  cli_text("")
}

if (length(nonprofit_categories) > 0) {
  cli_text("{.strong Nonprofit & Associations:}")
  for (cat in nonprofit_categories) {
    cli_text("  • {cat}")
  }
  cli_text("")
}

if (length(other_categories) > 0) {
  cli_text("{.strong Other Categories:}")
  for (cat in other_categories) {
    cli_text("  • {cat}")
  }
}

# =============================================================================
# SAMPLE CHANGE TRACKING CODE (FOR REFERENCE)
# =============================================================================
cli_h2("Change Tracking Preview")
cli_text("When corrections are applied, changes will be tracked as follows:")
cli_code_block(
  language = "r",
  '
# Sample change tracking (will be used in apply_manual_corrections.R)
changes <- track_changes(
  df = corrected_data,
  id_col = "respondent_id",
  original_col = "Current_Category",
  new_col = "Suggested_New_Category"
)

# Generate change summary
change_summary <- changes %>%
  count(Original, New, Change_Type) %>%
  arrange(desc(n))

# Save detailed change log
write_csv(changes, "output/applied_changes_log.csv", na = "")
'
)

# =============================================================================
# FINAL VALIDATION CHECKLIST
# =============================================================================
cli_h2("Pre-Review Checklist")
checklist <- c(
  if (n_total > 0) "v" else "x" = "Template has data ({n_total} rows)",
  if (file.exists(output_path)) "v" else "x" = "Template file saved successfully",
  if (file.exists(backup_path)) "v" else "x" = "Backup created",
  if (context_score > 0) "v" else "x" = "Context columns available ({context_score}/2)",
  if (length(quality_issues) == 0) "v" else "!" = "Data quality: {if (length(quality_issues) == 0) 'Good' else paste(length(quality_issues), 'issues')}",
  "i" = "Ready for manual review: {if (n_total > 0 && file.exists(output_path)) 'Yes' else 'No'}"
)
cli_bullets(checklist)

# =============================================================================
# COMPLETION MESSAGE
# =============================================================================
cli_rule()
cli_alert_success("{.strong Template generation complete!}")
cli_alert_info("Generated template with {n_total} rows")
cli_alert_info("Items flagged for review: {sum(grepl('Yes|Maybe', template$Needs_Review))}")
cli_alert_info("Template location: {.path {output_path}}")
cli_text("")
cli_text("{.strong Next steps:}")
cli_ol(c(
  "Open {.path {basename(output_path)}} in spreadsheet software",
  "Review and correct classifications as needed",
  "Save completed corrections with new filename",
  "Run {.code apply_manual_corrections.R} to apply changes"
))
cli_text("")
cli_alert_info("For questions, refer to the Appendix J documentation or contact the data team.")