#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# manual_classification_correction.R
# Purpose: Create a manual reassignment template for hand corrections
# Version: 2.1 - Fully integrated with central configuration, robust error handling
# Author: Richard McKinney
# Date: 2025-08-08

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
# FIND KEY COLUMNS USING CENTRALIZED HELPERS
# =============================================================================
cli_h2("Identifying Key Columns")

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
# CREATE MANUAL REVIEW TEMPLATE
# =============================================================================
cli_h2("Building Manual Review Template")

# Start with core columns
template <- tibble(
  !!id_col := df[[id_col]],
  Current_Category = df[[role_col]]
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

# Add review metadata columns
template <- template %>%
  mutate(
    # Determine which rows need review
    Needs_Review = case_when(
      # Has "Other" text that needs classification
      !is.null(role_text_col) & 
        !is.na(Role_Other_Text) & 
        Role_Other_Text != "" ~ "Yes",
      
      # In the catch-all category
      Current_Category == "Miscellaneous and Individual Respondents" ~ "Maybe",
      
      # Everything else probably OK
      TRUE ~ "No"
    ),
    
    # Columns for manual corrections
    Suggested_New_Category = NA_character_,
    Reassignment_Reason = NA_character_,
    Reviewer_Notes = NA_character_,
    Review_Date = NA_character_,
    Reviewed_By = NA_character_
  )

# Add classification metadata if available
if ("Classification_Stage" %in% names(df)) {
  template$Classification_Stage <- df$Classification_Stage
}
if ("Classification_Version" %in% names(df)) {
  template$Classification_Version <- df$Classification_Version
}
if ("Classification_Date" %in% names(df)) {
  template$Classification_Date <- df$Classification_Date
}

# Sort by review priority
template <- template %>%
  arrange(desc(Needs_Review), !!sym(id_col))

# =============================================================================
# SAVE TEMPLATE
# =============================================================================
# Define output path (add to PATHS dynamically if needed)
if (!"manual_template" %in% names(PATHS)) {
  PATHS$manual_template <- "output/manual_reassignment_template.csv"
}

output_path <- safe_path(PATHS$manual_template)
write_csv(template, output_path)
cli_alert_success("Manual review template saved to: {.path {output_path}}")

# =============================================================================
# SUMMARY STATISTICS
# =============================================================================
cli_h2("Template Summary")

n_total <- nrow(template)
n_needs_review <- sum(template$Needs_Review == "Yes", na.rm = TRUE)
n_maybe_review <- sum(template$Needs_Review == "Maybe", na.rm = TRUE)
n_no_review <- sum(template$Needs_Review == "No", na.rm = TRUE)

# Summary box
cli_div(theme = list(span.field = list(color = "blue", "font-weight" = "bold")))
cli_ul(c(
  "Total rows: {.field {n_total}}",
  "Definitely needs review: {.field {n_needs_review}} ({round(100*n_needs_review/n_total, 1)}%)",
  "May need review: {.field {n_maybe_review}} ({round(100*n_maybe_review/n_total, 1)}%)",
  "No review needed: {.field {n_no_review}} ({round(100*n_no_review/n_total, 1)}%)",
  "Data source: {.field {input_stage}}"
))
cli_end()

# Show category distribution for reference
cli_h3("Current Category Distribution (Top 10)")
category_dist <- df %>%
  count(!!sym(role_col), sort = TRUE, name = "Count") %>%
  mutate(Percentage = sprintf("%.1f%%", 100 * Count / sum(Count))) %>%
  head(10)

# Format as a nice table
for (i in seq_len(nrow(category_dist))) {
  cli_text("  {sprintf('%2d', i)}. {sprintf('%-40s', category_dist[[1]][i])}: {sprintf('%4d', category_dist$Count[i])} ({category_dist$Percentage[i]})")
}

# =============================================================================
# QUALITY CHECKS
# =============================================================================
cli_h2("Quality Checks")

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

# Check if we have enough context for manual review
has_context <- (!is.null(role_raw_col) || !is.null(role_text_col))
if (!has_context) {
  cli_alert_warning("Limited context available for manual review (no raw role or text columns found)")
  cli_alert_info("Consider using a dataset earlier in the pipeline for more context")
}

# =============================================================================
# INSTRUCTIONS FOR USE
# =============================================================================
cli_h2("Instructions for Manual Review")

cli_text("{.strong Step 1:} Open the template file")
cli_bullets(c(
  " " = "File location: {.path {output_path}}",
  "i" = "Use Excel, LibreOffice Calc, or similar spreadsheet software"
))

cli_text("\n{.strong Step 2:} Review classifications")
cli_bullets(c(
  " " = "Rows are sorted with 'Needs_Review = Yes' at the top",
  ">" = "Check 'Role_Other_Text' column for free-text responses",
  ">" = "Compare with 'Current_Category' to identify misclassifications"
))

cli_text("\n{.strong Step 3:} Make corrections")
cli_bullets(c(
  " " = "Enter the correct category in 'Suggested_New_Category'",
  " " = "Document your reasoning in 'Reassignment_Reason'",
  " " = "Add any notes in 'Reviewer_Notes'",
  "!" = "Use exact category names from Appendix J (see list above)"
))

cli_text("\n{.strong Step 4:} Complete metadata")
cli_bullets(c(
  " " = "Update 'Review_Date' with today's date (YYYY-MM-DD format)",
  " " = "Enter your name/initials in 'Reviewed_By'"
))

cli_text("\n{.strong Step 5:} Save and apply")
cli_bullets(c(
  " " = "Save the file with a new name (e.g., manual_reassignment_complete.csv)",
  "v" = "Run apply_manual_corrections.R to apply the changes to the dataset"
))

# =============================================================================
# VALID CATEGORIES REFERENCE
# =============================================================================
cli_h2("Valid Appendix J Categories")
cli_text("Use these exact category names when making corrections:")

target_categories <- get_appendix_j_target()$Category
for (i in seq_along(target_categories)) {
  if (i %% 2 == 1) {
    # Print two columns for compactness
    cat1 <- sprintf("%-40s", target_categories[i])
    if (i < length(target_categories)) {
      cat2 <- target_categories[i + 1]
      cli_text("  • {cat1}  • {cat2}")
    } else {
      cli_text("  • {cat1}")
    }
  }
}

# =============================================================================
# COMPLETION MESSAGE
# =============================================================================
cli_rule()
cli_alert_success("{.strong Template generation complete!}")
cli_alert_info("Template contains {n_needs_review + n_maybe_review} rows flagged for potential review")
cli_alert_info("Next step: Open {.path {basename(output_path)}} to begin manual review")