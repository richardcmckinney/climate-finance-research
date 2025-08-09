#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# manual_classification_correction.R
# Purpose: Create a manual reassignment template for hand corrections
# Input : Looks for available classified data files using canonical paths
# Output: output/manual_reassignment_template.csv

# =============================================================================
# LOAD CONFIGURATION
# =============================================================================

# Source the central configuration file
if (file.exists("R/00_config.R")) {
  source("R/00_config.R")
} else if (file.exists("00_config.R")) {
  source("00_config.R")
} else {
  stop("Configuration file not found. Please ensure R/00_config.R or 00_config.R exists.")
}

# =============================================================================
# LOAD LIBRARIES
# =============================================================================

suppressPackageStartupMessages(library(tidyverse))

# =============================================================================
# SETUP OUTPUT DIRECTORY
# =============================================================================

dir.create("output", recursive = TRUE, showWarnings = FALSE)

# =============================================================================
# FIND INPUT FILE USING CANONICAL PATHS
# =============================================================================

# Use canonical paths from configuration
# Priority order: final_1307 > preliminary_classified > basic_anon
input_files <- c(
  PATHS$final_1307,               # "data/climate_finance_survey_final_1307.csv"
  PATHS$preliminary_classified,   # "data/survey_responses_anonymized_preliminary.csv"
  PATHS$basic_anon                # "data/survey_responses_anonymized_basic.csv"
)

# Find first existing file
infile <- NULL
for (f in input_files) {
  if (file.exists(f)) {
    infile <- f
    break
  }
}

if (is.null(infile)) {
  stop("No input file found. Looked for:\n  ", paste(input_files, collapse = "\n  "),
       "\nPlease ensure pipeline stages have been run in order.")
}

cat("Using input file: ", infile, "\n")

# =============================================================================
# LOAD AND PROCESS DATA
# =============================================================================

df <- read.csv(infile, stringsAsFactors = FALSE)

# Check for privacy violations before proceeding
tryCatch({
  check_privacy_violations(df, stop_on_violation = FALSE)
}, error = function(e) {
  warning("Privacy check warning: ", e$message)
})

# Ensure ResponseId exists
if (!"ResponseId" %in% names(df)) {
  if ("respondent_id" %in% names(df)) {
    df$ResponseId <- df$respondent_id
  } else {
    df$ResponseId <- paste0("R_", seq_len(nrow(df)))
  }
}

# Standardize role column using config function
df <- standardize_role_column(df, verbose = TRUE)

# =============================================================================
# IDENTIFY COLUMNS FOR TEMPLATE
# =============================================================================

# Find role columns using standard naming
role_raw_candidates <- c("Q2.1", "role_raw", "QID174", "Role_Raw")
role_text_candidates <- c("Q2.1_12_TEXT", "role_other", "QID174_12_TEXT", "Role_Other_Text")
role_final_col <- "Final_Role_Category"  # Standardized by standardize_role_column()

# Select available columns
role_raw_col <- intersect(role_raw_candidates, names(df))[1]
role_text_col <- intersect(role_text_candidates, names(df))[1]

# Build template with available columns
template_cols <- c("ResponseId")

if (!is.na(role_raw_col)) {
  template_cols <- c(template_cols, role_raw_col)
}

if (!is.na(role_text_col)) {
  template_cols <- c(template_cols, role_text_col)
}

if (role_final_col %in% names(df)) {
  template_cols <- c(template_cols, role_final_col)
}

# =============================================================================
# CREATE TEMPLATE
# =============================================================================

# Create template with selected columns
template <- df %>%
  select(all_of(template_cols)) %>%
  mutate(
    Suggested_New_Category = NA_character_,
    Reassignment_Reason = NA_character_,
    Reviewer_Notes = NA_character_,
    Review_Date = NA_character_,
    Reviewed_By = NA_character_
  )

# Add metadata columns for tracking
template <- template %>%
  mutate(
    Original_Category = if (role_final_col %in% names(df)) df[[role_final_col]] else NA_character_,
    Needs_Review = ifelse(
      !is.na(role_text_col) & !is.na(df[[role_text_col]]) & df[[role_text_col]] != "",
      "Yes", "No"
    )
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

# Sort by review priority (those needing review first)
template <- template %>%
  arrange(desc(Needs_Review), ResponseId)

# =============================================================================
# SAVE OUTPUT
# =============================================================================

# Use safe_path function from config
output_file <- safe_path("output/manual_reassignment_template.csv")
write.csv(template, output_file, row.names = FALSE)

# =============================================================================
# CREATE SUMMARY REPORT
# =============================================================================

# Calculate statistics
n_total <- nrow(template)
n_needs_review <- sum(template$Needs_Review == "Yes", na.rm = TRUE)
n_has_other_text <- sum(!is.na(role_text_col) & !is.na(df[[role_text_col]]) & df[[role_text_col]] != "", na.rm = TRUE)

# Count categories if available
category_counts <- NULL
if (role_final_col %in% names(df)) {
  category_counts <- table(df[[role_final_col]], useNA = "ifany")
}

# =============================================================================
# DISPLAY RESULTS
# =============================================================================

cat("\n=== MANUAL CLASSIFICATION TEMPLATE CREATED ===\n")
cat("Source file: ", basename(infile), "\n", sep = "")
cat("Pipeline stage: ", ifelse(infile == PATHS$final_1307, "Final (1307)",
                               ifelse(infile == PATHS$preliminary_classified, "Preliminary", 
                                     "Basic")), "\n", sep = "")
cat("Total rows: ", n_total, "\n", sep = "")
cat("Rows needing review: ", n_needs_review, "\n", sep = "")
cat("Rows with 'Other' text: ", n_has_other_text, "\n", sep = "")
cat("Output saved → ", output_file, "\n", sep = "")

if (!is.null(category_counts)) {
  cat("\nCurrent category distribution:\n")
  for (i in seq_along(category_counts)) {
    cat(sprintf("  %-40s: %4d\n", names(category_counts)[i], category_counts[i]))
  }
}

cat("\nColumns included in template:\n")
for (col in names(template)) {
  cat("  - ", col, "\n", sep = "")
}

cat("\n=== INSTRUCTIONS FOR MANUAL REVIEW ===\n")
cat("1. Open output/manual_reassignment_template.csv in Excel or similar\n")
cat("2. Review rows where Needs_Review = 'Yes' (sorted to top)\n")
cat("3. For any misclassifications:\n")
cat("   a. Enter correct category in 'Suggested_New_Category'\n")
cat("   b. Provide reason in 'Reassignment_Reason'\n")
cat("   c. Add any additional notes in 'Reviewer_Notes'\n")
cat("4. Update 'Review_Date' (YYYY-MM-DD) and 'Reviewed_By' when complete\n")
cat("5. Save the reviewed file with a new name (e.g., manual_reassignment_complete.csv)\n")
cat("6. Run the reassignment application script to apply changes\n")

# =============================================================================
# QUALITY CHECK
# =============================================================================

# Check for any deprecated paths in use
deprecated_check <- check_deprecated(verbose = FALSE)
if (length(deprecated_check) > 0) {
  warning("\nWARNING: Deprecated files still exist in the project:")
  for (f in deprecated_check) {
    cat("  - ", f, " (should be removed)\n", sep = "")
  }
  cat("These files use old naming conventions and should be deleted.\n")
}

cat("\n=== TEMPLATE GENERATION COMPLETE ===\n")