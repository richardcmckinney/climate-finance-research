#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# manual_classification_correction.R
# Purpose: Create a manual reassignment template for hand corrections
# Input : Looks for available classified data files
# Output: output/manual_reassignment_template.csv

suppressPackageStartupMessages(library(tidyverse))
dir.create("output", recursive = TRUE, showWarnings = FALSE)

# Try multiple possible input files
input_files <- c(
  "data/climate_finance_survey_final_1307.csv",
  "data/survey_responses_anonymized_preliminary.csv",
  "data/climate_finance_survey_classified.csv"  # Legacy path
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
  stop("No input file found. Looked for:\n  ", paste(input_files, collapse = "\n  "))
}

cat("Using input file: ", infile, "\n")

df <- read.csv(infile, stringsAsFactors = FALSE)

# Ensure ResponseId exists
if (!"ResponseId" %in% names(df)) {
  if ("respondent_id" %in% names(df)) {
    df$ResponseId <- df$respondent_id
  } else {
    df$ResponseId <- paste0("R_", seq_len(nrow(df)))
  }
}

# Find role columns
role_raw_candidates <- c("Q2.1", "role_raw", "QID174")
role_text_candidates <- c("Q2.1_12_TEXT", "role_other", "QID174_12_TEXT")
role_final_candidates <- c("Final_Role_Category", "final_category_appendix_j", "stakeholder_category")

# Select available columns
role_raw_col <- intersect(role_raw_candidates, names(df))[1]
role_text_col <- intersect(role_text_candidates, names(df))[1]
role_final_col <- intersect(role_final_candidates, names(df))[1]

# Build template with available columns
template_cols <- c("ResponseId")

if (!is.na(role_raw_col)) {
  template_cols <- c(template_cols, role_raw_col)
}

if (!is.na(role_text_col)) {
  template_cols <- c(template_cols, role_text_col)
}

if (!is.na(role_final_col)) {
  template_cols <- c(template_cols, role_final_col)
}

# Create template
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
    Original_Category = if (!is.na(role_final_col)) df[[role_final_col]] else NA_character_,
    Needs_Review = ifelse(
      !is.na(role_text_col) & !is.na(df[[role_text_col]]) & df[[role_text_col]] != "",
      "Yes", "No"
    )
  )

# Sort by review priority (those needing review first)
template <- template %>%
  arrange(desc(Needs_Review), ResponseId)

# Save template
write.csv(template, "output/manual_reassignment_template.csv", row.names = FALSE)

# Create summary
n_total <- nrow(template)
n_needs_review <- sum(template$Needs_Review == "Yes", na.rm = TRUE)

cat("\n=== MANUAL CLASSIFICATION TEMPLATE CREATED ===\n")
cat("Total rows: ", n_total, "\n", sep = "")
cat("Rows needing review: ", n_needs_review, "\n", sep = "")
cat("Output saved → output/manual_reassignment_template.csv\n")
cat("\nColumns included:\n")
for (col in names(template)) {
  cat("  - ", col, "\n", sep = "")
}
cat("\nInstructions:\n")
cat("1. Review rows where Needs_Review = 'Yes'\n")
cat("2. Fill in Suggested_New_Category if reassignment needed\n")
cat("3. Add Reassignment_Reason and any Reviewer_Notes\n")
cat("4. Update Review_Date and Reviewed_By when complete\n")