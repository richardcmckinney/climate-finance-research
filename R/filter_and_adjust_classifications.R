#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# filter_and_adjust_classifications.R
# Purpose: Report deficits vs Appendix J and propose a donor pool (audit utility)
# Version: 2.0 - Updated to use centralized configuration
# Author: Richard McKinney
# Date: 2025-08-08

suppressPackageStartupMessages(library(tidyverse))

# ========================= CONFIGURATION =========================
# Source centralized Appendix J configuration
config_paths <- c(
  "R/appendix_j_config.R",
  "appendix_j_config.R",
  "../R/appendix_j_config.R"
)

config_loaded <- FALSE
for (config_path in config_paths) {
  if (file.exists(config_path)) {
    .appendix_j_config_silent <- TRUE  # Suppress loading message
    source(config_path)
    config_loaded <- TRUE
    cat("Loaded configuration from: ", config_path, "\n")
    break
  }
}

if (!config_loaded) {
  stop("Could not find appendix_j_config.R in any of the expected locations:\n  ",
       paste(config_paths, collapse = "\n  "),
       "\nPlease ensure the configuration file is in the correct location.")
}

# Get target distribution from centralized source
target <- get_appendix_j_target()

# ========================= SETUP =========================
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

# ========================= DATA LOADING =========================
df <- read.csv(infile, stringsAsFactors = FALSE)

# Ensure ResponseId exists
if (!"ResponseId" %in% names(df)) {
  if ("respondent_id" %in% names(df)) {
    df$ResponseId <- df$respondent_id
  } else {
    df$ResponseId <- seq_len(nrow(df))
  }
}

# ========================= PROGRESS HANDLING =========================
# Use helper function from config if available
progress_col <- find_progress_column(df)

if (!is.null(progress_col) && progress_col != "Progress") {
  cat("Using progress column: ", progress_col, "\n")
  df$Progress <- df[[progress_col]]
} else if (is.null(progress_col)) {
  # Handle missing Progress column
  if ("Progress" %in% names(df)) {
    # Already exists, just ensure it's numeric
  } else {
    cat("Warning: No Progress column found, using all rows\n")
    df$Progress <- 100  # Assume complete if no progress column
  }
}

# Convert to numeric
df$Progress <- suppressWarnings(as.numeric(df$Progress))

# Handle percentage values (if Progress is 0-1, convert to 0-100)
if (!all(is.na(df$Progress)) && max(df$Progress, na.rm = TRUE) <= 1) {
  cat("Converting Progress from decimal to percentage format\n")
  df$Progress <- df$Progress * 100
}

# Replace NA with 0
df$Progress[is.na(df$Progress)] <- 0

# ========================= FILTERING =========================
# Apply minimum progress filter from config
min_progress <- APPENDIX_J_CONFIG$min_progress

if ("Progress" %in% names(df)) {
  df_complete <- dplyr::filter(df, Progress >= min_progress)
  cat("Filtered to ", nrow(df_complete), " responses with Progress >= ", min_progress, "\n")
} else {
  df_complete <- df
}

# ========================= ROLE COLUMN DETECTION =========================
# Use helper function from config
role_col <- find_role_column(df_complete)
cat("Using role column: ", role_col, "\n")

# Validate categories
validation <- validate_categories(df_complete[[role_col]], target)
if (length(validation$missing) > 0) {
  cat("Warning: ", length(validation$missing), " target categories not found in data\n")
  if (length(validation$missing) <= 5) {
    for (cat in validation$missing) {
      cat("  - ", cat, "\n")
    }
  }
}

# ========================= ANALYSIS =========================
# Calculate current distribution
curr <- df_complete %>% 
  count(.data[[role_col]], name = "Current") %>% 
  rename(Category = !!rlang::sym(role_col))  # FIX: Added rlang:: namespace

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
    "Analysis Date"
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
    round(100 * categories_matched / categories_total, 1),
    round(mean(df_complete$Progress, na.rm = TRUE), 1),
    basename(infile),
    as.character(Sys.Date())
  )
)

# ========================= DONOR POOL ANALYSIS =========================
# Identify potential donor categories (those with surplus)
donor_pool <- cmp %>%
  filter(Surplus > 0) %>%
  arrange(desc(Surplus)) %>%
  mutate(
    Donor_Priority = row_number(),
    Cumulative_Available = cumsum(Surplus)
  )

# ========================= SAVE OUTPUTS =========================
write.csv(cmp, "output/classification_deficit_report.csv", row.names = FALSE)
write.csv(summary_stats, "output/classification_summary_stats.csv", row.names = FALSE)
write.csv(donor_pool, "output/donor_pool_analysis.csv", row.names = FALSE)

# ========================= CONSOLE OUTPUT =========================
cat("\n=== CLASSIFICATION DEFICIT REPORT ===\n")
cat("Configuration: Appendix J v", APPENDIX_J_CONFIG$version, "\n")
cat("Input: ", basename(infile), "\n")
cat("Total responses: ", total_current, " (Target: ", total_target, ")\n", sep = "")
cat("Difference: ", ifelse(total_current >= total_target, "+", ""), 
    total_current - total_target, "\n", sep = "")
cat("Categories matched: ", categories_matched, "/", categories_total, 
    " (", round(100 * categories_matched / categories_total, 1), "%)\n", sep = "")
cat("Total deficit to fill: ", total_deficit, "\n", sep = "")
cat("Total surplus available: ", total_surplus, "\n", sep = "")

if (nrow(donor_pool) > 0) {
  cat("\nTop donor categories (with surplus):\n")
  for (i in seq_len(min(5, nrow(donor_pool)))) {
    cat("  ", i, ". ", donor_pool$Category[i], ": ", 
        donor_pool$Surplus[i], " available\n", sep = "")
  }
}

cat("\nReports saved:\n")
cat("  → output/classification_deficit_report.csv\n")
cat("  → output/classification_summary_stats.csv\n")
cat("  → output/donor_pool_analysis.csv\n")

# ========================= VISUALIZATION (Optional) =========================
# Create a simple text-based visualization if requested
if (interactive()) {
  cat("\n=== CATEGORY STATUS (Top 10 by absolute difference) ===\n")
  cat(sprintf("%-45s %8s %8s %8s %12s\n", 
              "Category", "Current", "Target", "Diff", "Status"))
  cat(paste(rep("-", 85), collapse = ""), "\n")
  
  top_differences <- cmp %>%
    mutate(AbsDiff = abs(Current - Target)) %>%
    arrange(desc(AbsDiff)) %>%
    head(10)
  
  for (i in seq_len(nrow(top_differences))) {
    row <- top_differences[i, ]
    status_symbol <- if (row$Current == row$Target) "✓" 
                    else if (row$Current < row$Target) "↓" 
                    else "↑"
    
    cat(sprintf("%s %-43s %8d %8d %+8d %12s\n",
                status_symbol,
                substr(row$Category, 1, 43),
                row$Current,
                row$Target,
                row$Current - row$Target,
                row$Status))
  }
}

# Return results invisibly for potential downstream use
invisible(list(
  comparison = cmp,
  summary = summary_stats,
  donor_pool = donor_pool,
  config = APPENDIX_J_CONFIG,
  validation = validation
))