#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# filter_and_adjust_classifications.R
# Purpose: Report deficits vs Appendix J and propose a donor pool (audit utility)
# Version: 3.0 - Updated to use centralized configuration throughout
# Author: Richard McKinney
# Date: 2025-08-09

suppressPackageStartupMessages({
  library(tidyverse)
  if (!requireNamespace("rlang", quietly = TRUE)) {
    library(rlang)
  }
})

# ========================= LOAD CENTRAL CONFIGURATION FIRST =========================
# This MUST come before Appendix J config to ensure we have paths
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
    cat("Loaded central configuration from:", config_path, "\n")
    break
  }
}

if (!config_loaded) {
  stop("Could not find 00_config.R in any of the expected locations:\n  ",
       paste(config_paths, collapse = "\n  "),
       "\nThis file is required for centralized path configuration.")
}

# ========================= LOAD APPENDIX J CONFIGURATION =========================
# Source centralized Appendix J configuration
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
    cat("Loaded Appendix J configuration from:", config_path, "\n")
    break
  }
}

if (!appendix_j_loaded) {
  stop("Could not find appendix_j_config.R in any of the expected locations:\n  ",
       paste(appendix_j_paths, collapse = "\n  "))
}

# Get target distribution from centralized source
target <- get_appendix_j_target()

# ========================= SETUP =========================
dir.create("output", recursive = TRUE, showWarnings = FALSE)

# ========================= INPUT FILE SELECTION =========================
# Use centralized paths instead of hardcoded strings
# Priority order: final_1307 > preliminary_classified > basic_anon
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
  stop("No input file found. Looked for:\n  ", 
       paste(sapply(input_candidates, function(x) x$path), collapse = "\n  "),
       "\nRun earlier pipeline stages first.")
}

cat("Using input: ", infile_desc, "\n")
cat("File: ", basename(infile), "\n")

# ========================= DATA LOADING =========================
df <- tryCatch({
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_csv(infile, show_col_types = FALSE, progress = FALSE)
  } else {
    read.csv(infile, stringsAsFactors = FALSE, check.names = FALSE)
  }
}, error = function(e) {
  stop("Failed to read input file: ", e$message)
})

cat("Loaded ", nrow(df), " rows\n")

# Ensure ResponseId exists
if (!"ResponseId" %in% names(df)) {
  if ("respondent_id" %in% names(df)) {
    df$ResponseId <- df$respondent_id
  } else {
    df$ResponseId <- seq_len(nrow(df))
  }
}

# ========================= PROGRESS HANDLING =========================
# Use helper function from config
progress_col <- find_progress_column(df)

if (!is.null(progress_col)) {
  if (progress_col != "Progress") {
    cat("Using progress column:", progress_col, "\n")
    df$Progress <- df[[progress_col]]
  }
} else {
  if (!"Progress" %in% names(df)) {
    cat("Warning: No Progress column found, assuming all complete\n")
    df$Progress <- 100
  }
}

# Convert to numeric and normalize
df$Progress <- suppressWarnings(as.numeric(df$Progress))
if (!all(is.na(df$Progress)) && max(df$Progress, na.rm = TRUE) <= 1) {
  cat("Converting Progress from decimal to percentage format\n")
  df$Progress <- df$Progress * 100
}
df$Progress[is.na(df$Progress)] <- 0

# ========================= FILTERING =========================
# Apply minimum progress filter from config
min_progress <- QUALITY_PARAMS$min_progress

df_complete <- df %>% 
  filter(Progress >= min_progress)

cat("Filtered to", nrow(df_complete), "responses with Progress >=", min_progress, "\n")

# ========================= ROLE COLUMN DETECTION =========================
# Use helper function from config
role_col <- find_role_column(df_complete)
cat("Using role column:", role_col, "\n")

# Check for non-destructive quota matching
if (all(c("Original_Classification", "Final_Role_Category") %in% names(df_complete))) {
  cat("\nNon-destructive quota matching detected:\n")
  cat("  Analyzing quota-matched assignments (Final_Role_Category)\n")
  cat("  Original classifications available in Original_Classification\n")
  role_col <- "Final_Role_Category"  # Use quota-matched for deficit analysis
}

# Validate categories
validation <- validate_categories(df_complete[[role_col]], target)
if (length(validation$missing) > 0) {
  cat("Warning:", length(validation$missing), "target categories not found in data\n")
  if (length(validation$missing) <= 5) {
    for (cat in validation$missing) {
      cat("  -", cat, "\n")
    }
  }
}

# ========================= ANALYSIS =========================
# Calculate current distribution
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
    role_col,
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
# Use centralized paths where defined, create new ones for this script's specific outputs
output_files <- list(
  deficit_report = "output/classification_deficit_report.csv",
  summary_stats = "output/classification_summary_stats.csv",
  donor_pool = "output/donor_pool_analysis.csv"
)

write.csv(cmp, output_files$deficit_report, row.names = FALSE)
write.csv(summary_stats, output_files$summary_stats, row.names = FALSE)
write.csv(donor_pool, output_files$donor_pool, row.names = FALSE)

# ========================= CONSOLE OUTPUT =========================
cat("\n=== CLASSIFICATION DEFICIT REPORT ===\n")
cat("Configuration: Appendix J v", APPENDIX_J_CONFIG$version, "\n")
cat("Input:", infile_desc, "\n")
cat("File:", basename(infile), "\n")
cat("Total responses:", total_current, "(Target:", total_target, ")\n")
cat("Difference:", ifelse(total_current >= total_target, "+", ""), 
    total_current - total_target, "\n", sep = "")
cat("Categories matched:", categories_matched, "/", categories_total, 
    "(", round(100 * categories_matched / categories_total, 1), "%)\n", sep = "")
cat("Total deficit to fill:", total_deficit, "\n")
cat("Total surplus available:", total_surplus, "\n")

if (nrow(donor_pool) > 0) {
  cat("\nTop donor categories (with surplus):\n")
  for (i in seq_len(min(5, nrow(donor_pool)))) {
    cat("  ", i, ". ", donor_pool$Category[i], ": ", 
        donor_pool$Surplus[i], " available\n", sep = "")
  }
}

cat("\nReports saved:\n")
for (name in names(output_files)) {
  cat("  →", output_files[[name]], "\n")
}

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
  validation = validation,
  input_file = infile,
  role_column = role_col
))