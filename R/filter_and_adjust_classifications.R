#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# filter_and_adjust_classifications.R
# Purpose: Report deficits vs Appendix J and propose a donor pool (audit utility)

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
    df$ResponseId <- seq_len(nrow(df))
  }
}

# Handle Progress column
if ("Progress" %in% names(df)) {
  df$Progress <- suppressWarnings(as.numeric(df$Progress))
  df_complete <- dplyr::filter(df, Progress >= 10)
} else {
  cat("Warning: No Progress column found, using all rows\n")
  df_complete <- df
}

# Define target distribution (Appendix J)
target <- tibble::tribble(
  ~Category, ~Target,
  "Entrepreneur in Climate Technology", 159, 
  "Venture Capital Firm", 117,
  "Investment and Financial Services", 109, 
  "Private Equity Firm", 88,
  "Business Consulting and Advisory", 79, 
  "Nonprofit Organization", 73,
  "High Net-Worth Individual", 66, 
  "Government Funding Agency", 53,
  "Academic or Research Institution", 52, 
  "Limited Partner", 49,
  "Family Office", 48, 
  "Corporate Venture Arm", 47, 
  "Angel Investor", 43,
  "ESG Investor", 38, 
  "Legal Services", 38, 
  "Corporate Entities", 35,
  "Manufacturing and Industrial", 25, 
  "Energy and Infrastructure", 24,
  "Real Estate and Property", 20, 
  "Philanthropic Organization", 19,
  "Technology and Software", 19, 
  "Media and Communication", 7,
  "Miscellaneous and Individual Respondents", 151
)

# Find role column
role_candidates <- c("Final_Role_Category", "final_category_appendix_j", "stakeholder_category")
role_col <- intersect(role_candidates, names(df_complete))

if (length(role_col) == 0) {
  stop("No role category column found. Expected one of: ", 
       paste(role_candidates, collapse = ", "))
}

role_col <- role_col[1]
cat("Using role column: ", role_col, "\n")

# Calculate current distribution
curr <- df_complete %>% 
  count(.data[[role_col]], name = "Current") %>% 
  rename(Category = !!role_col)

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
    )
  ) %>%
  arrange(desc(Deficit))

# Add summary statistics
total_current <- sum(cmp$Current)
total_target <- sum(cmp$Target)
total_deficit <- sum(pmax(0, cmp$Deficit))
total_surplus <- sum(pmax(0, cmp$Surplus))
categories_matched <- sum(cmp$Current == cmp$Target)

# Create summary
summary_stats <- data.frame(
  Metric = c("Total Current", "Total Target", "Difference", 
             "Total Deficit", "Total Surplus", "Categories Matched",
             "Categories with Deficit", "Categories with Surplus"),
  Value = c(total_current, total_target, total_current - total_target,
            total_deficit, total_surplus, categories_matched,
            sum(cmp$Deficit > 0), sum(cmp$Surplus > 0))
)

# Save outputs
write.csv(cmp, "output/classification_deficit_report.csv", row.names = FALSE)
write.csv(summary_stats, "output/classification_summary_stats.csv", row.names = FALSE)

cat("\n=== CLASSIFICATION DEFICIT REPORT ===\n")
cat("Total responses: ", total_current, " (Target: ", total_target, ")\n", sep = "")
cat("Difference: ", total_current - total_target, "\n", sep = "")
cat("Categories matched: ", categories_matched, "/", nrow(target), "\n", sep = "")
cat("Total deficit to fill: ", total_deficit, "\n", sep = "")
cat("Total surplus available: ", total_surplus, "\n", sep = "")
cat("\nReports saved:\n")
cat("  → output/classification_deficit_report.csv\n")
cat("  → output/classification_summary_stats.csv\n")