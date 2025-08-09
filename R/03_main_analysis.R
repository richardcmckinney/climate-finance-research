#!/usr/bin/env Rscript
# 03_main_analysis.R
# Purpose: Main statistical analysis for climate finance manuscript (FINAL N=1,307)
# Version: 3.0 - CRITICAL FIX: Use quota_target_category for valid N=1,307 analysis
# Author: Richard McKinney
# Date: 2025-08-09
#
# CRITICAL CHANGES IN v3.0:
# - Uses ANALYSIS_ROLE_COLUMN from 02_main_analysis.R (quota_target_category for N=1,307)
# - Removed redundant pick_role_col() - uses centralized detect_role_column()
# - Fixed all rlang namespace issues
# - Ensures all analyses use the correct quota-matched categories
# - Added validation that we're analyzing the right column throughout
#
# Notes:
#   - Deterministic: sets seed + UTC
#   - Uses anonymized geographic data (hq_country/region) instead of raw Q2.2
#   - Outputs comprehensive figures and diagnostic CSVs for manuscript

# ---- Safety for Rscript/S4 (.local) ------------------------------------------
suppressPackageStartupMessages(library(methods))
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# ---- Determinism & options ---------------------------------------------------
set.seed(1307)
Sys.setenv(TZ = "UTC")
options(stringsAsFactors = FALSE)

# ---- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)  # includes readr, dplyr, tidyr, ggplot2, etc.
  library(scales)
  library(rlang)      # Explicitly load for tidy evaluation
})

# ---- Configuration ------------------------------------------------------------
# Source configuration if available
if (file.exists("R/00_config.R")) {
  source("R/00_config.R")
  INPUT_FINAL <- PATHS$final_1307
} else if (file.exists("00_config.R")) {
  source("00_config.R")
  INPUT_FINAL <- PATHS$final_1307
} else {
  # Fallback for standalone execution
  INPUT_FINAL <- "data/climate_finance_survey_final_1307.csv"
  warning("Configuration file not found, using default path: ", INPUT_FINAL)
}

# CRITICAL: Get the analysis role column from 02_main_analysis.R
if (!exists("ANALYSIS_ROLE_COLUMN")) {
  # If run directly without wrapper, try to detect
  warning("ANALYSIS_ROLE_COLUMN not set by wrapper. Attempting detection...")
  ANALYSIS_ROLE_COLUMN <- NULL
}

VERBOSE <- TRUE            # rich console logging for reviewers
SAVE_DIAGNOSTICS <- TRUE   # write extra CSVs used in the paper's methods/appendix

# ---- Helpers -----------------------------------------------------------------
inf <- function(...) if (VERBOSE) cat(paste0(...), "\n")
ensure_dir <- function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
save_plot <- function(path, plot, width, height, dpi = 300) {
  ggsave(path, plot, width = width, height = height, dpi = dpi)
  inf("✓ Saved plot: ", path)
}
save_csv <- function(df, path) {
  readr::write_csv(df, path)
  inf("✓ Saved CSV: ", path)
}

# ---- Folders -----------------------------------------------------------------
ensure_dir("figures")
ensure_dir("output")

# ---- Theme & palette ----------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
climate_colors <- c("#2E7D32", "#1976D2", "#F57C00", "#7B1FA2", "#C62828",
                    "#00796B", "#5D4037", "#455A64")

# ---- Load FINAL dataset (post-quota match) -----------------------------------
if (!file.exists(INPUT_FINAL)) {
  stop("Final analysis dataset not found at: ", INPUT_FINAL,
       "\nRun `R/get_exact_1307.R` (or `run_all.R --with-analysis`) first.")
}

# Use readr for consistency
df <- readr::read_csv(INPUT_FINAL, show_col_types = FALSE, progress = FALSE)

inf("Loaded FINAL dataset:\n  File: ", INPUT_FINAL,
    "\n  Rows: ", nrow(df), "\n  Cols: ", ncol(df), "\n")

# ---- CRITICAL: Determine correct role column for analysis -------------------
if (is.null(ANALYSIS_ROLE_COLUMN)) {
  # Detect the role column if not passed from wrapper
  if ("quota_target_category" %in% names(df)) {
    ANALYSIS_ROLE_COLUMN <- "quota_target_category"
    inf("✓ CRITICAL: Detected quota-matched dataset, using 'quota_target_category' for analysis")
  } else if ("Final_Role_Category" %in% names(df)) {
    ANALYSIS_ROLE_COLUMN <- "Final_Role_Category"
    inf("⚠ Using 'Final_Role_Category' (dataset may not be quota-matched)")
  } else if (exists("detect_role_column") && is.function(detect_role_column)) {
    ANALYSIS_ROLE_COLUMN <- detect_role_column(df)
    inf("⚠ Using detected role column: ", ANALYSIS_ROLE_COLUMN)
  } else {
    # Final fallback
    cand <- c("quota_target_category", "Final_Role_Category", 
              "final_category_appendix_j", "stakeholder_category")
    hit <- cand[cand %in% names(df)][1]
    if (is.na(hit)) {
      stop("No role category column found. Expected one of: ", paste(cand, collapse = ", "))
    }
    ANALYSIS_ROLE_COLUMN <- hit
    inf("⚠ Using fallback role column: ", ANALYSIS_ROLE_COLUMN)
  }
}

inf("\n=== ANALYSIS CONFIGURATION ===")
inf("Role column for analysis: ", ANALYSIS_ROLE_COLUMN)
inf("This column will be used for ALL stakeholder-based analyses\n")

# Validate the column exists and has data
if (!ANALYSIS_ROLE_COLUMN %in% names(df)) {
  stop("Critical error: Analysis role column '", ANALYSIS_ROLE_COLUMN, "' not found in dataset")
}

n_valid <- sum(!is.na(df[[ANALYSIS_ROLE_COLUMN]]))
if (n_valid < 1000) {
  warning("Only ", n_valid, " valid role categories found. Expected ~1,307")
}

# ---- Data preparation with correct role column -------------------------------
# Rename the analysis column to a standard name for consistency
df <- df %>%
  rename(RoleCategory = !!sym(ANALYSIS_ROLE_COLUMN))

# Make it an ordered factor by size for better visualization
df <- df %>%
  mutate(RoleCategory = if (!is.factor(RoleCategory)) factor(RoleCategory) else RoleCategory) %>%
  group_by(RoleCategory) %>%
  mutate(.role_n = n()) %>%
  ungroup() %>%
  mutate(RoleCategory = fct_reorder(RoleCategory, .role_n, .desc = TRUE)) %>%
  select(-.role_n)

# Save dataset schema for documentation
if (SAVE_DIAGNOSTICS) {
  schema <- tibble::tibble(
    variable   = names(df),
    class      = vapply(df, function(x) class(x)[1], character(1)),
    n_missing  = vapply(df, function(x) sum(is.na(x)), integer(1))
  )
  save_csv(schema, "output/dataset_schema_snapshot.csv")
  
  # Also save which column was used for analysis
  analysis_config <- tibble::tibble(
    parameter = c("input_file", "analysis_role_column", "n_rows", "n_valid_roles"),
    value = c(INPUT_FINAL, ANALYSIS_ROLE_COLUMN, nrow(df), n_valid)
  )
  save_csv(analysis_config, "output/analysis_configuration.csv")
}

# ============================================================================
# STAKEHOLDER DISTRIBUTION (using correct quota column)
# ============================================================================
stakeholder_summary <- df %>%
  filter(!is.na(RoleCategory)) %>%
  count(RoleCategory, name = "n", sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

inf("=== STAKEHOLDER DISTRIBUTION (", ANALYSIS_ROLE_COLUMN, ") ===")
inf(capture.output(stakeholder_summary) %>% paste(collapse = "\n"))

# Critical validation for N=1,307
if (sum(stakeholder_summary$n) != 1307) {
  warning("Total N = ", sum(stakeholder_summary$n), " (expected 1,307)")
}

if (SAVE_DIAGNOSTICS) {
  # Add metadata about which column was analyzed
  stakeholder_summary_export <- stakeholder_summary %>%
    mutate(
      source_column = ANALYSIS_ROLE_COLUMN,
      dataset = basename(INPUT_FINAL)
    )
  save_csv(stakeholder_summary_export, "output/stakeholder_distribution_final.csv")
}

# ============================================================================
# FIGURE 4: Geographic Distribution of Survey Respondents
# ============================================================================
# CRITICAL: Privacy-preserving geographic data handling

# Check what geographic columns are available
all_geo_cols <- grep("country|region|location|geo|Q2\\.2|hq_", names(df), 
                     value = TRUE, ignore.case = TRUE)
inf("\nGeographic columns present: ",
    if(length(all_geo_cols) > 0) paste(all_geo_cols, collapse = ", ") else "NONE")

# Define acceptable anonymized columns (priority order)
anonymized_candidates <- c("hq_country", "region", "geography", "country", "location")

# Check if Q2.2 (raw data) exists
has_raw_q22 <- "Q2.2" %in% names(df)

# Find the best available geographic column (EXCLUDE Q2.2)
safe_geo_cols <- setdiff(all_geo_cols, "Q2.2")
geo_col <- intersect(anonymized_candidates, safe_geo_cols)[1]

# Handle different scenarios
if (is.na(geo_col)) {
  if (has_raw_q22) {
    stop(paste(
      "\n=== CRITICAL ERROR: Geographic Data Privacy Protection ===",
      "\nRaw geographic data (Q2.2) detected but anonymized columns missing.",
      "\nThis violates privacy requirements stated in README.md.",
      "\n",
      "\nRequired Action:",
      "\n1. Run anonymization script first: Rscript R/01_anonymize_data.R",
      "\n2. Ensure geographic data is properly anonymized to regions",
      "\n3. Verify anonymized columns exist: hq_country, region, or geography",
      "\n",
      "\nDO NOT proceed with analysis using raw Q2.2 data.",
      "\n=========================================================",
      sep = "\n"
    ))
  } else {
    warning("No geographic data available for Figure 4")
    geo_col <- NA
  }
} else {
  inf("✓ Using anonymized geographic column: ", geo_col)
  inf("✓ Privacy requirements maintained per README.md")
}

# Proceed with geographic analysis only if safe column exists
if (!is.na(geo_col)) {
  geo_data <- df %>%
    filter(!is.na(.data[[geo_col]]) & .data[[geo_col]] != "") %>%
    count(.data[[geo_col]], name = "n", sort = TRUE) %>%
    rename(Region = 1) %>%
    mutate(percentage = n / sum(n) * 100)

  inf("\nGeographic distribution (anonymized regions):")
  inf(capture.output(utils::head(geo_data, 10)) %>% paste(collapse = "\n"))

  fig4 <- ggplot(geo_data, aes(x = "", y = percentage, fill = Region)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "right") +
    scale_fill_manual(values = rep(climate_colors, length.out = nrow(geo_data)), name = "Region") +
    labs(title = "Figure 4: Geographic Distribution of Survey Respondents",
         subtitle = paste0("N = ", sum(geo_data$n), " (Anonymized Regions)"))

  save_plot("figures/figure4_geographic_distribution.png", fig4, width = 10, height = 6)
  if (SAVE_DIAGNOSTICS) save_csv(geo_data, "output/figure4_geographic_distribution.csv")
}

# ============================================================================
# FIGURE 6: Financial vs. Impact Focus by Stakeholder Group
# ============================================================================
# Q3.3: 1 = pure impact ... 7 = pure financial
if ("Q3.3" %in% names(df)) {
  df <- df %>% mutate(Q3_3_num = suppressWarnings(as.numeric(.data[["Q3.3"]])))

  impact_data <- df %>%
    filter(!is.na(RoleCategory), !is.na(Q3_3_num)) %>%
    group_by(RoleCategory) %>%
    summarise(
      n = n(),
      mean_focus = mean(Q3_3_num, na.rm = TRUE),
      sd_focus   = sd(Q3_3_num,   na.rm = TRUE),
      se_focus   = sd_focus / sqrt(n),
      ci_lower   = mean_focus - 1.96 * se_focus,
      ci_upper   = mean_focus + 1.96 * se_focus,
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%   # minimum sample size
    arrange(desc(mean_focus))

  inf("\n=== FINANCIAL vs IMPACT FOCUS BY STAKEHOLDER (Q3.3) ===")
  inf("Analysis based on: ", ANALYSIS_ROLE_COLUMN)
  inf(capture.output(impact_data) %>% paste(collapse = "\n"))

  fig6 <- ggplot(impact_data, aes(x = reorder(RoleCategory, mean_focus), y = mean_focus)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    coord_flip() +
    scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
    labs(title = "Figure 6: Financial vs. Impact Focus by Stakeholder Group",
         subtitle = paste0("1 = Pure Impact Focus, 7 = Pure Financial Focus (95% CI) | N=", 
                          sum(impact_data$n)),
         x = "Stakeholder Group",
         y = "Mean Investment Focus")

  save_plot("figures/figure6_financial_impact_focus.png", fig6, width = 10, height = 8)
  
  if (SAVE_DIAGNOSTICS) {
    impact_data_export <- impact_data %>%
      mutate(source_column = ANALYSIS_ROLE_COLUMN)
    save_csv(impact_data_export, "output/figure6_financial_impact_focus.csv")
  }
} else {
  inf("! Q3.3 missing — skipping Figure 6")
}

# ============================================================================
# BARRIER ANALYSIS
# ============================================================================
barrier_cols <- grep("^(Q3\\.11|barrier|challenge)", names(df), value = TRUE, ignore.case = TRUE)

if (length(barrier_cols) > 0) {
  inf("\n=== BARRIER ANALYSIS ===")
  
  # Pivot to long format
  barrier_long <- df %>%
    select(RoleCategory, all_of(barrier_cols)) %>%
    pivot_longer(cols = -RoleCategory, names_to = "barrier_var", values_to = "val_raw",
                 values_transform = list(val_raw = as.character)) %>%
    mutate(val_present = !is.na(val_raw) & val_raw != "")

  # By stakeholder
  barrier_summary <- barrier_long %>%
    group_by(RoleCategory, barrier_var) %>%
    summarise(
      n = n(),
      pct_present = sum(val_present, na.rm = TRUE) / n * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(RoleCategory, desc(pct_present))

  inf("Barrier presence by stakeholder (first 12 rows):")
  inf(capture.output(utils::head(barrier_summary, 12)) %>% paste(collapse = "\n"))

  if (SAVE_DIAGNOSTICS) {
    barrier_summary_export <- barrier_summary %>%
      mutate(analysis_column = ANALYSIS_ROLE_COLUMN)
    save_csv(barrier_summary_export, "output/barrier_presence_by_stakeholder.csv")
  }
  
  # Overall barrier ranking
  overall_barriers <- barrier_long %>%
    group_by(barrier_var) %>%
    summarise(
      n_responses = n(),
      n_present = sum(val_present, na.rm = TRUE),
      pct_overall = n_present / n_responses * 100,
      .groups = "drop"
    ) %>%
    arrange(desc(pct_overall))
  
  if (SAVE_DIAGNOSTICS) save_csv(overall_barriers, "output/overall_barrier_ranking.csv")
  
  inf("\nTOP BARRIERS OVERALL:")
  inf(capture.output(utils::head(overall_barriers, 10)) %>% paste(collapse = "\n"))
}

# ============================================================================
# RISK PERCEPTION ANALYSIS
# ============================================================================
risk_cols <- grep("^(Q3\\.6|risk)", names(df), value = TRUE, ignore.case = TRUE)

if (length(risk_cols) > 0) {
  inf("\n=== RISK PERCEPTION ANALYSIS ===")
  
  # Convert risk columns to numeric
  risk_data <- df %>%
    select(RoleCategory, all_of(risk_cols)) %>%
    mutate(across(all_of(risk_cols), ~suppressWarnings(as.numeric(.x))))
  
  # Calculate mean risk perception by stakeholder
  risk_summary <- risk_data %>%
    group_by(RoleCategory) %>%
    summarise(
      n = n(),
      across(all_of(risk_cols), 
             list(mean = ~mean(.x, na.rm = TRUE),
                  sd = ~sd(.x, na.rm = TRUE)),
             .names = "{.col}_{.fn}"),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  if (SAVE_DIAGNOSTICS) {
    risk_summary_export <- risk_summary %>%
      mutate(analysis_column = ANALYSIS_ROLE_COLUMN)
    save_csv(risk_summary_export, "output/risk_perception_by_stakeholder.csv")
  }
  
  # Create risk perception heatmap
  risk_means <- risk_data %>%
    group_by(RoleCategory) %>%
    summarise(across(all_of(risk_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    filter(RoleCategory %in% stakeholder_summary$RoleCategory[1:min(10, nrow(stakeholder_summary))])
  
  if (nrow(risk_means) > 0 && length(risk_cols) > 1) {
    risk_heatmap_data <- risk_means %>%
      pivot_longer(cols = -RoleCategory, names_to = "Risk_Type", values_to = "Mean_Score")
    
    # Clean risk type names
    risk_heatmap_data <- risk_heatmap_data %>%
      mutate(Risk_Type = gsub("Q3\\.6_?|_risk", "", Risk_Type))
    
    fig_risk <- ggplot(risk_heatmap_data, aes(x = Risk_Type, y = RoleCategory, fill = Mean_Score)) +
      geom_tile() +
      scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 4,
                           limits = c(1, 7), name = "Risk Level") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Risk Perception Heatmap by Stakeholder Group",
           subtitle = paste0("Based on ", ANALYSIS_ROLE_COLUMN, " | N=", sum(stakeholder_summary$n)),
           x = "Risk Type", y = "Stakeholder Group")
    
    save_plot("figures/risk_perception_heatmap.png", fig_risk, width = 12, height = 8)
  }
}

# ============================================================================
# TECHNOLOGY PREFERENCE ANALYSIS
# ============================================================================
tech_cols <- grep("^(Q12\\.|technology|tech_|solution)", names(df), value = TRUE, ignore.case = TRUE)

if (length(tech_cols) > 0) {
  inf("\n=== TECHNOLOGY PREFERENCE ANALYSIS ===")
  
  tech_data <- df %>%
    select(RoleCategory, all_of(tech_cols)) %>%
    mutate(across(all_of(tech_cols), ~suppressWarnings(as.numeric(.x))))
  
  tech_summary <- tech_data %>%
    group_by(RoleCategory) %>%
    summarise(
      n = n(),
      across(all_of(tech_cols),
             ~mean(.x, na.rm = TRUE),
             .names = "{.col}_mean"),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  if (SAVE_DIAGNOSTICS) {
    tech_summary_export <- tech_summary %>%
      mutate(analysis_column = ANALYSIS_ROLE_COLUMN)
    save_csv(tech_summary_export, "output/technology_preferences_by_stakeholder.csv")
  }
}

# ============================================================================
# INVESTMENT CRITERIA ANALYSIS
# ============================================================================
investment_cols <- grep("^(Q3\\.[4-5]|investment|criteria)", names(df), value = TRUE, ignore.case = TRUE)

if (length(investment_cols) > 0) {
  inf("\n=== INVESTMENT CRITERIA ANALYSIS ===")
  
  investment_data <- df %>%
    select(RoleCategory, all_of(investment_cols)) %>%
    mutate(across(all_of(investment_cols), ~suppressWarnings(as.numeric(.x))))
  
  investment_summary <- investment_data %>%
    group_by(RoleCategory) %>%
    summarise(
      n = n(),
      across(all_of(investment_cols),
             ~mean(.x, na.rm = TRUE),
             .names = "{.col}_mean"),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  if (SAVE_DIAGNOSTICS) {
    investment_summary_export <- investment_summary %>%
      mutate(analysis_column = ANALYSIS_ROLE_COLUMN)
    save_csv(investment_summary_export, "output/investment_criteria_by_stakeholder.csv")
  }
}

# ============================================================================
# CORRELATION ANALYSIS BETWEEN KEY VARIABLES
# ============================================================================
if ("Q3_3_num" %in% names(df) && length(risk_cols) > 0) {
  inf("\n=== CORRELATION ANALYSIS ===")
  
  numeric_vars <- df %>%
    select(Q3_3_num, any_of(c("Progress", risk_cols[1:min(5, length(risk_cols))]))) %>%
    select_if(is.numeric)
  
  if (ncol(numeric_vars) > 1) {
    cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
    
    if (SAVE_DIAGNOSTICS) {
      cor_df <- as.data.frame(cor_matrix)
      cor_df$Variable <- rownames(cor_df)
      cor_df <- cor_df %>% select(Variable, everything())
      save_csv(cor_df, "output/correlation_matrix_key_variables.csv")
    }
    
    # Create correlation plot if corrplot available
    if (requireNamespace("corrplot", quietly = TRUE)) {
      png("figures/correlation_matrix.png", width = 10, height = 8, units = "in", res = 150)
      corrplot::corrplot(cor_matrix, method = "color", type = "upper",
                        order = "hclust", addCoef.col = "black",
                        tl.col = "black", tl.srt = 45,
                        title = "Correlation Matrix of Key Variables",
                        mar = c(0, 0, 2, 0))
      dev.off()
      inf("✓ Saved correlation matrix plot")
    }
  }
}

# ============================================================================
# SAMPLE SIZE ADEQUACY CHECK
# ============================================================================
inf("\n=== SAMPLE SIZE ADEQUACY ===")

sample_check <- stakeholder_summary %>%
  mutate(
    adequate_for_means = n >= 30,
    adequate_for_proportions = n >= 10,
    power_80_detect_d05 = n >= 64  # For detecting Cohen's d = 0.5 with 80% power
  )

inf("Groups with n >= 30 (adequate for means): ", sum(sample_check$adequate_for_means))
inf("Groups with n >= 10 (minimum for analysis): ", sum(sample_check$adequate_for_proportions))
inf("Groups with n >= 64 (80% power, d=0.5): ", sum(sample_check$power_80_detect_d05))

if (SAVE_DIAGNOSTICS) {
  sample_check_export <- sample_check %>%
    mutate(analysis_column = ANALYSIS_ROLE_COLUMN)
  save_csv(sample_check_export, "output/sample_size_adequacy.csv")
}

# ============================================================================
# DATA QUALITY METRICS
# ============================================================================
inf("\n=== DATA QUALITY METRICS ===")

# Check if Progress column exists
if ("Progress" %in% names(df)) {
  quality_metrics <- data.frame(
    Metric = c("Total Responses", "Complete Cases", "Progress >= 90%", 
               "Progress >= 75%", "Progress >= 50%"),
    Count = c(
      nrow(df),
      sum(complete.cases(df)),
      sum(df$Progress >= 90, na.rm = TRUE),
      sum(df$Progress >= 75, na.rm = TRUE),
      sum(df$Progress >= 50, na.rm = TRUE)
    ),
    Percentage = NA
  )
} else {
  quality_metrics <- data.frame(
    Metric = c("Total Responses", "Complete Cases"),
    Count = c(nrow(df), sum(complete.cases(df))),
    Percentage = NA
  )
}

quality_metrics$Percentage <- round(quality_metrics$Count / nrow(df) * 100, 1)

inf(capture.output(quality_metrics) %>% paste(collapse = "\n"))

if (SAVE_DIAGNOSTICS) {
  quality_metrics$analysis_column <- ANALYSIS_ROLE_COLUMN
  save_csv(quality_metrics, "output/data_quality_metrics.csv")
}

# ============================================================================
# MISSINGNESS ANALYSIS
# ============================================================================
if (exists("schema")) {
  inf("\n=== MISSINGNESS ANALYSIS ===")
  
  missingness <- schema %>%
    mutate(
      pct_missing = round(n_missing / nrow(df) * 100, 1),
      missingness_category = case_when(
        pct_missing == 0 ~ "Complete",
        pct_missing < 5 ~ "Minimal (<5%)",
        pct_missing < 10 ~ "Low (5-10%)",
        pct_missing < 20 ~ "Moderate (10-20%)",
        TRUE ~ "High (>20%)"
      )
    ) %>%
    arrange(desc(pct_missing))
  
  inf("Variables by missingness category:")
  inf(capture.output(table(missingness$missingness_category)) %>% paste(collapse = "\n"))
  
  if (SAVE_DIAGNOSTICS) save_csv(missingness, "output/missingness_analysis.csv")
}

# ============================================================================
# COMPLETENESS CHECK FOR QUOTA-MATCHED DATA
# ============================================================================
if ("Original_Classification" %in% names(df) && "Was_Reassigned" %in% names(df)) {
  inf("\n=== QUOTA MATCHING COMPLETENESS ===")
  
  # Check completeness by original vs reassigned
  completeness_by_reassignment <- df %>%
    group_by(Was_Reassigned) %>%
    summarise(
      n = n(),
      mean_progress = if("Progress" %in% names(df)) mean(Progress, na.rm = TRUE) else NA,
      complete_cases = sum(complete.cases(.)),
      .groups = "drop"
    )
  
  inf("Completeness by reassignment status:")
  inf(capture.output(completeness_by_reassignment) %>% paste(collapse = "\n"))
  
  if (SAVE_DIAGNOSTICS) {
    completeness_export <- completeness_by_reassignment %>%
      mutate(analysis_column = ANALYSIS_ROLE_COLUMN)
    save_csv(completeness_export, "output/completeness_by_reassignment.csv")
  }
}

# ============================================================================
# OVERALL STATS SNAPSHOT
# ============================================================================
inf("\n=== OVERALL STATISTICS ===")
inf("Total responses: ", nrow(df))
inf("Analysis role column: ", ANALYSIS_ROLE_COLUMN)
inf("With stakeholder classification: ", sum(!is.na(df$RoleCategory)))
if ("Q3.3" %in% names(df)) inf("With Q3.3 (financial/impact): ", sum(!is.na(df$Q3.3)))
if (!is.na(geo_col)) {
  inf("With geographic data (", geo_col, "): ", 
      sum(!is.na(df[[geo_col]]) & df[[geo_col]] != ""))
}

# Write final summary
if (SAVE_DIAGNOSTICS) {
  analysis_summary <- tibble::tibble(
    dataset = basename(INPUT_FINAL),
    analysis_column = ANALYSIS_ROLE_COLUMN,
    total_n = nrow(df),
    n_with_role = sum(!is.na(df$RoleCategory)),
    n_categories = n_distinct(df$RoleCategory, na.rm = TRUE),
    geographic_column = ifelse(is.na(geo_col), "None", geo_col),
    n_with_geography = if(!is.na(geo_col)) sum(!is.na(df[[geo_col]])) else 0,
    timestamp = Sys.time()
  )
  save_csv(analysis_summary, "output/analysis_summary.csv")
}

# ============================================================================
# FINAL SUMMARY
# ============================================================================
cat("\n")
cat("=" %.% rep("", 70), "\n")
cat("✓ MAIN ANALYSIS COMPLETED SUCCESSFULLY\n")
cat("=" %.% rep("", 70), "\n")
cat("\nAnalysis Configuration:\n")
cat("  Dataset: ", basename(INPUT_FINAL), " (N=", nrow(df), ")\n", sep = "")
cat("  Role Column: ", ANALYSIS_ROLE_COLUMN, "\n", sep = "")
if (!is.na(geo_col)) {
  cat("  Geographic: ", geo_col, " (anonymized)\n", sep = "")
}
cat("\nKey Outputs Generated:\n")
cat("  • Figure 4: Geographic distribution\n")
cat("  • Figure 6: Financial vs impact focus by stakeholder\n")
cat("  • Stakeholder distribution (quota-matched N=1,307)\n")
cat("  • Barrier analysis by stakeholder group\n")
if (length(risk_cols) > 0) cat("  • Risk perception heatmap\n")
if (exists("cor_matrix")) cat("  • Correlation matrix of key variables\n")
cat("\nAll figures saved to: figures/\n")
cat("All data files saved to: output/\n")
cat("\n✓ Ready for manuscript preparation\n")