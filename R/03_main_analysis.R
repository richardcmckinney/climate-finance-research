#!/usr/bin/env Rscript
# 03_main_analysis.R
# Purpose: Main statistical analysis for climate finance manuscript (FINAL N=1,307)
# Author: Richard McKinney
# Date: 2025-08-08
# Notes:
#   - Deterministic: sets seed + UTC (also done in run_all.R, but repeated for direct sourcing).
#   - Robust: harmonizes role column names, checks inputs, and fails with informative messages.
#   - CRITICAL FIX: Uses anonymized geographic data (hq_country/region) instead of raw Q2.2
#   - Outputs:
#       figures/figure4_geographic_distribution.png
#       output/figure4_geographic_distribution.csv
#       output/stakeholder_distribution_final.csv
#       figures/figure6_financial_impact_focus.png
#       output/figure6_financial_impact_focus.csv
#       output/barrier_presence_by_stakeholder.csv
#       output/dataset_schema_snapshot.csv

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
})

# ---- Configuration ------------------------------------------------------------
VERBOSE <- TRUE            # rich console logging for reviewers
SAVE_DIAGNOSTICS <- TRUE   # write extra CSVs used in the paper's methods/appendix
INPUT_FINAL <- "data/climate_finance_survey_final_1307.csv"

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

pick_role_col <- function(df) {
  # Accept legacy or canonical role columns
  cand <- c("Final_Role_Category", "final_category_appendix_j", "stakeholder_category")
  hit <- cand[cand %in% names(df)][1]
  if (is.na(hit) || !nzchar(hit))
    stop("Role category column not found. Expected one of: ", paste(cand, collapse = ", "))
  hit
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

# readr is faster/safer than base::read.csv and used by tidyverse
df <- readr::read_csv(INPUT_FINAL, show_col_types = FALSE, progress = FALSE)

inf("Loaded FINAL dataset:\n  File: ", INPUT_FINAL,
    "\n  Rows: ", nrow(df), "\n  Cols: ", ncol(df), "\n")

# Basic footprint for reviewers
if (SAVE_DIAGNOSTICS) {
  schema <- tibble::tibble(
    variable   = names(df),
    class      = vapply(df, function(x) class(x)[1], character(1)),
    n_missing  = vapply(df, function(x) sum(is.na(x)), integer(1))
  )
  save_csv(schema, "output/dataset_schema_snapshot.csv")
}

# Harmonize role column and make an ordered factor by size (for nicer plots/tables)
role_col <- pick_role_col(df)
df <- df %>%
  rename(RoleCategory = !!sym(role_col)) %>%
  mutate(RoleCategory = if (!is.factor(RoleCategory)) factor(RoleCategory) else RoleCategory) %>%
  group_by(RoleCategory) %>%
  mutate(.role_n = n()) %>%
  ungroup() %>%
  mutate(RoleCategory = fct_reorder(RoleCategory, .role_n, .desc = TRUE)) %>%
  select(-.role_n)

# ============================================================================
# FIGURE 4: Geographic Distribution of Survey Respondents
# ============================================================================
# CRITICAL FIX: Use anonymized geographic columns (hq_country/region) instead of raw Q2.2

# Priority order: anonymized columns first, then Q2.2 with warning
geo_candidates <- c("hq_country", "region", "geography", "country", "location")
geo_col <- intersect(geo_candidates, names(df))[1]

# Fallback to Q2.2 but with privacy warning
if (is.na(geo_col) && "Q2.2" %in% names(df)) {
  inf("! WARNING: Using raw Q2.2 column - should be anonymized for privacy!")
  inf("! Per README.md, geographic data must be generalized to regions.")
  geo_col <- "Q2.2"
}

if (!is.na(geo_col)) {
  inf("Geographic columns present:\n  ",
      paste(grep("country|region|location|geo|Q2\\.2|hq_", names(df), value = TRUE, ignore.case = TRUE),
            collapse = ", "))
  inf("Using column: ", geo_col, 
      if (geo_col == "Q2.2") " (WARNING: Not anonymized!)" else " (Anonymized)")

  geo_data <- df %>%
    filter(!is.na(.data[[geo_col]]) & .data[[geo_col]] != "") %>%
    count(.data[[geo_col]], name = "n", sort = TRUE) %>%
    rename(Region = 1) %>%
    mutate(percentage = n / sum(n) * 100)

  inf("\nGeographic distribution (", 
      if (geo_col == "Q2.2") "RAW - NOT ANONYMIZED" else "anonymized regions", 
      "):")
  inf(capture.output(utils::head(geo_data, 10)) %>% paste(collapse = "\n"))

  fig4 <- ggplot(geo_data, aes(x = "", y = percentage, fill = Region)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "right") +
    scale_fill_manual(values = rep(climate_colors, length.out = nrow(geo_data)), name = "Region") +
    labs(title = "Figure 4: Geographic Distribution of Survey Respondents",
         subtitle = paste0("N = ", sum(geo_data$n), 
                          if (geo_col == "Q2.2") " (WARNING: Raw Data)" else " (Anonymized Regions)"))

  save_plot("figures/figure4_geographic_distribution.png", fig4, width = 10, height = 6)
  if (SAVE_DIAGNOSTICS) save_csv(geo_data, "output/figure4_geographic_distribution.csv")
} else {
  inf("! No geographic column found — skipping Figure 4")
  inf("  Looked for: ", paste(geo_candidates, collapse = ", "), " and Q2.2")
}

# ============================================================================
# STAKEHOLDER DISTRIBUTION (post-classification, post-quota)
# ============================================================================
stakeholder_summary <- df %>%
  filter(!is.na(RoleCategory)) %>%
  count(RoleCategory, name = "n", sort = TRUE) %>%
  mutate(percentage = n / sum(n) * 100)

inf("=== STAKEHOLDER DISTRIBUTION (FINAL) ===")
inf(capture.output(stakeholder_summary) %>% paste(collapse = "\n"))
if (SAVE_DIAGNOSTICS) save_csv(stakeholder_summary, "output/stakeholder_distribution_final.csv")

# ============================================================================
# FIGURE 6: Financial vs. Impact Focus by Stakeholder Group
# ============================================================================
# Q3.3: 1 = pure impact ... 7 = pure financial (string in some exports)
if ("Q3.3" %in% names(df)) {
  df <- df %>% mutate(Q3_3_num = suppressWarnings(as.numeric(.data[["Q3.3"]])))

  impact_data <- df %>%
    filter(!is.na(RoleCategory), !is.na(Q3_3_num)) %>%
    group_by(RoleCategory) %>%
    summarise(
      n = dplyr::n(),
      mean_focus = mean(Q3_3_num, na.rm = TRUE),
      sd_focus   = sd(Q3_3_num,   na.rm = TRUE),
      se_focus   = sd_focus / sqrt(n),
      ci_lower   = mean_focus - 1.96 * se_focus,
      ci_upper   = mean_focus + 1.96 * se_focus,
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%   # reviewer-friendly sample size floor
    arrange(desc(mean_focus))

  inf("=== FINANCIAL vs IMPACT FOCUS BY STAKEHOLDER (Q3.3) ===")
  inf(capture.output(impact_data) %>% paste(collapse = "\n"))

  fig6 <- ggplot(impact_data, aes(x = reorder(RoleCategory, mean_focus), y = mean_focus)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    coord_flip() +
    scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
    labs(title = "Figure 6: Financial vs. Impact Focus by Stakeholder Group",
         subtitle = "1 = Pure Impact Focus, 7 = Pure Financial Focus (95% CI)",
         x = "Stakeholder Group",
         y = "Mean Investment Focus")

  save_plot("figures/figure6_financial_impact_focus.png", fig6, width = 10, height = 8)
  if (SAVE_DIAGNOSTICS) save_csv(impact_data, "output/figure6_financial_impact_focus.csv")
} else {
  inf("! Q3.3 missing — skipping Figure 6")
}

# ============================================================================
# BARRIER ANALYSIS (illustrative; aligns to manuscript's barrier constructs)
# ============================================================================
# Identify likely barrier columns; allow both coded Q3.11* and descriptive names.
barrier_cols <- grep("^(Q3\\.11|barrier|challenge)", names(df), value = TRUE, ignore.case = TRUE)

if (length(barrier_cols) > 0) {
  # Pivot to long format for concise group summaries. Using values_transform keeps types consistent.
  barrier_long <- df %>%
    select(RoleCategory, all_of(barrier_cols)) %>%
    pivot_longer(cols = -RoleCategory, names_to = "barrier_var", values_to = "val_raw",
                 values_transform = list(val_raw = as.character)) %>%
    mutate(val_present = !is.na(val_raw) & val_raw != "")

  barrier_summary <- barrier_long %>%
    group_by(RoleCategory, barrier_var) %>%
    summarise(
      n = dplyr::n(),
      pct_present = sum(val_present, na.rm = TRUE) / n * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%   # guard against tiny cells
    arrange(RoleCategory, desc(pct_present))

  inf("=== BARRIER ANALYSIS (presence % by stakeholder; first 12 rows) ===")
  inf(capture.output(utils::head(barrier_summary, 12)) %>% paste(collapse = "\n"))

  if (SAVE_DIAGNOSTICS) save_csv(barrier_summary, "output/barrier_presence_by_stakeholder.csv")
  
  # Additional barrier analysis: Overall barrier ranking
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
  
  inf("\n=== TOP BARRIERS OVERALL ===")
  inf(capture.output(utils::head(overall_barriers, 10)) %>% paste(collapse = "\n"))
} else {
  inf("! No barrier columns detected (Q3.11*/barrier*/challenge*) — skipping barrier analysis")
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
  
  if (SAVE_DIAGNOSTICS) save_csv(risk_summary, "output/risk_perception_by_stakeholder.csv")
  
  # Create risk perception heatmap data
  risk_means <- risk_data %>%
    group_by(RoleCategory) %>%
    summarise(across(all_of(risk_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
    filter(RoleCategory %in% stakeholder_summary$RoleCategory[1:10])  # Top 10 groups
  
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
  
  # Calculate technology preferences by stakeholder
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
  
  if (SAVE_DIAGNOSTICS) save_csv(tech_summary, "output/technology_preferences_by_stakeholder.csv")
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
  
  # Top investment criteria by stakeholder group
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
  
  if (SAVE_DIAGNOSTICS) save_csv(investment_summary, "output/investment_criteria_by_stakeholder.csv")
}

# ============================================================================
# CORRELATION ANALYSIS BETWEEN KEY VARIABLES
# ============================================================================
if ("Q3_3_num" %in% names(df) && length(risk_cols) > 0) {
  inf("\n=== CORRELATION ANALYSIS ===")
  
  # Select key numeric variables
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
    
    # Create correlation plot
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
# OVERALL STATS SNAPSHOT FOR METHODS
# ============================================================================
inf("\n=== OVERALL STATISTICS ===")
inf("Total responses: ", nrow(df))
inf("With stakeholder classification: ", sum(!is.na(df$RoleCategory)))
if ("Q3.3" %in% names(df)) inf("With Q3.3 (financial/impact): ", sum(!is.na(df$Q3.3)))
if (!is.na(geo_col)) {
  inf("With geographic data (", geo_col, "): ", 
      sum(!is.na(df[[geo_col]]) & df[[geo_col]] != ""))
  if (geo_col == "Q2.2") {
    inf("  ⚠ WARNING: Using raw geographic data - should be anonymized!")
  }
}

# Write a geographic summary table
if (SAVE_DIAGNOSTICS && !is.na(geo_col)) {
  geo_summary <- df %>%
    filter(!is.na(.data[[geo_col]]) & .data[[geo_col]] != "") %>%
    count(.data[[geo_col]], name = "n", sort = TRUE) %>%
    rename(Region = 1) %>%
    mutate(percentage = n / sum(n) * 100)
  save_csv(geo_summary, "output/geographic_distribution_final.csv")
}

# ============================================================================
# SAMPLE SIZE ADEQUACY CHECK
# ============================================================================
inf("\n=== SAMPLE SIZE ADEQUACY ===")

# Check sample sizes for main groups
sample_check <- stakeholder_summary %>%
  mutate(
    adequate_for_means = n >= 30,
    adequate_for_proportions = n >= 10,
    power_80_detect_d05 = n >= 64  # For detecting Cohen's d = 0.5 with 80% power
  )

inf("Groups with n >= 30 (adequate for means): ", sum(sample_check$adequate_for_means))
inf("Groups with n >= 10 (minimum for analysis): ", sum(sample_check$adequate_for_proportions))
inf("Groups with n >= 64 (80% power, d=0.5): ", sum(sample_check$power_80_detect_d05))

if (SAVE_DIAGNOSTICS) save_csv(sample_check, "output/sample_size_adequacy.csv")

# ============================================================================
# DATA QUALITY METRICS
# ============================================================================
inf("\n=== DATA QUALITY METRICS ===")

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
quality_metrics$Percentage <- round(quality_metrics$Count / nrow(df) * 100, 1)

inf(capture.output(quality_metrics) %>% paste(collapse = "\n"))

if (SAVE_DIAGNOSTICS) save_csv(quality_metrics, "output/data_quality_metrics.csv")

# ============================================================================
# MISSINGNESS ANALYSIS
# ============================================================================
inf("\n=== MISSINGNESS ANALYSIS ===")

# Calculate missingness by variable
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

# ============================================================================
# FINAL SUMMARY
# ============================================================================
cat("\n✓ Main analysis finished. See 'figures/' and 'output/' for artifacts.\n")
if (!is.na(geo_col) && geo_col == "Q2.2") {
  cat("  ⚠ WARNING: Analysis used raw Q2.2 geographic data.\n")
  cat("            Per README.md, this should be anonymized to regions for privacy.\n")
} else if (!is.na(geo_col)) {
  cat("  ✓ Using anonymized geographic data (", geo_col, ") per privacy requirements.\n")
}
cat("\nKey outputs:\n")
cat("  • Figure 4: Geographic distribution\n")
cat("  • Figure 6: Financial vs impact focus by stakeholder\n")
cat("  • Stakeholder distribution (N=1,307 quota-matched)\n")
cat("  • Barrier analysis by stakeholder group\n")
if (length(risk_cols) > 0) cat("  • Risk perception heatmap\n")
if (exists("cor_matrix")) cat("  • Correlation matrix of key variables\n")
cat("\nAll diagnostic CSVs saved to output/ directory.\n")