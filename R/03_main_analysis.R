#!/usr/bin/env Rscript
# 03_main_analysis.R
# Purpose: Main statistical analysis for climate finance manuscript (FINAL N=1,307)
# Version: 5.0 - ENHANCED with mandatory computations and Wilson CIs
# Author: Richard McKinney
# Date: 2025-08-09
#
# CRITICAL FIXES IN v5.0:
# 1. MANDATORY COMPUTATIONS: Geographic summary, headline percentages with Wilson CIs, screening counts
# 2. PRIVACY: Strict protection against using raw Q2.2 geographic data
# 3. SEED: Uses PIPELINE_SEED from config, not hardcoded value
# 4. WRITE_CSV: All calls include na="" parameter
# 5. VALIDATION: Enhanced checks for data integrity and column availability
#
# Notes:
#   - Deterministic: uses PIPELINE_SEED + UTC
#   - NEVER uses raw Q2.2 data - only anonymized geographic columns
#   - Outputs comprehensive figures and diagnostic CSVs for manuscript
#   - Can run standalone or as part of pipeline

# ---- Environment Setup -------------------------------------------------------
suppressPackageStartupMessages(library(methods))
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# ---- Load Configuration FIRST ------------------------------------------------
config_paths <- c("R/00_config.R", "00_config.R", "../R/00_config.R")
config_loaded <- FALSE

for (config_path in config_paths) {
  if (file.exists(config_path)) {
    source(config_path)
    config_loaded <- TRUE
    message("✓ Loaded configuration from: ", config_path)
    break
  }
}

if (!config_loaded) {
  stop("CRITICAL: Cannot find configuration file (00_config.R). ",
       "This file is required for proper pipeline execution.")
}

# ---- Determinism Setup (USING PIPELINE_SEED) --------------------------------
if (exists("PIPELINE_SEED")) {
  set.seed(PIPELINE_SEED)
  cat("✓ Using PIPELINE_SEED =", PIPELINE_SEED, "\n")
} else {
  # This should never happen if config loaded properly
  stop("PIPELINE_SEED not found in configuration. Check 00_config.R")
}

Sys.setenv(TZ = "UTC")
options(
  stringsAsFactors = FALSE,
  scipen = 999,  # Avoid scientific notation
  width = 120    # Wider console output
)

# ---- Libraries ---------------------------------------------------------------
suppressPackageStartupMessages({
  library(tidyverse)
  library(scales)
  library(rlang)
  library(cli)      # Better console output
  library(glue)     # String interpolation
  library(binom)    # For Wilson confidence intervals
})

# ---- Configuration & Parameters ----------------------------------------------
VERBOSE <- TRUE
SAVE_DIAGNOSTICS <- TRUE
MIN_GROUP_SIZE <- 10  # Minimum n for group analyses

# ---- Helper Functions --------------------------------------------------------
# Enhanced console output with cli
inf <- function(...) if (VERBOSE) cli_inform(paste0(...))

# Ensure directory exists
ensure_dir <- function(d) {
  if (!dir.exists(d)) {
    dir.create(d, recursive = TRUE, showWarnings = FALSE)
    cli_inform("Created directory: {.path {d}}")
  }
}

# Enhanced plot saving with validation
save_plot <- function(path, plot, width = 10, height = 6, dpi = 300) {
  tryCatch({
    ggsave(path, plot, width = width, height = height, dpi = dpi)
    cli_success("Saved plot: {.file {basename(path)}}")
  }, error = function(e) {
    cli_warn("Failed to save plot {basename(path)}: {e$message}")
  })
}

# Enhanced CSV saving with proper NA handling
save_csv <- function(df, path) {
  tryCatch({
    readr::write_csv(df, path, na = "")  # CRITICAL: Always include na parameter
    cli_success("Saved CSV: {.file {basename(path)}} ({nrow(df)} rows)")
  }, error = function(e) {
    cli_warn("Failed to save CSV {basename(path)}: {e$message}")
  })
}

# ---- Create Output Directories -----------------------------------------------
ensure_dir("figures")
ensure_dir("output")

# ---- Theme Configuration -----------------------------------------------------
theme_set(theme_minimal(base_size = 12))
climate_colors <- c("#2E7D32", "#1976D2", "#F57C00", "#7B1FA2", "#C62828",
                    "#00796B", "#5D4037", "#455A64")

# ---- Data Loading with Robust Fallback ---------------------------------------
cli_h1("Loading Data")

# Check if data already loaded by wrapper
if (!exists("df")) {
  cli_warn("Dataset not loaded from wrapper. Attempting standalone execution...")
  
  if (!exists("PATHS") || !file.exists(PATHS$final_1307)) {
    cli_abort(c(
      "Cannot run standalone: final dataset not found",
      "x" = "Expected at: {PATHS$final_1307}",
      "i" = "Run the complete pipeline first: Rscript R/00_run_all.R"
    ))
  }
  
  df <- readr::read_csv(PATHS$final_1307, show_col_types = FALSE, progress = FALSE)
  cli_success("Loaded {nrow(df)} rows from {.file {basename(PATHS$final_1307)}}")
}

# ==============================================================================
# MANDATORY COMPUTATIONS (AS PER FEEDBACK)
# ==============================================================================
cli_h1("Mandatory Computations for Manuscript")

# ---- 1. Geographic Summary (using anonymized region) ------------------------
cli_h2("1. Geographic Summary")

# Find the safe anonymized region column
SAFE_GEO_CANDIDATES <- c("region", "geography", "hq_region", "geo_region", "hq_country_grouped")
geo_col <- NULL

for (candidate in SAFE_GEO_CANDIDATES) {
  if (candidate %in% names(df)) {
    geo_col <- candidate
    cli_success("Found anonymized geographic column: {.field {geo_col}}")
    break
  }
}

if (!is.null(geo_col)) {
  region_summary <- df %>%
    filter(!is.na(.data[[geo_col]]) & .data[[geo_col]] != "") %>%
    count(.data[[geo_col]], name = "n", sort = TRUE) %>%
    rename(region = 1) %>%
    mutate(
      percentage = round(n / sum(n) * 100, 2),
      proportion = n / sum(n)
    )
  
  # Save mandatory geographic summary
  save_csv(region_summary, "output/region_summary_mandatory.csv")
  
  cli_inform("Geographic distribution (N = {sum(region_summary$n)}):")
  print(head(region_summary, 10))
} else {
  cli_warn("No anonymized geographic column found. Creating placeholder.")
  region_summary <- tibble(
    region = "Data not available",
    n = nrow(df),
    percentage = 100,
    proportion = 1
  )
  save_csv(region_summary, "output/region_summary_mandatory.csv")
}

# ---- 2. Headline Barrier Percentages with Wilson CIs ------------------------
cli_h2("2. Headline Barrier Percentages with Wilson CIs")

# Determine analysis role column
if (!exists("ANALYSIS_ROLE_COLUMN")) {
  candidates <- list(
    quota = "quota_target_category",
    final = "Final_Role_Category",
    fallback = "stakeholder_category"
  )
  
  ANALYSIS_ROLE_COLUMN <- NULL
  
  for (type in names(candidates)) {
    col <- candidates[[type]]
    if (col %in% names(df)) {
      ANALYSIS_ROLE_COLUMN <- col
      cli_success("Using role column: {.field {col}}")
      break
    }
  }
  
  if (is.null(ANALYSIS_ROLE_COLUMN)) {
    cli_abort("No valid role category column found")
  }
}

# Find barrier columns
barrier_cols <- names(df)[grepl("^(Q3\\.11|barrier|challenge)", names(df), ignore.case = TRUE)]

if (length(barrier_cols) > 0) {
  cli_inform("Computing headline percentages for {length(barrier_cols)} barriers")
  
  # Calculate barrier presence for each role
  headline_barriers <- df %>%
    filter(!is.na(.data[[ANALYSIS_ROLE_COLUMN]])) %>%
    rename(analysis_role = !!sym(ANALYSIS_ROLE_COLUMN)) %>%
    group_by(analysis_role) %>%
    summarise(
      n = n(),
      across(all_of(barrier_cols), 
             ~sum(!is.na(.x) & .x != "" & .x != "0", na.rm = TRUE),
             .names = "k_{.col}"),
      .groups = "drop"
    )
  
  # Calculate Wilson CIs for each barrier
  headline_with_ci <- headline_barriers %>%
    pivot_longer(
      cols = starts_with("k_"),
      names_to = "barrier",
      values_to = "k"
    ) %>%
    mutate(barrier = str_remove(barrier, "^k_")) %>%
    rowwise() %>%
    mutate(
      wilson_result = list(binom::binom.wilson(k, n)),
      percentage = round(k / n * 100, 2)
    ) %>%
    unnest_wider(wilson_result, names_sep = "_") %>%
    select(
      analysis_role, 
      barrier, 
      k, 
      n, 
      percentage,
      ci_lower = wilson_result_lower,
      ci_upper = wilson_result_upper
    ) %>%
    mutate(
      ci_lower_pct = round(ci_lower * 100, 2),
      ci_upper_pct = round(ci_upper * 100, 2),
      ci_95 = paste0("[", ci_lower_pct, ", ", ci_upper_pct, "]")
    )
  
  # Save headline barrier percentages
  save_csv(headline_with_ci, "output/headline_barriers_by_role_wilson.csv")
  
  # Create summary of top barriers
  top_barriers_summary <- headline_with_ci %>%
    group_by(barrier) %>%
    summarise(
      total_k = sum(k),
      total_n = sum(n),
      overall_pct = round(total_k / total_n * 100, 2),
      .groups = "drop"
    ) %>%
    arrange(desc(overall_pct)) %>%
    head(10)
  
  cli_inform("Top 5 barriers overall:")
  print(head(top_barriers_summary, 5))
  
} else {
  cli_warn("No barrier columns found. Creating placeholder.")
  headline_with_ci <- tibble(
    analysis_role = "No barriers found",
    barrier = NA_character_,
    k = 0,
    n = nrow(df),
    percentage = 0,
    ci_lower = 0,
    ci_upper = 0,
    ci_lower_pct = 0,
    ci_upper_pct = 0,
    ci_95 = "[0, 0]"
  )
  save_csv(headline_with_ci, "output/headline_barriers_by_role_wilson.csv")
}

# ---- 3. Screening Counts (for methods section) ------------------------------
cli_h2("3. Screening Counts for Methods Section")

# Try to load raw data for screening counts
screening_counts <- NULL

if (exists("PATHS") && !is.null(PATHS$raw) && file.exists(PATHS$raw)) {
  raw_data <- readr::read_csv(PATHS$raw, show_col_types = FALSE, progress = FALSE)
  raw_data$Progress <- suppressWarnings(as.numeric(raw_data$Progress))
  
  screening_counts <- tibble(
    stage = c("Total Raw Responses", 
              "Passed 10% Progress", 
              "Final Quota Sample"),
    n = c(nrow(raw_data),
          sum(raw_data$Progress >= 10, na.rm = TRUE),
          nrow(df)),
    percentage_of_raw = round(n / nrow(raw_data) * 100, 2)
  )
  
  cli_inform("Screening funnel:")
  print(screening_counts)
  
} else {
  cli_warn("Raw data not available. Using current dataset only.")
  screening_counts <- tibble(
    stage = c("Total Raw Responses", 
              "Passed 10% Progress", 
              "Final Quota Sample"),
    n = c(NA, NA, nrow(df)),
    percentage_of_raw = c(NA, NA, 100)
  )
}

# Save screening counts
save_csv(screening_counts, "output/screening_counts_mandatory.csv")

cli_rule(left = "Mandatory Computations Complete", line_col = "green")

# ---- CRITICAL: Determine Analysis Role Column --------------------------------
cli_h1("Determining Analysis Column")

# Already determined above, but ensure df_analysis is created
df_analysis <- df %>%
  rename(RoleCategory = !!sym(ANALYSIS_ROLE_COLUMN))

cli_inform("Analysis will use: {.strong {ANALYSIS_ROLE_COLUMN}}")

# Validate the column
n_valid <- sum(!is.na(df_analysis$RoleCategory))
if (n_valid != 1307) {
  cli_warn("Found {n_valid} valid role categories (expected 1,307)")
}

# ---- Data Preparation --------------------------------------------------------
cli_h1("Data Preparation")

# Convert to ordered factor by frequency
df_analysis <- df_analysis %>%
  mutate(RoleCategory = if_else(is.na(RoleCategory), "Unclassified", as.character(RoleCategory))) %>%
  mutate(RoleCategory = fct_infreq(factor(RoleCategory)))

# Prepare numeric versions of key variables
if ("Q3.3" %in% names(df_analysis)) {
  df_analysis <- df_analysis %>%
    mutate(Q3_3_num = suppressWarnings(as.numeric(Q3.3)))
}

# Save data schema for documentation
if (SAVE_DIAGNOSTICS) {
  schema <- tibble(
    variable = names(df_analysis),
    class = map_chr(df_analysis, ~class(.x)[1]),
    n_missing = map_int(df_analysis, ~sum(is.na(.x))),
    pct_missing = round(n_missing / nrow(df_analysis) * 100, 1)
  )
  
  save_csv(schema, "output/dataset_schema.csv")
  
  # Save analysis configuration
  config_info <- tibble(
    parameter = c("input_file", "analysis_role_column", "n_rows", "n_valid_roles", 
                  "seed", "geographic_column", "n_barriers", "timestamp"),
    value = c(
      basename(PATHS$final_1307),
      ANALYSIS_ROLE_COLUMN,
      as.character(nrow(df_analysis)),
      as.character(n_valid),
      as.character(PIPELINE_SEED),
      ifelse(!is.null(geo_col), geo_col, "None"),
      as.character(length(barrier_cols)),
      as.character(Sys.time())
    )
  )
  save_csv(config_info, "output/analysis_configuration.csv")
}

# ==============================================================================
# STAKEHOLDER DISTRIBUTION
# ==============================================================================
cli_h1("Stakeholder Distribution Analysis")

stakeholder_summary <- df_analysis %>%
  filter(RoleCategory != "Unclassified") %>%
  count(RoleCategory, name = "n", sort = TRUE) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 2),
    cumulative_pct = cumsum(percentage)
  )

# Display summary
cli_inform("Stakeholder distribution (N = {sum(stakeholder_summary$n)}):")
print(stakeholder_summary, n = 15)

# Critical validation
total_n <- sum(stakeholder_summary$n)
if (total_n != 1307) {
  cli_warn("Total N = {total_n} (expected 1,307)")
}

if (SAVE_DIAGNOSTICS) {
  stakeholder_export <- stakeholder_summary %>%
    mutate(
      source_column = ANALYSIS_ROLE_COLUMN,
      dataset = basename(PATHS$final_1307),
      export_timestamp = Sys.time()
    )
  save_csv(stakeholder_export, "output/stakeholder_distribution_final.csv")
}

# ==============================================================================
# FIGURE 4: GEOGRAPHIC DISTRIBUTION (WITH PRIVACY PROTECTION)
# ==============================================================================
cli_h1("Figure 4: Geographic Distribution")

# CRITICAL PRIVACY PROTECTION: Define forbidden columns
FORBIDDEN_GEO_COLS <- c("Q2.2", "q2.2", "Q2_2", "Q2.2_raw", "raw_location", "exact_location")

# Check if any forbidden columns exist
forbidden_present <- intersect(FORBIDDEN_GEO_COLS, names(df_analysis))
if (length(forbidden_present) > 0) {
  cli_abort(c(
    "PRIVACY VIOLATION: Raw geographic data detected",
    "x" = "Found forbidden columns: {paste(forbidden_present, collapse = ', ')}",
    "!" = "This violates privacy requirements",
    "i" = "Run anonymization first: Rscript R/01_anonymize_data.R"
  ))
}

# Use the geo_col determined in mandatory computations
if (!is.null(geo_col) && geo_col %in% names(df_analysis)) {
  geo_data <- df_analysis %>%
    filter(!is.na(.data[[geo_col]]) & .data[[geo_col]] != "") %>%
    count(.data[[geo_col]], name = "n", sort = TRUE) %>%
    rename(Region = 1) %>%
    mutate(
      percentage = round(n / sum(n) * 100, 2),
      label = paste0(Region, "\n(", percentage, "%)")
    )
  
  cli_inform("Geographic distribution (top 10):")
  print(head(geo_data, 10))
  
  # Create pie chart
  fig4 <- ggplot(geo_data, aes(x = "", y = percentage, fill = Region)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(
      legend.position = "right",
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    scale_fill_manual(
      values = rep(climate_colors, length.out = nrow(geo_data)),
      name = "Region"
    ) +
    labs(
      title = "Figure 4: Geographic Distribution of Survey Respondents",
      subtitle = glue("N = {sum(geo_data$n)} (Anonymized Regions)")
    )
  
  save_plot("figures/figure4_geographic_distribution.png", fig4, width = 10, height = 6)
  
  if (SAVE_DIAGNOSTICS) {
    geo_export <- geo_data %>%
      mutate(
        source_column = geo_col,
        privacy_status = "anonymized",
        export_timestamp = Sys.time()
      )
    save_csv(geo_export, "output/figure4_geographic_distribution.csv")
  }
} else {
  cli_warn(c(
    "No safe geographic data available for Figure 4",
    "i" = "Ensure data has been properly anonymized"
  ))
}

# ==============================================================================
# FIGURE 6: FINANCIAL VS. IMPACT FOCUS
# ==============================================================================
cli_h1("Figure 6: Financial vs. Impact Focus")

if ("Q3_3_num" %in% names(df_analysis)) {
  impact_data <- df_analysis %>%
    filter(!is.na(RoleCategory), !is.na(Q3_3_num), RoleCategory != "Unclassified") %>%
    group_by(RoleCategory) %>%
    summarise(
      n = n(),
      mean_focus = mean(Q3_3_num, na.rm = TRUE),
      sd_focus = sd(Q3_3_num, na.rm = TRUE),
      se_focus = sd_focus / sqrt(n),
      ci_lower = mean_focus - 1.96 * se_focus,
      ci_upper = mean_focus + 1.96 * se_focus,
      .groups = "drop"
    ) %>%
    filter(n >= MIN_GROUP_SIZE) %>%
    arrange(desc(mean_focus))
  
  cli_inform("Financial vs Impact Focus by Stakeholder:")
  print(impact_data)
  
  # Create lollipop chart
  fig6 <- ggplot(impact_data, aes(x = reorder(RoleCategory, mean_focus), y = mean_focus)) +
    geom_segment(aes(xend = RoleCategory, y = 4, yend = mean_focus),
                 color = "gray70", size = 0.5) +
    geom_point(size = 4, color = "#1976D2") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper),
                  width = 0.2, color = "#1976D2", alpha = 0.7) +
    coord_flip() +
    scale_y_continuous(
      limits = c(1, 7),
      breaks = 1:7,
      labels = c("1\nPure\nImpact", "2", "3", "4\nBalanced", "5", "6", "7\nPure\nFinancial")
    ) +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      axis.text.y = element_text(size = 10),
      plot.title = element_text(size = 14, face = "bold"),
      plot.subtitle = element_text(size = 11)
    ) +
    labs(
      title = "Figure 6: Financial vs. Impact Focus by Stakeholder Group",
      subtitle = glue("Mean scores with 95% CI | Total N = {sum(impact_data$n)}"),
      x = "Stakeholder Group",
      y = "Investment Focus Score"
    )
  
  save_plot("figures/figure6_financial_impact_focus.png", fig6, width = 10, height = 8)
  
  if (SAVE_DIAGNOSTICS) {
    impact_export <- impact_data %>%
      mutate(
        source_column = ANALYSIS_ROLE_COLUMN,
        analysis_date = Sys.date()
      )
    save_csv(impact_export, "output/figure6_financial_impact_focus.csv")
  }
} else {
  cli_warn("Q3.3 not found - skipping Figure 6")
}

# ==============================================================================
# ENHANCED BARRIER ANALYSIS (WITH WILSON CIs)
# ==============================================================================
cli_h1("Enhanced Barrier Analysis")

if (length(barrier_cols) > 0) {
  cli_inform("Analyzing {length(barrier_cols)} barrier variables")
  
  # Convert to long format
  barrier_long <- df_analysis %>%
    select(RoleCategory, all_of(barrier_cols)) %>%
    filter(RoleCategory != "Unclassified") %>%
    pivot_longer(
      cols = -RoleCategory,
      names_to = "barrier_var",
      values_to = "val_raw",
      values_transform = list(val_raw = as.character)
    ) %>%
    mutate(
      val_present = !is.na(val_raw) & val_raw != "" & val_raw != "0",
      barrier_clean = str_remove(barrier_var, "^Q3\\.11_?")
    )
  
  # Summary by stakeholder with Wilson CIs
  barrier_summary <- barrier_long %>%
    group_by(RoleCategory, barrier_clean) %>%
    summarise(
      n_responses = n(),
      k_present = sum(val_present, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    filter(n_responses >= MIN_GROUP_SIZE) %>%
    rowwise() %>%
    mutate(
      wilson_result = list(binom::binom.wilson(k_present, n_responses)),
      pct_present = round(k_present / n_responses * 100, 2)
    ) %>%
    unnest_wider(wilson_result, names_sep = "_") %>%
    select(
      RoleCategory,
      barrier_clean,
      k_present,
      n_responses,
      pct_present,
      ci_lower = wilson_result_lower,
      ci_upper = wilson_result_upper
    ) %>%
    mutate(
      ci_lower_pct = round(ci_lower * 100, 2),
      ci_upper_pct = round(ci_upper * 100, 2)
    ) %>%
    arrange(RoleCategory, desc(pct_present))
  
  # Overall ranking with Wilson CIs
  overall_barriers <- barrier_long %>%
    group_by(barrier_clean) %>%
    summarise(
      n_total = n(),
      k_present = sum(val_present, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rowwise() %>%
    mutate(
      wilson_result = list(binom::binom.wilson(k_present, n_total)),
      pct_overall = round(k_present / n_total * 100, 2)
    ) %>%
    unnest_wider(wilson_result, names_sep = "_") %>%
    select(
      barrier_clean,
      k_present,
      n_total,
      pct_overall,
      ci_lower = wilson_result_lower,
      ci_upper = wilson_result_upper
    ) %>%
    mutate(
      ci_lower_pct = round(ci_lower * 100, 2),
      ci_upper_pct = round(ci_upper * 100, 2)
    ) %>%
    arrange(desc(pct_overall))
  
  cli_inform("Top 10 barriers overall (with Wilson CIs):")
  print(head(overall_barriers, 10))
  
  if (SAVE_DIAGNOSTICS) {
    save_csv(barrier_summary, "output/barrier_presence_by_stakeholder_wilson.csv")
    save_csv(overall_barriers, "output/overall_barrier_ranking_wilson.csv")
  }
  
  # Create heatmap for top barriers
  if (nrow(overall_barriers) > 0) {
    top_barriers <- head(overall_barriers$barrier_clean, 10)
    
    heatmap_data <- barrier_summary %>%
      filter(barrier_clean %in% top_barriers) %>%
      select(RoleCategory, barrier_clean, pct_present)
    
    if (nrow(heatmap_data) > 0) {
      fig_barriers <- ggplot(heatmap_data, 
                            aes(x = barrier_clean, y = RoleCategory, fill = pct_present)) +
        geom_tile(color = "white", size = 0.5) +
        scale_fill_gradient2(
          low = "#2E7D32",
          mid = "#FFC107",
          high = "#C62828",
          midpoint = 50,
          limits = c(0, 100),
          name = "% Reporting\nBarrier"
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(size = 14, face = "bold")
        ) +
        labs(
          title = "Barrier Prevalence by Stakeholder Group",
          subtitle = "Top 10 barriers across all groups",
          x = "Barrier Type",
          y = "Stakeholder Group"
        )
      
      save_plot("figures/barrier_heatmap.png", fig_barriers, width = 12, height = 8)
    }
  }
} else {
  cli_warn("No barrier variables found")
}

# ==============================================================================
# RISK PERCEPTION ANALYSIS
# ==============================================================================
cli_h1("Risk Perception Analysis")

risk_cols <- names(df_analysis)[grepl("^(Q3\\.6|risk)", names(df_analysis), ignore.case = TRUE)]

if (length(risk_cols) > 0) {
  cli_inform("Found {length(risk_cols)} risk variables")
  
  # Convert to numeric
  risk_data <- df_analysis %>%
    select(RoleCategory, all_of(risk_cols)) %>%
    filter(RoleCategory != "Unclassified") %>%
    mutate(across(all_of(risk_cols), ~suppressWarnings(as.numeric(.x))))
  
  # Calculate means by stakeholder
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
    filter(n >= MIN_GROUP_SIZE)
  
  if (SAVE_DIAGNOSTICS) {
    save_csv(risk_summary, "output/risk_perception_by_stakeholder.csv")
  }
  
  # Create risk heatmap
  if (nrow(risk_summary) > 0 && length(risk_cols) > 1) {
    risk_means <- risk_data %>%
      group_by(RoleCategory) %>%
      summarise(across(all_of(risk_cols), ~mean(.x, na.rm = TRUE)), .groups = "drop") %>%
      filter(RoleCategory %in% head(levels(risk_data$RoleCategory), 10))
    
    risk_heatmap_data <- risk_means %>%
      pivot_longer(cols = -RoleCategory, names_to = "Risk_Type", values_to = "Mean_Score") %>%
      mutate(Risk_Type = str_remove(Risk_Type, "^Q3\\.6_?"))
    
    fig_risk <- ggplot(risk_heatmap_data, 
                       aes(x = Risk_Type, y = RoleCategory, fill = Mean_Score)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "#2E7D32",
        mid = "#FFC107",
        high = "#C62828",
        midpoint = 4,
        limits = c(1, 7),
        name = "Risk Level"
      ) +
      theme_minimal(base_size = 10) +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14, face = "bold")
      ) +
      labs(
        title = "Risk Perception Heatmap by Stakeholder Group",
        subtitle = glue("Based on {ANALYSIS_ROLE_COLUMN} | N={sum(risk_summary$n)}"),
        x = "Risk Type",
        y = "Stakeholder Group"
      )
    
    save_plot("figures/risk_perception_heatmap.png", fig_risk, width = 12, height = 8)
  }
} else {
  cli_warn("No risk perception variables found")
}

# ==============================================================================
# TECHNOLOGY PREFERENCE ANALYSIS
# ==============================================================================
cli_h1("Technology Preference Analysis")

tech_cols <- names(df_analysis)[grepl("^(Q12\\.|technology|tech_|solution)", 
                                      names(df_analysis), ignore.case = TRUE)]

if (length(tech_cols) > 0) {
  cli_inform("Found {length(tech_cols)} technology variables")
  
  tech_data <- df_analysis %>%
    select(RoleCategory, all_of(tech_cols)) %>%
    filter(RoleCategory != "Unclassified") %>%
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
    filter(n >= MIN_GROUP_SIZE)
  
  if (SAVE_DIAGNOSTICS) {
    save_csv(tech_summary, "output/technology_preferences_by_stakeholder.csv")
  }
}

# ==============================================================================
# CORRELATION ANALYSIS
# ==============================================================================
cli_h1("Correlation Analysis")

if ("Q3_3_num" %in% names(df_analysis) && length(risk_cols) > 0) {
  numeric_vars <- df_analysis %>%
    select(any_of(c("Q3_3_num", "Progress", risk_cols[1:min(5, length(risk_cols))]))) %>%
    select_if(is.numeric)
  
  if (ncol(numeric_vars) > 1) {
    cor_matrix <- cor(numeric_vars, use = "pairwise.complete.obs")
    
    if (SAVE_DIAGNOSTICS) {
      cor_df <- as_tibble(cor_matrix, rownames = "Variable")
      save_csv(cor_df, "output/correlation_matrix_key_variables.csv")
    }
    
    # Create correlation plot if corrplot available
    if (requireNamespace("corrplot", quietly = TRUE)) {
      png("figures/correlation_matrix.png", width = 10, height = 8, units = "in", res = 150)
      corrplot::corrplot(
        cor_matrix,
        method = "color",
        type = "upper",
        order = "hclust",
        addCoef.col = "black",
        number.cex = 0.7,
        tl.col = "black",
        tl.srt = 45,
        title = "Correlation Matrix of Key Variables",
        mar = c(0, 0, 2, 0)
      )
      dev.off()
      cli_success("Saved correlation matrix plot")
    }
  }
}

# ==============================================================================
# SAMPLE SIZE ADEQUACY CHECK
# ==============================================================================
cli_h1("Sample Size Adequacy Check")

sample_check <- stakeholder_summary %>%
  mutate(
    adequate_for_means = n >= 30,
    adequate_for_proportions = n >= 10,
    power_80_detect_d05 = n >= 64  # For detecting Cohen's d = 0.5 with 80% power
  )

cli_inform("Sample size adequacy:")
cli_inform("  Groups with n >= 30 (adequate for means): {sum(sample_check$adequate_for_means)}")
cli_inform("  Groups with n >= 10 (minimum for analysis): {sum(sample_check$adequate_for_proportions)}")
cli_inform("  Groups with n >= 64 (80% power, d=0.5): {sum(sample_check$power_80_detect_d05)}")

if (SAVE_DIAGNOSTICS) {
  save_csv(sample_check, "output/sample_size_adequacy.csv")
}

# ==============================================================================
# DATA QUALITY METRICS
# ==============================================================================
cli_h1("Data Quality Metrics")

quality_metrics <- tibble(
  Metric = character(),
  Count = numeric(),
  Percentage = numeric()
)

# Basic metrics
quality_metrics <- bind_rows(
  quality_metrics,
  tibble(
    Metric = "Total Responses",
    Count = nrow(df_analysis),
    Percentage = 100
  ),
  tibble(
    Metric = "Complete Cases",
    Count = sum(complete.cases(df_analysis)),
    Percentage = round(sum(complete.cases(df_analysis)) / nrow(df_analysis) * 100, 1)
  )
)

# Progress-based metrics if available
if ("Progress" %in% names(df_analysis)) {
  progress_metrics <- tibble(
    Metric = c("Progress >= 90%", "Progress >= 75%", "Progress >= 50%"),
    Count = c(
      sum(df_analysis$Progress >= 90, na.rm = TRUE),
      sum(df_analysis$Progress >= 75, na.rm = TRUE),
      sum(df_analysis$Progress >= 50, na.rm = TRUE)
    )
  ) %>%
    mutate(Percentage = round(Count / nrow(df_analysis) * 100, 1))
  
  quality_metrics <- bind_rows(quality_metrics, progress_metrics)
}

print(quality_metrics)

if (SAVE_DIAGNOSTICS) {
  save_csv(quality_metrics, "output/data_quality_metrics.csv")
}

# ==============================================================================
# MISSINGNESS ANALYSIS
# ==============================================================================
cli_h1("Missingness Analysis")

if (exists("schema")) {
  missingness <- schema %>%
    mutate(
      missingness_category = case_when(
        pct_missing == 0 ~ "Complete",
        pct_missing < 5 ~ "Minimal (<5%)",
        pct_missing < 10 ~ "Low (5-10%)",
        pct_missing < 20 ~ "Moderate (10-20%)",
        TRUE ~ "High (>20%)"
      )
    ) %>%
    arrange(desc(pct_missing))
  
  missingness_summary <- missingness %>%
    count(missingness_category) %>%
    mutate(percentage = round(n / sum(n) * 100, 1))
  
  cli_inform("Variables by missingness category:")
  print(missingness_summary)
  
  if (SAVE_DIAGNOSTICS) {
    save_csv(missingness, "output/missingness_analysis.csv")
  }
}

# ==============================================================================
# QUOTA MATCHING VALIDATION
# ==============================================================================
if (all(c("Original_Classification", "Was_Reassigned") %in% names(df_analysis))) {
  cli_h1("Quota Matching Validation")
  
  completeness_by_reassignment <- df_analysis %>%
    group_by(Was_Reassigned) %>%
    summarise(
      n = n(),
      mean_progress = if("Progress" %in% names(df_analysis)) {
        mean(Progress, na.rm = TRUE)
      } else {
        NA_real_
      },
      complete_cases = sum(complete.cases(.)),
      .groups = "drop"
    )
  
  cli_inform("Completeness by reassignment status:")
  print(completeness_by_reassignment)
  
  if (SAVE_DIAGNOSTICS) {
    save_csv(completeness_by_reassignment, "output/completeness_by_reassignment.csv")
  }
}

# ==============================================================================
# FINAL SUMMARY REPORT
# ==============================================================================
cli_h1("Analysis Complete")

# Generate summary statistics
summary_stats <- list(
  total_responses = nrow(df_analysis),
  analysis_column = ANALYSIS_ROLE_COLUMN,
  n_with_classification = sum(df_analysis$RoleCategory != "Unclassified"),
  n_categories = n_distinct(df_analysis$RoleCategory[df_analysis$RoleCategory != "Unclassified"]),
  geographic_column = ifelse(!is.null(geo_col), geo_col, "None"),
  n_with_geography = if(!is.null(geo_col) && geo_col %in% names(df_analysis)) {
    sum(!is.na(df_analysis[[geo_col]]))
  } else {
    0
  },
  n_barriers_analyzed = length(barrier_cols),
  seed_used = PIPELINE_SEED,
  timestamp = Sys.time()
)

# Create final summary
analysis_summary <- tibble(
  parameter = names(summary_stats),
  value = as.character(summary_stats)
)

if (SAVE_DIAGNOSTICS) {
  save_csv(analysis_summary, "output/analysis_summary_final.csv")
}

# Display final summary
cli_rule(left = "ANALYSIS COMPLETED SUCCESSFULLY", line_col = "green")
cli_inform("")
cli_inform("Configuration:")
cli_inform("  Dataset: {.file {basename(PATHS$final_1307)}} (N={nrow(df_analysis)})")
cli_inform("  Role Column: {.field {ANALYSIS_ROLE_COLUMN}}")
cli_inform("  Seed: {PIPELINE_SEED}")
if (!is.null(geo_col)) {
  cli_inform("  Geographic: {.field {geo_col}} (anonymized)")
}

cli_inform("")
cli_inform("Mandatory Outputs Generated:")
cli_inform("  ✓ Geographic summary (region_summary_mandatory.csv)")
cli_inform("  ✓ Headline barriers with Wilson CIs (headline_barriers_by_role_wilson.csv)")
cli_inform("  ✓ Screening counts (screening_counts_mandatory.csv)")

cli_inform("")
cli_inform("Additional Outputs Generated:")
cli_inform("  • Stakeholder distribution (N={sum(stakeholder_summary$n)})")
if (!is.null(geo_col)) {
  cli_inform("  • Figure 4: Geographic distribution")
}
if ("Q3_3_num" %in% names(df_analysis)) {
  cli_inform("  • Figure 6: Financial vs impact focus")
}
if (length(barrier_cols) > 0) {
  cli_inform("  • Enhanced barrier analysis with Wilson CIs")
}
if (length(risk_cols) > 0) {
  cli_inform("  • Risk perception heatmap")
}
if (exists("cor_matrix")) {
  cli_inform("  • Correlation matrix")
}

cli_inform("")
cli_inform("All figures saved to: {.path figures/}")
cli_inform("All data files saved to: {.path output/}")
cli_inform("")
cli_success("Ready for manuscript preparation")