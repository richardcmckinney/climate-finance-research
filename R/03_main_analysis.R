# 03_main_analysis.R
# Purpose: Main statistical analysis for climate finance manuscript (FINAL N=1,307)
# Author: Richard McKinney
# Date: 2025-08-08

if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(scales)
})

# ---- Configuration ------------------------------------------------------------
VERBOSE <- TRUE           # rich console logging for reviewers
SAVE_DIAGNOSTICS <- TRUE  # write extra CSVs used in the paper's methods/appendix

# ---- Folders ------------------------------------------------------------------
dir.create("figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output",  recursive = TRUE, showWarnings = FALSE)

# ---- Theme & palette ----------------------------------------------------------
theme_set(theme_minimal(base_size = 12))
climate_colors <- c("#2E7D32", "#1976D2", "#F57C00", "#7B1FA2", "#C62828",
                    "#00796B", "#5D4037", "#455A64")

# ---- Load FINAL dataset (post-quota match) -----------------------------------
infile <- "data/climate_finance_survey_final_1307.csv"
stopifnot(file.exists(infile))
df <- read.csv(infile, stringsAsFactors = FALSE)

if (VERBOSE) {
  cat("Loaded FINAL dataset:\n  File: ", infile, "\n  Rows: ", nrow(df),
      "\n  Cols: ", ncol(df), "\n\n", sep = "")
}

# Basic footprint for reviewers
if (SAVE_DIAGNOSTICS) {
  write.csv(
    data.frame(variable = names(df),
               class = vapply(df, function(x) class(x)[1], character(1)),
               n_missing = vapply(df, function(x) sum(is.na(x)), integer(1))),
    "output/dataset_schema_snapshot.csv",
    row.names = FALSE
  )
}

# ============================================================================
# FIGURE 4: Geographic Distribution of Survey Respondents
# ============================================================================

if ("Q2.2" %in% names(df)) {
  if (VERBOSE) {
    cat("Geographic columns present:\n")
    print(grep("country|region|location|geo|Q2.2", names(df), value = TRUE, ignore.case = TRUE))
  }

  geo_data <- df %>%
    filter(!is.na(Q2.2) & Q2.2 != "") %>%
    count(Q2.2, name = "n") %>%
    mutate(percentage = n / sum(n) * 100) %>%
    arrange(desc(n))

  if (VERBOSE) {
    cat("\nGeographic distribution (top 10 shown):\n")
    print(head(geo_data, 10))
  }

  fig4 <- ggplot(geo_data, aes(x = "", y = percentage, fill = Q2.2)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    theme_void() +
    theme(legend.position = "right") +
    scale_fill_manual(values = rep(climate_colors, length.out = nrow(geo_data)), name = "Region") +
    labs(title = "Figure 4: Geographic Distribution of Survey Respondents",
         subtitle = paste0("N = ", sum(geo_data$n)))

  ggsave("figures/figure4_geographic_distribution.png", fig4, width = 10, height = 6, dpi = 300)
  if (SAVE_DIAGNOSTICS) {
    write.csv(geo_data, "output/figure4_geographic_distribution.csv", row.names = FALSE)
  }
  if (VERBOSE) cat("✓ Figure 4 saved: figures/figure4_geographic_distribution.png\n\n")
} else if (VERBOSE) {
  cat("! Q2.2 not found — skipping Figure 4\n\n")
}

# ============================================================================
# STAKEHOLDER DISTRIBUTION (post-classification, post-quota)
# ============================================================================

if ("Final_Role_Category" %in% names(df)) {
  stakeholder_summary <- df %>%
    filter(!is.na(Final_Role_Category)) %>%
    count(Final_Role_Category, name = "n") %>%
    mutate(percentage = n / sum(n) * 100) %>%
    arrange(desc(n))

  if (VERBOSE) {
    cat("=== STAKEHOLDER DISTRIBUTION (FINAL) ===\n")
    print(stakeholder_summary)
    cat("\n")
  }

  if (SAVE_DIAGNOSTICS) {
    write.csv(stakeholder_summary, "output/stakeholder_distribution_final.csv", row.names = FALSE)
  }
} else if (VERBOSE) {
  cat("! Final_Role_Category not present — cannot compute stakeholder distribution\n\n")
}

# ============================================================================
# FIGURE 6: Financial vs. Impact Focus by Stakeholder Group
# ============================================================================

if (all(c("Q3.3", "Final_Role_Category") %in% names(df))) {
  df <- df %>% mutate(Q3_3_num = suppressWarnings(as.numeric(Q3.3)))

  impact_data <- df %>%
    filter(!is.na(Final_Role_Category), !is.na(Q3_3_num)) %>%
    group_by(Final_Role_Category) %>%
    summarise(
      n = n(),
      mean_focus = mean(Q3_3_num, na.rm = TRUE),
      sd_focus   = sd(Q3_3_num,   na.rm = TRUE),
      se_focus   = sd_focus / sqrt(n),
      ci_lower   = mean_focus - 1.96 * se_focus,
      ci_upper   = mean_focus + 1.96 * se_focus,
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%   # reviewer-friendly sample size floor
    arrange(desc(mean_focus))

  if (VERBOSE) {
    cat("=== FINANCIAL vs IMPACT FOCUS BY STAKEHOLDER (Q3.3) ===\n")
    print(impact_data)
    cat("\n")
  }

  fig6 <- ggplot(impact_data, aes(x = reorder(Final_Role_Category, mean_focus), y = mean_focus)) +
    geom_point(size = 3) +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), width = 0.2) +
    coord_flip() +
    scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
    labs(title = "Figure 6: Financial vs. Impact Focus by Stakeholder Group",
         subtitle = "1 = Pure Impact Focus, 7 = Pure Financial Focus (95% CI)",
         x = "Stakeholder Group",
         y = "Mean Investment Focus")

  ggsave("figures/figure6_financial_impact_focus.png", fig6, width = 10, height = 8, dpi = 300)
  if (SAVE_DIAGNOSTICS) {
    write.csv(impact_data, "output/figure6_financial_impact_focus.csv", row.names = FALSE)
  }
  if (VERBOSE) cat("✓ Figure 6 saved: figures/figure6_financial_impact_focus.png\n\n")
} else if (VERBOSE) {
  cat("! Q3.3 or Final_Role_Category missing — skipping Figure 6\n\n")
}

# ============================================================================
# BARRIER ANALYSIS (illustrative; aligns to manuscript’s barrier constructs)
# ============================================================================

barrier_cols <- grep("^(Q3\\.11|barrier|challenge)", names(df), value = TRUE, ignore.case = TRUE)

if (length(barrier_cols) > 0 && "Final_Role_Category" %in% names(df)) {
  # Compute a simple presence rate for each candidate barrier column, by stakeholder
  barrier_long <- df %>%
    select(Final_Role_Category, all_of(barrier_cols)) %>%
    pivot_longer(cols = -Final_Role_Category, names_to = "barrier_var", values_to = "val_raw") %>%
    mutate(val_present = !is.na(val_raw) & val_raw != "")

  barrier_summary <- barrier_long %>%
    group_by(Final_Role_Category, barrier_var) %>%
    summarise(n = n(),
              pct_present = sum(val_present, na.rm = TRUE) / n * 100,
              .groups = "drop") %>%
    filter(n >= 10) %>%
    arrange(Final_Role_Category, desc(pct_present))

  if (VERBOSE) {
    cat("=== BARRIER ANALYSIS (presence % by stakeholder; first 12 rows) ===\n")
    print(head(barrier_summary, 12))
    cat("\n")
  }

  if (SAVE_DIAGNOSTICS) {
    write.csv(barrier_summary, "output/barrier_presence_by_stakeholder.csv", row.names = FALSE)
  }
} else if (VERBOSE) {
  cat("! No barrier columns detected (Q3.11*/barrier*/challenge*) or Final_Role_Category missing — skipping barrier analysis\n\n")
}

# ============================================================================
# OVERALL STATS SNAPSHOT FOR METHODS
# ============================================================================

if (VERBOSE) {
  cat("=== OVERALL STATISTICS ===\n")
  cat("Total responses: ", nrow(df), "\n", sep = "")
  cat("With stakeholder classification: ", sum(!is.na(df$Final_Role_Category)), "\n", sep = "")
  cat("With Q3.3 (financial/impact): ", sum(!is.na(df$Q3.3)), "\n", sep = "")

  if ("Q2.2" %in% names(df)) {
    cat("With Q2.2 (geography): ", sum(!is.na(df$Q2.2) & df$Q2.2 != ""), "\n", sep = "")
  }
  cat("\n")
}

if (SAVE_DIAGNOSTICS && "Q2.2" %in% names(df)) {
  geo_summary <- df %>%
    filter(!is.na(Q2.2) & Q2.2 != "") %>%
    count(Q2.2, name = "n") %>%
    mutate(percentage = n / sum(n) * 100) %>%
    arrange(desc(n))
  write.csv(geo_summary, "output/geographic_distribution_final.csv", row.names = FALSE)
}

cat("✓ Main analysis finished. See 'figures/' and 'output/' for artifacts.\n")