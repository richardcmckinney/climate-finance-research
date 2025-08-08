# 03_main_analysis.R
# Purpose: Main statistical analysis for climate finance manuscript
# Author: Richard McKinney
# Date: 2025-08-07

# Load required libraries
library(tidyverse)
library(ggplot2)
library(scales)

# Set theme for plots
theme_set(theme_minimal(base_size = 12))

# Custom color palette
climate_colors <- c("#2E7D32", "#1976D2", "#F57C00", "#7B1FA2", "#C62828", 
                    "#00796B", "#5D4037", "#455A64")

# Load classified data
df <- read.csv("data/climate_finance_survey_classified.csv", 
               stringsAsFactors = FALSE)

cat("Loaded", nrow(df), "responses with stakeholder classifications\n\n")

# ============================================================================
# FIGURE 4: Geographic Distribution of Survey Respondents
# ============================================================================

# First, let's check what geographic columns we have
cat("Geographic columns available:\n")
geo_cols <- grep("country|region|location|geo|Q2.2", names(df), value = TRUE, ignore.case = TRUE)
print(geo_cols)

# Use Q2.2 for geographic location (you mentioned it was generalized to regions)
if("Q2.2" %in% names(df)) {
  geo_data <- df %>%
    filter(!is.na(Q2.2) & Q2.2 != "") %>%
    count(Q2.2) %>%
    mutate(percentage = n/sum(n) * 100) %>%
    arrange(desc(n))
  
  cat("\nGeographic distribution:\n")
  print(geo_data)
  
  # Create pie chart
  fig4 <- ggplot(geo_data, aes(x = "", y = percentage, fill = Q2.2)) +
    geom_bar(stat = "identity", width = 1) +
    coord_polar("y", start = 0) +
    theme_void() +
    theme(legend.position = "right") +
    scale_fill_manual(values = climate_colors, name = "Region") +
    labs(title = "Figure 4: Geographic Distribution of Survey Respondents",
         subtitle = paste0("N = ", sum(geo_data$n))) +
    geom_text(aes(label = paste0(round(percentage, 0), "%")), 
              position = position_stack(vjust = 0.5))
  
  ggsave("figures/figure4_geographic_distribution.png", 
         fig4, width = 10, height = 6, dpi = 300)
  cat("Figure 4 saved to figures/figure4_geographic_distribution.png\n")
}

# ============================================================================
# STAKEHOLDER DISTRIBUTION ANALYSIS
# ============================================================================

stakeholder_summary <- df %>%
  filter(!is.na(Final_Role_Category)) %>%
  count(Final_Role_Category) %>%
  mutate(percentage = n/sum(n) * 100) %>%
  arrange(desc(n))

cat("\n=== STAKEHOLDER DISTRIBUTION (Classified) ===\n")
print(stakeholder_summary)

write.csv(stakeholder_summary, "output/stakeholder_distribution.csv", row.names = FALSE)

# ============================================================================
# FIGURE 6: Financial vs. Impact Focus by Stakeholder Group
# ============================================================================

# Check for Q3.3 (7-point scale: 1=pure impact, 7=pure financial)
if("Q3.3" %in% names(df)) {
  impact_data <- df %>%
    filter(!is.na(Final_Role_Category), !is.na(Q3.3)) %>%
    mutate(Q3.3 = as.numeric(Q3.3)) %>%
    filter(!is.na(Q3.3)) %>%
    group_by(Final_Role_Category) %>%
    summarise(
      n = n(),
      mean_focus = mean(Q3.3, na.rm = TRUE),
      sd_focus = sd(Q3.3, na.rm = TRUE),
      se_focus = sd_focus / sqrt(n),
      ci_lower = mean_focus - 1.96 * se_focus,
      ci_upper = mean_focus + 1.96 * se_focus
    ) %>%
    filter(n >= 10)  # Only show groups with sufficient sample size
  
  cat("\n=== FINANCIAL vs IMPACT FOCUS BY STAKEHOLDER ===\n")
  print(impact_data)
  
  # Create plot
  fig6 <- ggplot(impact_data, aes(x = reorder(Final_Role_Category, mean_focus), 
                                  y = mean_focus)) +
    geom_point(size = 3, color = "#1976D2") +
    geom_errorbar(aes(ymin = ci_lower, ymax = ci_upper), 
                  width = 0.2, color = "#1976D2") +
    coord_flip() +
    scale_y_continuous(limits = c(1, 7), breaks = 1:7) +
    labs(title = "Figure 6: Financial vs. Impact Focus by Stakeholder Group",
         subtitle = "1 = Pure Impact Focus, 7 = Pure Financial Focus",
         x = "Stakeholder Group",
         y = "Mean Investment Focus (95% CI)") +
    theme(axis.text.y = element_text(size = 10))
  
  ggsave("figures/figure6_financial_impact_focus.png", 
         fig6, width = 10, height = 8, dpi = 300)
  cat("Figure 6 saved to figures/figure6_financial_impact_focus.png\n")
} else {
  cat("\nQ3.3 not found - cannot create Figure 6\n")
}

# ============================================================================
# BARRIER ANALYSIS
# ============================================================================

# Look for barrier-related columns (Q3.11 or similar)
barrier_cols <- grep("Q3.11|barrier|challenge", names(df), value = TRUE, ignore.case = TRUE)
cat("\n=== BARRIER-RELATED COLUMNS ===\n")
print(barrier_cols)

if(length(barrier_cols) > 0) {
  # Analyze first barrier column as example
  barrier_summary <- df %>%
    filter(!is.na(Final_Role_Category)) %>%
    group_by(Final_Role_Category) %>%
    summarise(
      n = n(),
      barrier_pct = sum(!is.na(!!sym(barrier_cols[1])) & !!sym(barrier_cols[1]) != "") / n * 100
    ) %>%
    filter(n >= 10)
  
  cat("\n=== BARRIER ANALYSIS (Example) ===\n")
  print(barrier_summary)
}

# ============================================================================
# SUMMARY STATISTICS
# ============================================================================

cat("\n=== OVERALL STATISTICS ===\n")
cat("Total responses:", nrow(df), "\n")
cat("Responses with stakeholder classification:", sum(!is.na(df$Final_Role_Category)), "\n")
cat("Responses with Q3.3 (financial/impact):", sum(!is.na(df$Q3.3)), "\n")

# Check what other key columns we have
cat("\n=== KEY COLUMNS AVAILABLE ===\n")
cat("Q2.1 (original role):", sum(!is.na(df$Q2.1)), "responses\n")
cat("Q2.2 (geography):", sum(!is.na(df$Q2.2)), "responses\n")
cat("Q3.3 (financial/impact):", sum(!is.na(df$Q3.3)), "responses\n")

# Save geographic distribution
if("Q2.2" %in% names(df)) {
  geo_summary <- df %>%
    filter(!is.na(Q2.2) & Q2.2 != "") %>%
    count(Q2.2) %>%
    mutate(percentage = n/sum(n) * 100) %>%
    arrange(desc(n))
  
  write.csv(geo_summary, "output/geographic_distribution.csv", row.names = FALSE)
}

cat("\n✓ Main analysis completed successfully\n")
cat("Check the 'figures/' directory for plots\n")
cat("Check the 'output/' directory for summary tables\n")