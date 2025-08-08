# 03_main_analysis.R
# Purpose: Main analysis & figures from the FINAL N=1,307 dataset
# Input : data/climate_finance_survey_final_1307.csv
# Output: figures/ + output/ tables

if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages({
  library(tidyverse)
  library(ggplot2)
  library(scales)
})

dir.create("figures", recursive = TRUE, showWarnings = FALSE)
dir.create("output",  recursive = TRUE, showWarnings = FALSE)

theme_set(theme_minimal(base_size = 12))

climate_colors <- c("#2E7D32", "#1976D2", "#F57C00", "#7B1FA2", "#C62828",
                    "#00796B", "#5D4037", "#455A64")

# ---- Load final dataset -------------------------------------------------------
infile <- "data/climate_finance_survey_final_1307.csv"
stopifnot(file.exists(infile))
df <- read.csv(infile, stringsAsFactors = FALSE)

cat("Loaded final dataset: ", nrow(df), " rows, ", ncol(df), " columns\n", sep = "")

# ---- Figure 4: Geographic Distribution (pie) ---------------------------------
if ("Q2.2" %in% names(df)) {
  geo_data <- df %>%
    filter(!is.na(Q2.2) & Q2.2 != "") %>%
    count(Q2.2, name = "n") %>%
    mutate(percentage = n / sum(n)) %>%
    arrange(desc(n))

  p4 <- ggplot(geo_data, aes(x = "", y = percentage, fill = Q2.2)) +
    geom_col(width = 1) +
    coord_polar(theta = "y") +
    scale_y_continuous(labels = percent) +
    scale_fill_manual(values = rep(climate_colors, length.out = nrow(geo_data))) +
    labs(title = "Figure 4. Geographic Distribution of Respondents",
         y = NULL, x = NULL, fill = "Region") +
    theme(axis.text = element_blank(),
          axis.ticks = element_blank(),
          panel.grid = element_blank())

  ggsave("figures/figure4_geographic_distribution.png", p4, width = 7, height = 5, dpi = 300)
  write.csv(geo_data, "output/figure4_geographic_distribution.csv", row.names = FALSE)
  cat("✓ Figure 4 saved\n")
} else {
  cat("! Q2.2 not found — skipping Figure 4\n")
}

# ---- Figure 6: Financial vs Impact Focus by Stakeholder ----------------------
# Q3.3: 1=pure impact ... 7=pure financial
if (all(c("Q3.3","Final_Role_Category") %in% names(df))) {
  df$Q3.3_num <- suppressWarnings(as.numeric(df$Q3.3))
  fig6 <- df %>%
    filter(!is.na(Q3.3_num), !is.na(Final_Role_Category)) %>%
    group_by(Final_Role_Category) %>%
    summarize(mean_focus = mean(Q3.3_num, na.rm = TRUE),
              n = n()) %>%
    arrange(desc(mean_focus))

  p6 <- ggplot(fig6, aes(x = reorder(Final_Role_Category, mean_focus), y = mean_focus)) +
    geom_col() +
    coord_flip() +
    labs(title = "Figure 6. Financial vs Impact Focus by Stakeholder",
         x = "Stakeholder Category", y = "Mean Orientation (1=Impact … 7=Financial)")

  ggsave("figures/figure6_financial_impact_focus.png", p6, width = 7, height = 6, dpi = 300)
  write.csv(fig6, "output/figure6_financial_impact_focus.csv", row.names = FALSE)
  cat("✓ Figure 6 saved\n")
} else {
  cat("! Q3.3 or Final_Role_Category missing — skipping Figure 6\n")
}

# ---- Table 1: Technology Risk Perceptions -----------------------------------
risk_cols <- paste0("Q3.6_", 1:5)
present <- risk_cols[risk_cols %in% names(df)]
if (length(present) > 0) {
  tab1 <- df %>%
    mutate(across(all_of(present), ~ suppressWarnings(as.numeric(.)))) %>%
    summarize(across(all_of(present),
                     list(mean = ~mean(., na.rm = TRUE),
                          sd   = ~sd(.,   na.rm = TRUE),
                          n    = ~sum(!is.na(.))))) %>%
    pivot_longer(everything(),
                 names_to = c("question","stat"),
                 names_pattern = "(Q3\\.6_\\d)_(mean|sd|n)",
                 values_to = "value") %>%
    pivot_wider(names_from = stat, values_from = value)

  write.csv(tab1, "output/table1_technology_risk_perceptions.csv", row.names = FALSE)
  cat("✓ Table 1 saved\n")
} else {
  cat("! Q3.6_1..5 not present — skipping Table 1\n")
}

cat("\nAnalysis complete. Artifacts in figures/ and output/\n")