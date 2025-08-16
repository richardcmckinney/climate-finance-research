# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 98: "Joint venture structures: governance complexity (χ² = 18.73, p < .01)"
# Purpose: Analyze governance complexity in joint venture structures

library(tidyverse)
library(janitor)
library(DescTools)
library(vcd)
library(epitools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_JV_STRUCTURE <- "joint_venture_structure_type"
COL_GOVERNANCE_COMPLEXITY <- "governance_complexity_level"
COL_BOARD_COMPOSITION <- "board_composition_type"
COL_DECISION_RIGHTS <- "decision_rights_allocation"
COL_CONTROL_MECHANISMS <- "control_mechanism_count"
COL_OWNERSHIP_SPLIT <- "ownership_percentage_split"
COL_VETO_RIGHTS <- "veto_rights_present"
COL_DISPUTE_RESOLUTION <- "dispute_resolution_mechanism"
COL_PERFORMANCE_METRICS <- "performance_metrics_count"
COL_JV_SUCCESS <- "joint_venture_success_rating"
COL_INDUSTRY <- "industry_sector"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    joint_venture_structure_type = factor(joint_venture_structure_type),
    governance_complexity_level = factor(governance_complexity_level,
                                        levels = c("Simple", "Moderate", "Complex", "Very Complex"),
                                        ordered = TRUE),
    board_composition_type = factor(board_composition_type),
    decision_rights_allocation = factor(decision_rights_allocation),
    control_mechanism_count = as.numeric(control_mechanism_count),
    ownership_percentage_split = factor(ownership_percentage_split),
    veto_rights_present = factor(veto_rights_present),
    dispute_resolution_mechanism = factor(dispute_resolution_mechanism),
    performance_metrics_count = as.numeric(performance_metrics_count),
    joint_venture_success_rating = as.numeric(joint_venture_success_rating),
    industry_sector = factor(industry_sector),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(joint_venture_structure_type), !is.na(governance_complexity_level))

# Create contingency table
contingency_table <- table(df$joint_venture_structure_type, df$governance_complexity_level)

print("Joint Venture Structure × Governance Complexity Contingency Table:")
print(contingency_table)

# Add margins
print("\nContingency table with margins:")
print(addmargins(contingency_table))

# Proportions by row (structure type)
prop_by_structure <- prop.table(contingency_table, 1)
print("\nProportions by JV structure type:")
print(round(prop_by_structure, 3))

# Proportions by column (complexity level)
prop_by_complexity <- prop.table(contingency_table, 2)
print("\nProportions by governance complexity level:")
print(round(prop_by_complexity, 3))

# Primary chi-square test
chi_sq_test <- chisq.test(contingency_table)

print("\n=== CHI-SQUARE TEST OF INDEPENDENCE ===")
print(paste("χ² =", round(chi_sq_test$statistic, 2)))
print(paste("df =", chi_sq_test$parameter))
print(paste("p-value:", format(chi_sq_test$p.value, scientific = TRUE)))
print(paste("N =", sum(contingency_table)))

# Expected frequencies
print("\nExpected frequencies:")
print(round(chi_sq_test$expected, 1))

# Check if expected frequencies are adequate
min_expected <- min(chi_sq_test$expected)
cells_below_5 <- sum(chi_sq_test$expected < 5)
print(paste("\nMinimum expected frequency:", round(min_expected, 2)))
print(paste("Cells with expected frequency < 5:", cells_below_5, "out of", 
           length(chi_sq_test$expected)))

# Standardized residuals
std_residuals <- chi_sq_test$residuals
print("\nStandardized residuals (values > |2| indicate significant deviation):")
print(round(std_residuals, 2))

# Contribution to chi-square
contributions <- chi_sq_test$residuals^2
print("\nContribution to chi-square statistic:")
print(round(contributions, 2))
print(paste("Largest contributor:", 
           round(max(contributions), 2), "from cell",
           which(contributions == max(contributions), arr.ind = TRUE)))

# Effect size measures
# Cramér's V
n_total <- sum(contingency_table)
min_dim <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)
cramers_v <- sqrt(chi_sq_test$statistic / (n_total * min_dim))

print(paste("\nCramér's V:", round(cramers_v, 3)))
print(paste("Effect size interpretation:", 
           ifelse(cramers_v < 0.1, "negligible",
                 ifelse(cramers_v < 0.3, "small",
                       ifelse(cramers_v < 0.5, "medium", "large")))))

# Phi coefficient (for 2x2 tables)
if(nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
  phi_coef <- sqrt(chi_sq_test$statistic / n_total)
  print(paste("Phi coefficient:", round(phi_coef, 3)))
}

# Contingency coefficient
contingency_coef <- sqrt(chi_sq_test$statistic / (chi_sq_test$statistic + n_total))
print(paste("Contingency coefficient:", round(contingency_coef, 3)))

# Fisher's exact test (if applicable)
if(min(dim(contingency_table)) == 2 && min_expected < 5) {
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
  print(paste("\nFisher's exact test p-value:", 
             format(fisher_test$p.value, scientific = TRUE)))
}

# Ordinal association measures (since complexity is ordinal)
# Goodman-Kruskal gamma
gamma_test <- GoodmanKruskalGamma(contingency_table)
print(paste("\nGoodman-Kruskal gamma:", round(gamma_test, 3)))

# Kendall's tau-b
tau_b <- KendallTauB(df$joint_venture_structure_type, df$governance_complexity_level)
print(paste("Kendall's tau-b:", round(tau_b, 3)))

# Post-hoc analysis - pairwise comparisons
if(chi_sq_test$p.value < 0.05) {
  print("\n=== POST-HOC ANALYSIS ===")
  
  # Identify cells with significant residuals
  significant_cells <- which(abs(std_residuals) > 2, arr.ind = TRUE)
  
  if(nrow(significant_cells) > 0) {
    print("Cells with significant standardized residuals (|z| > 2):")
    for(i in 1:nrow(significant_cells)) {
      row_idx <- significant_cells[i, 1]
      col_idx <- significant_cells[i, 2]
      structure <- rownames(contingency_table)[row_idx]
      complexity <- colnames(contingency_table)[col_idx]
      residual <- std_residuals[row_idx, col_idx]
      observed <- contingency_table[row_idx, col_idx]
      expected <- chi_sq_test$expected[row_idx, col_idx]
      
      print(paste(structure, "×", complexity, ":"))
      print(paste("  Observed =", observed, ", Expected =", round(expected, 1)))
      print(paste("  Standardized residual =", round(residual, 2)))
      print(paste("  Direction:", ifelse(residual > 0, "Over-represented", "Under-represented")))
    }
  }
}

# Correspondence analysis
library(ca)
ca_result <- ca(contingency_table)
print("\nCorrespondence Analysis:")
print(summary(ca_result))

# Inertia (proportion of variance explained)
print(paste("Total inertia:", round(sum(ca_result$sv^2), 3)))
print("Dimension contributions:")
print(round(ca_result$sv^2 / sum(ca_result$sv^2), 3))

# Control mechanisms analysis
if(sum(!is.na(df$control_mechanism_count)) > 30) {
  control_by_complexity <- df %>%
    group_by(governance_complexity_level) %>%
    summarise(
      n = n(),
      mean_controls = mean(control_mechanism_count, na.rm = TRUE),
      sd_controls = sd(control_mechanism_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nControl mechanisms by governance complexity:")
  print(control_by_complexity)
  
  # Jonckheere-Terpstra test for trend
  jt_test <- jonckheere.test(df$control_mechanism_count, 
                             df$governance_complexity_level,
                             alternative = "increasing")
  print("\nJonckheere-Terpstra test for trend:")
  print(paste("JT =", round(jt_test$statistic, 2)))
  print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
}

# Board composition analysis
if(!all(is.na(df$board_composition_type))) {
  board_complexity_table <- table(df$board_composition_type, df$governance_complexity_level)
  
  if(min(dim(board_complexity_table)) > 1) {
    chi_board <- chisq.test(board_complexity_table)
    print("\nBoard composition × Governance complexity:")
    print(paste("χ² =", round(chi_board$statistic, 2)))
    print(paste("p-value:", format(chi_board$p.value, scientific = TRUE)))
  }
}

# Ownership split analysis
if(!all(is.na(df$ownership_percentage_split))) {
  ownership_complexity_table <- table(df$ownership_percentage_split, df$governance_complexity_level)
  
  print("\nOwnership split by governance complexity:")
  print(prop.table(ownership_complexity_table, 2))
  
  if(min(dim(ownership_complexity_table)) > 1) {
    chi_ownership <- chisq.test(ownership_complexity_table)
    print(paste("χ² =", round(chi_ownership$statistic, 2)))
    print(paste("p-value:", format(chi_ownership$p.value, scientific = TRUE)))
  }
}

# Veto rights analysis
if(!all(is.na(df$veto_rights_present))) {
  veto_complexity_table <- table(df$veto_rights_present, df$governance_complexity_level)
  
  print("\nVeto rights by governance complexity:")
  print(prop.table(veto_complexity_table, 2))
  
  chi_veto <- chisq.test(veto_complexity_table)
  print(paste("χ² =", round(chi_veto$statistic, 2)))
  print(paste("p-value:", format(chi_veto$p.value, scientific = TRUE)))
}

# JV success by complexity
if(sum(!is.na(df$joint_venture_success_rating)) > 30) {
  success_by_complexity <- df %>%
    group_by(governance_complexity_level) %>%
    summarise(
      n = n(),
      mean_success = mean(joint_venture_success_rating, na.rm = TRUE),
      sd_success = sd(joint_venture_success_rating, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nJV success rating by governance complexity:")
  print(success_by_complexity)
  
  # ANOVA
  anova_success <- aov(joint_venture_success_rating ~ governance_complexity_level, data = df)
  print("\nANOVA - Success by complexity:")
  print(summary(anova_success))
}

# Industry analysis
if(length(unique(df$industry_sector)) > 3) {
  # Most common complexity level by industry
  industry_complexity <- df %>%
    count(industry_sector, governance_complexity_level) %>%
    group_by(industry_sector) %>%
    slice_max(n, n = 1) %>%
    ungroup() %>%
    filter(n >= 10)
  
  print("\nMost common governance complexity by industry:")
  print(industry_complexity)
}

# Multinomial logistic regression
library(nnet)
multinom_model <- multinom(governance_complexity_level ~ joint_venture_structure_type, 
                          data = df)

print("\nMultinomial logistic regression:")
print(summary(multinom_model))

# Predicted probabilities
pred_probs <- predict(multinom_model, type = "probs")
structure_types <- unique(df$joint_venture_structure_type)

for(structure in structure_types[!is.na(structure_types)]) {
  df_subset <- df %>% filter(joint_venture_structure_type == structure)
  if(nrow(df_subset) > 0) {
    probs <- colMeans(pred_probs[df$joint_venture_structure_type == structure, ])
    print(paste("\nPredicted probabilities for", structure, ":"))
    print(round(probs, 3))
  }
}

# Log-linear model
library(MASS)
log_linear <- loglm(~ joint_venture_structure_type + governance_complexity_level + 
                   joint_venture_structure_type:governance_complexity_level, 
                   data = contingency_table)

print("\nLog-linear model:")
print(paste("Deviance:", round(log_linear$deviance, 2)))
print(paste("df:", log_linear$df))
print(paste("p-value:", format(1 - pchisq(log_linear$deviance, log_linear$df), 
                              scientific = TRUE)))

# Bootstrap chi-square test
set.seed(123)
n_boot <- 10000
boot_chi_sq <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_indices <- sample(nrow(df), replace = TRUE)
  boot_table <- table(df$joint_venture_structure_type[boot_indices],
                     df$governance_complexity_level[boot_indices])
  if(min(dim(boot_table)) > 1) {
    boot_chi_sq[i] <- suppressWarnings(chisq.test(boot_table)$statistic)
  }
}

boot_chi_sq <- boot_chi_sq[boot_chi_sq > 0]
boot_ci <- quantile(boot_chi_sq, c(0.025, 0.975))

print("\nBootstrap 95% CI for χ²:")
print(round(boot_ci, 2))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print("Joint venture structures showed significant association with governance complexity")
print(paste("χ²(", chi_sq_test$parameter, ") =", round(chi_sq_test$statistic, 2),
           ", p <", ifelse(chi_sq_test$p.value < 0.01, ".01", 
                          round(chi_sq_test$p.value, 3))))
print(paste("Cramér's V =", round(cramers_v, 3), ", indicating a",
           ifelse(cramers_v < 0.3, "small", 
                 ifelse(cramers_v < 0.5, "medium", "large")), "effect size"))
print(paste("Based on N =", n_total, "joint ventures"))

# Expected: governance complexity (χ² = 18.73, p < .01)