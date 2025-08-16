# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 102: "Corporate venture capital roles: investment focus (χ² = 22.15, p < .001)"
# Purpose: Analyze investment focus patterns in corporate venture capital roles

library(tidyverse)
library(janitor)
library(DescTools)
library(vcd)
library(epitools)
library(ca)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_CVC_ROLE <- "cvc_role_type"
COL_INVESTMENT_FOCUS <- "investment_focus_area"
COL_INVESTMENT_STAGE <- "preferred_investment_stage"
COL_STRATEGIC_ALIGNMENT <- "strategic_alignment_score"
COL_FINANCIAL_RETURNS <- "financial_return_importance"
COL_PORTFOLIO_SIZE <- "portfolio_company_count"
COL_INVESTMENT_SIZE <- "average_investment_size"
COL_BOARD_PARTICIPATION <- "board_seat_percentage"
COL_EXIT_STRATEGY <- "preferred_exit_strategy"
COL_INNOVATION_METRICS <- "innovation_impact_score"
COL_PARENT_INDUSTRY <- "parent_company_industry"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    cvc_role_type = factor(cvc_role_type),
    investment_focus_area = factor(investment_focus_area),
    preferred_investment_stage = factor(preferred_investment_stage,
                                       levels = c("Pre-seed", "Seed", "Series A", 
                                                "Series B", "Series C+"),
                                       ordered = TRUE),
    strategic_alignment_score = as.numeric(strategic_alignment_score),
    financial_return_importance = as.numeric(financial_return_importance),
    portfolio_company_count = as.numeric(portfolio_company_count),
    average_investment_size = as.numeric(average_investment_size),
    board_seat_percentage = as.numeric(board_seat_percentage),
    preferred_exit_strategy = factor(preferred_exit_strategy),
    innovation_impact_score = as.numeric(innovation_impact_score),
    parent_company_industry = factor(parent_company_industry),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(cvc_role_type), !is.na(investment_focus_area))

# Create contingency table
contingency_table <- table(df$cvc_role_type, df$investment_focus_area)

print("CVC Role × Investment Focus Contingency Table:")
print(contingency_table)

# Add margins
print("\nContingency table with margins:")
print(addmargins(contingency_table))

# Proportions by row (CVC role)
prop_by_role <- prop.table(contingency_table, 1)
print("\nProportions by CVC role:")
print(round(prop_by_role, 3))

# Proportions by column (investment focus)
prop_by_focus <- prop.table(contingency_table, 2)
print("\nProportions by investment focus:")
print(round(prop_by_focus, 3))

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

# Check adequacy of expected frequencies
min_expected <- min(chi_sq_test$expected)
cells_below_5 <- sum(chi_sq_test$expected < 5)
print(paste("\nMinimum expected frequency:", round(min_expected, 2)))
print(paste("Cells with expected frequency < 5:", cells_below_5, "out of", 
           length(chi_sq_test$expected)))

if(cells_below_5 > 0) {
  print("Warning: Some expected frequencies are below 5")
  print("Consider using Fisher's exact test or combining categories")
}

# Standardized residuals
std_residuals <- chi_sq_test$residuals
print("\nStandardized residuals (|z| > 2 indicates significant deviation):")
print(round(std_residuals, 2))

# Identify significant cells
significant_cells <- which(abs(std_residuals) > 2, arr.ind = TRUE)
if(nrow(significant_cells) > 0) {
  print("\nCells with significant residuals:")
  for(i in 1:nrow(significant_cells)) {
    row_idx <- significant_cells[i, 1]
    col_idx <- significant_cells[i, 2]
    role <- rownames(contingency_table)[row_idx]
    focus <- colnames(contingency_table)[col_idx]
    residual <- std_residuals[row_idx, col_idx]
    
    print(paste(role, "×", focus, ": z =", round(residual, 2),
               ifelse(residual > 0, "(over-represented)", "(under-represented)")))
  }
}

# Effect size measures
n_total <- sum(contingency_table)
min_dim <- min(nrow(contingency_table) - 1, ncol(contingency_table) - 1)

# Cramér's V
cramers_v <- sqrt(chi_sq_test$statistic / (n_total * min_dim))
print(paste("\nCramér's V:", round(cramers_v, 3)))

# Contingency coefficient
contingency_coef <- sqrt(chi_sq_test$statistic / (chi_sq_test$statistic + n_total))
print(paste("Contingency coefficient:", round(contingency_coef, 3)))

# Phi coefficient (if 2x2)
if(nrow(contingency_table) == 2 && ncol(contingency_table) == 2) {
  phi_coef <- sqrt(chi_sq_test$statistic / n_total)
  print(paste("Phi coefficient:", round(phi_coef, 3)))
}

# Fisher's exact test (if needed)
if(min_expected < 5 && prod(dim(contingency_table)) <= 10) {
  fisher_test <- fisher.test(contingency_table, simulate.p.value = TRUE, B = 10000)
  print(paste("\nFisher's exact test p-value:", 
             format(fisher_test$p.value, scientific = TRUE)))
}

# Correspondence analysis
ca_result <- ca(contingency_table)
print("\nCorrespondence Analysis:")
print(summary(ca_result))

# Dimension contributions
print("\nDimension contributions to inertia:")
inertia_proportions <- ca_result$sv^2 / sum(ca_result$sv^2)
print(round(inertia_proportions, 3))

# Investment stage analysis
if(!all(is.na(df$preferred_investment_stage))) {
  stage_role_table <- table(df$cvc_role_type, df$preferred_investment_stage)
  
  if(min(dim(stage_role_table)) > 1) {
    chi_stage <- chisq.test(stage_role_table)
    print("\nCVC Role × Investment Stage:")
    print(paste("χ² =", round(chi_stage$statistic, 2)))
    print(paste("p-value:", format(chi_stage$p.value, scientific = TRUE)))
    
    # Ordinal association
    gamma_stage <- GoodmanKruskalGamma(stage_role_table)
    print(paste("Goodman-Kruskal gamma:", round(gamma_stage, 3)))
  }
}

# Strategic alignment analysis
if(sum(!is.na(df$strategic_alignment_score)) > 50) {
  strategic_by_role <- df %>%
    filter(!is.na(strategic_alignment_score)) %>%
    group_by(cvc_role_type) %>%
    summarise(
      n = n(),
      mean_strategic = mean(strategic_alignment_score, na.rm = TRUE),
      sd_strategic = sd(strategic_alignment_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_strategic))
  
  print("\nStrategic alignment by CVC role:")
  print(strategic_by_role)
  
  # ANOVA
  if(length(unique(df$cvc_role_type)) > 2) {
    anova_strategic <- aov(strategic_alignment_score ~ cvc_role_type, data = df)
    print("\nANOVA - Strategic alignment by role:")
    print(summary(anova_strategic))
  }
}

# Financial returns importance
if(sum(!is.na(df$financial_return_importance)) > 50) {
  financial_by_focus <- df %>%
    filter(!is.na(financial_return_importance)) %>%
    group_by(investment_focus_area) %>%
    summarise(
      n = n(),
      mean_financial = mean(financial_return_importance, na.rm = TRUE),
      sd_financial = sd(financial_return_importance, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_financial))
  
  print("\nFinancial return importance by investment focus:")
  print(financial_by_focus)
}

# Portfolio size analysis
if(sum(!is.na(df$portfolio_company_count)) > 50) {
  portfolio_summary <- df %>%
    filter(!is.na(portfolio_company_count)) %>%
    group_by(cvc_role_type, investment_focus_area) %>%
    summarise(
      n = n(),
      mean_portfolio = mean(portfolio_company_count, na.rm = TRUE),
      median_portfolio = median(portfolio_company_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nPortfolio size by role and focus:")
  print(portfolio_summary)
}

# Investment size analysis
if(sum(!is.na(df$average_investment_size)) > 50) {
  # Log transform investment size
  df$log_investment <- log10(df$average_investment_size + 1)
  
  investment_by_role_focus <- df %>%
    filter(!is.na(log_investment)) %>%
    group_by(cvc_role_type, investment_focus_area) %>%
    summarise(
      n = n(),
      mean_log_investment = mean(log_investment, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(mean_investment_millions = 10^mean_log_investment / 1e6)
  
  print("\nAverage investment size by role and focus (millions):")
  print(investment_by_role_focus)
}

# Board participation analysis
if(sum(!is.na(df$board_seat_percentage)) > 50) {
  board_participation <- df %>%
    filter(!is.na(board_seat_percentage)) %>%
    group_by(cvc_role_type) %>%
    summarise(
      n = n(),
      mean_board = mean(board_seat_percentage, na.rm = TRUE),
      median_board = median(board_seat_percentage, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nBoard seat participation by CVC role:")
  print(board_participation)
}

# Exit strategy analysis
if(!all(is.na(df$preferred_exit_strategy))) {
  exit_role_table <- table(df$cvc_role_type, df$preferred_exit_strategy)
  
  if(min(dim(exit_role_table)) > 1) {
    chi_exit <- chisq.test(exit_role_table)
    print("\nCVC Role × Exit Strategy:")
    print(paste("χ² =", round(chi_exit$statistic, 2)))
    print(paste("p-value:", format(chi_exit$p.value, scientific = TRUE)))
  }
}

# Innovation impact analysis
if(sum(!is.na(df$innovation_impact_score)) > 50) {
  innovation_by_focus <- df %>%
    filter(!is.na(innovation_impact_score)) %>%
    group_by(investment_focus_area) %>%
    summarise(
      n = n(),
      mean_innovation = mean(innovation_impact_score, na.rm = TRUE),
      sd_innovation = sd(innovation_impact_score, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    arrange(desc(mean_innovation))
  
  print("\nInnovation impact by investment focus:")
  print(innovation_by_focus)
}

# Parent industry analysis
if(length(unique(df$parent_company_industry)) > 3) {
  # Most common investment focus by parent industry
  industry_focus <- df %>%
    count(parent_company_industry, investment_focus_area) %>%
    group_by(parent_company_industry) %>%
    slice_max(n, n = 1) %>%
    ungroup() %>%
    filter(n >= 10)
  
  print("\nMost common investment focus by parent industry:")
  print(industry_focus)
}

# Multinomial logistic regression
library(nnet)
if(length(unique(df$investment_focus_area)) > 2) {
  multinom_model <- multinom(investment_focus_area ~ cvc_role_type, data = df)
  
  print("\nMultinomial logistic regression:")
  print(summary(multinom_model))
  
  # Predicted probabilities for each role
  roles <- unique(df$cvc_role_type)
  for(role in roles[!is.na(roles)]) {
    pred_data <- data.frame(cvc_role_type = role)
    pred_probs <- predict(multinom_model, newdata = pred_data, type = "probs")
    print(paste("\nPredicted focus probabilities for", role, ":"))
    print(round(pred_probs, 3))
  }
}

# Log-linear model
library(MASS)
log_linear <- loglm(~ cvc_role_type + investment_focus_area + 
                   cvc_role_type:investment_focus_area, 
                   data = contingency_table)

print("\nLog-linear model:")
print(paste("Deviance:", round(log_linear$deviance, 2)))
print(paste("df:", log_linear$df))
print(paste("p-value:", format(1 - pchisq(log_linear$deviance, log_linear$df), 
                              scientific = TRUE)))

# Mosaic plot data preparation
print("\nData for mosaic plot visualization:")
print("Contingency table proportions:")
print(round(prop.table(contingency_table), 3))

# Bootstrap chi-square
set.seed(123)
n_boot <- 10000
boot_chi_sq <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_indices <- sample(nrow(df), replace = TRUE)
  boot_table <- table(df$cvc_role_type[boot_indices],
                     df$investment_focus_area[boot_indices])
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
print("Corporate venture capital roles showed significant association with investment focus")
print(paste("χ²(", chi_sq_test$parameter, ") =", round(chi_sq_test$statistic, 2),
           ", p <", ifelse(chi_sq_test$p.value < 0.001, ".001", 
                          round(chi_sq_test$p.value, 3))))
print(paste("Cramér's V =", round(cramers_v, 3)))
print(paste("N =", n_total))
print("\nKey associations identified:")
if(nrow(significant_cells) > 0) {
  print("Strongest over/under-representations based on standardized residuals")
}

# Expected: investment focus (χ² = 22.15, p < .001)