# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 80: "Asset-based valuation methods: book value 52%, liquidation value 31%, replacement cost 17%"
# Purpose: Analyze asset-based valuation method usage patterns with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_ASSET_BOOK_VALUE <- "asset_valuation_book_value"
COL_ASSET_LIQUIDATION <- "asset_valuation_liquidation"
COL_ASSET_REPLACEMENT <- "asset_valuation_replacement_cost"
COL_ASSET_FAIR_VALUE <- "asset_valuation_fair_value"
COL_PRIMARY_ASSET_METHOD <- "primary_asset_valuation_method"
COL_STAKEHOLDER <- "stakeholder"
COL_SECTOR_FOCUS <- "sector_focus"
COL_ASSET_INTENSITY <- "asset_intensity_level"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    asset_valuation_book_value = as.integer(asset_valuation_book_value),
    asset_valuation_liquidation = as.integer(asset_valuation_liquidation),
    asset_valuation_replacement_cost = as.integer(asset_valuation_replacement_cost),
    asset_valuation_fair_value = as.integer(asset_valuation_fair_value),
    primary_asset_valuation_method = factor(primary_asset_valuation_method),
    stakeholder = factor(stakeholder),
    sector_focus = factor(sector_focus),
    asset_intensity_level = factor(asset_intensity_level)
  )

# Calculate proportions for each asset-based method
# Book Value
n_book_valid <- sum(!is.na(df$asset_valuation_book_value))
n_book_used <- sum(df$asset_valuation_book_value, na.rm = TRUE)
prop_book <- n_book_used / n_book_valid
ci_book <- BinomCI(n_book_used, n_book_valid, conf.level = 0.95, method = "wilson")

print("Book Value Method:")
print(paste("Valid responses:", n_book_valid))
print(paste("Using Book Value:", n_book_used))
print(paste("Proportion:", round(prop_book * 100, 1), "%"))
print(paste("95% CI: [", round(ci_book[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_book[,"upr.ci"] * 100, 1), "%]", sep=""))

# Liquidation Value
n_liquidation_valid <- sum(!is.na(df$asset_valuation_liquidation))
n_liquidation_used <- sum(df$asset_valuation_liquidation, na.rm = TRUE)
prop_liquidation <- n_liquidation_used / n_liquidation_valid
ci_liquidation <- BinomCI(n_liquidation_used, n_liquidation_valid, 
                          conf.level = 0.95, method = "wilson")

print("\nLiquidation Value Method:")
print(paste("Valid responses:", n_liquidation_valid))
print(paste("Using Liquidation Value:", n_liquidation_used))
print(paste("Proportion:", round(prop_liquidation * 100, 1), "%"))
print(paste("95% CI: [", round(ci_liquidation[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_liquidation[,"upr.ci"] * 100, 1), "%]", sep=""))

# Replacement Cost
n_replacement_valid <- sum(!is.na(df$asset_valuation_replacement_cost))
n_replacement_used <- sum(df$asset_valuation_replacement_cost, na.rm = TRUE)
prop_replacement <- n_replacement_used / n_replacement_valid
ci_replacement <- BinomCI(n_replacement_used, n_replacement_valid, 
                         conf.level = 0.95, method = "wilson")

print("\nReplacement Cost Method:")
print(paste("Valid responses:", n_replacement_valid))
print(paste("Using Replacement Cost:", n_replacement_used))
print(paste("Proportion:", round(prop_replacement * 100, 1), "%"))
print(paste("95% CI: [", round(ci_replacement[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_replacement[,"upr.ci"] * 100, 1), "%]", sep=""))

# Fair Value (if available)
if(!all(is.na(df$asset_valuation_fair_value))) {
  n_fair_valid <- sum(!is.na(df$asset_valuation_fair_value))
  n_fair_used <- sum(df$asset_valuation_fair_value, na.rm = TRUE)
  prop_fair <- n_fair_used / n_fair_valid
  ci_fair <- BinomCI(n_fair_used, n_fair_valid, conf.level = 0.95, method = "wilson")
  
  print("\nFair Value Method:")
  print(paste("Using Fair Value:", round(prop_fair * 100, 1), "%"))
  print(paste("95% CI: [", round(ci_fair[,"lwr.ci"] * 100, 1), "%, ", 
              round(ci_fair[,"upr.ci"] * 100, 1), "%]", sep=""))
}

# Summary comparison
asset_summary <- data.frame(
  Method = c("Book Value", "Liquidation Value", "Replacement Cost"),
  n_valid = c(n_book_valid, n_liquidation_valid, n_replacement_valid),
  n_using = c(n_book_used, n_liquidation_used, n_replacement_used),
  Proportion = c(prop_book, prop_liquidation, prop_replacement) * 100,
  CI_lower = c(ci_book[,"lwr.ci"], ci_liquidation[,"lwr.ci"], 
               ci_replacement[,"lwr.ci"]) * 100,
  CI_upper = c(ci_book[,"upr.ci"], ci_liquidation[,"upr.ci"], 
               ci_replacement[,"upr.ci"]) * 100
) %>%
  mutate(
    Proportion = round(Proportion, 1),
    CI_lower = round(CI_lower, 1),
    CI_upper = round(CI_upper, 1),
    CI_text = paste("[", CI_lower, "%, ", CI_upper, "%]", sep="")
  ) %>%
  arrange(desc(Proportion))

print("\nAsset-based valuation method summary:")
print(asset_summary %>% select(Method, n_using, Proportion, CI_text))

# Multiple method usage analysis
df_asset_methods <- df %>%
  filter(!is.na(asset_valuation_book_value), 
         !is.na(asset_valuation_liquidation),
         !is.na(asset_valuation_replacement_cost)) %>%
  mutate(
    total_methods = asset_valuation_book_value + 
                   asset_valuation_liquidation + 
                   asset_valuation_replacement_cost
  )

method_count_dist <- df_asset_methods %>%
  group_by(total_methods) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df_asset_methods) * 100, 1),
    .groups = "drop"
  )

print("\nNumber of asset methods used concurrently:")
print(method_count_dist)

# Method combinations
method_combinations <- df_asset_methods %>%
  mutate(
    combination = case_when(
      total_methods == 0 ~ "None",
      asset_valuation_book_value == 1 & total_methods == 1 ~ "Book Value only",
      asset_valuation_liquidation == 1 & total_methods == 1 ~ "Liquidation only",
      asset_valuation_replacement_cost == 1 & total_methods == 1 ~ "Replacement only",
      asset_valuation_book_value == 1 & asset_valuation_liquidation == 1 & 
        total_methods == 2 ~ "Book + Liquidation",
      asset_valuation_book_value == 1 & asset_valuation_replacement_cost == 1 & 
        total_methods == 2 ~ "Book + Replacement",
      asset_valuation_liquidation == 1 & asset_valuation_replacement_cost == 1 & 
        total_methods == 2 ~ "Liquidation + Replacement",
      total_methods == 3 ~ "All three methods",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(combination) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df_asset_methods) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print("\nAsset method combination patterns:")
print(method_combinations)

# Primary asset valuation method
if(!all(is.na(df$primary_asset_valuation_method))) {
  primary_asset_dist <- df %>%
    filter(!is.na(primary_asset_valuation_method)) %>%
    group_by(primary_asset_valuation_method) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$primary_asset_valuation_method)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("\nPrimary asset valuation method preferences:")
  print(primary_asset_dist)
  
  # Calculate Wilson CIs for primary methods
  primary_ci <- primary_asset_dist %>%
    rowwise() %>%
    mutate(
      ci_data = list(BinomCI(n, sum(primary_asset_dist$n), 
                            conf.level = 0.95, method = "wilson")),
      ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
      ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
      ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
    ) %>%
    select(-ci_data)
  
  print("\nPrimary methods with 95% Wilson CIs:")
  print(primary_ci %>% select(primary_asset_valuation_method, n, percentage, ci_text))
}

# Analysis by stakeholder
stakeholder_asset <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    book_pct = round(mean(asset_valuation_book_value, na.rm = TRUE) * 100, 1),
    liquidation_pct = round(mean(asset_valuation_liquidation, na.rm = TRUE) * 100, 1),
    replacement_pct = round(mean(asset_valuation_replacement_cost, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(stakeholder)

print("\nAsset methods by stakeholder (n ≥ 20):")
print(stakeholder_asset)

# Chi-square tests for stakeholder differences
for(method in c("book_value", "liquidation", "replacement_cost")) {
  col_name <- paste0("asset_valuation_", method)
  
  if(col_name %in% names(df)) {
    method_table <- table(df$stakeholder[!is.na(df[[col_name]])],
                         df[[col_name]][!is.na(df[[col_name]])])
    
    if(min(dim(method_table)) > 1) {
      chi_test <- chisq.test(method_table)
      print(paste("\nChi-square test for", method, "by stakeholder:"))
      print(paste("χ² =", round(chi_test$statistic, 2),
                  ", p =", format(chi_test$p.value, scientific = TRUE)))
    }
  }
}

# Asset intensity analysis
if(!all(is.na(df$asset_intensity_level))) {
  intensity_asset <- df %>%
    filter(!is.na(asset_intensity_level)) %>%
    group_by(asset_intensity_level) %>%
    summarise(
      n = n(),
      book_pct = round(mean(asset_valuation_book_value, na.rm = TRUE) * 100, 1),
      liquidation_pct = round(mean(asset_valuation_liquidation, na.rm = TRUE) * 100, 1),
      replacement_pct = round(mean(asset_valuation_replacement_cost, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(asset_intensity_level)
  
  print("\nAsset methods by asset intensity level (n ≥ 15):")
  print(intensity_asset)
  
  # Test for trends
  if(nrow(intensity_asset) > 2) {
    df_intensity <- df %>%
      filter(!is.na(asset_intensity_level)) %>%
      mutate(intensity_numeric = as.numeric(factor(asset_intensity_level)))
    
    # Book value vs intensity
    cor_book_intensity <- cor.test(df_intensity$asset_valuation_book_value[!is.na(df_intensity$asset_valuation_book_value)],
                                   df_intensity$intensity_numeric[!is.na(df_intensity$asset_valuation_book_value)],
                                   method = "spearman")
    print("\nCorrelations with asset intensity (Spearman):")
    print(paste("Book Value × Intensity: ρ =", round(cor_book_intensity$estimate, 3),
                ", p =", format(cor_book_intensity$p.value, scientific = TRUE)))
    
    # Liquidation vs intensity
    cor_liq_intensity <- cor.test(df_intensity$asset_valuation_liquidation[!is.na(df_intensity$asset_valuation_liquidation)],
                                  df_intensity$intensity_numeric[!is.na(df_intensity$asset_valuation_liquidation)],
                                  method = "spearman")
    print(paste("Liquidation × Intensity: ρ =", round(cor_liq_intensity$estimate, 3),
                ", p =", format(cor_liq_intensity$p.value, scientific = TRUE)))
    
    # Replacement vs intensity
    cor_rep_intensity <- cor.test(df_intensity$asset_valuation_replacement_cost[!is.na(df_intensity$asset_valuation_replacement_cost)],
                                  df_intensity$intensity_numeric[!is.na(df_intensity$asset_valuation_replacement_cost)],
                                  method = "spearman")
    print(paste("Replacement × Intensity: ρ =", round(cor_rep_intensity$estimate, 3),
                ", p =", format(cor_rep_intensity$p.value, scientific = TRUE)))
  }
}

# Sector focus analysis
if(!all(is.na(df$sector_focus))) {
  sector_asset <- df %>%
    filter(!is.na(sector_focus)) %>%
    group_by(sector_focus) %>%
    summarise(
      n = n(),
      book_pct = round(mean(asset_valuation_book_value, na.rm = TRUE) * 100, 1),
      liquidation_pct = round(mean(asset_valuation_liquidation, na.rm = TRUE) * 100, 1),
      replacement_pct = round(mean(asset_valuation_replacement_cost, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(book_pct)) %>%
    head(10)
  
  print("\nTop 10 sectors by book value usage (n ≥ 15):")
  print(sector_asset)
}

# Method correlations
if(nrow(df_asset_methods) > 50) {
  method_cors <- cor(df_asset_methods[, c("asset_valuation_book_value", 
                                          "asset_valuation_liquidation",
                                          "asset_valuation_replacement_cost")],
                    use = "complete.obs")
  
  print("\nAsset method usage correlations:")
  print(round(method_cors, 3))
  
  # Test significance
  print("\nCorrelation significance tests:")
  
  # Book vs Liquidation
  cor_book_liq <- cor.test(df$asset_valuation_book_value, df$asset_valuation_liquidation)
  print(paste("Book × Liquidation: r =", round(cor_book_liq$estimate, 3),
              ", p =", format(cor_book_liq$p.value, scientific = TRUE)))
  
  # Book vs Replacement
  cor_book_rep <- cor.test(df$asset_valuation_book_value, df$asset_valuation_replacement_cost)
  print(paste("Book × Replacement: r =", round(cor_book_rep$estimate, 3),
              ", p =", format(cor_book_rep$p.value, scientific = TRUE)))
  
  # Liquidation vs Replacement
  cor_liq_rep <- cor.test(df$asset_valuation_liquidation, df$asset_valuation_replacement_cost)
  print(paste("Liquidation × Replacement: r =", round(cor_liq_rep$estimate, 3),
              ", p =", format(cor_liq_rep$p.value, scientific = TRUE)))
}

# McNemar's tests for paired comparisons
# Book vs Liquidation
if(sum(!is.na(df$asset_valuation_book_value) & !is.na(df$asset_valuation_liquidation)) > 50) {
  mcnemar_table_1 <- table(df$asset_valuation_book_value, df$asset_valuation_liquidation)
  mcnemar_test_1 <- mcnemar.test(mcnemar_table_1)
  
  print("\nMcNemar's test - Book Value vs Liquidation:")
  print(paste("χ² =", round(mcnemar_test_1$statistic, 2)))
  print(paste("p-value:", format(mcnemar_test_1$p.value, scientific = TRUE)))
}

# Book vs Replacement
if(sum(!is.na(df$asset_valuation_book_value) & !is.na(df$asset_valuation_replacement_cost)) > 50) {
  mcnemar_table_2 <- table(df$asset_valuation_book_value, df$asset_valuation_replacement_cost)
  mcnemar_test_2 <- mcnemar.test(mcnemar_table_2)
  
  print("\nMcNemar's test - Book Value vs Replacement Cost:")
  print(paste("χ² =", round(mcnemar_test_2$statistic, 2)))
  print(paste("p-value:", format(mcnemar_test_2$p.value, scientific = TRUE)))
}

# Liquidation vs Replacement
if(sum(!is.na(df$asset_valuation_liquidation) & !is.na(df$asset_valuation_replacement_cost)) > 50) {
  mcnemar_table_3 <- table(df$asset_valuation_liquidation, df$asset_valuation_replacement_cost)
  mcnemar_test_3 <- mcnemar.test(mcnemar_table_3)
  
  print("\nMcNemar's test - Liquidation vs Replacement Cost:")
  print(paste("χ² =", round(mcnemar_test_3$statistic, 2)))
  print(paste("p-value:", format(mcnemar_test_3$p.value, scientific = TRUE)))
}

# Logistic regression models
if(nrow(df) > 150) {
  # Book value model
  df_book_logit <- df %>%
    filter(!is.na(asset_valuation_book_value), !is.na(stakeholder))
  
  if(nrow(df_book_logit) > 100) {
    predictors <- c("stakeholder")
    
    if(sum(!is.na(df_book_logit$asset_intensity_level)) > 80) {
      predictors <- c(predictors, "asset_intensity_level")
    }
    
    formula_str <- paste("asset_valuation_book_value ~", paste(predictors, collapse = " + "))
    book_model <- glm(as.formula(formula_str), data = df_book_logit, family = binomial)
    
    print("\nLogistic regression - Book Value usage predictors:")
    print(summary(book_model))
    
    # Odds ratios
    odds_ratios <- exp(coef(book_model))
    print("Odds ratios:")
    print(round(odds_ratios, 3))
  }
  
  # Liquidation value model
  df_liq_logit <- df %>%
    filter(!is.na(asset_valuation_liquidation), !is.na(stakeholder))
  
  if(nrow(df_liq_logit) > 100) {
    liq_model <- glm(asset_valuation_liquidation ~ stakeholder, 
                    data = df_liq_logit, family = binomial)
    
    print("\nLogistic regression - Liquidation Value usage predictors:")
    print(summary(liq_model))
  }
}

# Contextual usage analysis
context_analysis <- df %>%
  mutate(
    distressed_context = asset_valuation_liquidation == 1,
    stable_context = asset_valuation_book_value == 1,
    growth_context = asset_valuation_replacement_cost == 1
  ) %>%
  summarise(
    n = n(),
    distressed_pct = round(mean(distressed_context, na.rm = TRUE) * 100, 1),
    stable_pct = round(mean(stable_context, na.rm = TRUE) * 100, 1),
    growth_pct = round(mean(growth_context, na.rm = TRUE) * 100, 1)
  )

print("\nContextual usage interpretation:")
print(context_analysis)

# Time trend analysis (if date data available)
if("response_date" %in% names(df) && sum(!is.na(df$response_date)) > 100) {
  df_time <- df %>%
    filter(!is.na(response_date)) %>%
    mutate(
      response_quarter = paste0(year(as.Date(response_date)), "-Q",
                               quarter(as.Date(response_date)))
    ) %>%
    group_by(response_quarter) %>%
    summarise(
      n = n(),
      book_pct = round(mean(asset_valuation_book_value, na.rm = TRUE) * 100, 1),
      liquidation_pct = round(mean(asset_valuation_liquidation, na.rm = TRUE) * 100, 1),
      replacement_pct = round(mean(asset_valuation_replacement_cost, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nAsset method trends over time:")
    print(df_time)
    
    # Trend test for book value
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$book_pct, 
                          method = "spearman")
    print("Book value temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of asset-based valuation methods:")
print(paste("Book Value:", round(prop_book * 100, 1), "% (95% CI: [", 
            round(ci_book[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_book[,"upr.ci"] * 100, 1), "%])"))
print(paste("Liquidation Value:", round(prop_liquidation * 100, 1), "% (95% CI: [", 
            round(ci_liquidation[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_liquidation[,"upr.ci"] * 100, 1), "%])"))
print(paste("Replacement Cost:", round(prop_replacement * 100, 1), "% (95% CI: [", 
            round(ci_replacement[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_replacement[,"upr.ci"] * 100, 1), "%])"))

# Expected: book value 52%, liquidation value 31%, replacement cost 17%