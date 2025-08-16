# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 76: "Valuation methodology preferences: DCF 68%, comparables 55%, asset-based 27%"
# Purpose: Analyze valuation methodology usage patterns with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_VALUATION_DCF <- "valuation_method_dcf"
COL_VALUATION_COMPARABLES <- "valuation_method_comparables"
COL_VALUATION_ASSET_BASED <- "valuation_method_asset_based"
COL_VALUATION_PRIMARY <- "primary_valuation_method"
COL_STAKEHOLDER <- "stakeholder"
COL_INVESTMENT_STAGE_FOCUS <- "investment_stage_focus"
COL_SECTOR_FOCUS <- "sector_focus"
COL_VALUATION_CONFIDENCE <- "valuation_confidence_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    valuation_method_dcf = as.integer(valuation_method_dcf),
    valuation_method_comparables = as.integer(valuation_method_comparables),
    valuation_method_asset_based = as.integer(valuation_method_asset_based),
    primary_valuation_method = factor(primary_valuation_method),
    stakeholder = factor(stakeholder),
    investment_stage_focus = factor(investment_stage_focus),
    sector_focus = factor(sector_focus),
    valuation_confidence_score = as.numeric(valuation_confidence_score)
  )

# Calculate proportions for each method
# DCF Method
n_dcf_valid <- sum(!is.na(df$valuation_method_dcf))
n_dcf_used <- sum(df$valuation_method_dcf, na.rm = TRUE)
prop_dcf <- n_dcf_used / n_dcf_valid
ci_dcf <- BinomCI(n_dcf_used, n_dcf_valid, conf.level = 0.95, method = "wilson")

print("DCF (Discounted Cash Flow) Method:")
print(paste("Valid responses:", n_dcf_valid))
print(paste("Using DCF:", n_dcf_used))
print(paste("Proportion:", round(prop_dcf * 100, 1), "%"))
print(paste("95% CI: [", round(ci_dcf[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_dcf[,"upr.ci"] * 100, 1), "%]", sep=""))

# Comparables Method
n_comp_valid <- sum(!is.na(df$valuation_method_comparables))
n_comp_used <- sum(df$valuation_method_comparables, na.rm = TRUE)
prop_comp <- n_comp_used / n_comp_valid
ci_comp <- BinomCI(n_comp_used, n_comp_valid, conf.level = 0.95, method = "wilson")

print("\nComparables Method:")
print(paste("Valid responses:", n_comp_valid))
print(paste("Using Comparables:", n_comp_used))
print(paste("Proportion:", round(prop_comp * 100, 1), "%"))
print(paste("95% CI: [", round(ci_comp[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_comp[,"upr.ci"] * 100, 1), "%]", sep=""))

# Asset-Based Method
n_asset_valid <- sum(!is.na(df$valuation_method_asset_based))
n_asset_used <- sum(df$valuation_method_asset_based, na.rm = TRUE)
prop_asset <- n_asset_used / n_asset_valid
ci_asset <- BinomCI(n_asset_used, n_asset_valid, conf.level = 0.95, method = "wilson")

print("\nAsset-Based Method:")
print(paste("Valid responses:", n_asset_valid))
print(paste("Using Asset-Based:", n_asset_used))
print(paste("Proportion:", round(prop_asset * 100, 1), "%"))
print(paste("95% CI: [", round(ci_asset[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_asset[,"upr.ci"] * 100, 1), "%]", sep=""))

# Summary comparison
valuation_summary <- data.frame(
  Method = c("DCF", "Comparables", "Asset-Based"),
  n_valid = c(n_dcf_valid, n_comp_valid, n_asset_valid),
  n_using = c(n_dcf_used, n_comp_used, n_asset_used),
  Proportion = c(prop_dcf, prop_comp, prop_asset) * 100,
  CI_lower = c(ci_dcf[,"lwr.ci"], ci_comp[,"lwr.ci"], ci_asset[,"lwr.ci"]) * 100,
  CI_upper = c(ci_dcf[,"upr.ci"], ci_comp[,"upr.ci"], ci_asset[,"upr.ci"]) * 100
) %>%
  mutate(
    Proportion = round(Proportion, 1),
    CI_lower = round(CI_lower, 1),
    CI_upper = round(CI_upper, 1),
    CI_text = paste("[", CI_lower, "%, ", CI_upper, "%]", sep="")
  ) %>%
  arrange(desc(Proportion))

print("\nValuation methodology summary:")
print(valuation_summary %>% select(Method, n_using, Proportion, CI_text))

# Multiple method usage analysis
df_methods <- df %>%
  filter(!is.na(valuation_method_dcf), 
         !is.na(valuation_method_comparables),
         !is.na(valuation_method_asset_based)) %>%
  mutate(
    total_methods = valuation_method_dcf + 
                   valuation_method_comparables + 
                   valuation_method_asset_based
  )

method_count_dist <- df_methods %>%
  group_by(total_methods) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df_methods) * 100, 1),
    .groups = "drop"
  )

print("\nNumber of methods used concurrently:")
print(method_count_dist)

# Method combinations
method_combinations <- df_methods %>%
  mutate(
    combination = case_when(
      total_methods == 0 ~ "None",
      valuation_method_dcf == 1 & total_methods == 1 ~ "DCF only",
      valuation_method_comparables == 1 & total_methods == 1 ~ "Comparables only",
      valuation_method_asset_based == 1 & total_methods == 1 ~ "Asset-based only",
      valuation_method_dcf == 1 & valuation_method_comparables == 1 & total_methods == 2 ~ "DCF + Comparables",
      valuation_method_dcf == 1 & valuation_method_asset_based == 1 & total_methods == 2 ~ "DCF + Asset-based",
      valuation_method_comparables == 1 & valuation_method_asset_based == 1 & total_methods == 2 ~ "Comparables + Asset-based",
      total_methods == 3 ~ "All three methods",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(combination) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df_methods) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print("\nMethod combination patterns:")
print(combination_combinations)

# Primary valuation method analysis
if(!all(is.na(df$primary_valuation_method))) {
  primary_method_dist <- df %>%
    filter(!is.na(primary_valuation_method)) %>%
    group_by(primary_valuation_method) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$primary_valuation_method)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("\nPrimary valuation method preferences:")
  print(primary_method_dist)
}

# Analysis by stakeholder
stakeholder_valuation <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    dcf_pct = round(mean(valuation_method_dcf, na.rm = TRUE) * 100, 1),
    comparables_pct = round(mean(valuation_method_comparables, na.rm = TRUE) * 100, 1),
    asset_pct = round(mean(valuation_method_asset_based, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(stakeholder)

print("\nValuation methods by stakeholder (n ≥ 20):")
print(stakeholder_valuation)

# Chi-square tests for stakeholder differences
for(method in c("dcf", "comparables", "asset_based")) {
  col_name <- paste0("valuation_method_", method)
  
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

# Investment stage focus analysis
if(!all(is.na(df$investment_stage_focus))) {
  stage_valuation <- df %>%
    filter(!is.na(investment_stage_focus)) %>%
    group_by(investment_stage_focus) %>%
    summarise(
      n = n(),
      dcf_pct = round(mean(valuation_method_dcf, na.rm = TRUE) * 100, 1),
      comparables_pct = round(mean(valuation_method_comparables, na.rm = TRUE) * 100, 1),
      asset_pct = round(mean(valuation_method_asset_based, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(investment_stage_focus)
  
  print("\nValuation methods by investment stage (n ≥ 15):")
  print(stage_valuation)
}

# Sector focus analysis
if(!all(is.na(df$sector_focus))) {
  sector_valuation <- df %>%
    filter(!is.na(sector_focus)) %>%
    group_by(sector_focus) %>%
    summarise(
      n = n(),
      dcf_pct = round(mean(valuation_method_dcf, na.rm = TRUE) * 100, 1),
      comparables_pct = round(mean(valuation_method_comparables, na.rm = TRUE) * 100, 1),
      asset_pct = round(mean(valuation_method_asset_based, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(dcf_pct))
  
  print("\nValuation methods by sector focus (n ≥ 15):")
  print(head(sector_valuation, 10))
}

# Valuation confidence analysis
if(sum(!is.na(df$valuation_confidence_score)) > 50) {
  confidence_by_method <- df %>%
    filter(!is.na(valuation_confidence_score)) %>%
    mutate(
      uses_dcf = ifelse(valuation_method_dcf == 1, "Uses DCF", "No DCF"),
      uses_comparables = ifelse(valuation_method_comparables == 1, "Uses Comparables", "No Comparables"),
      uses_asset = ifelse(valuation_method_asset_based == 1, "Uses Asset-Based", "No Asset-Based")
    )
  
  # DCF confidence comparison
  dcf_confidence <- confidence_by_method %>%
    group_by(uses_dcf) %>%
    summarise(
      n = n(),
      mean_confidence = round(mean(valuation_confidence_score), 2),
      sd_confidence = round(sd(valuation_confidence_score), 2),
      .groups = "drop"
    )
  
  print("\nValuation confidence by DCF usage:")
  print(dcf_confidence)
  
  # t-test for DCF
  if(nrow(dcf_confidence) == 2) {
    dcf_yes <- df$valuation_confidence_score[df$valuation_method_dcf == 1 & 
                                            !is.na(df$valuation_confidence_score)]
    dcf_no <- df$valuation_confidence_score[df$valuation_method_dcf == 0 & 
                                           !is.na(df$valuation_confidence_score)]
    
    if(length(dcf_yes) >= 20 && length(dcf_no) >= 20) {
      t_test_dcf <- t.test(dcf_yes, dcf_no)
      print("DCF confidence comparison (t-test):")
      print(paste("t =", round(t_test_dcf$statistic, 2),
                  ", p =", format(t_test_dcf$p.value, scientific = TRUE)))
    }
  }
  
  # Comparables confidence comparison
  comp_confidence <- confidence_by_method %>%
    group_by(uses_comparables) %>%
    summarise(
      n = n(),
      mean_confidence = round(mean(valuation_confidence_score), 2),
      sd_confidence = round(sd(valuation_confidence_score), 2),
      .groups = "drop"
    )
  
  print("\nValuation confidence by Comparables usage:")
  print(comp_confidence)
}

# Method correlations
if(nrow(df_methods) > 50) {
  method_cors <- cor(df_methods[, c("valuation_method_dcf", 
                                    "valuation_method_comparables",
                                    "valuation_method_asset_based")],
                     use = "complete.obs")
  
  print("\nMethod usage correlations:")
  print(round(method_cors, 3))
  
  # Test significance of correlations
  print("\nCorrelation significance tests:")
  
  # DCF vs Comparables
  cor_dcf_comp <- cor.test(df$valuation_method_dcf, df$valuation_method_comparables)
  print(paste("DCF × Comparables: r =", round(cor_dcf_comp$estimate, 3),
              ", p =", format(cor_dcf_comp$p.value, scientific = TRUE)))
  
  # DCF vs Asset-based
  cor_dcf_asset <- cor.test(df$valuation_method_dcf, df$valuation_method_asset_based)
  print(paste("DCF × Asset-based: r =", round(cor_dcf_asset$estimate, 3),
              ", p =", format(cor_dcf_asset$p.value, scientific = TRUE)))
  
  # Comparables vs Asset-based
  cor_comp_asset <- cor.test(df$valuation_method_comparables, df$valuation_method_asset_based)
  print(paste("Comparables × Asset-based: r =", round(cor_comp_asset$estimate, 3),
              ", p =", format(cor_comp_asset$p.value, scientific = TRUE)))
}

# Logistic regression for each method
if(nrow(df) > 150) {
  # DCF model
  df_dcf_logit <- df %>%
    filter(!is.na(valuation_method_dcf), !is.na(stakeholder))
  
  if(nrow(df_dcf_logit) > 100) {
    predictors <- c("stakeholder")
    
    if(sum(!is.na(df_dcf_logit$investment_stage_focus)) > 80) {
      predictors <- c(predictors, "investment_stage_focus")
    }
    
    formula_str <- paste("valuation_method_dcf ~", paste(predictors, collapse = " + "))
    dcf_model <- glm(as.formula(formula_str), data = df_dcf_logit, family = binomial)
    
    print("\nLogistic regression - DCF usage predictors:")
    print(summary(dcf_model))
  }
  
  # Comparables model
  df_comp_logit <- df %>%
    filter(!is.na(valuation_method_comparables), !is.na(stakeholder))
  
  if(nrow(df_comp_logit) > 100) {
    comp_model <- glm(valuation_method_comparables ~ stakeholder, 
                     data = df_comp_logit, family = binomial)
    
    print("\nLogistic regression - Comparables usage predictors:")
    print(summary(comp_model))
  }
}

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
      dcf_pct = round(mean(valuation_method_dcf, na.rm = TRUE) * 100, 1),
      comparables_pct = round(mean(valuation_method_comparables, na.rm = TRUE) * 100, 1),
      asset_pct = round(mean(valuation_method_asset_based, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nValuation method trends over time:")
    print(df_time)
  }
}

# Summary of key findings
print("\nSummary of valuation methodology preferences:")
print(paste("DCF:", round(prop_dcf * 100, 1), "% (95% CI: [", 
            round(ci_dcf[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_dcf[,"upr.ci"] * 100, 1), "%])"))
print(paste("Comparables:", round(prop_comp * 100, 1), "% (95% CI: [", 
            round(ci_comp[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_comp[,"upr.ci"] * 100, 1), "%])"))
print(paste("Asset-based:", round(prop_asset * 100, 1), "% (95% CI: [", 
            round(ci_asset[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_asset[,"upr.ci"] * 100, 1), "%])"))

# Expected: DCF 68%, comparables 55%, asset-based 27%