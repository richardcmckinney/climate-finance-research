# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 85: "Regulatory compliance assessment: full compliance 72% (95% CI [68.8%, 75.1%])"
# Purpose: Analyze regulatory compliance levels with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_COMPLIANCE_LEVEL <- "regulatory_compliance_level"
COL_FULL_COMPLIANCE <- "full_regulatory_compliance"
COL_COMPLIANCE_SCORE <- "compliance_score_percentage"
COL_COMPLIANCE_AREAS <- "compliance_areas_covered"
COL_AUDIT_FREQUENCY <- "compliance_audit_frequency"
COL_STAKEHOLDER <- "stakeholder"
COL_JURISDICTION <- "primary_jurisdiction"
COL_COMPLIANCE_COST <- "annual_compliance_cost_percentage"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    regulatory_compliance_level = factor(regulatory_compliance_level),
    full_regulatory_compliance = as.integer(full_regulatory_compliance),
    compliance_score_percentage = as.numeric(compliance_score_percentage),
    compliance_areas_covered = as.character(compliance_areas_covered),
    compliance_audit_frequency = factor(compliance_audit_frequency),
    stakeholder = factor(stakeholder),
    primary_jurisdiction = factor(primary_jurisdiction),
    annual_compliance_cost_percentage = as.numeric(annual_compliance_cost_percentage)
  ) %>%
  filter(!is.na(full_regulatory_compliance))

# Overall full compliance rate
n_total <- nrow(df)
n_full_compliance <- sum(df$full_regulatory_compliance, na.rm = TRUE)
prop_full_compliance <- n_full_compliance / n_total

print("Regulatory compliance assessment:")
print(paste("Total respondents:", n_total))
print(paste("Full compliance:", n_full_compliance))
print(paste("Proportion:", round(prop_full_compliance * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_full_compliance, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence intervals
ci_exact <- BinomCI(n_full_compliance, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_full_compliance, n_total, conf.level = 0.95, method = "agresti-coull")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))

# Compliance level categories
if(!all(is.na(df$regulatory_compliance_level))) {
  compliance_levels <- df %>%
    filter(!is.na(regulatory_compliance_level)) %>%
    group_by(regulatory_compliance_level) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$regulatory_compliance_level)) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      regulatory_compliance_level = factor(regulatory_compliance_level,
                                         levels = c("Non-compliant", "Partial", 
                                                  "Substantial", "Full"))
    ) %>%
    arrange(regulatory_compliance_level)
  
  print("\nCompliance level distribution:")
  print(compliance_levels)
}

# Compliance score analysis
if(sum(!is.na(df$compliance_score_percentage)) > 50) {
  compliance_score_stats <- df %>%
    filter(!is.na(compliance_score_percentage)) %>%
    summarise(
      n = n(),
      mean_score = round(mean(compliance_score_percentage), 1),
      sd_score = round(sd(compliance_score_percentage), 1),
      median_score = round(median(compliance_score_percentage), 1),
      q25_score = round(quantile(compliance_score_percentage, 0.25), 1),
      q75_score = round(quantile(compliance_score_percentage, 0.75), 1)
    )
  
  print("\nCompliance score statistics (%):")
  print(compliance_score_stats)
  
  # 95% CI for mean score
  ci_mean_score <- MeanCI(df$compliance_score_percentage[!is.na(df$compliance_score_percentage)], 
                          conf.level = 0.95)
  print(paste("95% CI for mean score: [", round(ci_mean_score[2], 1), "%, ", 
              round(ci_mean_score[3], 1), "%]", sep=""))
  
  # Score categories
  score_categories <- df %>%
    filter(!is.na(compliance_score_percentage)) %>%
    mutate(
      score_category = case_when(
        compliance_score_percentage >= 95 ~ "Excellent (≥95%)",
        compliance_score_percentage >= 85 ~ "Good (85-94%)",
        compliance_score_percentage >= 70 ~ "Satisfactory (70-84%)",
        compliance_score_percentage >= 50 ~ "Needs Improvement (50-69%)",
        compliance_score_percentage < 50 ~ "Poor (<50%)"
      )
    ) %>%
    group_by(score_category) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$compliance_score_percentage)) * 100, 1),
      .groups = "drop"
    )
  
  print("\nCompliance score categories:")
  print(score_categories)
}

# Compliance areas analysis
if(!all(is.na(df$compliance_areas_covered))) {
  # Parse comma-separated areas
  areas_df <- df %>%
    filter(!is.na(compliance_areas_covered)) %>%
    mutate(areas_list = strsplit(compliance_areas_covered, ",")) %>%
    unnest(areas_list) %>%
    mutate(areas_list = trimws(areas_list))
  
  if(nrow(areas_df) > 0) {
    area_counts <- areas_df %>%
      group_by(areas_list) %>%
      summarise(
        n = n(),
        percentage = round(n() / nrow(df) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(n)) %>%
      head(15)
    
    print("\nTop 15 compliance areas covered:")
    print(area_counts)
    
    # Number of areas per respondent
    areas_per_respondent <- df %>%
      filter(!is.na(compliance_areas_covered)) %>%
      mutate(
        n_areas = str_count(compliance_areas_covered, ",") + 1
      ) %>%
      summarise(
        mean_areas = round(mean(n_areas), 1),
        sd_areas = round(sd(n_areas), 1),
        median_areas = median(n_areas)
      )
    
    print("\nNumber of compliance areas per respondent:")
    print(areas_per_respondent)
  }
}

# Audit frequency analysis
if(!all(is.na(df$compliance_audit_frequency))) {
  audit_frequency <- df %>%
    filter(!is.na(compliance_audit_frequency)) %>%
    group_by(compliance_audit_frequency) %>%
    summarise(
      n = n(),
      full_compliance_rate = round(mean(full_regulatory_compliance) * 100, 1),
      percentage = round(n() / sum(!is.na(df$compliance_audit_frequency)) * 100, 1),
      .groups = "drop"
    ) %>%
    mutate(
      compliance_audit_frequency = factor(compliance_audit_frequency,
                                         levels = c("Never", "Annually", "Semi-annually",
                                                  "Quarterly", "Monthly", "Continuous"))
    ) %>%
    arrange(compliance_audit_frequency)
  
  print("\nCompliance by audit frequency:")
  print(audit_frequency)
  
  # Chi-square test for audit frequency and compliance
  if(length(unique(df$compliance_audit_frequency[!is.na(df$compliance_audit_frequency)])) > 1) {
    audit_table <- table(df$compliance_audit_frequency[!is.na(df$compliance_audit_frequency)],
                        df$full_regulatory_compliance[!is.na(df$compliance_audit_frequency)])
    chi_audit <- chisq.test(audit_table)
    print("\nChi-square test - Audit frequency × Compliance:")
    print(paste("χ² =", round(chi_audit$statistic, 2)))
    print(paste("p-value:", format(chi_audit$p.value, scientific = TRUE)))
  }
}

# Analysis by stakeholder
stakeholder_compliance <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    full_compliance_count = sum(full_regulatory_compliance),
    full_compliance_rate = round(mean(full_regulatory_compliance) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(full_compliance_rate))

print("\nFull compliance by stakeholder (n ≥ 20):")
print(stakeholder_compliance)

# Calculate Wilson CIs for each stakeholder
stakeholder_ci <- stakeholder_compliance %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(full_compliance_count, n, conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("\nStakeholder compliance with 95% Wilson CIs:")
print(stakeholder_ci %>% select(stakeholder, n, full_compliance_rate, ci_text))

# Chi-square test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder, df$full_regulatory_compliance)
  chi_stakeholder <- chisq.test(stakeholder_table)
  print("\nChi-square test for stakeholder differences:")
  print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
  print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
  
  # Cramér's V
  cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * 
                                                 (min(dim(stakeholder_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Jurisdiction analysis
if(!all(is.na(df$primary_jurisdiction))) {
  jurisdiction_compliance <- df %>%
    filter(!is.na(primary_jurisdiction)) %>%
    group_by(primary_jurisdiction) %>%
    summarise(
      n = n(),
      full_compliance_count = sum(full_regulatory_compliance),
      full_compliance_rate = round(mean(full_regulatory_compliance) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(full_compliance_rate)) %>%
    head(15)
  
  print("\nTop 15 jurisdictions by compliance rate (n ≥ 15):")
  print(jurisdiction_compliance)
}

# Compliance cost analysis
if(sum(!is.na(df$annual_compliance_cost_percentage)) > 50) {
  cost_comparison <- df %>%
    filter(!is.na(annual_compliance_cost_percentage)) %>%
    mutate(
      compliance_status = ifelse(full_regulatory_compliance == 1,
                               "Full Compliance", "Partial/No Compliance")
    ) %>%
    group_by(compliance_status) %>%
    summarise(
      n = n(),
      mean_cost = round(mean(annual_compliance_cost_percentage), 2),
      sd_cost = round(sd(annual_compliance_cost_percentage), 2),
      median_cost = round(median(annual_compliance_cost_percentage), 2),
      .groups = "drop"
    )
  
  print("\nCompliance cost by status (% of revenue):")
  print(cost_comparison)
  
  # t-test for cost differences
  if(nrow(cost_comparison) == 2) {
    full_costs <- df$annual_compliance_cost_percentage[
      df$full_regulatory_compliance == 1 & !is.na(df$annual_compliance_cost_percentage)]
    partial_costs <- df$annual_compliance_cost_percentage[
      df$full_regulatory_compliance == 0 & !is.na(df$annual_compliance_cost_percentage)]
    
    if(length(full_costs) >= 20 && length(partial_costs) >= 20) {
      t_test <- t.test(full_costs, partial_costs)
      
      print("Compliance cost comparison (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 2), "%"))
    }
  }
  
  # Cost-benefit analysis
  cost_benefit <- df %>%
    filter(!is.na(annual_compliance_cost_percentage)) %>%
    mutate(
      cost_category = case_when(
        annual_compliance_cost_percentage < 1 ~ "Low (<1%)",
        annual_compliance_cost_percentage < 3 ~ "Moderate (1-3%)",
        annual_compliance_cost_percentage < 5 ~ "High (3-5%)",
        annual_compliance_cost_percentage >= 5 ~ "Very High (≥5%)"
      )
    ) %>%
    group_by(cost_category) %>%
    summarise(
      n = n(),
      full_compliance_rate = round(mean(full_regulatory_compliance) * 100, 1),
      .groups = "drop"
    )
  
  print("\nCompliance rate by cost category:")
  print(cost_benefit)
}

# Logistic regression model
if(nrow(df) > 150) {
  df_logistic <- df %>%
    filter(!is.na(stakeholder))
  
  # Build model with available predictors
  predictors <- c("stakeholder")
  
  if(sum(!is.na(df_logistic$compliance_audit_frequency)) > 100) {
    predictors <- c(predictors, "compliance_audit_frequency")
  }
  
  if(sum(!is.na(df_logistic$annual_compliance_cost_percentage)) > 100) {
    predictors <- c(predictors, "annual_compliance_cost_percentage")
  }
  
  if(sum(!is.na(df_logistic$primary_jurisdiction)) > 100) {
    predictors <- c(predictors, "primary_jurisdiction")
  }
  
  if(length(predictors) > 1 && nrow(df_logistic) > 100) {
    formula_str <- paste("full_regulatory_compliance ~", 
                        paste(predictors, collapse = " + "))
    logit_model <- glm(as.formula(formula_str), data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("\nLogistic regression: Full compliance predictors")
    print(logit_summary)
    
    # Odds ratios
    odds_ratios <- exp(coef(logit_model))
    odds_ci <- exp(confint(logit_model))
    
    print("\nOdds ratios with 95% CIs:")
    for(i in 1:min(10, length(odds_ratios))) {  # Limit to first 10 for readability
      print(paste(names(odds_ratios)[i], ": OR =", round(odds_ratios[i], 3),
                  ", 95% CI [", round(odds_ci[i, 1], 3), ", ", 
                  round(odds_ci[i, 2], 3), "]", sep=""))
    }
    
    # Model fit
    print("\nModel fit statistics:")
    print(paste("AIC:", round(AIC(logit_model), 1)))
    print(paste("BIC:", round(BIC(logit_model), 1)))
    
    # McFadden's pseudo R²
    null_model <- glm(full_regulatory_compliance ~ 1, 
                     data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

# Compliance trends analysis
compliance_trend <- df %>%
  mutate(
    compliance_group = case_when(
      full_regulatory_compliance == 1 ~ "Full Compliance",
      compliance_score_percentage >= 70 ~ "High Compliance",
      compliance_score_percentage >= 50 ~ "Moderate Compliance",
      TRUE ~ "Low Compliance"
    )
  ) %>%
  group_by(compliance_group) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df) * 100, 1),
    .groups = "drop"
  )

print("\nOverall compliance distribution:")
print(compliance_trend)

# Regional compliance patterns (if region data available)
if("region" %in% names(df) && sum(!is.na(df$region)) > 50) {
  regional_compliance <- df %>%
    filter(!is.na(region)) %>%
    group_by(region) %>%
    summarise(
      n = n(),
      full_compliance_count = sum(full_regulatory_compliance),
      full_compliance_rate = round(mean(full_regulatory_compliance) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(full_compliance_rate))
  
  print("\nCompliance by region (n ≥ 20):")
  print(regional_compliance)
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
      full_compliance_count = sum(full_regulatory_compliance),
      full_compliance_rate = round(mean(full_regulatory_compliance) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nCompliance trends over time:")
    print(df_time)
    
    # Trend test
    df_time <- df_time %>%
      mutate(quarter_num = row_number())
    
    trend_cor <- cor.test(df_time$quarter_num, df_time$full_compliance_rate, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of regulatory compliance assessment:")
print(paste("Full compliance rate:", round(prop_full_compliance * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: full compliance 72% (95% CI [68.8%, 75.1%])