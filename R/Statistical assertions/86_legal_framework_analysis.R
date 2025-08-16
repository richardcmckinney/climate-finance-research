# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 86: "Legal framework analysis: adequate in 58% of jurisdictions (95% CI [54.6%, 61.3%])"
# Purpose: Analyze legal framework adequacy across jurisdictions with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_LEGAL_FRAMEWORK_ADEQUATE <- "legal_framework_adequate"
COL_JURISDICTION <- "jurisdiction"
COL_LEGAL_CLARITY_SCORE <- "legal_clarity_score"
COL_REGULATORY_GAPS <- "regulatory_gaps_identified"
COL_ENFORCEMENT_EFFECTIVENESS <- "enforcement_effectiveness_score"
COL_STAKEHOLDER <- "stakeholder"
COL_LEGAL_CHALLENGES_FACED <- "legal_challenges_count"
COL_FRAMEWORK_UPDATE_FREQUENCY <- "framework_update_frequency_years"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    legal_framework_adequate = as.integer(legal_framework_adequate),
    jurisdiction = factor(jurisdiction),
    legal_clarity_score = as.numeric(legal_clarity_score),
    regulatory_gaps_identified = as.character(regulatory_gaps_identified),
    enforcement_effectiveness_score = as.numeric(enforcement_effectiveness_score),
    stakeholder = factor(stakeholder),
    legal_challenges_count = as.numeric(legal_challenges_count),
    framework_update_frequency_years = as.numeric(framework_update_frequency_years)
  ) %>%
  filter(!is.na(legal_framework_adequate))

# Overall legal framework adequacy
n_total <- nrow(df)
n_adequate <- sum(df$legal_framework_adequate, na.rm = TRUE)
prop_adequate <- n_adequate / n_total

print("Legal framework adequacy assessment:")
print(paste("Total jurisdictions assessed:", n_total))
print(paste("Adequate frameworks:", n_adequate))
print(paste("Proportion adequate:", round(prop_adequate * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_adequate, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence intervals
ci_exact <- BinomCI(n_adequate, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_agresti <- BinomCI(n_adequate, n_total, conf.level = 0.95, method = "agresti-coull")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Agresti-Coull CI: [", round(ci_agresti[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_agresti[,"upr.ci"] * 100, 1), "%]", sep=""))

# Legal clarity score analysis
if(sum(!is.na(df$legal_clarity_score)) > 50) {
  clarity_stats <- df %>%
    filter(!is.na(legal_clarity_score)) %>%
    summarise(
      n = n(),
      mean_clarity = round(mean(legal_clarity_score), 2),
      sd_clarity = round(sd(legal_clarity_score), 2),
      median_clarity = round(median(legal_clarity_score), 2),
      q25_clarity = round(quantile(legal_clarity_score, 0.25), 2),
      q75_clarity = round(quantile(legal_clarity_score, 0.75), 2)
    )
  
  print("\nLegal clarity score statistics:")
  print(clarity_stats)
  
  # Comparison by adequacy
  clarity_by_adequacy <- df %>%
    filter(!is.na(legal_clarity_score)) %>%
    mutate(
      adequacy_status = ifelse(legal_framework_adequate == 1,
                              "Adequate Framework", "Inadequate Framework")
    ) %>%
    group_by(adequacy_status) %>%
    summarise(
      n = n(),
      mean_clarity = round(mean(legal_clarity_score), 2),
      sd_clarity = round(sd(legal_clarity_score), 2),
      .groups = "drop"
    )
  
  print("\nClarity score by framework adequacy:")
  print(clarity_by_adequacy)
  
  # t-test for clarity differences
  if(nrow(clarity_by_adequacy) == 2) {
    adequate_clarity <- df$legal_clarity_score[df$legal_framework_adequate == 1 & 
                                               !is.na(df$legal_clarity_score)]
    inadequate_clarity <- df$legal_clarity_score[df$legal_framework_adequate == 0 & 
                                                 !is.na(df$legal_clarity_score)]
    
    if(length(adequate_clarity) >= 20 && length(inadequate_clarity) >= 20) {
      t_test <- t.test(adequate_clarity, inadequate_clarity)
      
      print("Clarity score comparison (t-test):")
      print(paste("t =", round(t_test$statistic, 2)))
      print(paste("df =", round(t_test$parameter, 1)))
      print(paste("p-value:", format(t_test$p.value, scientific = TRUE)))
      print(paste("Mean difference:", round(diff(t_test$estimate), 2)))
      
      # Cohen's d
      pooled_sd <- sqrt(((length(adequate_clarity) - 1) * var(adequate_clarity) + 
                        (length(inadequate_clarity) - 1) * var(inadequate_clarity)) / 
                       (length(adequate_clarity) + length(inadequate_clarity) - 2))
      cohens_d <- (mean(adequate_clarity) - mean(inadequate_clarity)) / pooled_sd
      print(paste("Cohen's d =", round(cohens_d, 3)))
    }
  }
}

# Regulatory gaps analysis
if(!all(is.na(df$regulatory_gaps_identified))) {
  # Parse comma-separated gaps
  gaps_df <- df %>%
    filter(!is.na(regulatory_gaps_identified)) %>%
    mutate(gaps_list = strsplit(regulatory_gaps_identified, ",")) %>%
    unnest(gaps_list) %>%
    mutate(gaps_list = trimws(gaps_list))
  
  if(nrow(gaps_df) > 0) {
    gap_counts <- gaps_df %>%
      group_by(gaps_list) %>%
      summarise(
        n = n(),
        percentage = round(n() / nrow(df) * 100, 1),
        .groups = "drop"
      ) %>%
      arrange(desc(n)) %>%
      head(15)
    
    print("\nTop 15 regulatory gaps identified:")
    print(gap_counts)
    
    # Number of gaps by adequacy
    gaps_by_adequacy <- df %>%
      filter(!is.na(regulatory_gaps_identified)) %>%
      mutate(
        n_gaps = str_count(regulatory_gaps_identified, ",") + 1,
        adequacy_status = ifelse(legal_framework_adequate == 1,
                                "Adequate", "Inadequate")
      ) %>%
      group_by(adequacy_status) %>%
      summarise(
        mean_gaps = round(mean(n_gaps), 1),
        sd_gaps = round(sd(n_gaps), 1),
        median_gaps = median(n_gaps),
        .groups = "drop"
      )
    
    print("\nRegulatory gaps by framework adequacy:")
    print(gaps_by_adequacy)
  }
}

# Enforcement effectiveness analysis
if(sum(!is.na(df$enforcement_effectiveness_score)) > 50) {
  enforcement_stats <- df %>%
    filter(!is.na(enforcement_effectiveness_score)) %>%
    summarise(
      n = n(),
      mean_enforcement = round(mean(enforcement_effectiveness_score), 2),
      sd_enforcement = round(sd(enforcement_effectiveness_score), 2),
      median_enforcement = round(median(enforcement_effectiveness_score), 2)
    )
  
  print("\nEnforcement effectiveness statistics:")
  print(enforcement_stats)
  
  # Correlation with framework adequacy
  cor_enforcement <- cor.test(df$legal_framework_adequate[!is.na(df$enforcement_effectiveness_score)],
                              df$enforcement_effectiveness_score[!is.na(df$enforcement_effectiveness_score)])
  
  print("Framework adequacy × Enforcement effectiveness correlation:")
  print(paste("r =", round(cor_enforcement$estimate, 3)))
  print(paste("95% CI: [", round(cor_enforcement$conf.int[1], 3), ", ", 
              round(cor_enforcement$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_enforcement$p.value, scientific = TRUE)))
}

# Analysis by jurisdiction
if(!all(is.na(df$jurisdiction))) {
  jurisdiction_adequacy <- df %>%
    filter(!is.na(jurisdiction)) %>%
    group_by(jurisdiction) %>%
    summarise(
      n = n(),
      adequate_count = sum(legal_framework_adequate),
      adequacy_rate = round(mean(legal_framework_adequate) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(adequacy_rate)) %>%
    head(20)
  
  print("\nTop 20 jurisdictions by framework adequacy (n ≥ 10):")
  print(jurisdiction_adequacy)
  
  # Regional grouping if possible
  if("region" %in% names(df)) {
    regional_adequacy <- df %>%
      filter(!is.na(region)) %>%
      group_by(region) %>%
      summarise(
        n = n(),
        adequate_count = sum(legal_framework_adequate),
        adequacy_rate = round(mean(legal_framework_adequate) * 100, 1),
        .groups = "drop"
      ) %>%
      filter(n >= 20) %>%
      arrange(desc(adequacy_rate))
    
    print("\nFramework adequacy by region (n ≥ 20):")
    print(regional_adequacy)
  }
}

# Analysis by stakeholder
stakeholder_adequacy <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    adequate_count = sum(legal_framework_adequate),
    adequacy_rate = round(mean(legal_framework_adequate) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(adequacy_rate))

print("\nFramework adequacy by stakeholder (n ≥ 20):")
print(stakeholder_adequacy)

# Calculate Wilson CIs for each stakeholder
stakeholder_ci <- stakeholder_adequacy %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(adequate_count, n, conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("\nStakeholder adequacy with 95% Wilson CIs:")
print(stakeholder_ci %>% select(stakeholder, n, adequacy_rate, ci_text))

# Legal challenges analysis
if(sum(!is.na(df$legal_challenges_count)) > 50) {
  challenges_comparison <- df %>%
    filter(!is.na(legal_challenges_count)) %>%
    mutate(
      adequacy_status = ifelse(legal_framework_adequate == 1,
                              "Adequate Framework", "Inadequate Framework")
    ) %>%
    group_by(adequacy_status) %>%
    summarise(
      n = n(),
      mean_challenges = round(mean(legal_challenges_count), 1),
      sd_challenges = round(sd(legal_challenges_count), 1),
      median_challenges = median(legal_challenges_count),
      .groups = "drop"
    )
  
  print("\nLegal challenges by framework adequacy:")
  print(challenges_comparison)
  
  # Mann-Whitney U test for challenges
  if(nrow(challenges_comparison) == 2) {
    adequate_challenges <- df$legal_challenges_count[df$legal_framework_adequate == 1 & 
                                                    !is.na(df$legal_challenges_count)]
    inadequate_challenges <- df$legal_challenges_count[df$legal_framework_adequate == 0 & 
                                                      !is.na(df$legal_challenges_count)]
    
    if(length(adequate_challenges) >= 15 && length(inadequate_challenges) >= 15) {
      wilcox_test <- wilcox.test(adequate_challenges, inadequate_challenges)
      
      print("Legal challenges comparison (Mann-Whitney U):")
      print(paste("W =", wilcox_test$statistic))
      print(paste("p-value:", format(wilcox_test$p.value, scientific = TRUE)))
    }
  }
}

# Framework update frequency analysis
if(sum(!is.na(df$framework_update_frequency_years)) > 50) {
  update_analysis <- df %>%
    filter(!is.na(framework_update_frequency_years)) %>%
    mutate(
      update_category = case_when(
        framework_update_frequency_years <= 1 ~ "Annual or more",
        framework_update_frequency_years <= 3 ~ "Every 2-3 years",
        framework_update_frequency_years <= 5 ~ "Every 4-5 years",
        framework_update_frequency_years > 5 ~ "More than 5 years"
      )
    ) %>%
    group_by(update_category) %>%
    summarise(
      n = n(),
      adequacy_rate = round(mean(legal_framework_adequate) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(update_category)
  
  print("\nFramework adequacy by update frequency:")
  print(update_analysis)
  
  # Correlation test
  cor_update <- cor.test(df$legal_framework_adequate[!is.na(df$framework_update_frequency_years)],
                         df$framework_update_frequency_years[!is.na(df$framework_update_frequency_years)],
                         method = "spearman")
  
  print("Update frequency × Adequacy correlation (Spearman):")
  print(paste("ρ =", round(cor_update$estimate, 3)))
  print(paste("p-value:", format(cor_update$p.value, scientific = TRUE)))
}

# Logistic regression model
if(nrow(df) > 150) {
  df_logistic <- df
  
  # Build model with available predictors
  predictors <- c()
  
  if(sum(!is.na(df_logistic$legal_clarity_score)) > 100) {
    predictors <- c(predictors, "legal_clarity_score")
  }
  
  if(sum(!is.na(df_logistic$enforcement_effectiveness_score)) > 100) {
    predictors <- c(predictors, "enforcement_effectiveness_score")
  }
  
  if(sum(!is.na(df_logistic$framework_update_frequency_years)) > 100) {
    predictors <- c(predictors, "framework_update_frequency_years")
  }
  
  if(length(unique(df_logistic$stakeholder)) > 1) {
    predictors <- c(predictors, "stakeholder")
  }
  
  if(length(predictors) > 0 && nrow(df_logistic) > 100) {
    formula_str <- paste("legal_framework_adequate ~", 
                        paste(predictors, collapse = " + "))
    logit_model <- glm(as.formula(formula_str), data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("\nLogistic regression: Framework adequacy predictors")
    print(logit_summary)
    
    # Odds ratios
    odds_ratios <- exp(coef(logit_model))
    odds_ci <- exp(confint(logit_model))
    
    print("\nOdds ratios with 95% CIs:")
    for(i in 1:min(10, length(odds_ratios))) {
      print(paste(names(odds_ratios)[i], ": OR =", round(odds_ratios[i], 3),
                  ", 95% CI [", round(odds_ci[i, 1], 3), ", ", 
                  round(odds_ci[i, 2], 3), "]", sep=""))
    }
    
    # Model fit
    print("\nModel fit statistics:")
    print(paste("AIC:", round(AIC(logit_model), 1)))
    print(paste("BIC:", round(BIC(logit_model), 1)))
    
    # McFadden's pseudo R²
    null_model <- glm(legal_framework_adequate ~ 1, 
                     data = df_logistic, family = binomial)
    mcfadden_r2 <- 1 - (logit_model$deviance / null_model$deviance)
    print(paste("McFadden's pseudo R² =", round(mcfadden_r2, 3)))
  }
}

# Framework components analysis
framework_components <- df %>%
  mutate(
    has_clarity = !is.na(legal_clarity_score) & legal_clarity_score >= 7,
    has_enforcement = !is.na(enforcement_effectiveness_score) & enforcement_effectiveness_score >= 7,
    regular_updates = !is.na(framework_update_frequency_years) & framework_update_frequency_years <= 3
  ) %>%
  mutate(
    component_count = has_clarity + has_enforcement + regular_updates
  ) %>%
  group_by(component_count) %>%
  summarise(
    n = n(),
    adequacy_rate = round(mean(legal_framework_adequate) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(component_count)

print("\nFramework adequacy by quality components:")
print(framework_components)

# International comparison if applicable
if("country_development_status" %in% names(df)) {
  development_adequacy <- df %>%
    filter(!is.na(country_development_status)) %>%
    group_by(country_development_status) %>%
    summarise(
      n = n(),
      adequate_count = sum(legal_framework_adequate),
      adequacy_rate = round(mean(legal_framework_adequate) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 20)
  
  print("\nFramework adequacy by development status (n ≥ 20):")
  print(development_adequacy)
}

# Time trend analysis (if date data available)
if("response_date" %in% names(df) && sum(!is.na(df$response_date)) > 100) {
  df_time <- df %>%
    filter(!is.na(response_date)) %>%
    mutate(
      response_year = year(as.Date(response_date))
    ) %>%
    group_by(response_year) %>%
    summarise(
      n = n(),
      adequate_count = sum(legal_framework_adequate),
      adequacy_rate = round(mean(legal_framework_adequate) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 30) %>%
    arrange(response_year)
  
  if(nrow(df_time) > 2) {
    print("\nFramework adequacy trends over time:")
    print(df_time)
    
    # Trend test
    trend_cor <- cor.test(df_time$response_year, df_time$adequacy_rate, 
                          method = "spearman")
    print("Temporal trend (Spearman):")
    print(paste("ρ =", round(trend_cor$estimate, 3)))
    print(paste("p-value:", format(trend_cor$p.value, scientific = TRUE)))
  }
}

# Summary of key findings
print("\nSummary of legal framework analysis:")
print(paste("Adequate frameworks:", round(prop_adequate * 100, 1), "% of jurisdictions"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Expected: adequate in 58% of jurisdictions (95% CI [54.6%, 61.3%])