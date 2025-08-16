# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 69: "Diversification approaches: sector-based 52%, geographic 31%, stage 17%"
# Purpose: Analyze distribution of diversification strategies with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_DIVERSIFICATION_SECTOR <- "diversification_sector_based"
COL_DIVERSIFICATION_GEOGRAPHIC <- "diversification_geographic"
COL_DIVERSIFICATION_STAGE <- "diversification_stage_based"
COL_STAKEHOLDER <- "stakeholder"
COL_PORTFOLIO_SIZE <- "portfolio_size"
COL_PORTFOLIO_PERFORMANCE <- "portfolio_performance"
COL_RISK_TOLERANCE <- "risk_tolerance_level"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    diversification_sector_based = as.integer(diversification_sector_based),
    diversification_geographic = as.integer(diversification_geographic),
    diversification_stage_based = as.integer(diversification_stage_based),
    stakeholder = factor(stakeholder),
    portfolio_size = as.numeric(portfolio_size),
    portfolio_performance = as.numeric(portfolio_performance),
    risk_tolerance_level = as.numeric(risk_tolerance_level)
  )

# Calculate proportions for each diversification approach
n_total <- nrow(df)

# Sector-based diversification
n_sector <- sum(df$diversification_sector_based, na.rm = TRUE)
prop_sector <- n_sector / sum(!is.na(df$diversification_sector_based))
ci_sector <- BinomCI(n_sector, sum(!is.na(df$diversification_sector_based)), 
                      conf.level = 0.95, method = "wilson")

print("Sector-based diversification:")
print(paste("Total valid responses:", sum(!is.na(df$diversification_sector_based))))
print(paste("Using sector diversification:", n_sector))
print(paste("Proportion:", round(prop_sector * 100, 1), "%"))
print(paste("95% CI: [", round(ci_sector[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_sector[,"upr.ci"] * 100, 1), "%]", sep=""))

# Geographic diversification
n_geographic <- sum(df$diversification_geographic, na.rm = TRUE)
prop_geographic <- n_geographic / sum(!is.na(df$diversification_geographic))
ci_geographic <- BinomCI(n_geographic, sum(!is.na(df$diversification_geographic)), 
                         conf.level = 0.95, method = "wilson")

print("Geographic diversification:")
print(paste("Total valid responses:", sum(!is.na(df$diversification_geographic))))
print(paste("Using geographic diversification:", n_geographic))
print(paste("Proportion:", round(prop_geographic * 100, 1), "%"))
print(paste("95% CI: [", round(ci_geographic[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_geographic[,"upr.ci"] * 100, 1), "%]", sep=""))

# Stage-based diversification
n_stage <- sum(df$diversification_stage_based, na.rm = TRUE)
prop_stage <- n_stage / sum(!is.na(df$diversification_stage_based))
ci_stage <- BinomCI(n_stage, sum(!is.na(df$diversification_stage_based)), 
                    conf.level = 0.95, method = "wilson")

print("Stage-based diversification:")
print(paste("Total valid responses:", sum(!is.na(df$diversification_stage_based))))
print(paste("Using stage diversification:", n_stage))
print(paste("Proportion:", round(prop_stage * 100, 1), "%"))
print(paste("95% CI: [", round(ci_stage[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_stage[,"upr.ci"] * 100, 1), "%]", sep=""))

# Summary comparison
diversification_summary <- data.frame(
  Approach = c("Sector-based", "Geographic", "Stage-based"),
  n = c(n_sector, n_geographic, n_stage),
  Proportion = c(prop_sector, prop_geographic, prop_stage) * 100,
  CI_lower = c(ci_sector[,"lwr.ci"], ci_geographic[,"lwr.ci"], ci_stage[,"lwr.ci"]) * 100,
  CI_upper = c(ci_sector[,"upr.ci"], ci_geographic[,"upr.ci"], ci_stage[,"upr.ci"]) * 100
) %>%
  mutate(
    Proportion = round(Proportion, 1),
    CI_lower = round(CI_lower, 1),
    CI_upper = round(CI_upper, 1),
    CI_text = paste("[", CI_lower, "%, ", CI_upper, "%]", sep="")
  ) %>%
  arrange(desc(Proportion))

print("Diversification approaches summary:")
print(diversification_summary %>% select(Approach, n, Proportion, CI_text))

# Test for differences between approaches
if(sum(!is.na(df$diversification_sector_based)) > 50 &&
   sum(!is.na(df$diversification_geographic)) > 50 &&
   sum(!is.na(df$diversification_stage_based)) > 50) {
  
  # McNemar's test for paired proportions
  # Sector vs Geographic
  sector_geo_table <- table(df$diversification_sector_based, df$diversification_geographic)
  if(dim(sector_geo_table)[1] == 2 && dim(sector_geo_table)[2] == 2) {
    mcnemar_sector_geo <- mcnemar.test(sector_geo_table)
    print("McNemar's test - Sector vs Geographic:")
    print(paste("χ² =", round(mcnemar_sector_geo$statistic, 2),
                ", p =", format(mcnemar_sector_geo$p.value, scientific = TRUE)))
  }
  
  # Sector vs Stage
  sector_stage_table <- table(df$diversification_sector_based, df$diversification_stage_based)
  if(dim(sector_stage_table)[1] == 2 && dim(sector_stage_table)[2] == 2) {
    mcnemar_sector_stage <- mcnemar.test(sector_stage_table)
    print("McNemar's test - Sector vs Stage:")
    print(paste("χ² =", round(mcnemar_sector_stage$statistic, 2),
                ", p =", format(mcnemar_sector_stage$p.value, scientific = TRUE)))
  }
  
  # Geographic vs Stage
  geo_stage_table <- table(df$diversification_geographic, df$diversification_stage_based)
  if(dim(geo_stage_table)[1] == 2 && dim(geo_stage_table)[2] == 2) {
    mcnemar_geo_stage <- mcnemar.test(geo_stage_table)
    print("McNemar's test - Geographic vs Stage:")
    print(paste("χ² =", round(mcnemar_geo_stage$statistic, 2),
                ", p =", format(mcnemar_geo_stage$p.value, scientific = TRUE)))
  }
}

# Multiple diversification strategies analysis
df_multi <- df %>%
  filter(!is.na(diversification_sector_based), 
         !is.na(diversification_geographic),
         !is.na(diversification_stage_based)) %>%
  mutate(
    total_strategies = diversification_sector_based + 
                       diversification_geographic + 
                       diversification_stage_based
  )

strategy_counts <- df_multi %>%
  group_by(total_strategies) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df_multi) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(total_strategies)

print("Number of diversification strategies used:")
print(strategy_counts)

# Combination patterns
if(nrow(df_multi) > 50) {
  combination_patterns <- df_multi %>%
    mutate(
      pattern = case_when(
        diversification_sector_based == 1 & diversification_geographic == 0 & 
          diversification_stage_based == 0 ~ "Sector only",
        diversification_sector_based == 0 & diversification_geographic == 1 & 
          diversification_stage_based == 0 ~ "Geographic only",
        diversification_sector_based == 0 & diversification_geographic == 0 & 
          diversification_stage_based == 1 ~ "Stage only",
        diversification_sector_based == 1 & diversification_geographic == 1 & 
          diversification_stage_based == 0 ~ "Sector + Geographic",
        diversification_sector_based == 1 & diversification_geographic == 0 & 
          diversification_stage_based == 1 ~ "Sector + Stage",
        diversification_sector_based == 0 & diversification_geographic == 1 & 
          diversification_stage_based == 1 ~ "Geographic + Stage",
        diversification_sector_based == 1 & diversification_geographic == 1 & 
          diversification_stage_based == 1 ~ "All three",
        TRUE ~ "None"
      )
    ) %>%
    group_by(pattern) %>%
    summarise(
      n = n(),
      percentage = round(n() / nrow(df_multi) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(n))
  
  print("Diversification pattern combinations:")
  print(combination_patterns)
}

# Analysis by stakeholder
stakeholder_diversification <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    sector_pct = round(mean(diversification_sector_based, na.rm = TRUE) * 100, 1),
    geographic_pct = round(mean(diversification_geographic, na.rm = TRUE) * 100, 1),
    stage_pct = round(mean(diversification_stage_based, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(stakeholder)

print("Diversification approaches by stakeholder (n ≥ 20):")
print(stakeholder_diversification)

# Chi-square tests for stakeholder differences
for(div_type in c("sector", "geographic", "stage")) {
  col_name <- paste0("diversification_", div_type, 
                    ifelse(div_type == "stage", "_based", 
                          ifelse(div_type == "sector", "_based", "")))
  
  if(col_name %in% names(df)) {
    div_table <- table(df$stakeholder, df[[col_name]])
    
    if(min(dim(div_table)) > 1) {
      chi_test <- chisq.test(div_table)
      print(paste("Chi-square test for", div_type, "diversification by stakeholder:"))
      print(paste("χ² =", round(chi_test$statistic, 2),
                  ", p =", format(chi_test$p.value, scientific = TRUE)))
    }
  }
}

# Portfolio size relationship
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_diversification <- df %>%
    filter(!is.na(portfolio_size)) %>%
    mutate(
      portfolio_category = case_when(
        portfolio_size <= 10 ~ "Small (≤10)",
        portfolio_size <= 50 ~ "Medium (11-50)",
        portfolio_size > 50 ~ "Large (>50)"
      )
    ) %>%
    group_by(portfolio_category) %>%
    summarise(
      n = n(),
      sector_pct = round(mean(diversification_sector_based, na.rm = TRUE) * 100, 1),
      geographic_pct = round(mean(diversification_geographic, na.rm = TRUE) * 100, 1),
      stage_pct = round(mean(diversification_stage_based, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Diversification by portfolio size (n ≥ 10):")
  print(portfolio_diversification)
  
  # Correlations with continuous portfolio size
  if(sum(!is.na(df$portfolio_size) & !is.na(df$diversification_sector_based)) > 30) {
    cor_sector <- cor.test(df$portfolio_size[!is.na(df$portfolio_size) & 
                                            !is.na(df$diversification_sector_based)],
                          df$diversification_sector_based[!is.na(df$portfolio_size) & 
                                                         !is.na(df$diversification_sector_based)])
    print(paste("Portfolio size × Sector diversification: r =", 
                round(cor_sector$estimate, 3),
                ", p =", format(cor_sector$p.value, scientific = TRUE)))
  }
  
  if(sum(!is.na(df$portfolio_size) & !is.na(df$diversification_geographic)) > 30) {
    cor_geo <- cor.test(df$portfolio_size[!is.na(df$portfolio_size) & 
                                         !is.na(df$diversification_geographic)],
                       df$diversification_geographic[!is.na(df$portfolio_size) & 
                                                    !is.na(df$diversification_geographic)])
    print(paste("Portfolio size × Geographic diversification: r =", 
                round(cor_geo$estimate, 3),
                ", p =", format(cor_geo$p.value, scientific = TRUE)))
  }
  
  if(sum(!is.na(df$portfolio_size) & !is.na(df$diversification_stage_based)) > 30) {
    cor_stage <- cor.test(df$portfolio_size[!is.na(df$portfolio_size) & 
                                           !is.na(df$diversification_stage_based)],
                         df$diversification_stage_based[!is.na(df$portfolio_size) & 
                                                       !is.na(df$diversification_stage_based)])
    print(paste("Portfolio size × Stage diversification: r =", 
                round(cor_stage$estimate, 3),
                ", p =", format(cor_stage$p.value, scientific = TRUE)))
  }
}

# Performance relationship
if(sum(!is.na(df$portfolio_performance)) > 50) {
  # Compare performance by number of diversification strategies
  performance_by_strategies <- df_multi %>%
    filter(!is.na(portfolio_performance)) %>%
    group_by(total_strategies) %>%
    summarise(
      n = n(),
      mean_performance = round(mean(portfolio_performance), 2),
      sd_performance = round(sd(portfolio_performance), 2),
      median_performance = round(median(portfolio_performance), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10)
  
  print("Portfolio performance by number of diversification strategies:")
  print(performance_by_strategies)
  
  # ANOVA for performance differences
  if(nrow(performance_by_strategies) > 2) {
    anova_model <- aov(portfolio_performance ~ as.factor(total_strategies), 
                       data = df_multi %>% filter(!is.na(portfolio_performance)))
    anova_summary <- summary(anova_model)
    
    print("ANOVA - Performance by diversification strategies:")
    print(anova_summary)
    
    # Post-hoc tests if significant
    if(anova_summary[[1]][["Pr(>F)"]][1] < 0.05) {
      tukey_test <- TukeyHSD(anova_model)
      print("Tukey's HSD post-hoc comparisons:")
      print(tukey_test)
    }
  }
}

# Risk tolerance relationship
if(sum(!is.na(df$risk_tolerance_level)) > 50) {
  risk_diversification <- df %>%
    filter(!is.na(risk_tolerance_level)) %>%
    mutate(
      risk_category = case_when(
        risk_tolerance_level <= 3 ~ "Low risk tolerance",
        risk_tolerance_level <= 7 ~ "Moderate risk tolerance",
        risk_tolerance_level > 7 ~ "High risk tolerance"
      )
    ) %>%
    group_by(risk_category) %>%
    summarise(
      n = n(),
      sector_pct = round(mean(diversification_sector_based, na.rm = TRUE) * 100, 1),
      geographic_pct = round(mean(diversification_geographic, na.rm = TRUE) * 100, 1),
      stage_pct = round(mean(diversification_stage_based, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15)
  
  print("Diversification by risk tolerance (n ≥ 15):")
  print(risk_diversification)
}

# Logistic regression for each diversification type
if(nrow(df) > 150) {
  # Sector diversification model
  if(sum(!is.na(df$diversification_sector_based)) > 100) {
    df_sector_logit <- df %>%
      filter(!is.na(diversification_sector_based), !is.na(portfolio_size), 
             !is.na(risk_tolerance_level))
    
    if(nrow(df_sector_logit) > 100) {
      sector_model <- glm(diversification_sector_based ~ portfolio_size + 
                         risk_tolerance_level + stakeholder,
                         data = df_sector_logit, family = binomial)
      
      print("Logistic regression - Sector diversification:")
      print(summary(sector_model))
    }
  }
  
  # Geographic diversification model
  if(sum(!is.na(df$diversification_geographic)) > 100) {
    df_geo_logit <- df %>%
      filter(!is.na(diversification_geographic), !is.na(portfolio_size), 
             !is.na(risk_tolerance_level))
    
    if(nrow(df_geo_logit) > 100) {
      geo_model <- glm(diversification_geographic ~ portfolio_size + 
                      risk_tolerance_level + stakeholder,
                      data = df_geo_logit, family = binomial)
      
      print("Logistic regression - Geographic diversification:")
      print(summary(geo_model))
    }
  }
  
  # Stage diversification model
  if(sum(!is.na(df$diversification_stage_based)) > 100) {
    df_stage_logit <- df %>%
      filter(!is.na(diversification_stage_based), !is.na(portfolio_size), 
             !is.na(risk_tolerance_level))
    
    if(nrow(df_stage_logit) > 100) {
      stage_model <- glm(diversification_stage_based ~ portfolio_size + 
                        risk_tolerance_level + stakeholder,
                        data = df_stage_logit, family = binomial)
      
      print("Logistic regression - Stage diversification:")
      print(summary(stage_model))
    }
  }
}

# Summary of key findings
print("Summary of diversification approaches:")
print(paste("Sector-based:", round(prop_sector * 100, 1), "%",
            "(95% CI: [", round(ci_sector[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_sector[,"upr.ci"] * 100, 1), "%])"))
print(paste("Geographic:", round(prop_geographic * 100, 1), "%",
            "(95% CI: [", round(ci_geographic[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_geographic[,"upr.ci"] * 100, 1), "%])"))
print(paste("Stage-based:", round(prop_stage * 100, 1), "%",
            "(95% CI: [", round(ci_stage[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_stage[,"upr.ci"] * 100, 1), "%])"))

# Expected: sector-based 52%, geographic 31%, stage 17%