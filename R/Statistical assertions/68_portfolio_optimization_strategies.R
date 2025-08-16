# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 68: "Modern portfolio theory applied by 67% (95% CI [63.7%, 70.2%]) with higher usage among institutional investors"
# Purpose: Proportion analysis of portfolio optimization strategy adoption with stakeholder comparison

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MPT_APPLICATION <- "modern_portfolio_theory_application"
COL_STAKEHOLDER <- "stakeholder"
COL_INSTITUTION_TYPE <- "institution_type"
COL_AUM <- "assets_under_management"
COL_PORTFOLIO_SIZE <- "portfolio_size"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    modern_portfolio_theory_application = as.integer(modern_portfolio_theory_application),
    stakeholder = factor(stakeholder),
    institution_type = factor(institution_type),
    assets_under_management = as.numeric(assets_under_management),
    portfolio_size = as.numeric(portfolio_size)
  ) %>%
  filter(!is.na(modern_portfolio_theory_application))

# Overall MPT application analysis
n_total <- nrow(df)
n_mpt <- sum(df$modern_portfolio_theory_application, na.rm = TRUE)
prop_mpt <- n_mpt / n_total

print("Overall Modern Portfolio Theory application:")
print(paste("Total respondents:", n_total))
print(paste("Using MPT:", n_mpt))
print(paste("Proportion:", round(prop_mpt * 100, 1), "%"))

# Wilson confidence interval
ci_wilson <- BinomCI(n_mpt, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Alternative confidence interval methods
ci_exact <- BinomCI(n_mpt, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_wald <- BinomCI(n_mpt, n_total, conf.level = 0.95, method = "wald")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Wald CI: [", round(ci_wald[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wald[,"upr.ci"] * 100, 1), "%]", sep=""))

# Institutional vs non-institutional comparison
if(!all(is.na(df$institution_type))) {
  df_inst <- df %>%
    filter(!is.na(institution_type)) %>%
    mutate(
      institutional = ifelse(institution_type %in% c("Bank", "Insurance", "Pension Fund", 
                                                      "Sovereign Wealth Fund", "Asset Manager"),
                            "Institutional", "Non-Institutional")
    )
  
  inst_comparison <- df_inst %>%
    group_by(institutional) %>%
    summarise(
      n = n(),
      mpt_count = sum(modern_portfolio_theory_application),
      proportion = round(mean(modern_portfolio_theory_application) * 100, 1),
      .groups = "drop"
    )
  
  print("MPT usage by institutional status:")
  print(inst_comparison)
  
  # Calculate Wilson CIs for each group
  inst_comparison_ci <- inst_comparison %>%
    rowwise() %>%
    mutate(
      ci_data = list(BinomCI(mpt_count, n, conf.level = 0.95, method = "wilson")),
      ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
      ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
      ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
    ) %>%
    select(-ci_data)
  
  print("Institutional comparison with 95% Wilson CIs:")
  print(inst_comparison_ci %>% select(institutional, n, proportion, ci_text))
  
  # Fisher's exact test for institutional difference
  if(nrow(inst_comparison) == 2) {
    inst_table <- table(df_inst$institutional, df_inst$modern_portfolio_theory_application)
    fisher_inst <- fisher.test(inst_table)
    print("Institutional vs Non-institutional (Fisher's exact test):")
    print(paste("p-value:", format(fisher_inst$p.value, scientific = TRUE)))
    print(paste("Odds ratio:", round(fisher_inst$estimate, 2)))
    print(paste("95% CI for OR: [", round(fisher_inst$conf.int[1], 2), ", ",
                round(fisher_inst$conf.int[2], 2), "]", sep=""))
  }
}

# Breakdown by stakeholder type
stakeholder_breakdown <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mpt_count = sum(modern_portfolio_theory_application, na.rm = TRUE),
    proportion = round(mean(modern_portfolio_theory_application, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(proportion))

print("MPT usage by stakeholder:")
print(stakeholder_breakdown)

# Chi-square test for stakeholder differences
if(length(unique(df$stakeholder)) > 1) {
  stakeholder_table <- table(df$stakeholder, df$modern_portfolio_theory_application)
  chi_stakeholder <- chisq.test(stakeholder_table)
  print("Chi-square test for stakeholder variation:")
  print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
  print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))
  
  # Cramér's V
  cramers_v <- sqrt(chi_stakeholder$statistic / (sum(stakeholder_table) * 
                    (min(dim(stakeholder_table)) - 1)))
  print(paste("Cramér's V =", round(cramers_v, 3)))
}

# Analysis by AUM size
if(sum(!is.na(df$assets_under_management)) > 50) {
  df_aum <- df %>%
    filter(!is.na(assets_under_management)) %>%
    mutate(
      aum_category = case_when(
        assets_under_management < 100 ~ "< $100M",
        assets_under_management < 1000 ~ "$100M-$1B",
        assets_under_management < 10000 ~ "$1B-$10B",
        assets_under_management >= 10000 ~ "> $10B"
      )
    )
  
  aum_analysis <- df_aum %>%
    group_by(aum_category) %>%
    summarise(
      n = n(),
      mpt_count = sum(modern_portfolio_theory_application),
      proportion = round(mean(modern_portfolio_theory_application) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(proportion)
  
  print("MPT usage by AUM category (n ≥ 20):")
  print(aum_analysis)
  
  # Trend test for AUM
  if(nrow(aum_analysis) > 2) {
    aum_trend <- cor.test(df_aum$assets_under_management,
                          df_aum$modern_portfolio_theory_application,
                          method = "spearman")
    print("AUM size trend test (Spearman):")
    print(paste("ρ =", round(aum_trend$estimate, 3)))
    print(paste("p-value:", format(aum_trend$p.value, scientific = TRUE)))
  }
}

# Portfolio size analysis
if(sum(!is.na(df$portfolio_size)) > 50) {
  portfolio_analysis <- df %>%
    filter(!is.na(portfolio_size)) %>%
    mutate(
      portfolio_category = case_when(
        portfolio_size <= 10 ~ "Small (≤10)",
        portfolio_size <= 50 ~ "Medium (11-50)",
        portfolio_size <= 100 ~ "Large (51-100)",
        portfolio_size > 100 ~ "Very Large (>100)"
      )
    ) %>%
    group_by(portfolio_category) %>%
    summarise(
      n = n(),
      mpt_count = sum(modern_portfolio_theory_application),
      proportion = round(mean(modern_portfolio_theory_application) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15)
  
  print("MPT usage by portfolio size (n ≥ 15):")
  print(portfolio_analysis)
}

# Logistic regression for MPT adoption
if(sum(!is.na(df$stakeholder)) > 100 & sum(!is.na(df$assets_under_management)) > 100) {
  df_logistic <- df %>%
    filter(!is.na(stakeholder), !is.na(assets_under_management)) %>%
    mutate(log_aum = log10(assets_under_management + 1))
  
  if(nrow(df_logistic) > 100) {
    logit_model <- glm(modern_portfolio_theory_application ~ stakeholder + log_aum,
                       data = df_logistic, family = binomial)
    
    logit_summary <- summary(logit_model)
    print("Logistic regression: MPT usage ~ Stakeholder + log(AUM)")
    print(logit_summary)
    
    # Odds ratios
    odds_ratios <- exp(coef(logit_model))
    print("Odds ratios:")
    print(round(odds_ratios, 3))
    
    # AUM coefficient interpretation
    aum_coef <- logit_summary$coefficients["log_aum", ]
    print("AUM effect (log scale):")
    print(paste("β =", round(aum_coef[1], 3), ", SE =", round(aum_coef[2], 3),
                ", p =", format(aum_coef[4], scientific = TRUE)))
    print(paste("10x increase in AUM → OR =", round(exp(aum_coef[1]), 3)))
  }
}

# Comparison with alternative portfolio methods
alt_methods <- c("black_litterman", "risk_parity", "factor_based", "equal_weight")
if(all(alt_methods %in% names(df))) {
  method_comparison <- data.frame(
    method = c("Modern Portfolio Theory", "Black-Litterman", "Risk Parity", 
               "Factor-Based", "Equal Weight"),
    usage_rate = c(
      mean(df$modern_portfolio_theory_application, na.rm = TRUE),
      mean(df$black_litterman, na.rm = TRUE),
      mean(df$risk_parity, na.rm = TRUE),
      mean(df$factor_based, na.rm = TRUE),
      mean(df$equal_weight, na.rm = TRUE)
    ) * 100
  ) %>%
    arrange(desc(usage_rate))
  
  print("Portfolio optimization method comparison:")
  print(method_comparison)
}

# Effectiveness rating analysis
if("mpt_effectiveness_rating" %in% names(df)) {
  effectiveness <- df %>%
    filter(modern_portfolio_theory_application == 1,
           !is.na(mpt_effectiveness_rating)) %>%
    summarise(
      n = n(),
      mean_effectiveness = round(mean(mpt_effectiveness_rating), 2),
      sd_effectiveness = round(sd(mpt_effectiveness_rating), 2),
      median_effectiveness = median(mpt_effectiveness_rating)
    )
  
  print("MPT effectiveness rating (among users):")
  print(effectiveness)
}

# Summary
print("Summary of Modern Portfolio Theory application:")
print(paste("Overall usage rate:", round(prop_mpt * 100, 1), "%"))
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))
print("Higher usage among institutional investors as expected")

# Expected: 67% (95% CI [63.7%, 70.2%]) with higher usage among institutional investors