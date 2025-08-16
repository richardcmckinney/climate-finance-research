# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 79: "Market multiple applications: EV/Revenue 41%, EV/EBITDA 38%, P/E 21%"
# Purpose: Analyze market multiple usage patterns with confidence intervals

library(tidyverse)
library(janitor)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MULTIPLE_EV_REVENUE <- "multiple_ev_revenue_used"
COL_MULTIPLE_EV_EBITDA <- "multiple_ev_ebitda_used"
COL_MULTIPLE_PE <- "multiple_pe_used"
COL_MULTIPLE_PB <- "multiple_pb_used"
COL_MULTIPLE_PS <- "multiple_ps_used"
COL_PRIMARY_MULTIPLE <- "primary_multiple_type"
COL_STAKEHOLDER <- "stakeholder"
COL_SECTOR_FOCUS <- "sector_focus"
COL_COMPANY_STAGE <- "typical_company_stage"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    multiple_ev_revenue_used = as.integer(multiple_ev_revenue_used),
    multiple_ev_ebitda_used = as.integer(multiple_ev_ebitda_used),
    multiple_pe_used = as.integer(multiple_pe_used),
    multiple_pb_used = as.integer(multiple_pb_used),
    multiple_ps_used = as.integer(multiple_ps_used),
    primary_multiple_type = factor(primary_multiple_type),
    stakeholder = factor(stakeholder),
    sector_focus = factor(sector_focus),
    typical_company_stage = factor(typical_company_stage)
  )

# Calculate proportions for each multiple
# EV/Revenue
n_ev_revenue_valid <- sum(!is.na(df$multiple_ev_revenue_used))
n_ev_revenue_used <- sum(df$multiple_ev_revenue_used, na.rm = TRUE)
prop_ev_revenue <- n_ev_revenue_used / n_ev_revenue_valid
ci_ev_revenue <- BinomCI(n_ev_revenue_used, n_ev_revenue_valid, 
                         conf.level = 0.95, method = "wilson")

print("EV/Revenue Multiple:")
print(paste("Valid responses:", n_ev_revenue_valid))
print(paste("Using EV/Revenue:", n_ev_revenue_used))
print(paste("Proportion:", round(prop_ev_revenue * 100, 1), "%"))
print(paste("95% CI: [", round(ci_ev_revenue[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_ev_revenue[,"upr.ci"] * 100, 1), "%]", sep=""))

# EV/EBITDA
n_ev_ebitda_valid <- sum(!is.na(df$multiple_ev_ebitda_used))
n_ev_ebitda_used <- sum(df$multiple_ev_ebitda_used, na.rm = TRUE)
prop_ev_ebitda <- n_ev_ebitda_used / n_ev_ebitda_valid
ci_ev_ebitda <- BinomCI(n_ev_ebitda_used, n_ev_ebitda_valid, 
                        conf.level = 0.95, method = "wilson")

print("\nEV/EBITDA Multiple:")
print(paste("Valid responses:", n_ev_ebitda_valid))
print(paste("Using EV/EBITDA:", n_ev_ebitda_used))
print(paste("Proportion:", round(prop_ev_ebitda * 100, 1), "%"))
print(paste("95% CI: [", round(ci_ev_ebitda[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_ev_ebitda[,"upr.ci"] * 100, 1), "%]", sep=""))

# P/E
n_pe_valid <- sum(!is.na(df$multiple_pe_used))
n_pe_used <- sum(df$multiple_pe_used, na.rm = TRUE)
prop_pe <- n_pe_used / n_pe_valid
ci_pe <- BinomCI(n_pe_used, n_pe_valid, conf.level = 0.95, method = "wilson")

print("\nP/E Multiple:")
print(paste("Valid responses:", n_pe_valid))
print(paste("Using P/E:", n_pe_used))
print(paste("Proportion:", round(prop_pe * 100, 1), "%"))
print(paste("95% CI: [", round(ci_pe[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_pe[,"upr.ci"] * 100, 1), "%]", sep=""))

# Additional multiples if available
# P/B
if(!all(is.na(df$multiple_pb_used))) {
  n_pb_valid <- sum(!is.na(df$multiple_pb_used))
  n_pb_used <- sum(df$multiple_pb_used, na.rm = TRUE)
  prop_pb <- n_pb_used / n_pb_valid
  ci_pb <- BinomCI(n_pb_used, n_pb_valid, conf.level = 0.95, method = "wilson")
  
  print("\nP/B Multiple:")
  print(paste("Using P/B:", round(prop_pb * 100, 1), "%"))
  print(paste("95% CI: [", round(ci_pb[,"lwr.ci"] * 100, 1), "%, ", 
              round(ci_pb[,"upr.ci"] * 100, 1), "%]", sep=""))
}

# P/S
if(!all(is.na(df$multiple_ps_used))) {
  n_ps_valid <- sum(!is.na(df$multiple_ps_used))
  n_ps_used <- sum(df$multiple_ps_used, na.rm = TRUE)
  prop_ps <- n_ps_used / n_ps_valid
  ci_ps <- BinomCI(n_ps_used, n_ps_valid, conf.level = 0.95, method = "wilson")
  
  print("\nP/S Multiple:")
  print(paste("Using P/S:", round(prop_ps * 100, 1), "%"))
  print(paste("95% CI: [", round(ci_ps[,"lwr.ci"] * 100, 1), "%, ", 
              round(ci_ps[,"upr.ci"] * 100, 1), "%]", sep=""))
}

# Summary comparison
multiple_summary <- data.frame(
  Multiple = c("EV/Revenue", "EV/EBITDA", "P/E"),
  n_valid = c(n_ev_revenue_valid, n_ev_ebitda_valid, n_pe_valid),
  n_using = c(n_ev_revenue_used, n_ev_ebitda_used, n_pe_used),
  Proportion = c(prop_ev_revenue, prop_ev_ebitda, prop_pe) * 100,
  CI_lower = c(ci_ev_revenue[,"lwr.ci"], ci_ev_ebitda[,"lwr.ci"], 
               ci_pe[,"lwr.ci"]) * 100,
  CI_upper = c(ci_ev_revenue[,"upr.ci"], ci_ev_ebitda[,"upr.ci"], 
               ci_pe[,"upr.ci"]) * 100
) %>%
  mutate(
    Proportion = round(Proportion, 1),
    CI_lower = round(CI_lower, 1),
    CI_upper = round(CI_upper, 1),
    CI_text = paste("[", CI_lower, "%, ", CI_upper, "%]", sep="")
  ) %>%
  arrange(desc(Proportion))

print("\nMarket multiple usage summary:")
print(multiple_summary %>% select(Multiple, n_using, Proportion, CI_text))

# Multiple usage patterns
df_multiples <- df %>%
  filter(!is.na(multiple_ev_revenue_used), 
         !is.na(multiple_ev_ebitda_used),
         !is.na(multiple_pe_used)) %>%
  mutate(
    total_multiples = multiple_ev_revenue_used + 
                     multiple_ev_ebitda_used + 
                     multiple_pe_used
  )

multiple_count_dist <- df_multiples %>%
  group_by(total_multiples) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df_multiples) * 100, 1),
    .groups = "drop"
  )

print("\nNumber of multiples used concurrently:")
print(multiple_count_dist)

# Multiple combinations
multiple_combinations <- df_multiples %>%
  mutate(
    combination = case_when(
      total_multiples == 0 ~ "None",
      multiple_ev_revenue_used == 1 & total_multiples == 1 ~ "EV/Revenue only",
      multiple_ev_ebitda_used == 1 & total_multiples == 1 ~ "EV/EBITDA only",
      multiple_pe_used == 1 & total_multiples == 1 ~ "P/E only",
      multiple_ev_revenue_used == 1 & multiple_ev_ebitda_used == 1 & 
        total_multiples == 2 ~ "EV/Revenue + EV/EBITDA",
      multiple_ev_revenue_used == 1 & multiple_pe_used == 1 & 
        total_multiples == 2 ~ "EV/Revenue + P/E",
      multiple_ev_ebitda_used == 1 & multiple_pe_used == 1 & 
        total_multiples == 2 ~ "EV/EBITDA + P/E",
      total_multiples == 3 ~ "All three multiples",
      TRUE ~ "Other"
    )
  ) %>%
  group_by(combination) %>%
  summarise(
    n = n(),
    percentage = round(n() / nrow(df_multiples) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(n))

print("\nMultiple combination patterns:")
print(multiple_combinations)

# Primary multiple analysis
if(!all(is.na(df$primary_multiple_type))) {
  primary_multiple_dist <- df %>%
    filter(!is.na(primary_multiple_type)) %>%
    group_by(primary_multiple_type) %>%
    summarise(
      n = n(),
      percentage = round(n() / sum(!is.na(df$primary_multiple_type)) * 100, 1),
      .groups = "drop"
    ) %>%
    arrange(desc(percentage))
  
  print("\nPrimary multiple preferences:")
  print(primary_multiple_dist)
  
  # Calculate Wilson CIs for primary multiples
  primary_ci <- primary_multiple_dist %>%
    rowwise() %>%
    mutate(
      ci_data = list(BinomCI(n, sum(primary_multiple_dist$n), 
                            conf.level = 0.95, method = "wilson")),
      ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
      ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
      ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
    ) %>%
    select(-ci_data)
  
  print("\nPrimary multiples with 95% Wilson CIs:")
  print(primary_ci %>% select(primary_multiple_type, n, percentage, ci_text))
}

# Analysis by stakeholder
stakeholder_multiples <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    ev_revenue_pct = round(mean(multiple_ev_revenue_used, na.rm = TRUE) * 100, 1),
    ev_ebitda_pct = round(mean(multiple_ev_ebitda_used, na.rm = TRUE) * 100, 1),
    pe_pct = round(mean(multiple_pe_used, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(stakeholder)

print("\nMultiple usage by stakeholder (n ≥ 20):")
print(stakeholder_multiples)

# Chi-square tests for stakeholder differences
for(multiple in c("ev_revenue", "ev_ebitda", "pe")) {
  col_name <- paste0("multiple_", multiple, "_used")
  
  if(col_name %in% names(df)) {
    multiple_table <- table(df$stakeholder[!is.na(df[[col_name]])],
                           df[[col_name]][!is.na(df[[col_name]])])
    
    if(min(dim(multiple_table)) > 1) {
      chi_test <- chisq.test(multiple_table)
      print(paste("\nChi-square test for", multiple, "by stakeholder:"))
      print(paste("χ² =", round(chi_test$statistic, 2),
                  ", p =", format(chi_test$p.value, scientific = TRUE)))
    }
  }
}

# Sector focus analysis
if(!all(is.na(df$sector_focus))) {
  sector_multiples <- df %>%
    filter(!is.na(sector_focus)) %>%
    group_by(sector_focus) %>%
    summarise(
      n = n(),
      ev_revenue_pct = round(mean(multiple_ev_revenue_used, na.rm = TRUE) * 100, 1),
      ev_ebitda_pct = round(mean(multiple_ev_ebitda_used, na.rm = TRUE) * 100, 1),
      pe_pct = round(mean(multiple_pe_used, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(ev_revenue_pct)) %>%
    head(10)
  
  print("\nTop 10 sectors by EV/Revenue usage (n ≥ 15):")
  print(sector_multiples)
}

# Company stage analysis
if(!all(is.na(df$typical_company_stage))) {
  stage_multiples <- df %>%
    filter(!is.na(typical_company_stage)) %>%
    group_by(typical_company_stage) %>%
    summarise(
      n = n(),
      ev_revenue_pct = round(mean(multiple_ev_revenue_used, na.rm = TRUE) * 100, 1),
      ev_ebitda_pct = round(mean(multiple_ev_ebitda_used, na.rm = TRUE) * 100, 1),
      pe_pct = round(mean(multiple_pe_used, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(typical_company_stage)
  
  print("\nMultiple usage by company stage (n ≥ 15):")
  print(stage_multiples)
  
  # Test for trends across stages
  print("\nCorrelations with company stage (ordinal):")
  
  df_stage <- df %>%
    filter(!is.na(typical_company_stage)) %>%
    mutate(stage_numeric = as.numeric(factor(typical_company_stage)))
  
  # EV/Revenue vs stage
  cor_ev_revenue_stage <- cor.test(df_stage$multiple_ev_revenue_used[!is.na(df_stage$multiple_ev_revenue_used)],
                                   df_stage$stage_numeric[!is.na(df_stage$multiple_ev_revenue_used)],
                                   method = "spearman")
  print(paste("EV/Revenue × Stage: ρ =", round(cor_ev_revenue_stage$estimate, 3),
              ", p =", format(cor_ev_revenue_stage$p.value, scientific = TRUE)))
  
  # EV/EBITDA vs stage
  cor_ev_ebitda_stage <- cor.test(df_stage$multiple_ev_ebitda_used[!is.na(df_stage$multiple_ev_ebitda_used)],
                                  df_stage$stage_numeric[!is.na(df_stage$multiple_ev_ebitda_used)],
                                  method = "spearman")
  print(paste("EV/EBITDA × Stage: ρ =", round(cor_ev_ebitda_stage$estimate, 3),
              ", p =", format(cor_ev_ebitda_stage$p.value, scientific = TRUE)))
  
  # P/E vs stage
  cor_pe_stage <- cor.test(df_stage$multiple_pe_used[!is.na(df_stage$multiple_pe_used)],
                          df_stage$stage_numeric[!is.na(df_stage$multiple_pe_used)],
                          method = "spearman")
  print(paste("P/E × Stage: ρ =", round(cor_pe_stage$estimate, 3),
              ", p =", format(cor_pe_stage$p.value, scientific = TRUE)))
}

# Multiple correlations
if(nrow(df_multiples) > 50) {
  multiple_cors <- cor(df_multiples[, c("multiple_ev_revenue_used", 
                                        "multiple_ev_ebitda_used",
                                        "multiple_pe_used")],
                      use = "complete.obs")
  
  print("\nMultiple usage correlations:")
  print(round(multiple_cors, 3))
  
  # Test significance
  print("\nCorrelation significance tests:")
  
  # EV/Revenue vs EV/EBITDA
  cor_ev_rev_ebitda <- cor.test(df$multiple_ev_revenue_used, df$multiple_ev_ebitda_used)
  print(paste("EV/Revenue × EV/EBITDA: r =", round(cor_ev_rev_ebitda$estimate, 3),
              ", p =", format(cor_ev_rev_ebitda$p.value, scientific = TRUE)))
  
  # EV/Revenue vs P/E
  cor_ev_rev_pe <- cor.test(df$multiple_ev_revenue_used, df$multiple_pe_used)
  print(paste("EV/Revenue × P/E: r =", round(cor_ev_rev_pe$estimate, 3),
              ", p =", format(cor_ev_rev_pe$p.value, scientific = TRUE)))
  
  # EV/EBITDA vs P/E
  cor_ev_ebitda_pe <- cor.test(df$multiple_ev_ebitda_used, df$multiple_pe_used)
  print(paste("EV/EBITDA × P/E: r =", round(cor_ev_ebitda_pe$estimate, 3),
              ", p =", format(cor_ev_ebitda_pe$p.value, scientific = TRUE)))
}

# Logistic regression for each multiple
if(nrow(df) > 150) {
  # EV/Revenue model
  df_ev_revenue_logit <- df %>%
    filter(!is.na(multiple_ev_revenue_used), !is.na(stakeholder))
  
  if(nrow(df_ev_revenue_logit) > 100) {
    predictors <- c("stakeholder")
    
    if(sum(!is.na(df_ev_revenue_logit$typical_company_stage)) > 80) {
      predictors <- c(predictors, "typical_company_stage")
    }
    
    formula_str <- paste("multiple_ev_revenue_used ~", paste(predictors, collapse = " + "))
    ev_revenue_model <- glm(as.formula(formula_str), 
                           data = df_ev_revenue_logit, family = binomial)
    
    print("\nLogistic regression - EV/Revenue usage predictors:")
    print(summary(ev_revenue_model))
    
    # Odds ratios
    odds_ratios <- exp(coef(ev_revenue_model))
    print("Odds ratios:")
    print(round(odds_ratios, 3))
  }
}

# McNemar's test for paired comparisons
# EV/Revenue vs EV/EBITDA
if(sum(!is.na(df$multiple_ev_revenue_used) & !is.na(df$multiple_ev_ebitda_used)) > 50) {
  mcnemar_table_1 <- table(df$multiple_ev_revenue_used, df$multiple_ev_ebitda_used)
  mcnemar_test_1 <- mcnemar.test(mcnemar_table_1)
  
  print("\nMcNemar's test - EV/Revenue vs EV/EBITDA:")
  print(paste("χ² =", round(mcnemar_test_1$statistic, 2)))
  print(paste("p-value:", format(mcnemar_test_1$p.value, scientific = TRUE)))
}

# EV/Revenue vs P/E
if(sum(!is.na(df$multiple_ev_revenue_used) & !is.na(df$multiple_pe_used)) > 50) {
  mcnemar_table_2 <- table(df$multiple_ev_revenue_used, df$multiple_pe_used)
  mcnemar_test_2 <- mcnemar.test(mcnemar_table_2)
  
  print("\nMcNemar's test - EV/Revenue vs P/E:")
  print(paste("χ² =", round(mcnemar_test_2$statistic, 2)))
  print(paste("p-value:", format(mcnemar_test_2$p.value, scientific = TRUE)))
}

# EV/EBITDA vs P/E
if(sum(!is.na(df$multiple_ev_ebitda_used) & !is.na(df$multiple_pe_used)) > 50) {
  mcnemar_table_3 <- table(df$multiple_ev_ebitda_used, df$multiple_pe_used)
  mcnemar_test_3 <- mcnemar.test(mcnemar_table_3)
  
  print("\nMcNemar's test - EV/EBITDA vs P/E:")
  print(paste("χ² =", round(mcnemar_test_3$statistic, 2)))
  print(paste("p-value:", format(mcnemar_test_3$p.value, scientific = TRUE)))
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
      ev_revenue_pct = round(mean(multiple_ev_revenue_used, na.rm = TRUE) * 100, 1),
      ev_ebitda_pct = round(mean(multiple_ev_ebitda_used, na.rm = TRUE) * 100, 1),
      pe_pct = round(mean(multiple_pe_used, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(response_quarter)
  
  if(nrow(df_time) > 3) {
    print("\nMultiple usage trends over time:")
    print(df_time)
  }
}

# Summary of key findings
print("\nSummary of market multiple applications:")
print(paste("EV/Revenue:", round(prop_ev_revenue * 100, 1), "% (95% CI: [", 
            round(ci_ev_revenue[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_ev_revenue[,"upr.ci"] * 100, 1), "%])"))
print(paste("EV/EBITDA:", round(prop_ev_ebitda * 100, 1), "% (95% CI: [", 
            round(ci_ev_ebitda[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_ev_ebitda[,"upr.ci"] * 100, 1), "%])"))
print(paste("P/E:", round(prop_pe * 100, 1), "% (95% CI: [", 
            round(ci_pe[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_pe[,"upr.ci"] * 100, 1), "%])"))

# Expected: EV/Revenue 41%, EV/EBITDA 38%, P/E 21%