# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 96: "University collaboration models: satisfaction score (M = 3.78, SD = 0.89)"
# Purpose: Analyze satisfaction scores for university collaboration models

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)
library(lavaan)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_COLLABORATION_SATISFACTION <- "university_collaboration_satisfaction"
COL_COLLABORATION_TYPE <- "collaboration_model_type"
COL_RESEARCH_OUTPUT <- "research_output_quality"
COL_KNOWLEDGE_TRANSFER <- "knowledge_transfer_effectiveness"
COL_COMMUNICATION_QUALITY <- "communication_quality_score"
COL_IP_MANAGEMENT <- "ip_management_satisfaction"
COL_FUNDING_ADEQUACY <- "funding_adequacy_score"
COL_PROJECT_DURATION <- "collaboration_duration_months"
COL_PUBLICATION_COUNT <- "joint_publication_count"
COL_PATENT_APPLICATIONS <- "joint_patent_applications"
COL_UNIVERSITY_RANKING <- "university_ranking_tier"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    university_collaboration_satisfaction = as.numeric(university_collaboration_satisfaction),
    collaboration_model_type = factor(collaboration_model_type),
    research_output_quality = as.numeric(research_output_quality),
    knowledge_transfer_effectiveness = as.numeric(knowledge_transfer_effectiveness),
    communication_quality_score = as.numeric(communication_quality_score),
    ip_management_satisfaction = as.numeric(ip_management_satisfaction),
    funding_adequacy_score = as.numeric(funding_adequacy_score),
    collaboration_duration_months = as.numeric(collaboration_duration_months),
    joint_publication_count = as.numeric(joint_publication_count),
    joint_patent_applications = as.numeric(joint_patent_applications),
    university_ranking_tier = factor(university_ranking_tier,
                                    levels = c("Top 50", "Top 100", "Top 200", "Other"),
                                    ordered = TRUE),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(university_collaboration_satisfaction))

# Primary descriptive statistics
n_total <- nrow(df)
mean_satisfaction <- mean(df$university_collaboration_satisfaction, na.rm = TRUE)
sd_satisfaction <- sd(df$university_collaboration_satisfaction, na.rm = TRUE)
median_satisfaction <- median(df$university_collaboration_satisfaction, na.rm = TRUE)
se_satisfaction <- sd_satisfaction / sqrt(n_total)

print("University Collaboration Satisfaction - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_satisfaction, 2)))
print(paste("SD =", round(sd_satisfaction, 2)))
print(paste("Median =", round(median_satisfaction, 2)))
print(paste("SE =", round(se_satisfaction, 3)))

# Confidence intervals
ci_lower <- mean_satisfaction - qt(0.975, n_total - 1) * se_satisfaction
ci_upper <- mean_satisfaction + qt(0.975, n_total - 1) * se_satisfaction

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$university_collaboration_satisfaction)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution analysis
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$university_collaboration_satisfaction, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$university_collaboration_satisfaction, na.rm = TRUE), 3)))

# Percentiles
percentiles <- quantile(df$university_collaboration_satisfaction, 
                       probs = c(0.05, 0.10, 0.25, 0.50, 0.75, 0.90, 0.95), 
                       na.rm = TRUE)
print("\nPercentiles:")
print(round(percentiles, 2))

# Coefficient of variation
cv <- (sd_satisfaction / mean_satisfaction) * 100
print(paste("\nCoefficient of variation:", round(cv, 1), "%"))

# Normality tests
shapiro_result <- shapiro.test(df$university_collaboration_satisfaction[1:min(5000, n_total)])
print("\nShapiro-Wilk normality test:")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint
scale_midpoint <- 3
t_test_midpoint <- t.test(df$university_collaboration_satisfaction, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_satisfaction - scale_midpoint, 2)))

# Bootstrap analysis
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$university_collaboration_satisfaction, 
                       size = n_total, 
                       replace = TRUE)
  boot_means[i] <- mean(boot_sample)
  boot_sds[i] <- sd(boot_sample)
}

boot_ci_mean <- quantile(boot_means, c(0.025, 0.975))
boot_ci_sd <- quantile(boot_sds, c(0.025, 0.975))

print("\nBootstrap 95% CI for mean:")
print(round(boot_ci_mean, 3))
print("Bootstrap 95% CI for SD:")
print(round(boot_ci_sd, 3))

# Analysis by collaboration model type
if(!all(is.na(df$collaboration_model_type))) {
  model_stats <- df %>%
    filter(!is.na(collaboration_model_type)) %>%
    group_by(collaboration_model_type) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(university_collaboration_satisfaction, na.rm = TRUE), 2),
      sd_satisfaction = round(sd(university_collaboration_satisfaction, na.rm = TRUE), 2),
      median_satisfaction = round(median(university_collaboration_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_satisfaction))
  
  print("\nSatisfaction by collaboration model type:")
  print(model_stats)
  
  # ANOVA if multiple types
  if(nrow(model_stats) > 2) {
    anova_model <- aov(university_collaboration_satisfaction ~ collaboration_model_type, 
                      data = df %>% filter(collaboration_model_type %in% model_stats$collaboration_model_type))
    print("\nANOVA by collaboration model:")
    print(summary(anova_model))
  }
}

# Correlation with research output quality
if(sum(!is.na(df$research_output_quality)) > 30) {
  cor_output <- cor.test(df$university_collaboration_satisfaction,
                        df$research_output_quality,
                        use = "complete.obs")
  
  print("\nCorrelation with research output quality:")
  print(paste("r =", round(cor_output$estimate, 3)))
  print(paste("95% CI: [", round(cor_output$conf.int[1], 3), ",", 
             round(cor_output$conf.int[2], 3), "]"))
  print(paste("p-value:", format(cor_output$p.value, scientific = TRUE)))
}

# Knowledge transfer effectiveness
if(sum(!is.na(df$knowledge_transfer_effectiveness)) > 30) {
  cor_knowledge <- cor.test(df$university_collaboration_satisfaction,
                           df$knowledge_transfer_effectiveness,
                           use = "complete.obs")
  
  print("\nCorrelation with knowledge transfer effectiveness:")
  print(paste("r =", round(cor_knowledge$estimate, 3)))
  print(paste("p-value:", format(cor_knowledge$p.value, scientific = TRUE)))
}

# Communication quality analysis
if(sum(!is.na(df$communication_quality_score)) > 30) {
  cor_communication <- cor.test(df$university_collaboration_satisfaction,
                               df$communication_quality_score,
                               use = "complete.obs")
  
  print("\nCorrelation with communication quality:")
  print(paste("r =", round(cor_communication$estimate, 3)))
  print(paste("p-value:", format(cor_communication$p.value, scientific = TRUE)))
  
  # Regression analysis
  lm_communication <- lm(university_collaboration_satisfaction ~ communication_quality_score, 
                        data = df)
  print("\nRegression - Satisfaction predicted by communication quality:")
  print(summary(lm_communication))
}

# IP management satisfaction
if(sum(!is.na(df$ip_management_satisfaction)) > 30) {
  cor_ip <- cor.test(df$university_collaboration_satisfaction,
                    df$ip_management_satisfaction,
                    use = "complete.obs")
  
  print("\nCorrelation with IP management satisfaction:")
  print(paste("r =", round(cor_ip$estimate, 3)))
  print(paste("p-value:", format(cor_ip$p.value, scientific = TRUE)))
}

# Funding adequacy analysis
if(sum(!is.na(df$funding_adequacy_score)) > 30) {
  cor_funding <- cor.test(df$university_collaboration_satisfaction,
                         df$funding_adequacy_score,
                         use = "complete.obs")
  
  print("\nCorrelation with funding adequacy:")
  print(paste("r =", round(cor_funding$estimate, 3)))
  print(paste("p-value:", format(cor_funding$p.value, scientific = TRUE)))
}

# Duration analysis
if(sum(!is.na(df$collaboration_duration_months)) > 30) {
  # Create duration categories
  df <- df %>%
    mutate(duration_category = case_when(
      collaboration_duration_months <= 12 ~ "Short (≤1 year)",
      collaboration_duration_months <= 36 ~ "Medium (1-3 years)",
      collaboration_duration_months <= 60 ~ "Long (3-5 years)",
      TRUE ~ "Very long (>5 years)"
    ))
  
  duration_stats <- df %>%
    filter(!is.na(duration_category)) %>%
    group_by(duration_category) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(university_collaboration_satisfaction, na.rm = TRUE), 2),
      sd_satisfaction = round(sd(university_collaboration_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nSatisfaction by collaboration duration:")
  print(duration_stats)
  
  # Correlation with continuous duration
  cor_duration <- cor.test(df$university_collaboration_satisfaction,
                          df$collaboration_duration_months,
                          use = "complete.obs",
                          method = "spearman")
  
  print("\nSpearman correlation with duration:")
  print(paste("rho =", round(cor_duration$estimate, 3)))
  print(paste("p-value:", format(cor_duration$p.value, scientific = TRUE)))
}

# Publication output analysis
if(sum(!is.na(df$joint_publication_count)) > 30) {
  # Log transform publication count
  df$log_publications <- log1p(df$joint_publication_count)
  
  cor_publications <- cor.test(df$university_collaboration_satisfaction,
                              df$log_publications,
                              use = "complete.obs")
  
  print("\nCorrelation with log(publication count + 1):")
  print(paste("r =", round(cor_publications$estimate, 3)))
  print(paste("p-value:", format(cor_publications$p.value, scientific = TRUE)))
  
  # Categorize publication output
  df <- df %>%
    mutate(publication_category = case_when(
      joint_publication_count == 0 ~ "No publications",
      joint_publication_count <= 2 ~ "1-2 publications",
      joint_publication_count <= 5 ~ "3-5 publications",
      TRUE ~ "6+ publications"
    ))
  
  pub_stats <- df %>%
    filter(!is.na(publication_category)) %>%
    group_by(publication_category) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(university_collaboration_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nSatisfaction by publication output:")
  print(pub_stats)
}

# Patent applications analysis
if(sum(!is.na(df$joint_patent_applications)) > 30) {
  df <- df %>%
    mutate(has_patents = ifelse(joint_patent_applications > 0, "Has patents", "No patents"))
  
  patent_comparison <- df %>%
    filter(!is.na(has_patents)) %>%
    group_by(has_patents) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(university_collaboration_satisfaction, na.rm = TRUE), 2),
      sd_satisfaction = round(sd(university_collaboration_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nSatisfaction by patent output:")
  print(patent_comparison)
  
  # t-test
  if(sum(df$has_patents == "Has patents", na.rm = TRUE) >= 10) {
    t_test_patents <- t.test(university_collaboration_satisfaction ~ has_patents, data = df)
    print(paste("t-test: t =", round(t_test_patents$statistic, 2)))
    print(paste("p-value:", format(t_test_patents$p.value, scientific = TRUE)))
  }
}

# University ranking analysis
if(!all(is.na(df$university_ranking_tier))) {
  ranking_stats <- df %>%
    filter(!is.na(university_ranking_tier)) %>%
    group_by(university_ranking_tier) %>%
    summarise(
      n = n(),
      mean_satisfaction = round(mean(university_collaboration_satisfaction, na.rm = TRUE), 2),
      sd_satisfaction = round(sd(university_collaboration_satisfaction, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nSatisfaction by university ranking tier:")
  print(ranking_stats)
  
  # Jonckheere-Terpstra test for trend
  if(nrow(ranking_stats) > 2) {
    jt_test <- jonckheere.test(df$university_collaboration_satisfaction,
                               df$university_ranking_tier,
                               alternative = "decreasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# Multiple regression model
predictors <- c("research_output_quality", "knowledge_transfer_effectiveness",
               "communication_quality_score", "ip_management_satisfaction", 
               "funding_adequacy_score")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 3) {
  formula_str <- paste("university_collaboration_satisfaction ~", 
                      paste(available_predictors, collapse = " + "))
  
  lm_full <- lm(as.formula(formula_str), data = df)
  
  print("\nMultiple regression model:")
  print(summary(lm_full))
  
  # Standardized coefficients
  df_std <- df %>%
    select(all_of(c("university_collaboration_satisfaction", available_predictors))) %>%
    na.omit() %>%
    mutate(across(everything(), scale))
  
  lm_std <- lm(as.formula(formula_str), data = df_std)
  
  print("\nStandardized coefficients:")
  print(round(coef(lm_std), 3))
  
  # VIF for multicollinearity
  library(car)
  vif_values <- vif(lm_full)
  print("\nVariance Inflation Factors:")
  print(round(vif_values, 2))
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_satisfaction = round(mean(university_collaboration_satisfaction, na.rm = TRUE), 2),
    sd_satisfaction = round(sd(university_collaboration_satisfaction, na.rm = TRUE), 2),
    se_satisfaction = round(sd_satisfaction / sqrt(n), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_satisfaction))

print("\nCollaboration satisfaction by stakeholder:")
print(stakeholder_stats)

# Outlier analysis
z_scores <- abs(scale(df$university_collaboration_satisfaction))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

# Robust statistics
mad_value <- mad(df$university_collaboration_satisfaction, na.rm = TRUE)
trimmed_mean <- mean(df$university_collaboration_satisfaction, trim = 0.1, na.rm = TRUE)
winsorized_mean <- mean(Winsorize(df$university_collaboration_satisfaction, probs = c(0.05, 0.95)))

print(paste("\nMedian Absolute Deviation (MAD):", round(mad_value, 3)))
print(paste("10% Trimmed mean:", round(trimmed_mean, 2)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 2)))

# Response distribution
freq_table <- table(round(df$university_collaboration_satisfaction, 1))
print("\nFrequency distribution:")
print(freq_table)
print("\nProportions:")
print(round(prop.table(freq_table), 3))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("University collaboration satisfaction: M =", round(mean_satisfaction, 2), 
           ", SD =", round(sd_satisfaction, 2)))
print(paste("N =", n_total))
print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("The mean of", round(mean_satisfaction, 2), "indicates moderately high satisfaction"))
print(paste("with university collaboration models (SD =", round(sd_satisfaction, 2), ")"))

# Expected: satisfaction score (M = 3.78, SD = 0.89)