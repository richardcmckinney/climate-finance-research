# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 87: "Jurisdictional differences: significant variance (F = 4.32, p < .001)"
# Purpose: Analyze jurisdictional differences in regulatory and operational metrics

library(tidyverse)
library(janitor)
library(DescTools)
library(car)
library(effectsize)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_JURISDICTION <- "jurisdiction"
COL_REGULATORY_SCORE <- "regulatory_environment_score"
COL_OPERATIONAL_EFFICIENCY <- "operational_efficiency_score"
COL_COMPLIANCE_BURDEN <- "compliance_burden_score"
COL_MARKET_ACCESS <- "market_access_score"
COL_STAKEHOLDER <- "stakeholder"
COL_JURISDICTION_TYPE <- "jurisdiction_type"
COL_REGULATORY_COMPLEXITY <- "regulatory_complexity_index"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    jurisdiction = factor(jurisdiction),
    regulatory_environment_score = as.numeric(regulatory_environment_score),
    operational_efficiency_score = as.numeric(operational_efficiency_score),
    compliance_burden_score = as.numeric(compliance_burden_score),
    market_access_score = as.numeric(market_access_score),
    stakeholder = factor(stakeholder),
    jurisdiction_type = factor(jurisdiction_type),
    regulatory_complexity_index = as.numeric(regulatory_complexity_index)
  ) %>%
  filter(!is.na(jurisdiction), !is.na(regulatory_environment_score))

# Filter to jurisdictions with sufficient sample size
jurisdiction_counts <- df %>%
  group_by(jurisdiction) %>%
  summarise(n = n()) %>%
  filter(n >= 10)

df_anova <- df %>%
  filter(jurisdiction %in% jurisdiction_counts$jurisdiction) %>%
  mutate(jurisdiction = droplevels(jurisdiction))

# Primary ANOVA for regulatory environment score
n_jurisdictions <- length(unique(df_anova$jurisdiction))
n_total <- nrow(df_anova)

print("Jurisdictional differences analysis:")
print(paste("Number of jurisdictions:", n_jurisdictions))
print(paste("Total observations:", n_total))

# Descriptive statistics by jurisdiction
jurisdiction_stats <- df_anova %>%
  group_by(jurisdiction) %>%
  summarise(
    n = n(),
    mean_regulatory = round(mean(regulatory_environment_score), 2),
    sd_regulatory = round(sd(regulatory_environment_score), 2),
    median_regulatory = round(median(regulatory_environment_score), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_regulatory))

print("\nRegulatory environment by jurisdiction:")
print(head(jurisdiction_stats, 15))

# One-way ANOVA
anova_regulatory <- aov(regulatory_environment_score ~ jurisdiction, data = df_anova)
anova_summary <- summary(anova_regulatory)

print("\nOne-way ANOVA - Regulatory environment by jurisdiction:")
print(anova_summary)

# Extract F-statistic and p-value
f_stat <- anova_summary[[1]][["F value"]][1]
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
df_between <- anova_summary[[1]][["Df"]][1]
df_within <- anova_summary[[1]][["Df"]][2]

print(paste("F(", df_between, ",", df_within, ") =", round(f_stat, 2)))
print(paste("p-value:", format(p_value, scientific = TRUE)))

# Effect size (eta-squared and omega-squared)
ss_between <- anova_summary[[1]][["Sum Sq"]][1]
ss_within <- anova_summary[[1]][["Sum Sq"]][2]
ss_total <- ss_between + ss_within

eta_squared <- ss_between / ss_total
omega_squared <- (ss_between - df_between * (ss_within / df_within)) / 
                (ss_total + (ss_within / df_within))

print(paste("Effect size - Eta-squared:", round(eta_squared, 3)))
print(paste("Effect size - Omega-squared:", round(omega_squared, 3)))

# Levene's test for homogeneity of variances
levene_test <- leveneTest(regulatory_environment_score ~ jurisdiction, data = df_anova)
print("\nLevene's test for homogeneity of variances:")
print(levene_test)

# Post-hoc tests if ANOVA is significant
if(p_value < 0.05) {
  # Tukey's HSD
  tukey_test <- TukeyHSD(anova_regulatory)
  tukey_results <- as.data.frame(tukey_test$jurisdiction)
  
  # Show only significant differences
  significant_diffs <- tukey_results[tukey_results$`p adj` < 0.05, ]
  
  if(nrow(significant_diffs) > 0) {
    print("\nSignificant pairwise differences (Tukey's HSD, p < 0.05):")
    print(head(significant_diffs[order(significant_diffs$`p adj`), ], 10))
  }
  
  # Games-Howell test (for unequal variances)
  if(levene_test$`Pr(>F)`[1] < 0.05) {
    print("\nNote: Variances are unequal. Consider Games-Howell test for post-hoc comparisons.")
  }
}

# Operational efficiency ANOVA
if(sum(!is.na(df_anova$operational_efficiency_score)) > 100) {
  anova_operational <- aov(operational_efficiency_score ~ jurisdiction, data = df_anova)
  anova_op_summary <- summary(anova_operational)
  
  print("\nANOVA - Operational efficiency by jurisdiction:")
  print(anova_op_summary)
  
  f_stat_op <- anova_op_summary[[1]][["F value"]][1]
  p_value_op <- anova_op_summary[[1]][["Pr(>F)"]][1]
  
  print(paste("F =", round(f_stat_op, 2), ", p =", format(p_value_op, scientific = TRUE)))
}

# Compliance burden ANOVA
if(sum(!is.na(df_anova$compliance_burden_score)) > 100) {
  anova_compliance <- aov(compliance_burden_score ~ jurisdiction, data = df_anova)
  anova_comp_summary <- summary(anova_compliance)
  
  print("\nANOVA - Compliance burden by jurisdiction:")
  print(anova_comp_summary)
  
  f_stat_comp <- anova_comp_summary[[1]][["F value"]][1]
  p_value_comp <- anova_comp_summary[[1]][["Pr(>F)"]][1]
  
  print(paste("F =", round(f_stat_comp, 2), ", p =", format(p_value_comp, scientific = TRUE)))
}

# Market access ANOVA
if(sum(!is.na(df_anova$market_access_score)) > 100) {
  anova_market <- aov(market_access_score ~ jurisdiction, data = df_anova)
  anova_market_summary <- summary(anova_market)
  
  print("\nANOVA - Market access by jurisdiction:")
  print(anova_market_summary)
  
  f_stat_market <- anova_market_summary[[1]][["F value"]][1]
  p_value_market <- anova_market_summary[[1]][["Pr(>F)"]][1]
  
  print(paste("F =", round(f_stat_market, 2), ", p =", format(p_value_market, scientific = TRUE)))
}

# MANOVA for multiple dependent variables
if(sum(!is.na(df_anova$operational_efficiency_score)) > 100 &&
   sum(!is.na(df_anova$compliance_burden_score)) > 100) {
  
  df_manova <- df_anova %>%
    filter(!is.na(operational_efficiency_score), 
           !is.na(compliance_burden_score))
  
  if(nrow(df_manova) > 100) {
    manova_model <- manova(cbind(regulatory_environment_score, 
                                operational_efficiency_score,
                                compliance_burden_score) ~ jurisdiction, 
                          data = df_manova)
    
    print("\nMANOVA - Multiple metrics by jurisdiction:")
    print(summary(manova_model))
    
    # Pillai's trace
    manova_summary <- summary(manova_model, test = "Pillai")
    print("\nPillai's trace:")
    print(manova_summary)
  }
}

# Jurisdiction type analysis (if available)
if(!all(is.na(df$jurisdiction_type))) {
  type_analysis <- df %>%
    filter(!is.na(jurisdiction_type)) %>%
    group_by(jurisdiction_type) %>%
    summarise(
      n = n(),
      mean_regulatory = round(mean(regulatory_environment_score), 2),
      sd_regulatory = round(sd(regulatory_environment_score), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 30)
  
  print("\nRegulatory environment by jurisdiction type:")
  print(type_analysis)
  
  if(nrow(type_analysis) > 1) {
    df_type <- df %>%
      filter(!is.na(jurisdiction_type), jurisdiction_type %in% type_analysis$jurisdiction_type)
    
    anova_type <- aov(regulatory_environment_score ~ jurisdiction_type, data = df_type)
    print("\nANOVA - By jurisdiction type:")
    print(summary(anova_type))
  }
}

# Regulatory complexity analysis
if(sum(!is.na(df$regulatory_complexity_index)) > 100) {
  complexity_by_jurisdiction <- df_anova %>%
    filter(!is.na(regulatory_complexity_index)) %>%
    group_by(jurisdiction) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(regulatory_complexity_index), 2),
      sd_complexity = round(sd(regulatory_complexity_index), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_complexity))
  
  print("\nRegulatory complexity by jurisdiction:")
  print(head(complexity_by_jurisdiction, 10))
  
  # Correlation between regulatory score and complexity
  cor_complexity <- cor.test(df$regulatory_environment_score[!is.na(df$regulatory_complexity_index)],
                             df$regulatory_complexity_index[!is.na(df$regulatory_complexity_index)])
  
  print("\nRegulatory environment × Complexity correlation:")
  print(paste("r =", round(cor_complexity$estimate, 3)))
  print(paste("p-value:", format(cor_complexity$p.value, scientific = TRUE)))
}

# Two-way ANOVA with stakeholder
if(length(unique(df_anova$stakeholder)) > 1) {
  # Ensure balanced design or use Type II/III SS
  two_way_anova <- aov(regulatory_environment_score ~ jurisdiction * stakeholder, 
                       data = df_anova)
  
  print("\nTwo-way ANOVA - Jurisdiction × Stakeholder:")
  print(summary(two_way_anova))
  
  # Type II SS for unbalanced design
  if(require(car, quietly = TRUE)) {
    print("\nType II Sum of Squares:")
    print(Anova(two_way_anova, type = "II"))
  }
}

# Kruskal-Wallis test (non-parametric alternative)
kruskal_test <- kruskal.test(regulatory_environment_score ~ jurisdiction, data = df_anova)
print("\nKruskal-Wallis test (non-parametric):")
print(paste("χ² =", round(kruskal_test$statistic, 2)))
print(paste("df =", kruskal_test$parameter))
print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))

# Welch's ANOVA (for unequal variances)
if(levene_test$`Pr(>F)`[1] < 0.05) {
  welch_test <- oneway.test(regulatory_environment_score ~ jurisdiction, 
                            data = df_anova, var.equal = FALSE)
  print("\nWelch's ANOVA (for unequal variances):")
  print(paste("F =", round(welch_test$statistic, 2)))
  print(paste("df1 =", welch_test$parameter[1], ", df2 =", round(welch_test$parameter[2], 1)))
  print(paste("p-value:", format(welch_test$p.value, scientific = TRUE)))
}

# Regional grouping analysis
if("region" %in% names(df)) {
  regional_analysis <- df %>%
    filter(!is.na(region)) %>%
    group_by(region) %>%
    summarise(
      n_jurisdictions = n_distinct(jurisdiction),
      n_observations = n(),
      mean_regulatory = round(mean(regulatory_environment_score), 2),
      sd_regulatory = round(sd(regulatory_environment_score), 2),
      .groups = "drop"
    ) %>%
    filter(n_observations >= 50)
  
  print("\nRegional analysis:")
  print(regional_analysis)
  
  if(nrow(regional_analysis) > 1) {
    df_region <- df %>%
      filter(!is.na(region), region %in% regional_analysis$region)
    
    anova_region <- aov(regulatory_environment_score ~ region, data = df_region)
    print("\nANOVA - By region:")
    print(summary(anova_region))
  }
}

# Variance components analysis
variance_components <- df_anova %>%
  summarise(
    total_variance = var(regulatory_environment_score),
    within_jurisdiction_variance = mean(tapply(regulatory_environment_score, 
                                              jurisdiction, var)),
    between_jurisdiction_variance = var(tapply(regulatory_environment_score, 
                                              jurisdiction, mean))
  )

print("\nVariance components:")
print(paste("Total variance:", round(variance_components$total_variance, 2)))
print(paste("Within-jurisdiction variance:", round(variance_components$within_jurisdiction_variance, 2)))
print(paste("Between-jurisdiction variance:", round(variance_components$between_jurisdiction_variance, 2)))

intraclass_correlation <- variance_components$between_jurisdiction_variance / 
                         variance_components$total_variance
print(paste("Intraclass correlation coefficient:", round(intraclass_correlation, 3)))

# Summary of key findings
print("\nSummary of jurisdictional differences:")
print(paste("F-statistic:", round(f_stat, 2)))
print(paste("p-value:", format(p_value, scientific = TRUE)))
print(paste("Number of jurisdictions analyzed:", n_jurisdictions))
print(paste("Effect size (eta-squared):", round(eta_squared, 3)))

# Expected: significant variance (F = 4.32, p < .001)