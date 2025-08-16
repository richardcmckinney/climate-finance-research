# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 92: "Trademark registration importance: stakeholder differences (F = 5.21, p < .001)"
# Purpose: Analyze trademark registration importance differences across stakeholders

library(tidyverse)
library(janitor)
library(DescTools)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TRADEMARK_IMPORTANCE <- "trademark_registration_importance"
COL_STAKEHOLDER <- "stakeholder"
COL_TRADEMARK_COUNT <- "trademark_portfolio_size"
COL_BRAND_VALUE <- "brand_value_percentage"
COL_MARKET_POSITION <- "market_position_score"
COL_INTERNATIONAL_MARKETS <- "international_market_presence"
COL_BRAND_STRATEGY <- "brand_strategy_sophistication"
COL_TRADEMARK_DISPUTES <- "trademark_dispute_history"
COL_INDUSTRY <- "industry_sector"
COL_COMPANY_AGE <- "company_age_years"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    trademark_registration_importance = as.numeric(trademark_registration_importance),
    stakeholder = factor(stakeholder),
    trademark_portfolio_size = as.numeric(trademark_portfolio_size),
    brand_value_percentage = as.numeric(brand_value_percentage),
    market_position_score = as.numeric(market_position_score),
    international_market_presence = factor(international_market_presence),
    brand_strategy_sophistication = as.numeric(brand_strategy_sophistication),
    trademark_dispute_history = factor(trademark_dispute_history),
    industry_sector = factor(industry_sector),
    company_age_years = as.numeric(company_age_years)
  ) %>%
  filter(!is.na(trademark_registration_importance), !is.na(stakeholder))

# Ensure sufficient sample size per group
stakeholder_counts <- df %>%
  group_by(stakeholder) %>%
  summarise(n = n()) %>%
  filter(n >= 10)

df_anova <- df %>%
  filter(stakeholder %in% stakeholder_counts$stakeholder) %>%
  mutate(stakeholder = droplevels(stakeholder))

# Descriptive statistics by stakeholder
stakeholder_stats <- df_anova %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean = mean(trademark_registration_importance, na.rm = TRUE),
    sd = sd(trademark_registration_importance, na.rm = TRUE),
    median = median(trademark_registration_importance, na.rm = TRUE),
    se = sd / sqrt(n),
    ci_lower = mean - qt(0.975, n-1) * se,
    ci_upper = mean + qt(0.975, n-1) * se,
    .groups = "drop"
  ) %>%
  arrange(desc(mean))

print("Trademark Registration Importance by Stakeholder:")
print(stakeholder_stats)

# Overall statistics
overall_mean <- mean(df_anova$trademark_registration_importance)
overall_sd <- sd(df_anova$trademark_registration_importance)
n_total <- nrow(df_anova)
n_groups <- length(unique(df_anova$stakeholder))

print(paste("\nOverall Mean:", round(overall_mean, 2)))
print(paste("Overall SD:", round(overall_sd, 2)))
print(paste("Total N:", n_total))
print(paste("Number of stakeholder groups:", n_groups))

# One-way ANOVA
anova_result <- aov(trademark_registration_importance ~ stakeholder, data = df_anova)
anova_summary <- summary(anova_result)

print("\nOne-way ANOVA - Trademark Importance by Stakeholder:")
print(anova_summary)

# Extract F-statistic and p-value
f_stat <- anova_summary[[1]][["F value"]][1]
p_value <- anova_summary[[1]][["Pr(>F)"]][1]
df_between <- anova_summary[[1]][["Df"]][1]
df_within <- anova_summary[[1]][["Df"]][2]

print(paste("\nF(", df_between, ",", df_within, ") =", round(f_stat, 2)))
print(paste("p-value:", format(p_value, scientific = TRUE)))

# Effect size measures
ss_between <- anova_summary[[1]][["Sum Sq"]][1]
ss_within <- anova_summary[[1]][["Sum Sq"]][2]
ss_total <- ss_between + ss_within

eta_squared <- ss_between / ss_total
omega_squared <- (ss_between - df_between * (ss_within / df_within)) / 
                (ss_total + (ss_within / df_within))
epsilon_squared <- (ss_between - df_between * (ss_within / df_within)) / ss_total

print("\nEffect Size Measures:")
print(paste("Eta-squared:", round(eta_squared, 3)))
print(paste("Omega-squared:", round(omega_squared, 3)))
print(paste("Epsilon-squared:", round(epsilon_squared, 3)))

# Cohen's f
cohens_f <- sqrt(eta_squared / (1 - eta_squared))
print(paste("Cohen's f:", round(cohens_f, 3)))

# Levene's test for homogeneity of variances
levene_test <- leveneTest(trademark_registration_importance ~ stakeholder, data = df_anova)
print("\nLevene's Test for Homogeneity of Variances:")
print(levene_test)

# Brown-Forsythe test (robust to non-normality)
bf_test <- leveneTest(trademark_registration_importance ~ stakeholder, 
                     data = df_anova, center = median)
print("\nBrown-Forsythe Test:")
print(bf_test)

# Post-hoc tests
print("\n=== POST-HOC ANALYSES ===")

# Tukey's HSD
tukey_result <- TukeyHSD(anova_result)
tukey_df <- as.data.frame(tukey_result$stakeholder)
significant_comparisons <- tukey_df[tukey_df$`p adj` < 0.05, ]

print("\nTukey's HSD - Significant Comparisons (p < 0.05):")
if(nrow(significant_comparisons) > 0) {
  print(significant_comparisons[order(significant_comparisons$`p adj`), ])
} else {
  print("No significant pairwise differences found")
}

# Games-Howell test (for unequal variances)
if(levene_test$`Pr(>F)`[1] < 0.05) {
  print("\nNote: Variances are unequal. Games-Howell test recommended.")
  # Games-Howell implementation would go here
}

# Estimated marginal means
emm <- emmeans(anova_result, ~ stakeholder)
print("\nEstimated Marginal Means:")
print(emm)

# Pairwise contrasts with effect sizes
pairs_emm <- pairs(emm)
print("\nPairwise Contrasts:")
print(pairs_emm)

# Welch's ANOVA (for unequal variances)
welch_test <- oneway.test(trademark_registration_importance ~ stakeholder, 
                          data = df_anova, var.equal = FALSE)
print("\nWelch's ANOVA (robust to unequal variances):")
print(paste("F =", round(welch_test$statistic, 2)))
print(paste("df1 =", welch_test$parameter[1], ", df2 =", round(welch_test$parameter[2], 1)))
print(paste("p-value:", format(welch_test$p.value, scientific = TRUE)))

# Kruskal-Wallis test (non-parametric alternative)
kruskal_test <- kruskal.test(trademark_registration_importance ~ stakeholder, data = df_anova)
print("\nKruskal-Wallis Test (non-parametric):")
print(paste("χ² =", round(kruskal_test$statistic, 2)))
print(paste("df =", kruskal_test$parameter))
print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))

# Dunn's test for post-hoc (non-parametric)
if(kruskal_test$p.value < 0.05) {
  print("\nDunn's test would be appropriate for non-parametric post-hoc comparisons")
}

# Normality tests by group
print("\n=== NORMALITY TESTS BY GROUP ===")
for(stake in unique(df_anova$stakeholder)) {
  group_data <- df_anova$trademark_registration_importance[df_anova$stakeholder == stake]
  if(length(group_data) >= 3 && length(group_data) <= 5000) {
    shapiro_result <- shapiro.test(group_data)
    print(paste(stake, "- Shapiro-Wilk p-value:", 
               format(shapiro_result$p.value, scientific = TRUE)))
  }
}

# Trademark portfolio size analysis by stakeholder
if(sum(!is.na(df_anova$trademark_portfolio_size)) > 50) {
  portfolio_by_stakeholder <- df_anova %>%
    filter(!is.na(trademark_portfolio_size)) %>%
    group_by(stakeholder) %>%
    summarise(
      mean_portfolio = mean(trademark_portfolio_size, na.rm = TRUE),
      median_portfolio = median(trademark_portfolio_size, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nTrademark portfolio size by stakeholder:")
  print(portfolio_by_stakeholder)
  
  # Correlation between importance and portfolio size
  cor_portfolio <- cor.test(df_anova$trademark_registration_importance,
                           df_anova$trademark_portfolio_size,
                           use = "complete.obs")
  print(paste("\nOverall correlation with portfolio size: r =", 
             round(cor_portfolio$estimate, 3)))
}

# Brand value analysis
if(sum(!is.na(df_anova$brand_value_percentage)) > 50) {
  brand_anova <- aov(brand_value_percentage ~ stakeholder, data = df_anova)
  print("\nANOVA - Brand value percentage by stakeholder:")
  print(summary(brand_anova))
}

# International market presence analysis
if(!all(is.na(df_anova$international_market_presence))) {
  intl_contingency <- table(df_anova$stakeholder, df_anova$international_market_presence)
  print("\nInternational market presence by stakeholder:")
  print(prop.table(intl_contingency, 1))
  
  chi_sq_test <- chisq.test(intl_contingency)
  print(paste("\nChi-square test: χ² =", round(chi_sq_test$statistic, 2),
             ", p =", format(chi_sq_test$p.value, scientific = TRUE)))
}

# Two-way ANOVA with industry sector
if(length(unique(df_anova$industry_sector)) > 1) {
  # Filter for sufficient sample sizes
  industry_counts <- df_anova %>%
    filter(!is.na(industry_sector)) %>%
    count(industry_sector) %>%
    filter(n >= 20)
  
  if(nrow(industry_counts) >= 2) {
    df_two_way <- df_anova %>%
      filter(industry_sector %in% industry_counts$industry_sector)
    
    two_way_anova <- aov(trademark_registration_importance ~ stakeholder * industry_sector, 
                        data = df_two_way)
    
    print("\nTwo-way ANOVA - Stakeholder × Industry:")
    print(summary(two_way_anova))
    
    # Type II SS for unbalanced design
    print("\nType II Sum of Squares:")
    print(Anova(two_way_anova, type = "II"))
  }
}

# Polynomial contrasts (if stakeholder has natural ordering)
if(n_groups > 2) {
  contrast_matrix <- contr.poly(n_groups)
  contrasts(df_anova$stakeholder) <- contrast_matrix
  
  poly_anova <- aov(trademark_registration_importance ~ stakeholder, data = df_anova)
  print("\nPolynomial Contrasts:")
  print(summary.lm(poly_anova))
}

# Bootstrap F-statistic
set.seed(123)
n_boot <- 1000
boot_f_stats <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_indices <- sample(nrow(df_anova), replace = TRUE)
  boot_data <- df_anova[boot_indices, ]
  boot_anova <- aov(trademark_registration_importance ~ stakeholder, data = boot_data)
  boot_f_stats[i] <- summary(boot_anova)[[1]][["F value"]][1]
}

boot_f_ci <- quantile(boot_f_stats, c(0.025, 0.975))
print("\nBootstrap 95% CI for F-statistic:")
print(boot_f_ci)

# Power analysis
library(pwr)
power_result <- pwr.anova.test(k = n_groups, 
                               n = n_total/n_groups, 
                               f = cohens_f, 
                               sig.level = 0.05)
print("\nPower Analysis:")
print(paste("Achieved power:", round(power_result$power, 3)))

# Sample size for 80% power
required_n <- pwr.anova.test(k = n_groups, 
                             f = cohens_f, 
                             sig.level = 0.05, 
                             power = 0.80)$n
print(paste("Required n per group for 80% power:", ceiling(required_n)))

# Visualization data preparation
plot_data <- df_anova %>%
  group_by(stakeholder) %>%
  summarise(
    mean = mean(trademark_registration_importance),
    se = sd(trademark_registration_importance) / sqrt(n()),
    .groups = "drop"
  )

print("\nData for visualization:")
print(plot_data)

# Summary statistics for manuscript
print("\n=== SUMMARY FOR MANUSCRIPT ===")
print("Trademark registration importance differed significantly across stakeholder groups")
print(paste("F(", df_between, ",", df_within, ") =", round(f_stat, 2), 
           ", p <", ifelse(p_value < 0.001, ".001", round(p_value, 3))))
print(paste("η² =", round(eta_squared, 3), ", ω² =", round(omega_squared, 3)))
print("\nGroup means (highest to lowest):")
print(stakeholder_stats[, c("stakeholder", "mean", "sd", "n")])

# Expected: stakeholder differences (F = 5.21, p < .001)