# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 88: "Cross-border considerations: regulatory complexity (t = 3.87, p < .001)"
# Purpose: Analyze cross-border regulatory complexity differences

library(tidyverse)
library(janitor)
library(DescTools)
library(car)
library(effectsize)
library(pwr)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_CROSS_BORDER <- "cross_border_operations"
COL_REGULATORY_COMPLEXITY <- "regulatory_complexity_index"
COL_COMPLIANCE_COST <- "compliance_cost_percentage"
COL_MARKET_ENTRY_TIME <- "market_entry_time_months"
COL_REGULATORY_BARRIERS <- "regulatory_barrier_score"
COL_JURISDICTION_COUNT <- "number_of_jurisdictions"
COL_HARMONIZATION_LEVEL <- "regulatory_harmonization_score"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    cross_border_operations = factor(cross_border_operations, 
                                    levels = c("No", "Yes"),
                                    labels = c("Domestic Only", "Cross-Border")),
    regulatory_complexity_index = as.numeric(regulatory_complexity_index),
    compliance_cost_percentage = as.numeric(compliance_cost_percentage),
    market_entry_time_months = as.numeric(market_entry_time_months),
    regulatory_barrier_score = as.numeric(regulatory_barrier_score),
    number_of_jurisdictions = as.numeric(number_of_jurisdictions),
    regulatory_harmonization_score = as.numeric(regulatory_harmonization_score),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(cross_border_operations), !is.na(regulatory_complexity_index))

# Descriptive statistics by group
group_stats <- df %>%
  group_by(cross_border_operations) %>%
  summarise(
    n = n(),
    mean_complexity = mean(regulatory_complexity_index, na.rm = TRUE),
    sd_complexity = sd(regulatory_complexity_index, na.rm = TRUE),
    median_complexity = median(regulatory_complexity_index, na.rm = TRUE),
    se_complexity = sd_complexity / sqrt(n),
    ci_lower = mean_complexity - qt(0.975, n-1) * se_complexity,
    ci_upper = mean_complexity + qt(0.975, n-1) * se_complexity,
    .groups = "drop"
  )

print("Regulatory complexity by cross-border status:")
print(group_stats)

# Primary t-test for regulatory complexity
t_test_result <- t.test(regulatory_complexity_index ~ cross_border_operations, 
                        data = df,
                        var.equal = FALSE)

print("\nWelch's t-test - Regulatory complexity by cross-border status:")
print(t_test_result)

# Extract statistics
t_stat <- t_test_result$statistic
p_value <- t_test_result$p.value
df_welch <- t_test_result$parameter
mean_diff <- diff(t_test_result$estimate)

print(paste("\nt-statistic:", round(t_stat, 2)))
print(paste("df:", round(df_welch, 1)))
print(paste("p-value:", format(p_value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_diff, 3)))

# Effect size calculations
cohens_d <- cohen.d(df$regulatory_complexity_index[df$cross_border_operations == "Cross-Border"],
                    df$regulatory_complexity_index[df$cross_border_operations == "Domestic Only"])

print(paste("\nCohen's d:", round(cohens_d$estimate, 3)))
print(paste("Effect size interpretation:", cohens_d$magnitude))

# Hedges' g (corrected for small sample bias)
hedges_g <- hedges_g(regulatory_complexity_index ~ cross_border_operations, data = df)
print(paste("Hedges' g:", round(hedges_g$Hedges_g, 3)))

# Levene's test for equality of variances
levene_result <- leveneTest(regulatory_complexity_index ~ cross_border_operations, data = df)
print("\nLevene's test for equality of variances:")
print(levene_result)

# Student's t-test (if variances are equal)
if(levene_result$`Pr(>F)`[1] > 0.05) {
  t_test_equal <- t.test(regulatory_complexity_index ~ cross_border_operations, 
                        data = df,
                        var.equal = TRUE)
  print("\nStudent's t-test (equal variances assumed):")
  print(t_test_equal)
}

# Mann-Whitney U test (non-parametric alternative)
wilcox_result <- wilcox.test(regulatory_complexity_index ~ cross_border_operations, 
                             data = df,
                             exact = FALSE)
print("\nMann-Whitney U test (non-parametric):")
print(wilcox_result)

# Rank-biserial correlation (effect size for Mann-Whitney)
rank_biserial <- rank_biserial_correlation(regulatory_complexity_index ~ cross_border_operations, 
                                          data = df)
print(paste("Rank-biserial correlation:", round(rank_biserial$rank_biserial_correlation, 3)))

# Power analysis
n1 <- sum(df$cross_border_operations == "Domestic Only")
n2 <- sum(df$cross_border_operations == "Cross-Border")
achieved_power <- pwr.t2n.test(n1 = n1, n2 = n2, 
                               d = cohens_d$estimate, 
                               sig.level = 0.05)$power
print(paste("\nAchieved statistical power:", round(achieved_power, 3)))

# Required sample size for 80% power
required_n <- pwr.t.test(d = cohens_d$estimate, 
                        sig.level = 0.05, 
                        power = 0.80, 
                        type = "two.sample")$n
print(paste("Required n per group for 80% power:", ceiling(required_n)))

# Bootstrap confidence intervals
set.seed(123)
n_boot <- 10000
boot_diffs <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- df %>%
    group_by(cross_border_operations) %>%
    sample_n(n(), replace = TRUE) %>%
    ungroup()
  
  boot_means <- boot_sample %>%
    group_by(cross_border_operations) %>%
    summarise(mean = mean(regulatory_complexity_index), .groups = "drop")
  
  boot_diffs[i] <- boot_means$mean[2] - boot_means$mean[1]
}

boot_ci <- quantile(boot_diffs, c(0.025, 0.975))
print("\nBootstrap 95% CI for mean difference:")
print(boot_ci)

# Additional cross-border metrics analysis
if(sum(!is.na(df$compliance_cost_percentage)) > 50) {
  t_test_cost <- t.test(compliance_cost_percentage ~ cross_border_operations, 
                        data = df,
                        var.equal = FALSE)
  
  print("\nCompliance cost analysis:")
  print(paste("Cross-border mean:", round(mean(df$compliance_cost_percentage[df$cross_border_operations == "Cross-Border"], na.rm = TRUE), 2), "%"))
  print(paste("Domestic mean:", round(mean(df$compliance_cost_percentage[df$cross_border_operations == "Domestic Only"], na.rm = TRUE), 2), "%"))
  print(paste("t =", round(t_test_cost$statistic, 2), ", p =", format(t_test_cost$p.value, scientific = TRUE)))
}

if(sum(!is.na(df$market_entry_time_months)) > 50) {
  t_test_time <- t.test(market_entry_time_months ~ cross_border_operations, 
                        data = df,
                        var.equal = FALSE)
  
  print("\nMarket entry time analysis:")
  print(paste("Cross-border mean:", round(mean(df$market_entry_time_months[df$cross_border_operations == "Cross-Border"], na.rm = TRUE), 1), "months"))
  print(paste("Domestic mean:", round(mean(df$market_entry_time_months[df$cross_border_operations == "Domestic Only"], na.rm = TRUE), 1), "months"))
  print(paste("t =", round(t_test_time$statistic, 2), ", p =", format(t_test_time$p.value, scientific = TRUE)))
}

# Jurisdiction count analysis for cross-border operations
if(sum(!is.na(df$number_of_jurisdictions)) > 50) {
  jurisdiction_summary <- df %>%
    filter(cross_border_operations == "Cross-Border") %>%
    summarise(
      mean_jurisdictions = mean(number_of_jurisdictions, na.rm = TRUE),
      median_jurisdictions = median(number_of_jurisdictions, na.rm = TRUE),
      sd_jurisdictions = sd(number_of_jurisdictions, na.rm = TRUE),
      max_jurisdictions = max(number_of_jurisdictions, na.rm = TRUE)
    )
  
  print("\nJurisdiction count for cross-border operations:")
  print(jurisdiction_summary)
  
  # Correlation between jurisdiction count and complexity
  cor_jurisdictions <- cor.test(df$number_of_jurisdictions[df$cross_border_operations == "Cross-Border"],
                                df$regulatory_complexity_index[df$cross_border_operations == "Cross-Border"],
                                use = "complete.obs")
  
  print("\nJurisdiction count × Complexity correlation:")
  print(paste("r =", round(cor_jurisdictions$estimate, 3), ", p =", format(cor_jurisdictions$p.value, scientific = TRUE)))
}

# Regulatory harmonization analysis
if(sum(!is.na(df$regulatory_harmonization_score)) > 50) {
  t_test_harmonization <- t.test(regulatory_harmonization_score ~ cross_border_operations, 
                                 data = df,
                                 var.equal = FALSE)
  
  print("\nRegulatory harmonization analysis:")
  print(paste("Cross-border mean:", round(mean(df$regulatory_harmonization_score[df$cross_border_operations == "Cross-Border"], na.rm = TRUE), 2)))
  print(paste("Domestic mean:", round(mean(df$regulatory_harmonization_score[df$cross_border_operations == "Domestic Only"], na.rm = TRUE), 2)))
  print(paste("t =", round(t_test_harmonization$statistic, 2), ", p =", format(t_test_harmonization$p.value, scientific = TRUE)))
}

# Stakeholder-specific analysis
stakeholder_analysis <- df %>%
  group_by(stakeholder, cross_border_operations) %>%
  summarise(
    n = n(),
    mean_complexity = mean(regulatory_complexity_index, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = cross_border_operations, 
              values_from = c(n, mean_complexity))

print("\nRegulatory complexity by stakeholder and cross-border status:")
print(stakeholder_analysis)

# Two-way ANOVA: Cross-border × Stakeholder
if(length(unique(df$stakeholder)) > 1) {
  anova_two_way <- aov(regulatory_complexity_index ~ cross_border_operations * stakeholder, 
                       data = df)
  
  print("\nTwo-way ANOVA - Cross-border × Stakeholder:")
  print(summary(anova_two_way))
  
  # Type II SS for unbalanced design
  print("\nType II Sum of Squares:")
  print(Anova(anova_two_way, type = "II"))
}

# Logistic regression: Predicting cross-border operations
logit_model <- glm(cross_border_operations ~ regulatory_complexity_index + 
                   compliance_cost_percentage + market_entry_time_months,
                   data = df,
                   family = binomial(link = "logit"))

print("\nLogistic regression - Predicting cross-border operations:")
print(summary(logit_model))

# Odds ratios
odds_ratios <- exp(coef(logit_model))
print("\nOdds ratios:")
print(odds_ratios)

# Normality tests
shapiro_domestic <- shapiro.test(df$regulatory_complexity_index[df$cross_border_operations == "Domestic Only"][1:min(5000, sum(df$cross_border_operations == "Domestic Only"))])
shapiro_crossborder <- shapiro.test(df$regulatory_complexity_index[df$cross_border_operations == "Cross-Border"][1:min(5000, sum(df$cross_border_operations == "Cross-Border"))])

print("\nNormality tests:")
print(paste("Domestic only - Shapiro-Wilk p-value:", format(shapiro_domestic$p.value, scientific = TRUE)))
print(paste("Cross-border - Shapiro-Wilk p-value:", format(shapiro_crossborder$p.value, scientific = TRUE)))

# Robustness check with trimmed means
trim_test <- t.test(regulatory_complexity_index ~ cross_border_operations, 
                    data = df,
                    trim = 0.1)
print("\n10% Trimmed means t-test:")
print(paste("t =", round(trim_test$statistic, 2), ", p =", format(trim_test$p.value, scientific = TRUE)))

# Summary statistics for reporting
print("\n=== SUMMARY FOR REPORTING ===")
print(paste("Cross-border operations (n =", group_stats$n[2], ") showed significantly higher"))
print(paste("regulatory complexity (M =", round(group_stats$mean_complexity[2], 2), 
           ", SD =", round(group_stats$sd_complexity[2], 2), ")"))
print(paste("compared to domestic operations (n =", group_stats$n[1], 
           ", M =", round(group_stats$mean_complexity[1], 2),
           ", SD =", round(group_stats$sd_complexity[1], 2), ")"))
print(paste("t(", round(df_welch, 1), ") =", round(t_stat, 2), ", p <", 
           ifelse(p_value < 0.001, ".001", round(p_value, 3))))
print(paste("Cohen's d =", round(cohens_d$estimate, 2)))

# Expected: regulatory complexity (t = 3.87, p < .001)