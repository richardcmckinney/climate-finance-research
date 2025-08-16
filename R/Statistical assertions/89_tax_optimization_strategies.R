# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 89: "Tax optimization strategies: effectiveness rating (M = 3.92, SD = 0.84)"
# Purpose: Analyze tax optimization strategy effectiveness ratings

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TAX_OPTIMIZATION <- "tax_optimization_effectiveness"
COL_TAX_STRATEGY_USE <- "tax_strategy_implementation"
COL_TAX_SAVINGS <- "estimated_tax_savings_percentage"
COL_JURISDICTION <- "jurisdiction"
COL_COMPANY_SIZE <- "company_size"
COL_INDUSTRY <- "industry_sector"
COL_INTERNATIONAL_STRUCTURE <- "international_tax_structure"
COL_TRANSFER_PRICING <- "transfer_pricing_usage"
COL_R_AND_D_CREDITS <- "r_and_d_tax_credits"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    tax_optimization_effectiveness = as.numeric(tax_optimization_effectiveness),
    tax_strategy_implementation = factor(tax_strategy_implementation),
    estimated_tax_savings_percentage = as.numeric(estimated_tax_savings_percentage),
    jurisdiction = factor(jurisdiction),
    company_size = factor(company_size, 
                          levels = c("Small", "Medium", "Large", "Enterprise"),
                          ordered = TRUE),
    industry_sector = factor(industry_sector),
    international_tax_structure = factor(international_tax_structure),
    transfer_pricing_usage = factor(transfer_pricing_usage),
    r_and_d_tax_credits = factor(r_and_d_tax_credits),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(tax_optimization_effectiveness))

# Primary descriptive statistics
n_total <- nrow(df)
mean_effectiveness <- mean(df$tax_optimization_effectiveness, na.rm = TRUE)
sd_effectiveness <- sd(df$tax_optimization_effectiveness, na.rm = TRUE)
median_effectiveness <- median(df$tax_optimization_effectiveness, na.rm = TRUE)
se_effectiveness <- sd_effectiveness / sqrt(n_total)

print("Tax Optimization Effectiveness - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_effectiveness, 2)))
print(paste("SD =", round(sd_effectiveness, 2)))
print(paste("Median =", round(median_effectiveness, 2)))
print(paste("SE =", round(se_effectiveness, 3)))

# Confidence intervals
ci_lower <- mean_effectiveness - qt(0.975, n_total - 1) * se_effectiveness
ci_upper <- mean_effectiveness + qt(0.975, n_total - 1) * se_effectiveness

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$tax_optimization_effectiveness)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution analysis
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$tax_optimization_effectiveness, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$tax_optimization_effectiveness, na.rm = TRUE), 3)))

# Quartiles and IQR
quartiles <- quantile(df$tax_optimization_effectiveness, 
                     probs = c(0, 0.25, 0.5, 0.75, 1), 
                     na.rm = TRUE)
iqr_value <- IQR(df$tax_optimization_effectiveness, na.rm = TRUE)

print("\nQuartiles:")
print(quartiles)
print(paste("IQR:", round(iqr_value, 2)))

# Mode calculation
mode_value <- Mode(df$tax_optimization_effectiveness)[1]
print(paste("Mode:", round(mode_value, 2)))

# Coefficient of variation
cv <- (sd_effectiveness / mean_effectiveness) * 100
print(paste("Coefficient of variation:", round(cv, 1), "%"))

# Normality tests
shapiro_result <- shapiro.test(df$tax_optimization_effectiveness[1:min(5000, n_total)])
print("\nNormality test (Shapiro-Wilk):")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# Anderson-Darling test
ad_test <- ad.test(df$tax_optimization_effectiveness)
print("\nAnderson-Darling normality test:")
print(paste("A =", round(ad_test$statistic, 3)))
print(paste("p-value:", format(ad_test$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint (assuming 1-5 scale)
scale_midpoint <- 3
t_test_midpoint <- t.test(df$tax_optimization_effectiveness, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_effectiveness - scale_midpoint, 2)))

# Bootstrap confidence intervals
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$tax_optimization_effectiveness, 
                       size = n_total, 
                       replace = TRUE)
  boot_means[i] <- mean(boot_sample)
  boot_sds[i] <- sd(boot_sample)
}

boot_ci_mean <- quantile(boot_means, c(0.025, 0.975))
boot_ci_sd <- quantile(boot_sds, c(0.025, 0.975))

print("\nBootstrap 95% CI for mean:")
print(boot_ci_mean)
print("Bootstrap 95% CI for SD:")
print(boot_ci_sd)

# Analysis by tax strategy implementation
if(!all(is.na(df$tax_strategy_implementation))) {
  strategy_stats <- df %>%
    filter(!is.na(tax_strategy_implementation)) %>%
    group_by(tax_strategy_implementation) %>%
    summarise(
      n = n(),
      mean = round(mean(tax_optimization_effectiveness, na.rm = TRUE), 2),
      sd = round(sd(tax_optimization_effectiveness, na.rm = TRUE), 2),
      median = round(median(tax_optimization_effectiveness, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    arrange(desc(mean))
  
  print("\nEffectiveness by tax strategy implementation:")
  print(strategy_stats)
  
  # ANOVA if multiple groups
  if(length(unique(df$tax_strategy_implementation)) > 2) {
    anova_strategy <- aov(tax_optimization_effectiveness ~ tax_strategy_implementation, 
                         data = df)
    print("\nANOVA by tax strategy:")
    print(summary(anova_strategy))
  }
}

# Analysis by company size
if(!all(is.na(df$company_size))) {
  size_stats <- df %>%
    filter(!is.na(company_size)) %>%
    group_by(company_size) %>%
    summarise(
      n = n(),
      mean = round(mean(tax_optimization_effectiveness, na.rm = TRUE), 2),
      sd = round(sd(tax_optimization_effectiveness, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nEffectiveness by company size:")
  print(size_stats)
  
  # Kruskal-Wallis test for ordered factor
  if(nrow(size_stats) > 1) {
    kruskal_size <- kruskal.test(tax_optimization_effectiveness ~ company_size, 
                                 data = df)
    print("\nKruskal-Wallis test by company size:")
    print(paste("χ² =", round(kruskal_size$statistic, 2), 
               ", p =", format(kruskal_size$p.value, scientific = TRUE)))
  }
}

# Correlation with tax savings
if(sum(!is.na(df$estimated_tax_savings_percentage)) > 30) {
  cor_savings <- cor.test(df$tax_optimization_effectiveness,
                          df$estimated_tax_savings_percentage,
                          use = "complete.obs")
  
  print("\nCorrelation with estimated tax savings:")
  print(paste("r =", round(cor_savings$estimate, 3)))
  print(paste("p-value:", format(cor_savings$p.value, scientific = TRUE)))
  
  # Regression analysis
  lm_savings <- lm(estimated_tax_savings_percentage ~ tax_optimization_effectiveness, 
                   data = df)
  print("\nRegression - Tax savings predicted by effectiveness:")
  print(summary(lm_savings))
}

# Industry sector analysis
if(!all(is.na(df$industry_sector))) {
  industry_stats <- df %>%
    filter(!is.na(industry_sector)) %>%
    group_by(industry_sector) %>%
    summarise(
      n = n(),
      mean = round(mean(tax_optimization_effectiveness, na.rm = TRUE), 2),
      sd = round(sd(tax_optimization_effectiveness, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean))
  
  print("\nTop industries by tax optimization effectiveness:")
  print(head(industry_stats, 10))
}

# International tax structure analysis
if(!all(is.na(df$international_tax_structure))) {
  intl_stats <- df %>%
    filter(!is.na(international_tax_structure)) %>%
    group_by(international_tax_structure) %>%
    summarise(
      n = n(),
      mean = round(mean(tax_optimization_effectiveness, na.rm = TRUE), 2),
      sd = round(sd(tax_optimization_effectiveness, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nEffectiveness by international tax structure:")
  print(intl_stats)
}

# Transfer pricing usage analysis
if(!all(is.na(df$transfer_pricing_usage))) {
  tp_comparison <- df %>%
    filter(!is.na(transfer_pricing_usage)) %>%
    group_by(transfer_pricing_usage) %>%
    summarise(
      n = n(),
      mean = round(mean(tax_optimization_effectiveness, na.rm = TRUE), 2),
      sd = round(sd(tax_optimization_effectiveness, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nEffectiveness by transfer pricing usage:")
  print(tp_comparison)
  
  if(length(unique(df$transfer_pricing_usage)) == 2) {
    t_test_tp <- t.test(tax_optimization_effectiveness ~ transfer_pricing_usage, 
                        data = df)
    print(paste("t-test result: t =", round(t_test_tp$statistic, 2), 
               ", p =", format(t_test_tp$p.value, scientific = TRUE)))
  }
}

# R&D tax credits analysis
if(!all(is.na(df$r_and_d_tax_credits))) {
  rd_comparison <- df %>%
    filter(!is.na(r_and_d_tax_credits)) %>%
    group_by(r_and_d_tax_credits) %>%
    summarise(
      n = n(),
      mean = round(mean(tax_optimization_effectiveness, na.rm = TRUE), 2),
      sd = round(sd(tax_optimization_effectiveness, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nEffectiveness by R&D tax credit usage:")
  print(rd_comparison)
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean = round(mean(tax_optimization_effectiveness, na.rm = TRUE), 2),
    sd = round(sd(tax_optimization_effectiveness, na.rm = TRUE), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean))

print("\nTax optimization effectiveness by stakeholder:")
print(stakeholder_stats)

# Outlier detection
z_scores <- abs(scale(df$tax_optimization_effectiveness))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 1), "%"))

# Robust statistics (median absolute deviation)
mad_value <- mad(df$tax_optimization_effectiveness, na.rm = TRUE)
print(paste("Median Absolute Deviation (MAD):", round(mad_value, 3)))

# Winsorized mean (5% trimming)
winsorized_mean <- mean(Winsorize(df$tax_optimization_effectiveness, probs = c(0.05, 0.95)))
print(paste("5% Winsorized mean:", round(winsorized_mean, 2)))

# Variance components analysis (if hierarchical structure exists)
if("jurisdiction" %in% names(df) && length(unique(df$jurisdiction)) > 5) {
  var_components <- df %>%
    filter(!is.na(jurisdiction)) %>%
    group_by(jurisdiction) %>%
    summarise(
      jurisdiction_mean = mean(tax_optimization_effectiveness, na.rm = TRUE),
      jurisdiction_var = var(tax_optimization_effectiveness, na.rm = TRUE),
      .groups = "drop"
    )
  
  between_var <- var(var_components$jurisdiction_mean)
  within_var <- mean(var_components$jurisdiction_var)
  
  print("\nVariance components:")
  print(paste("Between-jurisdiction variance:", round(between_var, 3)))
  print(paste("Within-jurisdiction variance:", round(within_var, 3)))
  print(paste("ICC:", round(between_var / (between_var + within_var), 3)))
}

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Tax optimization effectiveness rating: M =", round(mean_effectiveness, 2), 
           ", SD =", round(sd_effectiveness, 2)))
print(paste("N =", n_total))
print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("The mean rating of", round(mean_effectiveness, 2), 
           "indicates moderate to high perceived effectiveness"))

# Expected: effectiveness rating (M = 3.92, SD = 0.84)