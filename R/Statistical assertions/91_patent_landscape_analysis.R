# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 91: "Patent landscape analysis: complexity score (M = 3.67, SD = 1.12)"
# Purpose: Analyze patent landscape complexity scores

library(tidyverse)
library(janitor)
library(DescTools)
library(moments)
library(psych)
library(car)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_PATENT_COMPLEXITY <- "patent_landscape_complexity"
COL_PATENT_DENSITY <- "patent_density_score"
COL_PRIOR_ART_CHALLENGES <- "prior_art_challenges"
COL_FREEDOM_TO_OPERATE <- "freedom_to_operate_score"
COL_COMPETITOR_PATENTS <- "competitor_patent_count"
COL_PATENT_LITIGATION_RISK <- "patent_litigation_risk"
COL_TECHNOLOGY_FIELD <- "technology_field"
COL_PATENT_STRATEGY <- "patent_strategy_type"
COL_R_AND_D_INTENSITY <- "r_and_d_intensity"
COL_MARKET_MATURITY <- "market_maturity_stage"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    patent_landscape_complexity = as.numeric(patent_landscape_complexity),
    patent_density_score = as.numeric(patent_density_score),
    prior_art_challenges = as.numeric(prior_art_challenges),
    freedom_to_operate_score = as.numeric(freedom_to_operate_score),
    competitor_patent_count = as.numeric(competitor_patent_count),
    patent_litigation_risk = as.numeric(patent_litigation_risk),
    technology_field = factor(technology_field),
    patent_strategy_type = factor(patent_strategy_type),
    r_and_d_intensity = as.numeric(r_and_d_intensity),
    market_maturity_stage = factor(market_maturity_stage,
                                  levels = c("Emerging", "Growing", "Mature", "Declining"),
                                  ordered = TRUE),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(patent_landscape_complexity))

# Primary descriptive statistics
n_total <- nrow(df)
mean_complexity <- mean(df$patent_landscape_complexity, na.rm = TRUE)
sd_complexity <- sd(df$patent_landscape_complexity, na.rm = TRUE)
median_complexity <- median(df$patent_landscape_complexity, na.rm = TRUE)
se_complexity <- sd_complexity / sqrt(n_total)

print("Patent Landscape Complexity - Primary Statistics:")
print(paste("N =", n_total))
print(paste("Mean =", round(mean_complexity, 2)))
print(paste("SD =", round(sd_complexity, 2)))
print(paste("Median =", round(median_complexity, 2)))
print(paste("SE =", round(se_complexity, 3)))

# Confidence intervals
ci_lower <- mean_complexity - qt(0.975, n_total - 1) * se_complexity
ci_upper <- mean_complexity + qt(0.975, n_total - 1) * se_complexity

print(paste("95% CI: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))

# Detailed descriptive statistics
desc_stats <- describe(df$patent_landscape_complexity)
print("\nDetailed descriptive statistics:")
print(desc_stats)

# Distribution analysis
print("\nDistribution characteristics:")
print(paste("Skewness:", round(skewness(df$patent_landscape_complexity, na.rm = TRUE), 3)))
print(paste("Kurtosis:", round(kurtosis(df$patent_landscape_complexity, na.rm = TRUE), 3)))
print(paste("Jarque-Bera test p-value:", 
           format(jarque.test(df$patent_landscape_complexity)$p.value, scientific = TRUE)))

# Variance and range
variance <- var(df$patent_landscape_complexity, na.rm = TRUE)
range_val <- range(df$patent_landscape_complexity, na.rm = TRUE)
iqr_val <- IQR(df$patent_landscape_complexity, na.rm = TRUE)

print(paste("\nVariance:", round(variance, 3)))
print(paste("Range:", paste(range_val, collapse = " - ")))
print(paste("IQR:", round(iqr_val, 2)))

# Coefficient of variation
cv <- (sd_complexity / mean_complexity) * 100
print(paste("Coefficient of variation:", round(cv, 1), "%"))

# Percentiles
percentiles <- quantile(df$patent_landscape_complexity, 
                       probs = c(0.10, 0.25, 0.50, 0.75, 0.90), 
                       na.rm = TRUE)
print("\nKey percentiles:")
print(round(percentiles, 2))

# Normality tests
shapiro_result <- shapiro.test(df$patent_landscape_complexity[1:min(5000, n_total)])
print("\nShapiro-Wilk normality test:")
print(paste("W =", round(shapiro_result$statistic, 4)))
print(paste("p-value:", format(shapiro_result$p.value, scientific = TRUE)))

# Anderson-Darling test
ad_test <- ad.test(df$patent_landscape_complexity)
print("\nAnderson-Darling test:")
print(paste("A =", round(ad_test$statistic, 3)))
print(paste("p-value:", format(ad_test$p.value, scientific = TRUE)))

# One-sample t-test against scale midpoint
scale_midpoint <- 3
t_test_midpoint <- t.test(df$patent_landscape_complexity, mu = scale_midpoint)

print("\nOne-sample t-test (vs. scale midpoint = 3):")
print(paste("t(", n_total - 1, ") =", round(t_test_midpoint$statistic, 2)))
print(paste("p-value:", format(t_test_midpoint$p.value, scientific = TRUE)))
print(paste("Mean difference:", round(mean_complexity - scale_midpoint, 2)))

# Bootstrap analysis
set.seed(123)
n_boot <- 10000
boot_means <- numeric(n_boot)
boot_sds <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$patent_landscape_complexity, 
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

# Correlation with patent density
if(sum(!is.na(df$patent_density_score)) > 30) {
  cor_density <- cor.test(df$patent_landscape_complexity,
                         df$patent_density_score,
                         use = "complete.obs")
  
  print("\nCorrelation with patent density:")
  print(paste("r =", round(cor_density$estimate, 3)))
  print(paste("95% CI: [", round(cor_density$conf.int[1], 3), ",", 
             round(cor_density$conf.int[2], 3), "]"))
  print(paste("p-value:", format(cor_density$p.value, scientific = TRUE)))
}

# Prior art challenges analysis
if(sum(!is.na(df$prior_art_challenges)) > 30) {
  cor_prior_art <- cor.test(df$patent_landscape_complexity,
                           df$prior_art_challenges,
                           use = "complete.obs")
  
  print("\nCorrelation with prior art challenges:")
  print(paste("r =", round(cor_prior_art$estimate, 3)))
  print(paste("p-value:", format(cor_prior_art$p.value, scientific = TRUE)))
  
  # Regression analysis
  lm_prior_art <- lm(patent_landscape_complexity ~ prior_art_challenges, data = df)
  print("\nRegression - Complexity predicted by prior art challenges:")
  print(summary(lm_prior_art))
}

# Freedom to operate analysis
if(sum(!is.na(df$freedom_to_operate_score)) > 30) {
  cor_fto <- cor.test(df$patent_landscape_complexity,
                     df$freedom_to_operate_score,
                     use = "complete.obs")
  
  print("\nCorrelation with freedom to operate (expecting negative):")
  print(paste("r =", round(cor_fto$estimate, 3)))
  print(paste("p-value:", format(cor_fto$p.value, scientific = TRUE)))
}

# Technology field analysis
if(!all(is.na(df$technology_field))) {
  tech_stats <- df %>%
    filter(!is.na(technology_field)) %>%
    group_by(technology_field) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(patent_landscape_complexity, na.rm = TRUE), 2),
      sd_complexity = round(sd(patent_landscape_complexity, na.rm = TRUE), 2),
      median_complexity = round(median(patent_landscape_complexity, na.rm = TRUE), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_complexity))
  
  print("\nTop technology fields by patent complexity:")
  print(head(tech_stats, 10))
  
  # ANOVA by technology field
  if(nrow(tech_stats) > 2) {
    df_tech <- df %>% filter(technology_field %in% tech_stats$technology_field)
    anova_tech <- aov(patent_landscape_complexity ~ technology_field, data = df_tech)
    print("\nANOVA by technology field:")
    print(summary(anova_tech))
    
    # Effect size
    omega_sq <- omega_squared(anova_tech)
    print(paste("Omega-squared:", round(omega_sq$Omega2, 3)))
  }
}

# Patent strategy type analysis
if(!all(is.na(df$patent_strategy_type))) {
  strategy_stats <- df %>%
    filter(!is.na(patent_strategy_type)) %>%
    group_by(patent_strategy_type) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(patent_landscape_complexity, na.rm = TRUE), 2),
      sd_complexity = round(sd(patent_landscape_complexity, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nComplexity by patent strategy type:")
  print(strategy_stats)
  
  # Kruskal-Wallis test
  if(length(unique(df$patent_strategy_type)) > 2) {
    kruskal_strategy <- kruskal.test(patent_landscape_complexity ~ patent_strategy_type, 
                                     data = df)
    print("\nKruskal-Wallis test by strategy type:")
    print(paste("χ² =", round(kruskal_strategy$statistic, 2)))
    print(paste("p-value:", format(kruskal_strategy$p.value, scientific = TRUE)))
  }
}

# Market maturity analysis
if(!all(is.na(df$market_maturity_stage))) {
  maturity_stats <- df %>%
    filter(!is.na(market_maturity_stage)) %>%
    group_by(market_maturity_stage) %>%
    summarise(
      n = n(),
      mean_complexity = round(mean(patent_landscape_complexity, na.rm = TRUE), 2),
      sd_complexity = round(sd(patent_landscape_complexity, na.rm = TRUE), 2),
      .groups = "drop"
    )
  
  print("\nComplexity by market maturity stage:")
  print(maturity_stats)
  
  # Jonckheere-Terpstra test for trend
  if(nrow(maturity_stats) > 2) {
    jt_test <- jonckheere.test(df$patent_landscape_complexity, 
                               df$market_maturity_stage,
                               alternative = "increasing")
    print("\nJonckheere-Terpstra test for trend:")
    print(paste("JT statistic =", round(jt_test$statistic, 2)))
    print(paste("p-value:", format(jt_test$p.value, scientific = TRUE)))
  }
}

# R&D intensity correlation
if(sum(!is.na(df$r_and_d_intensity)) > 30) {
  cor_rd <- cor.test(df$patent_landscape_complexity,
                    df$r_and_d_intensity,
                    use = "complete.obs")
  
  print("\nCorrelation with R&D intensity:")
  print(paste("r =", round(cor_rd$estimate, 3)))
  print(paste("p-value:", format(cor_rd$p.value, scientific = TRUE)))
}

# Patent litigation risk analysis
if(sum(!is.na(df$patent_litigation_risk)) > 30) {
  cor_litigation <- cor.test(df$patent_landscape_complexity,
                            df$patent_litigation_risk,
                            use = "complete.obs")
  
  print("\nCorrelation with patent litigation risk:")
  print(paste("r =", round(cor_litigation$estimate, 3)))
  print(paste("p-value:", format(cor_litigation$p.value, scientific = TRUE)))
}

# Competitor patent count analysis
if(sum(!is.na(df$competitor_patent_count)) > 30) {
  # Log transform if needed due to skewness
  df$log_competitor_patents <- log1p(df$competitor_patent_count)
  
  cor_competitors <- cor.test(df$patent_landscape_complexity,
                             df$log_competitor_patents,
                             use = "complete.obs")
  
  print("\nCorrelation with log(competitor patent count + 1):")
  print(paste("r =", round(cor_competitors$estimate, 3)))
  print(paste("p-value:", format(cor_competitors$p.value, scientific = TRUE)))
}

# Multiple regression model
predictors <- c("patent_density_score", "prior_art_challenges", 
               "freedom_to_operate_score", "r_and_d_intensity")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 2) {
  formula_str <- paste("patent_landscape_complexity ~", 
                      paste(available_predictors, collapse = " + "))
  
  lm_full <- lm(as.formula(formula_str), data = df)
  
  print("\nMultiple regression model:")
  print(summary(lm_full))
  
  # VIF for multicollinearity
  if(length(available_predictors) > 1) {
    vif_values <- vif(lm_full)
    print("\nVariance Inflation Factors:")
    print(round(vif_values, 2))
  }
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_complexity = round(mean(patent_landscape_complexity, na.rm = TRUE), 2),
    sd_complexity = round(sd(patent_landscape_complexity, na.rm = TRUE), 2),
    se_complexity = round(sd_complexity / sqrt(n), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_complexity))

print("\nPatent landscape complexity by stakeholder:")
print(stakeholder_stats)

# Outlier detection
z_scores <- abs(scale(df$patent_landscape_complexity))
outliers <- which(z_scores > 3)
n_outliers <- length(outliers)

print(paste("\nNumber of outliers (|z| > 3):", n_outliers))
print(paste("Percentage of outliers:", round(n_outliers/n_total * 100, 2), "%"))

if(n_outliers > 0 && n_outliers <= 10) {
  print("Outlier values:")
  print(df$patent_landscape_complexity[outliers])
}

# Robust statistics
mad_value <- mad(df$patent_landscape_complexity, na.rm = TRUE)
trimmed_mean_10 <- mean(df$patent_landscape_complexity, trim = 0.1, na.rm = TRUE)
trimmed_sd_10 <- sd(df$patent_landscape_complexity[
  df$patent_landscape_complexity >= quantile(df$patent_landscape_complexity, 0.1) &
  df$patent_landscape_complexity <= quantile(df$patent_landscape_complexity, 0.9)
], na.rm = TRUE)

print(paste("\nMedian Absolute Deviation (MAD):", round(mad_value, 3)))
print(paste("10% Trimmed mean:", round(trimmed_mean_10, 2)))
print(paste("10% Trimmed SD:", round(trimmed_sd_10, 2)))

# Response distribution
freq_table <- table(round(df$patent_landscape_complexity, 1))
print("\nFrequency distribution (rounded to 0.1):")
print(freq_table)

# Heteroscedasticity test
if(sum(!is.na(df$patent_density_score)) > 30) {
  lm_simple <- lm(patent_landscape_complexity ~ patent_density_score, data = df)
  bp_test <- bptest(lm_simple)
  print("\nBreusch-Pagan test for heteroscedasticity:")
  print(paste("BP =", round(bp_test$statistic, 2)))
  print(paste("p-value:", format(bp_test$p.value, scientific = TRUE)))
}

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("Patent landscape complexity score: M =", round(mean_complexity, 2), 
           ", SD =", round(sd_complexity, 2)))
print(paste("N =", n_total))
print(paste("95% CI for mean: [", round(ci_lower, 2), ",", round(ci_upper, 2), "]"))
print(paste("The moderate mean of", round(mean_complexity, 2), 
           "with relatively high SD of", round(sd_complexity, 2)))
print("indicates varied complexity across different patent landscapes")

# Expected: complexity score (M = 3.67, SD = 1.12)