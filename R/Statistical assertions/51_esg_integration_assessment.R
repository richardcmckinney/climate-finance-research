# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 51: "ESG integration assessment scores varied significantly by organization size (F(3,1303) = 12.4, p < .001, η² = .028)"
# Purpose: One-way ANOVA testing differences in ESG integration assessment across organization size categories

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_ESG_INTEGRATION <- "esg_integration_assessment_score"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_STAKEHOLDER <- "stakeholder"
COL_ASSETS_UNDER_MANAGEMENT <- "assets_under_management_numeric"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    esg_integration_assessment_score = as.numeric(esg_integration_assessment_score),
    organization_size_category = factor(organization_size_category, 
                                       levels = c("Small", "Medium", "Large", "Very Large")),
    stakeholder = factor(stakeholder),
    assets_under_management_numeric = as.numeric(assets_under_management_numeric)
  ) %>%
  filter(!is.na(esg_integration_assessment_score), !is.na(organization_size_category)) %>%
  # Ensure scores are within expected range
  filter(esg_integration_assessment_score >= 1, esg_integration_assessment_score <= 10)

# Sample sizes by organization size
size_counts <- df %>%
  count(organization_size_category, sort = TRUE)
print("Sample sizes by organization size:")
print(size_counts)

# Descriptive statistics by organization size
desc_stats <- df %>%
  group_by(organization_size_category) %>%
  summarise(
    n = n(),
    mean_esg = round(mean(esg_integration_assessment_score), 2),
    sd_esg = round(sd(esg_integration_assessment_score), 2),
    median_esg = round(median(esg_integration_assessment_score), 2),
    se_esg = round(sd(esg_integration_assessment_score) / sqrt(n()), 3),
    ci_lower = round(mean_esg - 1.96 * se_esg, 2),
    ci_upper = round(mean_esg + 1.96 * se_esg, 2),
    .groups = "drop"
  )

print("ESG integration assessment by organization size:")
print(desc_stats)

# Check ANOVA assumptions
# Levene's test for homogeneity of variances
levene_test <- leveneTest(esg_integration_assessment_score ~ organization_size_category, 
                         data = df)
print("Levene's test for homogeneity of variances:")
print(levene_test)

# Normality check by group
shapiro_results <- df %>%
  group_by(organization_size_category) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n() > 5000, 
                      shapiro.test(sample(esg_integration_assessment_score, 5000))$p.value,
                      shapiro.test(esg_integration_assessment_score)$p.value),
    .groups = "drop"
  )
print("Shapiro-Wilk normality tests by organization size:")
print(shapiro_results)

# One-way ANOVA
anova_model <- lm(esg_integration_assessment_score ~ organization_size_category, data = df)
anova_result <- Anova(anova_model, type = "III")

# Calculate effect sizes
eta_squared_result <- eta_squared(anova_result)
omega_squared_result <- omega_squared(anova_result)

print("One-way ANOVA Results:")
print(anova_result)
print(paste("Effect size (η²):", round(eta_squared_result$Eta2_partial[1], 3)))
print(paste("Effect size (ω²):", round(omega_squared_result$Omega2_partial[1], 3)))

# Model fit statistics
model_summary <- summary(anova_model)
print(paste("R-squared:", round(model_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(model_summary$adj.r.squared, 3)))

# Post-hoc comparisons with Bonferroni correction
if (anova_result$`Pr(>F)`[1] < 0.05) {
  # Estimated marginal means
  emmeans_result <- emmeans(anova_model, ~ organization_size_category)
  print("Estimated marginal means:")
  print(emmeans_result)
  
  # Pairwise comparisons
  pairwise_comp <- pairs(emmeans_result, adjust = "bonferroni")
  print("Post-hoc pairwise comparisons (Bonferroni corrected):")
  print(pairwise_comp)
  
  # Compact letter display for significant differences
  cld_result <- cld(emmeans_result, alpha = 0.05, Letters = letters)
  print("Compact letter display:")
  print(cld_result)
}

# Alternative: Tukey HSD for comparison
tukey_result <- TukeyHSD(aov(esg_integration_assessment_score ~ organization_size_category, 
                            data = df))
print("Tukey HSD comparisons:")
print(tukey_result)

# Linear trend analysis (organization size as ordinal)
size_numeric <- as.numeric(df$organization_size_category)
linear_trend <- lm(esg_integration_assessment_score ~ size_numeric, data = df)
trend_summary <- summary(linear_trend)

print("Linear trend analysis (treating size as ordinal):")
print(paste("Linear trend: β =", round(coef(linear_trend)[2], 3)))
print(paste("R-squared:", round(trend_summary$r.squared, 3)))
print(paste("p-value:", format(trend_summary$coefficients[2, 4], scientific = TRUE)))

# Polynomial trend
poly_trend <- lm(esg_integration_assessment_score ~ poly(size_numeric, 2), data = df)
poly_summary <- summary(poly_trend)
print(paste("Quadratic trend R-squared:", round(poly_summary$r.squared, 3)))

# Breakdown by stakeholder within organization size
stakeholder_size_breakdown <- df %>%
  group_by(organization_size_category, stakeholder) %>%
  summarise(
    n = n(),
    mean_esg = round(mean(esg_integration_assessment_score), 2),
    sd_esg = round(sd(esg_integration_assessment_score), 2),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(organization_size_category, desc(mean_esg))

if(nrow(stakeholder_size_breakdown) > 0) {
  print("ESG integration by organization size and stakeholder (n ≥ 10):")
  print(stakeholder_size_breakdown)
}

# Kruskal-Wallis test (non-parametric alternative)
kruskal_test <- kruskal.test(esg_integration_assessment_score ~ organization_size_category, 
                            data = df)
print("Kruskal-Wallis test (non-parametric):")
print(paste("H =", round(kruskal_test$statistic, 2)))
print(paste("df =", kruskal_test$parameter))
print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))

# Effect size for Kruskal-Wallis (eta-squared)
if(kruskal_test$p.value < 0.05) {
  kruskal_eta <- (kruskal_test$statistic - length(levels(df$organization_size_category)) + 1) / 
                 (nrow(df) - length(levels(df$organization_size_category)))
  print(paste("Kruskal-Wallis eta-squared:", round(kruskal_eta, 3)))
}

# Analysis using AUM as continuous measure (if available)
if(sum(!is.na(df$assets_under_management_numeric)) > 100) {
  df_aum <- df %>%
    filter(!is.na(assets_under_management_numeric)) %>%
    # Log transform AUM due to likely skewness
    mutate(aum_log = log(assets_under_management_numeric + 1))
  
  # Correlation between ESG scores and log(AUM)
  cor_aum <- cor.test(df_aum$esg_integration_assessment_score, df_aum$aum_log)
  
  print("ESG integration × Log(Assets Under Management) correlation:")
  print(paste("r =", round(cor_aum$estimate, 3)))
  print(paste("95% CI: [", round(cor_aum$conf.int[1], 3), ", ", 
              round(cor_aum$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_aum$p.value, scientific = TRUE)))
  
  # Regression with continuous AUM
  lm_aum <- lm(esg_integration_assessment_score ~ aum_log, data = df_aum)
  aum_summary <- summary(lm_aum)
  print("Linear regression: ESG ~ Log(AUM):")
  print(paste("R-squared:", round(aum_summary$r.squared, 3)))
  print(paste("Slope:", round(coef(lm_aum)[2], 3)))
}

# Contrast analysis: Small+Medium vs Large+Very Large
df_contrast <- df %>%
  mutate(
    size_group = ifelse(organization_size_category %in% c("Small", "Medium"),
                       "Smaller Organizations", "Larger Organizations")
  )

contrast_test <- t.test(esg_integration_assessment_score ~ size_group, data = df_contrast)
small_medium_mean <- mean(df$esg_integration_assessment_score[df$organization_size_category %in% c("Small", "Medium")])
large_vlarge_mean <- mean(df$esg_integration_assessment_score[df$organization_size_category %in% c("Large", "Very Large")])

print("Small+Medium vs Large+Very Large contrast:")
print(paste("Smaller organizations mean:", round(small_medium_mean, 2)))
print(paste("Larger organizations mean:", round(large_vlarge_mean, 2)))
print(paste("t-test: t =", round(contrast_test$statistic, 2), 
            ", p =", format(contrast_test$p.value, scientific = TRUE)))

# Cohen's d for size contrast
size_cohens_d <- cohens_d(esg_integration_assessment_score ~ size_group, data = df_contrast)
print(paste("Cohen's d (size contrast):", round(size_cohens_d$Cohens_d, 3)))

# Distribution analysis
print("Distribution of ESG integration scores by organization size:")
size_distributions <- df %>%
  group_by(organization_size_category) %>%
  count(esg_integration_assessment_score) %>%
  group_by(organization_size_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  select(organization_size_category, score = esg_integration_assessment_score, percentage)

# Show distribution for scores 7-10 (high ESG integration)
high_esg_dist <- size_distributions %>%
  filter(score >= 7) %>%
  group_by(organization_size_category) %>%
  summarise(high_esg_percentage = sum(percentage), .groups = "drop")

print("Percentage with high ESG integration (scores 7-10) by size:")
print(high_esg_dist)

# Two-way ANOVA with stakeholder (if sufficient data)
if(length(unique(df$stakeholder)) > 1) {
  twoway_model <- lm(esg_integration_assessment_score ~ organization_size_category * stakeholder, 
                    data = df)
  twoway_result <- Anova(twoway_model, type = "III")
  
  print("Two-way ANOVA: Organization Size × Stakeholder")
  print(twoway_result)
}

# Identify highest and lowest scoring size categories
highest_esg_size <- desc_stats %>% slice_max(mean_esg, n = 1)
lowest_esg_size <- desc_stats %>% slice_min(mean_esg, n = 1)

print(paste("Highest ESG integration:", highest_esg_size$organization_size_category,
            "(M =", highest_esg_size$mean_esg, ", SD =", highest_esg_size$sd_esg, ")"))
print(paste("Lowest ESG integration:", lowest_esg_size$organization_size_category,
            "(M =", lowest_esg_size$mean_esg, ", SD =", lowest_esg_size$sd_esg, ")"))

# Effect size interpretation
eta_magnitude <- eta_squared_result$Eta2_partial[1]
eta_interpretation <- case_when(
  eta_magnitude < 0.01 ~ "Negligible",
  eta_magnitude < 0.06 ~ "Small",
  eta_magnitude < 0.14 ~ "Medium",
  TRUE ~ "Large"
)

print(paste("η² effect size interpretation:", eta_interpretation))

# Expected: F(3,1303) = 12.4, p < .001, η² = .028