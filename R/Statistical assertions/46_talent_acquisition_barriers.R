# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 46: "Talent acquisition barriers varied significantly by company stage (F(4,1302) = 8.3, p < .001, η² = .025)"
# Purpose: One-way ANOVA testing differences in talent acquisition barrier ratings across company development stages

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TALENT_BARRIERS <- "talent_acquisition_barrier_rating"
COL_COMPANY_STAGE <- "company_development_stage"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    talent_acquisition_barrier_rating = as.numeric(talent_acquisition_barrier_rating),
    company_development_stage = factor(company_development_stage, 
                                     levels = c("Idea", "Pre-seed", "Seed", "Series A", "Growth")),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(talent_acquisition_barrier_rating), !is.na(company_development_stage)) %>%
  # Ensure ratings are within expected scale
  filter(talent_acquisition_barrier_rating >= 1, talent_acquisition_barrier_rating <= 7)

# Sample sizes by company stage
stage_counts <- df %>%
  count(company_development_stage, sort = TRUE)
print("Sample sizes by company development stage:")
print(stage_counts)

# Filter to stages with adequate sample sizes (n ≥ 20)
stages_to_include <- stage_counts %>%
  filter(n >= 20) %>%
  pull(company_development_stage)

df_filtered <- df %>%
  filter(company_development_stage %in% stages_to_include) %>%
  mutate(company_development_stage = droplevels(company_development_stage))

print(paste("Analysis includes", length(stages_to_include), "stages with n ≥ 20"))

# Descriptive statistics by company stage
desc_stats <- df_filtered %>%
  group_by(company_development_stage) %>%
  summarise(
    n = n(),
    mean_talent_barriers = round(mean(talent_acquisition_barrier_rating), 2),
    sd_talent_barriers = round(sd(talent_acquisition_barrier_rating), 2),
    median_talent_barriers = round(median(talent_acquisition_barrier_rating), 2),
    se_talent_barriers = round(sd(talent_acquisition_barrier_rating) / sqrt(n()), 3),
    ci_lower = round(mean_talent_barriers - 1.96 * se_talent_barriers, 2),
    ci_upper = round(mean_talent_barriers + 1.96 * se_talent_barriers, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_talent_barriers))

print("Talent acquisition barriers by company development stage:")
print(desc_stats)

# Check ANOVA assumptions
# Levene's test for homogeneity of variances
levene_test <- leveneTest(talent_acquisition_barrier_rating ~ company_development_stage, 
                         data = df_filtered)
print("Levene's test for homogeneity of variances:")
print(levene_test)

# Normality check by group
shapiro_results <- df_filtered %>%
  group_by(company_development_stage) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n() > 5000, 
                      shapiro.test(sample(talent_acquisition_barrier_rating, 5000))$p.value,
                      shapiro.test(talent_acquisition_barrier_rating)$p.value),
    .groups = "drop"
  )
print("Shapiro-Wilk normality tests by company stage:")
print(shapiro_results)

# One-way ANOVA
anova_model <- lm(talent_acquisition_barrier_rating ~ company_development_stage, 
                  data = df_filtered)
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
  emmeans_result <- emmeans(anova_model, ~ company_development_stage)
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
tukey_result <- TukeyHSD(aov(talent_acquisition_barrier_rating ~ company_development_stage, 
                            data = df_filtered))
print("Tukey HSD comparisons:")
print(tukey_result)

# Trend analysis (if stages are ordinal)
if(length(levels(df_filtered$company_development_stage)) >= 3) {
  # Linear trend test
  stage_numeric <- as.numeric(df_filtered$company_development_stage)
  linear_trend <- lm(talent_acquisition_barrier_rating ~ stage_numeric, data = df_filtered)
  trend_summary <- summary(linear_trend)
  
  print("Linear trend analysis:")
  print(paste("Linear trend: β =", round(coef(linear_trend)[2], 3)))
  print(paste("R-squared:", round(trend_summary$r.squared, 3)))
  print(paste("p-value:", format(trend_summary$coefficients[2, 4], scientific = TRUE)))
  
  # Polynomial trend
  poly_trend <- lm(talent_acquisition_barrier_rating ~ poly(stage_numeric, 2), data = df_filtered)
  poly_summary <- summary(poly_trend)
  print(paste("Quadratic trend R-squared:", round(poly_summary$r.squared, 3)))
}

# Breakdown by stakeholder within stage (if sufficient data)
stakeholder_stage_breakdown <- df_filtered %>%
  group_by(company_development_stage, stakeholder) %>%
  summarise(
    n = n(),
    mean_barriers = round(mean(talent_acquisition_barrier_rating), 2),
    sd_barriers = round(sd(talent_acquisition_barrier_rating), 2),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(company_development_stage, desc(mean_barriers))

if(nrow(stakeholder_stage_breakdown) > 0) {
  print("Talent barriers by company stage and stakeholder (n ≥ 10):")
  print(stakeholder_stage_breakdown)
}

# Kruskal-Wallis test (non-parametric alternative)
kruskal_test <- kruskal.test(talent_acquisition_barrier_rating ~ company_development_stage, 
                            data = df_filtered)
print("Kruskal-Wallis test (non-parametric):")
print(paste("H =", round(kruskal_test$statistic, 2)))
print(paste("df =", kruskal_test$parameter))
print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))

# Effect size for Kruskal-Wallis (eta-squared)
if(kruskal_test$p.value < 0.05) {
  kruskal_eta <- (kruskal_test$statistic - length(levels(df_filtered$company_development_stage)) + 1) / 
                 (nrow(df_filtered) - length(levels(df_filtered$company_development_stage)))
  print(paste("Kruskal-Wallis eta-squared:", round(kruskal_eta, 3)))
}

# Contrast analysis: Early stage (Idea, Pre-seed) vs Later stages
if("Idea" %in% levels(df_filtered$company_development_stage) && 
   "Pre-seed" %in% levels(df_filtered$company_development_stage)) {
  
  df_contrast <- df_filtered %>%
    mutate(
      stage_group = ifelse(company_development_stage %in% c("Idea", "Pre-seed"),
                          "Early Stage", "Later Stage")
    )
  
  contrast_test <- t.test(talent_acquisition_barrier_rating ~ stage_group, data = df_contrast)
  print("Early stage vs Later stage contrast:")
  early_mean <- mean(df_contrast$talent_acquisition_barrier_rating[df_contrast$stage_group == "Early Stage"])
  later_mean <- mean(df_contrast$talent_acquisition_barrier_rating[df_contrast$stage_group == "Later Stage"])
  
  print(paste("Early stage mean:", round(early_mean, 2)))
  print(paste("Later stage mean:", round(later_mean, 2)))
  print(paste("t-test: t =", round(contrast_test$statistic, 2), 
              ", p =", format(contrast_test$p.value, scientific = TRUE)))
}

# Identify stages with highest and lowest barrier ratings
if(nrow(desc_stats) > 1) {
  highest_barriers <- desc_stats %>% slice_max(mean_talent_barriers, n = 1)
  lowest_barriers <- desc_stats %>% slice_min(mean_talent_barriers, n = 1)
  
  print(paste("Highest talent barriers:", highest_barriers$company_development_stage,
              "(M =", highest_barriers$mean_talent_barriers, ")"))
  print(paste("Lowest talent barriers:", lowest_barriers$company_development_stage,
              "(M =", lowest_barriers$mean_talent_barriers, ")"))
}

# Distribution analysis by stage
print("Distribution of talent barrier ratings by stage:")
stage_distributions <- df_filtered %>%
  group_by(company_development_stage) %>%
  count(talent_acquisition_barrier_rating) %>%
  group_by(company_development_stage) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  select(company_development_stage, rating = talent_acquisition_barrier_rating, percentage)

print(stage_distributions)

# Expected: F(4,1302) = 8.3, p < .001, η² = .025