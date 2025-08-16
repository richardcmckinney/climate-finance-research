# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 48: "Policy framework assessments differed significantly between stakeholders (F(3,1303) = 15.6, p < .001, η² = .035)"
# Purpose: One-way ANOVA testing differences in policy framework assessment ratings across stakeholder groups

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)
library(multcomp)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_POLICY_FRAMEWORK <- "policy_framework_assessment_rating"
COL_STAKEHOLDER <- "stakeholder"
COL_EXPERIENCE <- "experience_years"
COL_REGION <- "geographic_region"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    policy_framework_assessment_rating = as.numeric(policy_framework_assessment_rating),
    stakeholder = factor(stakeholder, levels = c("Venture Capital", "Government Agency", 
                                               "Entrepreneur", "Limited Partner")),
    experience_years = as.numeric(experience_years),
    geographic_region = factor(geographic_region)
  ) %>%
  filter(!is.na(policy_framework_assessment_rating), !is.na(stakeholder)) %>%
  # Ensure ratings are within expected scale
  filter(policy_framework_assessment_rating >= 1, policy_framework_assessment_rating <= 7)

# Sample sizes by stakeholder
stakeholder_counts <- df %>%
  count(stakeholder, sort = TRUE)
print("Sample sizes by stakeholder:")
print(stakeholder_counts)

# Descriptive statistics by stakeholder
desc_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_policy = round(mean(policy_framework_assessment_rating), 2),
    sd_policy = round(sd(policy_framework_assessment_rating), 2),
    median_policy = round(median(policy_framework_assessment_rating), 2),
    se_policy = round(sd(policy_framework_assessment_rating) / sqrt(n()), 3),
    ci_lower = round(mean_policy - 1.96 * se_policy, 2),
    ci_upper = round(mean_policy + 1.96 * se_policy, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_policy))

print("Policy framework assessments by stakeholder:")
print(desc_stats)

# Check ANOVA assumptions
# Levene's test for homogeneity of variances
levene_test <- leveneTest(policy_framework_assessment_rating ~ stakeholder, data = df)
print("Levene's test for homogeneity of variances:")
print(levene_test)

# Normality check by group
shapiro_results <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n() > 5000, 
                      shapiro.test(sample(policy_framework_assessment_rating, 5000))$p.value,
                      shapiro.test(policy_framework_assessment_rating)$p.value),
    .groups = "drop"
  )
print("Shapiro-Wilk normality tests by stakeholder:")
print(shapiro_results)

# One-way ANOVA
anova_model <- lm(policy_framework_assessment_rating ~ stakeholder, data = df)
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
  emmeans_result <- emmeans(anova_model, ~ stakeholder)
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
tukey_result <- TukeyHSD(aov(policy_framework_assessment_rating ~ stakeholder, data = df))
print("Tukey HSD comparisons:")
print(tukey_result)

# Kruskal-Wallis test (non-parametric alternative)
kruskal_test <- kruskal.test(policy_framework_assessment_rating ~ stakeholder, data = df)
print("Kruskal-Wallis test (non-parametric):")
print(paste("H =", round(kruskal_test$statistic, 2)))
print(paste("df =", kruskal_test$parameter))
print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))

# Effect size for Kruskal-Wallis (eta-squared)
if(kruskal_test$p.value < 0.05) {
  kruskal_eta <- (kruskal_test$statistic - length(levels(df$stakeholder)) + 1) / 
                 (nrow(df) - length(levels(df$stakeholder)))
  print(paste("Kruskal-Wallis eta-squared:", round(kruskal_eta, 3)))
}

# Specific contrasts of interest
# Government vs Non-government stakeholders
df_gov_contrast <- df %>%
  mutate(
    stakeholder_group = ifelse(stakeholder == "Government Agency", 
                              "Government", "Non-Government")
  )

gov_contrast <- t.test(policy_framework_assessment_rating ~ stakeholder_group, 
                      data = df_gov_contrast)

gov_mean <- mean(df$policy_framework_assessment_rating[df$stakeholder == "Government Agency"])
non_gov_mean <- mean(df$policy_framework_assessment_rating[df$stakeholder != "Government Agency"])

print("Government vs Non-Government contrast:")
print(paste("Government mean:", round(gov_mean, 2)))
print(paste("Non-Government mean:", round(non_gov_mean, 2)))
print(paste("t-test: t =", round(gov_contrast$statistic, 2), 
            ", p =", format(gov_contrast$p.value, scientific = TRUE)))

# Cohen's d for government contrast
gov_cohens_d <- cohens_d(policy_framework_assessment_rating ~ stakeholder_group, 
                        data = df_gov_contrast)
print(paste("Cohen's d (Government vs Non-Government):", round(gov_cohens_d$Cohens_d, 3)))

# Private sector (VC + LP) vs Public sector contrast
df_sector_contrast <- df %>%
  filter(stakeholder %in% c("Venture Capital", "Limited Partner", "Government Agency")) %>%
  mutate(
    sector = ifelse(stakeholder == "Government Agency", "Public", "Private")
  )

if(nrow(df_sector_contrast) > 50) {
  sector_contrast <- t.test(policy_framework_assessment_rating ~ sector, 
                           data = df_sector_contrast)
  
  private_mean <- mean(df_sector_contrast$policy_framework_assessment_rating[df_sector_contrast$sector == "Private"])
  public_mean <- mean(df_sector_contrast$policy_framework_assessment_rating[df_sector_contrast$sector == "Public"])
  
  print("Private vs Public sector contrast:")
  print(paste("Private sector mean:", round(private_mean, 2)))
  print(paste("Public sector mean:", round(public_mean, 2)))
  print(paste("t-test: t =", round(sector_contrast$statistic, 2), 
              ", p =", format(sector_contrast$p.value, scientific = TRUE)))
}

# Analysis controlling for experience
if(sum(!is.na(df$experience_years)) > 100) {
  df_experience <- df %>%
    filter(!is.na(experience_years))
  
  # ANCOVA with experience as covariate
  ancova_model <- lm(policy_framework_assessment_rating ~ stakeholder + experience_years, 
                    data = df_experience)
  ancova_result <- Anova(ancova_model, type = "III")
  
  print("ANCOVA controlling for experience:")
  print(ancova_result)
  
  # Adjusted means
  adjusted_means <- emmeans(ancova_model, ~ stakeholder)
  print("Experience-adjusted means:")
  print(adjusted_means)
}

# Regional analysis (if available)
if(!all(is.na(df$geographic_region))) {
  regional_breakdown <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(stakeholder, geographic_region) %>%
    summarise(
      n = n(),
      mean_policy = round(mean(policy_framework_assessment_rating), 2),
      sd_policy = round(sd(policy_framework_assessment_rating), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(stakeholder, geographic_region)
  
  if(nrow(regional_breakdown) > 0) {
    print("Policy assessments by stakeholder and region (n ≥ 10):")
    print(regional_breakdown)
  }
}

# Distribution analysis
print("Distribution of policy framework ratings by stakeholder:")
distribution_analysis <- df %>%
  group_by(stakeholder) %>%
  count(policy_framework_assessment_rating) %>%
  group_by(stakeholder) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  select(stakeholder, rating = policy_framework_assessment_rating, percentage) %>%
  pivot_wider(names_from = rating, values_from = percentage, 
              names_prefix = "Rating_", values_fill = 0)

print(distribution_analysis)

# Identify extreme groups
highest_rating_group <- desc_stats %>% slice_max(mean_policy, n = 1)
lowest_rating_group <- desc_stats %>% slice_min(mean_policy, n = 1)

print(paste("Highest policy ratings:", highest_rating_group$stakeholder,
            "(M =", highest_rating_group$mean_policy, ", SD =", highest_rating_group$sd_policy, ")"))
print(paste("Lowest policy ratings:", lowest_rating_group$stakeholder,
            "(M =", lowest_rating_group$mean_policy, ", SD =", lowest_rating_group$sd_policy, ")"))

# Effect size interpretation
eta_magnitude <- eta_squared_result$Eta2_partial[1]
eta_interpretation <- case_when(
  eta_magnitude < 0.01 ~ "Negligible",
  eta_magnitude < 0.06 ~ "Small",
  eta_magnitude < 0.14 ~ "Medium",
  TRUE ~ "Large"
)

print(paste("η² effect size interpretation:", eta_interpretation))

# Two-way ANOVA with region (if sufficient data)
if(!all(is.na(df$geographic_region))) {
  df_region <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    filter(n() >= 30) %>%
    ungroup() %>%
    mutate(geographic_region = droplevels(geographic_region))
  
  if(length(levels(df_region$geographic_region)) > 1 && nrow(df_region) > 100) {
    twoway_model <- lm(policy_framework_assessment_rating ~ stakeholder * geographic_region, 
                      data = df_region)
    twoway_result <- Anova(twoway_model, type = "III")
    
    print("Two-way ANOVA: Stakeholder × Region")
    print(twoway_result)
  }
}

# Expected: F(3,1303) = 15.6, p < .001, η² = .035