# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 56: "Stakeholder engagement strategies effectiveness varied by stakeholder type (F(3,1303) = 11.8, p < .001, η² = .027)"
# Purpose: One-way ANOVA testing differences in stakeholder engagement strategies effectiveness across stakeholder types

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_ENGAGEMENT_EFFECTIVENESS <- "stakeholder_engagement_strategies_effectiveness"
COL_STAKEHOLDER <- "stakeholder"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_EXPERIENCE <- "experience_years"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder_engagement_strategies_effectiveness = as.numeric(stakeholder_engagement_strategies_effectiveness),
    stakeholder = factor(stakeholder, levels = c("Venture Capital", "Government Agency", 
                                               "Entrepreneur", "Limited Partner")),
    organization_size_category = factor(organization_size_category),
    experience_years = as.numeric(experience_years)
  ) %>%
  filter(!is.na(stakeholder_engagement_strategies_effectiveness), !is.na(stakeholder)) %>%
  # Ensure ratings are within expected scale
  filter(stakeholder_engagement_strategies_effectiveness >= 1, stakeholder_engagement_strategies_effectiveness <= 10)

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
    mean_engagement = round(mean(stakeholder_engagement_strategies_effectiveness), 2),
    sd_engagement = round(sd(stakeholder_engagement_strategies_effectiveness), 2),
    median_engagement = round(median(stakeholder_engagement_strategies_effectiveness), 2),
    se_engagement = round(sd(stakeholder_engagement_strategies_effectiveness) / sqrt(n()), 3),
    ci_lower = round(mean_engagement - 1.96 * se_engagement, 2),
    ci_upper = round(mean_engagement + 1.96 * se_engagement, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_engagement))

print("Stakeholder engagement strategies effectiveness by stakeholder type:")
print(desc_stats)

# Check ANOVA assumptions
# Levene's test for homogeneity of variances
levene_test <- leveneTest(stakeholder_engagement_strategies_effectiveness ~ stakeholder, data = df)
print("Levene's test for homogeneity of variances:")
print(levene_test)

# Normality check by group
shapiro_results <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n() > 5000, 
                      shapiro.test(sample(stakeholder_engagement_strategies_effectiveness, 5000))$p.value,
                      shapiro.test(stakeholder_engagement_strategies_effectiveness)$p.value),
    .groups = "drop"
  )
print("Shapiro-Wilk normality tests by stakeholder:")
print(shapiro_results)

# One-way ANOVA
anova_model <- lm(stakeholder_engagement_strategies_effectiveness ~ stakeholder, data = df)
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
tukey_result <- TukeyHSD(aov(stakeholder_engagement_strategies_effectiveness ~ stakeholder, data = df))
print("Tukey HSD comparisons:")
print(tukey_result)

# Kruskal-Wallis test (non-parametric alternative)
kruskal_test <- kruskal.test(stakeholder_engagement_strategies_effectiveness ~ stakeholder, data = df)
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
# Investment professionals (VC + LP) vs Others
df_investment_contrast <- df %>%
  mutate(
    stakeholder_group = ifelse(stakeholder %in% c("Venture Capital", "Limited Partner"), 
                              "Investment Professionals", "Others")
  )

investment_contrast <- t.test(stakeholder_engagement_strategies_effectiveness ~ stakeholder_group, 
                             data = df_investment_contrast)

investment_mean <- mean(df$stakeholder_engagement_strategies_effectiveness[df$stakeholder %in% c("Venture Capital", "Limited Partner")])
others_mean <- mean(df$stakeholder_engagement_strategies_effectiveness[!df$stakeholder %in% c("Venture Capital", "Limited Partner")])

print("Investment Professionals vs Others contrast:")
print(paste("Investment professionals mean:", round(investment_mean, 2)))
print(paste("Others mean:", round(others_mean, 2)))
print(paste("t-test: t =", round(investment_contrast$statistic, 2), 
            ", p =", format(investment_contrast$p.value, scientific = TRUE)))

# Cohen's d for investment contrast
investment_cohens_d <- cohens_d(stakeholder_engagement_strategies_effectiveness ~ stakeholder_group, 
                               data = df_investment_contrast)
print(paste("Cohen's d (Investment vs Others):", round(investment_cohens_d$Cohens_d, 3)))

# Government vs Private sector contrast
df_sector_contrast <- df %>%
  mutate(
    sector = ifelse(stakeholder == "Government Agency", "Public Sector", "Private Sector")
  )

sector_contrast <- t.test(stakeholder_engagement_strategies_effectiveness ~ sector, 
                         data = df_sector_contrast)

public_mean <- mean(df$stakeholder_engagement_strategies_effectiveness[df$stakeholder == "Government Agency"])
private_mean <- mean(df$stakeholder_engagement_strategies_effectiveness[df$stakeholder != "Government Agency"])

print("Public vs Private sector contrast:")
print(paste("Public sector mean:", round(public_mean, 2)))
print(paste("Private sector mean:", round(private_mean, 2)))
print(paste("t-test: t =", round(sector_contrast$statistic, 2), 
            ", p =", format(sector_contrast$p.value, scientific = TRUE)))

# Analysis controlling for experience
if(sum(!is.na(df$experience_years)) > 100) {
  df_experience <- df %>%
    filter(!is.na(experience_years))
  
  # ANCOVA with experience as covariate
  ancova_model <- lm(stakeholder_engagement_strategies_effectiveness ~ stakeholder + experience_years, 
                    data = df_experience)
  ancova_result <- Anova(ancova_model, type = "III")
  
  print("ANCOVA controlling for experience:")
  print(ancova_result)
  
  # Adjusted means
  adjusted_means <- emmeans(ancova_model, ~ stakeholder)
  print("Experience-adjusted means:")
  print(adjusted_means)
}

# Analysis by organization size within stakeholder type
if(!all(is.na(df$organization_size_category))) {
  size_stakeholder_breakdown <- df %>%
    filter(!is.na(organization_size_category)) %>%
    group_by(stakeholder, organization_size_category) %>%
    summarise(
      n = n(),
      mean_engagement = round(mean(stakeholder_engagement_strategies_effectiveness), 2),
      sd_engagement = round(sd(stakeholder_engagement_strategies_effectiveness), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(stakeholder, desc(mean_engagement))
  
  if(nrow(size_stakeholder_breakdown) > 0) {
    print("Engagement effectiveness by stakeholder and organization size (n ≥ 10):")
    print(size_stakeholder_breakdown)
  }
}

# Distribution analysis
print("Distribution of engagement effectiveness ratings by stakeholder:")
distribution_analysis <- df %>%
  group_by(stakeholder) %>%
  count(stakeholder_engagement_strategies_effectiveness) %>%
  group_by(stakeholder) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  select(stakeholder, rating = stakeholder_engagement_strategies_effectiveness, percentage) %>%
  pivot_wider(names_from = rating, values_from = percentage, 
              names_prefix = "Rating_", values_fill = 0)

print(distribution_analysis)

# Identify extreme groups
highest_engagement_group <- desc_stats %>% slice_max(mean_engagement, n = 1)
lowest_engagement_group <- desc_stats %>% slice_min(mean_engagement, n = 1)

print(paste("Highest engagement effectiveness:", highest_engagement_group$stakeholder,
            "(M =", highest_engagement_group$mean_engagement, ", SD =", highest_engagement_group$sd_engagement, ")"))
print(paste("Lowest engagement effectiveness:", lowest_engagement_group$stakeholder,
            "(M =", lowest_engagement_group$mean_engagement, ", SD =", lowest_engagement_group$sd_engagement, ")"))

# Effect size interpretation
eta_magnitude <- eta_squared_result$Eta2_partial[1]
eta_interpretation <- case_when(
  eta_magnitude < 0.01 ~ "Negligible",
  eta_magnitude < 0.06 ~ "Small",
  eta_magnitude < 0.14 ~ "Medium",
  TRUE ~ "Large"
)

print(paste("η² effect size interpretation:", eta_interpretation))

# High engagement threshold analysis (ratings ≥ 8)
high_engagement_analysis <- df %>%
  mutate(high_engagement = as.integer(stakeholder_engagement_strategies_effectiveness >= 8)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    high_engagement_count = sum(high_engagement),
    proportion_high = round(mean(high_engagement) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(proportion_high))

print("Proportion with high engagement effectiveness (ratings ≥ 8):")
print(high_engagement_analysis)

# Chi-square test for high vs low engagement effectiveness
high_engagement_table <- table(df$stakeholder, df$stakeholder_engagement_strategies_effectiveness >= 8)
chi_engagement <- chisq.test(high_engagement_table)

print("Chi-square test for high engagement effectiveness across stakeholders:")
print(paste("χ² =", round(chi_engagement$statistic, 2)))
print(paste("p =", format(chi_engagement$p.value, scientific = TRUE)))

# Cramér's V
cramers_v <- sqrt(chi_engagement$statistic / (sum(high_engagement_table) * 
                                            (min(dim(high_engagement_table)) - 1)))
print(paste("Cramér's V =", round(cramers_v, 3)))

# Variance analysis by stakeholder
variance_analysis <- df %>%
  group_by(stakeholder) %>%
  summarise(
    variance = round(var(stakeholder_engagement_strategies_effectiveness), 3),
    range = max(stakeholder_engagement_strategies_effectiveness) - min(stakeholder_engagement_strategies_effectiveness),
    iqr = round(IQR(stakeholder_engagement_strategies_effectiveness), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(variance))

print("Variability in engagement effectiveness by stakeholder:")
print(variance_analysis)

# Two-way ANOVA with organization size (if sufficient data)
if(!all(is.na(df$organization_size_category))) {
  df_twoway <- df %>%
    filter(!is.na(organization_size_category)) %>%
    group_by(stakeholder, organization_size_category) %>%
    filter(n() >= 5) %>%
    ungroup() %>%
    mutate(organization_size_category = droplevels(organization_size_category))
  
  if(length(levels(df_twoway$organization_size_category)) > 1) {
    twoway_model <- lm(stakeholder_engagement_strategies_effectiveness ~ 
                      stakeholder * organization_size_category, 
                      data = df_twoway)
    twoway_result <- Anova(twoway_model, type = "III")
    
    print("Two-way ANOVA: Stakeholder × Organization Size")
    print(twoway_result)
  }
}

# Planned contrasts using specific hypotheses
contrast_matrix <- matrix(c(
  1, -1, 0, 0,  # VC vs Government
  0, 0, 1, -1,  # Entrepreneur vs LP
  0.5, -1, 0.5, 0  # Private (VC+Entrepreneur) vs Government
), nrow = 3, byrow = TRUE)

rownames(contrast_matrix) <- c("VC_vs_Gov", "Entrepreneur_vs_LP", "Private_vs_Gov")
colnames(contrast_matrix) <- levels(df$stakeholder)

print("Planned contrast matrix:")
print(contrast_matrix)

# Apply contrasts
for(i in 1:nrow(contrast_matrix)) {
  contrast_name <- rownames(contrast_matrix)[i]
  contrast_vector <- contrast_matrix[i, ]
  
  # Manual contrast calculation
  group_means <- tapply(df$stakeholder_engagement_strategies_effectiveness, df$stakeholder, mean)
  contrast_value <- sum(contrast_vector * group_means)
  
  print(paste("Contrast", contrast_name, ":", round(contrast_value, 3)))
}

# Expected: F(3,1303) = 11.8, p < .001, η² = .027