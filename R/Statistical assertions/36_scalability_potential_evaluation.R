# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 36: "Scalability potential evaluations varied by sector (F(4,1302) = 9.7, p < .001, η² = .029)"
# Purpose: One-way ANOVA testing differences in scalability potential across sector focus areas

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)
library(multcomp)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SCALABILITY <- "scalability_potential_rating"
COL_SECTOR <- "sector_focus"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    scalability_potential_rating = as.numeric(scalability_potential_rating),
    sector_focus = factor(sector_focus),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(scalability_potential_rating), !is.na(sector_focus)) %>%
  # Ensure ratings are within expected scale
  filter(scalability_potential_rating >= 1, scalability_potential_rating <= 7)

# Count observations by sector
sector_counts <- df %>%
  count(sector_focus, sort = TRUE)
print("Sample sizes by sector:")
print(sector_counts)

# Filter to sectors with adequate sample sizes (n ≥ 20)
sectors_to_include <- sector_counts %>%
  filter(n >= 20) %>%
  pull(sector_focus)

df_filtered <- df %>%
  filter(sector_focus %in% sectors_to_include) %>%
  mutate(sector_focus = droplevels(sector_focus))

print(paste("Analysis includes", length(sectors_to_include), "sectors with n ≥ 20"))

# Descriptive statistics by sector
desc_stats <- df_filtered %>%
  group_by(sector_focus) %>%
  summarise(
    n = n(),
    mean_scalability = round(mean(scalability_potential_rating), 2),
    sd_scalability = round(sd(scalability_potential_rating), 2),
    median_scalability = round(median(scalability_potential_rating), 2),
    se_scalability = round(sd(scalability_potential_rating) / sqrt(n()), 3),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_scalability))

print("Scalability potential by sector:")
print(desc_stats)

# Check ANOVA assumptions
# Levene's test for homogeneity of variances
levene_test <- leveneTest(scalability_potential_rating ~ sector_focus, data = df_filtered)
print("Levene's test for homogeneity of variances:")
print(levene_test)

# Normality check by group (sample from each group if large)
shapiro_results <- df_filtered %>%
  group_by(sector_focus) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n() > 5000, 
                      shapiro.test(sample(scalability_potential_rating, 5000))$p.value,
                      shapiro.test(scalability_potential_rating)$p.value),
    .groups = "drop"
  )
print("Shapiro-Wilk normality tests by sector:")
print(shapiro_results)

# One-way ANOVA
anova_model <- lm(scalability_potential_rating ~ sector_focus, data = df_filtered)
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
  emmeans_result <- emmeans(anova_model, ~ sector_focus)
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
tukey_result <- TukeyHSD(aov(scalability_potential_rating ~ sector_focus, data = df_filtered))
print("Tukey HSD comparisons:")
print(tukey_result)

# Breakdown by stakeholder within each sector (if sufficient data)
stakeholder_sector_breakdown <- df_filtered %>%
  group_by(sector_focus, stakeholder) %>%
  summarise(
    n = n(),
    mean_scalability = round(mean(scalability_potential_rating), 2),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(sector_focus, desc(mean_scalability))

if(nrow(stakeholder_sector_breakdown) > 0) {
  print("Scalability by sector and stakeholder (n ≥ 10):")
  print(stakeholder_sector_breakdown)
}

# Contrast analysis: Technology sectors vs Non-technology sectors
if("Technology" %in% levels(df_filtered$sector_focus)) {
  df_contrast <- df_filtered %>%
    mutate(
      sector_type = ifelse(grepl("Technology|Tech|Digital|AI|Software", 
                                sector_focus, ignore.case = TRUE),
                          "Technology", "Non-Technology")
    )
  
  contrast_test <- t.test(scalability_potential_rating ~ sector_type, data = df_contrast)
  print("Technology vs Non-Technology sectors contrast:")
  print(paste("Technology mean:", round(mean(df_contrast$scalability_potential_rating[df_contrast$sector_type == "Technology"]), 2)))
  print(paste("Non-Technology mean:", round(mean(df_contrast$scalability_potential_rating[df_contrast$sector_type == "Non-Technology"]), 2)))
  print(paste("t-test: t =", round(contrast_test$statistic, 2), 
              ", p =", format(contrast_test$p.value, scientific = TRUE)))
}

# Expected: F(4,1302) = 9.7, p < .001, η² = .029