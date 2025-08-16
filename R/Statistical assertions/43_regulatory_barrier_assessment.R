# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 43: "Regulatory barriers were rated most significant by government agencies (M = 6.1, SD = 0.9) vs other groups (M = 4.8, SD = 1.4), t(1305) = 11.2, p < .001"
# Purpose: Independent samples t-test comparing regulatory barrier ratings between government agencies and other stakeholder groups

library(tidyverse)
library(janitor)
library(effectsize)
library(car)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_REGULATORY_BARRIERS <- "regulatory_barrier_significance_rating"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    regulatory_barrier_significance_rating = as.numeric(regulatory_barrier_significance_rating),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(regulatory_barrier_significance_rating), !is.na(stakeholder)) %>%
  # Ensure ratings are within expected scale
  filter(regulatory_barrier_significance_rating >= 1, regulatory_barrier_significance_rating <= 7)

# Create government vs non-government grouping
df_grouped <- df %>%
  mutate(
    group = ifelse(stakeholder == "Government Agency", "Government Agency", "Other Stakeholders"),
    group = factor(group, levels = c("Government Agency", "Other Stakeholders"))
  )

# Sample sizes by group
group_counts <- df_grouped %>%
  count(group)
print("Sample sizes by group:")
print(group_counts)

# Detailed stakeholder breakdown
stakeholder_breakdown <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_regulatory = round(mean(regulatory_barrier_significance_rating), 2),
    sd_regulatory = round(sd(regulatory_barrier_significance_rating), 2),
    median_regulatory = round(median(regulatory_barrier_significance_rating), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_regulatory))

print("Regulatory barrier ratings by stakeholder group:")
print(stakeholder_breakdown)

# Descriptive statistics for government vs others comparison
desc_stats <- df_grouped %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_rating = round(mean(regulatory_barrier_significance_rating), 2),
    sd_rating = round(sd(regulatory_barrier_significance_rating), 2),
    median_rating = round(median(regulatory_barrier_significance_rating), 2),
    se_rating = round(sd(regulatory_barrier_significance_rating) / sqrt(n()), 3),
    ci_lower = round(mean(regulatory_barrier_significance_rating) - 1.96 * se_rating, 2),
    ci_upper = round(mean(regulatory_barrier_significance_rating) + 1.96 * se_rating, 2),
    .groups = "drop"
  )

print("Descriptive statistics for government agencies vs others:")
print(desc_stats)

# Check assumptions for t-test
# Levene's test for equality of variances
levene_test <- leveneTest(regulatory_barrier_significance_rating ~ group, data = df_grouped)
print("Levene's test for equality of variances:")
print(levene_test)

# Normality tests for each group
govt_data <- df_grouped$regulatory_barrier_significance_rating[df_grouped$group == "Government Agency"]
other_data <- df_grouped$regulatory_barrier_significance_rating[df_grouped$group == "Other Stakeholders"]

# Sample for Shapiro test if groups are large
govt_sample <- if(length(govt_data) > 5000) sample(govt_data, 5000) else govt_data
other_sample <- if(length(other_data) > 5000) sample(other_data, 5000) else other_data

shapiro_govt <- shapiro.test(govt_sample)
shapiro_other <- shapiro.test(other_sample)

print("Normality tests:")
print(paste("Government agencies: W =", round(shapiro_govt$statistic, 3),
            ", p =", format(shapiro_govt$p.value, scientific = TRUE)))
print(paste("Others: W =", round(shapiro_other$statistic, 3),
            ", p =", format(shapiro_other$p.value, scientific = TRUE)))

# Independent samples t-test
# Choose appropriate test based on variance equality
var_equal <- levene_test$`Pr(>F)`[1] > 0.05

t_test_result <- t.test(regulatory_barrier_significance_rating ~ group, 
                       data = df_grouped, 
                       var.equal = var_equal,
                       conf.level = 0.95)

print("Independent samples t-test results:")
print(paste("t(", round(t_test_result$parameter, 0), ") =", round(t_test_result$statistic, 2)))
print(paste("p-value:", format(t_test_result$p.value, scientific = TRUE)))
print(paste("95% CI for difference: [", round(t_test_result$conf.int[1], 3), ", ",
            round(t_test_result$conf.int[2], 3), "]", sep=""))

# Effect size (Cohen's d)
cohens_d <- cohens_d(regulatory_barrier_significance_rating ~ group, data = df_grouped)
print(paste("Cohen's d:", round(cohens_d$Cohens_d, 3)))

# Interpret effect size
d_magnitude <- abs(cohens_d$Cohens_d)
effect_interpretation <- case_when(
  d_magnitude < 0.2 ~ "Negligible",
  d_magnitude < 0.5 ~ "Small",
  d_magnitude < 0.8 ~ "Medium",
  TRUE ~ "Large"
)
print(paste("Effect size interpretation:", effect_interpretation))

# Alternative: Welch's t-test (unequal variances)
welch_test <- t.test(regulatory_barrier_significance_rating ~ group, 
                    data = df_grouped, 
                    var.equal = FALSE)

print("Welch's t-test (unequal variances):")
print(paste("t(", round(welch_test$parameter, 1), ") =", round(welch_test$statistic, 2)))
print(paste("p-value:", format(welch_test$p.value, scientific = TRUE)))

# Mann-Whitney U test (non-parametric alternative)
wilcox_test <- wilcox.test(regulatory_barrier_significance_rating ~ group, 
                          data = df_grouped,
                          conf.int = TRUE)

print("Mann-Whitney U test (non-parametric):")
print(paste("W =", wilcox_test$statistic))
print(paste("p-value:", format(wilcox_test$p.value, scientific = TRUE)))

# Distribution comparison
print("Distribution of ratings by group:")
govt_dist <- table(govt_data)
other_dist <- table(other_data)
print("Government agencies:")
print(govt_dist)
print("Others:")
print(other_dist)

# Percentage breakdown by rating level
govt_props <- round(prop.table(govt_dist) * 100, 1)
other_props <- round(prop.table(other_dist) * 100, 1)
print("Percentage distributions:")
print("Government agencies (%):")
print(govt_props)
print("Others (%):")
print(other_props)

# One-way ANOVA for all stakeholder groups
anova_model <- lm(regulatory_barrier_significance_rating ~ stakeholder, data = df)
anova_result <- summary(anova_model)
print("One-way ANOVA for all stakeholder groups:")
print(paste("F(", anova_result$df[1], ",", anova_result$df[2], ") =",
            round(anova_result$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_result$fstatistic[1], 
                                 anova_result$df[1], 
                                 anova_result$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Post-hoc comparisons for all groups (if significant)
if(pf(anova_result$fstatistic[1], anova_result$df[1], anova_result$df[2], 
      lower.tail = FALSE) < 0.05) {
  
  print("Post-hoc pairwise comparisons (Bonferroni correction):")
  pairwise_result <- pairwise.t.test(df$regulatory_barrier_significance_rating, 
                                    df$stakeholder,
                                    p.adjust.method = "bonferroni")
  print(pairwise_result)
}

# Examine within-group variability
print("Variance analysis:")
govt_var <- var(govt_data)
other_var <- var(other_data)
print(paste("Government agency variance:", round(govt_var, 3)))
print(paste("Other stakeholders variance:", round(other_var, 3)))
print(paste("Variance ratio (F-test):", round(max(govt_var, other_var) / min(govt_var, other_var), 2)))

# F-test for equality of variances
f_test <- var.test(govt_data, other_data)
print(paste("F-test for equal variances: F =", round(f_test$statistic, 2),
            ", p =", format(f_test$p.value, scientific = TRUE)))

# Cross-tabulation by high vs low ratings
df_categories <- df_grouped %>%
  mutate(
    rating_level = ifelse(regulatory_barrier_significance_rating >= 5, 
                         "High Significance (5-7)", 
                         "Low-Moderate Significance (1-4)")
  )

cross_tab <- table(df_categories$group, df_categories$rating_level)
print("Cross-tabulation of group by rating significance level:")
print(cross_tab)
print("Row percentages:")
print(round(prop.table(cross_tab, 1) * 100, 1))

# Chi-square test for high vs low categorization
chi_test <- chisq.test(cross_tab)
print(paste("Chi-square test: χ² =", round(chi_test$statistic, 2),
            ", df =", chi_test$parameter,
            ", p =", format(chi_test$p.value, scientific = TRUE)))

# Expected: Government M = 6.1, SD = 0.9; Others M = 4.8, SD = 1.4; t(1305) = 11.2, p < .001