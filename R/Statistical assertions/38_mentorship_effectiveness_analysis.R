# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 38: "Mentorship effectiveness ratings were highest among entrepreneurs (M = 6.2, SD = 1.1) vs others (M = 5.4, SD = 1.3), t(1305) = 8.9, p < .001"
# Purpose: Independent samples t-test comparing mentorship effectiveness ratings between entrepreneurs and other stakeholders

library(tidyverse)
library(janitor)
library(effectsize)
library(car)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MENTORSHIP_EFFECTIVENESS <- "mentorship_effectiveness_rating"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    mentorship_effectiveness_rating = as.numeric(mentorship_effectiveness_rating),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(mentorship_effectiveness_rating), !is.na(stakeholder)) %>%
  # Ensure ratings are within expected scale
  filter(mentorship_effectiveness_rating >= 1, mentorship_effectiveness_rating <= 7)

# Create entrepreneur vs non-entrepreneur grouping
df_grouped <- df %>%
  mutate(
    group = ifelse(stakeholder == "Entrepreneur", "Entrepreneur", "Other Stakeholders"),
    group = factor(group, levels = c("Entrepreneur", "Other Stakeholders"))
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
    mean_mentorship = round(mean(mentorship_effectiveness_rating), 2),
    sd_mentorship = round(sd(mentorship_effectiveness_rating), 2),
    median_mentorship = round(median(mentorship_effectiveness_rating), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_mentorship))

print("Mentorship effectiveness by stakeholder group:")
print(stakeholder_breakdown)

# Descriptive statistics for entrepreneur vs others comparison
desc_stats <- df_grouped %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_rating = round(mean(mentorship_effectiveness_rating), 2),
    sd_rating = round(sd(mentorship_effectiveness_rating), 2),
    median_rating = round(median(mentorship_effectiveness_rating), 2),
    se_rating = round(sd(mentorship_effectiveness_rating) / sqrt(n()), 3),
    ci_lower = round(mean(mentorship_effectiveness_rating) - 1.96 * se_rating, 2),
    ci_upper = round(mean(mentorship_effectiveness_rating) + 1.96 * se_rating, 2),
    .groups = "drop"
  )

print("Descriptive statistics for entrepreneurs vs others:")
print(desc_stats)

# Check assumptions for t-test
# Levene's test for equality of variances
levene_test <- leveneTest(mentorship_effectiveness_rating ~ group, data = df_grouped)
print("Levene's test for equality of variances:")
print(levene_test)

# Normality tests for each group
entrepreneur_data <- df_grouped$mentorship_effectiveness_rating[df_grouped$group == "Entrepreneur"]
other_data <- df_grouped$mentorship_effectiveness_rating[df_grouped$group == "Other Stakeholders"]

# Sample for Shapiro test if groups are large
entrepreneur_sample <- if(length(entrepreneur_data) > 5000) sample(entrepreneur_data, 5000) else entrepreneur_data
other_sample <- if(length(other_data) > 5000) sample(other_data, 5000) else other_data

shapiro_entrepreneur <- shapiro.test(entrepreneur_sample)
shapiro_other <- shapiro.test(other_sample)

print("Normality tests:")
print(paste("Entrepreneurs: W =", round(shapiro_entrepreneur$statistic, 3),
            ", p =", format(shapiro_entrepreneur$p.value, scientific = TRUE)))
print(paste("Others: W =", round(shapiro_other$statistic, 3),
            ", p =", format(shapiro_other$p.value, scientific = TRUE)))

# Independent samples t-test
# Choose appropriate test based on variance equality
var_equal <- levene_test$`Pr(>F)`[1] > 0.05

t_test_result <- t.test(mentorship_effectiveness_rating ~ group, 
                       data = df_grouped, 
                       var.equal = var_equal,
                       conf.level = 0.95)

print("Independent samples t-test results:")
print(paste("t(", round(t_test_result$parameter, 0), ") =", round(t_test_result$statistic, 2)))
print(paste("p-value:", format(t_test_result$p.value, scientific = TRUE)))
print(paste("95% CI for difference: [", round(t_test_result$conf.int[1], 3), ", ",
            round(t_test_result$conf.int[2], 3), "]", sep=""))

# Effect size (Cohen's d)
cohens_d <- cohens_d(mentorship_effectiveness_rating ~ group, data = df_grouped)
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
welch_test <- t.test(mentorship_effectiveness_rating ~ group, 
                    data = df_grouped, 
                    var.equal = FALSE)

print("Welch's t-test (unequal variances):")
print(paste("t(", round(welch_test$parameter, 1), ") =", round(welch_test$statistic, 2)))
print(paste("p-value:", format(welch_test$p.value, scientific = TRUE)))

# Mann-Whitney U test (non-parametric alternative)
wilcox_test <- wilcox.test(mentorship_effectiveness_rating ~ group, 
                          data = df_grouped,
                          conf.int = TRUE)

print("Mann-Whitney U test (non-parametric):")
print(paste("W =", wilcox_test$statistic))
print(paste("p-value:", format(wilcox_test$p.value, scientific = TRUE)))

# Distribution comparison
print("Distribution of ratings by group:")
entrepreneur_dist <- table(entrepreneur_data)
other_dist <- table(other_data)
print("Entrepreneurs:")
print(entrepreneur_dist)
print("Others:")
print(other_dist)

# Percentage breakdown by rating level
entrepreneur_props <- round(prop.table(entrepreneur_dist) * 100, 1)
other_props <- round(prop.table(other_dist) * 100, 1)
print("Percentage distributions:")
print("Entrepreneurs (%):")
print(entrepreneur_props)
print("Others (%):")
print(other_props)

# Additional analysis: ANOVA for all stakeholder groups
anova_model <- lm(mentorship_effectiveness_rating ~ stakeholder, data = df)
anova_result <- summary(anova_model)
print("One-way ANOVA for all stakeholder groups:")
print(paste("F(", anova_result$df[1], ",", anova_result$df[2], ") =",
            round(anova_result$fstatistic[1], 2)))
print(paste("p-value:", format(pf(anova_result$fstatistic[1], 
                                 anova_result$df[1], 
                                 anova_result$df[2], 
                                 lower.tail = FALSE), scientific = TRUE)))

# Expected: Entrepreneurs M = 6.2, SD = 1.1; Others M = 5.4, SD = 1.3; t(1305) = 8.9, p < .001