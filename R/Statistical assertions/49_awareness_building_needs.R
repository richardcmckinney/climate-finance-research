# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 49: "Awareness building needs were rated highest by entrepreneurs (M = 5.8, SD = 1.2) compared to other stakeholders"
# Purpose: Descriptive analysis and group comparisons of awareness building needs ratings across stakeholder groups

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_AWARENESS_BUILDING <- "awareness_building_needs_rating"
COL_STAKEHOLDER <- "stakeholder"
COL_SECTOR <- "sector_focus"
COL_COMPANY_STAGE <- "company_development_stage"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    awareness_building_needs_rating = as.numeric(awareness_building_needs_rating),
    stakeholder = factor(stakeholder, levels = c("Entrepreneur", "Venture Capital", 
                                               "Government Agency", "Limited Partner")),
    sector_focus = factor(sector_focus),
    company_development_stage = factor(company_development_stage)
  ) %>%
  filter(!is.na(awareness_building_needs_rating), !is.na(stakeholder)) %>%
  # Ensure ratings are within expected scale
  filter(awareness_building_needs_rating >= 1, awareness_building_needs_rating <= 7)

# Overall descriptive statistics
overall_stats <- df %>%
  summarise(
    n = n(),
    mean_awareness = round(mean(awareness_building_needs_rating), 2),
    sd_awareness = round(sd(awareness_building_needs_rating), 2),
    median_awareness = round(median(awareness_building_needs_rating), 2),
    q1 = round(quantile(awareness_building_needs_rating, 0.25), 2),
    q3 = round(quantile(awareness_building_needs_rating, 0.75), 2),
    .groups = "drop"
  )

print("Overall awareness building needs ratings:")
print(overall_stats)

# Descriptive statistics by stakeholder
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_awareness = round(mean(awareness_building_needs_rating), 2),
    sd_awareness = round(sd(awareness_building_needs_rating), 2),
    median_awareness = round(median(awareness_building_needs_rating), 2),
    se_awareness = round(sd(awareness_building_needs_rating) / sqrt(n()), 3),
    ci_lower = round(mean_awareness - 1.96 * se_awareness, 2),
    ci_upper = round(mean_awareness + 1.96 * se_awareness, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_awareness))

print("Awareness building needs by stakeholder:")
print(stakeholder_stats)

# Highlight entrepreneurs specifically
entrepreneur_stats <- stakeholder_stats %>%
  filter(stakeholder == "Entrepreneur")

print("Entrepreneurs specifically:")
print(paste("Mean:", entrepreneur_stats$mean_awareness))
print(paste("SD:", entrepreneur_stats$sd_awareness))
print(paste("95% CI: [", entrepreneur_stats$ci_lower, ", ", entrepreneur_stats$ci_upper, "]", sep=""))

# Compare entrepreneurs to each other stakeholder group
other_stakeholders <- c("Venture Capital", "Government Agency", "Limited Partner")

for(stakeholder_type in other_stakeholders) {
  entrepreneur_data <- df$awareness_building_needs_rating[df$stakeholder == "Entrepreneur"]
  other_data <- df$awareness_building_needs_rating[df$stakeholder == stakeholder_type]
  
  t_test <- t.test(entrepreneur_data, other_data)
  cohens_d <- cohens_d(entrepreneur_data, other_data)
  
  other_mean <- mean(other_data)
  other_sd <- sd(other_data)
  
  print(paste("Entrepreneurs vs", stakeholder_type, ":"))
  print(paste("  ", stakeholder_type, "mean:", round(other_mean, 2), "(SD =", round(other_sd, 2), ")"))
  print(paste("  t-test: t =", round(t_test$statistic, 2), ", p =", format(t_test$p.value, scientific = TRUE)))
  print(paste("  Cohen's d =", round(cohens_d, 3)))
}

# One-way ANOVA across all stakeholders
anova_model <- lm(awareness_building_needs_rating ~ stakeholder, data = df)
anova_result <- Anova(anova_model, type = "III")

print("One-way ANOVA across stakeholders:")
print(anova_result)

# Effect size for ANOVA
eta_squared_result <- eta_squared(anova_result)
print(paste("η² =", round(eta_squared_result$Eta2_partial[1], 3)))

# Post-hoc comparisons if significant
if(anova_result$`Pr(>F)`[1] < 0.05) {
  emmeans_result <- emmeans(anova_model, ~ stakeholder)
  pairwise_comp <- pairs(emmeans_result, adjust = "bonferroni")
  
  print("Post-hoc pairwise comparisons (Bonferroni corrected):")
  print(pairwise_comp)
}

# Distribution analysis by stakeholder
print("Distribution of awareness building ratings by stakeholder:")
distribution_table <- table(df$stakeholder, df$awareness_building_needs_rating)
print(distribution_table)

print("Row percentages:")
print(round(prop.table(distribution_table, 1) * 100, 1))

# Entrepreneurs vs all others combined
df_entrepreneur_vs_others <- df %>%
  mutate(
    group = ifelse(stakeholder == "Entrepreneur", "Entrepreneur", "Other Stakeholders"),
    group = factor(group, levels = c("Entrepreneur", "Other Stakeholders"))
  )

group_comparison <- df_entrepreneur_vs_others %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_awareness = round(mean(awareness_building_needs_rating), 2),
    sd_awareness = round(sd(awareness_building_needs_rating), 2),
    .groups = "drop"
  )

print("Entrepreneurs vs all other stakeholders combined:")
print(group_comparison)

# T-test: Entrepreneurs vs others
entrepreneur_vs_others_test <- t.test(awareness_building_needs_rating ~ group, 
                                     data = df_entrepreneur_vs_others)

print("t-test: Entrepreneurs vs others combined:")
print(paste("t =", round(entrepreneur_vs_others_test$statistic, 2)))
print(paste("p =", format(entrepreneur_vs_others_test$p.value, scientific = TRUE)))

# Effect size for entrepreneur vs others
entrepreneur_vs_others_d <- cohens_d(awareness_building_needs_rating ~ group, 
                                    data = df_entrepreneur_vs_others)
print(paste("Cohen's d =", round(entrepreneur_vs_others_d$Cohens_d, 3)))

# Analysis by sector within entrepreneurs
if(!all(is.na(df$sector_focus))) {
  entrepreneur_by_sector <- df %>%
    filter(stakeholder == "Entrepreneur", !is.na(sector_focus)) %>%
    group_by(sector_focus) %>%
    summarise(
      n = n(),
      mean_awareness = round(mean(awareness_building_needs_rating), 2),
      sd_awareness = round(sd(awareness_building_needs_rating), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_awareness))
  
  if(nrow(entrepreneur_by_sector) > 0) {
    print("Awareness building needs among entrepreneurs by sector (n ≥ 10):")
    print(entrepreneur_by_sector)
  }
}

# Analysis by company stage within entrepreneurs
if(!all(is.na(df$company_development_stage))) {
  entrepreneur_by_stage <- df %>%
    filter(stakeholder == "Entrepreneur", !is.na(company_development_stage)) %>%
    group_by(company_development_stage) %>%
    summarise(
      n = n(),
      mean_awareness = round(mean(awareness_building_needs_rating), 2),
      sd_awareness = round(sd(awareness_building_needs_rating), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(mean_awareness))
  
  if(nrow(entrepreneur_by_stage) > 0) {
    print("Awareness building needs among entrepreneurs by company stage (n ≥ 10):")
    print(entrepreneur_by_stage)
  }
}

# Proportion analysis: High awareness needs (ratings 5-7)
high_awareness_analysis <- df %>%
  mutate(high_awareness_need = as.integer(awareness_building_needs_rating >= 5)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    high_awareness_count = sum(high_awareness_need),
    proportion_high = round(mean(high_awareness_need) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(proportion_high))

print("Proportion with high awareness building needs (ratings 5-7):")
print(high_awareness_analysis)

# Chi-square test for high vs low awareness needs
high_awareness_table <- table(df$stakeholder, df$awareness_building_needs_rating >= 5)
chi_awareness <- chisq.test(high_awareness_table)

print("Chi-square test for high awareness needs across stakeholders:")
print(paste("χ² =", round(chi_awareness$statistic, 2)))
print(paste("p =", format(chi_awareness$p.value, scientific = TRUE)))

# Cramér's V
cramers_v <- sqrt(chi_awareness$statistic / (sum(high_awareness_table) * 
                                            (min(dim(high_awareness_table)) - 1)))
print(paste("Cramér's V =", round(cramers_v, 3)))

# Confidence intervals for entrepreneurs
entrepreneur_data <- df$awareness_building_needs_rating[df$stakeholder == "Entrepreneur"]
entrepreneur_ci <- t.test(entrepreneur_data)$conf.int

print("95% CI for entrepreneur awareness building needs:")
print(paste("[", round(entrepreneur_ci[1], 2), ", ", round(entrepreneur_ci[2], 2), "]", sep=""))

# Non-parametric analysis (Kruskal-Wallis)
kruskal_test <- kruskal.test(awareness_building_needs_rating ~ stakeholder, data = df)
print("Kruskal-Wallis test (non-parametric):")
print(paste("H =", round(kruskal_test$statistic, 2)))
print(paste("p =", format(kruskal_test$p.value, scientific = TRUE)))

# Mann-Whitney U test: Entrepreneurs vs others
wilcox_test <- wilcox.test(awareness_building_needs_rating ~ group, 
                          data = df_entrepreneur_vs_others)
print("Mann-Whitney U test (Entrepreneurs vs others):")
print(paste("W =", wilcox_test$statistic))
print(paste("p =", format(wilcox_test$p.value, scientific = TRUE)))

# Summary statistics comparison
print("Summary: Entrepreneur ratings compared to overall sample:")
entrepreneur_vs_overall <- data.frame(
  Metric = c("Mean", "SD", "Median"),
  Entrepreneurs = c(entrepreneur_stats$mean_awareness, 
                   entrepreneur_stats$sd_awareness, 
                   entrepreneur_stats$median_awareness),
  Overall_Sample = c(overall_stats$mean_awareness, 
                    overall_stats$sd_awareness, 
                    overall_stats$median_awareness)
)
print(entrepreneur_vs_overall)

# Expected: Entrepreneurs M = 5.8, SD = 1.2 (highest among stakeholders)