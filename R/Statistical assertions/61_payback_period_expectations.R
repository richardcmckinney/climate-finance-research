# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 61: "Payback period expectations varied significantly by investment sector (F(5,1301) = 7.2, p < .001, η² = .027)"
# Purpose: One-way ANOVA testing differences in payback period expectations across investment sector categories

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_PAYBACK_PERIOD <- "payback_period_expectations_years"
COL_INVESTMENT_SECTOR <- "investment_sector_focus"
COL_STAKEHOLDER <- "stakeholder"
COL_RISK_TOLERANCE <- "risk_tolerance_rating"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    payback_period_expectations_years = as.numeric(payback_period_expectations_years),
    investment_sector_focus = factor(investment_sector_focus),
    stakeholder = factor(stakeholder),
    risk_tolerance_rating = as.numeric(risk_tolerance_rating)
  ) %>%
  filter(!is.na(payback_period_expectations_years), !is.na(investment_sector_focus)) %>%
  # Remove extreme outliers (payback periods > 20 years seem unrealistic)
  filter(payback_period_expectations_years >= 1, payback_period_expectations_years <= 20)

# Sample sizes by investment sector
sector_counts <- df %>%
  count(investment_sector_focus, sort = TRUE)
print("Sample sizes by investment sector:")
print(sector_counts)

# Filter to sectors with adequate sample sizes (n ≥ 30)
sectors_to_include <- sector_counts %>%
  filter(n >= 30) %>%
  pull(investment_sector_focus)

df_filtered <- df %>%
  filter(investment_sector_focus %in% sectors_to_include) %>%
  mutate(investment_sector_focus = droplevels(investment_sector_focus))

print(paste("Analysis includes", length(sectors_to_include), "sectors with n ≥ 30"))

# Descriptive statistics by investment sector
desc_stats <- df_filtered %>%
  group_by(investment_sector_focus) %>%
  summarise(
    n = n(),
    mean_payback = round(mean(payback_period_expectations_years), 2),
    sd_payback = round(sd(payback_period_expectations_years), 2),
    median_payback = round(median(payback_period_expectations_years), 2),
    se_payback = round(sd(payback_period_expectations_years) / sqrt(n()), 3),
    ci_lower = round(mean_payback - 1.96 * se_payback, 2),
    ci_upper = round(mean_payback + 1.96 * se_payback, 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_payback))

print("Payback period expectations by investment sector:")
print(desc_stats)

# Check ANOVA assumptions
# Levene's test for homogeneity of variances
levene_test <- leveneTest(payback_period_expectations_years ~ investment_sector_focus, 
                         data = df_filtered)
print("Levene's test for homogeneity of variances:")
print(levene_test)

# Normality check by group
shapiro_results <- df_filtered %>%
  group_by(investment_sector_focus) %>%
  summarise(
    n = n(),
    shapiro_p = ifelse(n() > 5000, 
                      shapiro.test(sample(payback_period_expectations_years, 5000))$p.value,
                      shapiro.test(payback_period_expectations_years)$p.value),
    .groups = "drop"
  )
print("Shapiro-Wilk normality tests by sector:")
print(shapiro_results)

# One-way ANOVA
anova_model <- lm(payback_period_expectations_years ~ investment_sector_focus, 
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
  emmeans_result <- emmeans(anova_model, ~ investment_sector_focus)
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
tukey_result <- TukeyHSD(aov(payback_period_expectations_years ~ investment_sector_focus, 
                            data = df_filtered))
print("Tukey HSD comparisons:")
print(tukey_result)

# Kruskal-Wallis test (non-parametric alternative)
kruskal_test <- kruskal.test(payback_period_expectations_years ~ investment_sector_focus, 
                            data = df_filtered)
print("Kruskal-Wallis test (non-parametric):")
print(paste("H =", round(kruskal_test$statistic, 2)))
print(paste("df =", kruskal_test$parameter))
print(paste("p-value:", format(kruskal_test$p.value, scientific = TRUE)))

# Effect size for Kruskal-Wallis (eta-squared)
if(kruskal_test$p.value < 0.05) {
  kruskal_eta <- (kruskal_test$statistic - length(levels(df_filtered$investment_sector_focus)) + 1) / 
                 (nrow(df_filtered) - length(levels(df_filtered$investment_sector_focus)))
  print(paste("Kruskal-Wallis eta-squared:", round(kruskal_eta, 3)))
}

# Sector categorization analysis
df_sector_categories <- df_filtered %>%
  mutate(
    sector_category = case_when(
      grepl("Technology|Tech|Digital|Software", investment_sector_focus, ignore.case = TRUE) ~ "Technology",
      grepl("Energy|Renewable|Clean", investment_sector_focus, ignore.case = TRUE) ~ "Energy",
      grepl("Healthcare|Medical|Biotech", investment_sector_focus, ignore.case = TRUE) ~ "Healthcare",
      grepl("Financial|Fintech", investment_sector_focus, ignore.case = TRUE) ~ "Financial",
      TRUE ~ "Other"
    )
  )

# ANOVA by sector category
sector_category_stats <- df_sector_categories %>%
  group_by(sector_category) %>%
  filter(n() >= 30) %>%
  summarise(
    n = n(),
    mean_payback = round(mean(payback_period_expectations_years), 2),
    sd_payback = round(sd(payback_period_expectations_years), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_payback))

if(nrow(sector_category_stats) > 1) {
  print("Payback expectations by sector category (n ≥ 30):")
  print(sector_category_stats)
  
  # ANOVA by category
  anova_category <- lm(payback_period_expectations_years ~ sector_category, 
                      data = df_sector_categories %>% 
                      filter(sector_category %in% sector_category_stats$sector_category))
  anova_category_result <- Anova(anova_category, type = "III")
  
  print("ANOVA by sector category:")
  print(anova_category_result)
}

# Breakdown by stakeholder within sector
stakeholder_sector_breakdown <- df_filtered %>%
  group_by(investment_sector_focus, stakeholder) %>%
  summarise(
    n = n(),
    mean_payback = round(mean(payback_period_expectations_years), 2),
    sd_payback = round(sd(payback_period_expectations_years), 2),
    .groups = "drop"
  ) %>%
  filter(n >= 10) %>%
  arrange(investment_sector_focus, desc(mean_payback))

if(nrow(stakeholder_sector_breakdown) > 0) {
  print("Payback expectations by sector and stakeholder (n ≥ 10):")
  print(stakeholder_sector_breakdown)
}

# Risk tolerance correlation within sectors
if(sum(!is.na(df_filtered$risk_tolerance_rating)) > 50) {
  risk_correlation_by_sector <- df_filtered %>%
    filter(!is.na(risk_tolerance_rating)) %>%
    group_by(investment_sector_focus) %>%
    filter(n() >= 20) %>%
    summarise(
      n = n(),
      correlation = round(cor(payback_period_expectations_years, risk_tolerance_rating, 
                             use = "complete.obs"), 3),
      .groups = "drop"
    )
  
  print("Payback-Risk tolerance correlation by sector (n ≥ 20):")
  print(risk_correlation_by_sector)
}

# Payback period distribution analysis
payback_distribution <- df_filtered %>%
  mutate(
    payback_category = case_when(
      payback_period_expectations_years <= 3 ~ "Short-term (≤3 years)",
      payback_period_expectations_years <= 7 ~ "Medium-term (4-7 years)",
      payback_period_expectations_years <= 10 ~ "Long-term (8-10 years)",
      TRUE ~ "Very Long-term (>10 years)"
    )
  ) %>%
  group_by(investment_sector_focus, payback_category) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(investment_sector_focus) %>%
  mutate(percentage = round(n / sum(n) * 100, 1)) %>%
  select(-n) %>%
  pivot_wider(names_from = payback_category, values_from = percentage, values_fill = 0)

print("Payback period distribution by sector (%):")
print(payback_distribution)

# Identify sectors with shortest and longest expectations
if(nrow(desc_stats) > 1) {
  shortest_payback <- desc_stats %>% slice_min(mean_payback, n = 1)
  longest_payback <- desc_stats %>% slice_max(mean_payback, n = 1)
  
  print(paste("Shortest payback expectations:", shortest_payback$investment_sector_focus,
              "(M =", shortest_payback$mean_payback, "years, SD =", shortest_payback$sd_payback, ")"))
  print(paste("Longest payback expectations:", longest_payback$investment_sector_focus,
              "(M =", longest_payback$mean_payback, "years, SD =", longest_payback$sd_payback, ")"))
}

# Two-way ANOVA: Sector × Stakeholder (if sufficient data)
df_twoway <- df_filtered %>%
  group_by(investment_sector_focus, stakeholder) %>%
  filter(n() >= 5) %>%
  ungroup()

if(length(unique(df_twoway$stakeholder)) > 1) {
  twoway_model <- lm(payback_period_expectations_years ~ 
                    investment_sector_focus * stakeholder, 
                    data = df_twoway)
  twoway_result <- Anova(twoway_model, type = "III")
  
  print("Two-way ANOVA: Sector × Stakeholder")
  print(twoway_result)
}

# Variance analysis by sector
variance_analysis <- df_filtered %>%
  group_by(investment_sector_focus) %>%
  summarise(
    variance = round(var(payback_period_expectations_years), 3),
    cv = round(sd(payback_period_expectations_years) / mean(payback_period_expectations_years) * 100, 1),
    range = round(max(payback_period_expectations_years) - min(payback_period_expectations_years), 1),
    .groups = "drop"
  ) %>%
  arrange(desc(variance))

print("Variability in payback expectations by sector:")
print(variance_analysis)

# Effect size interpretation
eta_magnitude <- eta_squared_result$Eta2_partial[1]
eta_interpretation <- case_when(
  eta_magnitude < 0.01 ~ "Negligible",
  eta_magnitude < 0.06 ~ "Small",
  eta_magnitude < 0.14 ~ "Medium",
  TRUE ~ "Large"
)

print(paste("η² effect size interpretation:", eta_interpretation))

# Planned contrasts: Technology vs Traditional sectors
if("Technology" %in% df_sector_categories$sector_category) {
  df_tech_contrast <- df_sector_categories %>%
    mutate(
      tech_contrast = ifelse(sector_category == "Technology", "Technology", "Traditional")
    )
  
  tech_contrast_test <- t.test(payback_period_expectations_years ~ tech_contrast, 
                              data = df_tech_contrast)
  
  tech_mean <- mean(df_tech_contrast$payback_period_expectations_years[df_tech_contrast$tech_contrast == "Technology"])
  traditional_mean <- mean(df_tech_contrast$payback_period_expectations_years[df_tech_contrast$tech_contrast == "Traditional"])
  
  print("Technology vs Traditional sectors contrast:")
  print(paste("Technology mean:", round(tech_mean, 2), "years"))
  print(paste("Traditional mean:", round(traditional_mean, 2), "years"))
  print(paste("t-test: t =", round(tech_contrast_test$statistic, 2), 
              ", p =", format(tech_contrast_test$p.value, scientific = TRUE)))
}

# Expected: F(5,1301) = 7.2, p < .001, η² = .027