# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 37: "Networking support correlated with partnership preferences (r = .449, p < .001)"
# Purpose: Correlation analysis between networking support provision and partnership collaboration preferences

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_NETWORKING_SUPPORT <- "support_networking_provision"
COL_PARTNERSHIP_PREF <- "collaboration_preference_partnerships"
COL_STAKEHOLDER <- "stakeholder"
COL_EXPERIENCE <- "experience_years"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    support_networking_provision = as.numeric(support_networking_provision),
    collaboration_preference_partnerships = as.numeric(collaboration_preference_partnerships),
    stakeholder = factor(stakeholder),
    experience_years = as.numeric(experience_years)
  ) %>%
  filter(!is.na(support_networking_provision), 
         !is.na(collaboration_preference_partnerships)) %>%
  # Ensure variables are within expected ranges
  filter(support_networking_provision >= 1, support_networking_provision <= 7,
         collaboration_preference_partnerships >= 1, collaboration_preference_partnerships <= 7)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    networking_mean = round(mean(support_networking_provision), 2),
    networking_sd = round(sd(support_networking_provision), 2),
    partnership_mean = round(mean(collaboration_preference_partnerships), 2),
    partnership_sd = round(sd(collaboration_preference_partnerships), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Primary correlation analysis
cor_result <- cor.test(df$support_networking_provision, 
                      df$collaboration_preference_partnerships,
                      method = "pearson", conf.level = 0.95)

print("Pearson correlation results:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$support_networking_provision, d$collaboration_preference_partnerships,
             use = "complete.obs"))
}

set.seed(123)
boot_result <- boot(df, boot_cor, R = 10000)
boot_ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation:")
print(paste("[", round(boot_ci$percent[4], 3), ", ", round(boot_ci$percent[5], 3), "]", sep=""))

# Correlation by stakeholder group
cor_by_stakeholder <- df %>%
  group_by(stakeholder) %>%
  filter(n() >= 30) %>%
  summarise(
    n = n(),
    correlation = round(cor(support_networking_provision, 
                           collaboration_preference_partnerships,
                           use = "complete.obs"), 3),
    p_value = round(cor.test(support_networking_provision,
                            collaboration_preference_partnerships)$p.value, 4),
    .groups = "drop"
  )

print("Correlation by stakeholder group (n ≥ 30):")
print(cor_by_stakeholder)

# Partial correlation controlling for experience
if(sum(!is.na(df$experience_years)) > 50) {
  df_experience <- df %>%
    filter(!is.na(experience_years))
  
  partial_cor <- pcor.test(df_experience$support_networking_provision,
                          df_experience$collaboration_preference_partnerships,
                          df_experience$experience_years)
  
  print("Partial correlation (controlling for experience):")
  print(paste("r =", round(partial_cor$estimate, 3),
              ", p =", format(partial_cor$p.value, scientific = TRUE)))
}

# Spearman correlation for robustness
spearman_result <- cor.test(df$support_networking_provision,
                           df$collaboration_preference_partnerships,
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3),
            "(robustness check)"))

# Linear regression analysis
lm_model <- lm(collaboration_preference_partnerships ~ support_networking_provision, 
               data = df)
lm_summary <- summary(lm_model)

print("Linear regression results:")
print(paste("R-squared:", round(lm_summary$r.squared, 3)))
print(paste("Adjusted R-squared:", round(lm_summary$adj.r.squared, 3)))
print(paste("Slope (β):", round(coef(lm_model)[2], 3)))
print(paste("Standard error:", round(lm_summary$coefficients[2,2], 3)))

# Effect size interpretation
cor_magnitude <- abs(cor_result$estimate)
effect_size_interpretation <- case_when(
  cor_magnitude < 0.1 ~ "Negligible",
  cor_magnitude < 0.3 ~ "Small", 
  cor_magnitude < 0.5 ~ "Medium",
  cor_magnitude < 0.7 ~ "Large",
  TRUE ~ "Very Large"
)

print(paste("Effect size interpretation:", effect_size_interpretation))

# Cross-tabulation for categorical analysis
df_categories <- df %>%
  mutate(
    networking_level = case_when(
      support_networking_provision <= 3 ~ "Low",
      support_networking_provision <= 5 ~ "Moderate", 
      support_networking_provision <= 7 ~ "High"
    ),
    partnership_level = case_when(
      collaboration_preference_partnerships <= 3 ~ "Low",
      collaboration_preference_partnerships <= 5 ~ "Moderate",
      collaboration_preference_partnerships <= 7 ~ "High"
    )
  )

cross_tab <- table(df_categories$networking_level, df_categories$partnership_level)
print("Cross-tabulation of networking support and partnership preference levels:")
print(cross_tab)
print("Row percentages:")
print(round(prop.table(cross_tab, 1) * 100, 1))

# Chi-square test for categorical association
chi_test <- chisq.test(cross_tab)
print(paste("Chi-square test: χ² =", round(chi_test$statistic, 2),
            ", df =", chi_test$parameter,
            ", p =", format(chi_test$p.value, scientific = TRUE)))

# Cramér's V for effect size
cramers_v <- sqrt(chi_test$statistic / (sum(cross_tab) * (min(dim(cross_tab)) - 1)))
print(paste("Cramér's V:", round(cramers_v, 3)))

# Expected: r = .449, p < .001