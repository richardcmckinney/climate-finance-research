# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 32: "Market readiness scores increased with experience (r = .276, 95% CI [.231, .320], p < .001)"
# Purpose: Correlation analysis between years of experience and market readiness assessment scores

library(tidyverse)
library(janitor)
library(boot)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_EXPERIENCE <- "experience_years"
COL_MARKET_READINESS <- "market_readiness_score"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    experience_years = as.numeric(experience_years),
    market_readiness_score = as.numeric(market_readiness_score)
  ) %>%
  filter(!is.na(experience_years), !is.na(market_readiness_score)) %>%
  # Remove outliers (experience > 50 years or negative values)
  filter(experience_years >= 0, experience_years <= 50,
         market_readiness_score >= 1, market_readiness_score <= 7)

# Descriptive statistics
desc_stats <- df %>%
  summarise(
    n = n(),
    experience_mean = round(mean(experience_years), 1),
    experience_sd = round(sd(experience_years), 1),
    market_readiness_mean = round(mean(market_readiness_score), 2),
    market_readiness_sd = round(sd(market_readiness_score), 2),
    .groups = "drop"
  )

print("Descriptive statistics:")
print(desc_stats)

# Pearson correlation with confidence interval
cor_result <- cor.test(df$experience_years, df$market_readiness_score, 
                      method = "pearson", conf.level = 0.95)

print("Pearson correlation results:")
print(paste("r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("t(", cor_result$parameter, ") =", round(cor_result$statistic, 2)))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval for additional validation
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$experience_years, d$market_readiness_score, use = "complete.obs"))
}

set.seed(123)
boot_result <- boot(df, boot_cor, R = 10000)
boot_ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

print("Bootstrap 95% CI validation:")
print(paste("[", round(boot_ci$percent[4], 3), ", ", round(boot_ci$percent[5], 3), "]", sep=""))

# Experience level breakdown
df_categorized <- df %>%
  mutate(
    experience_category = case_when(
      experience_years < 5 ~ "Early Career (0-4 years)",
      experience_years >= 5 & experience_years < 15 ~ "Mid Career (5-14 years)",
      experience_years >= 15 ~ "Senior (15+ years)"
    )
  )

category_stats <- df_categorized %>%
  group_by(experience_category) %>%
  summarise(
    n = n(),
    mean_market_readiness = round(mean(market_readiness_score), 2),
    sd_market_readiness = round(sd(market_readiness_score), 2),
    .groups = "drop"
  )

print("Market readiness by experience category:")
print(category_stats)

# Spearman correlation for robustness check
spearman_result <- cor.test(df$experience_years, df$market_readiness_score, 
                           method = "spearman")
print(paste("Spearman's ρ =", round(spearman_result$estimate, 3), 
            "(robustness check)"))

# Expected: r = .276, 95% CI [.231, .320], p < .001