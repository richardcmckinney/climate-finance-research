# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 33: "Technology maturity ratings averaged 4.2 (SD = 1.8) on 7-point scale"
# Purpose: Descriptive analysis of technology maturity assessment ratings

library(tidyverse)
library(janitor)
library(DescTools)
library(psych)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TECH_MATURITY <- "technology_maturity_rating"
COL_STAKEHOLDER <- "stakeholder"
COL_SECTOR <- "sector_focus"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    technology_maturity_rating = as.numeric(technology_maturity_rating),
    stakeholder = factor(stakeholder),
    sector_focus = factor(sector_focus)
  ) %>%
  filter(!is.na(technology_maturity_rating)) %>%
  # Ensure ratings are within expected 7-point scale
  filter(technology_maturity_rating >= 1, technology_maturity_rating <= 7)

# Overall descriptive statistics
overall_stats <- df %>%
  summarise(
    n = n(),
    mean_rating = round(mean(technology_maturity_rating), 2),
    sd_rating = round(sd(technology_maturity_rating), 2),
    median_rating = round(median(technology_maturity_rating), 2),
    q1 = round(quantile(technology_maturity_rating, 0.25), 2),
    q3 = round(quantile(technology_maturity_rating, 0.75), 2),
    min_rating = min(technology_maturity_rating),
    max_rating = max(technology_maturity_rating),
    .groups = "drop"
  )

print("Overall Technology Maturity Ratings:")
print(overall_stats)

# Distribution analysis
print("Technology maturity rating distribution:")
rating_distribution <- table(df$technology_maturity_rating)
prop_distribution <- round(prop.table(rating_distribution) * 100, 1)
print(rating_distribution)
print("Proportions (%):")
print(prop_distribution)

# 95% Confidence interval for the mean
ci_mean <- t.test(df$technology_maturity_rating)$conf.int
print(paste("95% CI for mean: [", round(ci_mean[1], 2), ", ", 
            round(ci_mean[2], 2), "]", sep=""))

# Normality assessment
shapiro_test <- shapiro.test(sample(df$technology_maturity_rating, 
                                   min(5000, nrow(df))))
print(paste("Shapiro-Wilk normality test: W =", round(shapiro_test$statistic, 3),
            ", p =", format(shapiro_test$p.value, scientific = TRUE)))

# Skewness and kurtosis
skew_kurt <- describe(df$technology_maturity_rating)
print(paste("Skewness:", round(skew_kurt$skew, 3)))
print(paste("Kurtosis:", round(skew_kurt$kurtosis, 3)))

# Breakdown by stakeholder group
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_rating = round(mean(technology_maturity_rating), 2),
    sd_rating = round(sd(technology_maturity_rating), 2),
    median_rating = round(median(technology_maturity_rating), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_rating))

print("Technology maturity by stakeholder group:")
print(stakeholder_stats)

# Breakdown by sector focus
sector_stats <- df %>%
  filter(!is.na(sector_focus)) %>%
  group_by(sector_focus) %>%
  summarise(
    n = n(),
    mean_rating = round(mean(technology_maturity_rating), 2),
    sd_rating = round(sd(technology_maturity_rating), 2),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%  # Only sectors with sufficient sample size
  arrange(desc(mean_rating))

print("Technology maturity by sector focus (n ≥ 20):")
print(sector_stats)

# Mode calculation
mode_val <- as.numeric(names(sort(table(df$technology_maturity_rating), 
                                 decreasing = TRUE))[1])
print(paste("Mode:", mode_val))

# Interquartile range
iqr_val <- IQR(df$technology_maturity_rating)
print(paste("Interquartile range:", round(iqr_val, 2)))

# Expected: Mean = 4.2, SD = 1.8 on 7-point scale