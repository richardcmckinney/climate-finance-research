# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 34: "Team capability assessments showed strong reliability (α = .88) with mean scores of 5.1 (SD = 1.4)"
# Purpose: Reliability analysis and descriptive statistics for team capability assessment scale

library(tidyverse)
library(janitor)
library(psych)
library(DescTools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping - team capability assessment items
TEAM_CAPABILITY_ITEMS <- c("team_technical_expertise", "team_business_acumen", 
                          "team_leadership_quality", "team_market_knowledge",
                          "team_execution_ability", "team_adaptability",
                          "team_communication_skills", "team_network_strength")

# Overall team capability composite score
COL_TEAM_CAPABILITY_MEAN <- "team_capability_overall_rating"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  select(all_of(c(TEAM_CAPABILITY_ITEMS, COL_TEAM_CAPABILITY_MEAN, "stakeholder"))) %>%
  mutate(across(all_of(TEAM_CAPABILITY_ITEMS), as.numeric),
         team_capability_overall_rating = as.numeric(team_capability_overall_rating),
         stakeholder = factor(stakeholder))

# Remove cases with excessive missing data
df_complete <- df %>%
  filter(rowSums(is.na(select(., all_of(TEAM_CAPABILITY_ITEMS)))) <= 2) %>%
  filter(!is.na(team_capability_overall_rating))

# Calculate Cronbach's alpha for team capability scale
team_items_only <- df_complete %>%
  select(all_of(TEAM_CAPABILITY_ITEMS)) %>%
  na.omit()

alpha_result <- alpha(team_items_only)
print("Cronbach's Alpha Analysis for Team Capability Scale:")
print(paste("Overall α =", round(alpha_result$total$raw_alpha, 3)))
print(paste("Standardized α =", round(alpha_result$total$std.alpha, 3)))
print(paste("Number of items:", ncol(team_items_only)))
print(paste("Valid cases:", nrow(team_items_only)))

# Item-total correlations
print("Item-total correlations:")
item_stats <- alpha_result$item.stats
for(i in 1:nrow(item_stats)) {
  print(paste(rownames(item_stats)[i], ": r =", 
              round(item_stats$r.cor[i], 3),
              ", α if deleted =", round(item_stats$r.drop[i], 3)))
}

# Descriptive statistics for overall team capability rating
overall_stats <- df_complete %>%
  summarise(
    n = n(),
    mean_rating = round(mean(team_capability_overall_rating, na.rm = TRUE), 2),
    sd_rating = round(sd(team_capability_overall_rating, na.rm = TRUE), 2),
    median_rating = round(median(team_capability_overall_rating, na.rm = TRUE), 2),
    q1 = round(quantile(team_capability_overall_rating, 0.25, na.rm = TRUE), 2),
    q3 = round(quantile(team_capability_overall_rating, 0.75, na.rm = TRUE), 2),
    min_rating = min(team_capability_overall_rating, na.rm = TRUE),
    max_rating = max(team_capability_overall_rating, na.rm = TRUE),
    .groups = "drop"
  )

print("Overall Team Capability Ratings:")
print(overall_stats)

# 95% Confidence interval for the mean
ci_mean <- t.test(df_complete$team_capability_overall_rating)$conf.int
print(paste("95% CI for mean: [", round(ci_mean[1], 2), ", ", 
            round(ci_mean[2], 2), "]", sep=""))

# Distribution of ratings
print("Team capability rating distribution:")
rating_dist <- table(df_complete$team_capability_overall_rating)
prop_dist <- round(prop.table(rating_dist) * 100, 1)
print(rating_dist)
print("Proportions (%):")
print(prop_dist)

# Individual item means and standard deviations
item_descriptives <- df_complete %>%
  select(all_of(TEAM_CAPABILITY_ITEMS)) %>%
  summarise(across(everything(), 
                   list(mean = ~round(mean(.x, na.rm = TRUE), 2),
                        sd = ~round(sd(.x, na.rm = TRUE), 2),
                        n = ~sum(!is.na(.x))),
                   .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("item", "stat"),
               names_sep = "_(?=[^_]*$)",
               values_to = "value") %>%
  pivot_wider(names_from = stat, values_from = value)

print("Individual item descriptives:")
print(item_descriptives)

# Breakdown by stakeholder group
stakeholder_stats <- df_complete %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_rating = round(mean(team_capability_overall_rating, na.rm = TRUE), 2),
    sd_rating = round(sd(team_capability_overall_rating, na.rm = TRUE), 2),
    .groups = "drop"
  )

print("Team capability by stakeholder group:")
print(stakeholder_stats)

# Expected: α = .88, Mean = 5.1, SD = 1.4