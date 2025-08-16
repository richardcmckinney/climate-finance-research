# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 110: "International cooperation frameworks: participation rate (37.8%, 95% CI [33.4%, 42.2%])"
# Purpose: Analyze participation rates in international cooperation frameworks

library(tidyverse)
library(janitor)
library(DescTools)
library(PropCIs)
library(binom)
library(epitools)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_FRAMEWORK_PARTICIPATION <- "international_framework_participant"
COL_FRAMEWORK_TYPE <- "cooperation_framework_type"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_INTERNATIONAL_PRESENCE <- "international_presence_score"
COL_COLLABORATION_BENEFITS <- "collaboration_benefit_score"
COL_RESOURCE_COMMITMENT <- "resource_commitment_level"
COL_LANGUAGE_CAPABILITIES <- "language_capability_count"
COL_CULTURAL_COMPETENCE <- "cultural_competence_score"
COL_PREVIOUS_PARTNERSHIPS <- "previous_international_partnerships"
COL_BARRIER_COUNT <- "participation_barrier_count"
COL_GEOGRAPHIC_SCOPE <- "geographic_scope_category"
COL_SECTOR <- "sector_classification"
COL_PARTICIPATION_DURATION <- "participation_duration_years"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    international_framework_participant = as.numeric(international_framework_participant),
    cooperation_framework_type = factor(cooperation_framework_type),
    organization_size_category = factor(organization_size_category,
                                       levels = c("Small", "Medium", "Large", "Multinational"),
                                       ordered = TRUE),
    international_presence_score = as.numeric(international_presence_score),
    collaboration_benefit_score = as.numeric(collaboration_benefit_score),
    resource_commitment_level = as.numeric(resource_commitment_level),
    language_capability_count = as.numeric(language_capability_count),
    cultural_competence_score = as.numeric(cultural_competence_score),
    previous_international_partnerships = as.numeric(previous_international_partnerships),
    participation_barrier_count = as.numeric(participation_barrier_count),
    geographic_scope_category = factor(geographic_scope_category),
    sector_classification = factor(sector_classification),
    participation_duration_years = as.numeric(participation_duration_years),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(international_framework_participant))

# Convert to binary (1 = participant, 0 = non-participant)
df$participant <- ifelse(df$international_framework_participant >= 1, 1, 0)

# Primary participation rate calculation
n_total <- nrow(df)
n_participants <- sum(df$participant)
n_non_participants <- n_total - n_participants
participation_rate <- n_participants / n_total

print("International Cooperation Framework Participation Rate Analysis:")
print(paste("Total organizations:", n_total))
print(paste("Participating:", n_participants))
print(paste("Not participating:", n_non_participants))
print(paste("Participation rate:", round(participation_rate * 100, 1), "%"))

# Multiple confidence interval methods
print("\n=== CONFIDENCE INTERVALS ===")

# 1. Wilson score interval (recommended)
wilson_ci <- prop.test(n_participants, n_total, correct = FALSE)
ci_wilson_lower <- wilson_ci$conf.int[1] * 100
ci_wilson_upper <- wilson_ci$conf.int[2] * 100

print(paste("95% CI (Wilson): [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))

# 2. Exact binomial (Clopper-Pearson)
binom_ci <- binom.test(n_participants, n_total, conf.level = 0.95)
ci_exact_lower <- binom_ci$conf.int[1] * 100
ci_exact_upper <- binom_ci$conf.int[2] * 100

print(paste("95% CI (Exact/Clopper-Pearson): [", round(ci_exact_lower, 1), "%,", 
           round(ci_exact_upper, 1), "%]"))

# 3. Agresti-Coull interval
ac_ci <- binom.confint(n_participants, n_total, method = "ac")
print(paste("95% CI (Agresti-Coull): [", round(ac_ci$lower * 100, 1), "%,", 
           round(ac_ci$upper * 100, 1), "%]"))

# 4. Jeffreys interval
jeffreys_ci <- binom.confint(n_participants, n_total, method = "bayes")
print(paste("95% CI (Jeffreys/Bayesian): [", round(jeffreys_ci$lower * 100, 1), "%,", 
           round(jeffreys_ci$upper * 100, 1), "%]"))

# Standard error
se_proportion <- sqrt(participation_rate * (1 - participation_rate) / n_total)
print(paste("\nStandard error:", round(se_proportion * 100, 2), "%"))

# Bootstrap confidence interval
set.seed(123)
n_boot <- 10000
boot_rates <- numeric(n_boot)

for(i in 1:n_boot) {
  boot_sample <- sample(df$participant, size = n_total, replace = TRUE)
  boot_rates[i] <- mean(boot_sample)
}

boot_ci <- quantile(boot_rates, c(0.025, 0.975)) * 100

print(paste("95% CI (Bootstrap): [", round(boot_ci[1], 1), "%,", 
           round(boot_ci[2], 1), "%]"))

# Participation by framework type
if(!all(is.na(df$cooperation_framework_type))) {
  framework_stats <- df %>%
    filter(!is.na(cooperation_framework_type)) %>%
    group_by(cooperation_framework_type) %>%
    summarise(
      n = n(),
      participants = sum(participant),
      participation_rate = mean(participant) * 100,
      ci_lower = ifelse(n >= 10, 
                       binom.test(participants, n)$conf.int[1] * 100, NA),
      ci_upper = ifelse(n >= 10, 
                       binom.test(participants, n)$conf.int[2] * 100, NA),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(desc(participation_rate))
  
  print("\nParticipation rates by framework type:")
  print(framework_stats)
}

# Organization size analysis
if(!all(is.na(df$organization_size_category))) {
  size_stats <- df %>%
    filter(!is.na(organization_size_category)) %>%
    group_by(organization_size_category) %>%
    summarise(
      n = n(),
      participation_rate = mean(participant) * 100,
      .groups = "drop"
    )
  
  print("\nParticipation rate by organization size:")
  print(size_stats)
  
  # Cochran-Armitage trend test
  if(nrow(size_stats) > 2) {
    print("\nCochran-Armitage test for trend would be appropriate")
  }
}

# International presence analysis
if(sum(!is.na(df$international_presence_score)) > 50) {
  presence_comparison <- df %>%
    group_by(participant) %>%
    summarise(
      n = n(),
      mean_presence = mean(international_presence_score, na.rm = TRUE),
      sd_presence = sd(international_presence_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nInternational presence by participation status:")
  print(presence_comparison)
  
  # Logistic regression
  logit_presence <- glm(participant ~ international_presence_score, 
                       data = df, family = binomial)
  print("\nLogistic regression (international presence):")
  print(summary(logit_presence))
  
  # Odds ratio
  or_presence <- exp(coef(logit_presence)[2])
  print(paste("Odds ratio per unit increase:", round(or_presence, 3)))
}

# Cultural competence analysis
if(sum(!is.na(df$cultural_competence_score)) > 50) {
  cultural_comparison <- df %>%
    group_by(participant) %>%
    summarise(
      n = n(),
      mean_cultural = mean(cultural_competence_score, na.rm = TRUE),
      sd_cultural = sd(cultural_competence_score, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nCultural competence by participation status:")
  print(cultural_comparison)
  
  # T-test
  t_test_cultural <- t.test(cultural_competence_score ~ participant, data = df)
  print(paste("t-test: t =", round(t_test_cultural$statistic, 2)))
  print(paste("p-value:", format(t_test_cultural$p.value, scientific = TRUE)))
}

# Language capabilities analysis
if(sum(!is.na(df$language_capability_count)) > 50) {
  # Categorize language capabilities
  df <- df %>%
    mutate(language_category = case_when(
      language_capability_count == 1 ~ "Monolingual",
      language_capability_count == 2 ~ "Bilingual",
      language_capability_count <= 4 ~ "Multilingual (3-4)",
      TRUE ~ "Highly multilingual (5+)"
    ))
  
  language_stats <- df %>%
    filter(!is.na(language_category)) %>%
    group_by(language_category) %>%
    summarise(
      n = n(),
      participation_rate = mean(participant) * 100,
      .groups = "drop"
    )
  
  print("\nParticipation by language capabilities:")
  print(language_stats)
}

# Previous partnerships analysis
if(sum(!is.na(df$previous_international_partnerships)) > 50) {
  # Categorize previous partnerships
  df <- df %>%
    mutate(partnership_experience = case_when(
      previous_international_partnerships == 0 ~ "No experience",
      previous_international_partnerships <= 2 ~ "Limited (1-2)",
      previous_international_partnerships <= 5 ~ "Moderate (3-5)",
      TRUE ~ "Extensive (6+)"
    ))
  
  experience_stats <- df %>%
    filter(!is.na(partnership_experience)) %>%
    group_by(partnership_experience) %>%
    summarise(
      n = n(),
      participation_rate = mean(participant) * 100,
      .groups = "drop"
    )
  
  print("\nParticipation by previous partnership experience:")
  print(experience_stats)
  
  # Logistic regression
  logit_experience <- glm(participant ~ previous_international_partnerships, 
                         data = df, family = binomial)
  print("\nLogistic regression (previous partnerships):")
  print(summary(logit_experience))
}

# Barrier analysis
if(sum(!is.na(df$participation_barrier_count)) > 50) {
  barrier_comparison <- df %>%
    group_by(participant) %>%
    summarise(
      n = n(),
      mean_barriers = mean(participation_barrier_count, na.rm = TRUE),
      median_barriers = median(participation_barrier_count, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nBarriers by participation status:")
  print(barrier_comparison)
  
  # Logistic regression
  logit_barriers <- glm(participant ~ participation_barrier_count, 
                       data = df, family = binomial)
  print("\nLogistic regression (barriers):")
  print(summary(logit_barriers))
}

# Resource commitment analysis
if(sum(!is.na(df$resource_commitment_level)) > 50) {
  resource_comparison <- df %>%
    group_by(participant) %>%
    summarise(
      n = n(),
      mean_resources = mean(resource_commitment_level, na.rm = TRUE),
      sd_resources = sd(resource_commitment_level, na.rm = TRUE),
      .groups = "drop"
    )
  
  print("\nResource commitment by participation status:")
  print(resource_comparison)
}

# Geographic scope analysis
if(!all(is.na(df$geographic_scope_category))) {
  geographic_stats <- df %>%
    filter(!is.na(geographic_scope_category)) %>%
    group_by(geographic_scope_category) %>%
    summarise(
      n = n(),
      participation_rate = mean(participant) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 15) %>%
    arrange(desc(participation_rate))
  
  print("\nParticipation by geographic scope:")
  print(geographic_stats)
}

# Sector analysis
if(length(unique(df$sector_classification)) > 3) {
  sector_stats <- df %>%
    filter(!is.na(sector_classification)) %>%
    group_by(sector_classification) %>%
    summarise(
      n = n(),
      participation_rate = mean(participant) * 100,
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(participation_rate))
  
  print("\nTop sectors by participation rate:")
  print(head(sector_stats, 10))
}

# Duration analysis (for participants)
if(sum(!is.na(df$participation_duration_years)) > 30) {
  duration_summary <- df %>%
    filter(participant == 1, !is.na(participation_duration_years)) %>%
    summarise(
      n = n(),
      mean_duration = mean(participation_duration_years, na.rm = TRUE),
      median_duration = median(participation_duration_years, na.rm = TRUE),
      sd_duration = sd(participation_duration_years, na.rm = TRUE)
    )
  
  print("\nParticipation duration (years):")
  print(duration_summary)
}

# Multiple logistic regression
predictors <- c("international_presence_score", "cultural_competence_score",
               "previous_international_partnerships", "participation_barrier_count",
               "resource_commitment_level")
available_predictors <- predictors[predictors %in% names(df)]

if(length(available_predictors) >= 3) {
  formula_str <- paste("participant ~", paste(available_predictors, collapse = " + "))
  
  logit_full <- glm(as.formula(formula_str), data = df, family = binomial)
  
  print("\nMultiple logistic regression model:")
  print(summary(logit_full))
  
  # Odds ratios
  or_all <- exp(coef(logit_full))
  print("\nOdds ratios:")
  print(round(or_all, 3))
  
  # Model fit
  print(paste("\nAIC:", round(AIC(logit_full), 2)))
  
  # Pseudo R-squared
  null_model <- glm(participant ~ 1, data = df, family = binomial)
  mcfadden_r2 <- 1 - (logLik(logit_full) / logLik(null_model))
  print(paste("McFadden's R²:", round(mcfadden_r2, 3)))
}

# Stakeholder analysis
stakeholder_stats <- df %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    participation_rate = mean(participant) * 100,
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%
  arrange(desc(participation_rate))

print("\nParticipation rate by stakeholder:")
print(stakeholder_stats)

# Power analysis
library(pwr)
# For detecting 5% difference from observed rate
p0 <- participation_rate
p1 <- p0 + 0.05
effect_size <- ES.h(p1, p0)

power_calc <- pwr.p.test(h = effect_size, sig.level = 0.05, power = 0.80)
print(paste("\nSample size for detecting 5% difference (80% power):", 
           ceiling(power_calc$n)))

# Summary for reporting
print("\n=== FINAL SUMMARY ===")
print(paste("International cooperation framework participation rate:", 
           round(participation_rate * 100, 1), "%"))
print(paste("95% Confidence Interval: [", round(ci_wilson_lower, 1), "%,", 
           round(ci_wilson_upper, 1), "%]"))
print(paste("Based on", n_total, "organizations"))
print(paste("Number participating:", n_participants))
print(paste("Number not participating:", n_non_participants))
print("The participation rate indicates moderate engagement in international")
print("cooperation frameworks, with room for increased participation")

# Expected: participation rate (37.8%, 95% CI [33.4%, 42.2%])