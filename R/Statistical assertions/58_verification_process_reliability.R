# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 58: "Verification process reliability ratings averaged 5.9 (SD = 2.1) with significant differences by verification method"
# Purpose: Descriptive analysis and group comparisons of verification process reliability ratings across different verification methods

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_VERIFICATION_RELIABILITY <- "verification_process_reliability_rating"
COL_VERIFICATION_METHOD <- "verification_method_type"
COL_STAKEHOLDER <- "stakeholder"
COL_ORGANIZATION_SIZE <- "organization_size_category"
COL_THIRD_PARTY_VERIFICATION <- "third_party_verification_usage"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    verification_process_reliability_rating = as.numeric(verification_process_reliability_rating),
    verification_method_type = factor(verification_method_type),
    stakeholder = factor(stakeholder),
    organization_size_category = factor(organization_size_category),
    third_party_verification_usage = as.numeric(third_party_verification_usage)
  ) %>%
  filter(!is.na(verification_process_reliability_rating)) %>%
  # Ensure ratings are within expected scale
  filter(verification_process_reliability_rating >= 1, verification_process_reliability_rating <= 10)

# Overall descriptive statistics
overall_stats <- df %>%
  summarise(
    n = n(),
    mean_reliability = round(mean(verification_process_reliability_rating), 2),
    sd_reliability = round(sd(verification_process_reliability_rating), 2),
    median_reliability = round(median(verification_process_reliability_rating), 2),
    q1 = round(quantile(verification_process_reliability_rating, 0.25), 2),
    q3 = round(quantile(verification_process_reliability_rating, 0.75), 2),
    min_rating = min(verification_process_reliability_rating),
    max_rating = max(verification_process_reliability_rating),
    .groups = "drop"
  )

print("Overall verification process reliability ratings:")
print(overall_stats)

# 95% Confidence interval for the mean
ci_mean <- t.test(df$verification_process_reliability_rating)$conf.int
print(paste("95% CI for mean: [", round(ci_mean[1], 2), ", ", 
            round(ci_mean[2], 2), "]", sep=""))

# Distribution analysis
print("Verification reliability rating distribution:")
rating_distribution <- table(df$verification_process_reliability_rating)
prop_distribution <- round(prop.table(rating_distribution) * 100, 1)
print(rating_distribution)
print("Proportions (%):")
print(prop_distribution)

# Analysis by verification method
if(!all(is.na(df$verification_method_type))) {
  method_stats <- df %>%
    filter(!is.na(verification_method_type)) %>%
    group_by(verification_method_type) %>%
    summarise(
      n = n(),
      mean_reliability = round(mean(verification_process_reliability_rating), 2),
      sd_reliability = round(sd(verification_process_reliability_rating), 2),
      median_reliability = round(median(verification_process_reliability_rating), 2),
      se_reliability = round(sd(verification_process_reliability_rating) / sqrt(n()), 3),
      ci_lower = round(mean_reliability - 1.96 * se_reliability, 2),
      ci_upper = round(mean_reliability + 1.96 * se_reliability, 2),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%  # Only methods with sufficient sample size
    arrange(desc(mean_reliability))
  
  print("Verification reliability by verification method (n ≥ 20):")
  print(method_stats)
  
  # One-way ANOVA for method differences
  df_methods <- df %>%
    filter(!is.na(verification_method_type)) %>%
    group_by(verification_method_type) %>%
    filter(n() >= 20) %>%
    ungroup() %>%
    mutate(verification_method_type = droplevels(verification_method_type))
  
  if(length(levels(df_methods$verification_method_type)) > 1) {
    anova_methods <- lm(verification_process_reliability_rating ~ verification_method_type, 
                       data = df_methods)
    anova_result <- Anova(anova_methods, type = "III")
    
    print("One-way ANOVA: Verification reliability by method:")
    print(anova_result)
    
    # Effect size
    eta_squared_result <- eta_squared(anova_result)
    print(paste("η² =", round(eta_squared_result$Eta2_partial[1], 3)))
    
    # Model summary
    anova_summary <- summary(anova_methods)
    print(paste("R-squared:", round(anova_summary$r.squared, 3)))
    
    # Post-hoc comparisons if significant
    if(anova_result$`Pr(>F)`[1] < 0.05) {
      emmeans_result <- emmeans(anova_methods, ~ verification_method_type)
      pairwise_comp <- pairs(emmeans_result, adjust = "bonferroni")
      
      print("Post-hoc pairwise comparisons (Bonferroni corrected):")
      print(pairwise_comp)
      
      # Compact letter display
      cld_result <- cld(emmeans_result, alpha = 0.05, Letters = letters)
      print("Compact letter display:")
      print(cld_result)
    }
    
    # Identify highest and lowest methods
    if(nrow(method_stats) > 1) {
      highest_method <- method_stats %>% slice_max(mean_reliability, n = 1)
      lowest_method <- method_stats %>% slice_min(mean_reliability, n = 1)
      
      print(paste("Highest reliability method:", highest_method$verification_method_type,
                  "(M =", highest_method$mean_reliability, ", SD =", highest_method$sd_reliability, ")"))
      print(paste("Lowest reliability method:", lowest_method$verification_method_type,
                  "(M =", lowest_method$mean_reliability, ", SD =", lowest_method$sd_reliability, ")"))
    }
  }
}

# Breakdown by stakeholder
stakeholder_stats <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_reliability = round(mean(verification_process_reliability_rating), 2),
    sd_reliability = round(sd(verification_process_reliability_rating), 2),
    median_reliability = round(median(verification_process_reliability_rating), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_reliability))

print("Verification reliability by stakeholder:")
print(stakeholder_stats)

# ANOVA by stakeholder
anova_stakeholder <- lm(verification_process_reliability_rating ~ stakeholder, data = df)
anova_stakeholder_result <- Anova(anova_stakeholder, type = "III")

print("ANOVA: Verification reliability by stakeholder:")
print(anova_stakeholder_result)

# Analysis by organization size
if(!all(is.na(df$organization_size_category))) {
  size_stats <- df %>%
    filter(!is.na(organization_size_category)) %>%
    group_by(organization_size_category) %>%
    summarise(
      n = n(),
      mean_reliability = round(mean(verification_process_reliability_rating), 2),
      sd_reliability = round(sd(verification_process_reliability_rating), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(mean_reliability))
  
  print("Verification reliability by organization size (n ≥ 20):")
  print(size_stats)
}

# Third-party verification analysis
if(sum(!is.na(df$third_party_verification_usage)) > 50) {
  df_third_party <- df %>%
    filter(!is.na(third_party_verification_usage)) %>%
    mutate(
      third_party_usage_level = case_when(
        third_party_verification_usage <= 3 ~ "Low Usage",
        third_party_verification_usage <= 7 ~ "Moderate Usage",
        third_party_verification_usage >= 8 ~ "High Usage"
      )
    )
  
  third_party_analysis <- df_third_party %>%
    group_by(third_party_usage_level) %>%
    summarise(
      n = n(),
      mean_reliability = round(mean(verification_process_reliability_rating), 2),
      sd_reliability = round(sd(verification_process_reliability_rating), 2),
      .groups = "drop"
    )
  
  print("Verification reliability by third-party verification usage:")
  print(third_party_analysis)
  
  # Correlation with third-party verification usage
  cor_third_party <- cor.test(df_third_party$verification_process_reliability_rating, 
                             df_third_party$third_party_verification_usage)
  
  print("Verification reliability × Third-party usage correlation:")
  print(paste("r =", round(cor_third_party$estimate, 3)))
  print(paste("95% CI: [", round(cor_third_party$conf.int[1], 3), ", ", 
              round(cor_third_party$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_third_party$p.value, scientific = TRUE)))
}

# Method effectiveness comparison (internal vs external verification)
if(!all(is.na(df$verification_method_type))) {
  df_method_categories <- df %>%
    filter(!is.na(verification_method_type)) %>%
    mutate(
      method_category = case_when(
        grepl("Internal|Self", verification_method_type, ignore.case = TRUE) ~ "Internal",
        grepl("External|Third|Independent", verification_method_type, ignore.case = TRUE) ~ "External",
        TRUE ~ "Other"
      )
    ) %>%
    filter(method_category %in% c("Internal", "External"))
  
  if(nrow(df_method_categories) > 50) {
    method_comparison <- df_method_categories %>%
      group_by(method_category) %>%
      summarise(
        n = n(),
        mean_reliability = round(mean(verification_process_reliability_rating), 2),
        sd_reliability = round(sd(verification_process_reliability_rating), 2),
        .groups = "drop"
      )
    
    print("Internal vs External verification reliability:")
    print(method_comparison)
    
    # T-test comparing internal vs external
    if(all(c("Internal", "External") %in% method_comparison$method_category)) {
      method_t_test <- t.test(verification_process_reliability_rating ~ method_category, 
                             data = df_method_categories)
      
      print("Internal vs External verification t-test:")
      print(paste("t =", round(method_t_test$statistic, 2)))
      print(paste("p =", format(method_t_test$p.value, scientific = TRUE)))
      
      # Effect size
      method_cohens_d <- cohens_d(verification_process_reliability_rating ~ method_category, 
                                 data = df_method_categories)
      print(paste("Cohen's d =", round(method_cohens_d$Cohens_d, 3)))
    }
  }
}

# High reliability threshold analysis (ratings ≥ 7)
high_reliability_analysis <- df %>%
  mutate(high_reliability = as.integer(verification_process_reliability_rating >= 7)) %>%
  summarise(
    n_total = n(),
    n_high = sum(high_reliability),
    prop_high = round(mean(high_reliability) * 100, 1)
  )

print("High verification reliability (ratings ≥ 7):")
print(paste(high_reliability_analysis$prop_high, "% of respondents (",
            high_reliability_analysis$n_high, "/", high_reliability_analysis$n_total, ")"))

# High reliability by verification method
if(!all(is.na(df$verification_method_type))) {
  high_reliability_method <- df %>%
    filter(!is.na(verification_method_type)) %>%
    mutate(high_reliability = as.integer(verification_process_reliability_rating >= 7)) %>%
    group_by(verification_method_type) %>%
    summarise(
      n = n(),
      n_high = sum(high_reliability),
      prop_high = round(mean(high_reliability) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(prop_high))
  
  if(nrow(high_reliability_method) > 0) {
    print("High reliability by verification method (n ≥ 20):")
    print(high_reliability_method)
    
    # Chi-square test for high reliability across methods
    high_reliability_table <- df %>%
      filter(!is.na(verification_method_type)) %>%
      group_by(verification_method_type) %>%
      filter(n() >= 20) %>%
      ungroup() %>%
      mutate(verification_method_type = droplevels(verification_method_type),
             high_reliability = verification_process_reliability_rating >= 7) %>%
      select(verification_method_type, high_reliability) %>%
      table()
    
    if(min(dim(high_reliability_table)) > 1) {
      chi_high_reliability <- chisq.test(high_reliability_table)
      print("Chi-square test for high reliability across methods:")
      print(paste("χ² =", round(chi_high_reliability$statistic, 2),
                  ", p =", format(chi_high_reliability$p.value, scientific = TRUE)))
    }
  }
}

# Two-way ANOVA: Method × Stakeholder (if sufficient data)
if(!all(is.na(df$verification_method_type))) {
  df_twoway <- df %>%
    filter(!is.na(verification_method_type), !is.na(stakeholder)) %>%
    group_by(verification_method_type, stakeholder) %>%
    filter(n() >= 5) %>%
    ungroup() %>%
    mutate(
      verification_method_type = droplevels(verification_method_type),
      stakeholder = droplevels(stakeholder)
    )
  
  if(length(levels(df_twoway$verification_method_type)) > 1 && 
     length(levels(df_twoway$stakeholder)) > 1) {
    
    twoway_model <- lm(verification_process_reliability_rating ~ 
                      verification_method_type * stakeholder, 
                      data = df_twoway)
    twoway_result <- Anova(twoway_model, type = "III")
    
    print("Two-way ANOVA: Verification Method × Stakeholder")
    print(twoway_result)
  }
}

# Normality assessment
shapiro_test <- shapiro.test(sample(df$verification_process_reliability_rating, 
                                   min(5000, nrow(df))))
print(paste("Shapiro-Wilk normality test: W =", round(shapiro_test$statistic, 3),
            ", p =", format(shapiro_test$p.value, scientific = TRUE)))

# Skewness and kurtosis
library(psych)
skew_kurt <- describe(df$verification_process_reliability_rating)
print(paste("Skewness:", round(skew_kurt$skew, 3)))
print(paste("Kurtosis:", round(skew_kurt$kurtosis, 3)))

# Mode calculation
mode_val <- as.numeric(names(sort(table(df$verification_process_reliability_rating), 
                                 decreasing = TRUE))[1])
print(paste("Mode:", mode_val))

# Interquartile range and range
iqr_val <- IQR(df$verification_process_reliability_rating)
range_val <- range(df$verification_process_reliability_rating)
print(paste("Interquartile range:", round(iqr_val, 2)))
print(paste("Range:", range_val[1], "to", range_val[2]))

# Combined method-stakeholder analysis
if(!all(is.na(df$verification_method_type))) {
  method_stakeholder_breakdown <- df %>%
    filter(!is.na(verification_method_type), !is.na(stakeholder)) %>%
    group_by(verification_method_type, stakeholder) %>%
    summarise(
      n = n(),
      mean_reliability = round(mean(verification_process_reliability_rating), 2),
      sd_reliability = round(sd(verification_process_reliability_rating), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(verification_method_type, desc(mean_reliability))
  
  if(nrow(method_stakeholder_breakdown) > 0) {
    print("Verification reliability by method and stakeholder (n ≥ 10):")
    print(method_stakeholder_breakdown)
  }
}

# Reliability score categories analysis
reliability_categories <- df %>%
  mutate(
    reliability_category = case_when(
      verification_process_reliability_rating <= 4 ~ "Low Reliability (1-4)",
      verification_process_reliability_rating <= 7 ~ "Moderate Reliability (5-7)",
      verification_process_reliability_rating <= 10 ~ "High Reliability (8-10)"
    )
  ) %>%
  count(reliability_category) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print("Verification reliability categories:")
print(reliability_categories)

# Expected: Mean = 5.9, SD = 2.1, with significant differences by verification method