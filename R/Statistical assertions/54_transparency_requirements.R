# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 54: "Transparency requirements ratings averaged 6.4 (SD = 1.6) with significant variation by geographic region"
# Purpose: Descriptive analysis and regional variation testing for transparency requirements ratings

library(tidyverse)
library(janitor)
library(car)
library(effectsize)
library(emmeans)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TRANSPARENCY_REQUIREMENTS <- "transparency_requirements_rating"
COL_GEOGRAPHIC_REGION <- "geographic_region"
COL_STAKEHOLDER <- "stakeholder"
COL_REGULATORY_ENVIRONMENT <- "regulatory_environment_stringency"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    transparency_requirements_rating = as.numeric(transparency_requirements_rating),
    geographic_region = factor(geographic_region),
    stakeholder = factor(stakeholder),
    regulatory_environment_stringency = as.numeric(regulatory_environment_stringency)
  ) %>%
  filter(!is.na(transparency_requirements_rating)) %>%
  # Ensure ratings are within expected scale
  filter(transparency_requirements_rating >= 1, transparency_requirements_rating <= 10)

# Overall descriptive statistics
overall_stats <- df %>%
  summarise(
    n = n(),
    mean_transparency = round(mean(transparency_requirements_rating), 2),
    sd_transparency = round(sd(transparency_requirements_rating), 2),
    median_transparency = round(median(transparency_requirements_rating), 2),
    q1 = round(quantile(transparency_requirements_rating, 0.25), 2),
    q3 = round(quantile(transparency_requirements_rating, 0.75), 2),
    min_rating = min(transparency_requirements_rating),
    max_rating = max(transparency_requirements_rating),
    .groups = "drop"
  )

print("Overall transparency requirements ratings:")
print(overall_stats)

# 95% Confidence interval for the mean
ci_mean <- t.test(df$transparency_requirements_rating)$conf.int
print(paste("95% CI for mean: [", round(ci_mean[1], 2), ", ", 
            round(ci_mean[2], 2), "]", sep=""))

# Distribution analysis
print("Transparency requirements rating distribution:")
rating_distribution <- table(df$transparency_requirements_rating)
prop_distribution <- round(prop.table(rating_distribution) * 100, 1)
print(rating_distribution)
print("Proportions (%):")
print(prop_distribution)

# Regional analysis
if(!all(is.na(df$geographic_region))) {
  region_stats <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    summarise(
      n = n(),
      mean_transparency = round(mean(transparency_requirements_rating), 2),
      sd_transparency = round(sd(transparency_requirements_rating), 2),
      median_transparency = round(median(transparency_requirements_rating), 2),
      se_transparency = round(sd(transparency_requirements_rating) / sqrt(n()), 3),
      ci_lower = round(mean_transparency - 1.96 * se_transparency, 2),
      ci_upper = round(mean_transparency + 1.96 * se_transparency, 2),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%  # Only regions with sufficient sample size
    arrange(desc(mean_transparency))
  
  print("Transparency requirements by geographic region (n ≥ 20):")
  print(region_stats)
  
  # One-way ANOVA for regional differences
  df_regions <- df %>%
    filter(!is.na(geographic_region)) %>%
    group_by(geographic_region) %>%
    filter(n() >= 20) %>%
    ungroup() %>%
    mutate(geographic_region = droplevels(geographic_region))
  
  if(length(levels(df_regions$geographic_region)) > 1) {
    anova_regions <- lm(transparency_requirements_rating ~ geographic_region, 
                       data = df_regions)
    anova_result <- Anova(anova_regions, type = "III")
    
    print("One-way ANOVA: Transparency requirements by geographic region:")
    print(anova_result)
    
    # Effect size
    eta_squared_result <- eta_squared(anova_result)
    print(paste("η² =", round(eta_squared_result$Eta2_partial[1], 3)))
    
    # Model summary
    anova_summary <- summary(anova_regions)
    print(paste("R-squared:", round(anova_summary$r.squared, 3)))
    
    # Post-hoc comparisons if significant
    if(anova_result$`Pr(>F)`[1] < 0.05) {
      emmeans_result <- emmeans(anova_regions, ~ geographic_region)
      pairwise_comp <- pairs(emmeans_result, adjust = "bonferroni")
      
      print("Post-hoc pairwise comparisons (Bonferroni corrected):")
      print(pairwise_comp)
      
      # Compact letter display
      cld_result <- cld(emmeans_result, alpha = 0.05, Letters = letters)
      print("Compact letter display:")
      print(cld_result)
    }
    
    # Identify highest and lowest regions
    if(nrow(region_stats) > 1) {
      highest_region <- region_stats %>% slice_max(mean_transparency, n = 1)
      lowest_region <- region_stats %>% slice_min(mean_transparency, n = 1)
      
      print(paste("Highest transparency ratings:", highest_region$geographic_region,
                  "(M =", highest_region$mean_transparency, ", SD =", highest_region$sd_transparency, ")"))
      print(paste("Lowest transparency ratings:", lowest_region$geographic_region,
                  "(M =", lowest_region$mean_transparency, ", SD =", lowest_region$sd_transparency, ")"))
    }
  }
}

# Breakdown by stakeholder
stakeholder_stats <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    mean_transparency = round(mean(transparency_requirements_rating), 2),
    sd_transparency = round(sd(transparency_requirements_rating), 2),
    median_transparency = round(median(transparency_requirements_rating), 2),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_transparency))

print("Transparency requirements by stakeholder:")
print(stakeholder_stats)

# ANOVA by stakeholder
anova_stakeholder <- lm(transparency_requirements_rating ~ stakeholder, data = df)
anova_stakeholder_result <- Anova(anova_stakeholder, type = "III")

print("ANOVA: Transparency requirements by stakeholder:")
print(anova_stakeholder_result)

# Analysis by regulatory environment stringency (if available)
if(sum(!is.na(df$regulatory_environment_stringency)) > 50) {
  df_regulatory <- df %>%
    filter(!is.na(regulatory_environment_stringency))
  
  # Correlation with regulatory stringency
  cor_regulatory <- cor.test(df_regulatory$transparency_requirements_rating, 
                            df_regulatory$regulatory_environment_stringency)
  
  print("Transparency requirements × Regulatory stringency correlation:")
  print(paste("r =", round(cor_regulatory$estimate, 3)))
  print(paste("95% CI: [", round(cor_regulatory$conf.int[1], 3), ", ", 
              round(cor_regulatory$conf.int[2], 3), "]", sep=""))
  print(paste("p-value:", format(cor_regulatory$p.value, scientific = TRUE)))
}

# Two-way ANOVA: Region × Stakeholder (if sufficient data)
if(!all(is.na(df$geographic_region))) {
  df_twoway <- df %>%
    filter(!is.na(geographic_region), !is.na(stakeholder)) %>%
    group_by(geographic_region, stakeholder) %>%
    filter(n() >= 5) %>%  # At least 5 observations per cell
    ungroup() %>%
    mutate(
      geographic_region = droplevels(geographic_region),
      stakeholder = droplevels(stakeholder)
    )
  
  if(length(levels(df_twoway$geographic_region)) > 1 && 
     length(levels(df_twoway$stakeholder)) > 1) {
    
    twoway_model <- lm(transparency_requirements_rating ~ 
                      geographic_region * stakeholder, 
                      data = df_twoway)
    twoway_result <- Anova(twoway_model, type = "III")
    
    print("Two-way ANOVA: Region × Stakeholder")
    print(twoway_result)
  }
}

# Regional differences analysis
if(!all(is.na(df$geographic_region)) && nrow(region_stats) > 1) {
  # Range of means across regions
  region_range <- region_stats %>%
    summarise(
      min_mean = min(mean_transparency),
      max_mean = max(mean_transparency),
      range = max_mean - min_mean
    )
  
  print(paste("Range across regions:", region_range$min_mean, "to", 
              region_range$max_mean, "(range =", round(region_range$range, 2), ")"))
  
  # Kruskal-Wallis test (non-parametric)
  kruskal_regions <- kruskal.test(transparency_requirements_rating ~ geographic_region, 
                                 data = df_regions)
  print("Kruskal-Wallis test (non-parametric):")
  print(paste("H =", round(kruskal_regions$statistic, 2)))
  print(paste("p =", format(kruskal_regions$p.value, scientific = TRUE)))
}

# High transparency threshold analysis (ratings ≥ 7)
high_transparency_analysis <- df %>%
  mutate(high_transparency = as.integer(transparency_requirements_rating >= 7)) %>%
  summarise(
    n_total = n(),
    n_high = sum(high_transparency),
    prop_high = round(mean(high_transparency) * 100, 1)
  )

print("High transparency requirements (ratings ≥ 7):")
print(paste(high_transparency_analysis$prop_high, "% of respondents (",
            high_transparency_analysis$n_high, "/", high_transparency_analysis$n_total, ")"))

# High transparency by region
if(!all(is.na(df$geographic_region))) {
  high_transparency_region <- df %>%
    filter(!is.na(geographic_region)) %>%
    mutate(high_transparency = as.integer(transparency_requirements_rating >= 7)) %>%
    group_by(geographic_region) %>%
    summarise(
      n = n(),
      n_high = sum(high_transparency),
      prop_high = round(mean(high_transparency) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 20) %>%
    arrange(desc(prop_high))
  
  if(nrow(high_transparency_region) > 0) {
    print("High transparency requirements by region (n ≥ 20):")
    print(high_transparency_region)
    
    # Chi-square test for high transparency across regions
    high_transparency_table <- df %>%
      filter(!is.na(geographic_region)) %>%
      mutate(high_transparency = transparency_requirements_rating >= 7) %>%
      select(geographic_region, high_transparency) %>%
      table()
    
    if(min(dim(high_transparency_table)) > 1) {
      chi_high_transparency <- chisq.test(high_transparency_table)
      print("Chi-square test for high transparency across regions:")
      print(paste("χ² =", round(chi_high_transparency$statistic, 2),
                  ", p =", format(chi_high_transparency$p.value, scientific = TRUE)))
    }
  }
}

# Combined region-stakeholder analysis
if(!all(is.na(df$geographic_region))) {
  region_stakeholder_breakdown <- df %>%
    filter(!is.na(geographic_region), !is.na(stakeholder)) %>%
    group_by(geographic_region, stakeholder) %>%
    summarise(
      n = n(),
      mean_transparency = round(mean(transparency_requirements_rating), 2),
      sd_transparency = round(sd(transparency_requirements_rating), 2),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(geographic_region, desc(mean_transparency))
  
  if(nrow(region_stakeholder_breakdown) > 0) {
    print("Transparency requirements by region and stakeholder (n ≥ 10):")
    print(region_stakeholder_breakdown)
  }
}

# Normality assessment
shapiro_test <- shapiro.test(sample(df$transparency_requirements_rating, 
                                   min(5000, nrow(df))))
print(paste("Shapiro-Wilk normality test: W =", round(shapiro_test$statistic, 3),
            ", p =", format(shapiro_test$p.value, scientific = TRUE)))

# Skewness and kurtosis
library(psych)
skew_kurt <- describe(df$transparency_requirements_rating)
print(paste("Skewness:", round(skew_kurt$skew, 3)))
print(paste("Kurtosis:", round(skew_kurt$kurtosis, 3)))

# Mode calculation
mode_val <- as.numeric(names(sort(table(df$transparency_requirements_rating), 
                                 decreasing = TRUE))[1])
print(paste("Mode:", mode_val))

# Expected: Mean = 6.4, SD = 1.6, with significant variation by geographic region