# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 44: "Market access challenges were identified by 68% of respondents (95% CI [64.7%, 71.2%]) with significant variation by sector"
# Purpose: Proportion analysis with Wilson confidence intervals and sector-wise breakdown of market access challenges

library(tidyverse)
library(janitor)
library(DescTools)
library(car)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_MARKET_ACCESS_CHALLENGES <- "market_access_challenges_identified"
COL_SECTOR <- "sector_focus"
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    market_access_challenges_identified = as.integer(market_access_challenges_identified),
    sector_focus = factor(sector_focus),
    stakeholder = factor(stakeholder)
  ) %>%
  filter(!is.na(market_access_challenges_identified))

# Overall proportion analysis
n_total <- nrow(df)
n_challenges <- sum(df$market_access_challenges_identified, na.rm = TRUE)
prop_challenges <- n_challenges / n_total

print("Overall market access challenges identification:")
print(paste("Total respondents:", n_total))
print(paste("Identified challenges:", n_challenges))
print(paste("Proportion:", round(prop_challenges * 100, 1), "%"))

# Wilson confidence interval for overall proportion
ci_wilson <- BinomCI(n_challenges, n_total, conf.level = 0.95, method = "wilson")
print(paste("95% Wilson CI: [", round(ci_wilson[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wilson[,"upr.ci"] * 100, 1), "%]", sep=""))

# Additional confidence interval methods for comparison
ci_exact <- BinomCI(n_challenges, n_total, conf.level = 0.95, method = "clopper-pearson")
ci_wald <- BinomCI(n_challenges, n_total, conf.level = 0.95, method = "wald")

print("Alternative confidence intervals:")
print(paste("95% Exact CI: [", round(ci_exact[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_exact[,"upr.ci"] * 100, 1), "%]", sep=""))
print(paste("95% Wald CI: [", round(ci_wald[,"lwr.ci"] * 100, 1), "%, ", 
            round(ci_wald[,"upr.ci"] * 100, 1), "%]", sep=""))

# Breakdown by sector
sector_breakdown <- df %>%
  filter(!is.na(sector_focus)) %>%
  group_by(sector_focus) %>%
  summarise(
    n = n(),
    challenges_identified = sum(market_access_challenges_identified, na.rm = TRUE),
    proportion = round(mean(market_access_challenges_identified, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  filter(n >= 20) %>%  # Only sectors with sufficient sample size
  arrange(desc(proportion))

print("Market access challenges by sector (n ≥ 20):")
print(sector_breakdown)

# Calculate Wilson CIs for each sector
sector_breakdown_ci <- sector_breakdown %>%
  rowwise() %>%
  mutate(
    ci_data = list(BinomCI(challenges_identified, n, conf.level = 0.95, method = "wilson")),
    ci_lower = round(ci_data[[1]][,"lwr.ci"] * 100, 1),
    ci_upper = round(ci_data[[1]][,"upr.ci"] * 100, 1),
    ci_text = paste("[", ci_lower, "%, ", ci_upper, "%]", sep="")
  ) %>%
  select(-ci_data)

print("Sector-wise proportions with 95% Wilson CIs:")
print(sector_breakdown_ci %>% select(sector_focus, n, proportion, ci_text))

# Test for significant variation by sector
# Chi-square test of independence
sector_table <- table(df$sector_focus[!is.na(df$sector_focus)], 
                     df$market_access_challenges_identified[!is.na(df$sector_focus)])

chi_test <- chisq.test(sector_table)
print("Chi-square test for sector variation:")
print(paste("χ²(", chi_test$parameter, ") =", round(chi_test$statistic, 2)))
print(paste("p-value:", format(chi_test$p.value, scientific = TRUE)))

# Cramér's V for effect size
cramers_v <- sqrt(chi_test$statistic / (sum(sector_table) * (min(dim(sector_table)) - 1)))
print(paste("Cramér's V =", round(cramers_v, 3)))

# Fisher's exact test if any expected frequencies are low
expected_freq <- chi_test$expected
if(any(expected_freq < 5)) {
  print("Some expected frequencies < 5, performing Fisher's exact test...")
  fisher_test <- fisher.test(sector_table, simulate.p.value = TRUE, B = 10000)
  print(paste("Fisher's exact test p-value:", format(fisher_test$p.value, scientific = TRUE)))
}

# Breakdown by stakeholder
stakeholder_breakdown <- df %>%
  filter(!is.na(stakeholder)) %>%
  group_by(stakeholder) %>%
  summarise(
    n = n(),
    challenges_identified = sum(market_access_challenges_identified, na.rm = TRUE),
    proportion = round(mean(market_access_challenges_identified, na.rm = TRUE) * 100, 1),
    .groups = "drop"
  ) %>%
  arrange(desc(proportion))

print("Market access challenges by stakeholder:")
print(stakeholder_breakdown)

# Statistical comparison across stakeholders
stakeholder_table <- table(df$stakeholder, df$market_access_challenges_identified)
chi_stakeholder <- chisq.test(stakeholder_table)
print("Chi-square test for stakeholder variation:")
print(paste("χ²(", chi_stakeholder$parameter, ") =", round(chi_stakeholder$statistic, 2)))
print(paste("p-value:", format(chi_stakeholder$p.value, scientific = TRUE)))

# Post-hoc pairwise comparisons for sectors (if significant)
if(chi_test$p.value < 0.05) {
  print("Performing post-hoc pairwise sector comparisons...")
  
  # Filter to sectors with adequate sample sizes
  sectors_adequate <- sector_breakdown %>%
    filter(n >= 30) %>%
    pull(sector_focus)
  
  if(length(sectors_adequate) >= 2) {
    df_sectors <- df %>%
      filter(sector_focus %in% sectors_adequate, !is.na(sector_focus)) %>%
      mutate(sector_focus = droplevels(sector_focus))
    
    # Pairwise proportion tests with Bonferroni correction
    sector_levels <- levels(df_sectors$sector_focus)
    n_comparisons <- choose(length(sector_levels), 2)
    alpha_corrected <- 0.05 / n_comparisons
    
    print(paste("Bonferroni corrected alpha:", round(alpha_corrected, 4)))
    
    for(i in 1:(length(sector_levels)-1)) {
      for(j in (i+1):length(sector_levels)) {
        sector1_data <- df_sectors %>% filter(sector_focus == sector_levels[i])
        sector2_data <- df_sectors %>% filter(sector_focus == sector_levels[j])
        
        n1 <- nrow(sector1_data)
        n2 <- nrow(sector2_data)
        x1 <- sum(sector1_data$market_access_challenges_identified)
        x2 <- sum(sector2_data$market_access_challenges_identified)
        
        prop_test <- prop.test(c(x1, x2), c(n1, n2))
        
        print(paste(sector_levels[i], "vs", sector_levels[j], ":"))
        print(paste("  ", sector_levels[i], ": ", x1, "/", n1, " (", round(x1/n1*100, 1), "%)", sep=""))
        print(paste("  ", sector_levels[j], ": ", x2, "/", n2, " (", round(x2/n2*100, 1), "%)", sep=""))
        print(paste("  χ² =", round(prop_test$statistic, 2), 
                    ", p =", format(prop_test$p.value, scientific = TRUE),
                    ifelse(prop_test$p.value < alpha_corrected, " *", "")))
      }
    }
  }
}

# Identify highest and lowest sectors
if(nrow(sector_breakdown_ci) > 0) {
  highest_sector <- sector_breakdown_ci %>% slice_max(proportion, n = 1)
  lowest_sector <- sector_breakdown_ci %>% slice_min(proportion, n = 1)
  
  print(paste("Highest sector:", highest_sector$sector_focus, 
              "(", highest_sector$proportion, "%, ", highest_sector$ci_text, ")"))
  print(paste("Lowest sector:", lowest_sector$sector_focus, 
              "(", lowest_sector$proportion, "%, ", lowest_sector$ci_text, ")"))
}

# Combined sector-stakeholder analysis
if(length(unique(df$sector_focus)) > 1 && length(unique(df$stakeholder)) > 1) {
  combined_breakdown <- df %>%
    filter(!is.na(sector_focus), !is.na(stakeholder)) %>%
    group_by(sector_focus, stakeholder) %>%
    summarise(
      n = n(),
      challenges_identified = sum(market_access_challenges_identified, na.rm = TRUE),
      proportion = round(mean(market_access_challenges_identified, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    filter(n >= 10) %>%
    arrange(sector_focus, desc(proportion))
  
  if(nrow(combined_breakdown) > 0) {
    print("Market access challenges by sector and stakeholder (n ≥ 10):")
    print(combined_breakdown)
  }
}

# Range of proportions across sectors
if(nrow(sector_breakdown) > 1) {
  prop_range <- sector_breakdown %>%
    summarise(
      min_prop = min(proportion),
      max_prop = max(proportion),
      range = max_prop - min_prop
    )
  
  print(paste("Range across sectors:", prop_range$min_prop, "% to", 
              prop_range$max_prop, "% (range =", prop_range$range, "percentage points)"))
}

# Expected: 68% of respondents (95% CI [64.7%, 71.2%]) with significant variation by sector