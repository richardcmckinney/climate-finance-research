# =============== Global Setup & Variable Mapping =================
# Statistical Assertion 75: "Secondary market utilization correlated with portfolio size (r = .342, 95% CI [.298, .385])"
# Purpose: Calculate correlation between secondary market usage and portfolio size with bootstrap CI

library(tidyverse)
library(janitor)
library(boot)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_SECONDARY_MARKET <- "secondary_market_utilization"
COL_PORTFOLIO_SIZE <- "portfolio_size_numeric"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    secondary_market_utilization = as.numeric(secondary_market_utilization),
    portfolio_size_numeric = as.numeric(portfolio_size_numeric)
  ) %>%
  filter(!is.na(secondary_market_utilization), !is.na(portfolio_size_numeric))

# Basic correlation
cor_result <- cor.test(df$secondary_market_utilization, df$portfolio_size_numeric, 
                      method = "pearson")

print(paste("Pearson correlation: r =", round(cor_result$estimate, 3)))
print(paste("95% CI: [", round(cor_result$conf.int[1], 3), ", ", 
            round(cor_result$conf.int[2], 3), "]", sep=""))
print(paste("p-value:", format(cor_result$p.value, scientific = TRUE)))

# Bootstrap confidence interval for correlation
boot_cor <- function(data, indices) {
  d <- data[indices, ]
  return(cor(d$secondary_market_utilization, d$portfolio_size_numeric, use = "complete.obs"))
}

boot_result <- boot(df, boot_cor, R = 10000)
boot_ci <- boot.ci(boot_result, type = "perc", conf = 0.95)

print("Bootstrap 95% CI:")
print(paste("[", round(boot_ci$percent[4], 3), ", ", round(boot_ci$percent[5], 3), "]", sep=""))

# Scatter plot summary
print(paste("Sample size:", nrow(df)))
print(paste("Secondary market usage mean:", round(mean(df$secondary_market_utilization), 2)))
print(paste("Portfolio size mean:", round(mean(df$portfolio_size_numeric), 2)))

# Expected: r = .342, 95% CI [.298, .385]