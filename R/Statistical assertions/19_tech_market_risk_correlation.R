# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "venture capitalists showed mean r=.451... government agencies showed mean r=.312... ESG investors showed mean r=.248... philanthropic organizations showed mean r=.156"
# Purpose: Calculate average within-group correlations for strategic variables by stakeholder type

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"
WITHIN_GROUP_ITEMS <- c("criterion_returns", "criterion_impact", "criterion_scalability",
                       "criterion_team", "criterion_tech_maturity")

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(
    stakeholder = factor(stakeholder),
    across(all_of(WITHIN_GROUP_ITEMS), as.numeric)
  )

# Calculate mean within-group correlations
groups <- c("Venture Capital", "Government Agency", "ESG Investor", "Philanthropic")
for(grp in groups) {
  grp_data <- df %>%
    filter(stakeholder == grp) %>%
    select(all_of(WITHIN_GROUP_ITEMS)) %>%
    na.omit()
  
  if(nrow(grp_data) > 2) {
    cor_matrix <- cor(grp_data, use = "complete.obs")
    # Get lower triangle (excluding diagonal)
    lower_tri <- cor_matrix[lower.tri(cor_matrix)]
    mean_cor <- mean(lower_tri, na.rm = TRUE)
    
    print(paste(grp, "mean within-group correlation: r =", round(mean_cor, 3)))
  }
}

# Expected: VC r=.451, Govt r=.312, ESG r=.248, Philanthropic r=.156