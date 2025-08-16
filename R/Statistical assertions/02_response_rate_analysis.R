# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "venture capital firms (n=113, 8.6%), government funding agencies (n=38, 2.9%), 
#                        entrepreneurs (n=263, 20.1%), philanthropic organizations (n=16, 1.2%)"
# Purpose: Calculate stakeholder group distribution and percentages

library(tidyverse)
library(janitor)

# Load data
raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STAKEHOLDER <- "stakeholder"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(stakeholder = factor(stakeholder))

# Calculate counts and percentages
stakeholder_dist <- df %>%
  count(stakeholder) %>%
  mutate(percent = n / sum(n) * 100) %>%
  arrange(desc(n))

print(stakeholder_dist)
# Expected: VC=113 (8.6%), Govt=38 (2.9%), Entrepreneurs=263 (20.1%), etc. 