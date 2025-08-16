# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "Geographic distribution comprised North America (34%), Europe (32%), Asia (18%), and Other (16%)"
# Purpose: Calculate geographic distribution of respondents

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_REGION <- "region"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(region = factor(region))

# Calculate geographic distribution
geo_dist <- df %>%
  count(region) %>%
  mutate(percent = n / sum(n) * 100)

print(geo_dist)
# Expected: North America=34%, Europe=32%, Asia=18%, Other=16%