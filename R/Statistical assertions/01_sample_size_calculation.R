# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "1,307 provided usable responses"
# Purpose: Verify final sample size after filtering for complete responses

library(tidyverse)
library(janitor)

# Load data
raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_STATUS <- "status"  # Completion status
COL_PROGRESS <- "progress"  # Progress percentage

# Filter for complete responses
df <- raw %>%
  filter(status == "IP Address", 
         as.numeric(progress) >= 10)

n_final <- nrow(df)
print(paste("Total usable responses:", n_final))
# Expected: 1307 provided usable responses