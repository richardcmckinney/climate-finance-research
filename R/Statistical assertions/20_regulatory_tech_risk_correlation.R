# =============== Global Setup & Variable Mapping =================
# Statistical Assertion: "66% completion rate for technology module (n=862)"
# Purpose: Calculate technology module completion rate

library(tidyverse)
library(janitor)

raw <- read.csv("survey_responses_anonymized_preliminary.csv") %>% clean_names()

# Variable mapping
COL_TECH_MODULE_OK <- "tech_module_completed"

df <- raw %>%
  filter(status == "IP Address", as.numeric(progress) >= 10) %>%
  mutate(tech_module_completed = as.integer(tech_module_completed))

n_total <- nrow(df)
n_tech_complete <- sum(df$tech_module_completed, na.rm = TRUE)
completion_rate <- n_tech_complete / n_total * 100

print(paste("Technology module completion: n =", n_tech_complete, 
            "(", round(completion_rate, 1), "% )"))

# Expected: n=862 (66%) completion rate for technology module
#