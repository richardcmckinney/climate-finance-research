# 02_main_analysis.R
# Purpose: Compatibility wrapper to run core outputs off the FINAL dataset.
# Input : data/climate_finance_survey_final_1307.csv
# Output: delegates to 03_main_analysis.R artifacts.

if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))

stopifnot(file.exists("data/climate_finance_survey_final_1307.csv"))
df <- read.csv("data/climate_finance_survey_final_1307.csv", stringsAsFactors = FALSE)
cat("Dataset loaded for analysis: ", nrow(df), " rows, ", ncol(df), " columns\n", sep = "")
cat("Stakeholder categories present: ", paste(sort(unique(df$Final_Role_Category)), collapse = "; "), "\n\n")

source("R/03_main_analysis.R")