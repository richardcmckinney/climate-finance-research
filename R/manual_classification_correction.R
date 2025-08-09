#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# manual_classification_correction.R
# Purpose: Create a manual reassignment template (if you choose hand corrections)
# Input : data/climate_finance_survey_classified.csv
# Output: output/manual_reassignment_template.csv

suppressPackageStartupMessages(library(tidyverse))
dir.create("output", recursive = TRUE, showWarnings = FALSE)

infile <- "data/climate_finance_survey_classified.csv"
if (!file.exists(infile)) stop("Missing input: ", infile)

df <- read.csv(infile, stringsAsFactors = FALSE)
if (!"ResponseId" %in% names(df)) df$ResponseId <- seq_len(nrow(df))

template <- df %>%
  select(ResponseId, `Q2.1`, `Q2.1_12_TEXT`, Final_Role_Category) %>%
  mutate(Suggested_New_Category = NA_character_,
         Notes = NA_character_)

write.csv(template, "output/manual_reassignment_template.csv", row.names = FALSE)
cat("Wrote manual reassignment template → output/manual_reassignment_template.csv\n")