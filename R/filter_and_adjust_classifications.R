#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# filter_and_adjust_classifications.R
# Purpose: Report deficits vs Appendix J and propose a donor pool (audit utility)

suppressPackageStartupMessages(library(tidyverse))

dir.create("output", recursive = TRUE, showWarnings = FALSE)

df <- read.csv("data/climate_finance_survey_classified.csv", stringsAsFactors = FALSE)
if (!"ResponseId" %in% names(df)) df$ResponseId <- seq_len(nrow(df))
df$Progress <- suppressWarnings(as.numeric(df$Progress))

df_complete <- if ("Progress" %in% names(df)) dplyr::filter(df, Progress >= 10) else df

target <- tibble::tribble(
  ~Category, ~Target,
  "Entrepreneur in Climate Technology", 159, "Venture Capital Firm", 117,
  "Investment and Financial Services", 109, "Private Equity Firm", 88,
  "Business Consulting and Advisory", 79, "Nonprofit Organization", 73,
  "High Net-Worth Individual", 66, "Government Funding Agency", 53,
  "Academic or Research Institution", 52, "Limited Partner", 49,
  "Family Office", 48, "Corporate Venture Arm", 47, "Angel Investor", 43,
  "ESG Investor", 38, "Legal Services", 38, "Corporate Entities", 35,
  "Manufacturing and Industrial", 25, "Energy and Infrastructure", 24,
  "Real Estate and Property", 20, "Philanthropic Organization", 19,
  "Technology and Software", 19, "Media and Communication", 7,
  "Miscellaneous and Individual Respondents", 151
)

role_col <- "Final_Role_Category"
curr <- df_complete %>% count(.data[[role_col]], name = "Current") %>% rename(Category = !!role_col)
cmp <- full_join(curr, target, by = "Category") %>%
  mutate(across(c(Current, Target), ~replace_na(., 0L)),
         Deficit = Target - Current) %>%
  arrange(desc(Deficit))

write.csv(cmp, "output/classification_deficit_report.csv", row.names = FALSE)
cat("Wrote deficit report → output/classification_deficit_report.csv\n")