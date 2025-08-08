# 01_anonymize_data.R
# Enhanced Data Anonymization Script for Climate Finance Survey
# Author: Richard McKinney
# Date: 2025-08-08
# Purpose: Create anonymized versions of survey data with and without classifications

# =============================================================================
# Libraries (quiet + Rscript-safe)
# =============================================================================
if (!"methods" %in% loadedNamespaces()) library(methods)

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(stringr)
  library(purrr)
  library(digest)
  library(here)
})

# =============================================================================
# Output locations (ensure folders exist)
# =============================================================================
dir.create("data",           recursive = TRUE, showWarnings = FALSE)
dir.create("data_raw",       recursive = TRUE, showWarnings = FALSE)
dir.create("data_processed", recursive = TRUE, showWarnings = FALSE)
dir.create("docs",           recursive = TRUE, showWarnings = FALSE)
dir.create("output",         recursive = TRUE, showWarnings = FALSE)

out_anon_basic   <- here("data",           "survey_responses_anonymized_basic.csv")
out_class_prelim <- here("data_processed", "survey_responses_anonymized_preliminary.csv")
out_class_final  <- here("data_processed", "survey_responses_anonymized_classified.csv")
out_dict         <- here("data",           "data_dictionary.csv")
out_template     <- here("docs",           "appendix_j_classification_template.csv")
in_class_final   <- here("docs",           "appendix_j_classifications_complete.csv")

# =============================================================================
# Config: path to raw Qualtrics export (CSV)
#   - Keep the exact filename; update here if the export name changes.
# =============================================================================
raw_input <- here(
  "data_raw",
  "(Strategic Research) Accelerating climate finance_ An analysis of barriers and solutions_August 21, 2024_18.54.csv"
)

cat("Raw input:", raw_input, "\n\n")

# =============================================================================
# Helpers
# =============================================================================

# -- Robust Qualtrics reader ---------------------------------------------------
# Skips the 2-line Qualtrics preamble (question text + ImportId JSON).
# If the preamble is not standard, auto-detects & drops it.
read_qualtrics_csv <- function(path) {
  # First try the common case: skip the first 2 rows
  df <- suppressMessages(readr::read_csv(path, col_types = cols(.default = "c"), skip = 2))

  # Heuristic: if first row still looks like preamble, re-read and drop first 2
  looks_like_preamble <- function(x) {
    if (length(x) == 0) return(FALSE)
    head_vals <- na.omit(x)[seq_len(min(3, length(na.omit(x))))]
    any(grepl('^\\{"ImportId":', head_vals, useBytes = TRUE)) ||
      any(c("Start Date", "Recorded Date") %in% head_vals)
  }
  if (nrow(df) > 0 && looks_like_preamble(df$StartDate)) {
    df0 <- suppressMessages(readr::read_csv(path, col_types = cols(.default = "c"), skip = 0))
    if (nrow(df0) >= 3) df <- df0[-c(1, 2), , drop = FALSE]
  }

  # Make names syntactically simpler but keep key IDs readable
  names(df) <- gsub("\\s+", "", names(df))  # "Start Date" -> "StartDate"
  df
}

# -- Bullet-proof month extractor: returns "YYYY-MM" or NA; never errors -------
safe_month <- function(x) {
  if (is.null(x)) return(NA_character_)
  v <- trimws(as.character(x))
  v[v == ""] <- NA_character_

  try_formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y"
  )

  parse_one <- function(s) {
    if (is.na(s)) return(NA_character_)
    for (fmt in try_formats) {
      dt <- try(as.POSIXct(s, format = fmt, tz = "UTC"), silent = TRUE)
      if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m"))
    }
    # last-resort: lubridate if installed
    if (requireNamespace("lubridate", quietly = TRUE)) {
      dt <- try(lubridate::parse_date_time(
        s,
        orders = c("Ymd HMS","Ymd HM","Ymd","mdY HMS","mdY HM","mdY","dmy HMS","dmy HM","dmy"),
        tz = "UTC", quiet = TRUE
      ), silent = TRUE)
      if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m"))
    }
    NA_character_
  }

  vapply(v, parse_one, character(1))
}

# -- Deterministic hash helper -------------------------------------------------
hash_id <- function(x, prefix = "ID") {
  if (is.null(x)) return(NA_character_)
  sapply(x, function(i) {
    if (is.na(i) || i == "") return(NA_character_)
    paste0(prefix, "_", substr(digest(as.character(i), algo = "sha256"), 1, 10))
  }, USE.NAMES = FALSE)
}

# =============================================================================
#  PART 1: BASIC ANONYMIZATION
# =============================================================================

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")

if (!file.exists(raw_input)) {
  stop("Raw CSV not found at: ", raw_input, "\nPlease verify the filename in this script.")
}

raw_data <- read_qualtrics_csv(raw_input)

cat("Loaded", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")

cat("Anonymizing data (removing PII)...\n")

# Columns that can contain PII (remove if present)
pii_columns <- c(
  "IPAddress", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
  "ExternalReference", "LocationLatitude", "LocationLongitude",
  "FULL_NAME", "GENDER", "ORGANIZATION_NAME",
  "hq_address_line_1","hq_address_line_2","hq_email","hq_phone",
  "primary_contact_email","primary_contact_phone","website",
  "lp_name","PRIMARY_POSITION","description"
)

# Create anonymized basic dataset
df <- raw_data

# Safe month fields
if (!"StartDate_month" %in% names(df) && "StartDate" %in% names(df))   df$StartDate_month   <- safe_month(df$StartDate)
if (!"EndDate_month"   %in% names(df) && "EndDate"   %in% names(df))   df$EndDate_month     <- safe_month(df$EndDate)
if (!"RecordedDate_month" %in% names(df) && "RecordedDate" %in% names(df)) df$RecordedDate_month <- safe_month(df$RecordedDate)

# Hash IDs (when source columns exist)
if ("ResponseId" %in% names(df))     df$respondent_id   <- hash_id(df$ResponseId,   "RESP")
if ("ORGANIZATION_ID" %in% names(df))df$organization_id <- hash_id(df$ORGANIZATION_ID, "ORG")
if ("FIRM_ORGANIZATION_ID" %in% names(df))
  df$firm_id <- hash_id(df$FIRM_ORGANIZATION_ID, "FIRM")

# Region / geography: keep as is if Q2.2 exists (selected region)
# If you also have hq_* fields at street-level, drop them to avoid leakage.
geo_drop <- intersect(
  c("hq_state_province","hq_city","hq_zip_code","hq_country"),
  names(df)
)
if (length(geo_drop)) df <- select(df, -any_of(geo_drop))

# Remove explicit PII + known direct IDs that we’ve re-hashed
df <- df %>%
  select(-any_of(c(
    pii_columns,
    "ResponseId", "ORGANIZATION_ID", "FIRM_ORGANIZATION_ID",
    "FIRM_ORGANIZATION_ID_NUMBER", "pbid"
  )))

# Keep the raw date-time fields out (we’ve stored YYYY-MM)
df <- df %>% select(-any_of(c("StartDate","EndDate","RecordedDate")))

# Save the basic anonymized file
write_csv(df, out_anon_basic)
cat("Saved basic anonymized data ->", out_anon_basic, "\n")

# For downstream steps
anonymized_basic <- df

# =============================================================================
#  PART 2: PREPARE APPENDIX J CLASSIFICATION TEMPLATE
# =============================================================================

cat("\n=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

# Build template from Q2.1 + free text Q2.1_12_TEXT (when present)
q_role  <- if ("Q2.1" %in% names(anonymized_basic))  "Q2.1" else NA_character_
q_other <- if ("Q2.1_12_TEXT" %in% names(anonymized_basic)) "Q2.1_12_TEXT" else NA_character_

if (is.na(q_role)) {
  cat("Warning: Q2.1 not found; classification template will be empty.\n")
}

responses_to_classify <- anonymized_basic %>%
  mutate(
    role_raw = if (!is.na(q_role))  .data[[q_role]]  else NA_character_,
    role_other = if (!is.na(q_other)) .data[[q_other]] else NA_character_
  ) %>%
  select(any_of(c("respondent_id", "role_raw", "role_other"))) %>%
  filter(!is.na(role_raw) | !is.na(role_other))

# A light preliminary bucket (can be refined)
prelim_map <- function(x) {
  if (is.na(x) || x == "") return(NA_character_)
  y <- tolower(x)
  case_when(
    grepl("venture capital", y) & !grepl("corporate", y) ~ "Venture Capital Firm",
    grepl("corporate venture", y)                        ~ "Corporate Venture Arm",
    grepl("\\bangel\\b", y)                              ~ "Angel Investor",
    grepl("private equity", y)                           ~ "Private Equity Firm",
    grepl("family office", y)                            ~ "Family Office",
    grepl("limited partner|\\blp\\b", y)                 ~ "Limited Partner",
    grepl("philanthrop", y)                              ~ "Philanthropic Organization",
    grepl("non.?profit|ngo|charit", y)                   ~ "Nonprofit Organization",
    grepl("academic|research|universit", y)              ~ "Academic or Research Institution",
    grepl("government|ministry|department|agency|dfi", y)~ "Government Funding Agency",
    grepl("esg investor", y)                             ~ "ESG Investor",
    grepl("entrepreneur|founder|startup", y)             ~ "Entrepreneur in Climate Technology",
    TRUE ~ "Other"
  )
}

classification_template <- responses_to_classify %>%
  mutate(
    original_response = role_raw,
    other_text = role_other,
    preliminary_category = coalesce(prelim_map(original_response),
                                    ifelse(!is.na(other_text) & other_text != "", "NEEDS_CLASSIFICATION", NA_character_)),
    final_category_appendix_j = NA_character_
  ) %>%
  select(respondent_id, original_response, other_text,
         preliminary_category, final_category_appendix_j)

write_csv(classification_template, out_template)
cat("Classification template saved ->", out_template, "\n")
cat("Complete this file per Appendix J methodology and save as:\n  ", basename(in_class_final), " in docs/\n", sep = "")

# =============================================================================
#  PART 3: BUILD CLASSIFIED OUTPUT (preliminary or final if provided)
# =============================================================================

cat("\n=== PART 3: BUILD CLASSIFIED DATA ===\n")

if (file.exists(in_class_final)) {
  cat("Found completed classifications -> merging.\n")
  final_map <- suppressMessages(readr::read_csv(in_class_final, col_types = cols(.default = "c")))
  needed <- c("respondent_id", "final_category_appendix_j")
  missing_cols <- setdiff(needed, names(final_map))
  if (length(missing_cols)) {
    stop("Classification file is missing required columns: ", paste(missing_cols, collapse = ", "))
  }

  out_df <- anonymized_basic %>%
    left_join(
      final_map %>% select(respondent_id, final_category_appendix_j),
      by = "respondent_id"
    ) %>%
    left_join(
      classification_template %>% select(respondent_id, preliminary_category),
      by = "respondent_id"
    ) %>%
    mutate(stakeholder_category = coalesce(final_category_appendix_j, preliminary_category))

  write_csv(out_df, out_class_final)
  cat("Classified data saved ->", out_class_final, "\n")

} else {
  cat("Completed classification file not found.\n")
  cat("Creating PRELIMINARY classified data (using preliminary_category only)...\n")

  out_df <- anonymized_basic %>%
    left_join(
      classification_template %>% select(respondent_id, preliminary_category),
      by = "respondent_id"
    ) %>%
    mutate(stakeholder_category = preliminary_category)

  write_csv(out_df, out_class_prelim)
  cat("Preliminary classified data saved ->", out_class_prelim, "\n")
}

# =============================================================================
#  SUMMARY + DATA DICTIONARY
# =============================================================================

cat("\n=== ANONYMIZATION SUMMARY ===\n")
cat("Original columns:", ncol(raw_data), "\n")
cat("Anonymized columns:", ncol(anonymized_basic), "\n")
cat("Columns removed:", ncol(raw_data) - ncol(anonymized_basic), "\n")
cat("Rows preserved:", nrow(anonymized_basic), "\n")

data_dict <- tibble(
  variable_name   = names(anonymized_basic),
  type            = vapply(anonymized_basic, function(x) class(x)[1], character(1)),
  n_missing       = vapply(anonymized_basic, function(x) sum(is.na(x)), integer(1))
) %>%
  mutate(n_non_missing   = nrow(anonymized_basic) - n_missing,
         completeness_pct = round(100 * n_non_missing / nrow(anonymized_basic), 1))

write_csv(data_dict, out_dict)
cat("Data dictionary saved ->", out_dict, "\n")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next steps:\n")
cat("1) Open and complete:", out_template, "\n")
cat("2) Save completed file as:", in_class_final, "\n")
cat("3) Re-run this script to produce:", out_class_final, "\n\n")