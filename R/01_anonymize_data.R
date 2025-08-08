# =============================================================================
# 01_anonymize_data.R
# Enhanced Data Anonymization Script for Climate Finance Survey
# Author: Richard McKinney
# Date: 2025-08-08
#
# Purpose:
#   - Load raw Qualtrics export (all columns as character)
#   - Create a stable respondent_id from whatever ID column exists
#   - Strip PII + raw identifiers
#   - Coerce dates to year-month safely (handles many formats & empty cells)
#   - Coarsen geography to regions
#   - Emit:
#       data/survey_responses_anonymized_basic.csv
#       data/data_dictionary.csv
#       docs/appendix_j_classification_template.csv
#       data/survey_responses_anonymized_{classified|preliminary}.csv
# =============================================================================

suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
  library(lubridate)
})

# ---- Directories -------------------------------------------------------------
dir.create("data",           showWarnings = FALSE, recursive = TRUE)
dir.create("data_processed", showWarnings = FALSE, recursive = TRUE)
dir.create("docs",           showWarnings = FALSE, recursive = TRUE)
dir.create("output",         showWarnings = FALSE, recursive = TRUE)

# ---- User-configurable: RAW INPUT PATH --------------------------------------
# Put your raw Qualtrics CSV under data_raw/ (unchanged file name is fine).
raw_input <- list.files("data_raw", pattern = "\\.csv$", full.names = TRUE) %>%
  # If multiple, pick the latest modified file.
  { if (length(.) == 0) stop("No CSV found under data_raw/") else . } %>%
  { .[which.max(file.mtime(.))] }

cat("Raw input: ", raw_input, "\n\n", sep = "")

# ---- Helpers ----------------------------------------------------------------

has_col <- function(df, nm) nm %in% names(df)

# Find first present column among a set
first_present_col <- function(df, candidates) {
  cands <- candidates[candidates %in% names(df)]
  if (length(cands)) cands[1] else NA_character_
}

# Safe, tolerant year-month coercion for various Qualtrics/Excel formats.
# Returns character "YYYY-MM" or NA.
safe_month <- function(x) {
  if (is.null(x)) return(rep(NA_character_, 0))
  x <- trimws(as.character(x))
  x[x == ""] <- NA_character_
  if (all(is.na(x))) return(x)

  # Try several common patterns *without* erroring:
  try_formats <- c(
    # Full timestamp (US style)
    "m/d/Y H:M:S", "m/d/Y H:M", "m/d/Y",
    # ISO-ish
    "Y-m-d H:M:S", "Y-m-d",
    # D/M/Y (EU)
    "d/m/Y H:M:S", "d/m/Y",
    # Wordy dates (rare)
    "Y-b-d", "b d, Y", "B d, Y"
  )

  # lubridate::parse_date_time is very tolerant.
  dt <- suppressWarnings(parse_date_time(x, orders = try_formats, tz = "UTC"))
  out <- ifelse(is.na(dt), NA_character_, format(dt, "%Y-%m"))
  out
}

# Deterministic short hash for IDs
create_hash_id <- function(x, prefix = "ID") {
  vapply(
    x,
    function(i) {
      if (is.na(i) || i == "") return(NA_character_)
      paste0(prefix, "_", substr(digest(as.character(i), algo = "sha256"), 1, 8))
    },
    character(1)
  )
}

# ---- Load raw as character (prevents readr guessing issues) -----------------
raw_data <- readr::read_csv(
  raw_input,
  col_types = readr::cols(.default = readr::col_character())
)

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")
cat("Loaded", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")

# ---- Guarantee respondent_id exists -----------------------------------------
# Qualtrics exports vary: ResponseId, _recordId, Response ID, etc.
id_candidates <- c("ResponseId", "_recordId", "Response ID", "Response_Id", "Response Id")
id_src <- first_present_col(raw_data, id_candidates)

if (is.na(id_src)) {
  # No source ID present: generate a temporary index for hashing
  raw_data[[".row_index_tmp"]] <- seq_len(nrow(raw_data))
  id_src <- ".row_index_tmp"
}

raw_data[["respondent_id"]] <- create_hash_id(raw_data[[id_src]], "RESP")

# ---- Coarsen geography -------------------------------------------------------
# Use Qualtrics Q2.2 where available, else try to build a region from hq_country.
# We keep the final field as "hq_region".
norm_names <- tolower(names(raw_data))

# Try to find a direct region-like column (Q2.2 or similar)
region_col <- first_present_col(
  raw_data,
  c("Q2.2", "Where is your organization headquartered? - Selected Choice", "hq_country", "HQ_COUNTRY", "country", "Country")
)

hq_region <- rep(NA_character_, nrow(raw_data))
if (!is.na(region_col)) {
  # If the column already contains region categories like "Europe / North America", keep it
  # otherwise do a country -> region mapping
  v <- raw_data[[region_col]]

  # Very light heuristic: if values already look like regions, just use them
  looks_like_region <- function(z) {
    any(grepl("\\b(asia|europe|north america|south america|australia|oceania|africa|gcc)\\b",
              tolower(z %||% ""), ignore.case = TRUE))
  }

  if (looks_like_region(v)) {
    hq_region <- v
  } else {
    # Country -> region mapping fallback
    vc <- tolower(v %||% "")
    hq_region[vc %in% c("united states","usa","us","canada","ca")] <- "North America"
    hq_region[vc %in% c("united kingdom","uk","england","scotland","wales",
                        "ireland","germany","de","france","fr","netherlands",
                        "switzerland","sweden","denmark","norway","spain",
                        "italy","belgium","austria","finland","greece","portugal")] <- "Europe"
    hq_region[vc %in% c("china","cn","japan","jp","singapore","sg","india","in",
                        "south korea","kr","hong kong","taiwan","id","thailand","vietnam","gcc")] <- "Asia"
    hq_region[vc %in% c("brazil","argentina","chile","mexico","colombia","peru")] <- "Latin America"
    hq_region[vc %in% c("australia","new zealand","oceania")] <- "Australia/Oceania"
    hq_region[vc %in% c("south africa","nigeria","kenya","egypt","morocco")] <- "Africa"
    hq_region[is.na(hq_region) & vc != ""] <- "Other"
  }
}

# ---- Build date year-month fields (tolerant to naming) -----------------------
start_col    <- first_present_col(raw_data, c("StartDate","Start Date","startDate","start date"))
end_col      <- first_present_col(raw_data, c("EndDate","End Date","endDate","end date"))
recorded_col <- first_present_col(raw_data, c("RecordedDate","Recorded Date","recordedDate","recorded date"))

StartDate_month    <- if (!is.na(start_col))    safe_month(raw_data[[start_col]])    else NA_character_
EndDate_month      <- if (!is.na(end_col))      safe_month(raw_data[[end_col]])      else NA_character_
RecordedDate_month <- if (!is.na(recorded_col)) safe_month(raw_data[[recorded_col]]) else NA_character_

# ---- Define columns to remove (PII + raw identifiers) -----------------------
pii_columns <- c(
  # network/identity/contacts
  "IPAddress", "ipAddress", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
  "recipientLastName", "recipientFirstName", "recipientEmail",
  "ExternalReference", "externalDataReference",
  "LocationLatitude", "LocationLongitude", "locationLatitude", "locationLongitude",
  "FULL_NAME", "GENDER", "ORGANIZATION_NAME",
  "hq_address_line_1", "hq_address_line_2", "hq_email", "hq_phone", "primary_contact_email",
  "primary_contact_phone", "website", "lp_name", "PRIMARY_POSITION", "description",
  # raw IDs we do not keep
  "ResponseId", "_recordId", "Response ID", "Response_Id", "Response Id",
  "FIRM_ORGANIZATION_ID", "FIRM_ORGANIZATION_ID_NUMBER", "ORGANIZATION_ID",
  "pbid"
)

# ---- Construct anonymized_basic ---------------------------------------------
cat("Anonymizing data (removing PII)...\n")

anonymized_basic <- raw_data %>%
  mutate(
    hq_region = hq_region,
    StartDate_month = StartDate_month,
    EndDate_month = EndDate_month,
    RecordedDate_month = RecordedDate_month
  ) %>%
  # Drop raw date/time columns if present
  select(-any_of(c(start_col, end_col, recorded_col))) %>%
  # Drop PII and raw identifiers, but KEEP respondent_id
  select(-any_of(setdiff(pii_columns, "respondent_id")))

# ---- Save basic anonymized ---------------------------------------------------
out_basic <- "data/survey_responses_anonymized_basic.csv"
readr::write_csv(anonymized_basic, out_basic)
cat("Saved basic anonymized data -> ", normalizePath(out_basic), "\n\n", sep = "")

# =============================================================================
# PART 2: PREPARE FOR APPENDIX J CLASSIFICATIONS
# =============================================================================
cat("=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

# Try to find role columns in multiple shapes
role_col  <- first_present_col(anonymized_basic, c("Q2.1",
  "Which of the following best describes your role? (Please select the most appropriate option) - Selected Choice",
  "role", "Role"))
role_other <- first_present_col(anonymized_basic, c("Q2.1_12_TEXT",
  "Which of the following best describes your role? (Please select the most appropriate option) - Other (please specify)  - Text",
  "role_other", "Role_Other"))

if (is.na(role_col)) {
  warning("Role column not found; classification template will be empty.")
}

# Build template only if we have respondent_id + at least one role field
template <- anonymized_basic %>%
  select(any_of(c("respondent_id", role_col, role_other))) %>%
  rename(
    role_raw   = !!rlang::sym(role_col)  %||% "role_raw",
    role_other = !!rlang::sym(role_other) %||% "role_other"
  )

if (!"respondent_id" %in% names(template)) {
  # extreme edge-case fallback
  template <- anonymized_basic %>%
    mutate(respondent_id = respondent_id %||% row_number()) %>%
    select(any_of(c("respondent_id", role_col, role_other))) %>%
    rename(
      role_raw   = !!rlang::sym(role_col)  %||% "role_raw",
      role_other = !!rlang::sym(role_other) %||% "role_other"
    )
}

# Provide a preliminary bucket to help manual coders
prelim_map <- function(x) {
  z <- tolower(x %||% "")
  dplyr::case_when(
    grepl("entrepreneur|founder|startup", z) ~ "Entrepreneur in Climate Technology",
    grepl("venture capital", z) & !grepl("corporate", z) ~ "Venture Capital Firm",
    grepl("corporate venture|cvc", z) ~ "Corporate Venture Arm",
    grepl("private equity", z) ~ "Private Equity Firm",
    grepl("family office", z) ~ "Family Office",
    grepl("angel", z) ~ "Angel Investor",
    grepl("limited partner|\\blp\\b", z) ~ "Limited Partner",
    grepl("esg investor", z) ~ "ESG Investor",
    grepl("government|ministry|department|agency|dfi|development finance|multilateral|bilateral", z) ~ "Government Funding Agency",
    grepl("philanthropic|foundation|donor|grantmaker", z) ~ "Philanthropic Organization",
    grepl("nonprofit|non-profit|ngo|charity|association|advocacy|civil society", z) ~ "Nonprofit Organization",
    grepl("academic|university|research", z) ~ "Academic or Research Institution",
    z == "" ~ NA_character_,
    TRUE ~ "Other"
  )
}

classification_template <- template %>%
  mutate(
    preliminary_category = prelim_map(role_raw %||% role_other),
    final_category_appendix_j = NA_character_
  )

tmpl_out <- "docs/appendix_j_classification_template.csv"
readr::write_csv(classification_template, tmpl_out)
cat("Classification template -> ", normalizePath(tmpl_out), "\n", sep = "")

# ---- Merge final classifications if present ---------------------------------
final_class_file <- "docs/appendix_j_classifications_complete.csv"

if (file.exists(final_class_file)) {
  cat("Found completed classifications. Merging...\n")
  final_class <- readr::read_csv(final_class_file, show_col_types = FALSE)

  # Expect at least respondent_id + final_category_appendix_j
  if (!all(c("respondent_id","final_category_appendix_j") %in% names(final_class))) {
    stop("`appendix_j_classifications_complete.csv` must contain respondent_id and final_category_appendix_j.")
  }

  anonymized_classified <- anonymized_basic %>%
    left_join(
      final_class %>% select(respondent_id, final_category_appendix_j),
      by = "respondent_id"
    )

  out_class <- "data/survey_responses_anonymized_classified.csv"
  readr::write_csv(anonymized_classified, out_class)
  cat("Classified data -> ", normalizePath(out_class), "\n", sep = "")
} else {
  cat("No completed classifications found. Writing preliminary version...\n")
  prelim <- anonymized_basic %>%
    left_join(
      classification_template %>% select(respondent_id, preliminary_category),
      by = "respondent_id"
    ) %>%
    mutate(stakeholder_category = preliminary_category)

  out_prelim <- "data/survey_responses_anonymized_preliminary.csv"
  readr::write_csv(prelim, out_prelim)
  cat("Preliminary classified data -> ", normalizePath(out_prelim), "\n", sep = "")
}

# =============================================================================
# PART 3: SUMMARY + DATA DICTIONARY
# =============================================================================
cat("\n=== ANONYMIZATION SUMMARY ===\n")
cat("Original columns:  ", ncol(raw_data), "\n")
cat("Anonymized columns:", ncol(anonymized_basic), "\n")
cat("Columns removed:   ", ncol(raw_data) - ncol(anonymized_basic), "\n")
cat("Rows preserved:    ", nrow(anonymized_basic), "\n")

data_dict <- tibble(
  variable_name   = names(anonymized_basic),
  type            = vapply(anonymized_basic, function(x) class(x)[1], character(1)),
  n_missing       = vapply(anonymized_basic, function(x) sum(is.na(x)), integer(1)),
  n_non_missing   = nrow(anonymized_basic) - n_missing,
  completeness_pct = round((n_non_missing / nrow(anonymized_basic)) * 100, 1)
)

dict_out <- "data/data_dictionary.csv"
readr::write_csv(data_dict, dict_out)
cat("Data dictionary -> ", normalizePath(dict_out), "\n", sep = "")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next steps:\n")
cat("1) Fill in 'docs/appendix_j_classifications_complete.csv' (based on the template).\n")
cat("2) Re-run this script to produce a fully classified dataset in data/.\n")