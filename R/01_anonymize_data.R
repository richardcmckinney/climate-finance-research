#!/usr/bin/env Rscript
# 01_anonymize_data.R — Robust anonymization + Appendix J prep
# - Drops telemetry/PII columns by name pattern
# - Scrubs residual PII from free‑text fields (emails/phones/URLs/@handles)
# - Preserves `respondent_id` exactly (for stable joins)
# - Writes: basic anonymized CSV, classification template, preliminary CSV, dictionary

suppressPackageStartupMessages({
  if (!"methods" %in% loadedNamespaces()) library(methods)
  library(readr)
  library(dplyr)
  library(stringr)
  library(tibble)
  library(lubridate)
  library(digest)
  library(cli)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------- Configuration -------------------------
config <- list(
  pii_patterns = c(
    "(?i)ip[_ ]?address|^IPAddress$",
    "(?i)email|e[-_ ]?mail|RecipientEmail",
    "(?i)first[_ ]?name|last[_ ]?name|full[_ ]?name|^name$",
    "(?i)recipient|Recipient|ExternalDataReference|external.*reference",
    "(?i)latitude|longitude|LocationLatitude|LocationLongitude",
    "(?i)\\bhq_?address|address[_ ]?line|street|city|^state$|zip|postal",
    "(?i)phone|mobile|cell|tel|fax",
    "(?i)website|url|^link$",
    "^ResponseId$|^StartDate$|^EndDate$|^RecordedDate$",
    "(?i)birth.*date|dob|date.*birth",
    "(?i)ssn|social.*security",
    "(?i)passport|driver.*license|license.*number"
  ),
  output_paths = list(
    basic       = "data/survey_responses_anonymized_basic.csv",
    template    = "docs/appendix_j_classification_template.csv",
    preliminary = "data/survey_responses_anonymized_preliminary.csv",
    dictionary  = "data/data_dictionary.csv"
  ),
  hash_length = 10
)

# ------------------------- Helpers -------------------------
find_col <- function(df, candidates = character(0), pattern = NULL) {
  if (!is.data.frame(df)) {
    cli::cli_warn("find_col: input is not a data frame")
    return(NA_character_)
  }
  
  nms <- names(df)
  hit <- candidates[candidates %in% nms]
  if (length(hit)) return(hit[1])
  
  if (!is.null(pattern)) {
    rx <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
    if (length(rx)) return(rx[1])
  }
  NA_character_
}

safe_month <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x)
  
  # Try multiple date formats
  date_formats <- c(
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%d %H:%M",
    "%m/%d/%Y %H:%M:%S",
    "%m/%d/%Y %H:%M",
    "%d/%m/%Y %H:%M:%S",
    "%d/%m/%Y %H:%M",
    "%Y-%m-%d",
    "%m/%d/%Y",
    "%d/%m/%Y",
    "%Y/%m/%d"
  )
  
  parsed <- as.POSIXct(NA)
  for (fmt in date_formats) {
    attempt <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC"))
    parsed[!is.na(attempt)] <- attempt[!is.na(attempt)]
  }
  
  # For any remaining NAs, try lubridate
  if (anyNA(parsed)) {
    na_idx <- which(is.na(parsed))
    parsed[na_idx] <- suppressWarnings(lubridate::parse_date_time(
      x[na_idx],
      orders = c("ymd HMS", "ymd HM", "mdy HMS", "mdy HM", 
                 "dmy HMS", "dmy HM", "ymd", "mdy", "dmy"),
      tz = "UTC", quiet = TRUE
    ))
  }
  
  out <- format(parsed, "%Y-%m")
  out[is.na(parsed)] <- NA_character_
  out
}

hash_id <- function(x, pref) {
  vapply(as.character(x), function(i) {
    if (is.na(i) || i == "") return(NA_character_)
    paste0(pref, "_", substr(digest(i, algo = "sha256"), 1, config$hash_length))
  }, character(1), USE.NAMES = FALSE)
}

scrub_text <- function(x) {
  if (!is.character(x)) return(x)
  
  # Email addresses - more comprehensive pattern
  x <- gsub("(?i)[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}", "[REDACTED_EMAIL]", x, perl = TRUE)
  
  # Phone numbers - multiple formats
  # International format
  x <- gsub("\\+?[1-9]\\d{0,3}[\\s\\-\\.]?\\(?\\d{1,4}\\)?[\\s\\-\\.]?\\d{1,4}[\\s\\-\\.]?\\d{1,9}", "[REDACTED_PHONE]", x, perl = TRUE)
  # US format (xxx) xxx-xxxx
  x <- gsub("\\(?\\d{3}\\)?[\\s\\-\\.]?\\d{3}[\\s\\-\\.]?\\d{4}", "[REDACTED_PHONE]", x, perl = TRUE)
  # Generic phone pattern
  x <- gsub("\\b\\d{3,4}[\\s\\-\\.]\\d{3,4}[\\s\\-\\.]\\d{3,4}\\b", "[REDACTED_PHONE]", x, perl = TRUE)
  
  # URLs - comprehensive pattern
  x <- gsub("(?i)(https?://[^\\s]+|ftp://[^\\s]+|www\\.[A-Za-z0-9\\-]+\\.[A-Za-z]{2,}[^\\s]*)", "[REDACTED_URL]", x, perl = TRUE)
  
  # Social media handles
  x <- gsub("@[A-Za-z0-9_]{1,30}\\b", "[REDACTED_HANDLE]", x, perl = TRUE)
  
  # Credit card numbers (basic pattern)
  x <- gsub("\\b\\d{4}[\\s\\-]?\\d{4}[\\s\\-]?\\d{4}[\\s\\-]?\\d{4}\\b", "[REDACTED_CARD]", x, perl = TRUE)
  
  # SSN pattern (xxx-xx-xxxx)
  x <- gsub("\\b\\d{3}-\\d{2}-\\d{4}\\b", "[REDACTED_SSN]", x, perl = TRUE)
  
  x
}

map_prelim_role <- function(role, other_text) {
  patterns <- list(
    "Entrepreneur in Climate Technology" = "entrepreneur|founder|startup|cto|ceo",
    "Venture Capital Firm"               = "(?<!corporate\\s)venture\\s*capital",
    "Private Equity Firm"                = "private\\s*equity",
    "Corporate Venture Arm"              = "corporate\\s*venture",
    "Angel Investor"                     = "\\bangel\\b",
    "Limited Partner"                    = "limited\\s*partner|\\blp\\b",
    "Family Office"                      = "family\\s*office",
    "High Net-Worth Individual"          = "high\\s*net|hnwi",
    "Government Funding Agency"          = "government|dfi|development\\s*finance|multilateral|bilateral|agency",
    "Philanthropic Organization"         = "philanthrop|foundation|grantmaker",
    "Academic or Research Institution"   = "academic|research|university|institution",
    "Nonprofit Organization"             = "non[- ]?profit|ngo|civil\\s*society|association|institute",
    "Miscellaneous and Individual Respondents" = "consultant|individual|freelance|other"
  )
  z <- tolower(paste(coalesce(role, ""), coalesce(other_text, "")))
  vapply(z, function(txt) {
    for (cat in names(patterns)) {
      if (grepl(patterns[[cat]], txt, perl = TRUE)) return(cat)
    }
    "Other"
  }, character(1), USE.NAMES = FALSE)
}

# ------------------------- Validate Environment -------------------------
# Check for required directories
if (!dir.exists("data_raw")) {
  cli::cli_abort("Directory 'data_raw' not found. Please ensure raw data directory exists.")
}

# Create output directories
for (path in config$output_paths) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

# ------------------------- Load data -------------------------
cli::cli_h1("PART 1: BASIC ANONYMIZATION")
raw_files <- list.files("data_raw", pattern = "\\.csv$", full.names = TRUE)

if (length(raw_files) == 0) {
  cli::cli_abort("No CSV files found in data_raw directory")
}

raw_in <- raw_files[1]
cli::cli_inform(paste("Raw input:", raw_in))

# Load with error handling
raw <- tryCatch({
  readr::read_csv(raw_in, guess_max = 10000, show_col_types = FALSE)
}, error = function(e) {
  cli::cli_abort(paste("Failed to read CSV file:", e$message))
})

if (nrow(raw) == 0) {
  cli::cli_abort("Input file contains no data rows")
}

cli::cli_inform(paste("Loaded", nrow(raw), "rows and", ncol(raw), "columns"))

# Column detection
col_response_id <- find_col(raw, c("ResponseId", "_recordId", "Response ID", "response_id"), 
                           pattern = "^response[_ ]?id$|^_recordid$")
col_start  <- find_col(raw, c("StartDate", "startDate", "Start Date", "start_date"), 
                      pattern = "^start[_ ]?date$")
col_end    <- find_col(raw, c("EndDate", "endDate", "End Date", "end_date"), 
                      pattern = "^end[_ ]?date$")
col_record <- find_col(raw, c("RecordedDate", "recordedDate", "Recorded Date", "recorded_date"), 
                      pattern = "^recorded[_ ]?date$")

# role fields (for template)
col_role        <- find_col(raw, c("Q2.1", "QID174", "role"), 
                           pattern = "best describes your role|\\bQ2\\.1\\b|QID174")
col_role_other  <- find_col(raw, c("Q2.1_12_TEXT", "QID174_12_TEXT", "role_other"), 
                           pattern = "other.*text|Q2\\.1.*TEXT|QID174_.*TEXT")

# Build anonymized basic
resp_id <- if (!is.na(col_response_id)) {
  raw[[col_response_id]]
} else {
  cli::cli_warn("ResponseId column not found, using sequential IDs")
  seq_len(nrow(raw))
}

# Compile PII regex pattern
pii_regex <- paste(config$pii_patterns, collapse = "|")

# Create anonymized dataset
an_basic <- raw %>%
  mutate(
    respondent_id = hash_id(resp_id, "RESP"),
    across(.cols = any_of(c(col_start, col_end, col_record)), 
           .fns = ~ safe_month(.x), 
           .names = "{.col}_month")
  )

# Remove PII columns
cols_to_remove <- grep(pii_regex, names(an_basic), ignore.case = TRUE, value = TRUE)
if (length(cols_to_remove) > 0) {
  cli::cli_inform(paste("Removing", length(cols_to_remove), "PII columns"))
  an_basic <- an_basic %>% 
    select(-all_of(cols_to_remove)) %>%
    select(-any_of(c(col_start, col_end, col_record)))
}

# Ensure respondent_id is first
an_basic <- an_basic %>%
  relocate(respondent_id, .before = 1)

# Scrub PII in character columns EXCEPT respondent_id
char_cols <- names(an_basic)[vapply(an_basic, is.character, logical(1))]
char_cols <- setdiff(char_cols, "respondent_id")

if (length(char_cols) > 0) {
  cli::cli_inform(paste("Scrubbing PII from", length(char_cols), "text columns"))
  an_basic <- an_basic %>% 
    mutate(across(all_of(char_cols), scrub_text))
}

# Save basic anonymized file
cli::cli_inform("Saving basic anonymized data...")
readr::write_csv(an_basic, config$output_paths$basic)
cli::cli_inform(paste("✓ Saved basic anonymized data ->", normalizePath(config$output_paths$basic)))

# ------------------------- Appendix J preparation -------------------------
cli::cli_h1("PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS")

if (!is.na(col_role)) {
  role_df <- tibble(
    respondent_id = an_basic$respondent_id,
    role_raw      = if (!is.na(col_role)) raw[[col_role]] else NA_character_,
    role_other    = if (!is.na(col_role_other)) raw[[col_role_other]] else NA_character_
  )

  template <- role_df %>%
    mutate(
      needs_harmonization       = tolower(coalesce(role_raw, "")) %in% c("other", "other (please specify)"),
      preliminary_category      = map_prelim_role(role_raw, role_other),
      final_category_appendix_j = NA_character_
    ) %>%
    arrange(respondent_id) %>%
    distinct(respondent_id, .keep_all = TRUE)

  readr::write_csv(template, config$output_paths$template)
  cli::cli_inform(paste("✓ Classification template ->", normalizePath(config$output_paths$template)))

  out_prelim <- an_basic %>%
    left_join(template %>% select(respondent_id, preliminary_category), by = "respondent_id") %>%
    rename(stakeholder_category = preliminary_category) %>%
    arrange(respondent_id) %>%
    distinct(respondent_id, .keep_all = TRUE)

  readr::write_csv(out_prelim, config$output_paths$preliminary)
  cli::cli_inform(paste("✓ Preliminary classified data ->", normalizePath(config$output_paths$preliminary)))
} else {
  cli::cli_warn("Role column not found; skipping template and preliminary outputs.")
}

# ------------------------- Summary & dictionary -------------------------
cli::cli_h1("ANONYMIZATION SUMMARY")
cli::cli_inform(paste("Original columns:", ncol(raw)))
cli::cli_inform(paste("Anonymized columns:", ncol(an_basic)))
cli::cli_inform(paste("Rows preserved:", nrow(an_basic)))
cli::cli_inform(paste("PII columns removed:", length(cols_to_remove)))

# Create data dictionary
dict <- tibble(
  column_name      = names(an_basic),
  description      = "",
  type             = vapply(an_basic, function(x) class(x)[1], character(1)),
  n_missing        = vapply(an_basic, function(x) sum(is.na(x)), integer(1))
) %>%
  mutate(
    n_non_missing    = nrow(an_basic) - n_missing,
    completeness_pct = round(n_non_missing / nrow(an_basic) * 100, 1)
  )

readr::write_csv(dict, config$output_paths$dictionary)
cli::cli_inform(paste("✓ Data dictionary ->", normalizePath(config$output_paths$dictionary)))

# Final validation
cli::cli_h1("VALIDATION")
if (nrow(an_basic) != nrow(raw)) {
  cli::cli_warn(paste("Row count changed:", nrow(raw), "->", nrow(an_basic)))
}

if (any(duplicated(an_basic$respondent_id))) {
  cli::cli_warn("Duplicate respondent IDs detected!")
}

cli::cli_h1("✓ PROCESS COMPLETE")