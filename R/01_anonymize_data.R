#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# 01_anonymize_data.R — Pure anonymization with data cleaning
# Version 2.0 - Now uses central configuration
# - Drops telemetry/PII columns by name pattern
# - Cleans core metadata columns (Progress, dates)
# - Scrubs residual PII from free‑text fields
# - Preserves `respondent_id` exactly (for stable joins)
# - Writes: basic anonymized CSV and data dictionary

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

# SOURCE CENTRAL CONFIGURATION
source("R/00_config.R")
check_deprecated()  # Check for old files
set_stage("Anonymization")  # Set pipeline stage

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------- Configuration -------------------------
# Now uses PATHS from central config
config <- list(
  pii_patterns = PRIVACY_COLUMNS,  # Use central privacy patterns
  output_paths = list(
    basic = PATHS$basic_anon,      # Use path from central config
    dictionary = PATHS$dictionary   # Use path from central config
  ),
  hash_length = 10,
  min_progress = QUALITY_PARAMS$min_progress  # Use central quality params
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
  
  # Email addresses
  x <- gsub("(?i)[A-Za-z0-9._%+\\-]+@[A-Za-z0-9.\\-]+\\.[A-Za-z]{2,}", "[REDACTED_EMAIL]", x, perl = TRUE)
  
  # Phone numbers
  x <- gsub("\\+?[1-9]\\d{0,3}[\\s\\-\\.]?\\(?\\d{1,4}\\)?[\\s\\-\\.]?\\d{1,4}[\\s\\-\\.]?\\d{1,9}", "[REDACTED_PHONE]", x, perl = TRUE)
  x <- gsub("\\(?\\d{3}\\)?[\\s\\-\\.]?\\d{3}[\\s\\-\\.]?\\d{4}", "[REDACTED_PHONE]", x, perl = TRUE)
  x <- gsub("\\b\\d{3,4}[\\s\\-\\.]\\d{3,4}[\\s\\-\\.]\\d{3,4}\\b", "[REDACTED_PHONE]", x, perl = TRUE)
  
  # URLs
  x <- gsub("(?i)(https?://[^\\s]+|ftp://[^\\s]+|www\\.[A-Za-z0-9\\-]+\\.[A-Za-z]{2,}[^\\s]*)", "[REDACTED_URL]", x, perl = TRUE)
  
  # Social media handles
  x <- gsub("@[A-Za-z0-9_]{1,30}\\b", "[REDACTED_HANDLE]", x, perl = TRUE)
  
  # Credit card numbers
  x <- gsub("\\b\\d{4}[\\s\\-]?\\d{4}[\\s\\-]?\\d{4}[\\s\\-]?\\d{4}\\b", "[REDACTED_CARD]", x, perl = TRUE)
  
  # SSN pattern
  x <- gsub("\\b\\d{3}-\\d{2}-\\d{4}\\b", "[REDACTED_SSN]", x, perl = TRUE)
  
  x
}

# ------------------------- Validate Environment -------------------------
if (!dir.exists("data_raw")) {
  cli::cli_abort("Directory 'data_raw' not found. Please ensure raw data directory exists.")
}

# Create output directories using safe_path from config
for (path in config$output_paths) {
  safe_path(path)  # This creates directories as needed
}

# ------------------------- Load data -------------------------
cli::cli_h1("ANONYMIZATION AND DATA CLEANING")
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

# Column detection using STANDARD_COLUMNS where possible
col_response_id <- find_col(raw, c("ResponseId", "_recordId", "Response ID", "response_id"), 
                           pattern = "^response[_ ]?id$|^_recordid$")
col_start  <- find_col(raw, c("StartDate", "startDate", "Start Date", "start_date"), 
                      pattern = "^start[_ ]?date$")
col_end    <- find_col(raw, c("EndDate", "endDate", "End Date", "end_date"), 
                      pattern = "^end[_ ]?date$")
col_record <- find_col(raw, c("RecordedDate", "recordedDate", "Recorded Date", "recorded_date"), 
                      pattern = "^recorded[_ ]?date$")
col_progress <- find_col(raw, c("Progress", "progress", "Progress_pct", "completion", "Completion", 
                               "PercentComplete", "percent_complete", "pct_complete"),
                        pattern = "prog|compl|pct|percent")

# Build anonymized basic
resp_id <- if (!is.na(col_response_id)) {
  raw[[col_response_id]]
} else {
  cli::cli_warn("ResponseId column not found, using sequential IDs")
  seq_len(nrow(raw))
}

# Create anonymized dataset with initial transforms
an_basic <- raw %>%
  mutate(
    !!STANDARD_COLUMNS$respondent_id := hash_id(resp_id, "RESP"),  # Use standard column name
    across(.cols = any_of(c(col_start, col_end, col_record)), 
           .fns = ~ safe_month(.x), 
           .names = "{.col}_month")
  )

# Clean Progress column if present
if (!is.na(col_progress)) {
  cli::cli_inform(paste("Cleaning Progress column:", col_progress))
  
  # Convert to numeric
  an_basic[[col_progress]] <- suppressWarnings(as.numeric(an_basic[[col_progress]]))
  
  # Handle percentage values (if Progress is 0-1, convert to 0-100)
  if (!all(is.na(an_basic[[col_progress]])) && 
      max(an_basic[[col_progress]], na.rm = TRUE) <= 1) {
    cli::cli_inform("Converting Progress from decimal (0-1) to percentage (0-100)")
    an_basic[[col_progress]] <- an_basic[[col_progress]] * 100
  }
  
  # Replace NA with 0
  n_na_progress <- sum(is.na(an_basic[[col_progress]]))
  if (n_na_progress > 0) {
    cli::cli_inform(paste("Replacing", n_na_progress, "NA Progress values with 0"))
    an_basic[[col_progress]][is.na(an_basic[[col_progress]])] <- 0
  }
  
  # Ensure Progress column uses standard name
  if (col_progress != STANDARD_COLUMNS$progress) {
    an_basic[[STANDARD_COLUMNS$progress]] <- an_basic[[col_progress]]
  }
} else {
  cli::cli_warn("Progress column not found - downstream scripts may need adjustment")
}

# Build PII regex pattern from central config
pii_patterns_extra <- c(
  "(?i)ip[_ ]?address|^IPAddress$",
  "(?i)recipient|Recipient|ExternalDataReference|external.*reference",
  "(?i)\\bhq_?address|address[_ ]?line|street|city|^state$|zip|postal",
  "(?i)website|url|^link$",
  "^ResponseId$|^StartDate$|^EndDate$|^RecordedDate$",
  "(?i)birth.*date|dob|date.*birth",
  "(?i)ssn|social.*security",
  "(?i)passport|driver.*license|license.*number"
)

pii_regex <- paste(c(PRIVACY_COLUMNS, pii_patterns_extra), collapse = "|")

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
  relocate(!!STANDARD_COLUMNS$respondent_id, .before = 1)

# Scrub PII in character columns EXCEPT respondent_id
char_cols <- names(an_basic)[vapply(an_basic, is.character, logical(1))]
char_cols <- setdiff(char_cols, STANDARD_COLUMNS$respondent_id)

if (length(char_cols) > 0) {
  cli::cli_inform(paste("Scrubbing PII from", length(char_cols), "text columns"))
  an_basic <- an_basic %>% 
    mutate(across(all_of(char_cols), scrub_text))
}

# Check for privacy violations before saving
check_privacy_violations(an_basic, stop_on_violation = FALSE)

# Save basic anonymized file
cli::cli_inform("Saving basic anonymized data...")
readr::write_csv(an_basic, config$output_paths$basic)
cli::cli_inform(paste("✓ Saved basic anonymized data ->", normalizePath(config$output_paths$basic)))

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

if (any(duplicated(an_basic[[STANDARD_COLUMNS$respondent_id]]))) {
  cli::cli_warn("Duplicate respondent IDs detected!")
}

# Progress statistics if available
if (STANDARD_COLUMNS$progress %in% names(an_basic)) {
  progress_stats <- an_basic %>%
    summarise(
      mean_progress = mean(.data[[STANDARD_COLUMNS$progress]], na.rm = TRUE),
      median_progress = median(.data[[STANDARD_COLUMNS$progress]], na.rm = TRUE),
      min_progress = min(.data[[STANDARD_COLUMNS$progress]], na.rm = TRUE),
      max_progress = max(.data[[STANDARD_COLUMNS$progress]], na.rm = TRUE),
      n_complete = sum(.data[[STANDARD_COLUMNS$progress]] >= 100, na.rm = TRUE),
      n_sufficient = sum(.data[[STANDARD_COLUMNS$progress]] >= config$min_progress, na.rm = TRUE)
    )
  
  cli::cli_inform("Progress statistics:")
  cli::cli_inform(paste("  Mean:", round(progress_stats$mean_progress, 1), "%"))
  cli::cli_inform(paste("  Median:", round(progress_stats$median_progress, 1), "%"))
  cli::cli_inform(paste("  Complete (100%):", progress_stats$n_complete))
  cli::cli_inform(paste("  Sufficient (>=", config$min_progress, "%):", progress_stats$n_sufficient))
}

cli::cli_h1("✓ ANONYMIZATION COMPLETE")