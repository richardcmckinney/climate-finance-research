#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# 01_anonymize_data.R — Pure anonymization with data cleaning
# Version 3.0 - Fully integrated with central configuration
# - Uses exact-name matching for known PII columns
# - Pattern-based matching for generic PII (emails, phones, etc.)
# - Derives region from geographic data before removal
# - Preserves respondent_id for stable joins
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

# SOURCE CENTRAL CONFIGURATION - ALL config comes from here
source("R/00_config.R")
check_deprecated()  # Check for old files
set_stage("Anonymization")  # Set pipeline stage

`%||%` <- function(a, b) if (is.null(a)) b else a

# ------------------------- Helper Functions -------------------------
# Local helper for column detection (if not in central config)
find_col_local <- function(df, candidates = character(0), pattern = NULL) {
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

# Use central find_col if available, otherwise use local version
find_col <- if (exists("find_col", mode = "function")) find_col else find_col_local

# Simplified date parsing using lubridate
safe_month <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x)
  
  # Use lubridate's flexible parser with common date formats
  parsed <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c(
      "ymd HMS", "ymd HM", "ymd",      # ISO formats
      "mdy HMS", "mdy HM", "mdy",      # US formats
      "dmy HMS", "dmy HM", "dmy",      # European formats
      "ydm HMS", "ydm HM", "ydm"       # Less common but possible
    ),
    tz = "UTC",
    quiet = TRUE,
    truncated = 3  # Allow truncated formats
  ))
  
  # Format as year-month
  out <- format(parsed, "%Y-%m")
  out[is.na(parsed)] <- NA_character_
  out
}

# Hash function for anonymization
hash_id <- function(x, pref, hash_length = 10) {
  vapply(as.character(x), function(i) {
    if (is.na(i) || i == "") return(NA_character_)
    paste0(pref, "_", substr(digest(i, algo = "sha256"), 1, hash_length))
  }, character(1), USE.NAMES = FALSE)
}

# Scrub PII from text fields
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

# Derive region from country/geographic data
derive_region <- function(country_col) {
  if (is.null(country_col) || all(is.na(country_col))) return(rep(NA_character_, length(country_col)))
  
  # Define region mappings (expandable as needed)
  region_map <- list(
    "North America" = c("United States", "USA", "US", "Canada", "CA", "Mexico", "MX"),
    "Europe" = c("United Kingdom", "UK", "Germany", "DE", "France", "FR", "Italy", "IT", 
                  "Spain", "ES", "Netherlands", "NL", "Belgium", "BE", "Switzerland", "CH",
                  "Sweden", "SE", "Norway", "NO", "Denmark", "DK", "Finland", "FI"),
    "Asia Pacific" = c("China", "CN", "Japan", "JP", "India", "IN", "Australia", "AU", 
                       "Singapore", "SG", "South Korea", "KR", "New Zealand", "NZ",
                       "Hong Kong", "HK", "Taiwan", "TW"),
    "Latin America" = c("Brazil", "BR", "Argentina", "AR", "Chile", "CL", "Colombia", "CO",
                        "Peru", "PE", "Venezuela", "VE"),
    "Middle East & Africa" = c("United Arab Emirates", "UAE", "Saudi Arabia", "SA", 
                                "South Africa", "ZA", "Israel", "IL", "Egypt", "EG",
                                "Nigeria", "NG", "Kenya", "KE")
  )
  
  # Convert country to region
  country_clean <- trimws(as.character(country_col))
  regions <- rep(NA_character_, length(country_clean))
  
  for (region in names(region_map)) {
    countries <- region_map[[region]]
    match_idx <- which(country_clean %in% countries | 
                      tolower(country_clean) %in% tolower(countries))
    regions[match_idx] <- region
  }
  
  # Mark remaining as "Other" if not NA
  regions[!is.na(country_clean) & is.na(regions) & country_clean != ""] <- "Other"
  
  return(regions)
}

# ------------------------- Validate Environment -------------------------
if (!dir.exists("data_raw")) {
  cli::cli_abort("Directory 'data_raw' not found. Please ensure raw data directory exists.")
}

# Create output directories using safe_path from config
safe_path(PATHS$basic_anon)    # Use PATHS directly from central config
safe_path(PATHS$dictionary)

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

# ------------------------- Column Detection -------------------------
# Use standard patterns to find key columns
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

# Find geographic columns for region derivation
col_country <- find_col(raw, c("Q2.2", "hq_country", "HQ_Country", "Country", "country", "headquarter_country"),
                        pattern = "^Q2\\.2$|country|headquarter")

# ------------------------- Initial Transformations -------------------------
# Build response ID
resp_id <- if (!is.na(col_response_id)) {
  raw[[col_response_id]]
} else {
  cli::cli_warn("ResponseId column not found, using sequential IDs")
  seq_len(nrow(raw))
}

# Create anonymized dataset with initial transforms
an_basic <- raw %>%
  mutate(
    !!STANDARD_COLUMNS$respondent_id := hash_id(resp_id, "RESP"),
    across(.cols = any_of(c(col_start, col_end, col_record)), 
           .fns = ~ safe_month(.x), 
           .names = "{.col}_month")
  )

# ------------------------- Derive Region Before Removing Geographic Data -------------------------
if (!is.na(col_country)) {
  cli::cli_inform(paste("Deriving region from geographic column:", col_country))
  an_basic$region <- derive_region(raw[[col_country]])
  
  # Log region distribution
  region_dist <- table(an_basic$region, useNA = "ifany")
  cli::cli_inform("Region distribution:")
  for (r in names(region_dist)) {
    r_label <- if (is.na(r)) "Missing" else r
    cli::cli_inform(paste("  ", r_label, ":", region_dist[r]))
  }
} else {
  cli::cli_warn("No geographic column found for region derivation")
}

# ------------------------- Clean Progress Column -------------------------
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

# ------------------------- PII Removal: Exact + Pattern Matching -------------------------
# STEP 1: Exact-name removal for known PII columns from central config
cols_exact <- intersect(names(an_basic), PRIVACY_COLUMNS)
cli::cli_inform(paste("Found", length(cols_exact), "exact-match PII columns"))

# STEP 2: Pattern-based removal for generic PII patterns
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

pii_regex <- paste(pii_patterns_extra, collapse = "|")
cols_regex <- grep(pii_regex, names(an_basic), ignore.case = TRUE, value = TRUE)

# STEP 3: Combine all columns to remove (including date columns and Q2.2)
# Always remove Q2.2 since we've derived region from it
geographic_cols <- c("Q2.2", col_country) %>% na.omit() %>% as.character()
date_cols <- c(col_start, col_end, col_record) %>% na.omit() %>% as.character()
cols_to_remove <- unique(c(cols_exact, cols_regex, date_cols, geographic_cols))

# Remove from the list of columns to remove
cols_to_remove <- intersect(cols_to_remove, names(an_basic))

if (length(cols_to_remove) > 0) {
  cli::cli_inform(paste("Removing", length(cols_to_remove), "PII columns total:"))
  cli::cli_inform(paste("  - Exact matches:", length(cols_exact)))
  cli::cli_inform(paste("  - Pattern matches:", length(setdiff(cols_regex, cols_exact))))
  cli::cli_inform(paste("  - Date columns:", length(date_cols)))
  cli::cli_inform(paste("  - Geographic columns:", length(geographic_cols)))
  
  an_basic <- an_basic %>% select(-all_of(cols_to_remove))
}

# ------------------------- Final Column Organization -------------------------
# Ensure respondent_id is first, region (if exists) is second
if ("region" %in% names(an_basic)) {
  an_basic <- an_basic %>%
    relocate(!!STANDARD_COLUMNS$respondent_id, region, .before = 1)
} else {
  an_basic <- an_basic %>%
    relocate(!!STANDARD_COLUMNS$respondent_id, .before = 1)
}

# ------------------------- Scrub PII from Text Fields -------------------------
# Scrub PII in character columns EXCEPT respondent_id and region
char_cols <- names(an_basic)[vapply(an_basic, is.character, logical(1))]
char_cols <- setdiff(char_cols, c(STANDARD_COLUMNS$respondent_id, "region"))

if (length(char_cols) > 0) {
  cli::cli_inform(paste("Scrubbing PII from", length(char_cols), "text columns"))
  an_basic <- an_basic %>% 
    mutate(across(all_of(char_cols), scrub_text))
}

# ------------------------- Privacy Validation -------------------------
# Use central privacy check function with strict enforcement
check_privacy_violations(an_basic, stop_on_violation = TRUE)

# ------------------------- Save Output Files -------------------------
cli::cli_inform("Saving basic anonymized data...")
readr::write_csv(an_basic, PATHS$basic_anon)
cli::cli_inform(paste("✓ Saved basic anonymized data ->", normalizePath(PATHS$basic_anon)))

# ------------------------- Create Data Dictionary -------------------------
dict <- tibble(
  column_name      = names(an_basic),
  description      = case_when(
    column_name == STANDARD_COLUMNS$respondent_id ~ "Anonymized respondent identifier",
    column_name == "region" ~ "Geographic region derived from country data",
    column_name == STANDARD_COLUMNS$progress ~ "Survey completion percentage (0-100)",
    TRUE ~ ""
  ),
  type             = vapply(an_basic, function(x) class(x)[1], character(1)),
  n_missing        = vapply(an_basic, function(x) sum(is.na(x)), integer(1))
) %>%
  mutate(
    n_non_missing    = nrow(an_basic) - n_missing,
    completeness_pct = round(n_non_missing / nrow(an_basic) * 100, 1)
  )

readr::write_csv(dict, PATHS$dictionary)
cli::cli_inform(paste("✓ Data dictionary ->", normalizePath(PATHS$dictionary)))

# ------------------------- Summary & Validation -------------------------
cli::cli_h1("ANONYMIZATION SUMMARY")
cli::cli_inform(paste("Original columns:", ncol(raw)))
cli::cli_inform(paste("Anonymized columns:", ncol(an_basic)))
cli::cli_inform(paste("Rows preserved:", nrow(an_basic)))
cli::cli_inform(paste("PII columns removed:", length(cols_to_remove)))
if ("region" %in% names(an_basic)) {
  cli::cli_inform("✓ Region derived from geographic data")
}

# Final validation checks
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
      n_sufficient = sum(.data[[STANDARD_COLUMNS$progress]] >= QUALITY_PARAMS$min_progress, na.rm = TRUE)
    )
  
  cli::cli_inform("Progress statistics:")
  cli::cli_inform(paste("  Mean:", round(progress_stats$mean_progress, 1), "%"))
  cli::cli_inform(paste("  Median:", round(progress_stats$median_progress, 1), "%"))
  cli::cli_inform(paste("  Complete (100%):", progress_stats$n_complete))
  cli::cli_inform(paste("  Sufficient (>=", QUALITY_PARAMS$min_progress, "%):", 
                        progress_stats$n_sufficient))
}

cli::cli_h1("✓ ANONYMIZATION COMPLETE")
cli::cli_inform("Next step: Run 02_process_survey.R for survey processing")