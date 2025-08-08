# 01_anonymize_data.R
# Enhanced, robust anonymization + Appendix J prep (optimized)
# Works with Qualtrics headers like "Q2.1", "QID174", or long question text

suppresPackageStartupMessages({
  if (!"methods" %in% loadedNamespaces()) library(methods)
  library(tidyverse)
  library(lubridate)
  library(digest)
})

`%||%` <- function(a, b) if (is.null(a)) b else a

# PII scrubbing of free-text (emails, phones, URLs, @handles)
scrub_text <- function(x) {
  if (!is.character(x)) return(x)
  # emails
  x <- gsub("(?i)[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "[REDACTED_EMAIL]", x, perl = TRUE)
  # phone numbers (international and common formats)
  x <- gsub("(?i)\n?\\+?\\d[\\\nd\\s().-]{7,}\n?", "[REDACTED_PHONE]", x, perl = TRUE)
  # URLs
  # URLs (avoid \\\\S; use POSIX class for portability)
  x <- gsub("(?i)(https?://[^[:space:]]+|www\\.[A-Za-z0-9.-]+\\.[A-Za-z]{2,}[^[:space:]]*)", "[REDACTED_URL]", x, perl = TRUE)
  # social handles like @username
  x <- gsub("(^|\\s)@[A-Za-z0-9_]{2,15}\\b", "\\1[REDACTED_HANDLE]", x, perl = TRUE)
  x
}

# ------------------------- Configuration -------------------------

config <- list(
  pii_patterns = c(
    "ip[_ ]?address|^IPAddress$",
    "email|e[-_ ]?mail|RecipientEmail",
    "first[ _]?name|last[ _]?name|full[_ ]?name",
    "recipient|Recipient|ExternalDataReference|external.*reference",
    "latitude|longitude|LocationLatitude|LocationLongitude",
    "\\bhq_?address|address[ _]?line|street|city|state|zip|postal",
    "phone|mobile|cell|tel|fax",
    "website|url",
    "^ResponseId$|^StartDate$|^EndDate$|^RecordedDate$"
  ),
  output_paths = list(
    basic       = "data/survey_responses_anonymized_basic.csv",
    template    = "docs/appendix_j_classification_template.csv",
    classified  = "data/survey_responses_anonymized_classified.csv",
    preliminary = "data/survey_responses_anonymized_preliminary.csv",
    dictionary  = "data/data_dictionary.csv"
  ),
  hash_length = 10
)

# Region lookup (country/abbrev -> region)
.region_lookup <- list(
  "North America"    = c("United States", "USA", "US", "Canada", "CA"),
  "Europe"           = c("United Kingdom","England","Scotland","Wales","Ireland",
                         "Germany","DE","France","FR","Netherlands","Switzerland",
                         "Sweden","Denmark","Norway","Spain","Italy","Belgium",
                         "Austria","Finland","Greece","Portugal","Poland"),
  "Asia"             = c("China","CN","Japan","JP","Singapore","SG","India","IN",
                         "South Korea","KR","Hong Kong","Taiwan","Indonesia",
                         "Malaysia","Philippines","Thailand","Vietnam"),
  "Latin America"    = c("Brazil","Argentina","Chile","Mexico","Peru","Colombia"),
  "Africa"           = c("South Africa","Nigeria","Kenya","Egypt","Morocco"),
  "Australia/Oceania"= c("Australia","New Zealand")
)
country_to_region <- unlist(lapply(names(.region_lookup), function(r) {
  setNames(rep(r, length(.region_lookup[[r]])), .region_lookup[[r]])
}), use.names = TRUE)

# Ensure standard folders exist
dir.create("data",   recursive = TRUE, showWarnings = FALSE)
dir.create("docs",   recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

# ------------------------- Helpers -------------------------

# has a column?
has_col <- function(df, nm) isTRUE(nm %in% names(df))

# find a column by candidate names and/or regex
find_col <- function(df, candidates = character(0), pattern = NULL) {
  nms <- names(df)
  hit <- candidates[candidates %in% nms]
  if (length(hit)) return(hit[1])
  if (!is.null(pattern)) {
    rx <- grep(pattern, nms, ignore.case = TRUE, value = TRUE)
    if (length(rx)) return(rx[1])
  }
  NA_character_
}

# basic validation
validate_data <- function(df) {
  if (!is.data.frame(df) || nrow(df) < 1L) {
    stop("Input data has no rows or is not a data.frame.")
  }
  invisible(TRUE)
}

# staged, fast month extractor
safe_month <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x)

  # Try fast/common format first
  parsed <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))

  # Fallbacks only where needed
  if (anyNA(parsed)) {
    na_idx <- which(is.na(parsed))
    parsed[na_idx] <- suppressWarnings(parse_date_time(
      x[na_idx],
      orders = c("Y-m-d H:M", "m/d/Y H:M:S", "m/d/Y H:M",
                 "d/m/Y H:M:S", "d/m/Y H:M", "Y-m-d", "m/d/Y", "d/m/Y"),
      tz = "UTC", quiet = TRUE
    ))
  }

  out <- format(parsed, "%Y-%m")
  out[is.na(parsed)] <- NA_character_
  out
}

# deterministic hash IDs
hash_id <- function(x, pref) {
  if (is.null(x)) return(rep(NA_character_, length(x)))
  vapply(as.character(x), function(i) {
    if (is.na(i) || i == "") return(NA_character_)
    paste0(pref, "_", substr(digest(i, algo = "sha256"), 1, config$hash_length))
  }, character(1))
}

# optimized single-pass role mapper
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
    "Nonprofit Organization"             = "non[- ]?profit|ngo|civil\\s*society|association|institute"
  )

  z <- tolower(paste(coalesce(role, ""), coalesce(other_text, "")))
  vapply(z, function(txt) {
    for (cat in names(patterns)) {
      if (grepl(patterns[[cat]], txt, perl = TRUE)) return(cat)
    }
    "Other"
  }, character(1), USE.NAMES = FALSE)
}

# ------------------------- Load data -------------------------

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
raw_files <- list.files("data_raw", pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(raw_files) >= 1)
raw_in <- raw_files[1]
cat("Raw input:", raw_in, "\n")

raw <- readr::read_csv(
  file       = raw_in,
  col_types  = readr::cols(.default = readr::col_guess()),
  guess_max  = 10000,
  progress   = FALSE
)
validate_data(raw)
cat("Loaded", nrow(raw), "rows and", ncol(raw), "columns\n")

# ------------------------- Column detection -------------------------

# IDs
col_response_id <- find_col(
  raw, candidates = c("ResponseId", "_recordId", "Response ID"),
  pattern = "^response[_ ]?id$|^_recordid$"
)

# role
col_role <- find_col(
  raw, candidates = c("Q2.1", "QID174"),
  pattern = "best describes your role.*selected choice|\\bQ2\\.1\\b|QID174"
)
col_role_other <- find_col(
  raw, candidates = c("Q2.1_12_TEXT", "QID174_12_TEXT"),
  pattern = "best describes your role.*other.*text|Q2\\.1.*TEXT|QID174_.*TEXT"
)

# geography (HQ)
col_hq <- find_col(
  raw, candidates = c("hq_country", "Q2.2", "QID175"),
  pattern = "where is your organization headquartered.*selected choice|\\bQ2\\.2\\b|QID175"
)
col_hq_other <- find_col(
  raw, candidates = c("Q2.2_10_TEXT", "QID175_10_TEXT"),
  pattern = "headquartered.*other.*text|Q2\\.2.*TEXT|QID175_.*TEXT"
)

# dates
col_start  <- find_col(raw, candidates = c("StartDate","startDate","Start Date"),
                       pattern = "^start[ _]?date$")
col_end    <- find_col(raw, candidates = c("EndDate","endDate","End Date"),
                       pattern = "^end[ _]?date$")
col_record <- find_col(raw, candidates = c("RecordedDate","recordedDate","Recorded Date"),
                       pattern = "^recorded[ _]?date$")

# ------------------------- Build anonymized table -------------------------

resp_id <- if (!is.na(col_response_id)) raw[[col_response_id]] else seq_len(nrow(raw))
pii_regex <- paste(config$pii_patterns, collapse = "|")

an_basic <- raw %>%
  mutate(
    respondent_id = hash_id(resp_id, "RESP"),

    # add Month columns for any present date columns
    across(
      .cols  = any_of(c(col_start, col_end, col_record)),
      .fns   = ~ safe_month(.x),
      .names = "{.col}_month"
    ),

    # region mapping if HQ column exists
    hq_region = if (!is.na(col_hq)) unname(country_to_region[.[[col_hq]]]) else NA_character_
  ) %>%
  # drop PII and the original date columns (if present)
  select(
    -matches(pii_regex, ignore.case = TRUE),
    -any_of(c(col_start, col_end, col_record))
  ) %>%
  relocate(respondent_id, .before = 1) %>%
  mutate(across(where(is.character), scrub_text))

# Save basic anonymized
readr::write_csv(an_basic, config$output_paths$basic)
cat("Anonymizing data (removing PII)...\n")
cat("Saved basic anonymized data ->", normalizePath(config$output_paths$basic), "\n\n")

# ------------------------- PART 2: Appendix J template -------------------------

cat("=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

have_role <- !is.na(col_role)
if (!have_role) {
  warning("Role column not found; classification template will be empty.")
} else {
  role_df <- tibble(
    respondent_id = an_basic$respondent_id,
    role_raw      = raw[[col_role]],
    role_other    = if (!is.na(col_role_other)) raw[[col_role_other]] else NA_character_
  )

  template <- role_df %>%
    mutate(
      needs_harmonization       = tolower(coalesce(role_raw, "")) %in% c("other", "other (please specify)"),
      preliminary_category      = map_prelim_role(role_raw, role_other),
      final_category_appendix_j = NA_character_
    )

  readr::write_csv(template, config$output_paths$template)
  cat("Classification template ->", normalizePath(config$output_paths$template), "\n")

  # merge completed classifications if present
  completed_file <- file.path("docs", "appendix_j_classifications_complete.csv")
  if (file.exists(completed_file)) {
    cat("Found completed classifications. Merging...\n")
    final_map <- readr::read_csv(completed_file, show_col_types = FALSE) %>%
      select(respondent_id, final_category_appendix_j)

    out_class <- an_basic %>%
      left_join(template %>% select(respondent_id, preliminary_category), by = "respondent_id") %>%
      left_join(final_map, by = "respondent_id") %>%
      mutate(stakeholder_category = coalesce(final_category_appendix_j, preliminary_category)) %>%
      select(-preliminary_category, -final_category_appendix_j)

    readr::write_csv(out_class, config$output_paths$classified)
    cat("Classified data ->", normalizePath(config$output_paths$classified), "\n")
  } else {
    out_prelim <- an_basic %>%
      left_join(template %>% select(respondent_id, preliminary_category), by = "respondent_id") %>%
      rename(stakeholder_category = preliminary_category)

    readr::write_csv(out_prelim, config$output_paths$preliminary)
    cat("Preliminary classified data ->", normalizePath(config$output_paths$preliminary), "\n")
  }
}

# ------------------------- Summary & dictionary -------------------------

cat("\n=== ANONYMIZATION SUMMARY ===\n")
cat("Original columns:", ncol(raw), "\n")
cat("Anonymized columns:", ncol(an_basic), "\n")
cat("Rows preserved:", nrow(an_basic), "\n")

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
cat("Data dictionary ->", normalizePath(config$output_paths$dictionary), "\n")
cat("\n=== PROCESS COMPLETE ===\n")