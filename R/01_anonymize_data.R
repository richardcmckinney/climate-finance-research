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
    preliminary = "data/survey_responses_anonymized_preliminary.csv",
    dictionary  = "data/data_dictionary.csv"
  ),
  hash_length = 10
)

# ------------------------- Helpers -------------------------
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

safe_month <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x <- as.character(x)
  parsed <- suppressWarnings(as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S", tz = "UTC"))
  if (anyNA(parsed)) {
    na_idx <- which(is.na(parsed))
    parsed[na_idx] <- suppressWarnings(lubridate::parse_date_time(
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

hash_id <- function(x, pref) {
  vapply(as.character(x), function(i) {
    if (is.na(i) || i == "") return(NA_character_)
    paste0(pref, "_", substr(digest(i, algo = "sha256"), 1, config$hash_length))
  }, character(1))
}

scrub_text <- function(x) {
  if (!is.character(x)) return(x)
  # emails
  x <- gsub("(?i)[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\\.[A-Za-z]{2,}", "[REDACTED_EMAIL]", x, perl = TRUE)
  # phone numbers (international and common formats)
  x <- gsub("(?i)\\+?\\d[\\d\\s().-]{7,}\\d", "[REDACTED_PHONE]", x, perl = TRUE)
  # URLs (use POSIX class to avoid \\S portability issues)
  x <- gsub("(?i)(https?://[^[:space:]]+|www\\.[A-Za-z0-9.-]+\\.[A-Za-z]{2,}[^[:space:]]*)", "[REDACTED_URL]", x, perl = TRUE)
  # social handles like @username
  x <- gsub("(^|\\s)@[A-Za-z0-9_]{2,15}\\b", "\\1[REDACTED_HANDLE]", x, perl = TRUE)
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
    for (cat in names(patterns)) if (grepl(patterns[[cat]], txt, perl = TRUE)) return(cat)
    "Other"
  }, character(1), USE.NAMES = FALSE)
}

# ------------------------- Load data -------------------------
cli::cli_h1("PART 1: BASIC ANONYMIZATION")
raw_files <- list.files("data_raw", pattern = "\\.csv$", full.names = TRUE)
stopifnot(length(raw_files) >= 1)
raw_in <- raw_files[1]
cli::cli_inform(paste("Raw input:", raw_in))

raw <- readr::read_csv(raw_in, guess_max = 10000, show_col_types = FALSE)
cli::cli_inform(paste("Loaded", nrow(raw), "rows and", ncol(raw), "columns"))

# Column detection
col_response_id <- find_col(raw, c("ResponseId", "_recordId", "Response ID"), pattern = "^response[_ ]?id$|^_recordid$")
col_start  <- find_col(raw, c("StartDate","startDate","Start Date"), pattern = "^start[ _]?date$")
col_end    <- find_col(raw, c("EndDate","endDate","End Date"), pattern = "^end[ _]?date$")
col_record <- find_col(raw, c("RecordedDate","recordedDate","Recorded Date"), pattern = "^recorded[ _]?date$")

# role fields (for template)
col_role        <- find_col(raw, c("Q2.1", "QID174"), pattern = "best describes your role|\\bQ2\\.1\\b|QID174")
col_role_other  <- find_col(raw, c("Q2.1_12_TEXT", "QID174_12_TEXT"), pattern = "other.*text|Q2\\.1.*TEXT|QID174_.*TEXT")

# Build anonymized basic
resp_id <- if (!is.na(col_response_id)) raw[[col_response_id]] else seq_len(nrow(raw))
pii_regex <- paste(config$pii_patterns, collapse = "|")

an_basic <- raw %>%
  mutate(
    respondent_id = hash_id(resp_id, "RESP"),
    across(.cols = any_of(c(col_start, col_end, col_record)), .fns = ~ safe_month(.x), .names = "{.col}_month")
  ) %>%
  select(-matches(pii_regex, ignore.case = TRUE), -any_of(c(col_start, col_end, col_record))) %>%
  relocate(respondent_id, .before = 1)

# Scrub PII in character columns EXCEPT respondent_id (to preserve deterministic keys)
char_cols <- names(an_basic)[vapply(an_basic, is.character, logical(1))]
char_cols <- setdiff(char_cols, "respondent_id")
if (length(char_cols)) {
  an_basic <- an_basic %>% mutate(across(all_of(char_cols), scrub_text))
}

# Save basic
cli::cli_inform("Anonymizing data (removing PII & scrubbing free text)...")
readr::write_csv(an_basic, config$output_paths$basic)
cli::cli_inform(paste("Saved basic anonymized data ->", normalizePath(config$output_paths$basic)))

# ------------------------- Appendix J preparation -------------------------
cli::cli_h1("PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS")
if (!is.na(col_role)) {
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
    ) %>%
    arrange(respondent_id) %>%
    distinct(respondent_id, .keep_all = TRUE)

  readr::write_csv(template, config$output_paths$template)
  cli::cli_inform(paste("Classification template ->", normalizePath(config$output_paths$template)))

  out_prelim <- an_basic %>%
    left_join(template %>% select(respondent_id, preliminary_category), by = "respondent_id") %>%
    rename(stakeholder_category = preliminary_category) %>%
    arrange(respondent_id) %>%
    distinct(respondent_id, .keep_all = TRUE)

  readr::write_csv(out_prelim, config$output_paths$preliminary)
  cli::cli_inform(paste("Preliminary classified data ->", normalizePath(config$output_paths$preliminary)))
} else {
  cli::cli_warn("Role column not found; skipping template and preliminary outputs.")
}

# ------------------------- Summary & dictionary -------------------------
cli::cli_h1("ANONYMIZATION SUMMARY")
cli::cli_inform(paste("Original columns:", ncol(raw)))
cli::cli_inform(paste("Anonymized columns:", ncol(an_basic)))
cli::cli_inform(paste("Rows preserved:", nrow(an_basic)))

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
cli::cli_inform(paste("Data dictionary ->", normalizePath(config$output_paths$dictionary)))
cli::cli_h1("PROCESS COMPLETE")