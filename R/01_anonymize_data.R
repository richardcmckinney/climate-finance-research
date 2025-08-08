# 01_anonymize_data.R
# Purpose: Create anonymized survey data, prepare Appendix J classification template,
#          and (if present) merge manual classifications.
# Author : Richard McKinney
# Date   : 2025-08-08

## ---- Libraries --------------------------------------------------------------
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages({
  library(tidyverse)   # used only for readr/write and a few helpers
  library(digest)
})

## ---- Folders ----------------------------------------------------------------
dir.create("data",   recursive = TRUE, showWarnings = FALSE)
dir.create("docs",   recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

## ---- Locate raw CSV ----------------------------------------------------------
raw_candidates <- list.files(
  "data_raw",
  pattern = "Accelerating climate finance.*\\.csv$",
  full.names = TRUE
)
if (length(raw_candidates) == 0) {
  stop("No raw survey CSV found in data_raw/. Place the Qualtrics export there.")
}
raw_infile <- raw_candidates[order(file.info(raw_candidates)$mtime, decreasing = TRUE)][1]
message("Raw input: ", raw_infile)

## ---- Read raw as character ---------------------------------------------------
raw_data <- readr::read_csv(raw_infile, col_types = readr::cols(.default = "c"))

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")
cat("Loaded ", nrow(raw_data), " rows and ", ncol(raw_data), " columns\n", sep = "")
cat("Anonymizing data (removing PII)...\n")

## ---- Helpers ----------------------------------------------------------------
create_hash_id <- function(x, prefix = "ID") {
  vapply(x, function(i) {
    if (is.na(i) || identical(i, "")) return(NA_character_)
    paste0(prefix, "_", substr(digest(as.character(i), algo = "sha256"), 1, 8))
  }, character(1))
}
safe_month <- function(x) {
  if (is.null(x)) return(rep(NA_character_, length.out = nrow(raw_data)))
  ts <- suppressWarnings(as.POSIXct(
    x, tz = "UTC",
    tryFormats = c("%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S",
                   "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
  ))
  ifelse(is.na(ts), NA_character_, format(ts, "%Y-%m"))
}
has_col <- function(df, nm) nm %in% names(df)

## ---- PII to drop -------------------------------------------------------------
pii_columns <- c(
  "IPAddress","RecipientLastName","RecipientFirstName","RecipientEmail",
  "ExternalReference","LocationLatitude","LocationLongitude",
  "UserLanguage",
  "FULL_NAME","GENDER","ORGANIZATION_NAME","hq_address_line_1","hq_address_line_2",
  "hq_email","hq_phone","primary_contact_email","primary_contact_phone","website",
  "lp_name","PRIMARY_POSITION","description",
  "ORGANIZATION_ID","FIRM_ORGANIZATION_ID","FIRM_ORGANIZATION_ID_NUMBER","pbid"
)

## ---- Build anonymized data step-by-step (no conditional mutate) --------------
df <- raw_data

## IDs
if (has_col(df, "ResponseId")) {
  df$respondent_id <- create_hash_id(df$ResponseId, "RESP")
} else {
  df$respondent_id <- sprintf("RESP_%04d", seq_len(nrow(df)))
}
df$organization_id <- if (has_col(df, "ORGANIZATION_ID")) create_hash_id(df$ORGANIZATION_ID, "ORG") else NA_character_
df$firm_id         <- if (has_col(df, "FIRM_ORGANIZATION_ID")) create_hash_id(df$FIRM_ORGANIZATION_ID, "FIRM") else NA_character_

## Date generalization (write *_month alongside; then drop original)
if (has_col(raw_data, "StartDate"))   df$StartDate_month   <- safe_month(raw_data$StartDate)
if (has_col(raw_data, "EndDate"))     df$EndDate_month     <- safe_month(raw_data$EndDate)
if (has_col(raw_data, "RecordedDate"))df$RecordedDate_month<- safe_month(raw_data$RecordedDate)

drop_dates <- intersect(c("StartDate","EndDate","RecordedDate"), names(df))
if (length(drop_dates) > 0) df <- df[, setdiff(names(df), drop_dates), drop = FALSE]

## HQ geography generalization (only if present)
if (has_col(df, "hq_country")) {
  hc <- df$hq_country
  region <- rep(NA_character_, length(hc))
  region[hc %in% c("United States","USA","US","Canada","CA")] <- "North America"
  region[hc %in% c("United Kingdom","UK","England","Scotland","Wales","Ireland","Germany","DE",
                   "France","FR","Netherlands","Switzerland","Sweden","Denmark","Norway",
                   "Spain","Italy","Belgium","Austria","Finland","Portugal","Greece")] <- "Europe"
  region[hc %in% c("China","CN","Japan","JP","Singapore","SG","India","IN","South Korea","KR",
                   "Hong Kong","Taiwan","Indonesia","Malaysia","Philippines","Thailand","Vietnam")] <- "Asia"
  region[hc %in% c("Australia","New Zealand")] <- "Oceania"
  region[hc %in% c("Brazil","Argentina","Chile","Peru","Colombia","Mexico")] <- "Latin America"
  region[hc %in% c("South Africa","Nigeria","Kenya","Egypt","Morocco")] <- "Africa"
  region[!is.na(hc) & is.na(region)] <- "Other"

  df$hq_country <- region
  # remove finer-grained HQ fields if present
  for (nm in c("hq_state_province","hq_city","hq_zip_code")) {
    if (has_col(df, nm)) df[[nm]] <- NULL
  }
}

## Drop PII (after we’ve created hashed IDs & months)
drop_cols <- intersect(pii_columns, names(df))
if (length(drop_cols) > 0) df <- df[, setdiff(names(df), drop_cols), drop = FALSE]

## ---- Persist anonymized dataset ---------------------------------------------
out_anon <- "data/climate_finance_survey_anonymized.csv"
readr::write_csv(df, out_anon)
cat("Saved anonymized data → ", out_anon, "\n", sep = "")

## =============================================================================
## PART 2: Prepare Appendix J classification template
## =============================================================================
cat("\n=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

# pull respondent_id + role fields; keep names as used downstream
role_df <- tibble(
  respondent_id   = df$respondent_id,
  `Q2.1`          = if (has_col(df, "Q2.1")) df[["Q2.1"]] else NA_character_,
  `Q2.1_12_TEXT`  = if (has_col(df, "Q2.1_12_TEXT")) df[["Q2.1_12_TEXT"]] else NA_character_
)

# simple preliminary category rules (for template/placeholder only)
to_lc <- function(x) ifelse(is.na(x) | x == "", NA_character_, tolower(x))
prelim_category <- function(role, other_text) {
  rl <- to_lc(role); tx <- to_lc(other_text)
  out <- rep("Miscellaneous and Individual Respondents", length(rl))
  is_ent <- (str_detect(coalesce(rl,""), "entrepreneur|founder") |
             str_detect(coalesce(tx,""), "entrepreneur|founder|co[- ]?founder")) &
            (str_detect(coalesce(rl,""), "climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio") |
             str_detect(coalesce(tx,""), "climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio"))
  out[is_ent] <- "Entrepreneur in Climate Technology"
  out[str_detect(coalesce(rl,""), "\\bventure capital\\b") & !str_detect(coalesce(rl,""), "corporate")] <- "Venture Capital Firm"
  out[str_detect(coalesce(rl,""), "private equity")] <- "Private Equity Firm"
  out[str_detect(coalesce(rl,""), "corporate venture")] <- "Corporate Venture Arm"
  out[str_detect(coalesce(rl,""), "angel investor")] <- "Angel Investor"
  out[str_detect(coalesce(rl,""), "esg investor")] <- "ESG Investor"
  out[str_detect(coalesce(rl,""), "family office")] <- "Family Office"
  out[str_detect(coalesce(rl,""), "limited partner|\\blp\\b")] <- "Limited Partner"
  out[str_detect(coalesce(rl,""), "non[- ]?profit|ngo")] <- "Nonprofit Organization"
  out[str_detect(coalesce(rl,""), "academic|research institution|university")] <- "Academic or Research Institution"
  out[str_detect(coalesce(rl,""), "government")] <- "Government Funding Agency"
  out[str_detect(coalesce(rl,""), "philanthrop")] <- "Philanthropic Organization"
  out
}

classification_template <- role_df %>%
  mutate(
    preliminary_category     = prelim_category(`Q2.1`, `Q2.1_12_TEXT`),
    final_category_appendix_j = NA_character_,
    needs_harmonization      = `Q2.1` == "Other (please specify)" | is.na(preliminary_category)
  )

out_template <- "docs/appendix_j_classification_template.csv"
readr::write_csv(classification_template, out_template)
cat("Classification template → ", out_template, "\n", sep = "")
cat("Complete manually if desired and save as docs/appendix_j_classifications_complete.csv\n")

## =============================================================================
## PART 3: Join manual classifications (if present) or write placeholder
## =============================================================================
cat("\n=== PART 3: CLASSIFIED DATA OUTPUT ===\n")
manual_path <- "docs/appendix_j_classifications_complete.csv"

if (file.exists(manual_path)) {
  cat("Manual classifications found, joining…\n")
  manual <- readr::read_csv(manual_path, col_types = readr::cols(.default = "c"))
  needed <- c("respondent_id", "final_category_appendix_j")
  if (!all(needed %in% names(manual))) {
    stop("Manual file must include columns: respondent_id, final_category_appendix_j")
  }
  anon_classified <- df %>%
    left_join(manual %>% select(all_of(needed)), by = "respondent_id") %>%
    mutate(Final_Role_Category = coalesce(final_category_appendix_j, preliminary_category))

  out_classified <- "data/climate_finance_survey_anonymized_classified.csv"
  readr::write_csv(anon_classified, out_classified)
  cat("Classified anonymized data → ", out_classified, "\n", sep = "")
} else {
  cat("Manual classifications not found. Writing preliminary classified placeholder…\n")
  anon_classified_prelim <- df %>%
    left_join(classification_template %>% select(respondent_id, preliminary_category),
              by = "respondent_id") %>%
    rename(Final_Role_Category = preliminary_category)

  out_prelim <- "data/climate_finance_survey_anonymized_preliminary.csv"
  readr::write_csv(anon_classified_prelim, out_prelim)
  cat("Preliminary classified data → ", out_prelim, "\n", sep = "")
}

## =============================================================================
## PART 4: Data dictionary
## =============================================================================
cat("\n=== PART 4: DATA DICTIONARY ===\n")
anon_for_dict <- readr::read_csv(out_anon, col_types = readr::cols(.default = "c"))
dict <- tibble(
  variable_name    = names(anon_for_dict),
  type             = vapply(anon_for_dict, function(x) class(x)[1], character(1)),
  n_missing        = vapply(anon_for_dict, function(x) sum(is.na(x)), integer(1)),
  n_non_missing    = nrow(anon_for_dict) - n_missing,
  completeness_pct = round(n_non_missing / nrow(anon_for_dict) * 100, 1)
)
out_dict <- "data/data_dictionary.csv"
readr::write_csv(dict, out_dict)
cat("Data dictionary → ", out_dict, "\n", sep = "")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next:\n")
cat("  • Run R/02_classify_stakeholders.R (if using automated text rules)\n")
cat("  • Or complete docs/appendix_j_classifications_complete.csv and re-run this script\n")
cat("  • Then run R/get_exact_1307.R to build the exact Appendix J dataset (N=1,307)\n")