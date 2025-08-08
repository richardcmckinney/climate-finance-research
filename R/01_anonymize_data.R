# 01_anonymize_data.R
# Purpose: Create anonymized survey data, prepare Appendix J classification template,
#          and (if available) join manual classifications.
# Author : Richard McKinney
# Date   : 2025-08-08

# ---- Libraries ---------------------------------------------------------------
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
})

# ---- Folders -----------------------------------------------------------------
dir.create("data",   recursive = TRUE, showWarnings = FALSE)
dir.create("docs",   recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

# ---- Locate raw CSV ----------------------------------------------------------
# Expect the Qualtrics export in data_raw/, with the long "Accelerating climate finance" name.
raw_candidates <- list.files(
  "data_raw",
  pattern = "Accelerating climate finance.*\\.csv$",
  full.names = TRUE
)

if (length(raw_candidates) == 0) {
  stop("No raw survey CSV found in data_raw/. Place the Qualtrics export there.")
}
# If multiple, take the most recently modified
raw_infile <- raw_candidates[order(file.info(raw_candidates)$mtime, decreasing = TRUE)][1]
message("Raw input: ", raw_infile)

# ---- Read raw as character to avoid type surprises ---------------------------
raw_data <- readr::read_csv(raw_infile, col_types = readr::cols(.default = "c"))

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")
cat("Loaded ", nrow(raw_data), " rows and ", ncol(raw_data), " columns\n", sep = "")

# ---- Helpers -----------------------------------------------------------------
create_hash_id <- function(x, prefix = "ID") {
  vapply(x, function(i) {
    if (is.na(i) || identical(i, "")) return(NA_character_)
    paste0(prefix, "_", substr(digest(as.character(i), algo = "sha256"), 1, 8))
  }, character(1))
}

# robust year-month formatter
to_year_month <- function(x) {
  if (is.null(x)) return(NA_character_)
  ts <- suppressWarnings(as.POSIXct(
    x, tz = "UTC",
    tryFormats = c("%m/%d/%Y %H:%M", "%m/%d/%Y %H:%M:%S",
                   "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
  ))
  out <- ifelse(is.na(ts), NA_character_, format(ts, "%Y-%m"))
  out
}

# convenient column-existence predicate
has_col <- function(df, nm) nm %in% names(df)

# ---- PII columns to drop -----------------------------------------------------
pii_columns <- c(
  # common Qualtrics/system identifiers
  "IPAddress","RecipientLastName","RecipientFirstName","RecipientEmail",
  "ExternalReference","LocationLatitude","LocationLongitude","StartDate",
  "EndDate","RecordedDate","UserLanguage",
  # project-specific fields you flagged
  "FULL_NAME","GENDER","ORGANIZATION_NAME","hq_address_line_1","hq_address_line_2",
  "hq_email","hq_phone","primary_contact_email","primary_contact_phone","website",
  "lp_name","PRIMARY_POSITION","description",
  # known org IDs that could be re-identifying (we’ll keep hashed versions below)
  "ORGANIZATION_ID","FIRM_ORGANIZATION_ID","FIRM_ORGANIZATION_ID_NUMBER","pbid"
)

# ---- Build anonymized columns safely (vectorized) ----------------------------
# respondent_id
if (has_col(raw_data, "ResponseId")) {
  respondent_id <- create_hash_id(raw_data$ResponseId, "RESP")
} else {
  respondent_id <- sprintf("RESP_%04d", seq_len(nrow(raw_data)))
}

# organization_id (vectorized; length = nrow)
organization_id <- if (has_col(raw_data, "ORGANIZATION_ID")) {
  create_hash_id(raw_data$ORGANIZATION_ID, "ORG")
} else {
  rep(NA_character_, nrow(raw_data))
}

# firm_id (vectorized; length = nrow)
firm_id <- if (has_col(raw_data, "FIRM_ORGANIZATION_ID")) {
  create_hash_id(raw_data$FIRM_ORGANIZATION_ID, "FIRM")
} else {
  rep(NA_character_, nrow(raw_data))
}

# Attach IDs, then drop PII
anon_step1 <- raw_data %>%
  mutate(
    respondent_id   = respondent_id,
    organization_id = organization_id,
    firm_id         = firm_id
  ) %>%
  select(-any_of(pii_columns))

# ---- Geography generalization (HQ -> Region) ---------------------------------
# Keep original Q2.2 (respondent region) intact; only generalize raw HQ fields if present
if (has_col(anon_step1, "hq_country")) {
  anon_step1 <- anon_step1 %>%
    mutate(
      hq_country_region = case_when(
        hq_country %in% c("United States","USA","US","Canada","CA") ~ "North America",
        hq_country %in% c("United Kingdom","UK","England","Scotland","Wales","Ireland","Germany","DE",
                          "France","FR","Netherlands","Switzerland","Sweden","Denmark","Norway",
                          "Spain","Italy","Belgium","Austria","Finland","Portugal","Greece") ~ "Europe",
        hq_country %in% c("China","CN","Japan","JP","Singapore","SG","India","IN","South Korea","KR",
                          "Hong Kong","Taiwan","Indonesia","Malaysia","Philippines","Thailand","Vietnam") ~ "Asia",
        hq_country %in% c("Australia","New Zealand") ~ "Oceania",
        hq_country %in% c("Brazil","Argentina","Chile","Peru","Colombia","Mexico") ~ "Latin America",
        hq_country %in% c("South Africa","Nigeria","Kenya","Egypt","Morocco") ~ "Africa",
        !is.na(hq_country) ~ "Other",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-any_of(c("hq_state_province","hq_city","hq_zip_code","hq_country"))) %>%
    rename(hq_country = hq_country_region)
}

# ---- Date generalization (year-month) ----------------------------------------
date_cols <- intersect(c("StartDate","EndDate","RecordedDate"), names(raw_data))
if (length(date_cols) > 0) {
  # Use original raw_data for date strings; add *_month fields alongside anonymized data
  for (dc in date_cols) {
    anon_step1[[paste0(dc, "_month")]] <- to_year_month(raw_data[[dc]])
  }
}

# ---- Persist anonymized dataset ----------------------------------------------
out_anon <- "data/climate_finance_survey_anonymized.csv"
readr::write_csv(anon_step1, out_anon)
cat("Saved anonymized data → ", out_anon, "\n", sep = "")

# ==============================================================================
# PART 2: PREPARE APPENDIX J CLASSIFICATION TEMPLATE
# ==============================================================================

cat("\n=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

# Pull respondent_id + role fields; keep headers exactly as used elsewhere
role_df <- anon_step1 %>%
  transmute(
    respondent_id = respondent_id,
    `Q2.1`        = if (has_col(anon_step1, "Q2.1"))        `Q2.1`        else NA_character_,
    `Q2.1_12_TEXT`= if (has_col(anon_step1, "Q2.1_12_TEXT"))`Q2.1_12_TEXT` else NA_character_
  )

# Preliminary category (simple rules; reviewers can refine manually)
prelim_category <- function(role, other_text) {
  rl <- tolower(coalesce(role, ""))
  tx <- tolower(coalesce(other_text, ""))

  case_when(
    # Entrepreneur first, with climate tech keywords
    (str_detect(rl, "entrepreneur|founder") | str_detect(tx, "entrepreneur|founder|co[- ]?founder")) &
      (str_detect(rl, "climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio") |
         str_detect(tx, "climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio")) ~
      "Entrepreneur in Climate Technology",

    str_detect(rl, "\\bventure capital\\b") & !str_detect(rl, "corporate") ~ "Venture Capital Firm",
    str_detect(rl, "private equity")                                       ~ "Private Equity Firm",
    str_detect(rl, "corporate venture")                                    ~ "Corporate Venture Arm",
    str_detect(rl, "angel investor")                                       ~ "Angel Investor",
    str_detect(rl, "esg investor")                                         ~ "ESG Investor",
    str_detect(rl, "family office")                                        ~ "Family Office",
    str_detect(rl, "limited partner|\\blp\\b")                              ~ "Limited Partner",
    str_detect(rl, "non[- ]?profit|ngo")                                   ~ "Nonprofit Organization",
    str_detect(rl, "academic|research institution|university")             ~ "Academic or Research Institution",
    str_detect(rl, "government")                                           ~ "Government Funding Agency",
    str_detect(rl, "philanthrop")                                          ~ "Philanthropic Organization",
    TRUE ~ "Miscellaneous and Individual Respondents"
  )
}

classification_template <- role_df %>%
  mutate(
    preliminary_category = prelim_category(`Q2.1`, `Q2.1_12_TEXT`),
    final_category_appendix_j = NA_character_,        # to be filled manually if desired
    needs_harmonization = `Q2.1` == "Other (please specify)" | is.na(preliminary_category)
  )

out_template <- "docs/appendix_j_classification_template.csv"
readr::write_csv(classification_template, out_template)
cat("Classification template → ", out_template, "\n", sep = "")
cat("Complete manually if needed and save as: docs/appendix_j_classifications_complete.csv\n")

# ==============================================================================
# PART 3: JOIN MANUAL CLASSIFICATIONS (IF PRESENT) OR WRITE PLACEHOLDER
# ==============================================================================

cat("\n=== PART 3: CLASSIFIED DATA OUTPUT ===\n")
manual_path <- "docs/appendix_j_classifications_complete.csv"

if (file.exists(manual_path)) {
  cat("Manual classifications found, joining…\n")
  manual <- readr::read_csv(manual_path, col_types = readr::cols(.default = "c"))

  # Expect columns: respondent_id, final_category_appendix_j
  needed <- c("respondent_id", "final_category_appendix_j")
  if (!all(needed %in% names(manual))) {
    stop("Manual file must include columns: ", paste(needed, collapse = ", "))
  }

  anon_classified <- anon_step1 %>%
    left_join(manual %>% select(all_of(needed)), by = "respondent_id") %>%
    mutate(
      Final_Role_Category = coalesce(final_category_appendix_j, preliminary_category)
    )

  out_classified <- "data/climate_finance_survey_anonymized_classified.csv"
  readr::write_csv(anon_classified, out_classified)
  cat("Classified anonymized data → ", out_classified, "\n", sep = "")

} else {
  cat("Manual classifications not found. Writing preliminary classified placeholder…\n")
  anon_classified_prelim <- anon_step1 %>%
    left_join(classification_template %>%
                select(respondent_id, preliminary_category),
              by = "respondent_id") %>%
    rename(Final_Role_Category = preliminary_category)

  out_prelim <- "data/climate_finance_survey_anonymized_preliminary.csv"
  readr::write_csv(anon_classified_prelim, out_prelim)
  cat("Preliminary classified data → ", out_prelim, "\n", sep = "")
}

# ==============================================================================
# PART 4: DATA DICTIONARY
# ==============================================================================

cat("\n=== PART 4: DATA DICTIONARY ===\n")
anon_for_dict <- readr::read_csv(out_anon, col_types = readr::cols(.default = "c"))
dict <- tibble(
  variable_name  = names(anon_for_dict),
  type           = vapply(anon_for_dict, function(x) class(x)[1], character(1)),
  n_missing      = vapply(anon_for_dict, function(x) sum(is.na(x)), integer(1)),
  n_non_missing  = nrow(anon_for_dict) - n_missing,
  completeness_pct = round(n_non_missing / nrow(anon_for_dict) * 100, 1)
)

out_dict <- "data/data_dictionary.csv"
readr::write_csv(dict, out_dict)
cat("Data dictionary → ", out_dict, "\n", sep = "")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next:\n")
cat("  • Run R/02_classify_stakeholders.R (if relying on automated text rules).\n")
cat("  • Or complete docs/appendix_j_classifications_complete.csv and re-run this script.\n")
cat("  • Then run R/get_exact_1307.R to build the Appendix J–exact N=1,307 dataset.\n")