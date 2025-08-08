# 01_anonymize_data.R
# Enhanced Data Anonymization Script for Climate Finance Survey
# Author: Richard McKinney
# Purpose: Create anonymized, publication-safe datasets (with and without
#          preliminary classifications) and a data dictionary.

# ---- Libraries ---------------------------------------------------------------
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages({
  library(tidyverse)
  library(digest)
  library(here)
})

# ---- Output directories & canonical paths ------------------------------------
dir.create(here("data"),   recursive = TRUE, showWarnings = FALSE)
dir.create(here("docs"),   recursive = TRUE, showWarnings = FALSE)
dir.create(here("output"), recursive = TRUE, showWarnings = FALSE)

OUT_ANON_BASIC    <- here("data", "climate_finance_survey_anonymized.csv")
OUT_ANON_PRELIM   <- here("data", "climate_finance_survey_anonymized_preliminary.csv")
OUT_ANON_CLASS    <- here("data", "climate_finance_survey_anonymized_classified.csv")
OUT_DICTIONARY    <- here("data", "data_dictionary.csv")
OUT_TEMPLATE      <- here("docs", "appendix_j_classification_template.csv")
IN_CLASS_COMPLETE <- here("docs", "appendix_j_classifications_complete.csv")

# ---- Input raw file ----------------------------------------------------------
RAW_FILE <- here(
  "data_raw",
  "(Strategic Research) Accelerating climate finance_ An analysis of barriers and solutions_August 21, 2024_18.54.csv"
)
if (!file.exists(RAW_FILE)) {
  stop("Raw survey file not found at: ", RAW_FILE,
       "\nPlace the raw CSV under data_raw/ and re-run.")
}

# ---- Helpers -----------------------------------------------------------------
create_hash_id <- function(x, prefix = "ID") {
  vapply(
    x,
    function(i) {
      if (is.na(i) || i == "") return(NA_character_)
      paste0(prefix, "_", substr(digest(as.character(i), algo = "sha256"), 1, 8))
    },
    FUN.VALUE = character(1)
  )
}

safe_month <- function(x) {
  fmts <- c("%m/%d/%Y %H:%M", "%m/%d/%Y", "%Y-%m-%d %H:%M:%S", "%Y-%m-%d")
  out <- rep(NA_character_, length(x))
  for (f in fmts) {
    suppressWarnings({
      d <- as.Date(x, format = f, tz = "UTC")
      out[is.na(out) & !is.na(d)] <- format(d[!is.na(d)], "%Y-%m")
    })
  }
  out
}

# ---- PART 1: BASIC ANONYMIZATION --------------------------------------------
cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")

raw_data <- readr::read_csv(RAW_FILE, col_types = cols(.default = "c"))
cat("Loaded ", nrow(raw_data), " rows and ", ncol(raw_data), " columns\n", sep = "")

pii_columns <- c(
  "IPAddress","RecipientLastName","RecipientFirstName","RecipientEmail","ExternalReference",
  "LocationLatitude","LocationLongitude","FULL_NAME","GENDER","ORGANIZATION_NAME",
  "hq_address_line_1","hq_address_line_2","hq_email","hq_phone","primary_contact_email",
  "primary_contact_phone","website","lp_name","PRIMARY_POSITION","description",
  "FIRM_ORGANIZATION_ID_NUMBER","pbid"
)

cat("Anonymizing data (removing PII)...\n")
anonymized_basic <-
  raw_data %>%
  mutate(
    respondent_id   = if ("ResponseId"           %in% names(.)) create_hash_id(.data[["ResponseId"]],          "RESP") else NA_character_,
    organization_id = if ("ORGANIZATION_ID"      %in% names(.)) create_hash_id(.data[["ORGANIZATION_ID"]],     "ORG")  else NA_character_,
    firm_id         = if ("FIRM_ORGANIZATION_ID" %in% names(.)) create_hash_id(.data[["FIRM_ORGANIZATION_ID"]],"FIRM") else NA_character_
  ) %>%
  select(-any_of(c(pii_columns, "ResponseId", "ORGANIZATION_ID", "FIRM_ORGANIZATION_ID"))) %>%
  mutate(
    hq_country_region = case_when(
      "hq_country" %in% names(.) & .data[["hq_country"]] %in% c("United States","USA","US","Canada","CA") ~ "North America",
      "hq_country" %in% names(.) & .data[["hq_country"]] %in% c(
        "United Kingdom","UK","Germany","DE","France","FR","Netherlands","Switzerland","Sweden","Denmark",
        "Norway","Spain","Italy","Belgium","Austria","Finland","Portugal","Ireland","Greece","Czechia"
      ) ~ "Europe",
      "hq_country" %in% names(.) & .data[["hq_country"]] %in% c(
        "China","CN","Japan","JP","Singapore","SG","India","IN","South Korea","KR","Hong Kong","Taiwan"
      ) ~ "Asia",
      "hq_country" %in% names(.) & !is.na(.data[["hq_country"]]) ~ "Other",
      TRUE ~ NA_character_
    )
  ) %>%
  {
    if ("hq_country_region" %in% names(.)) {
      . %>% select(-any_of(c("hq_state_province","hq_city","hq_zip_code","hq_country"))) %>%
        rename(hq_country = "hq_country_region")
    } else .
  } %>%
  mutate(
    StartDate_month    = if ("StartDate"    %in% names(.)) safe_month(.data[["StartDate"]])    else NA_character_,
    EndDate_month      = if ("EndDate"      %in% names(.)) safe_month(.data[["EndDate"]])      else NA_character_,
    RecordedDate_month = if ("RecordedDate" %in% names(.)) safe_month(.data[["RecordedDate"]]) else NA_character_
  ) %>%
  select(-any_of(c("StartDate","EndDate","RecordedDate")))

readr::write_csv(anonymized_basic, OUT_ANON_BASIC)
cat("Saved anonymized dataset → ", OUT_ANON_BASIC, "\n", sep = "")

# ---- PART 2: CLASSIFICATION TEMPLATE -----------------------------------------
cat("\n=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

responses_to_classify <-
  anonymized_basic %>%
  select(any_of(c("respondent_id", "Q2.1", "Q2.1_12_TEXT"))) %>%
  filter(!is.na(`Q2.1`)) %>%
  filter(`Q2.1` != "{\"ImportId\":\"QID174\"}",
         `Q2.1` != "Which of the following best describes your role? (Please select the most appropriate option) - Selected Choice")

classification_template <-
  responses_to_classify %>%
  mutate(
    original_response = `Q2.1`,
    other_text        = `Q2.1_12_TEXT`,
    needs_harmonization = `Q2.1` == "Other (please specify)",
    preliminary_category = case_when(
      str_detect(`Q2.1`, fixed("Venture Capital Firm"))             ~ "Venture Capital Firm",
      `Q2.1` == "Entrepreneur in Climate Technology"                 ~ "Entrepreneur in Climate Technology",
      `Q2.1` == "Government Funding Agency"                          ~ "Government Funding Agency",
      `Q2.1` == "Philanthropic Organization"                         ~ "Philanthropic Organization",
      `Q2.1` == "Limited Partner"                                    ~ "Limited Partner",
      `Q2.1` == "High Net-Worth Individual"                          ~ "High Net-Worth Individual",
      str_detect(`Q2.1`, fixed("Family Office"))                     ~ "Family Office",
      str_detect(`Q2.1`, fixed("ESG Investor"))                      ~ "ESG Investor",
      `Q2.1` == "Private Equity Firm"                                ~ "Private Equity Firm",
      `Q2.1` == "Corporate Venture Arm"                              ~ "Corporate Venture Arm",
      `Q2.1` == "Angel Investor"                                     ~ "Angel Investor",
      `Q2.1` == "Nonprofit Organization"                             ~ "Nonprofit Organization",
      `Q2.1` == "Academic or Research Institution"                   ~ "Academic or Research Institution",
      `Q2.1` == "Other (please specify)"                             ~ "NEEDS_CLASSIFICATION",
      TRUE                                                           ~ "Other"
    ),
    final_category_appendix_j = NA_character_
  )

readr::write_csv(classification_template, OUT_TEMPLATE)
cat("Classification template saved → ", OUT_TEMPLATE, "\n", sep = "")
cat("Complete this file per Appendix J and save as:\n  ", IN_CLASS_COMPLETE, "\n", sep = "")

# ---- PART 3: PRELIM/CLASSIFIED OUTPUT ----------------------------------------
cat("\n=== PART 3: CLASSIFIED DATA OUTPUT ===\n")

if (file.exists(IN_CLASS_COMPLETE)) {
  cat("Found completed classifications → merging…\n")
  classifications <- readr::read_csv(IN_CLASS_COMPLETE, show_col_types = FALSE)
  anonymized_classified <-
    anonymized_basic %>%
    left_join(
      classifications %>% select(respondent_id, final_category_appendix_j),
      by = "respondent_id"
    ) %>%
    mutate(stakeholder_category = coalesce(final_category_appendix_j, NA_character_))
  readr::write_csv(anonymized_classified, OUT_ANON_CLASS)
  cat("Classified dataset saved → ", OUT_ANON_CLASS, "\n", sep = "")
} else {
  cat("No completed classifications found. Writing PRELIMINARY classified dataset…\n")
  anonymized_classified <-
    anonymized_basic %>%
    left_join(
      classification_template %>% select(respondent_id, preliminary_category),
      by = "respondent_id"
    ) %>%
    mutate(stakeholder_category = preliminary_category)
  readr::write_csv(anonymized_classified, OUT_ANON_PRELIM)
  cat("Preliminary classified dataset saved → ", OUT_ANON_PRELIM, "\n", sep = "")
}

# ---- PART 4: DATA DICTIONARY --------------------------------------------------
cat("\n=== ANONYMIZATION SUMMARY ===\n")
cat("Original columns:   ", ncol(raw_data), "\n", sep = "")
cat("Anonymized columns: ", ncol(anonymized_basic), "\n", sep = "")
cat("Columns removed:    ", ncol(raw_data) - ncol(anonymized_basic), "\n", sep = "")
cat("Rows preserved:     ", nrow(anonymized_basic), "\n", sep = "")

data_dict <-
  tibble(
    variable_name    = names(anonymized_basic),
    type             = map_chr(anonymized_basic, ~ class(.x)[1]),
    n_missing        = map_int(anonymized_basic, ~ sum(is.na(.x))),
    n_non_missing    = nrow(anonymized_basic) - n_missing,
    completeness_pct = round(n_non_missing / nrow(anonymized_basic) * 100, 1)
  )

readr::write_csv(data_dict, OUT_DICTIONARY)
cat("Data dictionary saved → ", OUT_DICTIONARY, "\n", sep = "")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next steps:\n",
    "1) Review/complete ", OUT_TEMPLATE, "\n",
    "2) Save completed classifications as ", IN_CLASS_COMPLETE, "\n",
    "3) Run 02_classify_stakeholders.R and then get_exact_1307.R\n", sep = "")