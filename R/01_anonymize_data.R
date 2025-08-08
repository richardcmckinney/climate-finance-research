# 01_anonymize_data.R
# Purpose: Create publication-ready anonymized datasets and a classification template
# Author: Richard McKinney
# Date: 2025-08-08

# ────────────────────────────────────────────────────────────────────────────────
# 0) Libraries & session hygiene
# ────────────────────────────────────────────────────────────────────────────────
# Ensure S4 methods are available for packages that use them in Rscript context
if (!"methods" %in% loadedNamespaces()) library(methods)

suppressPackageStartupMessages({
  library(tidyverse)  # dplyr, readr, stringr, tibble, etc.
  # lubridate is optional; used as a tolerant fallback for date parsing
})

# ────────────────────────────────────────────────────────────────────────────────
# 1) Paths & directories
# ────────────────────────────────────────────────────────────────────────────────
dir.create("data",           showWarnings = FALSE, recursive = TRUE)
dir.create("data_raw",       showWarnings = FALSE, recursive = TRUE)
dir.create("data_processed", showWarnings = FALSE, recursive = TRUE)
dir.create("docs",           showWarnings = FALSE, recursive = TRUE)
dir.create("output",         showWarnings = FALSE, recursive = TRUE)

out_basic_csv  <- file.path("data", "climate_finance_survey_anonymized.csv")
out_dict_csv   <- file.path("data", "data_dictionary.csv")
tmpl_csv       <- file.path("docs", "appendix_j_classification_template.csv")
final_classified_csv <- file.path("data", "climate_finance_survey_anonymized_classified.csv")
prelim_classified_csv <- file.path("data_processed", "survey_responses_anonymized_preliminary.csv")
completed_classifications_csv <- file.path("docs", "appendix_j_classifications_complete.csv")

# ────────────────────────────────────────────────────────────────────────────────
# 2) Helpers (safe, vectorized, never error)
# ────────────────────────────────────────────────────────────────────────────────
has_col <- function(df, nm) nm %in% names(df)

drop_if_present <- function(df, cols) {
  df %>% select(-any_of(cols))
}

# Robust month parser — NEVER errors; returns "YYYY-MM" or NA
safe_month <- function(x) {
  if (is.null(x)) return(NA_character_)
  v <- trimws(as.character(x))
  v[v == ""] <- NA_character_

  try_formats <- c(
    "%Y-%m-%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y-%m-%d",
    "%m/%d/%Y %H:%M:%S", "%m/%d/%Y %H:%M", "%m/%d/%Y",
    "%m/%d/%y %H:%M:%S", "%m/%d/%y %H:%M", "%m/%d/%y",
    "%d/%m/%Y %H:%M:%S", "%d/%m/%Y %H:%M", "%d/%m/%Y",
    "%d-%b-%Y %H:%M", "%d-%b-%Y",
    "%b %d, %Y %H:%M", "%b %d, %Y"
  )
  lub_orders <- c("Ymd HMS","Ymd HM","Ymd",
                  "mdY HMS","mdY HM","mdY",
                  "dmy HMS","dmy HM","dmy",
                  "d-b-Y HM","d-b-Y","b d, Y HM","b d, Y")

  parse_one <- function(s) {
    if (is.na(s)) return(NA_character_)
    # Try base formats one by one; swallow all errors
    for (fmt in try_formats) {
      dt <- try(as.POSIXct(s, format = fmt, tz = "UTC"), silent = TRUE)
      if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m"))
    }
    # Fallback to lubridate if present
    if (requireNamespace("lubridate", quietly = TRUE)) {
      dt <- try(
        lubridate::parse_date_time(s, orders = lub_orders, tz = "UTC", quiet = TRUE),
        silent = TRUE
      )
      if (!inherits(dt, "try-error") && !is.na(dt)) return(format(dt, "%Y-%m"))
    }
    NA_character_
  }

  vapply(v, parse_one, character(1))
}

# Preliminary category from Q2.1 text (conservative, Appendix-J aligned buckets)
map_q21_to_prelim <- function(role_value) {
  if (is.na(role_value) || role_value == "") return(NA_character_)
  rl <- tolower(role_value)

  case_when(
    str_detect(rl, "entrepreneur|climate tech|startup")                         ~ "Entrepreneur in Climate Technology",
    str_detect(rl, "venture capital") & !str_detect(rl, "corporate")            ~ "Venture Capital Firm",
    str_detect(rl, "private equity")                                            ~ "Private Equity Firm",
    str_detect(rl, "high net|hnwi")                                             ~ "High Net-Worth Individual",
    str_detect(rl, "nonprofit|non-profit") & !str_detect(rl, "philanthropic")   ~ "Nonprofit Organization",
    str_detect(rl, "academic|research|university|institute")                    ~ "Academic or Research Institution",
    str_detect(rl, "limited partner|\\blp\\b")                                  ~ "Limited Partner",
    str_detect(rl, "family office")                                             ~ "Family Office",
    str_detect(rl, "corporate venture")                                         ~ "Corporate Venture Arm",
    str_detect(rl, "angel investor")                                            ~ "Angel Investor",
    str_detect(rl, "esg investor")                                              ~ "ESG Investor",
    str_detect(rl, "government|public sector|ministry|department|agency|dfi")   ~ "Government Funding Agency",
    str_detect(rl, "philanthropic|foundation")                                   ~ "Philanthropic Organization",
    TRUE                                                                        ~ NA_character_
  )
}

# Category from “Other (free text)”
map_free_text_to_category <- function(txt) {
  if (is.na(txt) || txt == "") return("Miscellaneous and Individual Respondents")
  t <- tolower(txt)

  # Entrepreneur (high priority)
  if (str_detect(t, "founder|co[- ]?founder|cto|ceo|entrepreneur") &&
      str_detect(t, "climate|clean|green|sustain|energy|carbon|renewable|solar|wind|battery|hydrogen|bio")) {
    return("Entrepreneur in Climate Technology")
  }

  if (str_detect(t, "investment bank|asset manager|fund manager|sovereign wealth|insurance|private bank|debt fund|fintech|hedge fund|venture studio|commercial finance|private debt|impact investor|financial research|project finance|asset owner|pension fund|esg rating|wealth management|green bond|blended finance")) {
    return("Investment and Financial Services")
  }

  if (str_detect(t, "law firm|lawyer|attorney|solicitor|legal advisor|legal counsel|patent|barrister")) {
    return("Legal Services")
  }

  if (str_detect(t, "consult|advisor|advisory|strategist|m&a|management consulting|sustainability consultant|esg advisory") &&
      !str_detect(t, "law|legal|attorney")) {
    return("Business Consulting and Advisory")
  }

  if (str_detect(t, "corporate|conglomerate|multinational|holding company|plc|gmbh|ag |s\\.a\\.|inc\\.|corporation|ltd") &&
      !str_detect(t, "venture|consult|law")) {
    return("Corporate Entities")
  }

  if (str_detect(t, "manufactur|industrial|factory|production|assembly|processing|engineering firm") &&
      !str_detect(t, "consult|software")) {
    return("Manufacturing and Industrial")
  }

  if (str_detect(t, "renewable energy|power producer|energy services|utility|grid|infrastructure|solar developer|wind developer|hydro|geothermal|biomass|waste[- ]?to[- ]?energy")) {
    return("Energy and Infrastructure")
  }

  if (str_detect(t, "real estate|property|reit|building|housing|commercial property")) {
    return("Real Estate and Property")
  }

  if (str_detect(t, "software|saas|platform|technology|digital|data|ai |artificial intelligence|blockchain|app ")) {
    return("Technology and Software")
  }

  if (str_detect(t, "media|communication|press|journalism|publication|broadcast")) {
    return("Media and Communication")
  }

  if (str_detect(t, "foundation|philanthropic|charitable trust|donor|grantmaker|giving")) {
    return("Philanthropic Organization")
  }

  if (str_detect(t, "ngo|nonprofit|non-profit|charity|association|institute|advocacy|civil society")) {
    return("Nonprofit Organization")
  }

  if (str_detect(t, "government|public sector|ministry|department|agency|dfi|development finance|multilateral|bilateral|municipality|federal|state agency")) {
    return("Government Funding Agency")
  }

  "Miscellaneous and Individual Respondents"
}

# Deterministic hash id
create_hash_id <- function(x, prefix = "ID") {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for hashing; please install.packages('digest').")
  }
  sapply(x, function(i) if (is.na(i) || i == "") NA_character_ else {
    paste0(prefix, "_", substr(digest::digest(as.character(i), algo = "sha256"), 1, 8))
  })
}

# ────────────────────────────────────────────────────────────────────────────────
# 3) Read raw data (choose the first CSV in data_raw by default)
# ────────────────────────────────────────────────────────────────────────────────
raw_files <- list.files("data_raw", pattern = "\\.csv$", full.names = TRUE)
if (length(raw_files) == 0) stop("No CSV found in 'data_raw/'. Place the Qualtrics export there.")

infile <- raw_files[1]
cat("Raw input:", infile, "\n\n")

raw_data <- readr::read_csv(
  infile,
  col_types = readr::cols(.default = readr::col_character())  # keep everything as character
)

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")
cat("Loaded", nrow(raw_data), "rows and", ncol(raw_data), "columns\n")
cat("Anonymizing data (removing PII)...\n")

# ────────────────────────────────────────────────────────────────────────────────
# 4) Build anonymized dataset
# ────────────────────────────────────────────────────────────────────────────────
pii_columns <- c(
  "IPAddress", "RecipientLastName", "RecipientFirstName", "RecipientEmail",
  "ExternalReference", "LocationLatitude", "LocationLongitude", "FULL_NAME",
  "GENDER", "ORGANIZATION_NAME", "hq_address_line_1", "hq_address_line_2",
  "hq_email", "hq_phone", "primary_contact_email", "primary_contact_phone",
  "website", "lp_name", "PRIMARY_POSITION", "description",
  "FIRM_ORGANIZATION_ID_NUMBER", "pbid"
)

df <- raw_data

# IDs (deterministic, only if source columns exist)
if (has_col(df, "ResponseId"))           df$respondent_id  <- create_hash_id(df$ResponseId, "RESP")
if (has_col(df, "ORGANIZATION_ID"))      df$organization_id <- create_hash_id(df$ORGANIZATION_ID, "ORG")
if (has_col(df, "FIRM_ORGANIZATION_ID")) df$firm_id         <- create_hash_id(df$FIRM_ORGANIZATION_ID, "FIRM")

# Robust date → YYYY-MM
date_cols <- c("StartDate", "EndDate", "RecordedDate")
have_dates <- intersect(date_cols, names(df))
if (length(have_dates)) {
  for (nm in have_dates) {
    df[[paste0(nm, "_month")]] <- safe_month(df[[nm]])
  }
}

# Geography generalization to regions (if hq_country present)
if (has_col(df, "hq_country")) {
  region <- rep(NA_character_, nrow(df))
  hc <- tolower(df$hq_country)

  na_to_false <- function(x) ifelse(is.na(x), FALSE, x)

  region[na_to_false(hc %in% tolower(c("United States","USA","US","Canada","CA")))] <- "North America"

  region[na_to_false(hc %in% tolower(c(
    "United Kingdom","UK","England","Scotland","Wales","Ireland","Germany","DE",
    "France","FR","Netherlands","Switzerland","Sweden","Denmark","Norway",
    "Spain","Italy","Belgium","Austria","Portugal","Greece"
  )))] <- "Europe"

  region[na_to_false(hc %in% tolower(c(
    "China","CN","Japan","JP","Singapore","SG","India","IN","South Korea","KR",
    "Hong Kong","Taiwan","Indonesia","Malaysia","Philippines","Thailand","Vietnam"
  )))] <- "Asia"

  region[na_to_false(hc %in% tolower(c("Australia","New Zealand")))] <- "Oceania"

  region[na_to_false(hc %in% tolower(c(
    "Brazil","Argentina","Chile","Peru","Colombia","Mexico"
  )))] <- "Latin America"

  region[na_to_false(hc %in% tolower(c(
    "South Africa","Nigeria","Kenya","Egypt","Morocco"
  )))] <- "Africa"

  df$hq_region <- region
  # drop street-level columns if present
  df <- drop_if_present(df, c("hq_state_province","hq_city","hq_zip_code"))
}

# Remove PII + raw IDs
df <- drop_if_present(df, c(pii_columns, "ResponseId","ORGANIZATION_ID","FIRM_ORGANIZATION_ID"))

# Save canonical anonymized file
readr::write_csv(df, out_basic_csv)
cat("Saved anonymized dataset to:", out_basic_csv, "\n\n")

# ────────────────────────────────────────────────────────────────────────────────
# 5) Classification template (for Appendix J)
# ────────────────────────────────────────────────────────────────────────────────
cat("=== PART 2: PREPARING CLASSIFICATION TEMPLATE ===\n")

q21  <- if (has_col(df, "Q2.1")) df$Q2.1 else NA_character_
q21o <- if (has_col(df, "Q2.1_12_TEXT")) df$Q2.1_12_TEXT else NA_character_

classification_template <- tibble(
  respondent_id = if (has_col(df, "respondent_id")) df$respondent_id else NA_character_,
  Q2.1 = q21,
  Q2.1_12_TEXT = q21o
) %>%
  mutate(
    preliminary_category = coalesce(map_q21_to_prelim(Q2.1), NA_character_),
    needs_harmonization  = is.na(preliminary_category) | Q2.1 == "Other (please specify)",
    final_category_appendix_j = NA_character_
  )

readr::write_csv(classification_template, tmpl_csv)
cat("Classification template saved to:", tmpl_csv, "\n\n")

# ────────────────────────────────────────────────────────────────────────────────
# 6) Produce classified dataset (uses completed file if available, else preliminary)
# ────────────────────────────────────────────────────────────────────────────────
cat("=== PART 3: CLASSIFIED DATA OUTPUT ===\n")

if (file.exists(completed_classifications_csv)) {
  cat("Detected completed classifications at", completed_classifications_csv, "→ producing FINAL classified file.\n")
  completed <- readr::read_csv(completed_classifications_csv, show_col_types = FALSE)

  classified <- df %>%
    left_join(
      completed %>% select(respondent_id, final_category_appendix_j),
      by = "respondent_id"
    ) %>%
    mutate(
      stakeholder_category =
        coalesce(final_category_appendix_j, map_q21_to_prelim(Q2.1), map_free_text_to_category(Q2.1_12_TEXT))
    )

  readr::write_csv(classified, final_classified_csv)
  cat("Final classified data saved to:", final_classified_csv, "\n\n")

} else {
  cat("No completed classifications found. Writing PRELIMINARY classified file.\n")
  classified_prelim <- df %>%
    left_join(
      classification_template %>% select(respondent_id, preliminary_category),
      by = "respondent_id"
    ) %>%
    mutate(stakeholder_category = coalesce(preliminary_category, map_free_text_to_category(Q2.1_12_TEXT)))

  readr::write_csv(classified_prelim, prelim_classified_csv)
  cat("Preliminary classified data saved to:", prelim_classified_csv, "\n\n")
}

# ────────────────────────────────────────────────────────────────────────────────
# 7) Data dictionary
# ────────────────────────────────────────────────────────────────────────────────
cat("=== PART 4: DATA DICTIONARY ===\n")
dict <- tibble(
  variable_name  = names(df),
  type           = vapply(df, function(x) class(x)[1], character(1)),
  n_missing      = vapply(df, function(x) sum(is.na(x)), integer(1)),
  n_non_missing  = nrow(df) - n_missing,
  completeness   = round(100 * n_non_missing / nrow(df), 1)
)
readr::write_csv(dict, out_dict_csv)
cat("Data dictionary saved to:", out_dict_csv, "\n\n")

# ────────────────────────────────────────────────────────────────────────────────
# 8) Summary
# ────────────────────────────────────────────────────────────────────────────────
cat("=== PROCESS COMPLETE ===\n")
cat("Rows preserved:", nrow(df), "\n")
cat("Columns in anonymized dataset:", ncol(df), "\n")
cat("Outputs:\n")
cat(" - Anonymized:                       ", out_basic_csv, "\n")
cat(" - Classification template:         ", tmpl_csv, "\n")
if (file.exists(completed_classifications_csv)) {
  cat(" - Classified (final):              ", final_classified_csv, "\n")
} else {
  cat(" - Classified (preliminary):        ", prelim_classified_csv, "\n")
}
cat(" - Data dictionary:                 ", out_dict_csv, "\n")