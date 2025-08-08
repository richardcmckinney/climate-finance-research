# 01_anonymize_data.R  — publication-grade, base-R (no tidyverse)
# Purpose : Anonymize survey data, generalize dates & geography, prepare
#           Appendix J classification template, optionally merge manual
#           classifications, and write a data dictionary.
# Author  : Richard McKinney
# Date    : 2025-08-08

###############################################################################
# Configuration
###############################################################################
if (!"methods" %in% loadedNamespaces()) library(methods)

# Input directory for raw Qualtrics export (CSV)
RAW_DIR <- "data_raw"

# Output files (relative to project root)
OUT_ANON_CSV              <- "data/climate_finance_survey_anonymized.csv"
OUT_PRELIM_CLASSIFIED_CSV <- "data/climate_finance_survey_anonymized_preliminary.csv"
OUT_FINAL_CLASSIFIED_CSV  <- "data/climate_finance_survey_anonymized_classified.csv"
OUT_TEMPLATE_CSV          <- "docs/appendix_j_classification_template.csv"
IN_MANUAL_CLASSIFIED_CSV  <- "docs/appendix_j_classifications_complete.csv"
OUT_DATA_DICTIONARY_CSV   <- "data/data_dictionary.csv"

# Optional: deterministic random seed if any sampling is ever introduced
set.seed(1307)

###############################################################################
# Helpers (base-R only; digest optional; lubridate optional)
###############################################################################

# deterministic short-hash IDs using digest (sha256)
create_hash_id <- function(x, prefix = "ID") {
  if (!requireNamespace("digest", quietly = TRUE)) {
    stop("Package 'digest' is required for hashing. Install with install.packages('digest').")
  }
  vapply(x, function(i) {
    if (is.na(i) || identical(i, "")) return(NA_character_)
    paste0(prefix, "_", substr(digest::digest(as.character(i), algo = "sha256"), 1, 8))
  }, character(1))
}

has_col <- function(df, nm) nm %in% names(df)

# Very tolerant month extractor → returns "YYYY-MM" or NA
safe_month <- function(x, raw_df_rows = NULL) {
  if (is.null(x)) {
    if (is.null(raw_df_rows)) return(NA_character_)
    return(rep(NA_character_, raw_df_rows))
  }
  y <- as.character(x)
  y[is.na(y) | y == ""] <- NA_character_

  # Broad formats: US/ISO, with/without seconds, AM/PM, timezone, T separators
  try_formats <- c(
    "%m/%d/%Y %I:%M %p",
    "%m/%d/%Y %H:%M",
    "%m/%d/%y %I:%M %p",
    "%m/%d/%y %H:%M",
    "%m/%d/%Y",
    "%m/%d/%y",
    "%Y-%m-%d %H:%M:%S %z",
    "%Y-%m-%d %H:%M:%S",
    "%Y-%m-%dT%H:%M:%S%z",
    "%Y-%m-%dT%H:%M:%SZ",
    "%Y-%m-%dT%H:%M:%S",
    "%Y-%m-%d"
  )

  parsed <- suppressWarnings(as.POSIXct(y, tz = "UTC", tryFormats = try_formats))

  # Fallback: try lubridate if available
  if (all(is.na(parsed)) && requireNamespace("lubridate", quietly = TRUE)) {
    orders <- c("mdY HM", "mdy IMS p", "mdy HM p", "mdy", "Ymd HMSz", "Ymd HMS", "Ymd T", "Ymd")
    parsed <- suppressWarnings(lubridate::parse_date_time(y, orders = orders, tz = "UTC"))
  }

  # Fallback: regex to extract first YYYY-MM if present anywhere in the string
  ym <- sub("^.*?(\\d{4}-\\d{2}).*$", "\\1", y)
  ym[!grepl("\\d{4}-\\d{2}", ym)] <- NA_character_

  out <- rep(NA_character_, length(y))
  out[!is.na(parsed)] <- format(parsed[!is.na(parsed)], "%Y-%m")
  out[is.na(parsed)]  <- ym[is.na(parsed)]
  out
}

# Lowercase/normalize helper for text classification
to_lc <- function(x) {
  x <- as.character(x)
  x[is.na(x) | x == ""] <- NA_character_
  tolower(x)
}

###############################################################################
# Ensure folders exist
###############################################################################
if (!dir.exists("data"))   dir.create("data",   recursive = TRUE)
if (!dir.exists("docs"))   dir.create("docs",   recursive = TRUE)
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

###############################################################################
# Locate and read the newest raw CSV from data_raw/
###############################################################################
raw_candidates <- list.files(RAW_DIR, pattern = "\\.csv$", full.names = TRUE)
if (!length(raw_candidates)) {
  stop("No raw CSV found in ", RAW_DIR, "/ . Place your Qualtrics export there.")
}
raw_infile <- raw_candidates[order(file.info(raw_candidates)$mtime, decreasing = TRUE)][1]
cat("Raw input: ", raw_infile, "\n\n", sep = "")

raw_data <- read.csv(raw_infile, stringsAsFactors = FALSE, check.names = FALSE)

cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")
cat("Loaded ", nrow(raw_data), " rows and ", ncol(raw_data), " columns\n", sep = "")
cat("Anonymizing data (removing PII)...\n")

###############################################################################
# Start anonymized frame from raw
###############################################################################
df <- raw_data

# Deterministic respondent_id
if (has_col(df, "ResponseId")) {
  df$respondent_id <- create_hash_id(df$ResponseId, "RESP")
} else {
  # fallback sequential ID if missing ResponseId
  df$respondent_id <- sprintf("RESP_%04d", seq_len(nrow(df)))
}

# Optional org/firms
df$organization_id <- if (has_col(df, "ORGANIZATION_ID"))      create_hash_id(df$ORGANIZATION_ID, "ORG")  else NA_character_
df$firm_id         <- if (has_col(df, "FIRM_ORGANIZATION_ID")) create_hash_id(df$FIRM_ORGANIZATION_ID, "FIRM") else NA_character_

# Generalize date fields → add *_month and then drop raw date-time columns
if (has_col(raw_data, "StartDate"))    df$StartDate_month    <- safe_month(raw_data$StartDate)
if (has_col(raw_data, "EndDate"))      df$EndDate_month      <- safe_month(raw_data$EndDate)
if (has_col(raw_data, "RecordedDate")) df$RecordedDate_month <- safe_month(raw_data$RecordedDate)

drop_dates <- intersect(c("StartDate", "EndDate", "RecordedDate"), names(df))
if (length(drop_dates)) df <- df[, setdiff(names(df), drop_dates), drop = FALSE]

# Generalize geography (hq_country → region buckets; drop street-level)
if (has_col(df, "hq_country")) {
  hc <- as.character(df$hq_country)
  region <- rep(NA_character_, length(hc))
  # Regions (expand as needed)
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

  for (nm in c("hq_state_province","hq_city","hq_zip_code","hq_address_line_1","hq_address_line_2")) {
    if (has_col(df, nm)) df[[nm]] <- NULL
  }
}

# PII & high-risk fields to remove
pii_columns <- c(
  "IPAddress","RecipientLastName","RecipientFirstName","RecipientEmail",
  "ExternalReference","LocationLatitude","LocationLongitude","UserLanguage",
  "FULL_NAME","GENDER","ORGANIZATION_NAME","hq_email","hq_phone",
  "primary_contact_email","primary_contact_phone","website","lp_name",
  "PRIMARY_POSITION","description",
  "ORGANIZATION_ID","FIRM_ORGANIZATION_ID","FIRM_ORGANIZATION_ID_NUMBER","pbid"
)
drop_cols <- intersect(pii_columns, names(df))
if (length(drop_cols)) df <- df[, setdiff(names(df), drop_cols), drop = FALSE]

# Write anonymized dataset
write.csv(df, OUT_ANON_CSV, row.names = FALSE)
cat("Saved anonymized data → ", OUT_ANON_CSV, "\n", sep = "")

###############################################################################
# PART 2: Prepare Appendix J classification template
###############################################################################
cat("\n=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")

# Extract columns needed for classification
role_df <- data.frame(
  respondent_id = df$respondent_id,
  stringsAsFactors = FALSE, check.names = FALSE
)
role_df[["Q2.1"]]         <- if (has_col(df, "Q2.1")) df[["Q2.1"]] else NA_character_
role_df[["Q2.1_12_TEXT"]] <- if (has_col(df, "Q2.1_12_TEXT")) df[["Q2.1_12_TEXT"]] else NA_character_

# Preliminary heuristic bucket (expandable)
prelim_category <- function(role, other_text) {
  rl <- to_lc(role)
  tx <- to_lc(other_text)
  out <- rep("Miscellaneous and Individual Respondents", length(rl))

  # Entrepreneur in Climate Technology (priority rule)
  ent_flag <- (grepl("entrepreneur|founder|co[- ]?founder|ceo|cto", coalesce(rl, "")) |
               grepl("entrepreneur|founder|co[- ]?founder|ceo|cto", coalesce(tx, ""))) &
              (grepl("climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio", coalesce(rl, "")) |
               grepl("climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio", coalesce(tx, "")))
  out[ent_flag] <- "Entrepreneur in Climate Technology"

  out[grepl("\\bventure capital\\b", coalesce(rl,"")) & !grepl("corporate", coalesce(rl,""))] <- "Venture Capital Firm"
  out[grepl("private equity", coalesce(rl,""))] <- "Private Equity Firm"
  out[grepl("corporate venture", coalesce(rl,""))] <- "Corporate Venture Arm"
  out[grepl("angel investor", coalesce(rl,""))] <- "Angel Investor"
  out[grepl("esg investor",   coalesce(rl,""))] <- "ESG Investor"
  out[grepl("family office",  coalesce(rl,""))] <- "Family Office"
  out[grepl("limited partner|\\blp\\b", coalesce(rl,""))] <- "Limited Partner"
  out[grepl("non[- ]?profit|ngo",      coalesce(rl,""))] <- "Nonprofit Organization"
  out[grepl("academic|research institution|university", coalesce(rl,""))] <- "Academic or Research Institution"
  out[grepl("government|public sector|ministry|department|agency|dfi|development finance|multilateral|bilateral|municipality|federal|state", coalesce(rl,""))] <- "Government Funding Agency"
  out[grepl("philanthrop|foundation|grantmaker|giving", coalesce(rl,""))] <- "Philanthropic Organization"

  # Financial services broad bucket from free text
  inv_flag <- grepl("investment bank|asset manager|fund manager|sovereign wealth|insurance|private bank|debt fund|fintech|hedge fund|venture studio|commercial finance|private debt|impact investor|financial research|project finance|asset owner|pension fund|wealth management|green bond|blended finance",
                    coalesce(tx,""))
  out[is.na(rl) & inv_flag] <- "Investment and Financial Services"

  # Legal / consulting / corporate / manufacturing / energy / real estate / tech / media
  out[grepl("law firm|lawyer|attorney|solicitor|legal", coalesce(tx,""))] <- "Legal Services"
  out[grepl("consult|advisor|advisory|strategist|management consulting|sustainability consultant|esg advisory", coalesce(tx,"")) &
      !grepl("law|legal|attorney", coalesce(tx,""))] <- "Business Consulting and Advisory"
  out[grepl("corporate|conglomerate|multinational|holding company|plc|gmbh|ag |s\\.a\\.|incorporated|corporation|ltd", coalesce(tx,"")) &
      !grepl("venture|consult|law", coalesce(tx,""))] <- "Corporate Entities"
  out[grepl("manufactur|industrial|factory|production|assembly|processing|engineering firm", coalesce(tx,"")) &
      !grepl("consult|software", coalesce(tx,""))] <- "Manufacturing and Industrial"
  out[grepl("renewable energy|power producer|energy services|utility|grid|infrastructure|solar developer|wind developer|hydro|geothermal|biomass|waste-to-energy", coalesce(tx,""))] <- "Energy and Infrastructure"
  out[grepl("real estate|property|reit|building|housing|commercial property", coalesce(tx,""))] <- "Real Estate and Property"
  out[grepl("software|saas|platform|technology|digital|data|ai |artificial intelligence|blockchain|app |application", coalesce(tx,"")) &
      !grepl("consult|law", coalesce(tx,""))] <- "Technology and Software"
  out[grepl("media|communications|press|journalism|publication|broadcast", coalesce(tx,""))] <- "Media and Communication"

  out
}

classification_template <- role_df
classification_template$preliminary_category      <- prelim_category(role_df[["Q2.1"]], role_df[["Q2.1_12_TEXT"]])
classification_template$final_category_appendix_j <- NA_character_
classification_template$needs_harmonization       <- role_df[["Q2.1"]] == "Other (please specify)" |
                                                     is.na(classification_template$preliminary_category)

write.csv(classification_template, OUT_TEMPLATE_CSV, row.names = FALSE)
cat("Classification template → ", OUT_TEMPLATE_CSV, "\n", sep = "")
cat("Complete manually & save as ", IN_MANUAL_CLASSIFIED_CSV, " (optional)\n", sep = "")

###############################################################################
# PART 3: Join manual classifications (if available) or write preliminary file
###############################################################################
cat("\n=== PART 3: CLASSIFIED DATA OUTPUT ===\n")

if (file.exists(IN_MANUAL_CLASSIFIED_CSV)) {
  cat("Manual classifications found; merging…\n")
  manual <- read.csv(IN_MANUAL_CLASSIFIED_CSV, stringsAsFactors = FALSE, check.names = FALSE)

  required_cols <- c("respondent_id", "final_category_appendix_j")
  if (!all(required_cols %in% names(manual))) {
    stop("Manual classification file must include columns: respondent_id, final_category_appendix_j")
  }

  # Merge manual final categories
  anon_classified <- merge(df, manual[, required_cols], by = "respondent_id", all.x = TRUE, sort = FALSE)

  # Fallback to preliminary if manual is missing a row
  prelim_map <- classification_template[, c("respondent_id","preliminary_category")]
  anon_classified <- merge(anon_classified, prelim_map, by = "respondent_id", all.x = TRUE, sort = FALSE)

  anon_classified$Final_Role_Category <- ifelse(
    is.na(anon_classified$final_category_appendix_j),
    anon_classified$preliminary_category,
    anon_classified$final_category_appendix_j
  )

  # Clean columns
  if ("preliminary_category" %in% names(anon_classified))      anon_classified$preliminary_category <- NULL
  if ("final_category_appendix_j" %in% names(anon_classified)) anon_classified$final_category_appendix_j <- NULL

  write.csv(anon_classified, OUT_FINAL_CLASSIFIED_CSV, row.names = FALSE)
  cat("Classified anonymized data → ", OUT_FINAL_CLASSIFIED_CSV, "\n", sep = "")
} else {
  cat("Manual classifications NOT found; writing preliminary classified file…\n")
  prelim_map <- classification_template[, c("respondent_id","preliminary_category")]
  anon_classified_prelim <- merge(df, prelim_map, by = "respondent_id", all.x = TRUE, sort = FALSE)
  names(anon_classified_prelim)[names(anon_classified_prelim) == "preliminary_category"] <- "Final_Role_Category"
  write.csv(anon_classified_prelim, OUT_PRELIM_CLASSIFIED_CSV, row.names = FALSE)
  cat("Preliminary classified data → ", OUT_PRELIM_CLASSIFIED_CSV, "\n", sep = "")
}

###############################################################################
# PART 4: Data dictionary
###############################################################################
cat("\n=== PART 4: DATA DICTIONARY ===\n")
anon_for_dict <- read.csv(OUT_ANON_CSV, stringsAsFactors = FALSE, check.names = FALSE)
n <- nrow(anon_for_dict)

dict <- data.frame(
  variable_name    = names(anon_for_dict),
  type             = vapply(anon_for_dict, function(x) class(x)[1], character(1)),
  n_missing        = vapply(anon_for_dict, function(x) sum(is.na(x)), integer(1)),
  n_non_missing    = NA_integer_,
  completeness_pct = NA_real_,
  stringsAsFactors = FALSE, check.names = FALSE
)
dict$n_non_missing    <- n - dict$n_missing
dict$completeness_pct <- round(dict$n_non_missing / n * 100, 1)

write.csv(dict, OUT_DATA_DICTIONARY_CSV, row.names = FALSE)
cat("Data dictionary → ", OUT_DATA_DICTIONARY_CSV, "\n", sep = "")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next:\n")
cat("  • (Optional) Finalize ", IN_MANUAL_CLASSIFIED_CSV, " and re-run this file\n", sep = "")
cat("  • Then run: R/02_classify_stakeholders.R (if using automated text rules)\n")
cat("  • Then run: R/get_exact_1307.R to produce the exact Appendix J dataset (N=1,307)\n")