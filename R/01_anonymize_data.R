# 01_anonymize_data.R (base-R, no tidyverse)
# Purpose: Anonymize survey data, prepare Appendix J template, and (if present)
#          merge manual classifications.
# Author : Richard McKinney
# Date   : 2025-08-08

## ---- Helpers ----------------------------------------------------------------
create_hash_id <- function(x, prefix = "ID") {
  # digest is used only here; load it locally
  if (!requireNamespace("digest", quietly = TRUE))
    stop("Package 'digest' is required. Install it with install.packages('digest').")
  vapply(x, function(i) {
    if (is.na(i) || identical(i, "")) return(NA_character_)
    paste0(prefix, "_", substr(digest::digest(as.character(i), algo = "sha256"), 1, 8))
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

## ---- Folders ----------------------------------------------------------------
if (!dir.exists("data"))   dir.create("data",   recursive = TRUE)
if (!dir.exists("docs"))   dir.create("docs",   recursive = TRUE)
if (!dir.exists("output")) dir.create("output", recursive = TRUE)

## ---- Locate raw CSV ----------------------------------------------------------
raw_candidates <- list.files("data_raw", pattern = "Accelerating climate finance.*\\.csv$", full.names = TRUE)
if (length(raw_candidates) == 0) stop("No raw survey CSV found in data_raw/.")
raw_infile <- raw_candidates[order(file.info(raw_candidates)$mtime, decreasing = TRUE)][1]
message("Raw input: ", raw_infile)

## ---- Read raw (as character) -------------------------------------------------
raw_data <- read.csv(raw_infile, stringsAsFactors = FALSE, check.names = FALSE)
cat("=== PART 1: BASIC ANONYMIZATION ===\n")
cat("Loading raw survey data...\n")
cat("Loaded ", nrow(raw_data), " rows and ", ncol(raw_data), " columns\n", sep = "")
cat("Anonymizing data (removing PII)...\n")

## ---- Start from raw ----------------------------------------------------------
df <- raw_data

## IDs -------------------------------------------------------------------------
if (has_col(df, "ResponseId")) {
  df$respondent_id <- create_hash_id(df$ResponseId, "RESP")
} else {
  df$respondent_id <- sprintf("RESP_%04d", seq_len(nrow(df)))
}
df$organization_id <- if (has_col(df, "ORGANIZATION_ID")) create_hash_id(df$ORGANIZATION_ID, "ORG") else NA_character_
df$firm_id         <- if (has_col(df, "FIRM_ORGANIZATION_ID")) create_hash_id(df$FIRM_ORGANIZATION_ID, "FIRM") else NA_character_

## Date generalization ---------------------------------------------------------
if (has_col(raw_data, "StartDate"))    df$StartDate_month    <- safe_month(raw_data$StartDate)
if (has_col(raw_data, "EndDate"))      df$EndDate_month      <- safe_month(raw_data$EndDate)
if (has_col(raw_data, "RecordedDate")) df$RecordedDate_month <- safe_month(raw_data$RecordedDate)

drop_dates <- intersect(c("StartDate","EndDate","RecordedDate"), names(df))
if (length(drop_dates) > 0) df <- df[, setdiff(names(df), drop_dates), drop = FALSE]

## HQ geography generalization --------------------------------------------------
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
  for (nm in c("hq_state_province","hq_city","hq_zip_code")) {
    if (has_col(df, nm)) df[[nm]] <- NULL
  }
}

## PII columns to drop ----------------------------------------------------------
pii_columns <- c(
  "IPAddress","RecipientLastName","RecipientFirstName","RecipientEmail",
  "ExternalReference","LocationLatitude","LocationLongitude","UserLanguage",
  "FULL_NAME","GENDER","ORGANIZATION_NAME","hq_address_line_1","hq_address_line_2",
  "hq_email","hq_phone","primary_contact_email","primary_contact_phone","website",
  "lp_name","PRIMARY_POSITION","description",
  "ORGANIZATION_ID","FIRM_ORGANIZATION_ID","FIRM_ORGANIZATION_ID_NUMBER","pbid"
)
drop_cols <- intersect(pii_columns, names(df))
if (length(drop_cols) > 0) df <- df[, setdiff(names(df), drop_cols), drop = FALSE]

## Save anonymized dataset ------------------------------------------------------
out_anon <- "data/climate_finance_survey_anonymized.csv"
write.csv(df, out_anon, row.names = FALSE)
cat("Saved anonymized data → ", out_anon, "\n", sep = "")

# ==============================================================================
# PART 2: Appendix J classification template
# ==============================================================================
cat("\n=== PART 2: PREPARING FOR APPENDIX J CLASSIFICATIONS ===\n")
role_df <- data.frame(
  respondent_id  = df$respondent_id,
  check.names = FALSE,
  stringsAsFactors = FALSE
)
role_df[["Q2.1"]]         <- if (has_col(df, "Q2.1")) df[["Q2.1"]] else NA_character_
role_df[["Q2.1_12_TEXT"]] <- if (has_col(df, "Q2.1_12_TEXT")) df[["Q2.1_12_TEXT"]] else NA_character_

to_lc <- function(x) ifelse(is.na(x) | x == "", NA_character_, tolower(x))
prelim_category <- function(role, other_text) {
  rl <- to_lc(role); tx <- to_lc(other_text)
  out <- rep("Miscellaneous and Individual Respondents", length(rl))
  is_ent <- (grepl("entrepreneur|founder", coalesce(rl, "")) |
             grepl("entrepreneur|founder|co[- ]?founder", coalesce(tx, ""))) &
            (grepl("climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio", coalesce(rl, "")) |
             grepl("climate|clean|green|sustain|energy|carbon|renew|solar|wind|battery|hydrogen|bio", coalesce(tx, "")))
  out[is_ent] <- "Entrepreneur in Climate Technology"
  out[grepl("\\bventure capital\\b", coalesce(rl,"")) & !grepl("corporate", coalesce(rl,""))] <- "Venture Capital Firm"
  out[grepl("private equity", coalesce(rl,""))] <- "Private Equity Firm"
  out[grepl("corporate venture", coalesce(rl,""))] <- "Corporate Venture Arm"
  out[grepl("angel investor", coalesce(rl,""))] <- "Angel Investor"
  out[grepl("esg investor", coalesce(rl,""))] <- "ESG Investor"
  out[grepl("family office", coalesce(rl,""))] <- "Family Office"
  out[grepl("limited partner|\\blp\\b", coalesce(rl,""))] <- "Limited Partner"
  out[grepl("non[- ]?profit|ngo", coalesce(rl,""))] <- "Nonprofit Organization"
  out[grepl("academic|research institution|university", coalesce(rl,""))] <- "Academic or Research Institution"
  out[grepl("government", coalesce(rl,""))] <- "Government Funding Agency"
  out[grepl("philanthrop", coalesce(rl,""))] <- "Philanthropic Organization"
  out
}

classification_template <- role_df
classification_template$preliminary_category      <- prelim_category(role_df[["Q2.1"]], role_df[["Q2.1_12_TEXT"]])
classification_template$final_category_appendix_j <- NA_character_
classification_template$needs_harmonization       <- role_df[["Q2.1"]] == "Other (please specify)" |
                                                     is.na(classification_template$preliminary_category)

out_template <- "docs/appendix_j_classification_template.csv"
write.csv(classification_template, out_template, row.names = FALSE)
cat("Classification template → ", out_template, "\n", sep = "")
cat("Complete manually and save as docs/appendix_j_classifications_complete.csv (optional)\n")

# ==============================================================================
# PART 3: Join manual classifications or write placeholder
# ==============================================================================
cat("\n=== PART 3: CLASSIFIED DATA OUTPUT ===\n")
manual_path <- "docs/appendix_j_classifications_complete.csv"
if (file.exists(manual_path)) {
  cat("Manual classifications found, joining…\n")
  manual <- read.csv(manual_path, stringsAsFactors = FALSE, check.names = FALSE)
  needed <- c("respondent_id", "final_category_appendix_j")
  if (!all(needed %in% names(manual))) stop("Manual file must include: respondent_id, final_category_appendix_j")
  m <- manual[, needed]
  names(m) <- needed
  # merge
  anon_classified <- merge(df, m, by = "respondent_id", all.x = TRUE, sort = FALSE)
  # carry preliminary category from template for fallback
  prelim_map <- classification_template[, c("respondent_id","preliminary_category")]
  anon_classified <- merge(anon_classified, prelim_map, by = "respondent_id", all.x = TRUE, sort = FALSE)
  anon_classified$Final_Role_Category <- ifelse(
    is.na(anon_classified$final_category_appendix_j),
    anon_classified$preliminary_category,
    anon_classified$final_category_appendix_j
  )
  anon_classified$preliminary_category <- NULL
  out_classified <- "data/climate_finance_survey_anonymized_classified.csv"
  write.csv(anon_classified, out_classified, row.names = FALSE)
  cat("Classified anonymized data → ", out_classified, "\n", sep = "")
} else {
  cat("Manual classifications not found. Writing preliminary classified placeholder…\n")
  prelim_map <- classification_template[, c("respondent_id","preliminary_category")]
  anon_classified_prelim <- merge(df, prelim_map, by = "respondent_id", all.x = TRUE, sort = FALSE)
  names(anon_classified_prelim)[names(anon_classified_prelim) == "preliminary_category"] <- "Final_Role_Category"
  out_prelim <- "data/climate_finance_survey_anonymized_preliminary.csv"
  write.csv(anon_classified_prelim, out_prelim, row.names = FALSE)
  cat("Preliminary classified data → ", out_prelim, "\n", sep = "")
}

# ==============================================================================
# PART 4: Data dictionary
# ==============================================================================
cat("\n=== PART 4: DATA DICTIONARY ===\n")
anon_for_dict <- read.csv(out_anon, stringsAsFactors = FALSE, check.names = FALSE)
n <- nrow(anon_for_dict)
dict <- data.frame(
  variable_name    = names(anon_for_dict),
  type             = vapply(anon_for_dict, function(x) class(x)[1], character(1)),
  n_missing        = vapply(anon_for_dict, function(x) sum(is.na(x)), integer(1)),
  n_non_missing    = NA_integer_,
  completeness_pct = NA_real_,
  stringsAsFactors = FALSE
)
dict$n_non_missing    <- n - dict$n_missing
dict$completeness_pct <- round(dict$n_non_missing / n * 100, 1)
out_dict <- "data/data_dictionary.csv"
write.csv(dict, out_dict, row.names = FALSE)
cat("Data dictionary → ", out_dict, "\n", sep = "")

cat("\n=== PROCESS COMPLETE ===\n")
cat("Next:\n")
cat("  • Run R/02_classify_stakeholders.R (if using automated text rules)\n")
cat("  • Or complete docs/appendix_j_classifications_complete.csv and re-run this script\n")
cat("  • Then run R/get_exact_1307.R to build the exact Appendix J dataset (N=1,307)\n")