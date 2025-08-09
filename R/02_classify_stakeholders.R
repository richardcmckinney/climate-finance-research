#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# 02_classify_stakeholders.R
# Purpose: Produce stakeholder classifications per Appendix J methodology
# Version: 4.0 - Fixed critical data joining bug and removed redundancy
# Key Changes:
#   - FIXED: Critical bug in data joining that could cause silent data loss
#   - FIXED: Now tracks actual column names from source data for proper joining
#   - REMOVED: Redundant pick_col function - now uses centralized helpers
#   - ENHANCED: Robust column tracking throughout pipeline
#   - MAINTAINED: STRICT privacy violation checking (stop_on_violation = TRUE)
#   - MAINTAINED: Robust case_when classification logic
# Outputs:
#   - docs/appendix_j_classification_template.csv
#   - data/survey_responses_anonymized_preliminary.csv
#   - output/classification_audit.csv

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
  library(rlang)  # Explicitly load rlang for sym() function
})

# Source central configuration
source("R/00_config.R")
check_deprecated()  # Warn about old files
validate_stage("classify")  # Validate prerequisites

message("=== STAKEHOLDER CLASSIFICATION (APPENDIX J) ===")
set_stage("Classification")

# --------------------------------------------------------------------
# Use paths from central config
# --------------------------------------------------------------------
in_basic  <- PATHS$basic_anon
out_template <- PATHS$classification_template
out_prelim   <- PATHS$preliminary_classified
out_audit    <- PATHS$classification_audit

# --------------------------------------------------------------------
# Load data
# --------------------------------------------------------------------
stopifnot(file.exists(in_basic))
df <- suppressMessages(read_csv(in_basic, show_col_types = FALSE))

# --------------------------------------------------------------------
# CRITICAL FIX: Track actual column names from source data
# --------------------------------------------------------------------
# This ensures we can properly join back to the original data
# by tracking what column names actually exist in the source

# Track the actual column name for respondent ID in the source data
respondent_id_column <- find_column(df, c(
  "respondent_id", "ResponseId", "_recordId", "response_id", "ID", "id"
))

if (is.null(respondent_id_column)) {
  stop("CRITICAL ERROR: No respondent ID column found in source data. ",
       "Expected one of: respondent_id, ResponseId, _recordId, response_id, ID, id")
}

message("✓ Found respondent ID column: '", respondent_id_column, "'")

# Extract role columns using centralized helpers
role_raw_column <- find_column(df, c(
  "role_raw", "Q2.1",
  "Which of the following best describes your role? (Please select the most appropriate option) - Selected Choice"
))

role_other_column <- find_column(df, c(
  "role_other", "Q2.1_12_TEXT",
  "Which of the following best describes your role? (Please select the most appropriate option) - Other (please specify)  - Text"
))

# Extract actual values (with safety for missing columns)
respondent_id <- if (!is.null(respondent_id_column)) df[[respondent_id_column]] else stop("Missing respondent ID")
role_raw <- if (!is.null(role_raw_column)) df[[role_raw_column]] else rep(NA_character_, nrow(df))
role_other <- if (!is.null(role_other_column)) df[[role_other_column]] else rep(NA_character_, nrow(df))

# Log column mapping for audit trail
message("\n=== COLUMN MAPPING ===")
message("Respondent ID: '", respondent_id_column, "' -> respondent_id")
if (!is.null(role_raw_column)) message("Role Raw: '", role_raw_column, "' -> role_raw")
if (!is.null(role_other_column)) message("Role Other: '", role_other_column, "' -> role_other")

# --------------------------------------------------------------------
# Classification Functions
# --------------------------------------------------------------------

# Normalize text for matching
# Standardizes text for consistent pattern matching
normalize_text <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    tolower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

# Helper function to check if text contains climate-related terms
# Used to distinguish climate tech entrepreneurs from general entrepreneurs
is_climate_related <- function(text) {
  climate_terms <- "climate|clean|green|sustain|carbon|emission|renewable|solar|wind|battery|storage|hydrogen|bio|ccus|adapt|energy|electric|environmental|esg"
  str_detect(text, climate_terms)
}

# Helper function to check if text contains entrepreneur-related terms
# Identifies various forms of entrepreneurial roles
is_entrepreneur_related <- function(text) {
  entrepreneur_terms <- "entrepreneur|founder|co[- ]?founder|ceo|cto|startup|venture\\s*builder|launching|started"
  str_detect(text, entrepreneur_terms)
}

# ROBUST classification using case_when for clarity and maintainability
# Priority order is preserved through the sequence of conditions
# Each condition is mutually exclusive due to case_when's first-match behavior
classify_stakeholder_robust <- function(raw_text, other_text) {
  # Combine both texts for analysis
  combined <- paste(
    ifelse(is.na(raw_text), "", raw_text),
    ifelse(is.na(other_text), "", other_text),
    sep = " "
  ) %>% normalize_text()
  
  # Return default for empty text
  if (nchar(combined) == 0) return("Miscellaneous and Individual Respondents")
  
  # Use case_when for clear, maintainable classification logic
  # Priority order is preserved through the sequence of conditions
  case_when(
    # 1. Entrepreneur in Climate Technology (highest priority for climate-specific)
    is_entrepreneur_related(combined) & is_climate_related(combined) ~ 
      "Entrepreneur in Climate Technology",
    
    # 2. Real Estate - Move this BEFORE Government to avoid misclassification
    str_detect(combined, "real\\s*estate|property\\s*developer|property\\s*investor|reit|commercial\\s*property|industrial\\s*real\\s*estate") ~ 
      "Real Estate and Property",
    
    # 3. Government and Public Sector - MORE SPECIFIC PATTERNS
    str_detect(combined, "dfi\\b|development\\s*finance\\s*institution|multilateral\\s*(development|organization|bank)|bilateral|government\\s+(agency|funding|organization)|ministry|federal\\s+(agency|government)|state\\s+(agency|government)|municipal|public\\s*sector|public\\s*pension|\\bnhs\\b") ~ 
      "Government Funding Agency",
    
    # 4. Corporate venture with law/legal check
    str_detect(combined, "corporate\\s*venture") & !str_detect(combined, "law|legal|consult") ~ 
      "Corporate Venture Arm",
    
    # 5. Venture Studio should be consulting
    str_detect(combined, "venture\\s*studio") ~ 
      "Business Consulting and Advisory",
    
    # 6. Venture Capital with specific refinements
    str_detect(combined, "venture\\s*capital|\\bvc\\b(?!\\w)|venture\\s*firm") & !str_detect(combined, "corporate|law") ~ 
      "Venture Capital Firm",
    
    # 7. Private Equity
    str_detect(combined, "private\\s*equity|\\bpe\\b") & !str_detect(combined, "backed|owned") ~ 
      "Private Equity Firm",
    
    # 8. Angel Investor
    str_detect(combined, "angel\\s*investor") ~ 
      "Angel Investor",
    
    # 9. Limited Partner
    str_detect(combined, "limited\\s*partner|\\blp\\b") ~ 
      "Limited Partner",
    
    # 10. Family Office with single family office refinement
    str_detect(combined, "family\\s*office|single\\s*family\\s*office") ~ 
      "Family Office",
    
    # 11. High Net-Worth Individual
    str_detect(combined, "high\\s*net|hnwi|retired.*wealthy") ~ 
      "High Net-Worth Individual",
    
    # 12. ESG Investor
    str_detect(combined, "esg\\s*investor|impact\\s*invest|sustainability\\s*invest|impact\\s*first") ~ 
      "ESG Investor",
    
    # 13. Philanthropic Organization
    str_detect(combined, "foundation|philanthrop|charitable|donor|grantmaker|giving\\s*fund|community\\s*foundation") & 
      !str_detect(combined, "nhs|trust\\s*beneficiary") ~ 
      "Philanthropic Organization",
    
    # 14. Nonprofit Organization
    str_detect(combined, "nonprofit|non[- ]?profit|ngo|charity|civil\\s*society|association") & 
      !str_detect(combined, "law|consult") ~ 
      "Nonprofit Organization",
    
    # 15. Academic or Research Institution
    str_detect(combined, "university|academic|research\\s*(center|institute|institution)|\\blab\\b") ~ 
      "Academic or Research Institution",
    
    # 16. Hedge Fund
    str_detect(combined, "hedge\\s*fund") ~ 
      "Investment and Financial Services",
    
    # 17. Sovereign Wealth
    str_detect(combined, "sovereign\\s*wealth") ~ 
      "Investment and Financial Services",
    
    # 18. Pension Fund
    str_detect(combined, "pension\\s*fund") ~ 
      "Investment and Financial Services",
    
    # 19. Investment and Financial Services (broader category)
    str_detect(combined, "investment\\s*bank|asset\\s*manage|fund\\s*manage|insurance|private\\s*bank|debt\\s*fund|financial\\s*(advisor|advisory|holding|infrastructure)|fintech|wealth\\s*manage|alternative\\s*asset|permanent\\s*capital|securitized.*projects") ~ 
      "Investment and Financial Services",
    
    # 20. Law firms with specific pattern
    str_detect(combined, "law\\s*firm|lawyer|attorney|solicitor|legal|barrister|patent\\s*attorney|law\\s*office") ~ 
      "Legal Services",
    
    # 21. Consulting including venture studio (already caught above but included for completeness)
    str_detect(combined, "consult|advis|strategy|strategist|m\\s*&\\s*a|management\\s*consult|consultant\\s*agency") ~ 
      "Business Consulting and Advisory",
    
    # 22. Energy and Infrastructure
    str_detect(combined, "utility|utlity|grid|renewable\\s*energy|power\\s*producer|ipp|solar\\s*develop|wind\\s*develop|infrastructure\\s*fund|energy\\s*(service|firm|sector)|electric\\s*cooperative|gas\\s*transmission|climate\\s*adaptive\\s*infrastructure") ~ 
      "Energy and Infrastructure",
    
    # 23. Manufacturing and Industrial (with typo detection)
    str_detect(combined, "manufactur|manurfactur|industrial|factory|production|engineering\\s*firm|steel|chemical|packaging|mineral\\s*exploration|mfg\\s*company") ~ 
      "Manufacturing and Industrial",
    
    # 24. Technology and Software
    str_detect(combined, "software|saas|platform|tech\\s*company|technology|digital|blockchain|\\bai\\b|artificial\\s*intelligence|biotech|fintech\\s*start\\s*up|clean\\s*tech\\s*company") ~ 
      "Technology and Software",
    
    # 25. Media and Communication
    str_detect(combined, "media|press|publication|broadcast|communication|journalism") ~ 
      "Media and Communication",
    
    # 26. Generic Corporate - should come later to catch more specific patterns first
    str_detect(combined, "corporate|company|conglomerate|multinational|holding|plc|gmbh|ltd|llc|inc|limited\\s*company|publicly\\s*listed|privately\\s*held") ~ 
      "Corporate Entities",
    
    # 27. Translation and other service businesses - more specific
    str_detect(combined, "translation\\s*agency") ~ 
      "Miscellaneous and Individual Respondents",
    
    # 28. Individual/Small Business (non-climate)
    str_detect(combined, "entrepreneur|founder|business\\s*owner|self\\s*employ|startup|small\\s*business") & 
      !is_climate_related(combined) ~ 
      "Miscellaneous and Individual Respondents",
    
    # Default catch-all
    TRUE ~ "Miscellaneous and Individual Respondents"
  )
}

# --------------------------------------------------------------------
# Apply Classification
# --------------------------------------------------------------------

# Create classification data frame WITH PROPER ID TRACKING
# CRITICAL: Keep the respondent_id values for joining, but also track the source column name
cls <- tibble(
  # Use consistent internal name 'respondent_id' for processing
  respondent_id = respondent_id,
  # Keep source column name for reference
  .source_id_column = respondent_id_column,
  role_raw  = role_raw,
  role_other = role_other
) %>%
  mutate(
    role_raw_norm   = normalize_text(role_raw),
    role_other_norm = normalize_text(role_other)
  )

# Apply the robust classification function
classification_df <- cls %>%
  mutate(
    # Check if "Other (please specify)" was selected
    needs_harmonization = !is.na(role_raw) & str_detect(coalesce(role_raw_norm, ""), "other.*please.*specify"),
    
    # Apply robust classification using case_when
    preliminary_category = map2_chr(role_raw, role_other, classify_stakeholder_robust),
    
    # Final category starts as preliminary
    final_category_appendix_j = preliminary_category
  ) %>%
  mutate(
    # Ensure no NAs (using case_when for consistency)
    final_category_appendix_j = case_when(
      is.na(final_category_appendix_j) ~ "Miscellaneous and Individual Respondents",
      final_category_appendix_j == "" ~ "Miscellaneous and Individual Respondents",
      TRUE ~ final_category_appendix_j
    )
  )

# --------------------------------------------------------------------
# STANDARDIZE COLUMN NAMES
# --------------------------------------------------------------------
message("\n=== STANDARDIZING COLUMN NAMES ===")

# Rename columns to standard names BEFORE saving
classification_df <- classification_df %>%
  rename(
    # Use standard names from central config
    Role_Raw = role_raw,
    Role_Other_Text = role_other,
    Final_Role_Category = final_category_appendix_j  # Standardize primary column
  ) %>%
  select(-role_raw_norm, -role_other_norm, -.source_id_column)  # Remove temporary processing columns

# Add metadata columns for pipeline tracking
classification_df <- classification_df %>%
  mutate(
    Classification_Stage = "preliminary",
    Classification_Date = Sys.Date(),
    Classification_Version = "4.0"  # Updated version for join fix
  )

message("✓ Standardized column names applied")

# --------------------------------------------------------------------
# Save outputs
# --------------------------------------------------------------------

# Template for manual review (use standardized names)
template <- classification_df %>%
  arrange(desc(needs_harmonization), respondent_id)

dir.create(dirname(out_template), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(out_prelim),   showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(out_audit),    showWarnings = FALSE, recursive = TRUE)

write_csv(template, out_template)
message("Template saved to: ", normalizePath(out_template))

# --------------------------------------------------------------------
# CRITICAL FIX: Join back using the ACTUAL column name from source
# --------------------------------------------------------------------
message("\n=== JOINING CLASSIFICATION TO SOURCE DATA ===")
message("Using source column '", respondent_id_column, "' for join")

# Create temporary version of classification_df with the source column name for joining
join_df <- classification_df %>%
  select(respondent_id, preliminary_category, Final_Role_Category,
         Classification_Stage, Classification_Date, Classification_Version)

# Rename the respondent_id column to match the source data's actual column name
names(join_df)[names(join_df) == "respondent_id"] <- respondent_id_column

# Now perform the join using the actual column name that exists in both dataframes
df_prelim <- df %>%
  left_join(
    join_df,
    by = respondent_id_column  # Use the actual column name from source
  )

# Verify join success by checking for NAs in classification columns
join_na_count <- sum(is.na(df_prelim$Final_Role_Category))
if (join_na_count > 0) {
  warning("WARNING: Join may have failed - ", join_na_count, 
          " records have NA in Final_Role_Category after join!")
  
  # Additional diagnostics
  message("Checking for ID mismatches...")
  source_ids <- df[[respondent_id_column]]
  class_ids <- classification_df$respondent_id
  
  missing_in_class <- setdiff(source_ids, class_ids)
  missing_in_source <- setdiff(class_ids, source_ids)
  
  if (length(missing_in_class) > 0) {
    message("  - ", length(missing_in_class), " IDs from source not in classification")
  }
  if (length(missing_in_source) > 0) {
    message("  - ", length(missing_in_source), " IDs from classification not in source")
  }
} else {
  message("✓ Join successful - all records have classification")
}

# Ensure Final_Role_Category is the primary column name
if ("final_category_appendix_j" %in% names(df_prelim)) {
  df_prelim <- df_prelim %>% select(-final_category_appendix_j)
}

# --------------------------------------------------------------------
# CRITICAL FIX: STRICT PRIVACY VIOLATION CHECK
# --------------------------------------------------------------------
# Check for privacy violations before saving - STRICT enforcement
# This will STOP execution if any PII is detected, preventing data leakage
message("\n=== STRICT PRIVACY CHECK ===")
message("Checking for PII violations with STRICT enforcement...")

# CRITICAL: stop_on_violation = TRUE ensures pipeline halts if PII detected
check_privacy_violations(df_prelim, stop_on_violation = TRUE)

message("✓ Privacy check PASSED - No PII detected")

# Only save if privacy check passes (execution will have stopped if violations found)
write_csv(df_prelim, out_prelim)
message("Preliminary data saved to: ", normalizePath(out_prelim))

# --------------------------------------------------------------------
# Summary Statistics & AUDIT LOG 
# --------------------------------------------------------------------

message("\n=== CLASSIFICATION SUMMARY ===")

# Overall distribution (use standardized column name)
summary_stats <- classification_df %>%
  count(Final_Role_Category, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print(summary_stats, n = 30)

# Save audit log to file with additional metadata
audit_log <- summary_stats %>%
  rename(
    Category = Final_Role_Category,
    Count = n,
    Percentage = percentage
  ) %>%
  mutate(
    Audit_Date = Sys.Date(),
    Total_Records = nrow(classification_df),
    Needs_Harmonization = sum(classification_df$needs_harmonization, na.rm = TRUE),
    Direct_Classification = sum(!classification_df$needs_harmonization, na.rm = TRUE),
    Pipeline_Stage = "Classification",
    Script_Version = "4.0",  # Updated version
    Classification_Method = "case_when",
    Privacy_Check = "STRICT",
    Source_ID_Column = respondent_id_column,  # Track which column was used
    Join_Success_Rate = paste0(round((nrow(df_prelim) - join_na_count) / nrow(df_prelim) * 100, 2), "%")
  )

write_csv(audit_log, out_audit)
message("\nAudit log saved to: ", normalizePath(out_audit))

message("\n=== HARMONIZATION STATISTICS ===")
message("Total responses: ", nrow(classification_df))
message("Needs harmonization: ", sum(classification_df$needs_harmonization, na.rm = TRUE))
message("Direct classification: ", sum(!classification_df$needs_harmonization, na.rm = TRUE))

# Validation check - ensure no NAs in final category (use standardized name)
na_count <- sum(is.na(classification_df$Final_Role_Category))
if (na_count > 0) {
  warning("Found ", na_count, " NA values in Final_Role_Category!")
} else {
  message("\n✓ All records have valid final categories")
}

# Sample of harmonized entries for verification
message("\n=== SAMPLE HARMONIZED ENTRIES ===")
harmonized_sample <- classification_df %>%
  filter(needs_harmonization) %>%
  select(Role_Other_Text, Final_Role_Category) %>%
  slice_sample(n = min(10, sum(classification_df$needs_harmonization, na.rm = TRUE)))

if (nrow(harmonized_sample) > 0) {
  print(harmonized_sample)
} else {
  message("No harmonized entries to display")
}

# Final column name verification
message("\n=== COLUMN NAME VERIFICATION ===")
if ("Final_Role_Category" %in% names(df_prelim)) {
  message("✓ Final_Role_Category column present in output")
} else {
  warning("Final_Role_Category column missing from output!")
}

# Join verification details
message("\n=== JOIN VERIFICATION ===")
message("✓ Source ID column: '", respondent_id_column, "'")
message("✓ Join performed on matching column names")
message("✓ Join success rate: ", round((nrow(df_prelim) - join_na_count) / nrow(df_prelim) * 100, 2), "%")

# Classification method verification
message("\n=== CLASSIFICATION METHOD VERIFICATION ===")
message("✓ Using robust case_when classification logic (v4.0)")
message("✓ Classification rules are now auditable and maintainable")
message("✓ STRICT privacy enforcement enabled - pipeline will halt on PII detection")
message("✓ Data joining bug FIXED - using actual source column names")
message("✓ Using centralized helpers from 00_config.R")

message("\n✓ Classification complete with FIXED joining logic, centralized helpers, standardized column names, and STRICT privacy enforcement!")