#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# ============================================================================
# 02_classify_stakeholders.R
# ============================================================================
# Purpose: Produce stakeholder classifications per Appendix J methodology
# Version: 7.0 - Added ANALYSIS_ROLE column for quota-aware downstream analysis
# 
# Key Improvements in v7.0:
#   - NEW: Added ANALYSIS_ROLE column that prefers quota_target_category if present
#   - NEW: Preserves existing quota_target_category column (never overwrites)
#   - NEW: Provides consistent interface for downstream analysis scripts
#   - MAINTAINED: All v6.0 fixes and improvements
#   - MAINTAINED: Classification logic ordering - specific rules before generic
#   - MAINTAINED: Column naming consistency (Final_Role_Category throughout)
#   - MAINTAINED: All write_csv calls include na = "" parameter
#   - MAINTAINED: Uses centralized helpers from 00_config.R
#   - MAINTAINED: Comprehensive join validation with detailed diagnostics
#   - MAINTAINED: STRICT privacy violation checking
#
# Important Note on quota_target_category:
#   - This column is created by get_exact_1307.R on the final subset only
#   - This script NEVER creates or overwrites quota_target_category
#   - ANALYSIS_ROLE provides a consistent interface for downstream scripts
#
# Outputs:
#   - docs/appendix_j_classification_template.csv
#   - data/survey_responses_anonymized_preliminary.csv (with ANALYSIS_ROLE)
#   - output/classification_audit.csv
# ============================================================================

# --------------------------------------------------------------------
# 1. PACKAGE LOADING
# --------------------------------------------------------------------
# Load all required packages with explicit namespace declaration
suppressPackageStartupMessages({
  library(tidyverse)   # Core tidyverse packages
  library(stringr)     # String manipulation
  library(rlang)       # Explicitly load rlang for sym() and other functions
})

# --------------------------------------------------------------------
# 2. CONFIGURATION AND VALIDATION
# --------------------------------------------------------------------
# Source central configuration and helpers
# NO LOCAL FUNCTION DUPLICATION - using centralized versions
source("R/00_config.R")

# Validate pipeline state
check_deprecated()           # Warn about old/deprecated files
validate_stage("classify")   # Ensure prerequisites are met

# Initialize processing stage
message("=" %.% rep("=", 50))
message("=== STAKEHOLDER CLASSIFICATION (APPENDIX J) ===")
message("=" %.% rep("=", 50))
set_stage("Classification")

# --------------------------------------------------------------------
# 3. PATH CONFIGURATION
# --------------------------------------------------------------------
# Use centralized paths from config
in_basic     <- PATHS$basic_anon
out_template <- PATHS$classification_template
out_prelim   <- PATHS$preliminary_classified
out_audit    <- PATHS$classification_audit

# --------------------------------------------------------------------
# 4. DATA LOADING
# --------------------------------------------------------------------
message("\n=== LOADING DATA ===")

# Verify input file exists
if (!file.exists(in_basic)) {
  stop("Input file not found: ", in_basic)
}

# Load anonymized survey data
df <- suppressMessages(read_csv(in_basic, show_col_types = FALSE))
message("✓ Loaded ", nrow(df), " records from: ", basename(in_basic))

# Check for existing quota_target_category column
has_quota_column <- "quota_target_category" %in% names(df)
if (has_quota_column) {
  message("ℹ Detected existing quota_target_category column - will be preserved")
  quota_count <- sum(!is.na(df$quota_target_category))
  message("  ", quota_count, " records have quota_target_category values")
}

# --------------------------------------------------------------------
# 5. COLUMN IDENTIFICATION
# --------------------------------------------------------------------
message("\n=== IDENTIFYING REQUIRED COLUMNS ===")

# CRITICAL: Track actual column names from source data
# This prevents silent data loss from mismatched column names during joins

# Find respondent ID column (using centralized helper from 00_config.R)
respondent_id_column <- find_column(df, c(
  "respondent_id", "ResponseId", "_recordId", 
  "response_id", "ID", "id"
))

if (is.null(respondent_id_column)) {
  stop("CRITICAL ERROR: No respondent ID column found in source data.\n",
       "Expected one of: respondent_id, ResponseId, _recordId, ",
       "response_id, ID, id")
}
message("✓ Found respondent ID column: '", respondent_id_column, "'")

# Find role columns (using centralized helpers from 00_config.R)
role_raw_column <- find_column(df, c(
  "role_raw", "Q2.1",
  "Which of the following best describes your role? (Please select the most appropriate option) - Selected Choice"
))

role_other_column <- find_column(df, c(
  "role_other", "Q2.1_12_TEXT",
  "Which of the following best describes your role? (Please select the most appropriate option) - Other (please specify)  - Text"
))

# Extract column values with safety checks
respondent_id <- if (!is.null(respondent_id_column)) {
  df[[respondent_id_column]]
} else {
  stop("Missing respondent ID column")
}

role_raw <- if (!is.null(role_raw_column)) {
  df[[role_raw_column]]
} else {
  rep(NA_character_, nrow(df))
}

role_other <- if (!is.null(role_other_column)) {
  df[[role_other_column]]
} else {
  rep(NA_character_, nrow(df))
}

# Log column mapping for audit trail
message("\n=== COLUMN MAPPING ===")
message("Respondent ID: '", respondent_id_column, "' -> respondent_id")
if (!is.null(role_raw_column)) {
  message("Role Raw: '", role_raw_column, "' -> role_raw")
}
if (!is.null(role_other_column)) {
  message("Role Other: '", role_other_column, "' -> role_other")
}

# --------------------------------------------------------------------
# 6. CLASSIFICATION HELPER FUNCTIONS
# --------------------------------------------------------------------

#' Normalize text for consistent pattern matching
#' @param x Character vector to normalize
#' @return Normalized character vector
normalize_text <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%        # Collapse whitespace
    str_trim() %>%                          # Remove leading/trailing space
    tolower() %>%                           # Convert to lowercase
    str_replace_all("[^a-z0-9 ]", " ") %>% # Remove special characters
    str_squish()                            # Final whitespace cleanup
}

#' Check if text contains climate-related terms
#' @param text Character vector to check
#' @return Logical vector indicating climate-related content
is_climate_related <- function(text) {
  climate_terms <- paste0(
    "climate|clean|green|sustain|carbon|emission|renewable|",
    "solar|wind|battery|storage|hydrogen|bio|ccus|adapt|",
    "energy|electric|environmental|esg"
  )
  str_detect(text, climate_terms)
}

#' Check if text contains entrepreneur-related terms
#' @param text Character vector to check
#' @return Logical vector indicating entrepreneur-related content
is_entrepreneur_related <- function(text) {
  entrepreneur_terms <- paste0(
    "entrepreneur|founder|co[- ]?founder|ceo|cto|",
    "startup|venture\\s*builder|launching|started"
  )
  str_detect(text, entrepreneur_terms)
}

#' Classify stakeholder based on role text using robust case_when logic
#' @param raw_text Raw role text from survey
#' @param other_text Other/specify text from survey
#' @return Character string with stakeholder classification
#' 
#' CRITICAL FIX IN v6.0: Reordered classification logic to ensure
#' specific rules are evaluated before generic ones, preventing
#' misclassification of specialized roles like "legal consultant"
classify_stakeholder_robust <- function(raw_text, other_text) {
  # Combine and normalize both text fields
  combined <- paste(
    ifelse(is.na(raw_text), "", raw_text),
    ifelse(is.na(other_text), "", other_text),
    sep = " "
  ) %>% normalize_text()
  
  # Return default for empty text
  if (nchar(combined) == 0) {
    return("Miscellaneous and Individual Respondents")
  }
  
  # Apply classification rules in priority order
  # case_when ensures first match wins (mutual exclusivity)
  # FIX: Reordered to evaluate specific before generic
  case_when(
    # === HIGHEST PRIORITY: VERY SPECIFIC COMPOUND ROLES ===
    # These must come first to prevent partial matches
    str_detect(combined, "corporate\\s*venture") & 
      !str_detect(combined, "law|legal|consult") ~ 
      "Corporate Venture Arm",
    
    str_detect(combined, "angel\\s*investor") ~ 
      "Angel Investor",
    
    str_detect(combined, "limited\\s*partner|\\blp\\b") ~ 
      "Limited Partner",
    
    str_detect(combined, "family\\s*office|single\\s*family\\s*office") ~ 
      "Family Office",
    
    str_detect(combined, "high\\s*net|hnwi|ultra\\s*high\\s*net|uhnw|retired.*wealthy") ~ 
      "High Net-Worth Individual",
    
    str_detect(combined, paste0(
      "esg\\s*investor|impact\\s*invest|social\\s*impact\\s*invest|",
      "sustainability\\s*invest|impact\\s*first"
    )) ~ "ESG Investor",
    
    # === CLIMATE-SPECIFIC ENTREPRENEURSHIP (before general entrepreneur) ===
    is_entrepreneur_related(combined) & is_climate_related(combined) ~ 
      "Entrepreneur in Climate Technology",
    
    # === SPECIFIC PROFESSIONAL SERVICES (before general consulting) ===
    # FIX: Legal services MUST come before general consulting
    str_detect(combined, paste0(
      "law\\s*firm|lawyer|attorney|solicitor|legal\\s*consultant|",
      "legal\\s*services|legal\\s*counsel|barrister|patent\\s*attorney|",
      "law\\s*office"
    )) ~ "Legal Services",
    
    # === SPECIFIC INDUSTRIES (before general corporate) ===
    str_detect(combined, paste0(
      "real\\s*estate|property\\s*developer|property\\s*investor|",
      "reit|commercial\\s*property|industrial\\s*real\\s*estate|",
      "property\\s*management"
    )) ~ "Real Estate and Property",
    
    str_detect(combined, paste0(
      "utility|utlity|grid\\s*operator|power\\s*producer|",
      "energy\\s*service|energy\\s*provider|transmission|",
      "ipp|solar\\s*develop|wind\\s*develop|infrastructure\\s*fund|",
      "energy\\s*(service|firm|sector)|electric\\s*cooperative|",
      "gas\\s*transmission|climate\\s*adaptive\\s*infrastructure"
    )) ~ "Energy and Infrastructure",
    
    str_detect(combined, paste0(
      "manufactur|manurfactur|industrial|factory|production|",
      "engineering\\s*firm|steel|chemical|packaging|",
      "mineral\\s*exploration|mfg\\s*company"
    )) ~ "Manufacturing and Industrial",
    
    str_detect(combined, paste0(
      "software|saas|platform|biotech|tech\\s*company|",
      "technology\\s*firm|it\\s*company|technology|digital|",
      "blockchain|\\bai\\b|artificial\\s*intelligence|",
      "fintech\\s*start\\s*up|clean\\s*tech\\s*company"
    )) ~ "Technology and Software",
    
    str_detect(combined, paste0(
      "media|press|publication|journalist|news|broadcast|",
      "communication|journalism"
    )) ~ "Media and Communication",
    
    # === INSTITUTIONAL INVESTORS (specific types) ===
    str_detect(combined, "venture\\s*capital|\\bvc\\b(?!\\w)|venture\\s*firm|venture\\s*fund") & 
      !str_detect(combined, "corporate|law") ~ 
      "Venture Capital Firm",
    
    str_detect(combined, "private\\s*equity|\\bpe\\b") & 
      !str_detect(combined, "backed|owned") ~ 
      "Private Equity Firm",
    
    # === GOVERNMENT & PUBLIC SECTOR ===
    str_detect(combined, paste0(
      "dfi\\b|development\\s*finance\\s*institution|",
      "multilateral\\s*(development|organization|bank)|bilateral|",
      "government\\s+(agency|funding|organization)|ministry|",
      "federal\\s+(agency|government)|state\\s+(agency|government)|",
      "municipal|public\\s*sector|public\\s*pension|\\bnhs\\b|",
      "world\\s*bank|ifc"
    )) ~ "Government Funding Agency",
    
    # === ACADEMIC & RESEARCH ===
    str_detect(combined, paste0(
      "university|academic|professor|researcher|",
      "research\\s*(center|institute|institution)|\\blab\\b|",
      "think\\s*tank"
    )) ~ "Academic or Research Institution",
    
    # === PHILANTHROPIC (must come before general nonprofit) ===
    str_detect(combined, paste0(
      "foundation|philanthrop|charitable\\s*foundation|",
      "grantmaker|endowment|donor|",
      "giving\\s*fund|community\\s*foundation"
    )) & !str_detect(combined, "nhs|trust\\s*beneficiary") ~ 
      "Philanthropic Organization",
    
    # === NONPROFIT (after philanthropic) ===
    str_detect(combined, paste0(
      "nonprofit|non[- ]?profit|ngo|not-for-profit|501c|",
      "charity|civil\\s*society|association"
    )) & !str_detect(combined, "law|consult") ~ 
      "Nonprofit Organization",
    
    # === BROAD FINANCIAL SERVICES ===
    str_detect(combined, "hedge\\s*fund") ~ 
      "Investment and Financial Services",
    
    str_detect(combined, "sovereign\\s*wealth") ~ 
      "Investment and Financial Services",
    
    str_detect(combined, "pension\\s*fund") ~ 
      "Investment and Financial Services",
    
    str_detect(combined, paste0(
      "investment\\s*bank|asset\\s*manag|wealth\\s*manag|",
      "fund\\s*manag|insurance|private\\s*bank|debt\\s*fund|",
      "financial\\s*(advisor|advisory|holding|infrastructure)|",
      "fintech|alternative\\s*asset|permanent\\s*capital|",
      "securitized.*projects|mutual\\s*fund"
    )) ~ "Investment and Financial Services",
    
    # === GENERAL BUSINESS SERVICES (after all specific services) ===
    # FIX: This now comes AFTER legal services
    str_detect(combined, paste0(
      "consult|advisory|strategist|business\\s*service|",
      "strategy|m\\s*&\\s*a|management\\s*consult|",
      "consultant\\s*agency|venture\\s*studio"
    )) ~ "Business Consulting and Advisory",
    
    # === GENERAL CORPORATE (after all specific industries) ===
    str_detect(combined, paste0(
      "corporate|company|corporation|multinational|",
      "holding\\s*company|conglomerate|",
      "plc|gmbh|ltd|llc|inc|limited\\s*company|",
      "publicly\\s*listed|privately\\s*held"
    )) ~ "Corporate Entities",
    
    # === FALLBACK CLASSIFICATIONS ===
    # Translation services
    str_detect(combined, "translation\\s*agency") ~ 
      "Miscellaneous and Individual Respondents",
    
    # Individual/Small Business (non-climate)
    str_detect(combined, paste0(
      "entrepreneur|founder|business\\s*owner|",
      "self\\s*employ|startup|small\\s*business"
    )) & !is_climate_related(combined) ~ 
      "Miscellaneous and Individual Respondents",
    
    str_detect(combined, "individual|personal|private\\s*individual|retail\\s*investor") ~ 
      "Miscellaneous and Individual Respondents",
    
    # Default catch-all
    TRUE ~ "Miscellaneous and Individual Respondents"
  )
}

# --------------------------------------------------------------------
# 7. APPLY CLASSIFICATION
# --------------------------------------------------------------------
message("\n=== APPLYING CLASSIFICATION ===")

# Create classification dataframe with proper ID tracking
# FIX: Using consistent column naming from the start
classification_df <- tibble(
  # Internal consistent naming
  respondent_id = respondent_id,
  # Track source column for joining
  .source_id_column = respondent_id_column,
  # Role data
  role_raw = role_raw,
  role_other = role_other
) %>%
  mutate(
    # Normalize text for processing
    role_raw_norm = normalize_text(role_raw),
    role_other_norm = normalize_text(role_other),
    
    # Check if "Other" was selected
    needs_harmonization = !is.na(role_raw) & 
      str_detect(coalesce(role_raw_norm, ""), "other.*please.*specify"),
    
    # Apply classification
    preliminary_category = map2_chr(
      role_raw, role_other, 
      classify_stakeholder_robust
    ),
    
    # Set final category - FIX: Using Final_Role_Category consistently
    Final_Role_Category = coalesce(
      preliminary_category, 
      "Miscellaneous and Individual Respondents"
    )
  )

# Ensure no empty or NA categories
classification_df <- classification_df %>%
  mutate(
    Final_Role_Category = case_when(
      is.na(Final_Role_Category) ~ 
        "Miscellaneous and Individual Respondents",
      Final_Role_Category == "" ~ 
        "Miscellaneous and Individual Respondents",
      TRUE ~ Final_Role_Category
    )
  )

message("✓ Classification applied to ", nrow(classification_df), " records")

# --------------------------------------------------------------------
# 8. STANDARDIZE COLUMN NAMES
# --------------------------------------------------------------------
message("\n=== STANDARDIZING COLUMN NAMES ===")

# Apply standard column names for output consistency
classification_df <- classification_df %>%
  rename(
    Role_Raw = role_raw,
    Role_Other_Text = role_other
    # Note: Final_Role_Category already named correctly
  ) %>%
  select(
    -role_raw_norm, 
    -role_other_norm, 
    -.source_id_column
  )

# Add metadata for pipeline tracking
classification_df <- classification_df %>%
  mutate(
    Classification_Stage = "preliminary",
    Classification_Date = Sys.Date(),
    Classification_Version = "7.0",
    Classification_Method = case_when(
      !is.na(Role_Other_Text) & nchar(Role_Other_Text) > 0 ~ "other_text",
      !is.na(Role_Raw) ~ "predefined",
      TRUE ~ "missing"
    )
  )

message("✓ Column names standardized")

# --------------------------------------------------------------------
# 9. CREATE OUTPUTS
# --------------------------------------------------------------------
message("\n=== PREPARING OUTPUTS ===")

# Create output directories
for (path in c(out_template, out_prelim, out_audit)) {
  dir.create(dirname(path), showWarnings = FALSE, recursive = TRUE)
}

# Save classification template for manual review
# FIX: Added na = "" parameter
template <- classification_df %>%
  arrange(desc(needs_harmonization), respondent_id)

write_csv(template, out_template, na = "")
message("✓ Template saved: ", normalizePath(out_template))

# --------------------------------------------------------------------
# 10. JOIN CLASSIFICATION TO SOURCE DATA
# --------------------------------------------------------------------
message("\n=== JOINING CLASSIFICATION TO SOURCE DATA ===")
message("Using source column '", respondent_id_column, "' for join")

# CRITICAL FIX: Create join dataframe with source column name
join_df <- classification_df %>%
  select(
    respondent_id, 
    preliminary_category, 
    Final_Role_Category,
    Classification_Stage, 
    Classification_Date, 
    Classification_Version,
    Classification_Method
  )

# Rename to match source data's actual column name
names(join_df)[names(join_df) == "respondent_id"] <- respondent_id_column

# Perform join using actual column name from source
df_prelim <- df %>%
  left_join(join_df, by = respondent_id_column)

# FIX: Comprehensive join validation
join_na_count <- sum(is.na(df_prelim$Final_Role_Category))

if (join_na_count > 0) {
  warning(
    "WARNING: Join may have issues - ", join_na_count, 
    " records have NA in Final_Role_Category after join!"
  )
  warning("This may indicate a join key mismatch or missing source data")
  
  # Diagnostic information
  message("Performing join diagnostics...")
  source_ids <- df[[respondent_id_column]]
  class_ids <- classification_df$respondent_id
  
  missing_in_class <- setdiff(source_ids, class_ids)
  missing_in_source <- setdiff(class_ids, source_ids)
  
  if (length(missing_in_class) > 0) {
    message("  - ", length(missing_in_class), 
            " IDs from source not in classification")
    
    # Log the problematic IDs
    missing_ids <- df_prelim %>%
      filter(is.na(Final_Role_Category)) %>%
      pull(!!sym(respondent_id_column))
    
    if (length(missing_ids) <= 10) {
      warning("Missing classification IDs: ", paste(missing_ids, collapse=', '))
    } else {
      warning("First 10 missing classification IDs: ", 
              paste(head(missing_ids, 10), collapse=', '))
    }
  }
  
  if (length(missing_in_source) > 0) {
    message("  - ", length(missing_in_source), 
            " IDs from classification not in source")
  }
} else {
  message("✓ Join successful - all records classified")
}

# --------------------------------------------------------------------
# 11. CREATE ANALYSIS ROLE COLUMN
# --------------------------------------------------------------------
message("\n=== CREATING ANALYSIS ROLE COLUMN ===")

# CRITICAL v7.0 ADDITION: Create ANALYSIS_ROLE that prefers quota_target_category
# This provides a consistent interface for downstream scripts
# quota_target_category is created by get_exact_1307.R on the final subset only

# DO NOT CREATE OR OVERWRITE quota_target_category HERE
# Only use it if it already exists in the data

if ("quota_target_category" %in% names(df_prelim)) {
  # Use existing quota_target_category as primary source
  df_prelim <- df_prelim %>%
    mutate(
      ANALYSIS_ROLE = coalesce(quota_target_category, Final_Role_Category),
      ANALYSIS_ROLE_SOURCE = case_when(
        !is.na(quota_target_category) ~ "quota_matched",
        !is.na(Final_Role_Category) ~ "harmonized",
        TRUE ~ "unclassified"
      )
    )
  
  # Report statistics
  quota_used <- sum(df_prelim$ANALYSIS_ROLE_SOURCE == "quota_matched")
  harmonized_used <- sum(df_prelim$ANALYSIS_ROLE_SOURCE == "harmonized")
  
  message("✓ ANALYSIS_ROLE created:")
  message("  - ", quota_used, " records using quota_target_category")
  message("  - ", harmonized_used, " records using Final_Role_Category")
  message("  - Column preference: quota_target_category > Final_Role_Category")
  
} else {
  # No quota column exists - use Final_Role_Category for all records
  df_prelim <- df_prelim %>%
    mutate(
      ANALYSIS_ROLE = Final_Role_Category,
      ANALYSIS_ROLE_SOURCE = case_when(
        !is.na(Final_Role_Category) ~ "harmonized",
        TRUE ~ "unclassified"
      )
    )
  
  message("✓ ANALYSIS_ROLE created using Final_Role_Category")
  message("  - No quota_target_category column found")
  message("  - All ", nrow(df_prelim), " records use Final_Role_Category")
}

# Validate ANALYSIS_ROLE completeness
analysis_na_count <- sum(is.na(df_prelim$ANALYSIS_ROLE))
if (analysis_na_count > 0) {
  warning("WARNING: ", analysis_na_count, " records have NA in ANALYSIS_ROLE!")
} else {
  message("✓ All records have valid ANALYSIS_ROLE values")
}

# --------------------------------------------------------------------
# 12. PRIVACY VALIDATION
# --------------------------------------------------------------------
message("\n=== PRIVACY VALIDATION ===")
message("Running STRICT privacy check...")

# CRITICAL: Strict enforcement - stops pipeline if PII detected
check_privacy_violations(df_prelim, stop_on_violation = TRUE)
message("✓ Privacy check PASSED - No PII detected")

# Save preliminary classified data with ANALYSIS_ROLE
# FIX: Added na = "" parameter
write_csv(df_prelim, out_prelim, na = "")
message("✓ Preliminary data saved: ", normalizePath(out_prelim))
message("  - Contains Final_Role_Category (harmonized)")
if (has_quota_column) {
  message("  - Preserves original quota_target_category")
}
message("  - Contains ANALYSIS_ROLE for downstream scripts")

# --------------------------------------------------------------------
# 13. GENERATE SUMMARY STATISTICS
# --------------------------------------------------------------------
message("\n=== CLASSIFICATION SUMMARY ===")

# Calculate distribution statistics for Final_Role_Category
summary_stats <- classification_df %>%
  count(Final_Role_Category, sort = TRUE) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

# Display summary
message("\nFinal_Role_Category distribution:")
print(summary_stats, n = 30)

# If ANALYSIS_ROLE differs from Final_Role_Category, show that distribution too
if ("quota_target_category" %in% names(df_prelim)) {
  analysis_summary <- df_prelim %>%
    count(ANALYSIS_ROLE, ANALYSIS_ROLE_SOURCE, sort = TRUE) %>%
    mutate(
      percentage = round(n / sum(n) * 100, 1)
    )
  
  message("\nANALYSIS_ROLE distribution (for downstream analysis):")
  print(analysis_summary, n = 30)
}

# --------------------------------------------------------------------
# 14. CREATE AUDIT LOG
# --------------------------------------------------------------------
message("\n=== CREATING AUDIT LOG ===")

# Build comprehensive audit record
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
    Script_Version = "7.0",
    Classification_Method = "case_when_ordered",
    Privacy_Check = "STRICT",
    Source_ID_Column = respondent_id_column,
    Join_Success_Rate = paste0(
      round((nrow(df_prelim) - join_na_count) / nrow(df_prelim) * 100, 2), 
      "%"
    ),
    Missing_Classifications = join_na_count,
    Has_Quota_Column = has_quota_column,
    Analysis_Role_Created = TRUE,
    Analysis_Role_NA_Count = analysis_na_count
  )

# FIX: Added na = "" parameter
write_csv(audit_log, out_audit, na = "")
message("✓ Audit log saved: ", normalizePath(out_audit))

# --------------------------------------------------------------------
# 15. FINAL STATISTICS AND VERIFICATION
# --------------------------------------------------------------------
message("\n=== HARMONIZATION STATISTICS ===")
message("Total responses: ", nrow(classification_df))
message("Needs harmonization: ", 
        sum(classification_df$needs_harmonization, na.rm = TRUE))
message("Direct classification: ", 
        sum(!classification_df$needs_harmonization, na.rm = TRUE))

# Verify no NAs in final categories
na_count <- sum(is.na(classification_df$Final_Role_Category))
if (na_count > 0) {
  warning("Found ", na_count, " NA values in Final_Role_Category!")
} else {
  message("✓ All records have valid final categories")
}

# Sample harmonized entries
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

# Final verification summary
message("\n=== VERIFICATION SUMMARY ===")
message("✓ Classification logic ordering fixed (specific before generic)")
message("✓ Column naming consistent (Final_Role_Category throughout)")
message("✓ All write_csv calls include na = '' parameter")
message("✓ Using centralized helpers from 00_config.R")
message("✓ Comprehensive join validation implemented")
message("✓ Explicit rlang namespace loaded")
message("✓ Data joining using actual source column names")
message("✓ STRICT privacy enforcement active")
message("✓ Classification logic using robust case_when")
message("✓ Source ID column: '", respondent_id_column, "'")
message("✓ Join success rate: ", 
        round((nrow(df_prelim) - join_na_count) / nrow(df_prelim) * 100, 2), "%")

# v7.0 specific verifications
message("\n=== VERSION 7.0 ADDITIONS ===")
message("✓ ANALYSIS_ROLE column created for downstream scripts")
message("✓ Preserves existing quota_target_category (never overwrites)")
message("✓ ANALYSIS_ROLE prefers quota_target_category when present")
message("✓ ANALYSIS_ROLE_SOURCE tracks data provenance")
if (has_quota_column) {
  message("✓ Successfully preserved ", quota_count, " quota-matched records")
}

message("\n" %.% rep("=", 60))
message("✓ CLASSIFICATION COMPLETE - Version 7.0")
message("  ANALYSIS_ROLE provides consistent interface for downstream")
message("  All functionality from v6.0 preserved")
message(rep("=", 60))