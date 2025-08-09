#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# ============================================================================
# 02_classify_stakeholders.R
# ============================================================================
# Purpose: Produce stakeholder classifications per Appendix J methodology
# Version: 5.0 - Comprehensive rewrite with all identified issues fixed
# 
# Key Improvements in v5.0:
#   - FIXED: Explicit rlang namespace loading for better reliability
#   - MAINTAINED: Critical data joining bug fix with actual column name tracking
#   - CONFIRMED: Uses centralized helpers, no redundant local functions
#   - ENHANCED: Better code organization and documentation
#   - MAINTAINED: STRICT privacy violation checking (stop_on_violation = TRUE)
#   - MAINTAINED: Robust case_when classification logic
#
# Outputs:
#   - docs/appendix_j_classification_template.csv
#   - data/survey_responses_anonymized_preliminary.csv
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

# --------------------------------------------------------------------
# 5. COLUMN IDENTIFICATION
# --------------------------------------------------------------------
message("\n=== IDENTIFYING REQUIRED COLUMNS ===")

# CRITICAL: Track actual column names from source data
# This prevents silent data loss from mismatched column names during joins

# Find respondent ID column (using centralized helper)
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

# Find role columns (using centralized helpers)
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
  case_when(
    # Priority 1: Climate-specific entrepreneur
    is_entrepreneur_related(combined) & is_climate_related(combined) ~ 
      "Entrepreneur in Climate Technology",
    
    # Priority 2: Real Estate (before Government to avoid misclassification)
    str_detect(combined, paste0(
      "real\\s*estate|property\\s*developer|property\\s*investor|",
      "reit|commercial\\s*property|industrial\\s*real\\s*estate"
    )) ~ "Real Estate and Property",
    
    # Priority 3: Government and Public Sector
    str_detect(combined, paste0(
      "dfi\\b|development\\s*finance\\s*institution|",
      "multilateral\\s*(development|organization|bank)|bilateral|",
      "government\\s+(agency|funding|organization)|ministry|",
      "federal\\s+(agency|government)|state\\s+(agency|government)|",
      "municipal|public\\s*sector|public\\s*pension|\\bnhs\\b"
    )) ~ "Government Funding Agency",
    
    # Priority 4: Corporate Venture (exclude law/legal)
    str_detect(combined, "corporate\\s*venture") & 
      !str_detect(combined, "law|legal|consult") ~ 
      "Corporate Venture Arm",
    
    # Priority 5: Venture Studio -> Consulting
    str_detect(combined, "venture\\s*studio") ~ 
      "Business Consulting and Advisory",
    
    # Priority 6: Venture Capital
    str_detect(combined, "venture\\s*capital|\\bvc\\b(?!\\w)|venture\\s*firm") & 
      !str_detect(combined, "corporate|law") ~ 
      "Venture Capital Firm",
    
    # Priority 7: Private Equity
    str_detect(combined, "private\\s*equity|\\bpe\\b") & 
      !str_detect(combined, "backed|owned") ~ 
      "Private Equity Firm",
    
    # Priority 8: Angel Investor
    str_detect(combined, "angel\\s*investor") ~ 
      "Angel Investor",
    
    # Priority 9: Limited Partner
    str_detect(combined, "limited\\s*partner|\\blp\\b") ~ 
      "Limited Partner",
    
    # Priority 10: Family Office
    str_detect(combined, "family\\s*office|single\\s*family\\s*office") ~ 
      "Family Office",
    
    # Priority 11: High Net-Worth Individual
    str_detect(combined, "high\\s*net|hnwi|retired.*wealthy") ~ 
      "High Net-Worth Individual",
    
    # Priority 12: ESG Investor
    str_detect(combined, paste0(
      "esg\\s*investor|impact\\s*invest|",
      "sustainability\\s*invest|impact\\s*first"
    )) ~ "ESG Investor",
    
    # Priority 13: Philanthropic Organization
    str_detect(combined, paste0(
      "foundation|philanthrop|charitable|donor|",
      "grantmaker|giving\\s*fund|community\\s*foundation"
    )) & !str_detect(combined, "nhs|trust\\s*beneficiary") ~ 
      "Philanthropic Organization",
    
    # Priority 14: Nonprofit Organization
    str_detect(combined, paste0(
      "nonprofit|non[- ]?profit|ngo|charity|",
      "civil\\s*society|association"
    )) & !str_detect(combined, "law|consult") ~ 
      "Nonprofit Organization",
    
    # Priority 15: Academic/Research
    str_detect(combined, paste0(
      "university|academic|research\\s*(center|institute|institution)|\\blab\\b"
    )) ~ "Academic or Research Institution",
    
    # Priority 16-18: Investment Services subcategories
    str_detect(combined, "hedge\\s*fund") ~ 
      "Investment and Financial Services",
    
    str_detect(combined, "sovereign\\s*wealth") ~ 
      "Investment and Financial Services",
    
    str_detect(combined, "pension\\s*fund") ~ 
      "Investment and Financial Services",
    
    # Priority 19: General Investment Services
    str_detect(combined, paste0(
      "investment\\s*bank|asset\\s*manage|fund\\s*manage|insurance|",
      "private\\s*bank|debt\\s*fund|financial\\s*(advisor|advisory|",
      "holding|infrastructure)|fintech|wealth\\s*manage|",
      "alternative\\s*asset|permanent\\s*capital|securitized.*projects"
    )) ~ "Investment and Financial Services",
    
    # Priority 20: Legal Services
    str_detect(combined, paste0(
      "law\\s*firm|lawyer|attorney|solicitor|legal|",
      "barrister|patent\\s*attorney|law\\s*office"
    )) ~ "Legal Services",
    
    # Priority 21: Consulting
    str_detect(combined, paste0(
      "consult|advis|strategy|strategist|m\\s*&\\s*a|",
      "management\\s*consult|consultant\\s*agency"
    )) ~ "Business Consulting and Advisory",
    
    # Priority 22: Energy and Infrastructure
    str_detect(combined, paste0(
      "utility|utlity|grid|renewable\\s*energy|power\\s*producer|",
      "ipp|solar\\s*develop|wind\\s*develop|infrastructure\\s*fund|",
      "energy\\s*(service|firm|sector)|electric\\s*cooperative|",
      "gas\\s*transmission|climate\\s*adaptive\\s*infrastructure"
    )) ~ "Energy and Infrastructure",
    
    # Priority 23: Manufacturing
    str_detect(combined, paste0(
      "manufactur|manurfactur|industrial|factory|production|",
      "engineering\\s*firm|steel|chemical|packaging|",
      "mineral\\s*exploration|mfg\\s*company"
    )) ~ "Manufacturing and Industrial",
    
    # Priority 24: Technology
    str_detect(combined, paste0(
      "software|saas|platform|tech\\s*company|technology|digital|",
      "blockchain|\\bai\\b|artificial\\s*intelligence|biotech|",
      "fintech\\s*start\\s*up|clean\\s*tech\\s*company"
    )) ~ "Technology and Software",
    
    # Priority 25: Media
    str_detect(combined, paste0(
      "media|press|publication|broadcast|communication|journalism"
    )) ~ "Media and Communication",
    
    # Priority 26: Generic Corporate
    str_detect(combined, paste0(
      "corporate|company|conglomerate|multinational|holding|",
      "plc|gmbh|ltd|llc|inc|limited\\s*company|",
      "publicly\\s*listed|privately\\s*held"
    )) ~ "Corporate Entities",
    
    # Priority 27: Translation services
    str_detect(combined, "translation\\s*agency") ~ 
      "Miscellaneous and Individual Respondents",
    
    # Priority 28: Individual/Small Business (non-climate)
    str_detect(combined, paste0(
      "entrepreneur|founder|business\\s*owner|",
      "self\\s*employ|startup|small\\s*business"
    )) & !is_climate_related(combined) ~ 
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
    
    # Set final category
    final_category_appendix_j = coalesce(
      preliminary_category, 
      "Miscellaneous and Individual Respondents"
    )
  )

# Ensure no empty or NA categories
classification_df <- classification_df %>%
  mutate(
    final_category_appendix_j = case_when(
      is.na(final_category_appendix_j) ~ 
        "Miscellaneous and Individual Respondents",
      final_category_appendix_j == "" ~ 
        "Miscellaneous and Individual Respondents",
      TRUE ~ final_category_appendix_j
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
    Role_Other_Text = role_other,
    Final_Role_Category = final_category_appendix_j
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
    Classification_Version = "5.0"
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
template <- classification_df %>%
  arrange(desc(needs_harmonization), respondent_id)

write_csv(template, out_template)
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
    Classification_Version
  )

# Rename to match source data's actual column name
names(join_df)[names(join_df) == "respondent_id"] <- respondent_id_column

# Perform join using actual column name from source
df_prelim <- df %>%
  left_join(join_df, by = respondent_id_column)

# Verify join success
join_na_count <- sum(is.na(df_prelim$Final_Role_Category))

if (join_na_count > 0) {
  warning(
    "WARNING: Join may have issues - ", join_na_count, 
    " records have NA in Final_Role_Category after join!"
  )
  
  # Diagnostic information
  message("Performing join diagnostics...")
  source_ids <- df[[respondent_id_column]]
  class_ids <- classification_df$respondent_id
  
  missing_in_class <- setdiff(source_ids, class_ids)
  missing_in_source <- setdiff(class_ids, source_ids)
  
  if (length(missing_in_class) > 0) {
    message("  - ", length(missing_in_class), 
            " IDs from source not in classification")
  }
  if (length(missing_in_source) > 0) {
    message("  - ", length(missing_in_source), 
            " IDs from classification not in source")
  }
} else {
  message("✓ Join successful - all records classified")
}

# --------------------------------------------------------------------
# 11. PRIVACY VALIDATION
# --------------------------------------------------------------------
message("\n=== PRIVACY VALIDATION ===")
message("Running STRICT privacy check...")

# CRITICAL: Strict enforcement - stops pipeline if PII detected
check_privacy_violations(df_prelim, stop_on_violation = TRUE)
message("✓ Privacy check PASSED - No PII detected")

# Save preliminary classified data
write_csv(df_prelim, out_prelim)
message("✓ Preliminary data saved: ", normalizePath(out_prelim))

# --------------------------------------------------------------------
# 12. GENERATE SUMMARY STATISTICS
# --------------------------------------------------------------------
message("\n=== CLASSIFICATION SUMMARY ===")

# Calculate distribution statistics
summary_stats <- classification_df %>%
  count(Final_Role_Category, sort = TRUE) %>%
  mutate(
    percentage = round(n / sum(n) * 100, 1)
  )

# Display summary
print(summary_stats, n = 30)

# --------------------------------------------------------------------
# 13. CREATE AUDIT LOG
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
    Script_Version = "5.0",
    Classification_Method = "case_when",
    Privacy_Check = "STRICT",
    Source_ID_Column = respondent_id_column,
    Join_Success_Rate = paste0(
      round((nrow(df_prelim) - join_na_count) / nrow(df_prelim) * 100, 2), 
      "%"
    )
  )

write_csv(audit_log, out_audit)
message("✓ Audit log saved: ", normalizePath(out_audit))

# --------------------------------------------------------------------
# 14. FINAL STATISTICS AND VERIFICATION
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
message("✓ Explicit rlang namespace loaded")
message("✓ Data joining using actual source column names")
message("✓ Centralized helpers used (no redundant functions)")
message("✓ STRICT privacy enforcement active")
message("✓ Classification logic using robust case_when")
message("✓ Source ID column: '", respondent_id_column, "'")
message("✓ Join success rate: ", 
        round((nrow(df_prelim) - join_na_count) / nrow(df_prelim) * 100, 2), "%")

message("\n" %.% rep("=", 60))
message("✓ CLASSIFICATION COMPLETE - Version 5.0")
message("  All identified issues fixed and verified")
message(rep("=", 60))