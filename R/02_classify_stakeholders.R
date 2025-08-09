#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# 02_classify_stakeholders.R
# Purpose: Produce stakeholder classifications per Appendix J methodology
# Outputs:
#   - docs/appendix_j_classification_template.csv
#   - data/survey_responses_anonymized_preliminary.csv
#   - output/classification_audit.csv (NEW: auditable summary)

suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})

message("=== STAKEHOLDER CLASSIFICATION (APPENDIX J) ===")

# --------------------------------------------------------------------
# Config / paths
# --------------------------------------------------------------------
in_basic  <- "data/survey_responses_anonymized_basic.csv"
out_template <- "docs/appendix_j_classification_template.csv"
out_prelim   <- "data/survey_responses_anonymized_preliminary.csv"
out_audit    <- "output/classification_audit.csv"  # NEW: audit log

# --------------------------------------------------------------------
# Load data
# --------------------------------------------------------------------
stopifnot(file.exists(in_basic))
df <- suppressMessages(read_csv(in_basic, show_col_types = FALSE))

# Extract role columns (robustly)
pick_col <- function(df, candidates) {
  hits <- intersect(candidates, names(df))
  if (length(hits)) df[[hits[1]]] else rep(NA_character_, nrow(df))
}

role_raw <- pick_col(df, c(
  "role_raw", "Q2.1",
  "Which of the following best describes your role? (Please select the most appropriate option) - Selected Choice"
))

role_other <- pick_col(df, c(
  "role_other", "Q2.1_12_TEXT",
  "Which of the following best describes your role? (Please select the most appropriate option) - Other (please specify)  - Text"
))

respondent_id <- pick_col(df, c("respondent_id", "ResponseId", "_recordId", "response_id"))

# --------------------------------------------------------------------
# Classification Functions
# --------------------------------------------------------------------

# Normalize text for matching
normalize_text <- function(x) {
  x %>%
    str_replace_all("\\s+", " ") %>%
    str_trim() %>%
    tolower() %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

# Helper function to check if text contains climate-related terms
is_climate_related <- function(text) {
  climate_terms <- "climate|clean|green|sustain|carbon|emission|renewable|solar|wind|battery|storage|hydrogen|bio|ccus|adapt|energy|electric|environmental|esg"
  str_detect(text, climate_terms)
}

# Helper function to check if text contains entrepreneur-related terms
is_entrepreneur_related <- function(text) {
  entrepreneur_terms <- "entrepreneur|founder|co[- ]?founder|ceo|cto|startup|venture\\s*builder|launching|started"
  str_detect(text, entrepreneur_terms)
}

# INTEGRATED classification with all refinements in one function
classify_stakeholder <- function(raw_text, other_text) {
  # Combine both texts for analysis
  combined <- paste(
    ifelse(is.na(raw_text), "", raw_text),
    ifelse(is.na(other_text), "", other_text),
    sep = " "
  ) %>% normalize_text()
  
  if (nchar(combined) == 0) return("Miscellaneous and Individual Respondents")
  
  # Priority order classification based on Appendix J methodology
  # WITH INTEGRATED REFINEMENTS
  
  # 1. Entrepreneur in Climate Technology (highest priority for climate-specific)
  if (is_entrepreneur_related(combined) && is_climate_related(combined)) {
    return("Entrepreneur in Climate Technology")
  }
  
  # 2. Real Estate - Move this BEFORE Government to avoid misclassification
  if (str_detect(combined, "real\\s*estate|property\\s*developer|property\\s*investor|reit|commercial\\s*property|industrial\\s*real\\s*estate")) {
    return("Real Estate and Property")
  }
  
  # 3. Government and Public Sector - MORE SPECIFIC PATTERNS
  if (str_detect(combined, "dfi\\b|development\\s*finance\\s*institution|multilateral\\s*(development|organization|bank)|bilateral|government\\s+(agency|funding|organization)|ministry|federal\\s+(agency|government)|state\\s+(agency|government)|municipal|public\\s*sector|public\\s*pension|\\bnhs\\b")) {
    return("Government Funding Agency")
  }
  
  # 4. Specific investor types with refinements
  # Corporate venture with law/legal check
  if (str_detect(combined, "corporate\\s*venture") && !str_detect(combined, "law|legal|consult")) {
    return("Corporate Venture Arm")
  }
  
  # Venture Capital with specific refinements
  if (str_detect(combined, "venture\\s*capital|\\bvc\\b(?!\\w)|venture\\s*firm") && !str_detect(combined, "corporate|law")) {
    # Additional check for venture studio which should be consulting
    if (str_detect(combined, "venture\\s*studio")) {
      return("Business Consulting and Advisory")
    }
    return("Venture Capital Firm")
  }
  
  # Private Equity
  if (str_detect(combined, "private\\s*equity|\\bpe\\b") && !str_detect(combined, "backed|owned")) {
    return("Private Equity Firm")
  }
  
  # Angel Investor
  if (str_detect(combined, "angel\\s*investor")) {
    return("Angel Investor")
  }
  
  # Limited Partner
  if (str_detect(combined, "limited\\s*partner|\\blp\\b")) {
    return("Limited Partner")
  }
  
  # Family Office with single family office refinement
  if (str_detect(combined, "family\\s*office|single\\s*family\\s*office")) {
    return("Family Office")
  }
  
  # High Net-Worth Individual
  if (str_detect(combined, "high\\s*net|hnwi|retired.*wealthy")) {
    return("High Net-Worth Individual")
  }
  
  # ESG Investor
  if (str_detect(combined, "esg\\s*investor|impact\\s*invest|sustainability\\s*invest|impact\\s*first")) {
    return("ESG Investor")
  }
  
  # 5. Organizations
  if (str_detect(combined, "foundation|philanthrop|charitable|donor|grantmaker|giving\\s*fund|community\\s*foundation") && 
      !str_detect(combined, "nhs|trust\\s*beneficiary")) {
    return("Philanthropic Organization")
  }
  
  if (str_detect(combined, "nonprofit|non[- ]?profit|ngo|charity|civil\\s*society|association") && 
      !str_detect(combined, "law|consult")) {
    return("Nonprofit Organization")
  }
  
  if (str_detect(combined, "university|academic|research\\s*(center|institute|institution)|\\blab\\b")) {
    return("Academic or Research Institution")
  }
  
  # 6. Financial Services with specific refinements
  if (str_detect(combined, "hedge\\s*fund")) {
    return("Investment and Financial Services")
  }
  
  if (str_detect(combined, "sovereign\\s*wealth")) {
    return("Investment and Financial Services")
  }
  
  if (str_detect(combined, "pension\\s*fund")) {
    return("Investment and Financial Services")
  }
  
  if (str_detect(combined, "investment\\s*bank|asset\\s*manage|fund\\s*manage|insurance|private\\s*bank|debt\\s*fund|financial\\s*(advisor|advisory|holding|infrastructure)|fintech|wealth\\s*manage|alternative\\s*asset|permanent\\s*capital|securitized.*projects")) {
    return("Investment and Financial Services")
  }
  
  # 7. Professional Services
  # Law firms with specific pattern
  if (str_detect(combined, "law\\s*firm|lawyer|attorney|solicitor|legal|barrister|patent\\s*attorney|law\\s*office")) {
    return("Legal Services")
  }
  
  # Consulting including venture studio
  if (str_detect(combined, "consult|advis|strategy|strategist|m\\s*&\\s*a|management\\s*consult|venture\\s*studio|consultant\\s*agency")) {
    return("Business Consulting and Advisory")
  }
  
  # 8. Sector-specific
  if (str_detect(combined, "utility|utlity|grid|renewable\\s*energy|power\\s*producer|ipp|solar\\s*develop|wind\\s*develop|infrastructure\\s*fund|energy\\s*(service|firm|sector)|electric\\s*cooperative|gas\\s*transmission|climate\\s*adaptive\\s*infrastructure")) {
    return("Energy and Infrastructure")
  }
  
  # Fixed typo detection for manufacturing
  if (str_detect(combined, "manufactur|manurfactur|industrial|factory|production|engineering\\s*firm|steel|chemical|packaging|mineral\\s*exploration|mfg\\s*company")) {
    return("Manufacturing and Industrial")
  }
  
  if (str_detect(combined, "software|saas|platform|tech\\s*company|technology|digital|blockchain|\\bai\\b|artificial\\s*intelligence|biotech|fintech\\s*start\\s*up|clean\\s*tech\\s*company")) {
    return("Technology and Software")
  }
  
  if (str_detect(combined, "media|press|publication|broadcast|communication|journalism")) {
    return("Media and Communication")
  }
  
  # 9. Generic Corporate - should come later to catch more specific patterns first
  if (str_detect(combined, "corporate|company|conglomerate|multinational|holding|plc|gmbh|ltd|llc|inc|limited\\s*company|publicly\\s*listed|privately\\s*held")) {
    return("Corporate Entities")
  }
  
  # 10. Translation and other service businesses - more specific
  if (str_detect(combined, "translation\\s*agency")) {
    return("Miscellaneous and Individual Respondents")
  }
  
  # 11. Individual/Small Business
  if (str_detect(combined, "entrepreneur|founder|business\\s*owner|self\\s*employ|startup|small\\s*business") && 
      !is_climate_related(combined)) {
    return("Miscellaneous and Individual Respondents")
  }
  
  # Default
  return("Miscellaneous and Individual Respondents")
}

# --------------------------------------------------------------------
# Apply Classification
# --------------------------------------------------------------------

# Create classification data frame
cls <- tibble(
  respondent_id = respondent_id,
  role_raw  = role_raw,
  role_other = role_other
) %>%
  mutate(
    role_raw_norm   = normalize_text(role_raw),
    role_other_norm = normalize_text(role_other)
  )

classification_df <- cls %>%
  mutate(
    # Check if "Other (please specify)" was selected
    needs_harmonization = !is.na(role_raw) & str_detect(coalesce(role_raw_norm, ""), "other.*please.*specify"),
    # Apply integrated classification (includes all refinements)
    preliminary_category = map2_chr(role_raw, role_other, classify_stakeholder),
    # Final category starts as preliminary
    final_category_appendix_j = preliminary_category
  ) %>%
  mutate(
    # Ensure no NAs
    final_category_appendix_j = case_when(
      is.na(final_category_appendix_j) ~ "Miscellaneous and Individual Respondents",
      final_category_appendix_j == "" ~ "Miscellaneous and Individual Respondents",
      TRUE ~ final_category_appendix_j
    )
  )

# --------------------------------------------------------------------
# Save outputs
# --------------------------------------------------------------------

# Template for manual review
template <- classification_df %>%
  arrange(desc(needs_harmonization), respondent_id)

dir.create(dirname(out_template), showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(out_prelim),   showWarnings = FALSE, recursive = TRUE)
dir.create(dirname(out_audit),    showWarnings = FALSE, recursive = TRUE)

write_csv(template, out_template)
message("Template saved to: ", normalizePath(out_template))

# Join back to full dataset
df_prelim <- df %>%
  left_join(
    classification_df %>% 
      select(respondent_id, preliminary_category, final_category_appendix_j),
    by = "respondent_id"
  )

write_csv(df_prelim, out_prelim)
message("Preliminary data saved to: ", normalizePath(out_prelim))

# --------------------------------------------------------------------
# Summary Statistics & AUDIT LOG (NEW)
# --------------------------------------------------------------------

message("\n=== CLASSIFICATION SUMMARY ===")

# Overall distribution
summary_stats <- classification_df %>%
  count(final_category_appendix_j, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print(summary_stats, n = 30)

# Save audit log to file
audit_log <- summary_stats %>%
  rename(
    Category = final_category_appendix_j,
    Count = n,
    Percentage = percentage
  ) %>%
  mutate(
    Audit_Date = Sys.Date(),
    Total_Records = nrow(classification_df),
    Needs_Harmonization = sum(classification_df$needs_harmonization, na.rm = TRUE),
    Direct_Classification = sum(!classification_df$needs_harmonization, na.rm = TRUE)
  )

write_csv(audit_log, out_audit)
message("\nAudit log saved to: ", normalizePath(out_audit))

message("\n=== HARMONIZATION STATISTICS ===")
message("Total responses: ", nrow(classification_df))
message("Needs harmonization: ", sum(classification_df$needs_harmonization, na.rm = TRUE))
message("Direct classification: ", sum(!classification_df$needs_harmonization, na.rm = TRUE))

# Validation check - ensure no NAs in final category
na_count <- sum(is.na(classification_df$final_category_appendix_j))
if (na_count > 0) {
  warning("Found ", na_count, " NA values in final_category_appendix_j!")
} else {
  message("\n✓ All records have valid final categories")
}

# Sample of harmonized entries for verification
message("\n=== SAMPLE HARMONIZED ENTRIES ===")
harmonized_sample <- classification_df %>%
  filter(needs_harmonization) %>%
  select(role_other, final_category_appendix_j) %>%
  slice_sample(n = min(10, sum(classification_df$needs_harmonization, na.rm = TRUE)))

if (nrow(harmonized_sample) > 0) {
  print(harmonized_sample)
} else {
  message("No harmonized entries to display")
}

message("\n✓ Classification complete!")