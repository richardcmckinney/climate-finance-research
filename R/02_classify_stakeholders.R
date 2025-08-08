# 02_classify_stakeholders.R
# Purpose: Produce stakeholder classifications per Appendix J methodology
# Outputs:
#   - docs/appendix_j_classification_template.csv
#   - data/survey_responses_anonymized_preliminary.csv

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

# Comprehensive classification based on Appendix J methodology
classify_stakeholder <- function(raw_text, other_text) {
  # Combine both texts for analysis
  combined <- paste(
    ifelse(is.na(raw_text), "", raw_text),
    ifelse(is.na(other_text), "", other_text),
    sep = " "
  ) %>% normalize_text()
  
  if (nchar(combined) == 0) return("Miscellaneous and Individual Respondents")
  
  # Priority order classification based on Appendix J
  
  # 1. Entrepreneur in Climate Technology (highest priority for climate-specific)
  if (str_detect(combined, "(?=.*(entrepreneur|founder|co[- ]?founder|ceo|cto|startup|venture\\s*builder|launching|started)).*(?=.*(climate|clean|green|sustain|carbon|emission|renewable|solar|wind|battery|storage|hydrogen|bio|ccus|adapt|energy|electric|environmental))")) {
    return("Entrepreneur in Climate Technology")
  }
  
  # 2. Government and Public Sector
  if (str_detect(combined, "dfi|development\\s*finance|multilateral|bilateral|government|ministry|department|agency|municipal|federal|state|public\\s*sector|public\\s*pension|nhs")) {
    return("Government Funding Agency")
  }
  
  # 3. Specific investor types (check before generic terms)
  if (str_detect(combined, "corporate\\s*venture") && !str_detect(combined, "law|legal|consult")) {
    return("Corporate Venture Arm")
  }
  
  if (str_detect(combined, "venture\\s*capital|\\bvc\\b") && !str_detect(combined, "corporate|law")) {
    return("Venture Capital Firm")
  }
  
  if (str_detect(combined, "private\\s*equity|\\bpe\\b") && !str_detect(combined, "backed|owned")) {
    return("Private Equity Firm")
  }
  
  if (str_detect(combined, "angel\\s*investor")) {
    return("Angel Investor")
  }
  
  if (str_detect(combined, "limited\\s*partner|\\blp\\b")) {
    return("Limited Partner")
  }
  
  if (str_detect(combined, "family\\s*office")) {
    return("Family Office")
  }
  
  if (str_detect(combined, "high\\s*net|hnwi|retired.*wealthy")) {
    return("High Net-Worth Individual")
  }
  
  if (str_detect(combined, "esg\\s*investor|impact\\s*invest|sustainability\\s*invest")) {
    return("ESG Investor")
  }
  
  # 4. Organizations
  if (str_detect(combined, "foundation|philanthrop|charitable|donor|grantmaker|giving\\s*fund") && 
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
  
  # 5. Financial Services (broader category)
  if (str_detect(combined, "investment\\s*bank|asset\\s*manage|fund\\s*manage|sovereign\\s*wealth|insurance|private\\s*bank|hedge\\s*fund|debt\\s*fund|pension\\s*fund|financial|fintech|wealth\\s*manage")) {
    return("Investment and Financial Services")
  }
  
  # 6. Professional Services
  if (str_detect(combined, "law\\s*firm|lawyer|attorney|solicitor|legal|barrister|patent\\s*attorney")) {
    return("Legal Services")
  }
  
  if (str_detect(combined, "consult|advis|strategy|strategist|m\\s*&\\s*a|management\\s*consult")) {
    return("Business Consulting and Advisory")
  }
  
  # 7. Sector-specific
  if (str_detect(combined, "utility|grid|renewable\\s*energy|power\\s*producer|ipp|solar\\s*develop|wind\\s*develop|infrastructure|energy\\s*service")) {
    return("Energy and Infrastructure")
  }
  
  if (str_detect(combined, "manufactur|industrial|factory|production|engineering\\s*firm|steel|chemical")) {
    return("Manufacturing and Industrial")
  }
  
  if (str_detect(combined, "real\\s*estate|property|reit|building|housing|commercial\\s*property")) {
    return("Real Estate and Property")
  }
  
  if (str_detect(combined, "software|saas|platform|tech\\s*company|technology|digital|blockchain|\\bai\\b|artificial\\s*intelligence")) {
    return("Technology and Software")
  }
  
  if (str_detect(combined, "media|press|publication|broadcast|communication|journalism")) {
    return("Media and Communication")
  }
  
  # 8. Generic Corporate
  if (str_detect(combined, "corporate|company|conglomerate|multinational|holding|plc|gmbh|ltd|llc|inc")) {
    return("Corporate Entities")
  }
  
  # 9. Individual/Small Business
  if (str_detect(combined, "entrepreneur|founder|business\\s*owner|self\\s*employ|startup") && 
      !str_detect(combined, "climate|clean|green")) {
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
    # Apply classification on original text (function normalizes internally)
    preliminary_category = map2_chr(role_raw, role_other, classify_stakeholder),
    # For the template, final category starts as preliminary
    final_category_appendix_j = preliminary_category
  ) %>%
  mutate(
    final_category_appendix_j = case_when(
      is.na(final_category_appendix_j) ~ "Miscellaneous and Individual Respondents",
      final_category_appendix_j == "" ~ "Miscellaneous and Individual Respondents",
      TRUE ~ final_category_appendix_j
    )
  )

# --------------------------------------------------------------------
# Quality checks and adjustments
# --------------------------------------------------------------------

# Additional refinements for specific known patterns
classification_df <- classification_df %>%
  mutate(
    final_category_appendix_j = case_when(
      str_detect(coalesce(role_other_norm, ""), "venture\\s*studio") & needs_harmonization ~ "Business Consulting and Advisory",
      str_detect(coalesce(role_other_norm, ""), "dfi|development\\s*finance\\s*institution") & needs_harmonization ~ "Government Funding Agency",
      str_detect(coalesce(role_other_norm, ""), "single\\s*family\\s*office") & needs_harmonization ~ "Family Office",
      str_detect(coalesce(role_other_norm, ""), "law\\s*firm") & needs_harmonization ~ "Legal Services",
      str_detect(coalesce(role_other_norm, ""), "hedge\\s*fund") & needs_harmonization ~ "Investment and Financial Services",
      str_detect(coalesce(role_other_norm, ""), "sovereign\\s*wealth") & needs_harmonization ~ "Investment and Financial Services",
      str_detect(coalesce(role_other_norm, ""), "pension\\s*fund") & needs_harmonization ~ "Investment and Financial Services",
      str_detect(coalesce(role_other_norm, ""), "investment\\s*bank") & needs_harmonization ~ "Investment and Financial Services",
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
# Summary Statistics
# --------------------------------------------------------------------

message("\n=== CLASSIFICATION SUMMARY ===")

# Overall distribution
summary_stats <- classification_df %>%
  count(final_category_appendix_j, sort = TRUE) %>%
  mutate(percentage = round(n / sum(n) * 100, 1))

print(summary_stats, n = 30)

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

print(harmonized_sample)

message("\n✓ Classification complete!")