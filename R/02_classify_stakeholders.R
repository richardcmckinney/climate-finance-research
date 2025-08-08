# 02_classify_stakeholders.R
# Purpose: Classify stakeholders based on Q2.1 and Q2.1_12_TEXT following Appendix J methodology
# Author: Richard McKinney
# Date: 2025-08-06

# Ensure S4 methods are available when running via Rscript
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))  # if you use dplyr/readr/etc.
# Load required libraries
library(tidyverse)

# Read the anonymized data
df <- read.csv("data/climate_finance_survey_anonymized.csv", 
               stringsAsFactors = FALSE)

cat("Starting stakeholder classification...\n")
cat("Total responses:", nrow(df), "\n\n")

# Function to map predefined roles to final categories
map_predefined_role <- function(role_value) {
  if (is.na(role_value) || role_value == "") {
    return(NA)
  }
  
  # Convert to lowercase for matching
  role_lower <- tolower(role_value)
  
  # Map based on text content
  if (grepl("entrepreneur|climate technology", role_lower)) {
    return("Entrepreneur in Climate Technology")
  } else if (grepl("venture capital", role_lower) && !grepl("corporate", role_lower)) {
    return("Venture Capital Firm")
  } else if (grepl("private equity", role_lower)) {
    return("Private Equity Firm")
  } else if (grepl("high net", role_lower) || grepl("hnwi", role_lower)) {
    return("High Net-Worth Individual")
  } else if (grepl("nonprofit|non-profit", role_lower) && !grepl("philanthropic", role_lower)) {
    return("Nonprofit Organization")
  } else if (grepl("academic|research institution|university", role_lower)) {
    return("Academic or Research Institution")
  } else if (grepl("limited partner|\\blp\\b", role_lower)) {
    return("Limited Partner")
  } else if (grepl("family office", role_lower)) {
    return("Family Office")
  } else if (grepl("corporate venture", role_lower)) {
    return("Corporate Venture Arm")
  } else if (grepl("angel investor", role_lower)) {
    return("Angel Investor")
  } else if (grepl("esg investor", role_lower)) {
    return("ESG Investor")
  } else if (grepl("government funding|government agency", role_lower)) {
    return("Government Funding Agency")
  } else if (grepl("philanthropic", role_lower)) {
    return("Philanthropic Organization")
  } else if (grepl("other", role_lower)) {
    return(NA)  # Will need to look at Q2.1_12_TEXT
  } else {
    return(NA)  # Unknown value
  }
}

# Function to categorize free-text responses
categorize_free_text <- function(text) {
  if (is.na(text) || text == "") {
    return("Miscellaneous and Individual Respondents")
  }
  
  # Convert to lowercase for matching
  text_lower <- tolower(text)
  
  # Check for Entrepreneur in Climate Technology FIRST (high priority)
  if (grepl("founder|co-founder|ceo.*startup|cto.*startup|started.*company|launching.*venture|entrepreneur", text_lower) &&
      grepl("climate|clean|green|sustain|energy|carbon|emission|renewable|solar|wind|electric|battery|hydrogen|bio", text_lower)) {
    return("Entrepreneur in Climate Technology")
  }
  
  # Investment and Financial Services
  if (grepl("investment bank|asset manager|fund manager|sovereign wealth|insurance company|private bank|debt fund|fintech|hedge fund|venture studio|commercial finance|private debt|impact investor|financial research|project finance|asset owner|pension fund|esg ratings|wealth management|green bond|blended finance", text_lower)) {
    return("Investment and Financial Services")
  }
  
  # Legal Services
  if (grepl("law firm|lawyer|attorney|solicitor|legal advisor|legal counsel|patent attorney|barrister", text_lower)) {
    return("Legal Services")
  }
  
  # Business Consulting and Advisory
  if (grepl("consult|advisor|advisory|strategist|m&a|management consulting|sustainability consultant|esg advisory", text_lower) &&
      !grepl("law|legal|attorney", text_lower)) {
    return("Business Consulting and Advisory")
  }
  
  # Corporate Entities
  if (grepl("corporate|conglomerate|multinational|holding company|plc|gmbh|ag |s\\.a\\.|incorporated|corporation|ltd", text_lower) &&
      !grepl("venture|consult|law", text_lower)) {
    return("Corporate Entities")
  }
  
  # Manufacturing and Industrial
  if (grepl("manufactur|industrial|factory|production|assembly|processing|engineering firm", text_lower) &&
      !grepl("consult|software", text_lower)) {
    return("Manufacturing and Industrial")
  }
  
  # Energy and Infrastructure
  if (grepl("renewable energy|power producer|energy services|utility|grid|infrastructure|solar developer|wind developer|hydro|geothermal|biomass|waste-to-energy", text_lower)) {
    return("Energy and Infrastructure")
  }
  
  # Real Estate and Property
  if (grepl("real estate|property|reit|building|housing|commercial property", text_lower)) {
    return("Real Estate and Property")
  }
  
  # Technology and Software
  if (grepl("software|saas|platform|tech company|technology|digital|data|ai |artificial intelligence|blockchain|app |application", text_lower) &&
      !grepl("consult|law", text_lower)) {
    return("Technology and Software")
  }
  
  # Media and Communication
  if (grepl("media|communications|press|journalism|publication|broadcasting", text_lower)) {
    return("Media and Communication")
  }
  
  # Philanthropic Organization
  if (grepl("foundation|philanthropic|charitable trust|donor|grantmaker|giving", text_lower)) {
    return("Philanthropic Organization")
  }
  
  # Nonprofit Organization  
  if (grepl("ngo|nonprofit|non-profit|charity|association|institute|advocacy|civil society", text_lower)) {
    return("Nonprofit Organization")
  }
  
  # Government Funding Agency
  if (grepl("government|public sector|ministry|department|agency|dfi|development finance|multilateral|bilateral|municipality|federal|state agency", text_lower)) {
    return("Government Funding Agency")
  }
  
  # Default to Miscellaneous
  return("Miscellaneous and Individual Respondents")
}

# Create the Final_Role_Category column
df$Final_Role_Category <- NA

for (i in 1:nrow(df)) {
  # First try to map from Q2.1
  mapped_role <- map_predefined_role(df$Q2.1[i])
  
  if (!is.na(mapped_role)) {
    # Successfully mapped from Q2.1
    df$Final_Role_Category[i] <- mapped_role
  } else if (!is.na(df$Q2.1_12_TEXT[i]) && df$Q2.1_12_TEXT[i] != "") {
    # Q2.1 was "Other" or unmapped, so categorize the free text
    df$Final_Role_Category[i] <- categorize_free_text(df$Q2.1_12_TEXT[i])
  } else {
    # No text to categorize
    df$Final_Role_Category[i] <- "Miscellaneous and Individual Respondents"
  }
}

# Print summary statistics
cat("\n=== CLASSIFICATION RESULTS ===\n")
role_summary <- table(df$Final_Role_Category, useNA = "ifany")
role_summary <- sort(role_summary, decreasing = TRUE)

# Create a summary data frame
summary_df <- data.frame(
  Category = names(role_summary),
  Count = as.numeric(role_summary),
  Percentage = round(as.numeric(role_summary)/sum(role_summary) * 100, 1)
)

print(summary_df)

# Expected counts from Appendix J
expected <- data.frame(
  Category = c("Entrepreneur in Climate Technology", "Venture Capital Firm", 
               "Investment and Financial Services", "Private Equity Firm",
               "Business Consulting and Advisory", "Nonprofit Organization",
               "High Net-Worth Individual", "Government Funding Agency",
               "Academic or Research Institution", "Limited Partner",
               "Family Office", "Corporate Venture Arm", "Angel Investor",
               "ESG Investor", "Legal Services", "Corporate Entities",
               "Manufacturing and Industrial", "Energy and Infrastructure",
               "Real Estate and Property", "Philanthropic Organization",
               "Technology and Software", "Media and Communication",
               "Miscellaneous and Individual Respondents"),
  Expected = c(159, 117, 109, 88, 79, 73, 66, 53, 52, 49, 48, 47, 43, 38, 38, 35, 
               25, 24, 20, 19, 19, 7, 151)
)

# Compare actual vs expected
comparison <- merge(expected, summary_df, by = "Category", all = TRUE)
comparison[is.na(comparison)] <- 0
comparison$Difference <- comparison$Count - comparison$Expected

cat("\n=== COMPARISON WITH EXPECTED (APPENDIX J) ===\n")
print(comparison[order(-comparison$Count), ])

# Save the classified data
output_file <- "data/climate_finance_survey_classified.csv"
write.csv(df, output_file, row.names = FALSE)
cat("\n=== FILE SAVED ===\n")
cat("Classified data saved to:", output_file, "\n")

# Create verification file for manual review
verification_df <- df[, c("ResponseId", "Q2.1", "Q2.1_12_TEXT", "Final_Role_Category")]
verification_file <- "output/stakeholder_classification_verification.csv"
write.csv(verification_df, verification_file, row.names = FALSE)
cat("Verification file saved to:", verification_file, "\n")

# Print some examples
cat("\n=== SAMPLE CLASSIFICATIONS ===\n")
cat("Examples of 'Other' text categorizations:\n")
other_responses <- df[!is.na(df$Q2.1_12_TEXT) & df$Q2.1_12_TEXT != "", 
                      c("Q2.1_12_TEXT", "Final_Role_Category")]
if (nrow(other_responses) > 0) {
  sample_size <- min(10, nrow(other_responses))
  sample_indices <- sample(1:nrow(other_responses), sample_size)
  print(other_responses[sample_indices, ])
}

cat("\n✓ Stakeholder classification completed successfully\n")
cat("Total classified responses:", sum(!is.na(df$Final_Role_Category)), "\n")
cat("Unclassified responses:", sum(is.na(df$Final_Role_Category)), "\n")
