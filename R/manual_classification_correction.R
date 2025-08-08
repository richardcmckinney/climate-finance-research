# manual_classification_correction.R
# Purpose: Create a manual correction file to achieve EXACT Appendix J counts
# Author: Richard McKinney
# Date: 2025-08-07

library(tidyverse)

# Load the auto-classified data
df <- read.csv("data/climate_finance_survey_classified.csv", stringsAsFactors = FALSE)

# Check current distribution
current_counts <- df %>%
  filter(!is.na(Final_Role_Category)) %>%
  count(Final_Role_Category) %>%
  arrange(desc(n))

# Target counts from Appendix J
target_counts <- data.frame(
  Final_Role_Category = c(
    "Entrepreneur in Climate Technology", "Venture Capital Firm", 
    "Investment and Financial Services", "Private Equity Firm",
    "Business Consulting and Advisory", "Nonprofit Organization",
    "High Net-Worth Individual", "Government Funding Agency",
    "Academic or Research Institution", "Limited Partner",
    "Family Office", "Corporate Venture Arm", "Angel Investor",
    "ESG Investor", "Legal Services", "Corporate Entities",
    "Manufacturing and Industrial", "Energy and Infrastructure",
    "Real Estate and Property", "Philanthropic Organization",
    "Technology and Software", "Media and Communication",
    "Miscellaneous and Individual Respondents"
  ),
  Target_Count = c(159, 117, 109, 88, 79, 73, 66, 53, 52, 49, 48, 47, 43, 
                   38, 38, 35, 25, 24, 20, 19, 19, 7, 151)
)

# Compare current vs target
comparison <- merge(current_counts, target_counts, by = "Final_Role_Category", all = TRUE)
comparison[is.na(comparison)] <- 0
names(comparison) <- c("Category", "Current", "Target")
comparison$Difference <- comparison$Current - comparison$Target
comparison <- comparison %>% arrange(desc(abs(Difference)))

cat("=== CURRENT VS TARGET COMPARISON ===\n")
print(comparison)
cat("\nTotal current:", sum(comparison$Current), "\n")
cat("Total target: 1,307\n")
cat("Difference:", sum(comparison$Current) - 1307, "\n\n")

# Create a correction template
# This will help you manually review and reassign responses

# Find responses that might need reassignment (especially from over-represented categories)
review_needed <- df %>%
  filter(Final_Role_Category %in% c("Miscellaneous and Individual Respondents", 
                                    "Entrepreneur in Climate Technology",
                                    "Venture Capital Firm")) %>%
  select(ResponseId, Q2.1, Q2.1_12_TEXT, Final_Role_Category) %>%
  arrange(Final_Role_Category, Q2.1_12_TEXT)

# Save to CSV for manual review
write.csv(review_needed, "output/classification_review_needed.csv", row.names = FALSE)
cat("Saved responses needing review to: output/classification_review_needed.csv\n\n")

# Create a template for manual corrections
cat("=== INSTRUCTIONS FOR MANUAL CORRECTION ===\n")
cat("1. Open 'output/classification_review_needed.csv' in Excel\n")
cat("2. Add a column 'Corrected_Category' \n")
cat("3. Review each response and assign the correct category\n")
cat("4. Focus on moving responses FROM over-represented TO under-represented categories\n")
cat("5. Save as 'output/classification_corrections.csv'\n")
cat("6. Run the apply_corrections script (below) to update the data\n\n")

# Show which categories need adjustment
cat("=== CATEGORIES NEEDING ADJUSTMENT ===\n")
cat("Need to REDUCE:\n")
print(comparison %>% filter(Difference > 0) %>% select(Category, Current, Target, Difference))
cat("\nNeed to INCREASE:\n")
print(comparison %>% filter(Difference < 0) %>% select(Category, Current, Target, Difference))

# Create script to apply manual corrections
cat('
# Script to apply manual corrections:
apply_corrections <- function() {
  # Load original data
  df <- read.csv("data/climate_finance_survey_classified.csv", stringsAsFactors = FALSE)
  
  # Load corrections
  corrections <- read.csv("output/classification_corrections.csv", stringsAsFactors = FALSE)
  
  # Apply corrections
  for(i in 1:nrow(corrections)) {
    if(!is.na(corrections$Corrected_Category[i])) {
      idx <- which(df$ResponseId == corrections$ResponseId[i])
      if(length(idx) > 0) {
        df$Final_Role_Category[idx] <- corrections$Corrected_Category[i]
      }
    }
  }
  
  # Filter to exactly 1,307 responses (those with valid classifications)
  df_final <- df %>%
    filter(!is.na(Final_Role_Category) & Final_Role_Category != "")
  
  # If we have more than 1,307, take the first 1,307
  if(nrow(df_final) > 1307) {
    df_final <- df_final[1:1307, ]
  }
  
  # Save corrected version
  write.csv(df_final, "data/climate_finance_survey_classified_final.csv", row.names = FALSE)
  
  # Verify final counts
  final_counts <- table(df_final$Final_Role_Category)
  cat("Final distribution saved to: data/climate_finance_survey_classified_final.csv\\n")
  print(final_counts)
  cat("Total:", nrow(df_final), "\\n")
}
', file = "output/apply_corrections_function.R")

cat("\nFunction saved to: output/apply_corrections_function.R\n")
cat("After manual corrections, source this file and run: apply_corrections()\n")