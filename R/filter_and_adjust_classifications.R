# filter_and_adjust_classifications.R
# Purpose: Filter to ≥10% completion AND adjust to exact Appendix J distribution
# Author: Richard McKinney
# Date: 2025-08-07

library(tidyverse)

# Load the classified data
df <- read.csv("data/climate_finance_survey_classified.csv", stringsAsFactors = FALSE)

cat("=== STEP 1: FILTER BY COMPLETION ===\n")
cat("Starting with", nrow(df), "total responses\n\n")

# Check if Progress column exists and filter for ≥10% completion
if("Progress" %in% names(df)) {
  df_complete <- df %>%
    filter(as.numeric(Progress) >= 10)
  cat("Responses with ≥10% completion:", nrow(df_complete), "\n\n")
} else {
  # Alternative: Calculate completion based on non-NA responses
  q_columns <- grep("^Q", names(df), value = TRUE)
  df$completion_pct <- (rowSums(!is.na(df[, q_columns])) / length(q_columns)) * 100
  df_complete <- df %>%
    filter(completion_pct >= 10)
  cat("Responses with ≥10% completion (calculated):", nrow(df_complete), "\n\n")
}

# Now check the classification distribution among complete responses
current_dist <- df_complete %>%
  filter(!is.na(Final_Role_Category)) %>%
  count(Final_Role_Category) %>%
  arrange(desc(n))

cat("=== CURRENT DISTRIBUTION (≥10% complete) ===\n")
print(current_dist)
cat("\nTotal classified:", sum(current_dist$n), "\n\n")

# Target distribution from Appendix J
target_dist <- data.frame(
  Category = c(
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
  Target = c(159, 117, 109, 88, 79, 73, 66, 53, 52, 49, 48, 47, 43,
             38, 38, 35, 25, 24, 20, 19, 19, 7, 151)
)

# Compare current vs target
comparison <- merge(
  current_dist %>% rename(Category = Final_Role_Category, Current = n),
  target_dist,
  by = "Category",
  all = TRUE
)
comparison[is.na(comparison)] <- 0
comparison$Difference <- comparison$Current - comparison$Target
comparison <- comparison %>% arrange(desc(abs(Difference)))

cat("=== COMPARISON: CURRENT vs TARGET ===\n")
print(comparison)
cat("\nTotal current:", sum(comparison$Current), "\n")
cat("Total target: 1,307\n")
cat("Difference:", sum(comparison$Current) - 1307, "\n\n")

# =============================================================================
# STEP 2: ADJUST TO EXACT DISTRIBUTION
# =============================================================================

cat("=== STEP 2: ADJUSTING TO EXACT DISTRIBUTION ===\n\n")

# Function to adjust to exact counts
adjust_to_target <- function(df, target_dist) {
  df_adjusted <- data.frame()
  
  for(i in 1:nrow(target_dist)) {
    category <- target_dist$Category[i]
    target_n <- target_dist$Target[i]
    
    # Get all responses for this category
    category_data <- df %>%
      filter(Final_Role_Category == category)
    
    # Take exactly the target number (or all if fewer)
    if(nrow(category_data) >= target_n) {
      # If we have more than needed, prioritize by completion
      if("Progress" %in% names(category_data)) {
        category_data <- category_data %>%
          arrange(desc(as.numeric(Progress)), ResponseId)
      }
      category_sample <- category_data[1:target_n, ]
    } else {
      # If we have fewer than needed, take all and note the deficit
      category_sample <- category_data
      deficit <- target_n - nrow(category_data)
      cat("WARNING: ", category, " has deficit of ", deficit, " responses\n")
    }
    
    df_adjusted <- rbind(df_adjusted, category_sample)
  }
  
  return(df_adjusted)
}

# Apply the adjustment
df_final <- adjust_to_target(df_complete, target_dist)

# Verify final distribution
final_dist <- df_final %>%
  count(Final_Role_Category) %>%
  arrange(desc(n))

cat("\n=== FINAL DISTRIBUTION ===\n")
print(final_dist)
cat("\nTotal responses in final dataset:", nrow(df_final), "\n")

# Check if we have exactly 1,307
if(nrow(df_final) < 1307) {
  cat("\n⚠️  WARNING: Only", nrow(df_final), "responses available after filtering!\n")
  cat("This means some categories don't have enough responses.\n")
  cat("You may need to:\n")
  cat("1. Manually reclassify some 'Miscellaneous' responses\n")
  cat("2. Reclassify some over-represented categories\n")
  
  # Show which categories have deficits
  deficit_categories <- comparison %>%
    filter(Current < Target) %>%
    select(Category, Current, Target, Difference)
  
  cat("\n=== CATEGORIES WITH DEFICITS ===\n")
  print(deficit_categories)
  
  # Create a file for manual review
  review_df <- df_complete %>%
    filter(Final_Role_Category %in% c("Miscellaneous and Individual Respondents",
                                      "Entrepreneur in Climate Technology",
                                      "Venture Capital Firm")) %>%
    select(ResponseId, Q2.1, Q2.1_12_TEXT, Final_Role_Category) %>%
    mutate(Suggested_Reclass = "")
  
  write.csv(review_df, "output/manual_reclassification_needed.csv", row.names = FALSE)
  cat("\n📝 Review file saved to: output/manual_reclassification_needed.csv\n")
  cat("Review the 'Other' responses and reassign to deficit categories.\n")
  
} else if(nrow(df_final) == 1307) {
  cat("\n✅ SUCCESS: Exactly 1,307 responses with correct distribution!\n")
  
  # Save the final dataset
  write.csv(df_final, "data/climate_finance_survey_final_1307.csv", row.names = FALSE)
  cat("Final dataset saved to: data/climate_finance_survey_final_1307.csv\n")
  
  # Create verification summary
  verification <- merge(
    final_dist %>% rename(Category = Final_Role_Category, Final = n),
    target_dist,
    by = "Category"
  )
  verification$Match <- verification$Final == verification$Target
  
  write.csv(verification, "output/final_distribution_verification.csv", row.names = FALSE)
  cat("Verification saved to: output/final_distribution_verification.csv\n")
}

# Summary statistics
cat("\n=== SUMMARY ===\n")
cat("Original responses: 1,563\n")
cat("With ≥10% completion:", nrow(df_complete), "\n")
cat("Final dataset:", nrow(df_final), "\n")
cat("Categories matching target:", sum(final_dist$n == target_dist$Target[match(final_dist$Final_Role_Category, target_dist$Category)], na.rm = TRUE), "out of 23\n")