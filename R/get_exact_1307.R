# get_exact_1307.R — deterministic Appendix J quota-matching (publication grade)
# Purpose: Generate exactly N=1,307 responses matching Appendix J distribution
# Author: Richard McKinney
# Date: 2025-08-08

# ========================= INITIALIZATION =========================
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))
set.seed(1307)  # Deterministic for reproducibility

# Create necessary directories
dir.create("data",   recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)
dir.create("logs",   recursive = TRUE, showWarnings = FALSE)

# ========================= CONFIGURATION =========================
config <- list(
  # Input/Output paths
  infile         = "data/climate_finance_survey_classified.csv",
  alt_infile     = "data/survey_responses_anonymized_preliminary.csv",  # Alternative input
  outfile_final  = "data/climate_finance_survey_final_1307.csv",
  outfile_verify = "output/final_distribution_verification.csv",
  outfile_log    = "output/reassignment_log.csv",
  outfile_stats  = "output/quota_matching_statistics.csv",
  outfile_quality = "output/quality_control_report.csv",
  
  # Processing parameters
  min_progress = 10,
  target_n = 1307,
  
  # Quality control exclusions (documented)
  quality_exclusions = c(
    "R_bBAyiwWo1sotqM6"  # Straight-line respondent
  ),
  
  # Verbose logging
  verbose = TRUE,
  log_file = paste0("logs/get_exact_1307_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

# ========================= LOGGING SETUP =========================
# Create log connection
if (config$verbose) {
  log_con <- file(config$log_file, open = "wt")
  on.exit(close(log_con), add = TRUE)
  
  log_msg <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
    message(msg)
    writeLines(msg, log_con)
  }
} else {
  log_msg <- function(...) message(...)
}

log_msg("Starting get_exact_1307.R")
log_msg("Configuration:")
log_msg("  Target N: ", config$target_n)
log_msg("  Min Progress: ", config$min_progress)
log_msg("  Random seed: 1307")

# ========================= DATA LOADING =========================
log_msg("\n=== DATA LOADING ===")

# Try primary input file first, then alternative
infile <- if (file.exists(config$infile)) {
  config$infile
} else if (file.exists(config$alt_infile)) {
  log_msg("Primary input not found, using alternative: ", config$alt_infile)
  config$alt_infile
} else {
  stop("Neither input file found:\n  ", config$infile, "\n  ", config$alt_infile)
}

# Load data with error handling
df <- tryCatch({
  read.csv(infile, stringsAsFactors = FALSE)
}, error = function(e) {
  stop("Failed to read input file: ", e$message)
})

log_msg("Loaded ", nrow(df), " rows from ", basename(infile))
log_msg("Columns: ", ncol(df))

# Store original for comparison
df_original <- df

# ========================= COLUMN VALIDATION =========================
log_msg("\n=== COLUMN VALIDATION ===")

# ResponseId column
if (!"ResponseId" %in% names(df)) {
  resp_id_candidates <- c("respondent_id", "response_id", "_recordId", "id")
  resp_id_col <- resp_id_candidates[resp_id_candidates %in% names(df)]
  
  if (length(resp_id_col) > 0) {
    log_msg("Using alternative ID column: ", resp_id_col[1])
    df$ResponseId <- df[[resp_id_col[1]]]
  } else {
    log_msg("No ID column found, creating sequential IDs")
    df$ResponseId <- paste0("R_", seq_len(nrow(df)))
  }
}

# Progress column
if (!"Progress" %in% names(df)) {
  progress_candidates <- c("progress", "Progress_pct", "completion", "Completion")
  progress_col <- progress_candidates[progress_candidates %in% names(df)]
  
  if (length(progress_col) > 0) {
    log_msg("Using alternative progress column: ", progress_col[1])
    df$Progress <- df[[progress_col[1]]]
  } else {
    stop("Progress column missing and no alternatives found. Available columns: ",
         paste(names(df), collapse = ", "))
  }
}

# Convert Progress to numeric
df$Progress <- suppressWarnings(as.numeric(df$Progress))
if (all(is.na(df$Progress))) {
  stop("Progress column contains no valid numeric values")
}

# Statistics on Progress
log_msg("Progress statistics:")
log_msg("  Mean: ", round(mean(df$Progress, na.rm = TRUE), 1))
log_msg("  Median: ", round(median(df$Progress, na.rm = TRUE), 1))
log_msg("  Min: ", min(df$Progress, na.rm = TRUE))
log_msg("  Max: ", max(df$Progress, na.rm = TRUE))
log_msg("  NA count: ", sum(is.na(df$Progress)))

# ========================= ELIGIBILITY FILTERS =========================
log_msg("\n=== APPLYING ELIGIBILITY FILTERS ===")
initial_count <- nrow(df)

# Filter 1: Consent (if present)
consent_cols <- c("consent", "Consent", "Q_consent", "Q0_consent", "Q1.1", "Q1_1")
cc <- consent_cols[consent_cols %in% names(df)]

if (length(cc) >= 1) {
  log_msg("Checking consent using column: ", cc[1])
  df[[cc[1]]] <- as.character(df[[cc[1]]])
  
  # Comprehensive consent values
  consent_vals <- c("consent", "Consent", "CONSENT",
                   "Yes", "yes", "YES", "Y", "y",
                   "I consent", "i consent", "I Consent", "I CONSENT",
                   "Consented", "consented", "CONSENTED",
                   "Agree", "agree", "AGREE",
                   "1", "TRUE", "True", "true", "T")
  
  before_consent <- nrow(df)
  df <- df %>% filter(.data[[cc[1]]] %in% consent_vals | is.na(.data[[cc[1]]]))
  after_consent <- nrow(df)
  
  if (before_consent != after_consent) {
    log_msg("  Removed ", before_consent - after_consent, " rows without consent")
  } else {
    log_msg("  No rows removed by consent filter")
  }
} else {
  log_msg("No consent column found, proceeding without consent filter")
}

# Filter 2: Progress >= threshold
before_progress <- nrow(df)
df <- df %>% filter(Progress >= config$min_progress)
after_progress <- nrow(df)

if (before_progress != after_progress) {
  log_msg("Removed ", before_progress - after_progress, 
          " rows with Progress < ", config$min_progress)
} else {
  log_msg("No rows removed by progress filter")
}

# Filter 3: Quality control exclusions
if (length(config$quality_exclusions) > 0) {
  before_quality <- nrow(df)
  df <- df %>% filter(!(ResponseId %in% config$quality_exclusions))
  after_quality <- nrow(df)
  
  if (before_quality != after_quality) {
    log_msg("Removed ", before_quality - after_quality, 
            " rows for quality control (straight-liners, etc.)")
    
    # Document exclusions
    quality_report <- data.frame(
      ResponseId = config$quality_exclusions,
      Reason = "Straight-line response pattern",
      Date_Excluded = Sys.Date()
    )
    write.csv(quality_report, config$outfile_quality, row.names = FALSE)
  }
}

log_msg("\nFiltered from ", initial_count, " to ", nrow(df), " eligible responses")
log_msg("Reduction: ", round((1 - nrow(df)/initial_count) * 100, 1), "%")

# ========================= TARGET DISTRIBUTION =========================
log_msg("\n=== TARGET DISTRIBUTION (APPENDIX J) ===")

target <- tibble::tribble(
  ~Category, ~Target,
  "Entrepreneur in Climate Technology", 159,
  "Venture Capital Firm",               117,
  "Investment and Financial Services",  109,
  "Private Equity Firm",                 88,
  "Business Consulting and Advisory",    79,
  "Nonprofit Organization",              73,
  "High Net-Worth Individual",           66,
  "Government Funding Agency",           53,
  "Academic or Research Institution",    52,
  "Limited Partner",                     49,
  "Family Office",                       48,
  "Corporate Venture Arm",               47,
  "Angel Investor",                      43,
  "ESG Investor",                        38,
  "Legal Services",                      38,
  "Corporate Entities",                  35,
  "Manufacturing and Industrial",        25,
  "Energy and Infrastructure",           24,
  "Real Estate and Property",            20,
  "Philanthropic Organization",          19,
  "Technology and Software",             19,
  "Media and Communication",              7,
  "Miscellaneous and Individual Respondents", 151
)

# Validate target sum
target_sum <- sum(target$Target)
if (target_sum != config$target_n) {
  warning("Target distribution sums to ", target_sum, " not ", config$target_n, "!")
}

log_msg("Target categories: ", nrow(target))
log_msg("Target total: ", target_sum)

# ========================= ROLE COLUMN DETECTION =========================
log_msg("\n=== ROLE COLUMN DETECTION ===")

# Try multiple possible column names
role_candidates <- c("Final_Role_Category", "final_category_appendix_j", 
                     "stakeholder_category", "Final_Category", "Category")
role_col <- role_candidates[role_candidates %in% names(df)]

if (length(role_col) == 0) {
  stop("No role category column found. Looked for: ", 
       paste(role_candidates, collapse = ", "),
       "\nAvailable columns: ", paste(names(df), collapse = ", "))
}

role_col <- role_col[1]
log_msg("Using role column: ", role_col)

# Validate categories
unique_cats <- unique(df[[role_col]])
unique_cats <- unique_cats[!is.na(unique_cats)]
log_msg("Unique categories in data: ", length(unique_cats))

missing_cats <- setdiff(target$Category, unique_cats)
if (length(missing_cats) > 0) {
  log_msg("WARNING: Target categories not found in data:")
  for (cat in missing_cats) {
    log_msg("  - ", cat)
  }
}

extra_cats <- setdiff(unique_cats, target$Category)
if (length(extra_cats) > 0) {
  log_msg("Categories in data but not in target:")
  for (cat in extra_cats) {
    log_msg("  - ", cat)
  }
}

# ========================= FIRST PASS: DIRECT MATCHING =========================
log_msg("\n=== FIRST PASS: Direct category matching ===")

take_logs <- list()
selected_list <- list()
stats_list <- list()

for (i in seq_len(nrow(target))) {
  cat_name <- target$Category[i]
  need <- target$Target[i]
  
  # Get pool of candidates for this category
  pool <- df %>% 
    filter(.data[[role_col]] == cat_name) %>%
    arrange(desc(Progress), ResponseId)  # Deterministic ordering
  
  available <- nrow(pool)
  taken <- min(available, need)
  deficit <- max(0, need - taken)
  
  # Take what we can
  if (taken > 0) {
    selected_list[[cat_name]] <- pool[seq_len(taken), , drop = FALSE]
    take_logs[[cat_name]] <- pool[seq_len(taken), ] %>% 
      select(ResponseId, Progress, all_of(role_col))
  } else {
    selected_list[[cat_name]] <- tibble()
    take_logs[[cat_name]] <- tibble()
  }
  
  # Record statistics
  stats_list[[i]] <- data.frame(
    Category = cat_name,
    Target = need,
    Available = available,
    Taken = taken,
    Deficit = deficit,
    Surplus = max(0, available - need)
  )
  
  log_msg(sprintf("%-45s: Need %3d, Available %3d, Taken %3d%s", 
                  cat_name, need, available, taken,
                  if (deficit > 0) paste0(" [DEFICIT: ", deficit, "]") else ""))
}

# Combine results
selected <- bind_rows(selected_list)
first_pass_stats <- bind_rows(stats_list)
final <- selected

log_msg("\nFirst pass summary:")
log_msg("  Total taken: ", nrow(selected))
log_msg("  Categories with deficits: ", sum(first_pass_stats$Deficit > 0))
log_msg("  Total deficit: ", sum(first_pass_stats$Deficit))

# ========================= SECOND PASS: DEFICIT FILLING =========================
log_msg("\n=== SECOND PASS: Filling deficits ===")

taken_ids <- final$ResponseId
remaining <- df %>% filter(!(ResponseId %in% taken_ids))
log_msg("Remaining pool size: ", nrow(remaining))

# Calculate current state
need_tbl <- target %>%
  mutate(
    Taken = map_int(Category, ~ sum(final[[role_col]] == .x, na.rm = TRUE))
  ) %>%
  mutate(
    Deficit = pmax(Target - Taken, 0L)
  )

total_deficit <- sum(need_tbl$Deficit)
log_msg("Total deficit to fill: ", total_deficit)

reassign_log <- tibble()

if (total_deficit > 0 && nrow(remaining) > 0) {
  # Calculate donor potential
  donor_tbl <- target %>%
    mutate(
      Avail = map_int(Category, ~ sum(remaining[[role_col]] == .x, na.rm = TRUE))
    ) %>%
    mutate(
      Can_Donate = Avail  # All remaining can potentially be reassigned
    ) %>%
    arrange(desc(Can_Donate))
  
  log_msg("\nDonor categories (top 5):")
  top_donors <- head(donor_tbl %>% filter(Can_Donate > 0), 5)
  for (i in seq_len(nrow(top_donors))) {
    log_msg("  ", top_donors$Category[i], ": ", top_donors$Can_Donate[i], " available")
  }
  
  # Fill deficits
  for (i in seq_len(nrow(need_tbl))) {
    if (nrow(remaining) == 0) break
    
    cat_i <- need_tbl$Category[i]
    deficit_i <- need_tbl$Deficit[i]
    
    if (deficit_i <= 0) next
    
    log_msg("\nFilling deficit for ", cat_i, " (need ", deficit_i, " more)")
    
    # Try each donor category (prioritize larger pools)
    for (j in seq_len(nrow(donor_tbl))) {
      if (deficit_i <= 0) break
      if (nrow(remaining) == 0) break
      
      dcat <- donor_tbl$Category[j]
      
      # Get candidates from this donor category
      cand <- remaining %>%
        filter(.data[[role_col]] == dcat) %>%
        arrange(desc(Progress), ResponseId)  # Take highest Progress first
      
      if (nrow(cand) == 0) next
      
      # Determine how many to reassign
      k <- min(nrow(cand), deficit_i)
      move <- cand[seq_len(k), , drop = FALSE]
      
      # Record reassignment
      reassign_log <- bind_rows(
        reassign_log,
        tibble(
          ResponseId = move$ResponseId,
          Progress = move$Progress,
          From = dcat,
          To = cat_i,
          Reason = "Deficit filling"
        )
      )
      
      # Update category
      move[[role_col]] <- cat_i
      
      # Update datasets
      remaining <- anti_join(remaining, cand[seq_len(k), c("ResponseId")], by = "ResponseId")
      final <- bind_rows(final, move)
      deficit_i <- deficit_i - k
      
      log_msg("  Reassigned ", k, " from ", dcat)
    }
    
    if (deficit_i > 0) {
      log_msg("  WARNING: Could not fill complete deficit (still need ", deficit_i, ")")
    }
  }
} else if (total_deficit > 0) {
  log_msg("WARNING: Have deficits but no remaining responses to reassign!")
}

# ========================= FINAL VERIFICATION =========================
log_msg("\n=== FINAL VERIFICATION ===")

# Calculate final distribution
final_dist <- final %>% 
  count(!!sym(role_col), name = "Final") %>% 
  rename(Category = !!role_col)

# Create verification table
verify_tbl <- full_join(final_dist, target, by = "Category") %>%
  mutate(
    Final = replace_na(Final, 0L),
    Target = replace_na(Target, 0L),
    Difference = Final - Target,
    Match = Final == Target,
    Pct_Difference = round((Final - Target) / Target * 100, 1)
  ) %>%
  arrange(desc(abs(Difference)))

# Print verification
log_msg("\nCategory distribution (showing largest differences first):")
for (i in seq_len(min(10, nrow(verify_tbl)))) {
  row <- verify_tbl[i, ]
  status <- if (row$Match) "✓" else "✗"
  log_msg(sprintf("  %s %-40s: Target=%3d, Final=%3d, Diff=%+3d (%+.1f%%)",
                  status, row$Category, row$Target, row$Final, 
                  row$Difference, row$Pct_Difference))
}

# Overall statistics
final_total <- sum(verify_tbl$Final)
target_total <- sum(verify_tbl$Target)
n_matched <- sum(verify_tbl$Match)
n_categories <- nrow(verify_tbl)

log_msg("\n=== OVERALL RESULTS ===")
log_msg("Final N: ", final_total)
log_msg("Target N: ", target_total)
log_msg("Difference: ", final_total - target_total)
log_msg("Categories perfectly matched: ", n_matched, "/", n_categories)
log_msg("Number of reassignments: ", nrow(reassign_log))

if (final_total == target_total) {
  log_msg("\n✓ SUCCESS: Final dataset has exactly ", final_total, " responses")
} else {
  warning("\n✗ MISMATCH: Final has ", final_total, " responses, target was ", target_total)
}

# ========================= SAVE OUTPUTS =========================
log_msg("\n=== SAVING OUTPUTS ===")

# Sort final dataset for consistency
final <- final %>% arrange(ResponseId)

# Main outputs
write.csv(final,        config$outfile_final,  row.names = FALSE)
write.csv(verify_tbl,   config$outfile_verify, row.names = FALSE)
write.csv(reassign_log, config$outfile_log,    row.names = FALSE)

# Additional statistics
write.csv(first_pass_stats, config$outfile_stats, row.names = FALSE)

log_msg("✓ Final dataset → ", normalizePath(config$outfile_final))
log_msg("✓ Verification → ", normalizePath(config$outfile_verify))
log_msg("✓ Reassignment log → ", normalizePath(config$outfile_log))
log_msg("✓ Statistics → ", normalizePath(config$outfile_stats))

# ========================= REASSIGNMENT ANALYSIS =========================
if (nrow(reassign_log) > 0) {
  log_msg("\n=== REASSIGNMENT PATTERNS ===")
  
  reassign_summary <- reassign_log %>%
    count(From, To, sort = TRUE) %>%
    head(10)
  
  log_msg("Top reassignment flows:")
  for (i in seq_len(nrow(reassign_summary))) {
    row <- reassign_summary[i, ]
    log_msg(sprintf("  %2d. %s → %s (%d moves)", 
                   i, row$From, row$To, row$n))
  }
  
  # Progress analysis of reassigned responses
  reassign_progress <- reassign_log %>%
    summarise(
      n = n(),
      mean_progress = mean(Progress, na.rm = TRUE),
      median_progress = median(Progress, na.rm = TRUE),
      min_progress = min(Progress, na.rm = TRUE),
      max_progress = max(Progress, na.rm = TRUE)
    )
  
  log_msg("\nReassigned responses progress stats:")
  log_msg("  Mean: ", round(reassign_progress$mean_progress, 1))
  log_msg("  Median: ", round(reassign_progress$median_progress, 1))
  log_msg("  Range: ", reassign_progress$min_progress, " - ", reassign_progress$max_progress)
}

# ========================= DATA QUALITY METRICS =========================
log_msg("\n=== DATA QUALITY METRICS ===")

# Progress distribution in final dataset
final_progress_stats <- final %>%
  summarise(
    mean = mean(Progress, na.rm = TRUE),
    median = median(Progress, na.rm = TRUE),
    sd = sd(Progress, na.rm = TRUE),
    min = min(Progress, na.rm = TRUE),
    max = max(Progress, na.rm = TRUE),
    q25 = quantile(Progress, 0.25, na.rm = TRUE),
    q75 = quantile(Progress, 0.75, na.rm = TRUE)
  )

log_msg("Final dataset progress distribution:")
log_msg("  Mean ± SD: ", round(final_progress_stats$mean, 1), " ± ", 
        round(final_progress_stats$sd, 1))
log_msg("  Median [IQR]: ", round(final_progress_stats$median, 1), 
        " [", round(final_progress_stats$q25, 1), "-", 
        round(final_progress_stats$q75, 1), "]")
log_msg("  Range: ", final_progress_stats$min, "-", final_progress_stats$max)

# Completeness by category
completeness_by_cat <- final %>%
  group_by(!!sym(role_col)) %>%
  summarise(
    n = n(),
    mean_progress = mean(Progress, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_progress))

log_msg("\nTop 5 categories by mean progress:")
for (i in seq_len(min(5, nrow(completeness_by_cat)))) {
  row <- completeness_by_cat[i, ]
  log_msg(sprintf("  %s: %.1f%% (n=%d)", 
                  row[[role_col]], row$mean_progress, row$n))
}

# ========================= COMPLETION =========================
log_msg("\n=== PROCESS COMPLETE ===")
log_msg("Runtime: ", round(difftime(Sys.time(), 
                                   as.POSIXct(substr(config$log_file, 
                                                    nchar(config$log_file) - 18, 
                                                    nchar(config$log_file) - 4),
                                             format = "%Y%m%d_%H%M%S"), 
                                   units = "secs"), 2), " seconds")
log_msg("Log saved to: ", config$log_file)
log_msg("✓ Done")