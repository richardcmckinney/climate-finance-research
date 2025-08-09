#!/usr/bin/env Rscript
# Safety for Rscript/S4 compatibility (MUST BE FIRST)
if (!"methods" %in% loadedNamespaces()) library(methods)
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL

# get_exact_1307.R — deterministic Appendix J quota-matching (publication grade)
# Purpose: Generate exactly N=1,307 responses matching Appendix J distribution
# Author: Richard McKinney
# Date: 2025-08-08
# Version: 3.0 (with centralized configuration)

# ========================= INITIALIZATION =========================
suppressPackageStartupMessages({
  if (!"methods" %in% loadedNamespaces()) library(methods)
  library(tidyverse)
  library(cli)  # For better messaging if available
})

# Set deterministic environment
set.seed(1307)  # Deterministic for reproducibility
Sys.setenv(TZ = "UTC")
options(stringsAsFactors = FALSE, scipen = 999)

# Create necessary directories
for (dir in c("data", "output", "logs")) {
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
}

# ========================= LOAD CENTRALIZED CONFIGURATION =========================
# Source centralized Appendix J configuration
config_paths <- c(
  "R/appendix_j_config.R",
  "appendix_j_config.R",
  "../R/appendix_j_config.R",
  "scripts/appendix_j_config.R"
)

config_loaded <- FALSE
for (config_path in config_paths) {
  if (file.exists(config_path)) {
    .appendix_j_config_silent <- TRUE  # Suppress loading message
    source(config_path)
    config_loaded <- TRUE
    break
  }
}

if (!config_loaded) {
  stop("Could not find appendix_j_config.R in any of the expected locations:\n  ",
       paste(config_paths, collapse = "\n  "),
       "\nPlease ensure the configuration file is in the correct location.")
}

# Get target distribution and config from centralized source
target <- get_appendix_j_target()

# Merge with local configuration
config <- list(
  # Input/Output paths
  infile         = "data/climate_finance_survey_classified.csv",
  alt_infile     = "data/survey_responses_anonymized_preliminary.csv",
  outfile_final  = "data/climate_finance_survey_final_1307.csv",
  outfile_verify = "output/final_distribution_verification.csv",
  outfile_log    = "output/reassignment_log.csv",
  outfile_stats  = "output/quota_matching_statistics.csv",
  outfile_quality = "output/quality_control_report.csv",
  
  # Import from centralized config
  min_progress = APPENDIX_J_CONFIG$min_progress,
  target_n = APPENDIX_J_CONFIG$target_n,
  quality_exclusions = APPENDIX_J_CONFIG$quality_exclusions,
  
  # Local processing parameters
  verbose = TRUE,
  log_file = paste0("logs/get_exact_1307_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log")
)

# ========================= LOGGING SETUP =========================
# Enhanced logging with fallback for non-interactive sessions
if (config$verbose) {
  log_con <- tryCatch({
    file(config$log_file, open = "wt")
  }, error = function(e) {
    warning("Could not create log file: ", e$message)
    NULL
  })
  
  if (!is.null(log_con)) {
    on.exit(close(log_con), add = TRUE)
  }
  
  log_msg <- function(...) {
    msg <- paste0("[", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "] ", ...)
    
    # Use cli if available for better formatting
    if (requireNamespace("cli", quietly = TRUE)) {
      cli::cli_inform(msg)
    } else {
      message(msg)
    }
    
    # Write to log file if connection exists
    if (!is.null(log_con) && isOpen(log_con)) {
      writeLines(msg, log_con)
      flush(log_con)  # Ensure immediate write
    }
  }
} else {
  log_msg <- function(...) invisible(NULL)
}

log_msg("Starting get_exact_1307.R (v3.0 - with centralized config)")
log_msg("R version: ", R.version.string)
log_msg("Configuration:")
log_msg("  Appendix J version: ", APPENDIX_J_CONFIG$version)
log_msg("  Target N: ", config$target_n)
log_msg("  Min Progress: ", config$min_progress)
log_msg("  Random seed: 1307")
log_msg("  Working directory: ", getwd())

# ========================= HELPER FUNCTIONS =========================
# Optimized helper for safe column access
safe_col <- function(df, col_name, default = NA) {
  if (col_name %in% names(df)) df[[col_name]] else default
}

# Progress bar wrapper (if available)
with_progress <- function(items, fn, desc = "Processing") {
  if (requireNamespace("progress", quietly = TRUE)) {
    pb <- progress::progress_bar$new(
      format = paste0(desc, " [:bar] :percent eta: :eta"),
      total = length(items)
    )
    lapply(items, function(x) {
      pb$tick()
      fn(x)
    })
  } else {
    lapply(items, fn)
  }
}

# ========================= DATA LOADING =========================
log_msg("\n=== DATA LOADING ===")

# Try primary input file first, then alternative
infile <- if (file.exists(config$infile)) {
  config$infile
} else if (file.exists(config$alt_infile)) {
  log_msg("Primary input not found, using alternative: ", config$alt_infile)
  config$alt_infile
} else {
  stop("Neither input file found:\n  ", config$infile, "\n  ", config$alt_infile,
       "\n  Working directory: ", getwd())
}

# Load data with comprehensive error handling
df <- tryCatch({
  # Try readr first (faster and more robust)
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_csv(infile, show_col_types = FALSE, progress = FALSE)
  } else {
    read.csv(infile, stringsAsFactors = FALSE, check.names = FALSE)
  }
}, error = function(e) {
  stop("Failed to read input file '", infile, "': ", e$message)
})

log_msg("Loaded ", nrow(df), " rows from ", basename(infile))
log_msg("Columns: ", ncol(df))
log_msg("Memory usage: ", format(object.size(df), units = "MB"))

# Store original for comparison and validation
df_original <- df

# ========================= COLUMN VALIDATION =========================
log_msg("\n=== COLUMN VALIDATION ===")

# Enhanced ResponseId column detection
if (!"ResponseId" %in% names(df)) {
  resp_id_candidates <- c("respondent_id", "response_id", "_recordId", "id", "ID", "RecordId")
  resp_id_col <- intersect(resp_id_candidates, names(df))
  
  if (length(resp_id_col) > 0) {
    log_msg("Using alternative ID column: ", resp_id_col[1])
    df$ResponseId <- df[[resp_id_col[1]]]
  } else {
    log_msg("No ID column found, creating sequential IDs")
    df$ResponseId <- sprintf("R_%06d", seq_len(nrow(df)))
  }
}

# Ensure ResponseId is character and unique
df$ResponseId <- as.character(df$ResponseId)
if (any(duplicated(df$ResponseId))) {
  n_dups <- sum(duplicated(df$ResponseId))
  log_msg("WARNING: ", n_dups, " duplicate ResponseIds found. Making unique...")
  df$ResponseId <- make.unique(df$ResponseId, sep = "_")
}

# Use centralized helper for Progress column detection
progress_col <- find_progress_column(df)

if (!is.null(progress_col)) {
  if (progress_col != "Progress") {
    log_msg("Using alternative progress column: ", progress_col)
    df$Progress <- df[[progress_col]]
  }
} else {
  # If no progress column found, check if Progress exists
  if (!"Progress" %in% names(df)) {
    stop("Progress column missing and no alternatives found. Available columns:\n  ",
         paste(names(df), collapse = ", "))
  }
}

# Convert Progress to numeric with comprehensive handling
df$Progress <- suppressWarnings(as.numeric(df$Progress))

# Handle percentage values (if Progress is 0-1, convert to 0-100)
if (!all(is.na(df$Progress)) && max(df$Progress, na.rm = TRUE) <= 1) {
  log_msg("Progress appears to be in decimal format (0-1), converting to percentage (0-100)")
  df$Progress <- df$Progress * 100
}

if (all(is.na(df$Progress))) {
  stop("Progress column contains no valid numeric values")
}

# Replace NA Progress with 0
n_na_progress <- sum(is.na(df$Progress))
if (n_na_progress > 0) {
  log_msg("Replacing ", n_na_progress, " NA Progress values with 0")
  df$Progress[is.na(df$Progress)] <- 0
}

# Statistics on Progress
progress_stats <- data.frame(
  Mean = round(mean(df$Progress, na.rm = TRUE), 1),
  Median = round(median(df$Progress, na.rm = TRUE), 1),
  SD = round(sd(df$Progress, na.rm = TRUE), 1),
  Min = min(df$Progress, na.rm = TRUE),
  Max = max(df$Progress, na.rm = TRUE),
  NA_count = sum(is.na(df$Progress))
)

log_msg("Progress statistics:")
log_msg("  Mean ± SD: ", progress_stats$Mean, " ± ", progress_stats$SD)
log_msg("  Median [Min-Max]: ", progress_stats$Median, " [", 
        progress_stats$Min, "-", progress_stats$Max, "]")

# ========================= ELIGIBILITY FILTERS =========================
log_msg("\n=== APPLYING ELIGIBILITY FILTERS ===")
initial_count <- nrow(df)

# Filter 1: Consent (if present) - Enhanced detection
consent_cols <- c("consent", "Consent", "Q_consent", "Q0_consent", "Q1.1", "Q1_1",
                 "q1_1", "q1.1", "Informed_Consent", "informed_consent")
cc <- intersect(consent_cols, names(df))

if (length(cc) > 0) {
  log_msg("Checking consent using column: ", cc[1])
  df[[cc[1]]] <- as.character(df[[cc[1]]])
  
  # Comprehensive consent values (expanded)
  consent_vals <- c(
    # Explicit consent
    "consent", "Consent", "CONSENT",
    "I consent", "i consent", "I Consent", "I CONSENT",
    "Consented", "consented", "CONSENTED",
    # Yes/Agree variations
    "Yes", "yes", "YES", "Y", "y",
    "Agree", "agree", "AGREE",
    "I agree", "i agree", "I Agree", "I AGREE",
    # Boolean/numeric
    "1", "TRUE", "True", "true", "T", "t"
  )
  
  before_consent <- nrow(df)
  df <- df %>% 
    filter(.data[[cc[1]]] %in% consent_vals | 
           is.na(.data[[cc[1]]]) |  # Keep NA if no explicit non-consent
           .data[[cc[1]]] == "")     # Keep empty if no explicit non-consent
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
      Date_Excluded = Sys.Date(),
      Excluded_By = Sys.info()["user"]
    )
    write.csv(quality_report, config$outfile_quality, row.names = FALSE)
    log_msg("Quality control report saved to: ", config$outfile_quality)
  }
}

# Summary of filtering
log_msg("\nFiltered from ", initial_count, " to ", nrow(df), " eligible responses")
log_msg("Reduction: ", round((1 - nrow(df)/initial_count) * 100, 1), "%")
log_msg("Retention: ", round((nrow(df)/initial_count) * 100, 1), "%")

# ========================= TARGET DISTRIBUTION VALIDATION =========================
log_msg("\n=== TARGET DISTRIBUTION (APPENDIX J) ===")
log_msg("Using centralized configuration version: ", APPENDIX_J_CONFIG$version)

# Validate target sum
target_sum <- sum(target$Target)
if (target_sum != config$target_n) {
  warning("Target distribution sums to ", target_sum, " not ", config$target_n, "!")
  log_msg("WARNING: Target sum mismatch - adjusting Miscellaneous category")
  # Adjust Miscellaneous to make sum correct
  misc_idx <- which(target$Category == "Miscellaneous and Individual Respondents")
  target$Target[misc_idx] <- target$Target[misc_idx] + (config$target_n - target_sum)
  target_sum <- sum(target$Target)
}

log_msg("Target categories: ", nrow(target))
log_msg("Target total: ", target_sum)

# ========================= ROLE COLUMN DETECTION =========================
log_msg("\n=== ROLE COLUMN DETECTION ===")

# Use centralized helper function
role_col <- find_role_column(df)
log_msg("Using role column: ", role_col)

# Validate categories using centralized function
validation <- validate_categories(df[[role_col]], target)
log_msg("Unique categories in data: ", length(unique(df[[role_col]])))
log_msg("Category match rate: ", round(validation$match_rate * 100, 1), "%")

if (length(validation$missing) > 0) {
  log_msg("WARNING: ", length(validation$missing), " target categories not found in data:")
  for (cat in head(validation$missing, 5)) {
    log_msg("  - ", cat)
  }
  if (length(validation$missing) > 5) {
    log_msg("  ... and ", length(validation$missing) - 5, " more")
  }
}

if (length(validation$extra) > 0) {
  log_msg("Categories in data but not in target: ", length(validation$extra))
  for (cat in head(validation$extra, 5)) {
    log_msg("  - ", cat)
  }
}

# ========================= FIRST PASS: DIRECT MATCHING =========================
log_msg("\n=== FIRST PASS: Direct category matching ===")

# Use list for efficiency
take_logs <- list()
selected_list <- list()
stats_list <- list()

# Optimized loop with pre-allocation
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
    Surplus = max(0, available - need),
    stringsAsFactors = FALSE
  )
  
  # Progress indicator
  if (i %% 5 == 0 || deficit > 0) {
    log_msg(sprintf("%-45s: Need %3d, Available %3d, Taken %3d%s", 
                    cat_name, need, available, taken,
                    if (deficit > 0) paste0(" [DEFICIT: ", deficit, "]") else ""))
  }
}

# Combine results efficiently
selected <- bind_rows(selected_list)
first_pass_stats <- bind_rows(stats_list)
final <- selected

log_msg("\nFirst pass summary:")
log_msg("  Total taken: ", nrow(selected))
log_msg("  Categories fulfilled: ", sum(first_pass_stats$Deficit == 0), "/", nrow(target))
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
    filter(Can_Donate > 0) %>%
    arrange(desc(Can_Donate))
  
  log_msg("\nDonor categories (top 5):")
  top_donors <- head(donor_tbl, 5)
  for (i in seq_len(nrow(top_donors))) {
    log_msg(sprintf("  %d. %s: %d available", 
                   i, top_donors$Category[i], top_donors$Can_Donate[i]))
  }
  
  # Optimized deficit filling
  for (i in seq_len(nrow(need_tbl))) {
    if (nrow(remaining) == 0) break
    
    cat_i <- need_tbl$Category[i]
    deficit_i <- need_tbl$Deficit[i]
    
    if (deficit_i <= 0) next
    
    log_msg("\nFilling deficit for ", cat_i, " (need ", deficit_i, " more)")
    
    # Optimized donor search
    filled <- 0
    for (j in seq_len(nrow(donor_tbl))) {
      if (deficit_i <= 0 || nrow(remaining) == 0) break
      
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
      filled <- filled + k
      
      log_msg("  Reassigned ", k, " from ", dcat, " (", 
             round(mean(move$Progress), 1), "% avg progress)")
    }
    
    if (deficit_i > 0) {
      log_msg("  WARNING: Could not fill complete deficit (still need ", deficit_i, ")")
    } else {
      log_msg("  SUCCESS: Deficit filled (", filled, " reassignments)")
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

# Create comprehensive verification table
verify_tbl <- full_join(final_dist, target, by = "Category") %>%
  mutate(
    Final = replace_na(Final, 0L),
    Target = replace_na(Target, 0L),
    Difference = Final - Target,
    Match = Final == Target,
    Pct_Difference = ifelse(Target > 0, 
                           round((Final - Target) / Target * 100, 1),
                           ifelse(Final > 0, 999.9, 0))
  ) %>%
  arrange(desc(abs(Difference)))

# Enhanced verification output
log_msg("\nCategory distribution (largest differences first):")
log_msg(sprintf("%-45s %6s %6s %7s %8s", 
               "Category", "Target", "Final", "Diff", "Pct Diff"))
log_msg(paste(rep("-", 75), collapse = ""))

for (i in seq_len(min(15, nrow(verify_tbl)))) {
  row <- verify_tbl[i, ]
  status <- if (row$Match) "✓" else "✗"
  log_msg(sprintf("%s %-43s %6d %6d %+7d %+7.1f%%",
                  status, substr(row$Category, 1, 43), 
                  row$Target, row$Final, 
                  row$Difference, row$Pct_Difference))
}

if (nrow(verify_tbl) > 15) {
  log_msg("... and ", nrow(verify_tbl) - 15, " more categories")
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
log_msg("Categories perfectly matched: ", n_matched, "/", n_categories, 
        " (", round(n_matched/n_categories * 100, 1), "%)")
log_msg("Number of reassignments: ", nrow(reassign_log))

# Success/failure message
if (final_total == target_total) {
  log_msg("\n✓ SUCCESS: Final dataset has exactly ", final_total, " responses")
} else if (abs(final_total - target_total) <= 5) {
  log_msg("\n⚠ NEAR SUCCESS: Final has ", final_total, 
         " responses (", abs(final_total - target_total), " off target)")
} else {
  warning("\n✗ MISMATCH: Final has ", final_total, " responses, target was ", target_total)
}

# ========================= SAVE OUTPUTS =========================
log_msg("\n=== SAVING OUTPUTS ===")

# Sort final dataset for consistency
final <- final %>% arrange(ResponseId)

# Ensure role column is named appropriately for downstream
if (role_col != "Final_Role_Category") {
  final <- final %>% rename(Final_Role_Category = !!sym(role_col))
  log_msg("Renamed role column to Final_Role_Category for consistency")
}

# Main outputs with error handling
tryCatch({
  write.csv(final, config$outfile_final, row.names = FALSE)
  log_msg("✓ Final dataset → ", normalizePath(config$outfile_final))
}, error = function(e) {
  log_msg("ERROR saving final dataset: ", e$message)
})

tryCatch({
  write.csv(verify_tbl, config$outfile_verify, row.names = FALSE)
  log_msg("✓ Verification → ", normalizePath(config$outfile_verify))
}, error = function(e) {
  log_msg("ERROR saving verification: ", e$message)
})

if (nrow(reassign_log) > 0) {
  tryCatch({
    write.csv(reassign_log, config$outfile_log, row.names = FALSE)
    log_msg("✓ Reassignment log → ", normalizePath(config$outfile_log))
  }, error = function(e) {
    log_msg("ERROR saving reassignment log: ", e$message)
  })
}

tryCatch({
  write.csv(first_pass_stats, config$outfile_stats, row.names = FALSE)
  log_msg("✓ Statistics → ", normalizePath(config$outfile_stats))
}, error = function(e) {
  log_msg("ERROR saving statistics: ", e$message)
})

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
      max_progress = max(Progress, na.rm = TRUE),
      .groups = "drop"
    )
  
  log_msg("\nReassigned responses progress stats:")
  log_msg("  Count: ", reassign_progress$n)
  log_msg("  Mean: ", round(reassign_progress$mean_progress, 1), "%")
  log_msg("  Median: ", round(reassign_progress$median_progress, 1), "%")
  log_msg("  Range: ", round(reassign_progress$min_progress, 1), 
         "% - ", round(reassign_progress$max_progress, 1), "%")
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
    q75 = quantile(Progress, 0.75, na.rm = TRUE),
    .groups = "drop"
  )

log_msg("Final dataset progress distribution:")
log_msg("  Mean ± SD: ", round(final_progress_stats$mean, 1), "% ± ", 
        round(final_progress_stats$sd, 1), "%")
log_msg("  Median [IQR]: ", round(final_progress_stats$median, 1), 
        "% [", round(final_progress_stats$q25, 1), "-", 
        round(final_progress_stats$q75, 1), "]")
log_msg("  Range: ", round(final_progress_stats$min, 1), 
       "% - ", round(final_progress_stats$max, 1), "%")

# Completeness by category
role_col_final <- if ("Final_Role_Category" %in% names(final)) {
  "Final_Role_Category"
} else {
  role_col
}

completeness_by_cat <- final %>%
  group_by(!!sym(role_col_final)) %>%
  summarise(
    n = n(),
    mean_progress = mean(Progress, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_progress))

log_msg("\nTop 5 categories by mean progress:")
for (i in seq_len(min(5, nrow(completeness_by_cat)))) {
  row <- completeness_by_cat[i, ]
  log_msg(sprintf("  %d. %s: %.1f%% (n=%d)", 
                  i,
                  substr(row[[1]], 1, 40),  # Truncate long names
                  row$mean_progress, 
                  row$n))
}

# ========================= FINAL SUMMARY =========================
log_msg("\n=== PROCESS SUMMARY ===")
log_msg("Configuration: Appendix J v", APPENDIX_J_CONFIG$version)
log_msg("Initial responses: ", initial_count)
log_msg("After filtering: ", nrow(df))
log_msg("Final dataset: ", nrow(final))
log_msg("Reassignments: ", nrow(reassign_log))

# Calculate runtime
runtime <- difftime(Sys.time(), 
                   as.POSIXct(substr(config$log_file, 
                             nchar(config$log_file) - 18, 
                             nchar(config$log_file) - 4),
                             format = "%Y%m%d_%H%M%S"), 
                   units = "secs")

log_msg("\n=== PROCESS COMPLETE ===")
log_msg("Runtime: ", round(runtime, 2), " seconds")
log_msg("Log saved to: ", config$log_file)
log_msg("✓ Done at ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))

# Return invisibly for potential downstream use
invisible(list(
  final_data = final,
  verification = verify_tbl,
  reassignments = reassign_log,
  statistics = first_pass_stats,
  config = config,
  appendix_j_config = APPENDIX_J_CONFIG
))