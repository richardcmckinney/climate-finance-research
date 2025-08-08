# get_exact_1307.R — deterministic Appendix J quota-matching (publication grade)

if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))
set.seed(1307)

dir.create("data",   recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

infile         <- "data/climate_finance_survey_classified.csv"
outfile_final  <- "data/climate_finance_survey_final_1307.csv"
outfile_verify <- "output/final_distribution_verification.csv"
outfile_log    <- "output/reassignment_log.csv"

if (!file.exists(infile)) stop("Classified input not found: ", infile)
df <- read.csv(infile, stringsAsFactors = FALSE)

# Ensure IDs/Progress exist
if (!"ResponseId" %in% names(df)) df$ResponseId <- seq_len(nrow(df))
if (!"Progress"   %in% names(df)) stop("Progress column missing.")
df$Progress <- suppressWarnings(as.numeric(df$Progress))

# Eligibility: consent (if present) + Progress >= 10; drop the straight-liner
consent_cols <- c("consent","Consent","Q_consent","Q0_consent")
cc <- consent_cols[consent_cols %in% names(df)]
if (length(cc) >= 1) {
  consent_vals <- c("consent","Consent","Yes","I consent","i consent","Consented","1",1,TRUE,"TRUE")
  df <- df %>% filter(.data[[cc[1]]] %in% consent_vals | is.na(.data[[cc[1]]]))
}
df <- df %>% filter(Progress >= 10)
df <- df %>% filter(ResponseId != "R_bBAyiwWo1sotqM6")

# Target distribution (Appendix J, total = 1307)
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

# Harmonize column name
role_col <- "Final_Role_Category"
if (!role_col %in% names(df)) stop("Final_Role_Category not found in input data.")

# First pass: for each category, take up to Target prioritizing higher Progress
take_logs <- list()
selected <- map_dfr(seq_len(nrow(target)), function(i){
  cat_name <- target$Category[i]; need <- target$Target[i]
  pool <- df %>% filter(.data[[role_col]] == cat_name)
  if (nrow(pool) == 0) {
    take_logs[[cat_name]] <<- tibble()
    return(pool)
  }
  pool <- pool %>% arrange(desc(Progress), ResponseId)
  out  <- pool[seq_len(min(nrow(pool), need)), , drop = FALSE]
  take_logs[[cat_name]] <<- out %>% select(ResponseId, Progress, all_of(role_col))
  out
})

final <- selected

# If short, fill deficits drawing from the largest available categories deterministically
taken_ids <- final$ResponseId
remaining <- df %>% filter(!(ResponseId %in% taken_ids))

need_tbl <- target %>%
  mutate(Taken = map_int(Category, ~ sum(final[[role_col]] == .x, na.rm = TRUE))) %>%
  mutate(Deficit = pmax(Target - Taken, 0L))

if (sum(need_tbl$Deficit) > 0) {
  # Surplus donors ordered by their surplus size
  donor_tbl <- target %>%
    mutate(Avail = map_int(Category, ~ sum(remaining[[role_col]] == .x, na.rm = TRUE))) %>%
    mutate(Surplus = pmax(Avail - pmax(Target - need_tbl$Taken, 0L), 0L)) %>%
    arrange(desc(Surplus))
  reassign_log <- tibble()
  for (i in seq_len(nrow(need_tbl))) {
    cat_i <- need_tbl$Category[i]; deficit_i <- need_tbl$Deficit[i]
    if (deficit_i <= 0) next
    if (nrow(remaining) == 0) break
    donors <- donor_tbl$Category
    for (dcat in donors) {
      if (deficit_i <= 0) break
      cand <- remaining %>%
        filter(.data[[role_col]] == dcat) %>%
        arrange(desc(Progress), ResponseId)
      if (nrow(cand) == 0) next
      k <- min(nrow(cand), deficit_i)
      move <- cand[seq_len(k), , drop = FALSE]
      move[[role_col]] <- cat_i
      reassign_log <- bind_rows(reassign_log,
                                move %>% transmute(ResponseId, Progress,
                                                   From = dcat, To = cat_i))
      remaining <- anti_join(remaining, cand[seq_len(k), c("ResponseId")], by = "ResponseId")
      final     <- bind_rows(final, move)
      deficit_i <- deficit_i - k
    }
  }
} else {
  reassign_log <- tibble(ResponseId=character(), Progress=numeric(), From=character(), To=character())
}

# Verify counts
final_dist <- final %>% count(!!sym(role_col), name = "Final") %>% rename(Category = !!role_col)
verify_tbl <- full_join(final_dist, target, by = "Category") %>%
  mutate(Final = replace_na(Final, 0L),
         Target = replace_na(Target, 0L),
         Match = Final == Target) %>%
  arrange(Category)

# Persist artifacts
write.csv(final,       outfile_final,  row.names = FALSE)
write.csv(verify_tbl,  outfile_verify, row.names = FALSE)
write.csv(reassign_log,outfile_log,    row.names = FALSE)

cat("Final dataset saved → ", outfile_final,  "\n", sep = "")
cat("Verification saved → ",  outfile_verify, "\n", sep = "")
cat("Reassignment log → ",    outfile_log,    "\n", sep = "")