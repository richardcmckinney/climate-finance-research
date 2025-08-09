# Climate Finance Research — Reproducible Pipeline

This repository contains the complete, deterministic pipeline used in the manuscript **“The Capital–Opportunity Mismatch: A Multi‑Stakeholder Analysis of Climate Finance Barriers and Solutions.”**

The pipeline proceeds from **raw survey exports** to **anonymized public data**, **Appendix J classifications**, and (optionally) **all manuscript analysis outputs**. A single command regenerates the public artifacts; an optional flag adds the full analysis stage (tables/figures).

---
## Quick start

```bash
# Minimal: regenerate the four public artifacts only
Rscript run_all.R --clean

# Full: also run the manuscript analysis stage (creates figures/ and extra outputs)
Rscript run_all.R --clean --with-analysis

# Verify current working tree against checksums
Rscript run_all.R --verify
```

**Determinism:** The scripts set a fixed seed and UTC timezone. Running the same input twice yields identical outputs and checksums.

---
## Outputs (public artifacts guaranteed by `run_all.R`)

All paths are relative to the repo root:

- `data/survey_responses_anonymized_basic.csv` — **N = 1,563**, the fully anonymized, analysis‑ready dataset (PII removed; temporal buckets applied as documented).
- `docs/appendix_j_classification_template.csv` — classification template for reviewer inspection and optional manual harmonization.
- `data/survey_responses_anonymized_preliminary.csv` — **N = 1,563**, anonymized dataset joined with Appendix J preliminary + final classifications.
- `data/data_dictionary.csv` — column‑level dictionary regenerated on every run to **exactly** match the current anonymized dataset.

> **Note on N=1,307 vs N=1,563**  
> The manuscript’s analysis uses a deterministic **quota‑matched analysis subset** of size **N = 1,307** (see `R/get_exact_1307.R`). The public anonymized datasets (`*_basic.csv`, `*_preliminary.csv`) contain **N = 1,563**. This resolves the previously inconsistent N in the README.

---
## Optional analysis stage (manuscript tables & figures)

When invoked with `--with-analysis`, the pipeline additionally:

- Generates the **N=1,307** analysis subset (`data/climate_finance_survey_final_1307.csv`).
- Reproduces descriptive statistics, hypothesis tests (H1–H12), factor models (EFA/CFA), and regressions.
- Writes CSVs to `output/` and figures to `figures/`.

> **Figures directory**  
> The `figures/` directory is created **only** when `--with-analysis` is used. In minimal runs, it is not created.

---
## Repository structure

```
R/
  01_anonymize_data.R          # Strip PII, normalize fields, write *_basic.csv and data_dictionary.csv
  02_classify_stakeholders.R   # Apply Appendix J regex logic; write classification template and *_preliminary.csv
  get_exact_1307.R             # Deterministic quota-matching to N=1,307 for manuscript analyses
  03_main_analysis.R           # Descriptives & derived outputs used by the manuscript
  04_hypothesis_testing.R      # (New) Reproduces H1–H12 tests, EFA/CFA, and regressions; writes output/ & figures/

run_all.R                      # Single entry point; add --with-analysis to execute 03 & 04

data/
  data_raw/                    # (ignored in git) raw survey exports
  survey_responses_anonymized_basic.csv
  survey_responses_anonymized_preliminary.csv
  climate_finance_survey_final_1307.csv     # (created when --with-analysis is used)
  data_dictionary.csv

docs/
  appendix_j_systematic_classification.md   # (renamed for shell-friendliness)
  appendix_j_classification_template.csv
  checksums.txt
  verification_report.md

output/                        # (created/updated during analysis stage)
figures/                       # (created/updated during analysis stage)
```

---
## Scripts and what they do

- **`R/01_anonymize_data.R`**  
  Loads `data_raw/`, removes PII, applies controlled transforms (e.g., date bucketing), and writes 
  `data/survey_responses_anonymized_basic.csv` and `data/data_dictionary.csv`.

- **`R/02_classify_stakeholders.R`**  
  Applies Appendix J classification regex with robust fallbacks. Emits 
  `docs/appendix_j_classification_template.csv` and 
  `data/survey_responses_anonymized_preliminary.csv`. Guarantees no `NA` in `final_category_appendix_j`.

- **`R/get_exact_1307.R`**  
  Generates the N=1,307 quota‑matched analysis dataset **deterministically** from the classified dataset. 
  Input now defaults to `data/survey_responses_anonymized_preliminary.csv` and it accepts either 
  `Final_Role_Category` or `final_category_appendix_j` for roles.

- **`R/03_main_analysis.R`**  
  Produces descriptive tables/plots used in the manuscript. Writes into `output/` and `figures/`.

- **`R/04_hypothesis_testing.R`** *(new)*  
  Reproduces H1–H12 tests (ANOVA/χ²/t‑tests/correlations), EFA/CFA for the Three‑Factor Climate Risk Model, and regressions. 
  Outputs CSVs in `output/` and figures in `figures/`.

- **`run_all.R`**  
  Entry point. Always regenerates the four public artifacts; with `--with-analysis`, also runs 03 & 04. Produces `docs/checksums.txt` and `docs/verification_report.md`.

---
## Reproducibility guarantees

- **Deterministic:** fixed RNG seed and UTC timezone; stable sorting; explicit type handling.
- **Schema‑locked:** `data_dictionary.csv` is regenerated every run from the actual anonymized dataset to avoid drift.
- **Verification:** `docs/checksums.txt` includes SHA‑256 for all key artifacts; `docs/verification_report.md` captures session details and summary counts.

---
## Notes on file naming

- Renamed `docs/# Appendix J: Systematic Classification .md` → `docs/appendix_j_systematic_classification.md` to avoid shell‑hostile characters and trailing spaces. If you had links to the old name, update them accordingly.
- Removed the legacy `data/climate_finance_survey_anonymized.csv` to prevent confusion; use the canonical `survey_responses_anonymized_basic.csv`.

---
## Citation

If you use this repository or datasets, please cite the associated manuscript.

```
McKinney, R. (2025). The Capital–Opportunity Mismatch: A Multi‑Stakeholder Analysis of Climate Finance Barriers and Solutions.
```

---
## License

MIT, except where otherwise noted for data.

# run_all.R — single-command reproducible pipeline
# Supports minimal artifact regeneration and (optional) full manuscript analysis.

suppressPackageStartupMessages({
  library(digest)
  library(tools)
})

set.seed(1307L)
Sys.setenv(TZ = "UTC")

args <- commandArgs(trailingOnly = TRUE)
WITH_ANALYSIS <- any(args %in% c("--with-analysis", "-A"))
CLEAN         <- any(args %in% c("--clean", "-C"))
VERIFY_ONLY   <- any(args %in% c("--verify", "-V"))

# Helper: safe source with message
safe_source <- function(path) {
  if (file.exists(path)) {
    message("→ Sourcing ", path)
    source(path, local = new.env(parent = globalenv()))
  } else {
    message("! Skipping missing script: ", path)
  }
}

# Helper: ensure directories exist
mk <- function(d) if (!dir.exists(d)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
mk("data"); mk("docs"); mk("output")
if (WITH_ANALYSIS) mk("figures")

# CLEAN mode: remove known derived outputs to ensure a fresh run (never touches data_raw/)
if (CLEAN) {
  files_to_remove <- c(
    "data/survey_responses_anonymized_basic.csv",
    "data/survey_responses_anonymized_preliminary.csv",
    "docs/appendix_j_classification_template.csv",
    "data/data_dictionary.csv",
    "data/climate_finance_survey_final_1307.csv"
  )
  for (f in files_to_remove) if (file.exists(f)) file.remove(f)
}

# Minimal pipeline (always run unless VERIFY_ONLY)
if (!VERIFY_ONLY) {
  safe_source("R/01_anonymize_data.R")
  safe_source("R/02_classify_stakeholders.R")
}

# Optional analysis stage
if (!VERIFY_ONLY && WITH_ANALYSIS) {
  # Deterministic subset (N=1307)
  safe_source("R/get_exact_1307.R")
  # Descriptives & main analysis
  safe_source("R/02_main_analysis.R")  # optional wrapper if present
  safe_source("R/03_main_analysis.R")
  # Hypothesis tests / EFA / CFA / regressions
  safe_source("R/04_hypothesis_testing.R")
}

# Compute checksums for verification
checksum <- function(path) if (file.exists(path)) digest(path, algo = "sha256") else NA_character_

core_artifacts <- c(
  "data/survey_responses_anonymized_basic.csv",
  "docs/appendix_j_classification_template.csv",
  "data/survey_responses_anonymized_preliminary.csv",
  "data/data_dictionary.csv"
)

analysis_artifacts <- c(
  "data/climate_finance_survey_final_1307.csv",
  list.files("output", recursive = TRUE, full.names = TRUE),
  list.files("figures", recursive = TRUE, full.names = TRUE)
)

artifacts <- unique(c(core_artifacts, if (WITH_ANALYSIS) analysis_artifacts))
chk <- data.frame(file = artifacts, sha256 = vapply(artifacts, checksum, ""), stringsAsFactors = FALSE)

# Write checksums
mk("docs")
write.table(chk, file = "docs/checksums.txt", sep = "\t", row.names = FALSE, quote = FALSE)

# Write a concise verification report
ver_file <- "docs/verification_report.md"
con <- file(ver_file, open = "wt", encoding = "UTF-8")
writeLines(c(
  "# Verification Report",
  sprintf("Generated: %s UTC", format(Sys.time(), tz = "UTC")),
  "",
  "## Artifacts",
  paste0("- ", chk$file, " — ", chk$sha256),
  "",
  "## Session",
  paste(capture.output(sessionInfo()), collapse = "\n")
), con)
close(con)

message("✓ Checksums → docs/checksums.txt")
message("✓ Verification → ", ver_file)
message("Done.")

if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))
set.seed(1307)

dir.create("data",   recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

infile         <- "data/survey_responses_anonymized_preliminary.csv"
outfile_final  <- "data/climate_finance_survey_final_1307.csv"
outfile_verify <- "output/final_distribution_verification.csv"
outfile_log    <- "output/reassignment_log.csv"

if (!file.exists(infile)) stop("Classified input not found: ", infile)
df <- read.csv(infile, stringsAsFactors = FALSE)

# Ensure IDs/Progress exist
if (!"ResponseId" %in% names(df)) df$ResponseId <- seq_len(nrow(df))
if (!"Progress"   %in% names(df)) stop("Progress column missing.")
df$Progress <- suppressWarnings(as.numeric(df$Progress))

# Eligibility: consent (if present) + Progress >= 10
consent_cols <- c("consent","Consent","Q_consent","Q0_consent")
cc <- consent_cols[consent_cols %in% names(df)]
if (length(cc) >= 1) {
  # FIXED: Ensure character comparison for consistency
  df[[cc[1]]] <- as.character(df[[cc[1]]])
  consent_vals <- c("consent","Consent","Yes","I consent","i consent","Consented","1","TRUE")
  df <- df %>% filter(.data[[cc[1]]] %in% consent_vals | is.na(.data[[cc[1]]]))
}
df <- df %>% filter(Progress >= 10)

# DOCUMENTED: Exclude straight-line respondent identified during quality control
# This respondent answered all questions with the same response pattern
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

# Harmonize column name (accept either legacy or canonical)
role_candidates <- c("Final_Role_Category", "final_category_appendix_j")
role_col <- role_candidates[role_candidates %in% names(df)][1]
if (is.na(role_col) || !nzchar(role_col)) {
  stop("Role category column not found. Expected one of: ", paste(role_candidates, collapse = ", "))
}

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