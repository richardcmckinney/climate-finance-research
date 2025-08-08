#!/usr/bin/env Rscript
# run_all.R
# Purpose: One-command, deterministic end-to-end pipeline runner + verifier.
# Assumes repo layout:
#   R/01_anonymize_data.R
#   R/02_classify_stakeholders.R
# Inputs (local, not committed): data_raw/*.csv
# Outputs (public, committed):
#   data/survey_responses_anonymized_basic.csv
#   docs/appendix_j_classification_template.csv
#   data/survey_responses_anonymized_preliminary.csv (or ..._classified.csv if final map exists)
#   data/data_dictionary.csv
#
# Flags:
#   --clean         Remove known outputs before running
#   --verify        Run verification after pipeline (default TRUE)
#   --no-install    Do not install missing packages; fail if missing
#   --quiet         Suppress verbose console logging

suppressPackageStartupMessages({
  required_pkgs <- c("readr","dplyr","tibble","digest","cli")
})

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) any(grepl(paste0("^", flag, "$"), args))
CLEAN      <- has_flag("--clean")
VERIFY     <- has_flag("--verify") || !any(grepl("^--(no-)?verify$", args))
NO_INSTALL <- has_flag("--no-install")
QUIET      <- has_flag("--quiet")

# Deterministic session
set.seed(12345)
Sys.setenv(TZ = "UTC")
try(suppressWarnings(Sys.setlocale(category = "LC_ALL", locale = "C")), silent = TRUE)
options(stringsAsFactors = FALSE, scipen = 999, readr.show_col_types = FALSE)

# Logs
if (!dir.exists("logs")) dir.create("logs", recursive = TRUE, showWarnings = FALSE)
log_file <- file.path("logs", paste0("run_all_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".log"))
if (!QUIET) message("Logging to: ", normalizePath(log_file, mustWork = FALSE))
zz <- file(log_file, open = "wt")
sink(zz, type = "output"); sink(zz, type = "message")
on.exit({ sink(type = "message"); sink(type = "output"); close(zz) }, add = TRUE)

# Dependencies
install_if_missing <- function(pkgs) {
  missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(missing) && !NO_INSTALL) install.packages(missing, repos = "https://cloud.r-project.org")
  still_missing <- pkgs[!pkgs %in% rownames(installed.packages())]
  if (length(still_missing)) stop("Missing packages: ", paste(still_missing, collapse = ", "),
                                  "\nRe-run without --no-install or install manually.")
}
if (file.exists("renv/activate.R")) source("renv/activate.R")
install_if_missing(required_pkgs)
suppressPackageStartupMessages({ library(readr); library(dplyr); library(tibble); library(digest); library(cli) })

# Paths
scripts <- c("R/01_anonymize_data.R", "R/02_classify_stakeholders.R")
inputs_dir <- "data_raw"
outputs <- list(
  basic   = "data/survey_responses_anonymized_basic.csv",
  prelim  = "data/survey_responses_anonymized_preliminary.csv",
  jtempl  = "docs/appendix_j_classification_template.csv",
  dict    = "data/data_dictionary.csv"
)

# Sanity
if (!dir.exists(inputs_dir)) stop("Required folder `", inputs_dir, "` not found. Place raw CSVs there.")
dir.create("data", showWarnings = FALSE, recursive = TRUE)
dir.create("docs", showWarnings = FALSE, recursive = TRUE)

# Optional clean
if (CLEAN) {
  cli::cli_alert("Cleaning previous outputs …")
  file.remove(unlist(outputs)[file.exists(unlist(outputs))])
  if (file.exists("docs/checksums.txt")) file.remove("docs/checksums.txt")
  if (file.exists("docs/verification_report.md")) file.remove("docs/verification_report.md")
}

# Runner
run_script <- function(path) {
  cli::cli_alert_info(paste("Running", path, "…"))
  if (!file.exists(path)) stop("Script not found: ", path)
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env, keep.source = FALSE)
  cli::cli_alert_success(paste("Completed", path))
}

# Execute
for (s in scripts) run_script(s)

# Verification
verify_outputs <- function(outputs) {
  cli::cli_h1("Verification")

  # 1) Presence
  missing <- names(outputs)[!file.exists(unlist(outputs))]
  if (length(missing)) stop("Missing expected outputs: ", paste(missing, collapse = ", "))
  cli::cli_alert_success("All expected output files are present.")

  # 2) Dictionary vs. basic schema (names & order)
  basic <- readr::read_csv(outputs$basic, show_col_types = FALSE)
  dict  <- readr::read_csv(outputs$dict,  show_col_types = FALSE)
  req_cols <- c("column_name","description")
  if (!all(req_cols %in% names(dict)))
    stop("data_dictionary.csv must contain columns: ", paste(req_cols, collapse = ", "))
  if (!identical(names(basic), dict$column_name)) {
    only_in_basic <- setdiff(names(basic), dict$column_name)
    only_in_dict  <- setdiff(dict$column_name, names(basic))
    stop(paste0(
      "Column mismatch between basic and dictionary.\n",
      if (length(only_in_basic)) paste0("- Only in basic: ", paste(only_in_basic, collapse = ", "), "\n") else "",
      if (length(only_in_dict))  paste0("- Only in dictionary: ", paste(only_in_dict,  collapse = ", "), "\n") else "",
      "- Order must match exactly."
    ))
  }
  cli::cli_alert_success("Dictionary columns exactly match anonymized_basic (names and order).")

  # 3) Preliminary integrity (no NA categories)
  prelim_path <- outputs$prelim
  if (file.exists(prelim_path)) {
    prelim <- readr::read_csv(prelim_path, show_col_types = FALSE)
    if (!"final_category_appendix_j" %in% names(prelim) &&
        !"stakeholder_category" %in% names(prelim)) {
      stop("Expected one of: final_category_appendix_j or stakeholder_category in preliminary.")
    }
    cat_col <- if ("final_category_appendix_j" %in% names(prelim)) "final_category_appendix_j" else "stakeholder_category"
    if (any(is.na(prelim[[cat_col]]))) stop("Found NA values in ", cat_col, ".")
    if ("respondent_id" %in% names(basic) && "respondent_id" %in% names(prelim)) {
      if (nrow(basic) != nrow(prelim))
        stop("Row count mismatch after join: basic=", nrow(basic), " prelim=", nrow(prelim))
    }
    cli::cli_alert_success("Preliminary dataset has valid categories and consistent row count.")
  }

  # 4) Checksums
  cs <- vapply(unlist(outputs), function(p) digest::digest(file = p, algo = "sha256"), character(1))
  checksum_df <- tibble::tibble(file = names(outputs), path = unlist(outputs), sha256 = unname(cs))
  readr::write_csv(checksum_df, "docs/checksums.txt")
  cli::cli_alert_success("Checksums written to docs/checksums.txt")

  # 5) Verification report
  ver_md <- c(
    "# Verification Report",
    paste0("- Timestamp (UTC): ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
    paste0("- Git commit: ", tryCatch(system("git rev-parse HEAD", intern = TRUE), error = function(e) "N/A")),
    "",
    "## Outputs present",
    paste0("- ", names(outputs), ": ", unname(unlist(outputs))),
    "",
    "## Checksums (sha256)",
    paste0("- ", checksum_df$file, ": ", checksum_df$sha256),
    ""
  )
  writeLines(ver_md, "docs/verification_report.md")
  cli::cli_alert_success("Verification report written to docs/verification_report.md")
  invisible(TRUE)
}
if (VERIFY) verify_outputs(outputs)

cat("\n=== SESSION INFO ===\n"); print(sessionInfo())
cli::cli_alert_success("Pipeline completed successfully.")