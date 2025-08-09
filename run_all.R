#!/usr/bin/env Rscript
suppressPackageStartupMessages(library(methods))
if (!exists(".local", inherits = TRUE)) .local <- function(...) NULL
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
#   --clean           Remove known outputs before running
#   --verify          Run verification after pipeline (default TRUE)
#   --no-install      Do not install missing packages; fail if missing
#   --quiet           Suppress verbose console logging
#   --with-analysis   Also run analysis stage (1307 subset + 03/04 scripts; creates output/ and figures/)

suppressPackageStartupMessages({
  if (!"methods" %in% loadedNamespaces()) library(methods)
  required_pkgs <- c("readr","dplyr","tibble","digest","cli")
  analysis_pkgs <- c("psych","lavaan","MASS","ggplot2","reshape2","corrplot","broom","effectsize","pwr","gridExtra")
})

args <- commandArgs(trailingOnly = TRUE)
has_flag <- function(flag) any(grepl(paste0("^", flag, "$"), args))
CLEAN      <- has_flag("--clean")
VERIFY     <- has_flag("--verify") || !any(grepl("^--(no-)?verify$", args))
NO_INSTALL <- has_flag("--no-install")
QUIET      <- has_flag("--quiet")
WITH_ANALYSIS <- has_flag("--with-analysis")

# add near the top of run_all.R
options(
  error = function(e) {
    dir.create("docs", showWarnings = FALSE, recursive = TRUE)
    cur <- getOption("run_all_current_script")
    cat("ERROR: ", conditionMessage(e), "\n\n", file = "docs/last_error.txt")
    if (!is.null(cur)) cat("While running: ", cur, "\n\n", file = "docs/last_error.txt", append = TRUE)
    cat(paste(capture.output(traceback(30)), collapse = "\n"), file = "docs/last_error.txt", append = TRUE)
    quit(status = 1)
  }
)

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

# Ensure analysis packages are installed/loaded if WITH_ANALYSIS
if (WITH_ANALYSIS) install_if_missing(analysis_pkgs)
if (WITH_ANALYSIS) suppressPackageStartupMessages({ library(psych); library(lavaan); library(MASS); library(ggplot2); library(reshape2) })

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
dir.create("output", showWarnings = FALSE, recursive = TRUE)
if (WITH_ANALYSIS) dir.create("figures", showWarnings = FALSE, recursive = TRUE)

# Optional clean
if (CLEAN) {
  cli::cli_alert("Cleaning previous outputs …")
  file.remove(unlist(outputs)[file.exists(unlist(outputs))])
  if (file.exists("docs/checksums.txt")) file.remove("docs/checksums.txt")
  if (file.exists("docs/verification_report.md")) file.remove("docs/verification_report.md")
}

# Runner
run_script <- function(path, required = TRUE) {
  options(run_all_current_script = path)
  cli::cli_alert_info(paste("Running", path, "…"))
  if (!file.exists(path)) {
    msg <- paste0("Script not found: ", path)
    if (required) stop(msg) else { cli::cli_alert_warning(msg); return(invisible(FALSE)) }
  }
  suppressPackageStartupMessages(library(methods))
  # ensure S4 .local exists in the target environment for packages that expect it
  if (!exists(".local", envir = .GlobalEnv, inherits = TRUE)) assign(".local", function(...) NULL, envir = .GlobalEnv)
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env, keep.source = FALSE)
  cli::cli_alert_success(paste("Completed", path))
  options(run_all_current_script = NULL)
  invisible(TRUE)
}

# Execute
for (s in scripts) run_script(s)

if (WITH_ANALYSIS) {
  # Deterministic analysis subset (required)
  run_script("R/get_exact_1307.R", required = TRUE)
  # Optional wrapper (run if present)
  run_script("R/02_main_analysis.R", required = FALSE)
  # Main analysis (required)
  run_script("R/03_main_analysis.R", required = TRUE)
  # Hypothesis testing / EFA / CFA / regressions (optional until finalized)
  run_script("R/04_hypothesis_testing.R", required = FALSE)
}

# Verification
verify_outputs <- function(outputs, extra_files = character(0)) {
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

  # 4) Checksums (core + optional analysis artifacts)
  core_paths  <- unlist(outputs)
  extra_files <- extra_files[file.exists(extra_files)]
  all_paths   <- c(core_paths, extra_files)
  all_labels  <- c(names(outputs), basename(extra_files))
  cs <- vapply(all_paths, function(p) digest::digest(file = p, algo = "sha256"), character(1))
  checksum_df <- tibble::tibble(file = all_labels, path = all_paths, sha256 = unname(cs))
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
    if (length(extra_files)) c("", "## Analysis artifacts", paste0("- ", basename(extra_files))) else character(0),
    "",
    "## Checksums (sha256)",
    paste0("- ", checksum_df$file, ": ", checksum_df$sha256),
    ""
  )
  writeLines(ver_md, "docs/verification_report.md")
  cli::cli_alert_success("Verification report written to docs/verification_report.md")
  invisible(TRUE)
}
extra <- character(0)
if (WITH_ANALYSIS) {
  extra <- c(
    "data/climate_finance_survey_final_1307.csv",
    list.files("output",  recursive = TRUE, full.names = TRUE),
    list.files("figures", recursive = TRUE, full.names = TRUE)
  )
}
if (VERIFY) verify_outputs(outputs, extra_files = extra)

cat("\n=== SESSION INFO ===\n"); print(sessionInfo())
cli::cli_alert_success("Pipeline completed successfully.")