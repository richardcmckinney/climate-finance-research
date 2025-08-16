# scripts/renv_sync.R
# One-button "install missing deps + snapshot + verify" for renv.

if (!"renv" %in% rownames(installed.packages())) install.packages("renv")

renv_sync <- function(ci = FALSE, quiet = TRUE) {
  options(repos = c(CRAN = "https://cran.r-project.org"))
  try(renv::settings$use.cache(TRUE), silent = TRUE)

  message("▶ Discovering project dependencies…")
  d <- renv::dependencies()
  deps <- unique(d$Package)

  # Defensive exclusions (we removed its only usage)
  deps <- setdiff(deps, c("BaylorEdPsych"))

  proj_lib <- rownames(installed.packages(lib.loc = renv::paths$library()))
  missing  <- setdiff(deps, proj_lib)

  if (length(missing)) {
    message("▶ Missing packages (project lib): ", paste(missing, collapse = ", "))

    message("▶ Hydrating from user library where possible…")
    try(renv::hydrate(packages = missing), silent = quiet)

    proj_lib <- rownames(installed.packages(lib.loc = renv::paths$library()))
    still_missing <- setdiff(missing, proj_lib)

    if (length(still_missing)) {
      message("▶ Installing remaining from CRAN: ", paste(still_missing, collapse = ", "))
      renv::install(still_missing)
    } else {
      message("✓ Hydrate satisfied all missing packages.")
    }
  } else {
    message("✓ No missing packages in the project library.")
  }

  message("▶ Snapshotting lockfile…")
  renv::snapshot(prompt = FALSE)

  st <- capture.output(renv::status())
  message("▶ renv::status():\n", paste(st, collapse = "\n"))
  inconsistent <- any(grepl("inconsistent state", st, ignore.case = TRUE))
  if (inconsistent) {
    msg <- "renv_sync(): Project still out-of-sync after install + snapshot."
    if (isTRUE(ci)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  } else {
    message("✓ Project synchronized with the lockfile.")
  }
  invisible(!inconsistent)
}

# Allow command-line usage: Rscript scripts/renv_sync.R
if (identical(environment(), globalenv()) && !interactive()) {
  ok <- renv_sync(ci = TRUE, quiet = FALSE)
  quit(status = if (isTRUE(ok)) 0 else 1, save = "no")
}
