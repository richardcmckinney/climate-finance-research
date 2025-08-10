#!/usr/bin/env Rscript
# ============================================
# Minimal renv Setup - Essential Packages Only
# Version 2.0 - With Core Statistical Suite
# ============================================
#
# This is the minimal version that installs only essential packages
# as specified in the review feedback. Use this for quick setup
# or when you only need the core functionality.
#
# For the full package set, use setup_renv.R instead.
#
# ============================================

cat("============================================\n")
cat("Minimal renv Setup - Essential Packages\n")
cat("============================================\n\n")

# Display R version that will be locked
cat(sprintf("R Version: %s\n", R.version.string))
cat(sprintf("Platform: %s\n\n", R.version$platform))

# Install renv if needed
if (!require("renv", quietly = TRUE)) {
  cat("Installing renv package manager...\n")
  install.packages("renv", repos = "https://cran.rstudio.com/")
}

# Initialize renv
cat("Initializing renv...\n")
library(renv)
renv::init(
  settings = list(
    snapshot.type = "explicit",
    repos = c(CRAN = "https://cran.rstudio.com/")
  )
)

# Essential packages as specified in feedback
cat("\nInstalling essential packages from review feedback...\n")

essential_packages <- c(
  # Data manipulation (from feedback)
  "dplyr",         # Data manipulation
  "tidyr",         # Data reshaping  
  "readr",         # CSV reading
  "stringr",       # String operations
  
  # Data cleaning (from feedback)
  "janitor",       # Clean names, remove empty
  
  # Statistical analysis (from feedback)
  "psych",         # Factor analysis
  "effectsize",    # Effect sizes
  "performance",   # Model diagnostics
  "binom",         # Binomial tests
  "pwr",           # Power analysis
  "lavaan",        # SEM/CFA
  "rstatix",       # Pipe-friendly stats
  
  # Visualization (from feedback)
  "ggplot2",       # Plotting
  
  # Additional essentials for pipeline
  "tidyverse",     # Meta-package
  "cli",           # Console output
  "digest",        # SHA-256 hashing (critical for anonymization)
  "lubridate",     # Date handling
  "here",          # Project paths
  "MASS",          # Statistical functions
  "nnet",          # Multinomial regression
  "car",           # Regression diagnostics
  "corrplot",      # Correlation plots
  "glue",          # String interpolation
  "readxl",        # Excel reading
  "writexl"        # Excel writing
)

# Remove duplicates and sort
essential_packages <- sort(unique(essential_packages))
cat(sprintf("Total packages to install: %d\n\n", length(essential_packages)))

# Install packages
successful <- character()
failed <- character()

for (pkg in essential_packages) {
  if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
    cat(sprintf("Installing %s...", pkg))
    tryCatch({
      install.packages(pkg, 
                       repos = "https://cran.rstudio.com/",
                       quiet = TRUE,
                       dependencies = TRUE)
      successful <- c(successful, pkg)
      cat(" ✓\n")
    }, error = function(e) {
      failed <- c(failed, pkg)
      cat(" ✗\n")
    })
  } else {
    cat(sprintf("%s already installed (v%s) ✓\n", pkg, packageVersion(pkg)))
    successful <- c(successful, pkg)
  }
}

# Load packages for snapshot
cat("\nLoading packages for snapshot...\n")
invisible(lapply(successful, function(x) {
  suppressPackageStartupMessages(library(x, character.only = TRUE))
}))

# Create snapshot with R version
cat("\nCreating renv.lock snapshot...\n")
renv::snapshot(
  type = "explicit",
  packages = successful,
  prompt = FALSE
)

# Verify snapshot
if (file.exists("renv.lock")) {
  lockfile <- renv::lockfile_read("renv.lock")
  
  cat("\n============================================\n")
  cat("✓ SUCCESS! renv.lock created\n")
  cat("============================================\n\n")
  
  cat("LOCKED ENVIRONMENT:\n")
  cat(sprintf("  • R Version: %s\n", lockfile$R$Version))
  cat(sprintf("  • Packages: %d locked\n", length(lockfile$Packages)))
  cat(sprintf("  • Date: %s\n", Sys.Date()))
  
  cat("\nKEY PACKAGES LOCKED:\n")
  key_pkgs <- c("dplyr", "tidyr", "readr", "stringr", "janitor", 
                "psych", "effectsize", "performance", "binom", 
                "pwr", "lavaan", "rstatix", "ggplot2")
  
  for (pkg in key_pkgs) {
    if (pkg %in% names(lockfile$Packages)) {
      cat(sprintf("  • %s: v%s\n", pkg, lockfile$Packages[[pkg]]$Version))
    }
  }
  
  if (length(failed) > 0) {
    cat("\n⚠ WARNING - Failed packages:\n")
    for (pkg in failed) {
      cat(sprintf("  • %s\n", pkg))
    }
  }
  
  cat("\nNEXT STEPS:\n")
  cat("1. Commit renv.lock to version control:\n")
  cat("   git add renv.lock\n")
  cat("   git commit -m \"Lock dependencies with renv\"\n")
  cat("\n2. Team members/reviewers restore with:\n")
  cat("   renv::restore()\n")
  cat("\n3. Verify with:\n")
  cat("   renv::status()\n")
  
} else {
  cat("\n✗ ERROR: renv.lock was not created\n")
  cat("Check errors above and try again.\n")
}

cat("\n============================================\n")
cat("Done! Your environment is now reproducible.\n")
cat("============================================\n")