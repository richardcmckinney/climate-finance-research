# ============================================
# Initialize renv for Climate Finance Pipeline
# Version 4.0 - With Comprehensive Documentation
# ============================================
#
# PURPOSE OF renv AND THIS SCRIPT:
#
# The renv.lock file is crucial for computational reproducibility. It serves
# as a "snapshot" of your exact R package environment, ensuring that anyone
# who runs your code gets identical results. Here's why it matters:
#
# 1. EXACT REPRODUCIBILITY:
#    - Locks specific package versions (e.g., dplyr 1.1.2, not just "latest")
#    - Records package sources (CRAN, GitHub, etc.)
#    - Captures dependency versions (packages that packages depend on)
#    - Ensures identical behavior across time and machines
#
# 2. PREVENTS "WORKS ON MY MACHINE" PROBLEMS:
#    - Package updates can change function behavior
#    - Breaking changes in dependencies can crash old code
#    - Different package versions = different results
#    - This file guarantees everyone uses the same versions
#
# 3. SCIENTIFIC VALIDITY:
#    - Peer reviewers can replicate exact analyses
#    - Results remain reproducible years later
#    - Protects against regression in package updates
#    - Documents computational environment for methods section
#
# 4. COLLABORATION BENEFITS:
#    - Team members auto-sync to same package versions
#    - New contributors get working environment instantly
#    - Eliminates "install package X" email chains
#    - Reduces onboarding time from hours to minutes
#
# HOW renv WORKS:
# 1. Creates project-specific library (renv/library/)
# 2. Installs packages in isolation from system library
# 3. Records exact versions in renv.lock
# 4. Others run renv::restore() to recreate environment
#
# WHAT THIS SCRIPT DOES:
# 1. Installs renv package manager
# 2. Initializes renv in your project
# 3. Installs all required packages
# 4. Creates renv.lock snapshot file
# 5. Sets up auto-activation via .Rprofile
#
# AFTER RUNNING THIS SCRIPT:
# - Commit renv.lock to Git (it's a text file)
# - Don't commit renv/library/ (it's in .gitignore)
# - Team members just run: renv::restore()
# - Your environment is now reproducible!
#
# ============================================

cat("============================================\n")
cat("Climate Finance Pipeline - renv Setup\n")
cat("Creating reproducible R environment...\n")
cat("============================================\n\n")

# Step 1: Install renv if not already installed
cat("Step 1: Checking for renv package...\n")
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("  → Installing renv package manager...\n")
  install.packages("renv")
  cat("  ✓ renv installed successfully\n")
} else {
  cat("  ✓ renv already installed\n")
}

# Step 2: Initialize renv in the project
cat("\nStep 2: Initializing renv project environment...\n")
cat("  → This creates renv/ directory and renv.lock file\n")

renv::init(
  settings = list(
    # Use explicit snapshots (don't auto-detect)
    # This gives us control over what packages are included
    snapshot.type = "explicit",
    
    # Use CRAN as primary repository for stability
    repos = c(CRAN = "https://cran.rstudio.com/"),
    
    # Ignore user library to ensure isolation
    external.libraries = character(),
    
    # Use project library only for true isolation
    use.cache = TRUE
  )
)
cat("  ✓ renv initialized\n")

# Step 3: Define all required packages
cat("\nStep 3: Defining package requirements...\n")

# These packages are organized by their role in the pipeline
core_packages <- c(
  # ===== CORE DATA MANIPULATION (Required) =====
  # These packages are essential for the pipeline to run
  "tidyverse",     # Meta-package: dplyr, tidyr, ggplot2, readr, purrr, tibble, stringr, forcats
  "readr",         # Fast CSV reading with type guessing
  "dplyr",         # Data manipulation (filter, select, mutate, summarize)
  "tidyr",         # Data reshaping (pivot_longer, pivot_wider)
  "stringr",       # String manipulation for text cleaning
  "forcats",       # Factor handling for categorical variables
  "purrr",         # Functional programming for iterations
  
  # ===== VISUALIZATION (Required) =====
  # For creating publication-quality figures
  "ggplot2",       # Grammar of graphics plotting
  "scales",        # Scale functions for axes and legends
  
  # ===== UTILITIES (Required) =====
  # Core utilities for pipeline functionality
  "cli",           # Beautiful console output with progress bars
  "rlang",         # Programming utilities for tidy evaluation
  "digest",        # SHA-256 hashing for anonymization (CRITICAL!)
  "lubridate",     # Date/time handling for temporal analysis
  
  # ===== STATISTICAL ANALYSIS (For --with-analysis flag) =====
  # These packages are needed for hypothesis testing and modeling
  "psych",         # Factor analysis, KMO tests, reliability
  "lavaan",        # Confirmatory factor analysis, SEM
  "MASS",          # Statistical functions (stepwise, LDA)
  "nnet",          # Multinomial logistic regression
  "car",           # Regression diagnostics (VIF, outliers)
  "effectsize",    # Cohen's d, Cramer's V, eta-squared
  "corrplot",      # Correlation matrix visualization
  
  # ===== ADDITIONAL UTILITIES (Recommended) =====
  # Helpful but not strictly required
  "here",          # Project-relative paths (better than setwd)
  "janitor",       # Data cleaning (clean_names, remove_empty)
  "glue",          # String interpolation for messages
  "readxl",        # Read Excel files if needed
  "writexl"        # Write Excel files for sharing
)

cat("  → Total packages to install:", length(core_packages), "\n")

# Step 4: Install all packages
cat("\nStep 4: Installing required packages...\n")
cat("  (This may take several minutes on first run)\n\n")

# Track installation progress
installed_count <- 0
failed_packages <- character()

for (pkg in core_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  [%d/%d] Installing %s...\n", 
                installed_count + 1, length(core_packages), pkg))
    tryCatch({
      install.packages(pkg, quiet = TRUE)
      installed_count <- installed_count + 1
    }, error = function(e) {
      cat(sprintf("    ⚠ Failed to install %s: %s\n", pkg, e$message))
      failed_packages <- c(failed_packages, pkg)
    })
  } else {
    cat(sprintf("  ✓ %s already installed (v%s)\n", 
                pkg, as.character(packageVersion(pkg))))
    installed_count <- installed_count + 1
  }
}

# Report installation results
if (length(failed_packages) > 0) {
  cat("\n⚠ WARNING: Some packages failed to install:\n")
  for (pkg in failed_packages) {
    cat(sprintf("  - %s\n", pkg))
  }
  cat("\nYou may need to install these manually.\n")
}

# Step 5: Load packages to ensure they're captured
cat("\nStep 5: Loading packages for snapshot...\n")
invisible(lapply(setdiff(core_packages, failed_packages), 
                library, character.only = TRUE))
cat("  ✓ Packages loaded\n")

# Step 6: Create snapshot
cat("\nStep 6: Creating renv.lock snapshot file...\n")
cat("  → This records exact package versions\n")

renv::snapshot(
  type = "explicit",
  packages = setdiff(core_packages, failed_packages),
  prompt = FALSE
)

# Step 7: Verify snapshot was created
cat("\nStep 7: Verifying snapshot creation...\n")

if (file.exists("renv.lock")) {
  cat("  ✅ SUCCESS: renv.lock file created!\n")
  cat("  📍 Location: ./renv.lock\n")
  
  # Show summary of what was captured
  lockfile <- renv::lockfile_read("renv.lock")
  cat(sprintf("\n  📦 Total packages locked: %d\n", length(lockfile$Packages)))
  
  # Display key package versions for verification
  cat("\n  📋 Key package versions captured:\n")
  key_packages <- c("tidyverse", "dplyr", "ggplot2", "cli", 
                   "digest", "lubridate", "psych", "lavaan")
  
  for (pkg in key_packages) {
    if (pkg %in% names(lockfile$Packages)) {
      version <- lockfile$Packages[[pkg]]$Version
      source <- lockfile$Packages[[pkg]]$Source
      cat(sprintf("     • %-12s v%-10s [%s]\n", pkg, version, source))
    }
  }
  
  # Show R version information
  cat(sprintf("\n  🔧 R version: %s\n", R.version.string))
  cat(sprintf("  🔧 Platform: %s\n", R.version$platform))
  
} else {
  cat("\n❌ ERROR: renv.lock file was not created.\n")
  cat("Please check for errors above.\n")
  stop("renv initialization failed")
}

# Step 8: Create .Rprofile to auto-activate renv
cat("\nStep 8: Setting up automatic renv activation...\n")

rprofile_content <- '# ============================================
# Project .Rprofile - Auto-activates renv
# ============================================
# This file runs automatically when R starts in this project
# It ensures renv is activated so the correct package versions are used

# Activate renv if available
if (file.exists("renv/activate.R")) {
  source("renv/activate.R")
  
  # Optional: Show message confirming activation
  if (interactive()) {
    cat("✓ renv activated - using project package library\\n")
    cat("  Run renv::status() to check environment\\n")
  }
}

# Optional: Set other project-specific options
options(
  # Use tidyverse conflicts resolution
  tidyverse.quiet = TRUE,
  
  # Set random seed for reproducibility
  # (Note: Pipeline also sets seed in 00_config.R)
  # set.seed(12345)
  
  # Increase width for console output
  width = 120,
  
  # Use UTC timezone for consistency
  # (Note: Pipeline sets this in 00_config.R)
  # Sys.setenv(TZ = "UTC")
  
  # Suppress scientific notation for readability
  scipen = 999
)

# Optional: Load frequently used packages
# Uncomment if you want these auto-loaded:
# if (interactive()) {
#   library(tidyverse)
#   library(here)
# }
'

if (!file.exists(".Rprofile") || 
    readline("📝 .Rprofile exists. Overwrite? (y/n): ") == "y") {
  writeLines(rprofile_content, ".Rprofile")
  cat("  ✓ Created .Rprofile for auto-activation\n")
} else {
  cat("  ℹ Kept existing .Rprofile\n")
}

# Step 9: Generate helpful documentation
cat("\nStep 9: Creating renv documentation...\n")

readme_content <- '# renv: Package Management for Reproducibility

## Quick Start for Team Members

After cloning this repository, run:
```r
renv::restore()
```

This will install all packages with the exact versions used in the analysis.

## Common renv Commands

| Command | Purpose |
|---------|---------|
| `renv::status()` | Check if your packages match renv.lock |
| `renv::restore()` | Install packages from renv.lock |
| `renv::update()` | Update packages to latest versions |
| `renv::snapshot()` | Save current package versions to renv.lock |
| `renv::clean()` | Remove unused packages |

## Troubleshooting

**Problem**: "Package X not available"
**Solution**: Try a different CRAN mirror:
```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
renv::restore()
```

**Problem**: Installation fails on Linux
**Solution**: Install system dependencies first:
```bash
# Ubuntu/Debian
sudo apt-get install libcurl4-openssl-dev libssl-dev libxml2-dev
```

**Problem**: Wrong package versions loading
**Solution**: Ensure renv is activated:
```r
renv::activate()
renv::status()
```

## Important Files

- `renv.lock`: Package versions (commit this!)
- `renv/`: Local library (don\'t commit - in .gitignore)
- `.Rprofile`: Auto-activates renv (commit this)

## Updating Packages

To update packages while maintaining reproducibility:

1. Update specific package: `renv::update("packagename")`
2. Update all: `renv::update()`
3. Test your code thoroughly
4. Save new versions: `renv::snapshot()`
5. Commit updated renv.lock
'

writeLines(readme_content, "renv_README.md")
cat("  ✓ Created renv_README.md with instructions\n")

# Step 10: Final summary and instructions
cat("\n" , strrep("=", 50), "\n")
cat("🎉 SETUP COMPLETE!\n")
cat(strrep("=", 50), "\n\n")

cat("📊 Summary:\n")
cat(sprintf("  • R version: %s\n", R.version.string))
cat(sprintf("  • Platform: %s\n", R.version$platform))
cat(sprintf("  • renv version: %s\n", packageVersion("renv")))
cat(sprintf("  • Packages locked: %d\n", length(lockfile$Packages)))
cat(sprintf("  • Lock file size: %.1f KB\n", file.size("renv.lock") / 1024))

cat("\n📌 Next Steps:\n")
cat("1. ✓ Commit these files to Git:\n")
cat("     git add renv.lock .Rprofile\n")
cat("     git commit -m \"Add renv for reproducibility\"\n")
cat("\n2. ✓ Team instructions:\n")
cat("     After cloning, team members run: renv::restore()\n")
cat("\n3. ✓ Verify setup:\n")
cat("     Restart R, then run: renv::status()\n")
cat("\n4. ✓ Future updates:\n")
cat("     After installing new packages: renv::snapshot()\n")

cat("\n💡 Tips:\n")
cat("  • renv.lock is human-readable JSON - you can review it\n")
cat("  • Each project has isolated package versions\n")
cat("  • Packages are cached globally to save space\n")
cat("  • See renv_README.md for troubleshooting\n")

cat("\n🔬 For reproducible science!\n")
cat("Your analysis environment is now frozen in time.\n")
cat("Anyone can recreate it exactly, even years from now.\n\n")

# ============================================
# End of renv setup script
# Your environment is now reproducible!
# ============================================