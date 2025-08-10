#!/usr/bin/env Rscript
# ============================================
# Initialize renv for Climate Finance Pipeline
# Version 5.0 - Complete Package Set with Statistical Suite
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
#    - Records R version for complete environment documentation
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
# 3. Installs all required packages (including statistical suite)
# 4. Creates renv.lock snapshot file with R version
# 5. Sets up auto-activation via .Rprofile
# 6. Generates comprehensive documentation
#
# AFTER RUNNING THIS SCRIPT:
# - Commit renv.lock to Git (it's a text file with R version included)
# - Don't commit renv/library/ (it's in .gitignore)
# - Team members just run: renv::restore()
# - Your environment is now reproducible!
#
# ============================================

cat("============================================\n")
cat("Climate Finance Pipeline - renv Setup v5.0\n")
cat("Creating reproducible R environment...\n")
cat("============================================\n\n")

# Record current R version for documentation
cat(sprintf("Current R Version: %s\n", R.version.string))
cat(sprintf("Platform: %s\n\n", R.version$platform))

# Step 1: Install renv if not already installed
cat("Step 1: Checking for renv package...\n")
if (!requireNamespace("renv", quietly = TRUE)) {
  cat("  --> Installing renv package manager...\n")
  install.packages("renv", repos = "https://cran.rstudio.com/")
  cat("  [OK] renv installed successfully\n")
} else {
  cat(sprintf("  [OK] renv already installed (v%s)\n", packageVersion("renv")))
}

# Step 2: Initialize renv in the project
cat("\nStep 2: Initializing renv project environment...\n")
cat("  --> This creates renv/ directory and renv.lock file\n")
cat("  --> R version will be recorded in renv.lock\n")

# Load renv
library(renv)

# Initialize with explicit settings
renv::init(
  settings = list(
    # Use explicit snapshots for full control
    snapshot.type = "explicit",
    
    # Use CRAN as primary repository for stability
    repos = c(CRAN = "https://cran.rstudio.com/"),
    
    # Ignore user library to ensure isolation
    external.libraries = character(),
    
    # Use project library only for true isolation
    use.cache = TRUE,
    
    # Record R version in lockfile
    r.version = getRversion()
  )
)
cat("  [OK] renv initialized with R version tracking\n")

# Step 3: Define all required packages
cat("\nStep 3: Defining comprehensive package requirements...\n")

# Organize packages by category for clarity
packages_list <- list(
  # ===== CORE DATA MANIPULATION (Required) =====
  core = c(
    "tidyverse",     # Meta-package: dplyr, tidyr, ggplot2, readr, purrr, tibble, stringr, forcats
    "readr",         # Fast CSV reading with type guessing
    "dplyr",         # Data manipulation (filter, select, mutate, summarize)
    "tidyr",         # Data reshaping (pivot_longer, pivot_wider)
    "stringr",       # String manipulation for text cleaning
    "forcats",       # Factor handling for categorical variables
    "purrr",         # Functional programming for iterations
    "tibble"         # Modern data frames
  ),
  
  # ===== VISUALIZATION (Required) =====
  viz = c(
    "ggplot2",       # Grammar of graphics plotting
    "scales",        # Scale functions for axes and legends
    "corrplot"       # Correlation matrix visualization
  ),
  
  # ===== UTILITIES (Required) =====
  utils = c(
    "cli",           # Beautiful console output with progress bars
    "rlang",         # Programming utilities for tidy evaluation
    "digest",        # SHA-256 hashing for anonymization (CRITICAL!)
    "lubridate",     # Date/time handling for temporal analysis
    "here",          # Project-relative paths (better than setwd)
    "janitor",       # Data cleaning (clean_names, remove_empty)
    "glue",          # String interpolation for messages
    "readxl",        # Read Excel files if needed
    "writexl"        # Write Excel files for sharing
  ),
  
  # ===== STATISTICAL ANALYSIS (Required for Analysis) =====
  stats = c(
    "psych",         # Factor analysis, KMO tests, reliability
    "lavaan",        # Confirmatory factor analysis, SEM
    "MASS",          # Statistical functions (stepwise, LDA)
    "nnet",          # Multinomial logistic regression
    "car",           # Regression diagnostics (VIF, outliers)
    "effectsize",    # Cohen's d, Cramer's V, eta-squared
    "performance",   # Model performance checks and diagnostics (NEW from feedback)
    "binom",         # Binomial confidence intervals and tests (NEW from feedback)
    "pwr",           # Power analysis for sample size (NEW from feedback)
    "rstatix"        # Pipe-friendly statistical tests (NEW from feedback)
  ),
  
  # ===== ADDITIONAL ANALYSIS TOOLS =====
  analysis = c(
    "broom",         # Tidy model outputs
    "modelr",        # Modeling helpers
    "margins",       # Marginal effects
    "sandwich",      # Robust standard errors
    "lmtest",        # Linear model tests
    "multcomp"       # Multiple comparisons
  )
)

# Flatten the list for installation
all_packages <- unique(unlist(packages_list))
cat(sprintf("  --> Total packages to install: %d\n", length(all_packages)))
cat("  --> Including new statistical packages from review feedback:\n")
cat("      * performance - for model diagnostics\n")
cat("      * binom - for binomial tests\n")
cat("      * pwr - for power analysis\n")
cat("      * rstatix - for pipe-friendly statistics\n")

# Step 4: Install all packages
cat("\nStep 4: Installing required packages...\n")
cat("  (This may take several minutes on first run)\n\n")

# Track installation progress and failures
installed_count <- 0
failed_packages <- character()
skipped_packages <- character()

for (pkg in all_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cat(sprintf("  [%d/%d] Installing %s...\n", 
                installed_count + 1, length(all_packages), pkg))
    
    tryCatch({
      install.packages(pkg, 
                      repos = "https://cran.rstudio.com/",
                      quiet = TRUE,
                      dependencies = TRUE)
      installed_count <- installed_count + 1
      cat(sprintf("    [OK] %s installed successfully\n", pkg))
    }, error = function(e) {
      cat(sprintf("    [WARNING] Failed to install %s: %s\n", pkg, e$message))
      failed_packages <<- c(failed_packages, pkg)
    })
  } else {
    cat(sprintf("  [OK] %s already installed (v%s)\n", 
                pkg, as.character(packageVersion(pkg))))
    skipped_packages <- c(skipped_packages, pkg)
    installed_count <- installed_count + 1
  }
}

# Report installation results
cat("\n============================================\n")
cat("Installation Summary:\n")
cat(sprintf("  - Newly installed: %d packages\n", 
            installed_count - length(skipped_packages)))
cat(sprintf("  - Already present: %d packages\n", length(skipped_packages)))
cat(sprintf("  - Failed: %d packages\n", length(failed_packages)))

if (length(failed_packages) > 0) {
  cat("\n[WARNING] Some packages failed to install:\n")
  for (pkg in failed_packages) {
    cat(sprintf("  - %s\n", pkg))
  }
  cat("\nYou may need to install these manually or check system dependencies.\n")
}

# Step 5: Load packages to ensure they're captured in snapshot
cat("\nStep 5: Loading packages for snapshot...\n")
successfully_installed <- setdiff(all_packages, failed_packages)
invisible(lapply(successfully_installed, 
                function(x) suppressPackageStartupMessages(library(x, character.only = TRUE))))
cat(sprintf("  [OK] %d packages loaded\n", length(successfully_installed)))

# Step 6: Create snapshot with R version
cat("\nStep 6: Creating renv.lock snapshot file...\n")
cat("  --> This records exact package versions\n")
cat(sprintf("  --> R version %s will be included\n", R.version.string))

renv::snapshot(
  type = "explicit",
  packages = successfully_installed,
  prompt = FALSE
)

# Step 7: Verify snapshot was created and contains R version
cat("\nStep 7: Verifying snapshot creation...\n")

if (file.exists("renv.lock")) {
  cat("  [SUCCESS] renv.lock file created!\n")
  cat("  [LOCATION] ./renv.lock\n")
  
  # Read and parse the lockfile
  lockfile <- renv::lockfile_read("renv.lock")
  
  # Display summary information
  cat(sprintf("\n  [R VERSION] %s recorded in lockfile\n", lockfile$R$Version))
  cat(sprintf("  [PACKAGES] Total packages locked: %d\n", length(lockfile$Packages)))
  
  # Display key package versions for verification
  cat("\n  [KEY PACKAGE VERSIONS] Captured in lockfile:\n")
  key_packages <- c("tidyverse", "dplyr", "ggplot2", "cli", 
                   "digest", "lubridate", "psych", "lavaan",
                   "performance", "binom", "pwr", "rstatix")
  
  for (pkg in key_packages) {
    if (pkg %in% names(lockfile$Packages)) {
      version <- lockfile$Packages[[pkg]]$Version
      source <- lockfile$Packages[[pkg]]$Source
      cat(sprintf("     - %-12s v%-10s [%s]", pkg, version, source))
      if (pkg %in% c("performance", "binom", "pwr", "rstatix")) {
        cat(" (NEW)")
      }
      cat("\n")
    }
  }
  
  # Show system information
  cat(sprintf("\n  [PLATFORM] %s\n", R.version$platform))
  cat(sprintf("  [OS] %s\n", R.version$os))
  
} else {
  cat("\n[ERROR] renv.lock file was not created.\n")
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
  
  # Show activation message in interactive sessions
  if (interactive()) {
    cat("\\n[renv] Project environment activated\\n")
    cat("  - Using project-specific package library\\n")
    cat("  - Run renv::status() to check consistency\\n")
    cat("  - Run renv::restore() if packages are missing\\n\\n")
  }
}

# Set project-specific options
options(
  # Use tidyverse without startup messages
  tidyverse.quiet = TRUE,
  
  # Increase console width for better output
  width = 120,
  
  # Suppress scientific notation
  scipen = 999,
  
  # Set digits for consistency
  digits = 4,
  
  # UTF-8 encoding
  encoding = "UTF-8"
)

# Set timezone to UTC for reproducibility (if not already set)
if (Sys.getenv("TZ") == "") {
  Sys.setenv(TZ = "UTC")
}

# Optional: Load frequently used packages in interactive sessions
# Uncomment to auto-load packages:
# if (interactive()) {
#   suppressPackageStartupMessages({
#     library(tidyverse)
#     library(here)
#   })
# }
'

# Write or update .Rprofile
if (!file.exists(".Rprofile")) {
  writeLines(rprofile_content, ".Rprofile")
  cat("  [OK] Created .Rprofile for auto-activation\n")
} else {
  cat("  [INFO] .Rprofile already exists\n")
  cat("  --> To update, delete existing .Rprofile and run this script again\n")
}

# Step 9: Generate comprehensive documentation
cat("\nStep 9: Creating renv documentation...\n")

readme_content <- sprintf('# renv: Package Management for Reproducibility

## Quick Start for Team Members

After cloning this repository, restore the exact environment:
```r
renv::restore()
```

This will install all packages with the exact versions used in the analysis.

## Environment Information

- **R Version:** %s
- **Platform:** %s
- **Total Packages:** %d
- **Snapshot Date:** %s

## Key Packages Included

### Data Manipulation
- tidyverse (dplyr, tidyr, ggplot2, readr, etc.)
- janitor (data cleaning)
- lubridate (date handling)

### Statistical Analysis
- psych (factor analysis)
- lavaan (SEM/CFA)
- effectsize (effect sizes)
- performance (model diagnostics)
- binom (binomial tests)
- pwr (power analysis)
- rstatix (pipe-friendly stats)

### Utilities
- digest (SHA-256 hashing for anonymization)
- here (project paths)
- cli (console interface)

## Common renv Commands

| Command | Purpose |
|---------|---------|
| `renv::status()` | Check if packages match renv.lock |
| `renv::restore()` | Install packages from renv.lock |
| `renv::update()` | Update packages to latest versions |
| `renv::snapshot()` | Save current package versions |
| `renv::clean()` | Remove unused packages |
| `renv::dependencies()` | Find package dependencies in code |

## Troubleshooting

### "Package X not available"
Try a different CRAN mirror:
```r
options(repos = c(CRAN = "https://cloud.r-project.org"))
renv::restore()
```

### Installation fails on Linux
Install system dependencies first:
```bash
# Ubuntu/Debian
sudo apt-get update
sudo apt-get install -y \\
  libcurl4-openssl-dev \\
  libssl-dev \\
  libxml2-dev \\
  libfontconfig1-dev \\
  libharfbuzz-dev \\
  libfribidi-dev \\
  libfreetype6-dev \\
  libpng-dev \\
  libtiff5-dev \\
  libjpeg-dev
```

### Wrong package versions loading
Ensure renv is activated:
```r
renv::activate()
renv::status()
```

### Conflicts with system packages
```r
# Isolate project completely
renv::isolate()
```

## Important Files

| File | Purpose | Version Control |
|------|---------|-----------------|
| `renv.lock` | Package versions & R version | ✅ Commit this |
| `renv/` | Local library | ❌ Don\'t commit (in .gitignore) |
| `.Rprofile` | Auto-activation | ✅ Commit this |
| `renv.lock` | Contains R version info | ✅ Always commit |

## Updating Packages

To update packages while maintaining reproducibility:

1. Update specific package:
   ```r
   renv::update("packagename")
   ```

2. Update all packages:
   ```r
   renv::update()
   ```

3. Test your code thoroughly

4. Save new versions:
   ```r
   renv::snapshot()
   ```

5. Commit updated renv.lock:
   ```bash
   git add renv.lock
   git commit -m "Update package versions"
   ```

## For Reviewers

To reproduce the analysis environment:

1. Clone the repository
2. Open R in the project directory
3. Run: `renv::restore()`
4. All packages will be installed with exact versions
5. R version used: %s

This ensures complete reproducibility of all analyses.
', 
R.version.string,
R.version$platform, 
length(successfully_installed),
Sys.Date(),
R.version.string)

writeLines(readme_content, "renv_README.md")
cat("  [OK] Created renv_README.md with full instructions\n")

# Step 10: Final summary
cat("\n")
cat(strrep("=", 50))
cat("\n  SETUP COMPLETE!\n")
cat(strrep("=", 50))
cat("\n\n")

cat("SUMMARY:\n")
cat(sprintf("  ✓ R version: %s (recorded in lockfile)\n", R.version.string))
cat(sprintf("  ✓ Platform: %s\n", R.version$platform))
cat(sprintf("  ✓ renv version: %s\n", packageVersion("renv")))
cat(sprintf("  ✓ Packages locked: %d\n", length(successfully_installed)))
cat(sprintf("  ✓ Lock file created: %s\n", file.exists("renv.lock")))

if (length(failed_packages) == 0) {
  cat("  ✓ All packages installed successfully\n")
} else {
  cat(sprintf("  ⚠ %d packages failed to install\n", length(failed_packages)))
}

cat("\nKEY PACKAGES FOR REVIEWERS:\n")
cat("  • Data manipulation: dplyr, tidyr, readr, stringr\n")
cat("  • Data cleaning: janitor\n")
cat("  • Statistical analysis: psych, effectsize, lavaan\n")
cat("  • Model diagnostics: performance (NEW)\n")
cat("  • Power analysis: pwr (NEW)\n")
cat("  • Binomial tests: binom (NEW)\n")
cat("  • Pipe-friendly stats: rstatix (NEW)\n")
cat("  • Visualization: ggplot2\n")

cat("\nNEXT STEPS:\n")
cat("1. Commit these files to Git:\n")
cat("     git add renv.lock .Rprofile renv_README.md\n")
cat("     git commit -m \"Add renv with locked dependencies and R version\"\n")
cat("\n2. For team members/reviewers:\n")
cat("     After cloning, run: renv::restore()\n")
cat("     This ensures exact package versions\n")
cat("\n3. Verify setup:\n")
cat("     Restart R, then run: renv::status()\n")
cat("\n4. Future updates:\n")
cat("     After installing new packages: renv::snapshot()\n")

cat("\n✨ REPRODUCIBILITY ACHIEVED! ✨\n")
cat("Your analysis environment is now locked and reproducible.\n")
cat("Reviewers will get the exact same package versions.\n")
cat(sprintf("R version %s is recorded in the lockfile.\n\n", getRversion()))

# ============================================
# End of renv setup script
# Your environment is now fully reproducible!
# ============================================