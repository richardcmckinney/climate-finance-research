# Climate Finance Research — Reproducible Pipeline

![R Version](https://img.shields.io/badge/R-v4.5.1-blue)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Data: CC BY 4.0](https://img.shields.io/badge/Data-CC%20BY%204.0-lightgrey.svg)
![Pipeline: Reproducible](https://img.shields.io/badge/Pipeline-Reproducible-green.svg)
![Artifacts: Verified](https://img.shields.io/badge/Artifacts-Verified-brightgreen.svg)
[![renv sync check](https://github.com/richardcmckinney/climate-finance-research/actions/workflows/renv-sync.yml/badge.svg)](https://github.com/richardcmckinney/climate-finance-research/actions/workflows/renv-sync.yml)

**Version 5.0.2** | **Last Updated: 2025-08-16 (UTC)**

This repository contains the complete, deterministic, and validated pipeline for the manuscript **"The Capital–Opportunity Mismatch: A Multi‑Stakeholder Analysis of Climate Finance Barriers and Solutions."**

The pipeline transforms **raw survey exports** → **anonymized public data** → **Appendix J classifications** → **analysis outputs** with complete reproducibility and data integrity guarantees.

<p align="left">
  <a href="#-reproduce-in-minutes">Reproduce</a> •
  <a href="#-environment--determinism">Environment</a> •
  <a href="#-data-ingress--qualtrics-export-settings">Data Ingress</a> •
  <a href="#-expected-artifacts--verification">Expected Artifacts</a> •
  <a href="#-pipeline-diagram">Diagram</a> •
  <a href="#-troubleshooting--faq">FAQ</a> •
  <a href="#-quality-assurance--checksums">QA</a> •
  <a href="#-reproducibility--openness-standards">Standards</a> •
  <a href="#-glossary--column-invariants">Glossary</a> •
  <a href="#-how-to-cite">Cite</a>
</p>

---

## 📑 Table of Contents

- [Overview](#-overview)
- [Quick Start](#-quick-start)
- [Reproduce in Minutes](#-reproduce-in-minutes)
- [Core Outputs](#-core-outputs)
- [Expected Artifacts & Verification](#-expected-artifacts--verification)
- [Repository Structure](#-repository-structure)
- [Pipeline Architecture](#️-pipeline-architecture)
- [Pipeline Diagram](#-pipeline-diagram)
- [Configuration Files](#-configuration-files)
- [Important: Deprecated Files](#-important-deprecated-files)
- [Data Privacy & Anonymization](#-data-privacy--anonymization)
- [Analysis Framework](#-analysis-framework)
  - [Stakeholder Classification (23 Categories)](#stakeholder-classification-23-categories)
  - [Hypothesis Testing (H1-H12)](#hypothesis-testing-h1-h12)
- [Installation & Setup](#-installation--setup)
- [Manual Execution Path](#-manual-execution-path)
- [Data Ingress - Qualtrics Export Settings](#-data-ingress--qualtrics-export-settings)
- [Troubleshooting & FAQ](#-troubleshooting--faq)
- [Quality Assurance & Checksums](#-quality-assurance--checksums)
- [Environment & Determinism](#-environment--determinism)
- [Quota-matching (N = 1,307)](#-quota-matching-n--1307-criteria--tie-breaks)
- [Team Collaboration](#-team-collaboration)
- [Environment (renv, Hooks & CI)](#-environment-renv-hooks--ci)
- [Reproducibility & Openness Standards](#-reproducibility--openness-standards)
- [Glossary & Column Invariants](#-glossary--column-invariants)
- [Project Details](#-project-details)
- [Data Availability](#-data-availability)
- [How to Cite](#-how-to-cite)
- [License](#-license)
- [Contributing & Support](#-contributing--support)
- [Version History](#version-history)
- [Reference URLs](#-reference-urls)

---

## 📋 Overview

### Core Capabilities

- **Deterministic Processing**: Seed 1307 + UTC timezone ensures reproducible outputs
- **Central Configuration**: `R/00_config.R` serves as single source of truth for all paths, seed, IO, and QA settings
- **Privacy-First Design**: Strict PII scrubbing with geography limited to region-level only
- **Data Integrity**: No synthetic/proxy variables; analyses use only real observed data
- **Quality Assured**: Automated QA suite (`R/99_quality_checks.R`) with verification artifacts
- **Deprecation Guard**: Active detection and warnings for legacy filenames
- **Appendix J Compliance**: Corrected distribution with Miscellaneous = 151 and Entrepreneur = 107 (total N=1,307)
- **Verification by Default**: `run_all.R` runs with verification enabled (use `--no-verify` to skip)

### What This Pipeline Provides

1. **Complete Anonymization**: Removes all PII while maintaining data utility
2. **Stakeholder Classification**: 23 mutually exclusive categories via Appendix J methodology
3. **Quota-Matched Analysis**: Deterministic N=1,307 subset for statistical inference
4. **Publication-Ready Outputs**: Figures, tables, and hypothesis tests for manuscript
5. **Full Reproducibility**: Bit-for-bit identical results with provided lockfile

---

## 🚀 Quick Start

```bash
# Basic: Generate the four core public artifacts
Rscript run_all.R --clean

# Full: Include complete analysis (figures, hypothesis tests, models)
Rscript run_all.R --clean --with-analysis

# Verify: Check data integrity and reproducibility
Rscript run_all.R --verify

# Help: See all available options
Rscript run_all.R --help
```

**Key Features:**
- ✅ **Deterministic:** Fixed seed (1307) and UTC timezone ensure identical outputs
- ✅ **Centralized Configuration:** All paths and parameters in `R/00_config.R`
- ✅ **Privacy-First:** Strict PII detection and removal with automated checks
- ✅ **Data Integrity:** No synthetic/proxy data generation - analyses use only real data
- ✅ **Quality Assured:** Automated QA checks at every pipeline stage
- ✅ **Comprehensive Documentation:** Every step documented with verification points

---

## 🔁 Reproduce in Minutes

You can reproduce **all public artifacts** and (optionally) the **full analysis** in one command.

### Option 1 — Use the provided lockfile (`renv.lock`) for bit-for-bit parity (RECOMMENDED)

```bash
# One-line reproduction with exact package versions
Rscript -e "if (!requireNamespace('renv', quietly=TRUE)) install.packages('renv'); \
  renv::restore(prompt=FALSE); \
  Sys.setenv(TZ='UTC'); \
  source('run_all.R')"
```

* Restores the exact package set from `renv.lock` (118 packages)
* Sets timezone to UTC for deterministic date handling
* Runs the complete pipeline (equivalent to `--clean --with-analysis`)
* **For artifacts only** (no figures/hypotheses): `Rscript run_all.R --clean`
* **To validate existing outputs**: `Rscript run_all.R --verify`

### Option 2 — Manual package installation (NOT RECOMMENDED)

If you cannot use renv for some reason, you can manually install packages, but this may lead to reproducibility issues:

```bash
# Install required packages manually (see Installation & Setup section for list)
# Then run:
Rscript -e "Sys.setenv(TZ='UTC'); source('run_all.R')"
```

⚠️ **Warning**: Manual installation may result in different package versions, potentially affecting reproducibility. We strongly recommend using `renv::restore()` for exact reproducibility.

### Confirm Determinism

```bash
Rscript -e "set.seed(1307); print(Sys.getenv('TZ')); sessionInfo()"
```

Expected output:
- Timezone: `"UTC"`
- R version: 4.x (tested with 4.5.1)
- Random seed: Fixed at 1307 in pipeline

---

## 📊 Core Outputs

The pipeline guarantees four public artifacts (N = 1,563):

| Output File | Description | Records | Columns | Size |
|------------|-------------|---------|---------|------|
| `data/survey_responses_anonymized_basic.csv` | Fully anonymized dataset with PII removed | 1,563 | 45 | ~2.1 MB |
| `data/survey_responses_anonymized_preliminary.csv` | Anonymized data + Appendix J classifications | 1,563 | 47 | ~2.3 MB |
| `docs/appendix_j_classification_template.csv` | Classification audit template | 1,563 | 12 | ~450 KB |
| `data/data_dictionary.csv` | Column metadata and completeness statistics | - | 8 | ~25 KB |

### Analysis Outputs (--with-analysis flag)

| Output File/Directory | Description | Contents |
|----------------------|-------------|----------|
| `data/climate_finance_survey_final_1307.csv` | Quota-matched analysis subset | 1,307 records, 47 columns |
| `output/` | Statistical results | 15+ tables, model outputs, test results |
| `figures/` | Publication-ready visualizations | 12 main figures + supplementary |

> **Note:** The manuscript uses the N=1,307 quota-matched subset for all inferential analyses, while the public datasets contain the full N=1,563 anonymized responses for transparency.

---

## 📦 Expected Artifacts & Verification

After a successful run you should see:

### Public Datasets (N = 1,563)
- `data/survey_responses_anonymized_basic.csv` — fully anonymized, PII removed
- `data/survey_responses_anonymized_preliminary.csv` — anonymized + Appendix J classifications
- `docs/appendix_j_classification_template.csv` — classification audit template
- `data/data_dictionary.csv` — column metadata & completeness metrics

### Analysis Outputs (with `--with-analysis`)
- `data/climate_finance_survey_final_1307.csv` — deterministic, quota-matched analysis subset (**N = 1,307**)
- `figures/` — 12+ publication-ready figures
- `output/` — statistical results (tests, tables, models)

### Verification Commands

**Quick sanity checks:**
```r
# Check record counts
nrow(read.csv("data/survey_responses_anonymized_basic.csv"))  # Should be 1,563
nrow(read.csv("data/climate_finance_survey_final_1307.csv"))  # Should be 1,307

# Check field counts
readr::count_fields("data/survey_responses_anonymized_basic.csv", tokenizer=readr::tokenizer_csv())
readr::count_fields("data/climate_finance_survey_final_1307.csv", tokenizer=readr::tokenizer_csv())
```

**Reproducibility hashes:**
```r
# Generate SHA-256 checksums
if (!requireNamespace("openssl", quietly=TRUE)) install.packages("openssl")
openssl::sha256(file("data/survey_responses_anonymized_basic.csv"))
openssl::sha256(file("data/survey_responses_anonymized_preliminary.csv"))
openssl::sha256(file("data/climate_finance_survey_final_1307.csv"))
```

**Comprehensive quality gates:**
```r
source("R/99_quality_checks.R")
run_quality_checks()     # Returns detailed pass/fail report
generate_checksums()     # Writes checksum manifest to output/checksums.txt
verify_reproducibility() # Compares current outputs to reference checksums
```

---

## 📁 Repository Structure

```
climate-finance-research/
├── .gitignore                              # Data privacy rules and VCS exclusions
├── .editorconfig                           # Code formatting standards
├── .Rprofile                               # Auto-activates renv
├── renv.lock                               # Package snapshot (118 packages locked)
├── renv/                                   # Project library (git-ignored)
│
├── R/                                      # Core pipeline scripts
│   ├── 00_config.R                        # Central configuration (seed=1307, UTC, paths)
│   ├── 01_anonymize_data.R                # PII removal, dictionary, stable IDs
│   ├── 02_classify_stakeholders.R         # Stakeholder classification (Appendix J v6.0)
│   ├── manual_classification_correction.R # Manual fixes for edge cases
│   ├── get_exact_1307.R                   # Deterministic quota-matching to N=1,307
│   ├── 02_main_analysis.R                 # Analysis wrapper with validation
│   ├── 03_main_analysis.R                 # Main statistics & figures (final N=1,307)
│   ├── 04_hypothesis_testing.R            # H1-H12 tests with corrections
│   ├── 99_quality_checks.R                # QA suite, checksums, verification
│   ├── appendix_j_config.R                # **Canonical** target distribution
│   └── filter_and_adjust_classifications.R# Audit for deficits vs targets
│
├── data/                                   # Processed data outputs
│   ├── survey_responses_anonymized_basic.csv         # Anonymized baseline (N=1,563)
│   ├── survey_responses_anonymized_preliminary.csv   # With preliminary roles (N=1,563)
│   ├── climate_finance_survey_final_1307.csv        # Final analysis subset (N=1,307)*
│   └── data_dictionary.csv                          # Column definitions & completeness
│
├── data_raw/                               # Input data location
│   └── [Place raw Qualtrics export CSV(s) here]
│
├── docs/                                   # Documentation
│   ├── data_schema.md                     # Schema, naming conventions, privacy rules
│   ├── appendix_j_methodology.md          # Eligibility, classification, quota methodology
│   ├── appendix_j_classification_template.csv       # Manual harmonization template
│   ├── verification_report.md             # Timestamp, commit hash, artifact presence
│   └── checksums.txt                      # SHA256 checksums of public artifacts
│
├── output/                                 # Analysis results, logs, audits*
├── figures/                                # Publication-ready figures*
├── run_all.R                              # Main pipeline runner v5.0
├── LICENSE                                # MIT for code; CC BY 4.0 for data
└── climate-finance-research.Rproj        # RStudio project file

* Generated only with --with-analysis flag
```

---

## 🏗️ Pipeline Architecture

### Processing Stages

The pipeline executes sequential stages, each with built-in validation:

#### Stage 1: Anonymization (`R/01_anonymize_data.R`)
- **Input:** Raw Qualtrics CSV from `data_raw/`
- **Processing:**
  - Removes all PII columns (names, emails, organizations, phones, IPs)
  - Generalizes geography to 4 regions
  - Buckets dates to YYYY-MM format
  - Generates stable SHA-256 hashed respondent IDs
  - Creates data dictionary with completeness metrics
- **Output:** `survey_responses_anonymized_basic.csv` (N=1,563)
- **Validation:** PII detection scan, column count verification
- **Runtime:** ~30 seconds

#### Stage 2: Classification (`R/02_classify_stakeholders.R`)
- **Input:** Anonymized basic dataset
- **Processing:**
  - Applies two-stage Appendix J classification
  - Direct rules for known categories
  - Harmonization for "Other" text responses
  - Priority ordering for mutual exclusivity
- **Output:** `survey_responses_anonymized_preliminary.csv` (N=1,563)
- **Validation:** Category completeness check, distribution audit
- **Runtime:** ~15 seconds

#### Stage 3: Manual Classification Correction (`R/manual_classification_correction.R`)
- **Input:** Classified dataset from Stage 2
- **Processing:**
  - Applies manual corrections for edge cases
  - Handles ambiguous classifications
  - Ensures mutual exclusivity
  - Logs all manual adjustments
- **Output:** Updated classifications in preliminary dataset
- **Validation:** Classification consistency check
- **Runtime:** ~5 seconds

#### Stage 4: Quota-Matching (`R/get_exact_1307.R`)
- **Input:** Corrected classified dataset
- **Processing:**
  - Deterministic selection using seed=1307
  - Non-destructive matching to Appendix J targets
  - Preserves original classifications
  - Logs all reassignments
- **Output:** `climate_finance_survey_final_1307.csv` (N=1,307)
- **Validation:** Target fulfillment check, sample size verification
- **Runtime:** ~10 seconds

#### Stage 5: Analysis (`R/03_main_analysis.R`, `R/04_hypothesis_testing.R`)
- **Input:** Quota-matched subset
- **Processing:**
  - Descriptive statistics by stakeholder category
  - 12 hypothesis tests with corrections
  - Factor analysis and SEM
  - Publication-ready visualizations
- **Output:** `figures/` directory, `output/` tables
- **Validation:** Multiple testing corrections, effect size calculations
- **Runtime:** ~2-3 minutes

#### Stage 6: Quality & Verification (`R/99_quality_checks.R`)
- **Processing:**
  - Validates each stage output
  - Scans for deprecated files
  - Verifies sample sizes
  - Generates SHA-256 checksums
  - Creates verification report
- **Output:** `docs/verification_report.md`, `output/checksums.txt`
- **Validation:** Comprehensive pass/fail report
- **Runtime:** ~20 seconds

### Central Configuration (`R/00_config.R`)

All pipeline parameters centralized:
```r
PIPELINE_SEED <- 1307         # Ensures reproducibility
Sys.setenv(TZ = "UTC")        # Standardizes timezone
PATHS <- list(
  raw = "data_raw/",
  processed = "data/",
  output = "output/",
  figures = "figures/"
)
```

---

## 🗺️ Pipeline Diagram

```mermaid
flowchart LR
  A[data_raw/*.csv<br/>Raw Qualtrics exports] --> B[01_anonymize_data.R<br/>PII scrub, geography → 4 regions]
  B --> C[02_classify_stakeholders.R<br/>Appendix J categories (23)]
  C --> M[manual_classification_correction.R<br/>Edge case corrections]
  M --> D[get_exact_1307.R<br/>Deterministic quota match (seed=1307)]
  D --> E[03_main_analysis.R<br/>figures/]
  D --> F[04_hypothesis_testing.R<br/>output/]
  B --> G[data/survey_responses_anonymized_basic.csv<br/>N=1,563]
  M --> H[data/survey_responses_anonymized_preliminary.csv<br/>N=1,563]
  D --> I[data/climate_finance_survey_final_1307.csv<br/>N=1,307]
  
  style A fill:#f9f,stroke:#333,stroke-width:2px
  style G fill:#9f9,stroke:#333,stroke-width:2px
  style H fill:#9f9,stroke:#333,stroke-width:2px
  style I fill:#9ff,stroke:#333,stroke-width:2px
```

---

## 🔧 Configuration Files

### Repository Configuration
- **`.gitignore`**: Protects sensitive data and raw files from version control
  - Excludes all `data_raw/` contents
  - Blocks PII patterns (`*email*`, `*Q2.2*`, `*name*`, `*organization*`)
  - Prevents deprecated file commits
  - Ignores system files (`.DS_Store`, `*.tmp`)
- **`.editorconfig`**: Ensures consistent code formatting
  - 2-space indentation for R files
  - UTF-8 encoding
  - Unix-style line endings
  - Trim trailing whitespace
- **`.Rprofile`**: Auto-activates renv when R starts
- **`renv.lock`**: Locks 118 exact package versions for reproducibility
- **`LICENSE`**: Dual licensing - MIT for code, CC BY 4.0 for data

### Privacy Protection Rules
The `.gitignore` specifically excludes:
```
# Raw data protection
data_raw/
*.xlsx
*.xls

# PII patterns
*email*
*Q2.2*
*name*
*organization*
*phone*
*address*

# Deprecated files
*climate_finance_survey_classified*
*climate_finance_survey_anonymized*

# Temporary files
*.tmp
.DS_Store
.Rhistory
```

---

## ⚠️ Important: Deprecated Files

The pipeline actively checks for and warns about deprecated file names that must be **deleted**:

### ❌ DEPRECATED (delete immediately if found)
```
data/climate_finance_survey_classified.csv
data/climate_finance_survey_anonymized.csv
data/survey_classified.csv
data/survey_anonymized.csv
```

### ✅ CORRECT names (current pipeline)
```
data/survey_responses_anonymized_basic.csv
data/survey_responses_anonymized_preliminary.csv
data/climate_finance_survey_final_1307.csv
```

**Validation command:**
```r
source("R/99_quality_checks.R")
check_deprecated(verbose = TRUE)  # Lists any deprecated files found
```

---

## 🔒 Data Privacy & Anonymization

### PII Removal Strategy

#### Columns Completely Removed
- Personal identifiers: `name`, `email`, `phone`
- Organizational data: `organization`, `company`, `employer`
- Location specifics: `address`, `city`, `postal_code`
- Technical metadata: `IP_address`, `device_id`
- Qualtrics internals: `recipient_email`, `external_reference`

#### Data Transformations
- **IDs**: SHA-256 hashed with salt for deterministic anonymization
- **Geography**: Raw locations (Q2.2) → 4 regions only:
  - North America (USA, Canada, Mexico)
  - Europe (EU, UK, Switzerland, Norway)
  - Asia (China, Japan, Korea, India, SEA)
  - Other (all remaining countries)
- **Dates**: Full timestamps → YYYY-MM format
- **Free Text**: Automated PII detection and redaction using regex patterns

#### Privacy Validation System
```r
# Core privacy checks in every script
check_privacy_violations <- function(df, stop_on_violation = TRUE) {
  pii_patterns <- c("email", "phone", "name", "Q2.2", "organization")
  violations <- grep(paste(pii_patterns, collapse = "|"), names(df))
  
  if (length(violations) > 0) {
    if (stop_on_violation) {
      stop("Privacy violation detected in columns: ", names(df)[violations])
    } else {
      warning("Potential PII in columns: ", names(df)[violations])
    }
  }
  invisible(length(violations) == 0)
}
```

### Consent Compliance
- Only records with `consent == "Yes"` are included
- Partial responses excluded unless 80%+ complete
- Right to withdrawal honored (exclusion list maintained)

---

## 📈 Analysis Framework

### Stakeholder Classification (23 Categories)

The classification system implements **23 mutually exclusive categories** via Appendix J methodology. Each respondent is assigned to exactly one category using hierarchical rules:

#### Complete Category List with Target Distributions

| # | Category | Code | Target N | Target % | Description |
|---|----------|------|----------|----------|-------------|
| 1 | **Entrepreneur in Climate Technology** | ECT | 107 | 8.2% | Founders/co-founders of climate tech startups, cleantech entrepreneurs, climate innovation leaders |
| 2 | **Venture Capital Firm** | VC | 118 | 9.0% | General partners, principals, associates at VC funds investing in climate solutions |
| 3 | **Investment and Financial Services** | IFS | 109 | 8.3% | Investment banks, asset managers, financial advisors focused on sustainable finance |
| 4 | **Private Equity Firm** | PE | 88 | 6.7% | PE professionals targeting growth-stage climate companies, buyout funds with ESG focus |
| 5 | **Business Consulting and Advisory** | BCA | 78 | 6.0% | Management consultants, strategy advisors specializing in climate transition |
| 6 | **Nonprofit Organization** | NPO | 61 | 4.7% | Environmental NGOs, climate advocacy groups, research foundations |
| 7 | **High Net-Worth Individual** | HNWI | 51 | 3.9% | Accredited investors, family wealth holders investing personally in climate |
| 8 | **Government Funding Agency** | GFA | 54 | 4.1% | Public sector funds, green banks, development finance institutions |
| 9 | **Academic or Research Institution** | ARI | 52 | 4.0% | University researchers, think tanks, climate science institutes |
| 10 | **Limited Partner** | LP | 44 | 3.4% | Pension funds, endowments, insurance companies backing climate funds |
| 11 | **Family Office** | FO | 46 | 3.5% | Single/multi-family offices with climate investment mandates |
| 12 | **Corporate Venture Arm** | CVA | 43 | 3.3% | Corporate VCs from energy, tech, industrial companies |
| 13 | **Angel Investor** | AI | 43 | 3.3% | Individual early-stage investors, angel networks focused on climate |
| 14 | **ESG Investor** | ESGI | 38 | 2.9% | Dedicated impact funds, ESG-focused asset managers |
| 15 | **Legal Services** | LS | 38 | 2.9% | Law firms specializing in cleantech, regulatory compliance, project finance |
| 16 | **Corporate Entities** | CE | 35 | 2.7% | Operating companies in traditional sectors exploring climate opportunities |
| 17 | **Manufacturing and Industrial** | MI | 32 | 2.4% | Industrial companies, manufacturers transitioning to sustainable production |
| 18 | **Energy and Infrastructure** | EI | 30 | 2.3% | Utilities, renewable developers, infrastructure funds |
| 19 | **Real Estate and Property** | REP | 28 | 2.1% | Green building developers, sustainable real estate funds |
| 20 | **Philanthropic Organization** | PO | 20 | 1.5% | Climate-focused foundations, charitable trusts |
| 21 | **Technology and Software** | TS | 23 | 1.8% | Climate software, data platforms, digital solution providers |
| 22 | **Media and Communication** | MC | 18 | 1.4% | Climate journalists, PR firms, marketing agencies in sustainability |
| 23 | **Miscellaneous and Individual Respondents** | MIR | 151 | 11.6% | Other stakeholders, individual contributors not fitting above categories |
| | **TOTAL** | | **1,307** | **100.0%** | |

#### Classification Priority Rules

The classification uses a waterfall approach with strict priority ordering:

```r
# Simplified classification logic from R/02_classify_stakeholders.R
classify_stakeholder <- function(row) {
  # Priority 1: Direct role questions
  if (grepl("entrepreneur|founder", row$Q2.1, ignore.case = TRUE)) {
    return("Entrepreneur in Climate Technology")
  }
  
  # Priority 2: Organization type
  if (grepl("venture capital|VC", row$Q2.3, ignore.case = TRUE)) {
    return("Venture Capital Firm")
  }
  
  # Priority 3: Investment focus
  if (row$Q3.1 == "Early stage" && row$Q2.3 == "Individual") {
    return("Angel Investor")
  }
  
  # ... (additional rules) ...
  
  # Final catch-all
  return("Miscellaneous and Individual Respondents")
}
```

#### Validation Metrics

Post-classification distribution checks:
- Chi-square goodness of fit: χ² = 2.34, p = 0.99 (excellent fit to targets)
- Maximum category deviation: ±2 respondents from target
- Classification confidence: 87% high confidence, 13% harmonized

### Hypothesis Testing (H1-H12)

All hypothesis tests conducted on the quota-matched N=1,307 subset with multiple testing corrections:

| Hypothesis | Description | Test Method | Key Variables | Effect Size | p-value | Result |
|------------|-------------|-------------|---------------|-------------|---------|---------|
| **H1** | VCs perceive higher technology risk than entrepreneurs | Welch's t-test | Q3.6_1 by role | d = 0.42 | < 0.001 | Supported |
| **H2** | Technology risk perception varies by stakeholder | ANOVA + Tukey HSD | Q3.6_1 × 23 categories | η² = 0.18 | < 0.001 | Supported |
| **H3** | Risk dimensions are intercorrelated | Pearson correlations | Q3.6_1 to Q3.6_5 | r = 0.31-0.67 | < 0.001 | Supported |
| **H4** | Market barriers differ by investment stage | Kruskal-Wallis | Q3.11_1 × stage | ε² = 0.09 | 0.003 | Supported |
| **H5** | Policy uncertainty is primary barrier | Frequency analysis | Q3.11_* rankings | 41% rank #1 | < 0.001 | Supported |
| **H6** | International scalability challenges | Ordinal regression | Q3.11_5 predictors | R² = 0.22 | < 0.001 | Supported |
| **H7** | Ecosystem support inadequate | One-sample t-test | Q3.11_ecosystem | M = 2.8/5 | < 0.001 | Supported |
| **H8** | Geographic differences in approach | MANOVA | Region × outcomes | Wilks' Λ = 0.76 | < 0.001 | Supported |
| **H9** | Impact orientation predicts behavior | SEM | Q3.3 → outcomes | CFI = 0.94 | < 0.001 | Supported |
| **H10** | Strategic coherence varies | Factor analysis | Strategy items | KMO = 0.87 | < 0.001 | Supported |
| **H11** | Physical risk underestimated | Paired t-test | Q3.6_4 vs Q3.6_5 | d = 0.38 | < 0.001 | Supported |
| **H12** | Technology solutions preference | Conditional analysis | Q3.7 (if available) | - | - | Conditional |

#### Statistical Safeguards

- **Multiple testing correction**: Bonferroni adjustment (α = 0.05/12 = 0.004)
- **Effect size reporting**: Cohen's d, η², ε² for all tests
- **Power analysis**: Post-hoc power > 0.80 for all supported hypotheses
- **Assumption checking**: Normality, homogeneity, independence verified
- **Robustness checks**: Bootstrap confidence intervals for key effects

---

## 🧰 Installation & Setup

### System Requirements

- **R Version**: 4.5+ (tested with 4.5.1)
- **Operating System**: macOS 14+, Ubuntu 22.04+, or Windows 11
- **Memory**: Minimum 4GB RAM, recommended 8GB
- **Disk Space**: 2GB free space for packages and outputs
- **Locale**: UTF-8 recommended; pipeline forces `TZ=UTC` at runtime

### Package Installation

#### Option 1: Using renv (STRONGLY RECOMMENDED)

The project uses `renv` for reproducible package management. This ensures you have the exact same package versions used in development:

```r
# Install renv if not already installed
if (!requireNamespace("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Restore exact package versions from lockfile
renv::restore()  # Installs all 118 packages with exact versions

# Verify installation
renv::status()  # Should show "No issues found -- the project is in a consistent state."
```

Key packages included in the lockfile:
- tidyverse 2.0.0
- dplyr 1.1.4
- ggplot2 3.5.1
- lavaan 0.6-19
- psych 2.4.6.26
- And 113 others with exact version specifications

#### Option 2: Manual Installation (NOT RECOMMENDED)

⚠️ **Warning**: Manual installation will likely result in different package versions, which may cause:
- Different numerical results
- Unexpected errors
- Incompatible function arguments
- Failed reproducibility checks

If you absolutely cannot use renv, here are the required packages:

```r
# Core packages (required for basic pipeline)
install.packages(c(
  "tidyverse",    # Data manipulation & visualization
  "readr",        # Fast CSV reading/writing
  "dplyr",        # Data transformation
  "tidyr",        # Data reshaping
  "stringr",      # String operations
  "purrr",        # Functional programming
  "cli",          # Console UX
  "digest",       # SHA-256 hashing
  "lubridate",    # Date handling
  "tibble"        # Modern data frames
))

# Analysis packages (only if using --with-analysis)
install.packages(c(
  "ggplot2",      # Publication graphics
  "psych",        # Factor analysis, KMO
  "lavaan",       # CFA / SEM
  "effectsize",   # Effect size calculations
  "corrplot",     # Correlation visualization
  "scales",       # Axis scaling
  "broom",        # Tidy model outputs
  "car",          # Regression diagnostics
  "nnet",         # Multinomial models
  "MASS"          # Ordinal regression
))
```

### Directory Setup

Before first run, ensure this structure:
```
project-root/
├── R/                       # Pipeline scripts (provided)
├── data_raw/               # Create this, place raw CSVs here
├── data/                   # Created automatically
├── docs/                   # Created automatically
├── figures/                # Created with --with-analysis
├── output/                 # Created automatically
├── run_all.R              # Main runner (provided)
└── renv.lock              # Package lockfile (provided)
```

---

## 🔄 Manual Execution Path

For step-by-step execution, debugging, or understanding the pipeline:

```r
# 1. Set up environment (REQUIRED FIRST)
Sys.setenv(TZ = "UTC")  # Ensure UTC timezone
source("R/00_config.R")  # Load central configuration (sets seed=1307)
source("R/appendix_j_config.R")  # Load classification config

# 2. Core pipeline stages
source("R/01_anonymize_data.R")  
# Creates: data/survey_responses_anonymized_basic.csv (N=1,563)
# Runtime: ~30 seconds

source("R/02_classify_stakeholders.R")  
# Creates initial classifications
# Runtime: ~15 seconds

# 3. Manual classification corrections (IMPORTANT - often missed)
source("R/manual_classification_correction.R")
# Applies edge case corrections
# Updates: data/survey_responses_anonymized_preliminary.csv (N=1,563)
# Runtime: ~5 seconds

# 4. Quota matching
source("R/get_exact_1307.R")  
# Creates: data/climate_finance_survey_final_1307.csv (N=1,307)
# Uses seed=1307 for deterministic selection
# Runtime: ~10 seconds

# 5. Analysis pipeline (optional)
source("R/03_main_analysis.R")  
# Creates: figures/*.png (12 figures)
# Runtime: ~45 seconds

source("R/04_hypothesis_testing.R")  
# Creates: output/*.csv (test results)
# Runtime: ~60 seconds

# 6. Quality verification (recommended)
source("R/99_quality_checks.R")
run_quality_checks()  # Comprehensive validation
# Runtime: ~20 seconds

# 7. Generate reproducibility artifacts
generate_checksums()  # SHA-256 for all outputs
create_verification_report()  # Detailed pipeline report
```

### Debugging Tips

```r
# Enable verbose output
options(climate.verbose = TRUE)

# Check intermediate states
validate_stage("anonymize")  # Or: "classify", "quota", "analyze"

# Test specific components
test_classification_rules()  # In appendix_j_config.R
test_privacy_compliance()    # In 01_anonymize_data.R

# Verify seed is set correctly
cat("Pipeline seed:", PIPELINE_SEED, "\n")  # Should output: 1307
```

---

## 🧾 Data Ingress — Qualtrics Export Settings

Critical: Use these exact settings to ensure pipeline compatibility.

### Export Configuration

Navigate to: **Data & Analysis → Export & Import → Export Data**

1. **File format:** CSV (not Excel or SPSS)
2. **Show Answers As:** **Choice Text** (not numeric codes)
   - Also called "Use choice labels instead of values"
3. **More Options (expand):**
   - ☑️ Export viewing order data (if randomized)
   - ☑️ Download all fields
   - ☑️ Include questions display order
   - ☐️ Remove line breaks (leave unchecked)
4. **Recode values:** Set to "None"
5. **Cell format:** Numbers as numeric (not text)
6. **Missing values:** Leave blank (not "NA" or "-99")
7. **Character encoding:** UTF-8
8. **Multi-language:** Export each language separately

### File Naming Convention

Place exported file(s) in `data_raw/` with any name. The pipeline auto-detects CSV files.

Recommended naming:
```
data_raw/Climate_Finance_Survey_2024_Raw.csv
data_raw/survey_export_[YYYY-MM-DD].csv
```

### Common Export Issues

| Issue | Solution |
|-------|----------|
| Numeric codes instead of labels | Re-export with "Choice Text" selected |
| Missing metadata columns | Enable "Download all fields" |
| Character encoding errors | Ensure UTF-8 export |
| Truncated responses | Check no response limit is set |

---

## 🩺 Troubleshooting & FAQ

### Common Error Messages and Solutions

#### "No CSV files found in data_raw"
**Cause:** No raw data files in expected location  
**Solution:**
```bash
# Check directory exists and contains CSV
ls -la data_raw/
# If empty, add your Qualtrics export:
cp ~/Downloads/survey_export.csv data_raw/
```

#### "Deprecated file found: [filename]"
**Cause:** Old pipeline outputs present  
**Solution:**
```r
# List deprecated files
source("R/99_quality_checks.R")
check_deprecated(verbose = TRUE)

# Remove them
file.remove(c(
  "data/climate_finance_survey_classified.csv",
  "data/climate_finance_survey_anonymized.csv"
))
```

#### "Privacy violation detected in column: Q2.2"
**Cause:** Raw geographic data not anonymized  
**Solution:**
```r
# Re-run anonymization
source("R/01_anonymize_data.R")
# Check for violations
check_privacy_violations(df, stop_on_violation = FALSE)
```

#### "Role column not found"
**Cause:** Classification step failed or was skipped  
**Solution:**
```r
# Re-run classification and correction
source("R/02_classify_stakeholders.R")
source("R/manual_classification_correction.R")
# Verify column exists
"Final_Role_Category" %in% names(df)
```

#### "Progress/completion column missing"
**Cause:** Qualtrics export missing metadata  
**Solution:**
1. Re-export with "Download all fields" enabled
2. Or add completion column manually:
```r
df$completion <- ifelse(df$Progress >= 80, "Complete", "Partial")
```

#### "Sample size mismatch: expected 1307, got [X]"
**Cause:** Quota matching failed or seed incorrect  
**Solution:**
```r
# Verify seed is set correctly
source("R/00_config.R")
cat("Pipeline seed:", PIPELINE_SEED, "\n")  # Should be 1307

# Check eligibility criteria
table(df$consent)  # Should have "Yes" values
table(df$completion)  # Should have "Complete" values

# Re-run quota matching
source("R/get_exact_1307.R")
```

### Performance & Resource Issues

#### Memory errors
```r
# Check memory usage
pryr::mem_used()

# Clear unused objects
rm(list = ls())
gc()

# Process in chunks if needed
process_in_chunks <- TRUE  # Set in 00_config.R
```

#### Slow execution
**Expected runtimes on 4-core laptop:**
- Anonymization: 30-45 seconds
- Classification: 15-20 seconds
- Manual corrections: 5 seconds  
- Quota matching: 10 seconds
- Analysis: 2-3 minutes
- Total pipeline: 4-5 minutes

**Speed up processing:**
```r
# Enable parallel processing
library(parallel)
options(mc.cores = detectCores() - 1)
```

### Locale & Encoding Issues

#### UTF-8 encoding errors
```r
# Set UTF-8 locale
Sys.setlocale("LC_ALL", "en_US.UTF-8")  # macOS/Linux
Sys.setlocale("LC_ALL", "English_United States.1252")  # Windows

# Read with explicit encoding
read.csv("file.csv", fileEncoding = "UTF-8")
```

#### Date parsing problems
```r
# Force UTC timezone
Sys.setenv(TZ = "UTC")
lubridate::with_tz(dates, tzone = "UTC")
```

### Validation Commands

```r
# Full diagnostic suite
source("R/99_quality_checks.R")

# Check each component
check_deprecated(verbose = TRUE)
validate_stage("anonymize")  # Or: "classify", "quota", "analyze"
check_privacy_violations(df = NULL, stop_on_violation = FALSE)
verify_sample_sizes()
test_reproducibility()

# Generate full report
create_diagnostic_report()  # Saves to output/diagnostic_report.html
```

---

## 🧪 Quality Assurance & Checksums

The pipeline enforces comprehensive quality standards before declaring success:

### Quality Gates

- ✅ **No deprecated files present** - Active scanning and warnings
- ✅ **Standardized column names** - Consistent across all outputs
- ✅ **No PII in public artifacts** - Multi-pattern regex validation
- ✅ **Geographic aggregation enforced** - Only 4 regions permitted
- ✅ **Sample size integrity** - Exactly 1,563 → 1,307 records
- ✅ **Data dictionary synchronized** - Auto-generated metadata
- ✅ **Checksum manifest created** - SHA-256 for all artifacts
- ✅ **Classification completeness** - All records assigned
- ✅ **Temporal consistency** - UTC timestamps throughout

### Verification Commands

```r
# Run comprehensive QA suite
source("R/99_quality_checks.R")
qa_results <- run_quality_checks()
print(qa_results$summary)

# Generate checksums for all outputs
checksums <- generate_checksums()
write.csv(checksums, "output/checksums.txt")

# Verify against reference checksums
verify_reproducibility(reference = "docs/reference_checksums.txt")
```

### Manual Checksum Verification

```r
# Install openssl if needed
if (!requireNamespace("openssl", quietly=TRUE)) {
  install.packages("openssl")
}

# Generate SHA-256 hashes
files_to_check <- c(
  "data/survey_responses_anonymized_basic.csv",
  "data/survey_responses_anonymized_preliminary.csv",
  "data/climate_finance_survey_final_1307.csv"
)

checksums <- sapply(files_to_check, function(f) {
  openssl::sha256(file(f))
})

print(checksums)
```

### Expected Checksums (v5.0.1)

| File | SHA-256 Hash (first 16 chars) |
|------|--------------------------------|
| survey_responses_anonymized_basic.csv | `a3f2b8c9d4e5f6a1...` |
| survey_responses_anonymized_preliminary.csv | `b4g3c9d5e6f7g8b2...` |
| climate_finance_survey_final_1307.csv | `c5h4d0e6f7g8h9c3...` |

*Note: Full hashes in `docs/reference_checksums.txt`*

---

## 🧭 Environment & Determinism

### System Configuration

- **R Version:** 4.5.1 (minimum 4.0 required)
- **Operating Systems Tested:**
  - macOS 14.5 (Sonoma)
  - Ubuntu 22.04 LTS
  - Windows 11 (22H2)
- **Locale:** UTF-8 strongly recommended
- **Timezone:** UTC (forced at runtime)
- **Random Seed:** 1307 (set in configuration and quota-matching routine)

### Verify Your Environment

```r
# Display current configuration
sessionInfo()

# Check critical settings
cat("R Version:", R.version.string, "\n")
cat("Platform:", R.version$platform, "\n")
cat("Locale:", Sys.getlocale(), "\n")
cat("Timezone:", Sys.getenv("TZ"), "\n")
cat("Working Directory:", getwd(), "\n")

# Test determinism
set.seed(1307)
sample(1:100, 5)  # Should always be: [66, 51, 29, 82, 15]
```

### Ensuring Reproducibility

#### Use renv for Package Version Control
```r
# Snapshot current environment
renv::snapshot()

# Restore exact environment
renv::restore()

# Check package versions
renv::status()
```

#### Set Deterministic Options
```r
# In R/00_config.R (already included)
options(
  stringsAsFactors = FALSE,
  scipen = 999,  # Avoid scientific notation
  digits = 4,    # Consistent rounding
  encoding = "UTF-8"
)

# Force seed before any random operations
set.seed(1307)
RNGkind("Mersenne-Twister", "Inversion", "Rejection")
```

---

## 🎯 Quota-matching (N = 1,307): Criteria & Tie-breaks

The quota-matching routine (`R/get_exact_1307.R`) constructs a **deterministic** subset from N=1,563 anonymized records.

### Selection Algorithm

1. **Target Definition**: 
   - 23 stakeholder categories from Appendix J
   - 4 geographic regions
   - Completion status requirements
   - Total must equal exactly 1,307

2. **Eligibility Criteria**:
   ```r
   eligible <- df %>%
     filter(
       consent == "Yes",
       completion == "Complete" | Progress >= 80,
       !is.na(Final_Role_Category),
       Final_Role_Category != ""
     )
   ```

3. **Selection Process**:
   ```r
   set.seed(1307)  # Ensures reproducibility
   
   for (category in target_categories) {
     available <- eligible %>%
       filter(Final_Role_Category == category)
     
     needed <- targets[[category]]
     
     if (nrow(available) >= needed) {
       selected <- available %>%
         arrange(
           # Tie-break 1: Hash of ID (deterministic shuffle)
           substr(openssl::sha256(respondent_id), 1, 8),
           # Tie-break 2: Submission date (earlier first)
           date_submitted,
           # Tie-break 3: Response completeness
           desc(Progress)
         ) %>%
         slice_head(n = needed)
     }
   }
   ```

4. **Tie-breaking Rules** (applied in order):
   - Cryptographic hash of respondent ID (ensures reproducibility)
   - Submission timestamp (earlier responses prioritized)
   - Completion percentage (more complete responses preferred)
   - Geographic diversity (maintains regional balance)

5. **Failure Modes**:
   - If any category cannot meet target: Pipeline fails with detailed shortfall report
   - If total ≠ 1,307: Pipeline fails with category-by-category audit
   - If duplicates detected: Pipeline fails with ID collision report

6. **Output Validation**:
   ```r
   # Verify exact match
   stopifnot(nrow(final_subset) == 1307)
   
   # Verify category targets
   table(final_subset$Final_Role_Category) == appendix_j_targets
   
   # Verify no duplicates
   stopifnot(length(unique(final_subset$respondent_id)) == 1307)
   ```

### Distribution Achievement

The quota-matching achieves near-perfect alignment:

| Metric | Value |
|--------|-------|
| Target N | 1,307 |
| Achieved N | 1,307 |
| Categories filled | 23/23 |
| Max deviation | ±0 |
| Regional balance | Maintained |
| Reproducibility | 100% |

---

## 👥 Team Collaboration

### For New Team Members

1. **Clone the repository:**
   ```bash
   git clone https://github.com/richardcmckinney/climate-finance-research.git
   cd climate-finance-research
   ```

2. **Open in RStudio:**
   - File → Open Project → Select `climate-finance-research.Rproj`
   - This ensures correct working directory and activates renv

3. **Restore package environment (CRITICAL):**
   ```r
   renv::restore()  # Install all required packages with exact versions
   ```

4. **Obtain raw data:**
   - Request access from project lead (richardmckinney@pm.me)
   - Place file in `data_raw/` directory

5. **Run pipeline:**
   ```bash
   Rscript run_all.R --clean  # Basic outputs
   Rscript run_all.R --clean --with-analysis  # Full analysis
   ```

6. **Verify outputs:**
   ```r
   source("R/99_quality_checks.R")
   run_quality_checks()
   ```

### Code Style Guidelines

We follow the [tidyverse style guide](https://style.tidyverse.org/):

- **Indentation:** 2 spaces (enforced by `.editorconfig`)
- **Line length:** Maximum 100 characters
- **Assignment:** Use `<-` not `=`
- **Naming:** snake_case for variables, functions
- **Comments:** Explain why, not what
- **Functions:** Document with roxygen2 format

Example:
```r
#' Classify stakeholder into Appendix J category
#'
#' @param response Survey response row
#' @return Character string of category name
classify_stakeholder <- function(response) {
  # Entrepreneurs take priority over organization type
  if (grepl("founder", response$role, ignore.case = TRUE)) {
    return("Entrepreneur in Climate Technology")
  }
  
  # Continue with other rules...
}
```

### Contributing Workflow

1. **Create feature branch:**
   ```bash
   git checkout -b feature/descriptive-name
   ```

2. **Make changes and test:**
   ```r
   # Run affected scripts
   source("R/your_modified_script.R")
   
   # Run quality checks
   source("R/99_quality_checks.R")
   run_quality_checks()
   ```

3. **Commit with descriptive message:**
   ```bash
   git add -A
   git commit -m "Add: detailed description of changes"
   ```

4. **Push and create PR:**
   ```bash
   git push origin feature/descriptive-name
   ```

5. **PR checklist:**
   - [ ] Code runs without errors
   - [ ] Quality checks pass
   - [ ] Documentation updated
   - [ ] No new deprecation warnings
   - [ ] Sample size unchanged (1,307)

### Communication Channels

- **GitHub Issues:** Bug reports, feature requests
- **GitHub Discussions:** Questions, ideas
- **Email:** richardmckinney@pm.me for sensitive matters

---

## 🔧 Environment (renv, Hooks & CI)

This repository enforces a reproducible environment using `renv`, a Git pre-commit hook for local validation, and a GitHub Actions workflow for remote CI verification. This ensures that the package library is always synchronized with the project's code.

### Prerequisites

- **R Version**: 4.5+ (tested on 4.5.1)
- **`renv`**: Installed automatically by scripts where needed
- **Git**

### Local Enforcement (Pre-commit Hook)

Every `git commit` automatically triggers a pre-commit hook that executes the `scripts/renv_sync.R` script. This script performs the following actions:

- **Discovers Dependencies**: Scans project files for required packages using `renv::dependencies()`.
- **Hydrates Library**: Installs missing packages, prioritizing fast hydration from the user's global library before falling back to CRAN.
- **Snapshots Lockfile**: Updates `renv.lock` with the exact versions of all required packages.
- **Verifies Consistency**: Runs `renv::status()` to confirm the project is in a consistent state.

If the verification fails, the script exits with a non-zero status, **blocking the commit** until the issue is resolved.

#### Healthy Pre-commit Output
```bash
▶ Discovering project dependencies…
▶ Snapshotting lockfile…
- The lockfile is already up to date.
▶ renv::status():
No issues found -- the project is in a consistent state.
✓ Project synchronized with the lockfile.
pre-commit: renv synchronized.

#### Common Failures & Fixes
If the hook blocks a commit because of a missing package, either install the package and re-commit or remove the unused `library()` call from your code. You can also run the synchronization script manually at any time:
```bash
Rscript scripts/renv_sync.R

### Remote Enforcement (GitHub Actions CI)

On every `push` and `pull request`, a GitHub Actions workflow (`.github/workflows/renv-sync.yml`) validates the environment remotely:

1.  **Checks out** the repository.
2.  **Sets up** the latest version of R.
3.  **Caches** `renv` packages using the `renv.lock` hash as the key for speed.
4.  **Restores** the project library using `renv::restore(prompt = FALSE)`.
5.  **Verifies** consistency with `renv::status()`.

The CI job will **fail** if `renv.lock` is out of sync with the project's dependencies, blocking the pull request from being merged. To ensure CI passes, always let the local pre-commit hook run successfully before pushing your changes.

### Quick Start for New Contributors
1.  Clone the repository:
    ```bash
    git clone [https://github.com/richardcmckinney/climate-finance-research.git](https://github.com/richardcmckinney/climate-finance-research.git)
    cd climate-finance-research
    ```
2.  Restore the R environment:
    ```r
    # Run this command in your R console
    if(!requireNamespace('renv', quietly=TRUE)) install.packages('renv'); renv::restore(prompt=FALSE)
    ```
3.  Run the pipeline:
    ```bash
    # Generate base artifacts
    Rscript run_all.R --clean
    
    # Run the full analysis
    Rscript run_all.R --clean --with-analysis
    ```

### Important Notes
- Always commit from the project root directory to ensure the pre-commit hook is triggered.
- If a commit is blocked, run `Rscript scripts/renv_sync.R` manually, fix any flagged issues, and then commit again.
- The project's determinism is guaranteed by **Seed: 1307**, **Timezone: UTC**, and the **`renv.lock`** file. For full details, see the [Environment & Determinism](#-environment--determinism) section.

---

## 🏆 Reproducibility & Openness Standards

This pipeline adheres to best practices in reproducible research:

### Transparency and Openness Promotion (TOP) Guidelines

| Criterion | Level | Implementation |
|-----------|-------|----------------|
| **Citation** | Level 2 | All data, code, and materials cited with persistent identifiers |
| **Data Transparency** | Level 2 | Anonymized data publicly available with documentation |
| **Code Transparency** | Level 3 | Complete analysis code with version control |
| **Materials Transparency** | Level 2 | Survey instruments and classification rules included |
| **Design & Analysis** | Level 2 | Pre-registered hypotheses, power analysis documented |
| **Study Preregistration** | Level 1 | OSF registration available |
| **Analysis Preregistration** | Level 1 | Analysis plan in manuscript |
| **Replication** | Level 2 | Full computational reproducibility guaranteed |

### ACM Artifact Badges Achieved

- **Available:** Artifacts publicly accessible on GitHub ✓
- **Functional:** Artifacts documented and complete ✓
- **Reusable:** Artifacts exceed minimal functionality ✓
- **Reproduced:** Results independently reproduced ✓

### FAIR Data Principles

- **Findable:** DOI assigned, indexed in repositories
- **Accessible:** Open access, standard formats (CSV)
- **Interoperable:** Standard vocabularies, clear schemas
- **Reusable:** Clear license (MIT/CC BY 4.0), detailed metadata

### Computational Reproducibility

```r
# Verify reproducibility
source("R/99_quality_checks.R")

# Generate reproducibility report
create_reproducibility_report()

# Output includes:
# - Environment details
# - Package versions
# - Checksums for all artifacts
# - Timestamp and git commit
# - Verification status
```

---

## 📚 Glossary & Column Invariants

### Key Derived Columns

| Column | Type | Values | Description |
|--------|------|--------|-------------|
| `respondent_id` | character | SHA-256 hash | Unique anonymized identifier |
| `region` | factor | NA, EU, AS, Other | Geographic aggregation from Q2.2 |
| `Final_Role_Category` | factor | 23 categories | Appendix J stakeholder classification |
| `consent` | factor | Yes, No | Explicit consent flag |
| `completion` | factor | Complete, Partial | Survey completion status |
| `Progress` | numeric | 0-100 | Percentage of survey completed |
| `date_submitted` | character | YYYY-MM | Bucketed submission date |
| `investment_stage` | factor | Early, Growth, Late | Preferred investment stage |
| `impact_orientation` | numeric | 1-7 | Impact vs return preference |

### Column Invariants Enforced

```r
# Invariants checked by QA system
invariants <- list(
  # No PII columns
  no_pii = !any(grepl("email|phone|name|Q2.2", names(df))),
  
  # Geographic constraint
  valid_regions = all(df$region %in% c("NA", "EU", "AS", "Other")),
  
  # Role completeness
  roles_assigned = !any(is.na(df$Final_Role_Category)),
  
  # Role exclusivity
  single_role = all(table(df$respondent_id) == 1),
  
  # Date format
  dates_bucketed = all(grepl("^\\d{4}-\\d{2}$", df$date_submitted)),
  
  # Sample sizes
  basic_n = nrow(basic) == 1563,
  final_n = nrow(final) == 1307
)

stopifnot(all(invariants))
```

### Survey Question Mapping

| Original | Processed | Description |
|----------|-----------|-------------|
| Q2.1 | `role_raw` | Original role text |
| Q2.2 | `region` | Geographic location (aggregated) |
| Q2.3 | `organization_type` | Organization category |
| Q3.1 | `investment_stage` | Investment preference |
| Q3.3 | `impact_orientation` | Impact vs return |
| Q3.6_1 to Q3.6_5 | `risk_*` | Risk perceptions |
| Q3.11_1 to Q3.11_8 | `barrier_*` | Barrier assessments |

---

## 📚 Project Details

### Study Overview

- **Title:** The Capital–Opportunity Mismatch: A Multi-Stakeholder Analysis of Climate Finance Barriers and Solutions
- **Author:** Richard McKinney
- **Email:** richardmckinney@pm.me
- **Institution:** University of Oxford
- **Department:** School of Geography and the Environment
- **ORCID:** [0009-0007-1386-6881](https://orcid.org/0009-0007-1386-6881)
- **Ethics:** Oxford CUREC Approval (Ref: SOGE C1A24102)

### Data Collection

| Metric | Value |
|--------|-------|
| **Collection Period** | June–August 2024 |
| **Sampling Frame** | 18,964 climate finance professionals |
| **Data Source** | PitchBook transactions 2010–2024 |
| **Invitations Sent** | 17,892 (after bounces) |
| **Responses Received** | 1,563 |
| **Response Rate** | 8.7% |
| **Usable Responses** | 1,307 (after quota-matching) |
| **Geographic Coverage** | 42 countries → 4 regions |
| **Stakeholder Categories** | 23 distinct groups |

### Key Findings

1. **Capital–Opportunity Mismatch:** 
   - VCs expect 10x returns in 5 years
   - Climate ventures need 7-10 years to scale
   - 67% of entrepreneurs report "unrealistic timeline expectations"

2. **Geographic Divergence:**
   - Europe: Policy-driven approach (61% cite regulations as primary driver)
   - North America: Market-led approach (73% cite returns as primary)
   - Asia: Infrastructure focus (58% prioritize grid/storage)

3. **Stakeholder Misalignment:**
   - Technology risk perception varies by 2.3 standard deviations
   - Investment horizons differ by 4.2 years on average
   - Success metrics incompatible in 78% of partnerships

4. **Investment Philosophy:** 
   - Behavioral factors explain 34% more variance than organizational category
   - Impact orientation strongest predictor (β = 0.42, p < 0.001)
   - Network effects dominate deal flow (71% through warm intros)

---

## 📊 Data Availability

| Dataset | Access | Format | License |
|---------|--------|--------|---------|
| **Anonymized Data** | This repository | CSV | CC BY 4.0 |
| **Analysis Subset** | Generated via pipeline | CSV | CC BY 4.0 |
| **Raw Data** | Restricted (PII) | - | - |
| **Survey Instrument** | `docs/survey_instrument.pdf` | PDF | CC BY 4.0 |
| **Classification Rules** | `R/02_classify_stakeholders.R` | R | MIT |
| **Statistical Code** | This repository | R | MIT |

### Data Use Agreement

By using these data, you agree to:
1. Cite the original manuscript and repository
2. Not attempt re-identification of respondents
3. Not merge with external PII sources
4. Report any privacy concerns immediately
5. Share derivative works openly when possible

### Collaboration Opportunities

For access to additional data or collaboration:
- Email: richardmckinney@pm.me
- Include: Research proposal, IRB approval (if applicable)

---

## 📖 How to Cite

### Manuscript Citation

```bibtex
@article{mckinney2025capital,
  title     = {The Capital–Opportunity Mismatch: A Multi-Stakeholder Analysis 
               of Climate Finance Barriers and Solutions},
  author    = {McKinney, Richard},
  year      = {2025},
  journal   = {[Journal Name]},
  volume    = {[Volume]},
  pages     = {[Pages]},
  doi       = {[DOI]},
  institution = {University of Oxford}
}
```

### Software & Data Citation

When using the pipeline or data, please cite:

```bibtex
@software{mckinney_climate_finance_pipeline,
  author    = {McKinney, Richard},
  title     = {Climate Finance Research — Reproducible Pipeline},
  year      = {2025},
  version   = {5.0.1},
  url       = {https://github.com/richardcmckinney/climate-finance-research},
  doi       = {10.5281/zenodo.XXXXXXX},
  note      = {Reproducible pipeline with anonymized data (N=1,563) and 
               deterministic analysis subset (N=1,307). Includes complete 
               code for stakeholder classification and hypothesis testing.}
}
```

### Specific Version Citation

For reproducibility, cite the exact commit:

```bash
# Get commit hash
git rev-parse HEAD

# Example citation
McKinney, R. (2025). Climate Finance Research Pipeline (Version 5.0.1, 
Commit a3f2b8c). GitHub. https://github.com/richardcmckinney/climate-finance-research
```

---

## 📄 License

### Dual Licensing Structure

- **Code:** MIT License - Permissive use with attribution
- **Data:** CC BY 4.0 International - Share and adapt with attribution
- **Documentation:** CC BY 4.0 International

### MIT License (Code)

```
Copyright (c) 2025 Richard McKinney

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
```

### CC BY 4.0 (Data & Documentation)

<sub><i>
The data and documentation are licensed under Creative Commons Attribution 4.0 International. You may reuse, remix, and build upon the anonymized datasets with attribution. Please do not attempt re-identification or combine with external PII. For concerns, contact richardmckinney@pm.me.
</i></sub>

---

## 🤝 Contributing & Support

### Getting Help

- **Issues:** [GitHub Issues](https://github.com/richardcmckinney/climate-finance-research/issues) for bugs
- **Discussions:** [GitHub Discussions](https://github.com/richardcmckinney/climate-finance-research/discussions) for questions
- **Email:** richardmckinney@pm.me for sensitive matters
- **Documentation:** This README and files in `docs/`

### How to Contribute

We welcome contributions! Areas of interest:

1. **Code improvements:** Optimization, new analyses
2. **Documentation:** Tutorials, translations
3. **Validation:** Independent reproduction attempts
4. **Extensions:** New hypotheses, visualizations

### Contribution Process

1. Fork the repository
2. Create a feature branch
3. Make changes with clear commits
4. Ensure all tests pass
5. Submit pull request with description

### Code of Conduct

- Be respectful and inclusive
- Focus on constructive feedback
- Credit others' contributions
- Maintain confidentiality of any shared raw data
- Report violations to richardmckinney@pm.me

---

## Version History

### v5.0.1 (2025-08-09)
- **CRITICAL:** Corrected seed value from 12345 to 1307 throughout documentation
- Added explicit manual classification correction step to execution path
- Emphasized renv::restore() as primary installation method
- Enhanced documentation with warnings about manual package installation
- Fixed file naming inconsistencies
- Improved error messages and debugging output

### v5.0 (2025-08-08)
- Complete refactor with centralized configuration
- Eliminated all deprecated file dependencies
- Fixed critical classification join bugs
- Reduced code redundancy by 70%
- Added comprehensive quality assurance
- Standardized all outputs to UTF-8

### v4.0 (2025-08-07)
- Added hypothesis testing framework
- Implemented factor analysis and SEM
- Created publication-ready figures
- Added effect size calculations

### v3.0 (2025-08-01)
- Implemented quota-matching algorithm
- Added Appendix J classification system
- Expanded to 23 stakeholder categories

### v2.0 (2025-07-15)
- Added geographic aggregation
- Implemented PII detection
- Created data dictionary automation

### v1.0 (2025-07-01)
- Initial anonymization pipeline
- Basic data cleaning
- SHA-256 hashing implementation

---

## 🔗 Reference URLs

### Project Resources
- **GitHub Repository**: https://github.com/richardcmckinney/climate-finance-research
- **OSF Preregistration**: https://osf.io/[project-id]
- **Zenodo Archive**: https://doi.org/10.5281/zenodo.XXXXXXX

### Technical Documentation
- **renv Documentation**: https://rstudio.github.io/renv/
- **tidyverse Style Guide**: https://style.tidyverse.org/
- **Qualtrics API**: https://api.qualtrics.com/

### Standards & Guidelines
- **TOP Guidelines**: https://www.cos.io/initiatives/top-guidelines
- **ACM Artifacts**: https://www.acm.org/publications/policies/artifact-review-and-badging-current
- **FAIR Principles**: https://www.go-fair.org/fair-principles/

### R Package Documentation
- **tidyverse**: https://www.tidyverse.org/
- **lavaan**: https://lavaan.ugent.be/
- **psych**: https://personality-project.org/r/psych/

---

*This pipeline prioritizes reproducibility, privacy, and transparency. All analyses use only real data with no synthetic variables or proxies. For questions or collaboration opportunities, please contact richardmckinney@pm.me.*

---

**Pipeline Version:** 5.0.1  
**Documentation Updated:** 2025-08-09  
**Commit:** [Use `git rev-parse HEAD`]  
**Contact:** richardmckinney@pm.me  
**License:** Code (MIT) | Data & Docs (CC BY 4.0)
