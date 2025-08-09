# Climate Finance Research — Reproducible Pipeline

**Version 4.0** | **Last Updated: 2025-08-08**

This repository contains the complete, deterministic, and validated pipeline for the manuscript **"The Capital–Opportunity Mismatch: A Multi‑Stakeholder Analysis of Climate Finance Barriers and Solutions."**

The pipeline transforms **raw survey exports** → **anonymized public data** → **Appendix J classifications** → **analysis outputs** with complete reproducibility and data integrity guarantees.

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
- ✅ **Deterministic:** Fixed seed (12345) and UTC timezone ensure identical outputs
- ✅ **Centralized Configuration:** All paths and parameters in `R/00_config.R`
- ✅ **Privacy-First:** Strict PII detection and removal with automated checks
- ✅ **Data Integrity:** No synthetic/proxy data generation - analyses use only real data
- ✅ **Quality Assured:** Automated QA checks at every pipeline stage

---

## 📊 Core Outputs

The pipeline guarantees four public artifacts (N = 1,563):

| Output File | Description | Records |
|------------|-------------|---------|
| `data/survey_responses_anonymized_basic.csv` | Fully anonymized dataset with PII removed | 1,563 |
| `data/survey_responses_anonymized_preliminary.csv` | Anonymized data + Appendix J classifications | 1,563 |
| `docs/appendix_j_classification_template.csv` | Classification audit template | 1,563 |
| `data/data_dictionary.csv` | Column metadata and completeness statistics | - |

### Analysis Outputs (--with-analysis flag)

| Output File | Description | Records |
|------------|-------------|---------|
| `data/climate_finance_survey_final_1307.csv` | Quota-matched analysis subset | 1,307 |
| `output/` | Statistical results, hypothesis tests, model outputs | - |
| `figures/` | Publication-ready figures and visualizations | - |

> **Note:** The manuscript uses the N=1,307 quota-matched subset for all analyses, while the public datasets contain the full N=1,563 anonymized responses.

---

## 🏗️ Pipeline Architecture

### Central Configuration System

```
R/
├── 00_config.R              # Central paths, parameters, validation functions
├── appendix_j_config.R      # Appendix J targets, helper functions
├── 01_anonymize_data.R      # PII removal, geographic generalization
├── 02_classify_stakeholders.R # Stakeholder classification (23 categories)
├── get_exact_1307.R         # Deterministic quota-matching
├── 03_main_analysis.R       # Descriptive statistics and visualizations
├── 04_hypothesis_testing.R  # H1-H12 hypothesis tests, factor models
└── 99_quality_checks.R      # Comprehensive quality assurance

run_all.R                    # Simplified pipeline runner (300 lines, was 1000+)
```

### Key Improvements in v4.0

1. **Centralized Configuration:** All file paths and parameters defined in `00_config.R`
2. **No Deprecated Files:** Active detection and warnings for legacy file names
3. **Fixed Critical Bugs:** Resolved join failures in classification script
4. **Eliminated Redundancy:** Shared helper functions in `appendix_j_config.R`
5. **Strict Privacy:** Hardened PII detection with word boundaries
6. **Data Integrity:** Removed all proxy/synthetic variable creation
7. **Simplified Runner:** `run_all.R` reduced by 70% while improving functionality

---

## ⚠️ Important: Deprecated Files

The pipeline actively checks for and warns about deprecated file names that should be **deleted**:

```
❌ DEPRECATED (delete if found):
  - data/climate_finance_survey_classified.csv
  - data/climate_finance_survey_anonymized.csv

✅ CORRECT names used by pipeline:
  - data/survey_responses_anonymized_basic.csv
  - data/survey_responses_anonymized_preliminary.csv
  - data/climate_finance_survey_final_1307.csv
```

---

## 🔒 Data Privacy & Anonymization

### PII Removal Strategy
- **Columns Removed:** Names, emails, addresses, phone numbers, organization identifiers
- **IDs Hashed:** SHA-256 deterministic hashing for reproducible anonymization
- **Geography Generalized:** Raw locations → 4 regions (North America, Europe, Asia, Other)
- **Dates Bucketed:** Full dates → Year-Month format only
- **Text Scrubbed:** Automated PII detection and redaction in free-text fields
- **Consent Required:** Only records with explicit consent are included

### Privacy Validation
- **Automated Checks:** Every script validates no PII in outputs
- **Strict Mode:** Pipeline halts if PII detected (stop_on_violation = TRUE)
- **Audit Trail:** All privacy checks logged in `output/quality_assurance_report.csv`

---

## 📈 Analysis Framework

### Stakeholder Classification (23 Categories)

The classification system uses a robust `case_when` logic with priority ordering:

1. **Climate-Specific:** Entrepreneur in Climate Technology (12.2%)
2. **Investment Firms:** VC (9.0%), PE (6.7%), Angel (3.3%)
3. **Institutional:** Government (4.1%), Academic (4.0%)
4. **Financial Services:** Investment/Banking (8.3%)
5. **Advisory:** Consulting (6.0%), Legal (2.9%)
6. **Impact-Focused:** ESG Investor (2.9%), Philanthropic (1.5%)
7. **Other:** 8 additional categories + Miscellaneous (11.6%)

### Hypothesis Testing (H1-H12)

All tests run only on available real data (no proxies):

| Hypothesis | Test | Data Required | Status |
|-----------|------|---------------|---------|
| H1-H2 | Technology risk perception | Q3.6_1 | ✓ |
| H3 | Risk correlations | Q3.6_1, Q3.6_2 | ✓ |
| H4-H5 | Market barriers | Q3.11_1 | ✓ |
| H6 | International scalability | Q3.11_5 | ✓ |
| H7 | Ecosystem support | Q3.11_ecosystem | ✓ |
| H8 | Geographic differences | region + Q3.6_3 | ✓ |
| H9 | Impact orientation | Q3.3 | ✓ |
| H10 | Strategic coherence | Multiple | ✓ |
| H11 | Physical-operational risk | Q3.6_4, Q3.6_5 | ✓ |
| H12 | Technology solutions | Q3.7 (if available) | Conditional |

---

## 🛠️ Installation & Setup

### Prerequisites

```r
# Core packages (required)
install.packages(c(
  "tidyverse",    # Data manipulation and visualization
  "cli",          # Enhanced console output
  "digest",       # SHA-256 hashing
  "lubridate"     # Date handling
))

# Analysis packages (for --with-analysis)
install.packages(c(
  "psych",        # Factor analysis, KMO
  "lavaan",       # Confirmatory factor analysis
  "effectsize",   # Effect size calculations
  "corrplot"      # Correlation visualization
))
```

### Data Setup

1. Create `data_raw/` directory
2. Place raw survey CSV file(s) in `data_raw/`
3. Run pipeline as shown in Quick Start

---

## 🔄 Manual Execution Path

For step-by-step execution or debugging:

```r
# 1. Load configurations (REQUIRED FIRST)
source("R/00_config.R")
source("R/appendix_j_config.R")

# 2. Core pipeline
source("R/01_anonymize_data.R")         # Creates anonymized_basic.csv
source("R/02_classify_stakeholders.R")  # Creates anonymized_preliminary.csv

# 3. Analysis pipeline (optional)
source("R/get_exact_1307.R")           # Creates final_1307.csv
source("R/03_main_analysis.R")         # Creates figures/
source("R/04_hypothesis_testing.R")    # Creates output/tables/

# 4. Quality checks (recommended)
source("R/99_quality_checks.R")
run_quality_checks()                   # Returns pass/fail status
```

---

## 🐛 Troubleshooting

### Common Issues & Solutions

| Issue | Solution |
|-------|----------|
| "No CSV files found in data_raw" | Place raw survey export in `data_raw/` directory |
| "Deprecated file found" | Delete old files listed in warning, re-run pipeline |
| "Privacy violation detected" | Check for non-anonymized columns (e.g., Q2.2), run anonymization |
| "Role column not found" | Ensure `Final_Role_Category` or `final_category_appendix_j` exists |
| "Progress column missing" | Check for `Progress` or `completion` column in raw data |

### Validation Commands

```r
# Check for deprecated files
check_deprecated(verbose = TRUE)

# Validate current pipeline stage
validate_stage("classify")  # Options: anonymize, classify, quota, analyze

# Test privacy compliance
check_privacy_violations(df, stop_on_violation = FALSE)

# Generate checksums for reproducibility
source("R/99_quality_checks.R")
generate_checksums()
```

---

## 📋 Quality Assurance

The pipeline includes comprehensive quality checks via `R/99_quality_checks.R`:

- ✅ No deprecated files present
- ✅ Standard column names used consistently  
- ✅ No PII in output files
- ✅ Geographic data properly anonymized
- ✅ Final sample size = 1,307
- ✅ Data dictionary synchronized
- ✅ Checksums generated for reproducibility

Run quality checks: `Rscript R/99_quality_checks.R`

---

## 📚 Project Details

### Study Overview
- **Author:** Richard McKinney (richardmckinney@pm.me)
- **Institution:** University of Oxford
- **ORCID:** 0009-0007-1386-6881
- **Ethics:** Approved by Oxford CUREC (Ref: SOGE C1A24102)

### Data Collection
- **Period:** June–August 2024
- **Frame:** 18,964 professionals from 47,832 climate transactions (PitchBook 2010–2024)
- **Response Rate:** 7.3% (1,307 usable from 17,892 delivered)
- **Geographic Coverage:** 42 countries aggregated to 4 regions

### Key Findings
1. **Capital–Opportunity Mismatch:** Structural misalignment between VC expectations and entrepreneur needs
2. **Geographic Divergence:** Europe (policy-driven) vs North America (market-led) approaches
3. **Investment Philosophy:** Behavioral predictor stronger than organizational category

---

## 📖 Citation

```bibtex
@article{mckinney2025capital,
  title={The Capital–Opportunity Mismatch: A Multi-Stakeholder Analysis of Climate Finance Barriers and Solutions},
  author={McKinney, Richard},
  year={2025},
  institution={University of Oxford}
}
```

---

## 📄 License

- **Code:** MIT License
- **Data:** CC BY 4.0 International
- **Documentation:** CC BY 4.0 International

See LICENSE files for full terms.

---

## 🤝 Contributing & Support

- **Issues:** Use GitHub Issues for bug reports or questions
- **Contact:** richardmckinney@pm.me
- **Updates:** Check releases for pipeline version updates

### Version History

- **v4.0 (2025-08-08):** Complete refactor with centralized configuration
- **v3.0:** Added quota-matching and hypothesis testing
- **v2.0:** Implemented Appendix J classification
- **v1.0:** Initial anonymization pipeline

---

*This pipeline prioritizes reproducibility, privacy, and data integrity. All analyses use only real data with no synthetic variables or proxies.*