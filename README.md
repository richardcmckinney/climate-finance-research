# Climate Finance Investment Barriers: Research Data and Analysis

## Overview
This repository contains the complete anonymized survey data and analysis code for the study:  
**"The Capital-Opportunity Mismatch: A Multi-Stakeholder Analysis of Investment Barriers in Climate Finance"**

## Research Overview
**Author:** Richard McKinney  
**Data Collection:** June-August 2024  
**Sample:** 1,307 climate finance stakeholders (from 1,563 total survey responses)  
**Sampling Frame:** 18,964 professionals identified through 47,832 climate-relevant transactions (2010-2024)  
**Response Rate:** 7.3% (1,307 usable responses from 17,892 delivered invitations)

## Repository Structure
```
climate-finance-research/
├── data/                        # Anonymized survey data (safe to share)
│   ├── survey_responses_anonymized_basic.csv     # Basic anonymized responses (N=1,563)
│   └── data_dictionary.csv                       # Variable descriptions and completeness
├── data_raw/                    # Original data with PII (NOT tracked by Git)
├── data_processed/              # Enhanced datasets
│   ├── survey_responses_anonymized_classified.csv    # With Appendix J classifications
│   └── survey_responses_anonymized_preliminary.csv   # With preliminary classifications
├── R/                           # Analysis scripts
│   ├── 01_anonymize_data.R     # Data anonymization and PII removal
│   ├── 02_main_analysis.R      # Primary analyses from manuscript
│   ├── 03_generate_figures.R   # Figure generation
│   └── 04_hypothesis_tests.R   # Statistical hypothesis testing
├── figures/                     # Generated figures
├── output/                      # Analysis outputs
│   └── analysis_results.rds    # Saved R objects
├── docs/                        # Documentation
│   ├── appendix_j_methodology.md              # Complete classification methodology
│   ├── appendix_j_classification_template.csv # Template for manual classification
│   └── appendix_j_classifications_complete.csv # Final classifications (when available)
└── README.md
```

## Data Privacy and Anonymization

All personally identifiable information (PII) has been removed from the shared dataset:
- **25 PII columns removed** including names, emails, addresses, phone numbers
- **Direct identifiers replaced** with deterministic hash IDs (SHA-256)
- **Geographic data generalized** to regions (North America, Europe, Asia, Other)
- **Dates generalized** to month-year format
- **Organization names removed** or generalized
- **Raw data with PII** is excluded from version control (.gitignore)

## Stakeholder Classification (Appendix J)

The study employs a sophisticated dual-tiered classification system:
- **775 respondents (59.3%)** - Direct assignment from 13 predefined categories
- **532 respondents (40.7%)** - Manual harmonization of free-text "Other" responses
- **Final distribution** across 23 distinct stakeholder categories
- **Validation:** Dual-coder review with inter-rater reliability κ = 0.92
- **Key groups:** Entrepreneurs (12.2%), VCs (9.0%), Investment Services (8.3%), Private Equity (6.7%)

## Key Findings

- **86%** of Venture Capitalists cite market readiness as primary barrier
- **81%** of Entrepreneurs cite limited access to capital
- **Capital-Opportunity Mismatch** identified as structural barrier, not simple funding gap
- **Three-Factor Climate Risk Model** explains 62.3% of variance in risk perceptions
- **Geographic divergence:** Europe focuses on regulatory barriers (86%), North America on cost competitiveness (89%)

## Reproducing the Analysis

### Prerequisites
```r
# Install required R packages
install.packages(c(
  "tidyverse",   # Data manipulation and visualization
  "here",        # Project-relative file paths
  "digest",      # Hash generation for anonymization
  "psych",       # Factor analysis
  "effectsize",  # Effect size calculations
  "broom"        # Tidy statistical outputs
))
```

### Step-by-Step Reproduction

```bash
# 1. Clone this repository
git clone https://github.com/[username]/climate-finance-research.git

# 2. Set working directory to project root
cd climate-finance-research
```

```r
# 3. Run anonymization (only if starting from raw data)
# Note: Requires raw data file in data_raw/ directory
source("R/01_anonymize_data.R")

# 4. Run main analysis
source("R/02_main_analysis.R")

# 5. Generate figures
source("R/03_generate_figures.R")

# 6. Run hypothesis tests
source("R/04_hypothesis_tests.R")
```

## Data Files Description

### Primary Data Files
- `survey_responses_anonymized_basic.csv` - Core anonymized survey responses with all PII removed
- `survey_responses_anonymized_classified.csv` - Enhanced version with Appendix J stakeholder classifications
- `data_dictionary.csv` - Complete variable descriptions, types, and completeness statistics

### Classification Files
- `appendix_j_methodology.md` - Detailed methodology for 23-category classification system
- `appendix_j_classification_template.csv` - Template for manual classification of free-text responses
- `appendix_j_classifications_complete.csv` - Final manual classifications (when completed)

## Statistical Methods
- **Hypothesis Testing:** 12 theory-driven hypotheses tested using appropriate parametric/non-parametric tests
- **Factor Analysis:** Principal Component Analysis with Varimax rotation (KMO = 0.847)
- **Effect Sizes:** Cohen's d, Cramér's V, and correlation coefficients reported
- **Multiple Comparisons:** Bonferroni correction applied within hypothesis families
- **Missing Data:** Little's MCAR test confirmed missing completely at random (p = 0.346)

## Citation
If you use this data or code, please cite:
```
McKinney, R. (2025). The Capital-Opportunity Mismatch: A Multi-Stakeholder 
Analysis of Investment Barriers in Climate Finance. [Journal Name], 
[Volume], [Pages]. https://doi.org/[DOI]
```

## License
- **Data:** Creative Commons Attribution 4.0 International (CC BY 4.0)
- **Code:** MIT License
- See LICENSE file for details

## Ethics Statement
This research was approved by the Central University Research Ethics Committee at the University of Oxford (Reference: SOGE C1A24102). All participants provided informed consent.

## Contact
Richard McKinney  
Email: richardmckinney@pm.me  
ORCID: [Your ORCID if available]  
Institution: [Your Institution]

## Acknowledgments
We thank the 1,307 climate finance professionals who participated in this survey and the reviewers who provided valuable feedback on the manuscript.
