# Climate Finance Research: Replication Data and Code

## Project Overview

This repository contains the complete anonymized survey data and analysis code for the manuscript:

**"The Capital-Opportunity Mismatch: A Multi-Stakeholder Analysis of Investment Barriers in Climate Finance"**

**Author:** Richard McKinney  
**Email:** richardmckinney@pm.me  
**ORCID:** 0009-0007-1386-6881
**Institution:** University of Oxford

### Study Summary

This study surveyed 1,307 climate finance stakeholders (from 1,563 total survey responses) to identify barriers and solutions to accelerating climate investment. The 1,307 represents respondents with both sufficient completion (≥10%) and successful stakeholder classification per Appendix J methodology. The research reveals a fundamental Capital-Opportunity Mismatch where venture capitalists seek market-ready ventures (86%) while entrepreneurs need patient capital (81%). Through multi-stakeholder analysis, we identify three key findings:

1. **The Capital-Opportunity Mismatch** represents structural misalignment, not merely insufficient funding
2. **Geographic divergence** between Europe's policy-driven and North America's market-led ecosystems  
3. **Investment philosophy** predicts behavior better than organizational category

### Data Collection Details

- **Collection Period:** June-August 2024  
- **Total Responses:** 1,563 survey initiations
- **Usable Responses:** 1,307 (≥10% completion, with stakeholder classifications)
- **Sampling Frame:** 18,964 professionals identified through 47,832 climate-relevant transactions (PitchBook 2010-2024)  
- **Response Rate:** 7.3% (1,307 usable responses from 17,892 delivered invitations)

## Repository Structure

```
climate-finance-research/
├── data/                        # Anonymized survey data (safe to share)
│   ├── survey_responses_anonymized_basic.csv           # Basic anonymized responses (N=1,563)
│   ├── survey_responses_anonymized_preliminary.csv     # With stakeholder classifications (N=1,307)
│   └── data_dictionary.csv                          # Variable descriptions and completeness
├── data_raw/                    # Original data with PII (NOT tracked by Git)
├── R/                           # Analysis scripts
│   ├── 01_anonymize_data.R             # Data anonymization and PII removal
│   ├── 02_classify_stakeholders.R      # Appendix J stakeholder classification
│   ├── 03_main_analysis.R              # Primary analyses and main figures
│   └── 04_hypothesis_testing.R         # Statistical hypothesis testing (12 hypotheses)
├── figures/                     # Generated figures
│   ├── figure4_geographic_distribution.png
│   ├── figure6_financial_impact_focus.png
│   └── [other manuscript figures]
├── output/                      # Analysis outputs
│   ├── stakeholder_classification_verification.csv   # Classification audit trail
│   ├── hypothesis_testing_summary.csv
│   ├── table1_technology_risk.csv
│   ├── factor_analysis_results.csv
│   ├── within_group_coherence.csv
│   ├── stakeholder_distribution.csv
│   └── geographic_distribution.csv
├── docs/                        # Documentation
│   ├── appendix_j_methodology.md              # Complete classification methodology
│   └── appendix_j_classification_template.csv # Template for manual classification
└── README.md                    # This file
```

## Data Privacy and Anonymization

All personally identifiable information (PII) has been removed from the shared dataset:

- **25 PII columns removed** including names, emails, addresses, phone numbers, organization identifiers
- **Direct identifiers replaced** with deterministic hash IDs (SHA-256)
- **Geographic data generalized** to regions (North America, Europe, Asia, Other)
- **Dates generalized** to month-year format
- **Organization names removed** or generalized to categories
- **Raw data with PII** is excluded from version control (.gitignore)

## Stakeholder Classification (Appendix J)

The study employs a sophisticated dual-tiered classification system resulting in 23 distinct stakeholder categories:

- **775 respondents (59.3%)** - Direct assignment from 13 predefined categories
- **532 respondents (40.7%)** - Manual harmonization of free-text "Other" responses
- **Total classified:** 1,307 respondents across 23 categories
- **Validation:** Dual-coder review with inter-rater reliability κ = 0.92

### Complete Stakeholder Distribution (N=1,307)

| Final Role Category | Total Respondents (n) | Percentage of Sample |
|---|---|---|
| Entrepreneur in Climate Technology | 159 | 12.2% |
| Venture Capital Firm | 117 | 9.0% |
| Investment and Financial Services | 109 | 8.3% |
| Private Equity Firm | 88 | 6.7% |
| Business Consulting and Advisory | 79 | 6.0% |
| Nonprofit Organization | 73 | 5.6% |
| High Net-Worth Individual | 66 | 5.0% |
| Government Funding Agency | 53 | 4.1% |
| Academic or Research Institution | 52 | 4.0% |
| Limited Partner | 49 | 3.7% |
| Family Office | 48 | 3.7% |
| Corporate Venture Arm | 47 | 3.6% |
| Angel Investor | 43 | 3.3% |
| ESG Investor | 38 | 2.9% |
| Legal Services | 38 | 2.9% |
| Corporate Entities | 35 | 2.7% |
| Manufacturing and Industrial | 25 | 1.9% |
| Energy and Infrastructure | 24 | 1.8% |
| Real Estate and Property | 20 | 1.5% |
| Philanthropic Organization | 19 | 1.5% |
| Technology and Software | 19 | 1.5% |
| Media and Communication | 7 | 0.5% |
| Miscellaneous and Individual Respondents | 151 | 11.6% |
| **Total** | **1,307** | **100.0%** |

## Key Variables

- `Final_Role_Category`: Stakeholder classification (23 categories per Appendix J)
- `Q2.1`: Original stakeholder role selection
- `Q2.1_12_TEXT`: Free-text responses for "Other" category
- `Q3.3`: Financial vs impact orientation (7-point scale: 1=pure impact, 7=pure financial)
- `Q3.6_1` to `Q3.6_5`: Risk perception ratings
- `Q3.11_*`: Barrier identification questions
- `hq_country`: Geographic region (anonymized to 4 regions)

## Reproducibility Instructions

### Prerequisites

```r
# Install required R packages
install.packages(c(
  "tidyverse",   # Data manipulation and visualization
  "here",        # Project-relative file paths
  "digest",      # Hash generation for anonymization
  "ggplot2",     # Publication-quality figures
  "scales",      # Scale formatting
  "corrplot",    # Correlation matrices
  "psych",       # Factor analysis
  "gridExtra",   # Multiple plot arrangements
  "broom",       # Tidy statistical outputs
  "effectsize",  # Effect size calculations
  "pwr"          # Power analysis
))
```

### Running the Analysis

1. **Clone the repository**:
   ```bash
   git clone https://github.com/[username]/climate-finance-research.git
   cd climate-finance-research
   ```

2. **Run the analysis scripts in order**:
   ```r
   # In R/RStudio:
   source("R/01_anonymize_data.R")        # Already completed, creates anonymized data
   source("R/02_classify_stakeholders.R") # Classifies stakeholders per Appendix J
   source("R/03_main_analysis.R")         # Main analysis and figures
   source("R/04_hypothesis_testing.R")    # Tests all 12 hypotheses
   ```

3. **View outputs**:
   - Figures: `figures/` directory
   - Tables: `output/` directory
   - Classified data: `data/survey_responses_anonymized_preliminary.csv`

## Key Findings Replication

### Main Results
- **86%** of Venture Capitalists (n=117) cite market readiness as primary barrier
- **81%** of Entrepreneurs (n=159) cite limited access to capital
- **Capital-Opportunity Mismatch** identified as structural barrier, not simple funding gap
- **Three-Factor Climate Risk Model** explains 62.3% of variance in risk perceptions
- **Geographic divergence:** Europe focuses on regulatory barriers (86%), North America on cost competitiveness (89%)
- **23 distinct stakeholder categories** identified through dual-tiered classification system

### Output Files
- **Table 1:** `output/table1_technology_risk.csv`
- **Figure 4:** `figures/figure4_geographic_distribution.png`
- **Figure 6:** `figures/figure6_financial_impact_focus.png`
- **Hypothesis Results:** `output/hypothesis_testing_summary.csv`

## Statistical Methods

- **Hypothesis Testing:** 12 theory-driven hypotheses tested using appropriate parametric/non-parametric tests
- **Factor Analysis:** Principal Component Analysis with Varimax rotation (KMO = 0.847, Bartlett's test p<.001)
- **Effect Sizes:** Cohen's d for mean differences, Cramér's V for categorical associations, Pearson's r for correlations
- **Multiple Comparisons:** Bonferroni correction applied within hypothesis families
- **Missing Data:** Little's MCAR test confirmed missing completely at random (χ²=1823.45, df=1799, p=.346)
- **Power Analysis:** Post-hoc power analysis showed adequate power (0.80) for medium effect sizes
- **Sample Size Considerations:** Primary analyses focus on groups with n≥30 for robust statistical inference. Groups with smaller samples (e.g., Philanthropic Organizations n=19, Media and Communication n=7) are included for completeness but findings should be interpreted as exploratory.

## Citation

If you use this data or code, please cite:

```bibtex
@article{mckinney2025capital,
  title={The Capital-Opportunity Mismatch: A Multi-Stakeholder Analysis of Investment Barriers in Climate Finance},
  author={McKinney, Richard},
  journal={[Journal Name]},
  year={2025},
  volume={XX},
  pages={XXX-XXX},
  doi={[DOI]}
}
```

## License

- **Data:** Creative Commons Attribution 4.0 International (CC BY 4.0)
- **Code:** MIT License
- See LICENSE file for full details

## Ethics Statement

This research was approved by the Central University Research Ethics Committee at the University of Oxford (Reference: SOGE C1A24102). All participants provided informed consent before participating, were assured of data confidentiality, and retained the right to withdraw at any point.

## Acknowledgments

We thank the 1,307 climate finance professionals who participated in this survey, the reviewers who provided valuable feedback on the manuscript, and the PitchBook team for access to transaction data.

## Contact

**Richard McKinney**  
Email: richardmckinney@pm.me  
ORCID: 0009-0007-1386-6881  
Institution: University of Oxford

For questions about the data or analysis, please open an issue in this repository or contact the author directly.

---

# GitHub Publishing Instructions

## Step-by-Step Guide to Publish to GitHub

### 1. Stage and Commit All Files
```bash
# In RStudio Terminal or Git pane:
git add .
git commit -m "Complete replication package: Climate finance research data and analysis code"
```

### 2. Create GitHub Repository and Push
```r
# In RStudio Console:
usethis::use_github(
  private = FALSE,  # Make it public
  protocol = "https",
  host = "https://github.com"
)
```

This will:
- Create a new repository on your GitHub account
- Set up the remote connection
- Push your local commits to GitHub
- Open the repository in your browser

### 3. Alternative: Manual GitHub Setup

If the above doesn't work, you can manually create the repository:

1. Go to https://github.com/new
2. Repository name: `climate-finance-research`
3. Description: "Replication data and code for Capital-Opportunity Mismatch in Climate Finance"
4. Set to **Public**
5. Don't initialize with README (you already have one)
6. Click "Create repository"

Then in RStudio Terminal:
```bash
git remote add origin https://github.com/yourusername/climate-finance-research.git
git branch -M main
git push -u origin main
```

### 4. Verify Files on GitHub

Check that these critical files are visible:
- [ ] README.md with complete stakeholder distribution table
- [ ] data/survey_responses_anonymized_basic.csv (N=1,563 total responses)
- [ ] data/survey_responses_anonymized_preliminary.csv (N=1,307 classified responses)
- [ ] output/stakeholder_classification_verification.csv (audit trail)
- [ ] All R scripts in R/
- [ ] All figures in figures/
- [ ] All outputs in output/

The classified dataset should show exactly 1,307 respondents distributed across 23 categories as per the table above.

### 5. Add Repository Link to Manuscript

In your manuscript's data availability statement:
```
Replication data and code are publicly available at: 
https://github.com/[yourusername]/climate-finance-research

The repository includes anonymized survey data (N=1,563), 
stakeholder classification algorithms implementing Appendix J 
methodology, and complete R code to reproduce all analyses 
and figures presented in this manuscript.
```

## Troubleshooting

### If you get authentication errors:
```r
# Set up GitHub PAT (Personal Access Token):
usethis::create_github_token()  # Opens GitHub to create token
gitcreds::gitcreds_set()        # Enter the token when prompted
```

### If files are too large:
The anonymized CSV should be ~2-5 MB. If larger:
- Remove additional unnecessary columns
- Compress to .csv.gz format
- Use Git LFS for large files

### If you need to update files later:
```bash
git add -A
git commit -m "Update: [describe changes]"
git push
```

## Final Checklist

Before sharing the repository link:

- [ ] All PII removed from data
- [ ] Scripts run without errors on clean environment
- [ ] Stakeholder classification yields 1,307 total across 23 categories
- [ ] Distribution matches Appendix J table (largest groups: Entrepreneurs n=159, Miscellaneous n=151, VCs n=117)
- [ ] README clearly explains the project with complete stakeholder table
- [ ] License files included (separate for data and code)
- [ ] Citation information complete
- [ ] Repository is PUBLIC (not private)
- [ ] Test that someone else can clone and run your code
- [ ] Manuscript DOI updated when available
- [ ] ORCID added if available

---

## Notes for Reviewers

This repository provides full transparency for our research methodology. The classification algorithm in `R/02_classify_stakeholders.R` implements the harmonization rules detailed in Appendix J of the manuscript. While some free-text responses required manual review in the original analysis, this code provides a systematic approach that can be verified and improved upon.

The dual-coder validation process achieved high inter-rater reliability (κ = 0.92), and all classification decisions are documented in the `docs/appendix_j_methodology.md` file.

**Note on Classification:** The "Miscellaneous and Individual Respondents" category (n=151, 11.6%) includes respondents whose roles did not fit clearly into the primary 22 categories. These include self-employed individuals, private investors, small business owners, and other professionals with climate finance involvement. This category ensures complete representation of all survey participants while maintaining the integrity of the main stakeholder classifications.

---

## Reproduce Everything With One Command

**Quick start for reviewers:**

```r
source("run_all.R")
```

This executes:

1. `R/01_anonymize_data.R` → writes
   - `data/survey_responses_anonymized_basic.csv`
   - `docs/appendix_j_classification_template.csv`
   - `data/data_dictionary.csv`
2. `R/02_classify_stakeholders.R` → writes
   - `data/survey_responses_anonymized_preliminary.csv` (or `data/survey_responses_anonymized_classified.csv` if final mapping is present)

### Determinism & Safety
* All PII columns are removed by name and **all remaining free‑text is scrubbed** for emails, phone numbers, URLs, and @handles.
* Outputs are deterministic; re-running on identical `data_raw/*.csv` produces identical files.

### Expected public artifacts
- `data/survey_responses_anonymized_basic.csv`
- `docs/appendix_j_classification_template.csv`
- `data/survey_responses_anonymized_preliminary.csv`
- `data/data_dictionary.csv`
