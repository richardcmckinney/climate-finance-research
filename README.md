# Climate Finance Research — Reproducible Pipeline

This repository contains the complete, deterministic pipeline used in the manuscript **“The Capital–Opportunity Mismatch: A Multi‑Stakeholder Analysis of Climate Finance Barriers and Solutions.”**

The pipeline proceeds from **raw survey exports** to **anonymized public data**, **Appendix J classifications**, and (optionally) **all manuscript analysis outputs**. A single command regenerates the public artifacts; an optional flag adds the full analysis stage (tables/figures).

---

## Quick start

```bash
# Minimal: regenerate the four public artifacts only
Rscript run_all.R --clean

# Full: also run the manuscript analysis stage (creates figures/ and extra outputs)
Rscript run_all.R --clean --with-analysis

# Verify current working tree against checksums
Rscript run_all.R --verify
```

**Determinism:** The scripts set a fixed seed and UTC timezone. Running the same input twice yields identical outputs and checksums.

---

## Outputs (public artifacts guaranteed by `run_all.R`)

All paths are relative to the repo root:

- `data/survey_responses_anonymized_basic.csv` — **N = 1,563**, the fully anonymized, analysis‑ready dataset (PII removed; temporal & geography generalization applied as documented).
- `docs/appendix_j_classification_template.csv` — classification template for reviewer inspection and optional manual harmonization.
- `data/survey_responses_anonymized_preliminary.csv` — **N = 1,563**, anonymized dataset joined with Appendix J preliminary + final classifications.
- `data/data_dictionary.csv` — column‑level dictionary regenerated on every run to **exactly** match the current anonymized dataset.

> **Note on N=1,307 vs N=1,563**  
> The manuscript’s analysis uses a deterministic **quota‑matched analysis subset** of size **N = 1,307** (see `R/get_exact_1307.R`). The public anonymized datasets (`*_basic.csv`, `*_preliminary.csv`) contain **N = 1,563**.

---

## Optional analysis stage (manuscript tables & figures)

When invoked with `--with-analysis`, the pipeline additionally:

- Generates the **N=1,307** analysis subset (`data/climate_finance_survey_final_1307.csv`).
- Reproduces descriptive statistics, hypothesis tests (H1–H12), factor models (EFA/CFA), and regressions.
- Writes CSVs to `output/` and figures to `figures/`.

> **Figures directory**  
> The `figures/` directory is created **only** when `--with-analysis` is used. In minimal runs, it is not created.

---

## Repository structure

```
R/
  01_anonymize_data.R          # Strip PII, normalize fields, write *_basic.csv and data_dictionary.csv
  02_classify_stakeholders.R   # Apply Appendix J regex logic; write classification template and *_preliminary.csv
  get_exact_1307.R             # Deterministic quota-matching to N=1,307 for manuscript analyses
  03_main_analysis.R           # Descriptives & derived outputs used by the manuscript
  04_hypothesis_testing.R      # Reproduces H1–H12 tests, EFA/CFA, and regressions; writes output/ & figures/

run_all.R                      # Single entry point; add --with-analysis to execute 03 & 04

data/
  data_raw/                    # (ignored in git) raw survey exports
  survey_responses_anonymized_basic.csv
  survey_responses_anonymized_preliminary.csv
  climate_finance_survey_final_1307.csv     # (created when --with-analysis is used)
  data_dictionary.csv

docs/
  appendix_j_systematic_classification.md   # (renamed for shell-friendliness)
  appendix_j_classification_template.csv
  checksums.txt
  verification_report.md

output/                        # (created/updated during analysis stage)
figures/                       # (created/updated during analysis stage)
```

---

## Scripts and what they do

- **`R/01_anonymize_data.R`**  
  Loads `data_raw/`, removes PII, applies controlled transforms (e.g., date bucketing), **generalizes geography to 4 regions**, and writes 
  `data/survey_responses_anonymized_basic.csv` and `data/data_dictionary.csv`.

- **`R/02_classify_stakeholders.R`**  
  Applies Appendix J classification regex with robust fallbacks. Emits 
  `docs/appendix_j_classification_template.csv` and 
  `data/survey_responses_anonymized_preliminary.csv`. Guarantees no `NA` in `final_category_appendix_j`.

- **`R/get_exact_1307.R`**  
  Generates the N=1,307 quota‑matched analysis dataset **deterministically** from the classified dataset. 
  Input defaults to `data/survey_responses_anonymized_preliminary.csv` and it accepts either 
  `final_category_appendix_j` or `Final_Role_Category` for roles.  
  **Important:** The script now preserves the truth label (`final_category_appendix_j` / `Final_Role_Category`) and writes any quota adjustments to a **separate** column `quota_target_category` (no relabeling of the truth column).

- **`R/03_main_analysis.R`**  
  Produces descriptive tables/plots used in the manuscript. Writes into `output/` and `figures/`.

- **`R/04_hypothesis_testing.R`**  
  Reproduces H1–H12 tests (ANOVA/χ²/t‑tests/correlations), EFA/CFA for the Three‑Factor Climate Risk Model, and regressions. 
  Outputs CSVs in `output/` and figures in `figures/`.

- **`run_all.R`**  
  Entry point. Always regenerates the four public artifacts; with `--with-analysis`, also runs 03 & 04. Produces `docs/checksums.txt` and `docs/verification_report.md`.

---

## Project Overview

**Author:** Richard McKinney  
**Email:** richardmckinney@pm.me  
**ORCID:** 0009-0007-1386-6881  
**Institution:** University of Oxford

### Study Summary

This study surveyed 1,307 climate finance stakeholders (from 1,563 total survey responses) to identify barriers and solutions to accelerating climate investment. The **1,307** represents the deterministic analysis subset (quota-matched; see `R/get_exact_1307.R`), while **1,563** is the full anonymized sample. The research reveals a fundamental Capital–Opportunity Mismatch where venture capitalists seek market‑ready ventures while entrepreneurs need patient capital. Through multi‑stakeholder analysis, we identify three key findings:

1. **The Capital–Opportunity Mismatch** represents structural misalignment, not merely insufficient funding.  
2. **Geographic divergence** between Europe’s policy‑driven and North America’s market‑led ecosystems.  
3. **Investment philosophy** predicts behavior better than organizational category.

### Data Collection Details

- **Collection Period:** June–August 2024  
- **Total Responses:** 1,563 survey initiations  
- **Analysis Subset:** 1,307 (quota‑matched; see `R/get_exact_1307.R`)  
- **Sampling Frame:** 18,964 professionals identified through 47,832 climate‑relevant transactions (PitchBook 2010–2024)  
- **Response Rate:** 7.3% (1,307 usable responses from 17,892 delivered invitations)

---

## Data Privacy and Anonymization

All personally identifiable information (PII) has been removed from the shared dataset:

- **PII columns removed** including names, emails, addresses, phone numbers, organization identifiers.  
- **Direct identifiers replaced** with deterministic hash IDs (SHA‑256).  
- **Geographic data generalized** to regions (North America, Europe, Asia, Other).  
- **Dates generalized** to month‑year format.  
- **Organization names removed** or generalized to categories.  
- **Consent policy:** Only records with **explicit consent** are retained in public artifacts and analysis subsets; **NA or missing consent is treated as non‑consent**.  
- **Raw data with PII** is excluded from version control (`.gitignore`).

---

## Stakeholder Classification (Appendix J)

The study employs a dual‑tier classification system resulting in **23** stakeholder categories:

- **Direct assignment** from predefined categories.  
- **Manual harmonization** of free‑text “Other” responses following MECE principles.

### Final Stakeholder Distribution (Analysis subset N = 1,307)

> The full anonymized datasets published in `data/` contain **N = 1,563** rows. The deterministic analysis subset is created by `R/get_exact_1307.R` and used for manuscript figures/tables.

**Quota logic and transparency**  
- The classification truth is stored in `final_category_appendix_j` (or `Final_Role_Category`).  
- Quota adjustments are stored separately in `quota_target_category`.  
- **We never overwrite the truth label** for quota purposes; all balancing uses `quota_target_category`.  
- We publish `docs/verification_report.md` and `docs/checksums.txt` on every run.

---

### Final Stakeholder Distribution (Analysis subset N = 1,307)

| Final Role Category | Total Respondents (n) | Percentage of Sample |
|---|---:|---:|
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

- `final_category_appendix_j` / `Final_Role_Category`: Stakeholder classification (23 categories per Appendix J)  
- `quota_target_category`: Quota‑matched category used for subset balancing (truth label is preserved separately as above)  
- `Q2.1`: Original stakeholder role selection  
- `Q2.1_12_TEXT`: Free‑text responses for “Other” category  
- `Q3.3`: Financial vs impact orientation (7‑point scale: 1=pure impact, 7=pure financial)  
- `Q3.6_1` to `Q3.6_5`: Risk perception ratings  
- `Q3.11_*`: Barrier identification questions  
- `hq_country`: Geographic region (anonymized to 4 regions)

---

## Prerequisites (R packages)

```r
install.packages(c(
  "tidyverse","here","digest","ggplot2","scales","corrplot",
  "psych","gridExtra","broom","effectsize","pwr","lavaan","MASS"
))
```

---

## Statistical Methods (overview)

- **Hypothesis Testing:** 12 theory‑driven hypotheses (ANOVA/Welch, χ², t‑tests, correlations).  
- **Factor Analysis:** EFA (KMO/Bartlett checked) and CFA (lavaan, 3‑factor template).  
- **Effect Sizes:** Cohen’s d, Cramér’s V, Pearson’s r.  
- **Multiple Comparisons:** Bonferroni within hypothesis families.  
- **Missing Data:** MCAR checks; robust fallbacks used where appropriate.  
- **Sample Size Considerations:** Primary inference focuses on groups with n ≥ 30; smaller groups reported with caution.

---
## Key Findings (replication highlights)

- Venture capitalists emphasize market readiness; entrepreneurs emphasize access to patient capital (Capital–Opportunity Mismatch).  
- Geographic divergence: Europe policy-driven vs North America market-led.  
- Investment philosophy predicts behavior beyond organizational labels.  
- A three-factor structure captures climate-risk perception across items (see EFA/CFA outputs).

---

## Reproducibility – alternate (manual) run path

```r
source("R/01_anonymize_data.R")
source("R/02_classify_stakeholders.R")
source("R/get_exact_1307.R")     # creates the N=1,307 analysis subset
source("R/03_main_analysis.R")
source("R/04_hypothesis_testing.R")
```

Outputs appear in `data/`, `output/`, and (if analysis stage is run) `figures/`.

---

## Ethics Statement

This research was approved by the Central University Research Ethics Committee at the University of Oxford (Reference: SOGE C1A24102). All participants provided informed consent, were assured of data confidentiality, and retained the right to withdraw.

---

## Acknowledgments

We thank the 1,307 climate finance professionals who participated in this survey, the reviewers who provided valuable feedback, and the PitchBook team for access to transaction data.

---

## Notes on file naming

- Renamed `docs/# Appendix J: Systematic Classification .md` → `docs/appendix_j_systematic_classification.md` to avoid shell‑hostile characters and trailing spaces.  
- Removed the legacy `data/climate_finance_survey_anonymized.csv` to prevent confusion; use the canonical `survey_responses_anonymized_basic.csv`.

---
## Citation

If you use this repository or datasets, please cite the associated manuscript: McKinney, R. (2025). The Capital–Opportunity Mismatch: A Multi-Stakeholder Analysis of Climate Finance Barriers and Solutions.

---

## License

- **Code:** MIT License  
- **Data:** Creative Commons Attribution 4.0 International (CC BY 4.0)  
See LICENSE files for full details.

---

## Contact

**Richard McKinney**  
Email: richardmckinney@pm.me  
ORCID: 0009-0007-1386-6881  
Institution: University of Oxford

For questions about the data or analysis, please open an issue or contact the author directly.