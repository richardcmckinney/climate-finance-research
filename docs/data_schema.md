# Data Schema Specification

## Standard Column Names

### Core Identifiers
- `respondent_id`: Unique SHA-256 hash identifier
- `Progress`: Numeric 0-100 completion percentage
- `Final_Role_Category`: Standardized Appendix J classification

### Geographic Fields (Anonymized Only)
- `region`: High-level geographic region (Europe, North America, Asia, Other)
- NEVER: Raw country names, cities, or Q2.2 data

### Classification Fields
- `Role_Raw`: Original Q2.1 response
- `Role_Other_Text`: Free text from Q2.1_12_TEXT
- `Classification_Stage`: Pipeline stage marker
- `Classification_Version`: Schema version number

## Privacy Requirements
1. All geographic data MUST be aggregated to regions
2. No organization names in any output
3. Response IDs must be hashed
4. Dates limited to month-year precision

## File Naming Conventions
- Stage 1: `survey_responses_anonymized_*.csv`
- Stage 2: `*_classified.csv` or `*_preliminary.csv`
- Stage 3: `*_final_1307.csv`
- Stage 4: `hypothesis_*.csv`, `figure*.png`