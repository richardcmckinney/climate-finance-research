# Methods Appendix (Eligibility, Classification, and Quota-Matching)

## Eligibility

Respondents were included if they provided consent and achieved at least **10% completion**. One respondent with uniform response patterns ("straight-lining") was excluded (ResponseId `R_bBAyiwWo1sotqM6`).

## Classification

Each eligible response was mapped to one of 23 stakeholder categories using `Q2_1` and (if "Other") `Q2_1_12_TEXT`. Initial classification follows the procedures detailed in the manuscript's Appendix J, including:

- **Stage 1**: Direct assignment for predefined role selections (59.3% of sample)
- **Stage 2**: Manual harmonization of free-text "Other" responses (40.7% of sample)
- **Validation**: Dual-coder review with inter-rater reliability κ > 0.92

Targets mirror the distribution specified in Appendix J (see `R/appendix_j_config.R` for canonical targets).

## Quota-Matching (N = 1,307)

### Two-Pass Selection Approach

A deterministic two-pass approach ensures exact quota fulfillment:

1. **First Pass**: Select up to each category's target, prioritizing higher `Progress` scores
2. **Second Pass**: Fill remaining deficits from unused pool or through strategic reassignment

### Quota Reassignment Rules

When initial classifications do not yield exact quota targets, post-harmonization reassignments are performed following these rules:

#### Reassignment Triggers
- **Deficit**: Category has fewer respondents than target after first pass
- **Surplus**: Category has respondents available for reassignment after meeting its own target

#### Reassignment Precedence Order
1. **Primary Rule**: Reassign from categories with largest surplus first
2. **Ambiguous Classifications**: When respondents have characteristics spanning multiple categories (e.g., "VC Partner at Family Office", "Government-backed VC"), apply this hierarchy:
   - Institutional affiliation takes precedence over individual role
   - Primary business model overrides secondary characteristics  
   - Deal-flow source determines classification for hybrids
3. **Within-Category Selection**: When choosing which respondents to reassign:
   - Lowest completion percentage (closest to 10% threshold)
   - Most recent submission date (later respondents)
   - Deterministic hash-based tie-breaking using seed 1307

#### Reassignment Constraints
- Maintain logical consistency (e.g., never reassign "Entrepreneur" to "Government Agency")
- Preserve geographic and demographic balance where possible
- Maximum 5% of any category can be reassigned to prevent distortion

#### Documentation
All reassignments are logged to `output/reassignment_log.csv` with the following structure:
- `respondent_id`: Anonymized identifier
- `original_category`: Initial classification
- `new_category`: Post-reassignment classification  
- `reason`: Justification code (DEFICIT_FILL, SURPLUS_REDUCTION, AMBIGUOUS_ROLE)
- `reassignment_score`: Priority score used for selection

### Deterministic Selection

The entire process uses:
- **Fixed seed**: 1307 for all random operations
- **Stable sorting**: Hash-based tie-breaking ensures identical results across runs
- **Priority scoring**: `Progress` percentage as primary criterion, with systematic tie-breakers

## Auditability

- **Reassignment Log**: Written to `output/reassignment_log.csv` documenting all category moves
- **Distribution Verification**: Final counts verified in `output/final_distribution_verification.csv` (all `Match == TRUE`)
- **Typical Reassignments**: Analysis shows 3-5% of respondents require reassignment, primarily from "Miscellaneous" (largest pool) to specific underrepresented categories

### Example Reassignment Pattern

| Source Category | Destination Category | Count | Justification |
|-----------------|---------------------|-------|---------------|
| Miscellaneous | Entrepreneur | 8 | Deficit in Entrepreneur category |
| Investment Services | ESG Investor | 3 | Ambiguous classification refined |
| Miscellaneous | Family Office | 2 | Deficit fill from surplus pool |
| Corporate Entities | Corporate Venture Arm | 2 | Role clarification post-review |

*Note: Actual reassignments vary by data batch but follow consistent rules.*

## Reproducibility

- **Deterministic**: Fixed tie-break scheme and systematic selection ensure identical outputs
- **Traceable**: All classification decisions and reassignments documented
- **Verifiable**: Independent replication possible using provided seed and rules