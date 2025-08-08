# Methods Appendix (Eligibility, Classification, and Quota-Matching)

**Eligibility.** Respondents were included if they provided consent and achieved at least **10% completion**. One respondent with uniform response patterns (“straight-lining”) was excluded (ResponseId `R_bBAyiwWo1sotqM6`).

**Classification.** Each eligible response was mapped to one of 23 stakeholder categories using `Q2_1` and (if “Other”) `Q2_1_12_TEXT`. Targets mirror Appendix J.

**Quota-matching (N = 1,307).** Deterministic two-pass approach: first select up to each target, prioritizing higher `Progress`; then fill remaining deficits from unused pool or reassign from the largest populated categories until all 23 match Appendix J exactly.

**Auditability.** Reassignments are written to `output/reassignment_log.csv`. Final counts are verified in `output/final_distribution_verification.csv` (all `Match == TRUE`).

**Reproducibility.** Fixed tie-break scheme and deterministic selection.
