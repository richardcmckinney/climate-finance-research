<file name=0 path=/Users/rmckinney/Documents/GitHub/climate-finance-research/R/01_anonymize_data.R>an_basic <- raw %>%
  # ... previous transformations ...
  relocate(respondent_id, .before = 1)

# Scrub PII in character columns EXCEPT respondent_id (to preserve deterministic keys)
char_cols <- names(an_basic)[vapply(an_basic, is.character, logical(1))]
char_cols <- setdiff(char_cols, "respondent_id")
if (length(char_cols)) {
  an_basic <- an_basic %>% mutate(across(all_of(char_cols), scrub_text))
}
</file>

<file name=0 path=/Users/rmckinney/Documents/GitHub/climate-finance-research/R/02_classify_stakeholders.R>template <- role_df %>%
  mutate(preliminary_category = case_when(
    role %in% c("Investor", "Asset Manager", "Pension Fund") ~ "Investor",
    role %in% c("Company", "Corporate") ~ "Company",
    TRUE ~ "Other"
  ))

template <- template %>% arrange(respondent_id) %>% distinct(respondent_id, .keep_all = TRUE)

out_prelim <- an_basic %>%
  left_join(template %>% select(respondent_id, preliminary_category), by = "respondent_id") %>%
  rename(stakeholder_category = preliminary_category) %>%
  arrange(respondent_id) %>%
  distinct(respondent_id, .keep_all = TRUE)

out_class <- an_basic %>%
  left_join(template %>% select(respondent_id, preliminary_category), by = "respondent_id") %>%
  left_join(final_map, by = "respondent_id") %>%
  mutate(stakeholder_category = coalesce(final_category_appendix_j, preliminary_category)) %>%
  select(-preliminary_category, -final_category_appendix_j) %>%
  arrange(respondent_id) %>%
  distinct(respondent_id, .keep_all = TRUE)
</file>