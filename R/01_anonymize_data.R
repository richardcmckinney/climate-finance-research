verify_outputs <- function(outputs) {
  # ... existing code ...

  # After dictionary equality check success line
  cli::cli_alert_success("Dictionary columns exactly match anonymized_basic (names and order).")

  # Duplicate check in basic dataset
  if ("respondent_id" %in% names(basic)) {
    tab_basic <- table(basic$respondent_id)
    dup_basic <- sum(tab_basic[tab_basic > 1] - 1)
    if (dup_basic > 0) {
      stop(sprintf("%d duplicate respondent_id(s) found in anonymized_basic. Example IDs: %s",
                   dup_basic, paste(head(names(tab_basic[tab_basic > 1]), 10), collapse = ", ")))
    } else {
      cli::cli_alert_success("No duplicate respondent_id in anonymized_basic.")
    }
  }

  # ... existing code ...

  if (file.exists(prelim_path)) {
    prelim <- readr::read_csv(prelim_path, show_col_types = FALSE)

    # Duplicate check in preliminary dataset
    if ("respondent_id" %in% names(prelim)) {
      tab_pre <- table(prelim$respondent_id)
      dup_pre <- sum(tab_pre[tab_pre > 1] - 1)
      if (dup_pre > 0) {
        stop(sprintf("%d duplicate respondent_id(s) found in preliminary dataset (extra rows beyond first). Example IDs: %s",
                     dup_pre, paste(head(names(tab_pre[tab_pre > 1]), 10), collapse = ", ")))
      } else {
        cli::cli_alert_success("No duplicate respondent_id in preliminary dataset.")
      }
    }

    # ... existing code ...
  }

  # ... existing code ...
}