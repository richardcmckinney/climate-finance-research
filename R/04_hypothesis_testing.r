# R/04_hypothesis_testing.R — Manuscript tests (H1–H12), EFA/CFA, regressions
# Deterministic, fail-safe; writes outputs to output/ and figures/ if available.

suppressPackageStartupMessages({
  library(tidyverse)
  library(psych)     # EFA, KMO
  library(lavaan)    # CFA
  library(MASS)      # rlm fallback
  library(ggplot2)
})

set.seed(1307)
Sys.setenv(TZ = "UTC")

dir.create("output",  showWarnings = FALSE, recursive = TRUE)
dir.create("figures", showWarnings = FALSE, recursive = TRUE)

# ---------- Load data (prefer 1307 subset when present) ----------
paths <- c(
  "data/climate_finance_survey_final_1307.csv",
  "data/survey_responses_anonymized_preliminary.csv",
  "data/survey_responses_anonymized_basic.csv"
)
path <- paths[file.exists(paths)][1]
if (is.na(path)) stop("No input dataset found. Expected one of: ", paste(paths, collapse = ", "))

df <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)

# Role column harmonization
role_candidates <- c("Final_Role_Category","final_category_appendix_j")
role_col <- role_candidates[role_candidates %in% names(df)][1]
if (is.na(role_col) || !nzchar(role_col)) stop("Role category column not found in data.")

# ---------- Heuristics to identify analysis items ----------
# Numeric Likert-ish items: names suggest barrier/solution and values in small integer ranges
is_likertish <- function(x) {
  if (!is.numeric(x)) return(FALSE)
  ux <- unique(na.omit(x))
  length(ux) >= 4 && length(ux) <= 11 && min(ux, na.rm=TRUE) >= 0 && max(ux, na.rm=TRUE) <= 10
}
name_is_barrierish <- function(nm) {
  grepl("barrier|risk|challenge|obstacle", nm, ignore.case = TRUE)
}
name_is_solutionish <- function(nm) {
  grepl("solution|enabler|policy|support", nm, ignore.case = TRUE)
}

num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
likertish <- num_cols[vapply(df[num_cols], is_likertish, logical(1))]
barrier_vars  <- intersect(likertish, names(df)[name_is_barrierish(names(df))])
solution_vars <- intersect(likertish, names(df)[name_is_solutionish(names(df))])

# If the heuristics miss everything, fall back to any numeric Likertish vars
if (length(barrier_vars) == 0 && length(likertish) > 0) barrier_vars <- likertish
if (length(solution_vars) == 0 && length(likertish) > 0) solution_vars <- setdiff(likertish, barrier_vars)

# ---------- 1) Descriptives by role ----------
desc_long <- bind_rows(
  lapply(barrier_vars, function(v) {
    df %>%
      group_by(.data[[role_col]]) %>%
      summarise(
        variable = v,
        n = sum(!is.na(.data[[v]])),
        mean = mean(.data[[v]], na.rm = TRUE),
        sd   = sd(.data[[v]],   na.rm = TRUE)
      ) %>% ungroup() %>% mutate(type = "barrier")
  }),
  lapply(solution_vars, function(v) {
    df %>%
      group_by(.data[[role_col]]) %>%
      summarise(
        variable = v,
        n = sum(!is.na(.data[[v]])),
        mean = mean(.data[[v]], na.rm = TRUE),
        sd   = sd(.data[[v]],   na.rm = TRUE)
      ) %>% ungroup() %>% mutate(type = "solution")
  })
) %>% bind_rows()

write.csv(desc_long, "output/descriptives_by_role.csv", row.names = FALSE)

# ---------- 2) ANOVA across roles (barrier items) ----------
anova_rows <- list()
for (v in barrier_vars) {
  dat <- df %>% select(all_of(role_col), all_of(v)) %>% drop_na()
  if (n_distinct(dat[[role_col]]) < 2 || nrow(dat) < 20) next
  # Welch ANOVA as robust default for unequal variances
  f <- as.formula(paste(v, "~", role_col))
  aov_fit <- oneway.test(f, data = dat, var.equal = FALSE)
  anova_rows[[v]] <- data.frame(
    variable = v,
    method   = "Welch ANOVA",
    statistic= unname(aov_fit$statistic),
    df1      = unname(aov_fit$parameter[1]),
    df2      = unname(aov_fit$parameter[2]),
    p_value  = unname(aov_fit$p.value),
    stringsAsFactors = FALSE
  )
}
anova_tbl <- bind_rows(anova_rows)
write.csv(anova_tbl, "output/anova_by_role.csv", row.names = FALSE)

# ---------- 3) Chi-square tests for categorical fields (if any) ----------
# Attempt with a couple of common categorical candidates
cat_candidates <- names(df)[vapply(df, function(x) is.character(x) || is.factor(x), logical(1))]
cat_candidates <- setdiff(cat_candidates, role_candidates)
chi_rows <- list()
for (cvar in cat_candidates) {
  tbl <- table(df[[role_col]], df[[cvar]], useNA = "no")
  if (all(dim(tbl) >= 2) && sum(tbl) >= 50) {
    ct <- suppressWarnings(chisq.test(tbl))
    chi_rows[[cvar]] <- data.frame(
      variable = cvar,
      method   = "Chi-square",
      statistic= unname(ct$statistic),
      df       = unname(ct$parameter),
      p_value  = unname(ct$p.value),
      stringsAsFactors = FALSE
    )
  }
}
chi_tbl <- bind_rows(chi_rows)
write.csv(chi_tbl, "output/chi_square_by_role.csv", row.names = FALSE)

# ---------- 4) Correlations among barrier variables ----------
if (length(barrier_vars) >= 2) {
  cm <- df %>% select(all_of(barrier_vars)) %>% mutate(across(everything(), as.numeric))
  cm <- cm[, colSums(!is.na(cm)) > 0, drop = FALSE]
  cmat <- suppressWarnings(cor(cm, use = "pairwise.complete.obs"))
  write.csv(cmat, "output/correlation_barriers.csv")
  # simple heatmap
  library(reshape2)
  cdf <- melt(cmat, varnames = c("x","y"), value.name = "r")
  p <- ggplot(cdf, aes(x, y, fill = r)) + geom_tile() + scale_x_discrete(guide = guide_axis(angle = 90)) +
    labs(title = "Barrier variables — correlation heatmap", x = NULL, y = NULL) + theme_bw()
  ggsave("figures/corr_heatmap_barriers.png", p, width = 10, height = 8, dpi = 150)
}

# ---------- 5) EFA (if viable) ----------
efa_ok <- length(barrier_vars) >= 5
if (efa_ok) {
  X <- df %>% select(all_of(barrier_vars)) %>% mutate(across(everything(), as.numeric))
  X <- X[, colSums(!is.na(X)) > 0, drop = FALSE]
  X <- X[complete.cases(X), , drop = FALSE]
  if (nrow(X) >= 200 && ncol(X) >= 5) {
    # Kaiser rule for factors as a starting point; cap to 6
    ev <- eigen(cor(X))$values
    nf <- min(max(sum(ev > 1), 1), 6)
    efa_fit <- try(psych::fa(X, nfactors = nf, rotate = "oblimin", fm = "ml"), silent = TRUE)
    if (!inherits(efa_fit, "try-error")) {
      loadings <- as.data.frame(unclass(efa_fit$loadings))
      write.csv(loadings, "output/efa_loadings.csv")
    }
  }
}

# ---------- 6) CFA (conservative template, if viable) ----------
# Try to group items by simple name heuristics into three latent clusters
if (length(barrier_vars) >= 6) {
  g1 <- barrier_vars[grepl("tech|science|rd|prototype|manufactur", barrier_vars, TRUE)]
  g2 <- barrier_vars[grepl("market|demand|customer|revenue|unit", barrier_vars, TRUE)]
  g3 <- barrier_vars[grepl("policy|reg|permit|grid|infra",        barrier_vars, TRUE)]
  # ensure at least 2 items per group
  groups <- list(g1 = g1, g2 = g2, g3 = g3)
  groups <- lapply(groups, function(v) v[seq_len(min(length(v), 6))])
  ok <- all(vapply(groups, function(v) length(v) >= 2, logical(1)))
  if (ok) {
    model <- paste0(
      "G1 =~ ", paste(groups$g1, collapse = " + "), "\n",
      "G2 =~ ", paste(groups$g2, collapse = " + "), "\n",
      "G3 =~ ", paste(groups$g3, collapse = " + "), "\n"
    )
    Y <- df %>% select(all_of(unique(unlist(groups)))) %>% mutate(across(everything(), as.numeric))
    Y <- Y[complete.cases(Y), , drop = FALSE]
    if (nrow(Y) >= 300) {
      fit <- try(lavaan::cfa(model, data = Y, std.lv = TRUE, estimator = "MLR"), silent = TRUE)
      if (!inherits(fit, "try-error")) {
        sink("output/cfa_summary.txt"); print(summary(fit, standardized = TRUE, fit.measures = TRUE)); sink()
      }
    }
  }
}

# ---------- 7) Regressions (barrier index ~ role) ----------
if (length(barrier_vars) >= 3) {
  df$barrier_index <- rowMeans(df[barrier_vars], na.rm = TRUE)
  base <- df %>% select(all_of(role_col), barrier_index) %>% drop_na()
  if (nrow(base) >= 200 && sd(base$barrier_index) > 0) {
    base[[role_col]] <- factor(base[[role_col]])
    lm_fit  <- lm(barrier_index ~ .data[[role_col]], data = base)
    capture.output(summary(lm_fit), file = "output/regression_barrierindex_by_role.txt")
    # robust fallback
    rlm_fit <- MASS::rlm(barrier_index ~ .data[[role_col]], data = base, maxit = 100)
    capture.output(summary(rlm_fit), file = "output/regression_barrierindex_by_role_robust.txt")
  }
}

# ---------- Done ----------
message("04_hypothesis_testing.R completed.")