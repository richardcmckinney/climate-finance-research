# 02_classify_stakeholders.R
# Purpose: Classify stakeholders per Appendix J using Q2.1 and Q2.1_12_TEXT
# Input : data/climate_finance_survey_anonymized.csv
# Output: data/climate_finance_survey_classified.csv
#         output/stakeholder_classification_verification.csv

# ---- Libraries ---------------------------------------------------------------
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))

# ---- Folders -----------------------------------------------------------------
dir.create("data",   recursive = TRUE, showWarnings = FALSE)
dir.create("output", recursive = TRUE, showWarnings = FALSE)

# ---- IO paths ----------------------------------------------------------------
infile  <- "data/climate_finance_survey_anonymized.csv"
outfile <- "data/climate_finance_survey_classified.csv"
verifyf <- "output/stakeholder_classification_verification.csv"

if (!file.exists(infile)) stop("Anonymized input not found: ", infile)
df <- read.csv(infile, stringsAsFactors = FALSE)
if (!"ResponseId" %in% names(df)) df$ResponseId <- seq_len(nrow(df))
if (!"Q2.1" %in% names(df))         df[["Q2.1"]]         <- NA_character_
if (!"Q2.1_12_TEXT" %in% names(df)) df[["Q2.1_12_TEXT"]] <- NA_character_

cat("Starting stakeholder classification...\n")
cat("Total responses: ", nrow(df), "\n\n", sep = "")

# ---- Helpers -----------------------------------------------------------------
to_lc <- function(x) ifelse(is.na(x) | x == "", NA_character_, stringr::str_to_lower(x))
rx_any <- function(...) stringr::regex(paste0("(", paste0(..., collapse = "|"), ")"),
                                       ignore_case = TRUE)

# ---- Mapping rules (predefined Q2.1) -----------------------------------------
map_predefined_role_vec <- function(role_value) {
  rl <- to_lc(role_value)
  case_when(
    str_detect(rl, rx_any("entrepreneur","founder","co[- ]?founder")) &
      str_detect(rl, rx_any("climate","clean","green","sustain","energy","carbon","emission",
                            "renewable","solar","wind","electric","battery","hydrogen","bio")) ~
      "Entrepreneur in Climate Technology",
    str_detect(rl, rx_any("\\bventure capital\\b")) & !str_detect(rl, "corporate") ~
      "Venture Capital Firm",
    str_detect(rl, "private equity")                                                   ~ "Private Equity Firm",
    str_detect(rl, rx_any("high net","hnwi"))                                          ~ "High Net-Worth Individual",
    str_detect(rl, rx_any("nonprofit","non-profit")) & !str_detect(rl, "philanthrop") ~ "Nonprofit Organization",
    str_detect(rl, rx_any("academic","research institution","university"))            ~ "Academic or Research Institution",
    str_detect(rl, rx_any("limited partner","\\blp\\b"))                               ~ "Limited Partner",
    str_detect(rl, "family office")                                                    ~ "Family Office",
    str_detect(rl, "corporate venture")                                                ~ "Corporate Venture Arm",
    str_detect(rl, "angel investor")                                                   ~ "Angel Investor",
    str_detect(rl, "esg investor")                                                     ~ "ESG Investor",
    str_detect(rl, rx_any("government funding","government agency"))                   ~ "Government Funding Agency",
    str_detect(rl, "philanthrop")                                                      ~ "Philanthropic Organization",
    str_detect(rl, "\\bother\\b")                                                      ~ NA_character_,  # defer to text
    TRUE                                                                               ~ NA_character_
  )
}

# ---- Mapping rules (free text in Q2.1_12_TEXT) --------------------------------
categorize_free_text_vec <- function(text) {
  tx <- to_lc(text)
  case_when(
    str_detect(tx, rx_any("founder","co[- ]?founder","\\bceo\\b.*startup","\\bcto\\b.*startup",
                          "started.*company","launching.*venture","entrepreneur")) &
    str_detect(tx, rx_any("climate","clean","green","sustain","energy","carbon","emission",
                          "renewable","solar","wind","electric","battery","hydrogen","bio")) ~
      "Entrepreneur in Climate Technology",

    str_detect(tx, rx_any("investment bank","asset manager","fund manager","sovereign wealth",
                          "insurance company","private bank","debt fund","fintech","hedge fund",
                          "venture studio","commercial finance","private debt","impact investor",
                          "financial research","project finance","asset owner","pension fund",
                          "esg ratings","wealth management","green bond","blended finance")) ~
      "Investment and Financial Services",

    str_detect(tx, rx_any("law firm","lawyer","attorney","solicitor","legal advisor","legal counsel",
                          "patent attorney","barrister")) ~
      "Legal Services",

    str_detect(tx, rx_any("consult","advisor","advisory","strategist","m&a","management consulting",
                          "sustainability consultant","esg advisory")) &
      !str_detect(tx, rx_any("\\blaw\\b","legal","attorney")) ~
      "Business Consulting and Advisory",

    str_detect(tx, rx_any("corporate","conglomerate","multinational","holding company","plc",
                          "gmbh","\\bag\\b","s\\.a\\.","incorporated","corporation","\\bltd\\b")) &
      !str_detect(tx, rx_any("venture","consult","\\blaw\\b")) ~
      "Corporate Entities",

    str_detect(tx, rx_any("manufactur","industrial","factory","production","assembly","processing",
                          "engineering firm")) &
      !str_detect(tx, rx_any("consult","software")) ~
      "Manufacturing and Industrial",

    str_detect(tx, rx_any("renewable energy","power producer","energy services","utility","grid",
                          "infrastructure","solar developer","wind developer","hydro","geothermal",
                          "biomass","waste[- ]?to[- ]?energy")) ~
      "Energy and Infrastructure",

    str_detect(tx, rx_any("real estate","property","reit","building","housing","commercial property")) ~
      "Real Estate and Property",

    str_detect(tx, rx_any("software","saas","platform","tech company","technology","digital","data",
                          "\\bai\\b","artificial intelligence","blockchain","\\bapp\\b","application")) &
      !str_detect(tx, rx_any("consult","\\blaw\\b")) ~
      "Technology and Software",

    str_detect(tx, rx_any("media","communications","press","journalism","publication","broadcasting")) ~
      "Media and Communication",

    str_detect(tx, rx_any("foundation","philanthropic","charitable trust","donor","grantmaker","giving")) ~
      "Philanthropic Organization",

    str_detect(tx, rx_any("ngo","nonprofit","non-profit","charity","association","institute",
                          "advocacy","civil society")) ~
      "Nonprofit Organization",

    str_detect(tx, rx_any("government","public sector","ministry","department","agency","\\bdfi\\b",
                          "development finance","multilateral","bilateral","municipality","federal",
                          "state agency")) ~
      "Government Funding Agency",

    TRUE ~ "Miscellaneous and Individual Respondents"
  )
}

# ---- Vectorized classification ------------------------------------------------
df <- df %>%
  mutate(
    q2       = `Q2.1`,
    q2_text  = `Q2.1_12_TEXT`,
    from_q2  = map_predefined_role_vec(q2),
    from_txt = ifelse(is.na(from_q2), categorize_free_text_vec(q2_text), NA_character_),
    Final_Role_Category = coalesce(from_q2, from_txt,
                                   "Miscellaneous and Individual Respondents")
  ) %>%
  select(-from_q2, -from_txt)

# ---- Summary + Appendix-J comparison -----------------------------------------
cat("\n=== CLASSIFICATION RESULTS ===\n")
role_summary <- sort(table(df$Final_Role_Category, useNA = "ifany"), decreasing = TRUE)
summary_df <- data.frame(
  Category   = names(role_summary),
  Count      = as.numeric(role_summary),
  Percentage = round(as.numeric(role_summary) / sum(role_summary) * 100, 1)
)
print(summary_df)

expected <- data.frame(
  Category = c("Entrepreneur in Climate Technology","Venture Capital Firm",
               "Investment and Financial Services","Private Equity Firm",
               "Business Consulting and Advisory","Nonprofit Organization",
               "High Net-Worth Individual","Government Funding Agency",
               "Academic or Research Institution","Limited Partner","Family Office",
               "Corporate Venture Arm","Angel Investor","ESG Investor","Legal Services",
               "Corporate Entities","Manufacturing and Industrial","Energy and Infrastructure",
               "Real Estate and Property","Philanthropic Organization","Technology and Software",
               "Media and Communication","Miscellaneous and Individual Respondents"),
  Expected = c(159,117,109,88,79,73,66,53,52,49,48,47,43,38,38,35,25,24,20,19,19,7,151)
)

comparison <- merge(expected, summary_df, by = "Category", all = TRUE)
comparison[is.na(comparison)] <- 0
comparison$Difference <- comparison$Count - comparison$Expected

cat("\n=== COMPARISON WITH EXPECTED (APPENDIX J) ===\n")
print(comparison[order(-comparison$Count), ])

# ---- Persist outputs ----------------------------------------------------------
write.csv(df, outfile, row.names = FALSE)
cat("\n=== FILE SAVED ===\nClassified data → ", outfile, "\n", sep = "")

verification_df <- df[, c("ResponseId","Q2.1","Q2.1_12_TEXT","Final_Role_Category")]
write.csv(verification_df, verifyf, row.names = FALSE)
cat("Verification file → ", verifyf, "\n", sep = "")

# ---- Deterministic sample of 'Other' mappings --------------------------------
set.seed(1307)
cat("\n=== SAMPLE CLASSIFICATIONS (from 'Other' free text) ===\n")
other_responses <- df[!is.na(df$`Q2.1_12_TEXT`) & df$`Q2.1_12_TEXT` != "",
                      c("Q2.1_12_TEXT","Final_Role_Category")]
if (nrow(other_responses) > 0) {
  sample_idx <- head(sample(seq_len(nrow(other_responses))), 10)
  print(other_responses[sample_idx, ])
}

cat("\n✓ Stakeholder classification completed successfully\n")
cat("Total classified responses: ", sum(!is.na(df$Final_Role_Category)), "\n", sep = "")
cat("Unclassified responses: ", sum(is.na(df$Final_Role_Category)), "\n", sep = "")