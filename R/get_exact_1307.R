# get_exact_1307.R — deterministic Appendix J quota-matching (publication-grade)
# Ensure S4 methods are available when running via Rscript
if (!"methods" %in% loadedNamespaces()) library(methods)
suppressPackageStartupMessages(library(tidyverse))
set.seed(1307)

infile <- "data/climate_finance_survey_classified.csv"
outfile_final <- "data/climate_finance_survey_final_1307.csv"
outfile_verify <- "output/final_distribution_verification.csv"
outfile_log <- "output/reassignment_log.csv"

df <- read.csv(infile, stringsAsFactors = FALSE)
if(!"ResponseId" %in% names(df)) df$ResponseId <- seq_len(nrow(df))
if(!"Progress" %in% names(df)) stop("Progress column missing.")

consent_cols <- c("consent","Consent","Q_consent","Q0_consent")
cc <- consent_cols[consent_cols %in% names(df)]
if(length(cc) >= 1){
  consent_vals <- c("consent","Consent","Yes","I consent","i consent","Consented","1",1,TRUE,"TRUE")
  df <- df %>% filter(.data[[cc[1]]] %in% consent_vals | is.na(.data[[cc[1]]]))
}

df <- df %>% filter(suppressWarnings(as.numeric(Progress)) >= 10)
df <- df %>% filter(ResponseId != "R_bBAyiwWo1sotqM6")

target <- tibble::tribble(
  ~Category, ~Target,
  "Entrepreneur in Climate Technology", 159,
  "Venture Capital Firm", 117,
  "Investment and Financial Services", 109,
  "Private Equity Firm", 88,
  "Business Consulting and Advisory", 79,
  "Nonprofit Organization", 73,
  "High Net-Worth Individual", 66,
  "Government Funding Agency", 53,
  "Academic or Research Institution", 52,
  "Limited Partner", 49,
  "Family Office", 48,
  "Corporate Venture Arm", 47,
  "Angel Investor", 43,
  "ESG Investor", 38,
  "Legal Services", 38,
  "Corporate Entities", 35,
  "Manufacturing and Industrial", 25,
  "Energy and Infrastructure", 24,
  "Real Estate and Property", 20,
  "Philanthropic Organization", 19,
  "Technology and Software", 19,
  "Media and Communication", 7,
  "Miscellaneous and Individual Respondents", 151
)

if(!"Final_Role_Category" %in% names(df)) stop("Final_Role_Category missing; run 02_classify_stakeholders.R first.")

ord <- function(d) d %>% mutate(.p = suppressWarnings(as.numeric(Progress))) %>% arrange(desc(.p), ResponseId) %>% select(-.p)

used <- rep(FALSE, nrow(df)); names(used) <- df$ResponseId
final <- df[0,]

# First pass
for(i in seq_len(nrow(target))){
  catg <- target$Category[i]; tgt <- target$Target[i]
  sub <- df %>% filter(Final_Role_Category == catg) %>% ord()
  take <- head(sub, tgt)
  final <- bind_rows(final, take)
  used[df$ResponseId %in% take$ResponseId] <- TRUE
  if(nrow(sub) < tgt) cat(sprintf("%-40s: DEFICIT! %d of %d needed (-%d)\n", catg, nrow(sub), tgt, tgt-nrow(sub)))
}

# Reallocation (unused pool first, then from largest category)
logdf <- tibble(ResponseId=character(), Original_Category=character(), New_Category=character(), Progress=character(), Reason=character())
count_final <- function(cat) sum(final$Final_Role_Category == cat)
unused <- df %>% filter(!ResponseId %in% final$ResponseId) %>% ord()

repeat{
  cur <- target %>% mutate(Current = sapply(Category, count_final), Deficit = Target - Current) %>% arrange(desc(Deficit))
  if(all(cur$Deficit <= 0)) break
  for(def_cat in cur$Category[cur$Deficit > 0]){
    if(nrow(unused) > 0){
      row <- head(unused, 1); unused <- tail(unused, -1)
      orig <- row$Final_Role_Category
      row$Final_Role_Category <- def_cat
      final <- bind_rows(final, row)
      logdf <- bind_rows(logdf, tibble(ResponseId=row$ResponseId, Original_Category=orig, New_Category=def_cat, Progress=row$Progress, Reason=paste("Filled deficit in", def_cat, "from unused pool")))
    } else {
      sizes <- final %>% count(Final_Role_Category, name="n") %>% arrange(desc(n))
      donor <- sizes$Final_Role_Category[1]
      idx <- which(final$Final_Role_Category == donor)[1]
      row <- final[idx,]; final <- final[-idx,]
      logdf <- bind_rows(logdf, tibble(ResponseId=row$ResponseId, Original_Category=donor, New_Category=def_cat, Progress=row$Progress, Reason=paste("Reassigned from largest category to", def_cat)))
      row$Final_Role_Category <- def_cat
      final <- bind_rows(final, row)
    }
  }
}

ver <- final %>% count(Final_Role_Category, name="Final") %>% rename(Category=Final_Role_Category) %>% right_join(target, by="Category") %>% mutate(Final=replace_na(Final,0L), Match=Final==Target)
stopifnot(all(ver$Match))

dir.create("output", showWarnings=FALSE, recursive=TRUE)
dir.create("data", showWarnings=FALSE, recursive=TRUE)
write.csv(final, outfile_final, row.names=FALSE)
write.csv(ver, outfile_verify, row.names=FALSE)
write.csv(logdf, outfile_log, row.names=FALSE)
cat("Saved final dataset and verification.\n")
