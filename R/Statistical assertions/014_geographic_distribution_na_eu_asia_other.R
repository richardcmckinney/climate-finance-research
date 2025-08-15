# ===============================
# File: 014_geographic_distribution_na_eu_asia_other.R
# Purpose: Geographic distribution (Q2.2)
# ===============================
data <- read.csv("survey_responses_anonymized_preliminary.csv", stringsAsFactors = FALSE)
analytic <- subset(data, Status == "IP Address" & as.numeric(Progress) >= 10)
geo_tab <- prop.table(table(analytic$Q2.2)) * 100
print(round(geo_tab, 1))
