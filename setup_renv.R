#!/usr/bin/env Rscript
# Minimal renv setup - no special characters

# Install renv
if (!require("renv", quietly = TRUE)) {
  install.packages("renv")
}

# Initialize
library(renv)
renv::init()

# Install core packages
pkgs <- c(
  "tidyverse",
  "cli",
  "digest",
  "lubridate",
  "psych",
  "lavaan",
  "MASS",
  "nnet",
  "car",
  "effectsize",
  "corrplot",
  "here",
  "janitor",
  "glue",
  "readxl",
  "writexl"
)

for (p in pkgs) {
  if (!require(p, character.only = TRUE, quietly = TRUE)) {
    install.packages(p)
  }
}

# Create snapshot
renv::snapshot()

print("Done! Check for renv.lock file")