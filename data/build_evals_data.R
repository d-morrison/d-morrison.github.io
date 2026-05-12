#!/usr/bin/env Rscript

# This script extracts evaluation data from PDFs and saves it to an RDS file
# Run this script whenever the evaluation PDFs are updated

source("data/extract_evals.R")

# Extract the data
evals_data <- extract_evals_data()

# Save to RDS file
saveRDS(evals_data, "data/evals_data.rds")

cat("Evaluation data extracted and saved to data/evals_data.rds\n")
cat("Rows:", nrow(evals_data), "\n")
print(evals_data)
