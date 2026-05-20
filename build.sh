#!/bin/bash
# Build script for the Quarto website

set -e

# Extract evaluation data from PDFs before rendering
if ! command -v Rscript &> /dev/null; then
  echo "Rscript is required to build this site because Quarto renders R code in evals.qmd." >&2
  exit 1
fi

if ! command -v pdftotext &> /dev/null; then
  echo "pdftotext is required to extract evaluation data from PDFs." >&2
  echo "Install it with: sudo apt-get install -y poppler-utils (Debian/Ubuntu)" >&2
  echo "                 brew install poppler (macOS)" >&2
  exit 1
fi

echo "Extracting evaluation data from PDFs..."
Rscript data/build_evals_data.R

echo "Rendering Quarto website..."
quarto render

echo "Copying additional resources..."
# Copy .nojekyll for GitHub Pages
cp .nojekyll docs/

# Copy CV file
mkdir -p docs/files/CV
cp static/files/CV/Morrison_CV.pdf docs/files/CV/

# Copy eval files
mkdir -p docs/files/evals
cp static/files/evals/*.pdf docs/files/evals/

echo "Build complete! Website is ready in docs/"
