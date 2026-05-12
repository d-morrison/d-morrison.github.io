#!/bin/bash
# Build script for the Quarto website

set -e

# Extract evaluation data from PDFs if Rscript is available
if command -v Rscript &> /dev/null; then
  echo "Extracting evaluation data from PDFs..."
  Rscript data/build_evals_data.R
else
  echo "Rscript not found, skipping pre-extraction (will extract during render if needed)"
fi

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
