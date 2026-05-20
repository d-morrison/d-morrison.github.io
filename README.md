# Douglas Ezra Morrison's Personal Website

This is the source repository for my personal academic website, built with [Quarto](https://quarto.org/).

## Building the Site

To build the website, you'll need to install the following prerequisites:

### Prerequisites

1. **Quarto** - Install from [quarto.org](https://quarto.org/docs/get-started/)
2. **R** - Install from [r-project.org](https://www.r-project.org/)
3. **poppler** (for PDF text extraction) - Install using your system's package manager:
   - **Debian/Ubuntu**: `sudo apt-get install -y poppler-utils`
   - **macOS**: `brew install poppler`
   - **Windows**: Download from [poppler for Windows](https://blog.alivate.com.au/poppler-windows/)

### Build Command

Once prerequisites are installed, run:

```bash
./build.sh
```

The build script will:
1. Extract evaluation data from PDFs (creates `data/evals_data.rds`)
2. Render the Quarto site
3. Copy additional resources to the `docs/` directory

## Development

The site is structured with the following main files:

- `index.qmd` - Homepage with profile and bio
- `publications.qmd` - Publications list
- `projects.qmd` - Research projects and R packages
- `blog.qmd` - Blog posts (placeholder)
- `_quarto.yml` - Quarto configuration

## Deployment

The site is deployed to GitHub Pages from the `docs/` directory.

## Previous Version

This site was previously built with Hugo Academic theme and blogdown. The old files are preserved in the repository for reference:

- `content/` - Old Hugo content
- `themes/` - Hugo themes
- `config.toml` - Hugo configuration
- `index.Rmd.old` - Old blogdown index file
