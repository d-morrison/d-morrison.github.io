# Douglas Ezra Morrison's Personal Website

This is the source repository for my personal academic website, built with [Quarto](https://quarto.org/).

## Building the Site

To build the website:

```bash
quarto render
```

After rendering, you need to copy additional resources:

```bash
# Copy .nojekyll for GitHub Pages
cp .nojekyll docs/

# Copy CV file
mkdir -p docs/files/CV
cp static/files/CV/Morrison_CV.pdf docs/files/CV/
```

Or use the provided script:

```bash
./build.sh
```

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
