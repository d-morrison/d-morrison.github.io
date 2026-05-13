#!/usr/bin/env Rscript
# data/generate_software_page.R
#
# Reads data/software_repos.json and writes software.qmd.
# Run with --refresh to fetch current PR counts from the GitHub API
# (requires gh CLI to be installed and authenticated).
#
# Usage:
#   Rscript data/generate_software_page.R            # Use cached counts
#   Rscript data/generate_software_page.R --refresh  # Fetch live counts

suppressPackageStartupMessages(library(jsonlite))

args      <- commandArgs(trailingOnly = TRUE)
do_refresh <- "--refresh" %in% args

data_file   <- "data/software_repos.json"
output_file <- "software.qmd"

# ---- Helpers ----

`%||%` <- function(a, b) if (!is.null(a) && !identical(a, "null")) a else b

# Build the GitHub PR search URL for a repo + state
pr_search_url <- function(repo, state, user) {
  sprintf(
    "https://github.com/%s/pulls?q=is%%3Apr+is%%3A%s+involves%%3A%s",
    repo, state, user
  )
}

# Return a markdown cell: linked number when count > 0, plain "0" otherwise
pr_cell <- function(count, repo, state, user) {
  count <- count %||% 0L
  if (isTRUE(count == 0)) return("0")
  sprintf("[%d](%s)", as.integer(count), pr_search_url(repo, state, user))
}

# Given a github "owner/repo" string, return the display name:
# just the repo name for personal repos, "org/repo" for org repos
display_name <- function(github_slug, personal = "d-morrison") {
  parts <- strsplit(github_slug, "/", fixed = TRUE)[[1]]
  if (parts[[1]] == personal) parts[[2]] else github_slug
}

# Assemble a pipe-table row
pipe_row <- function(...) {
  cells <- c(...)
  paste0("| ", paste(cells, collapse = " | "), " |")
}

# ---- Table generators ----

make_cran_table <- function(pkgs, user) {
  header <- pipe_row("Package", "Status", "Description", "CRAN",
                     "GitHub", "Docs", "Publication", "Merged PRs", "Open PRs")
  sep    <- "|---------|--------|-------------|------|--------|------|-------------|:----------:|:--------:|"
  rows   <- vapply(pkgs, function(p) {
    cran_cell <- if (!is.null(p$cran_url) && p$cran_url != "null")
      sprintf("[%s](%s)", p$cran_label %||% "CRAN", p$cran_url) else ""
    gh_cell   <- sprintf("[GitHub](https://github.com/%s)", p$github)
    docs_cell <- if (!is.null(p$docs_url) && p$docs_url != "null")
      sprintf("[Docs](%s)", p$docs_url) else ""
    pub_cell  <- if (!is.null(p$publication_url) && p$publication_url != "null")
      sprintf("[%s](%s)", p$publication_label %||% "DOI", p$publication_url) else ""
    pipe_row(
      p$name, p$status, p$description,
      cran_cell, gh_cell, docs_cell, pub_cell,
      pr_cell(p$merged_prs, p$github, "merged", user),
      pr_cell(p$open_prs,   p$github, "open",   user)
    )
  }, character(1))
  paste(c(header, sep, rows), collapse = "\n")
}

make_repo_table <- function(repos, col1, user) {
  header <- pipe_row(col1, "Description", "GitHub", "Merged PRs", "Open PRs")
  sep    <- "|---------|-------------|--------|:----------:|:--------:|"
  rows   <- vapply(repos, function(r) {
    gh_cell <- sprintf("[GitHub](https://github.com/%s)", r$github)
    pipe_row(
      display_name(r$github), r$description, gh_cell,
      pr_cell(r$merged_prs, r$github, "merged", user),
      pr_cell(r$open_prs,   r$github, "open",   user)
    )
  }, character(1))
  paste(c(header, sep, rows), collapse = "\n")
}

make_external_table <- function(repos, user) {
  header <- pipe_row("Repository", "Description", "Merged PRs", "Open PRs")
  sep    <- "|------------|-------------|:----------:|:--------:|"
  rows   <- vapply(repos, function(r) {
    repo_link <- sprintf("[%s](https://github.com/%s)", r$github, r$github)
    pipe_row(
      repo_link, r$description,
      pr_cell(r$merged_prs, r$github, "merged", user),
      pr_cell(r$open_prs,   r$github, "open",   user)
    )
  }, character(1))
  paste(c(header, sep, rows), collapse = "\n")
}

# ---- GitHub API (via gh CLI) ----

fetch_count <- function(repo, state, user) {
  query <- sprintf("repo:%s is:pr is:%s involves:%s", repo, state, user)
  url   <- sprintf(
    "search/issues?q=%s&per_page=1",
    utils::URLencode(query, reserved = TRUE)
  )
  out <- tryCatch(
    system2("gh", c("api", url, "--jq", ".total_count"),
            stdout = TRUE, stderr = FALSE),
    error = function(e) NA_character_
  )
  n <- suppressWarnings(as.integer(out[[1]]))
  if (is.na(n)) 0L else as.integer(n)
}

refresh_list <- function(lst, user) {
  lapply(lst, function(r) {
    cat(sprintf("  %s ...\n", r$github))
    r$merged_prs <- fetch_count(r$github, "merged", user)
    Sys.sleep(2)   # GitHub Search API: 30 req/min for auth users = 1 req/2 s
    r$open_prs   <- fetch_count(r$github, "open",   user)
    Sys.sleep(2)
    r
  })
}

# ---- Load data ----

data <- fromJSON(data_file, simplifyVector = FALSE)
user <- data$search_user %||% "d-morrison"

# ---- Optionally refresh counts ----

if (do_refresh) {
  cat("Fetching PR counts from GitHub API (this may take a few minutes)...\n")
  cat("CRAN packages:\n")
  data$cran_packages                       <- refresh_list(data$cran_packages, user)
  cat("My repos - R Packages:\n")
  data$my_repos$r_packages                 <- refresh_list(data$my_repos$r_packages, user)
  cat("My repos - Quarto Extensions:\n")
  data$my_repos$quarto_extensions          <- refresh_list(data$my_repos$quarto_extensions, user)
  cat("My repos - Quarto Books & Templates:\n")
  data$my_repos$quarto_books_templates     <- refresh_list(data$my_repos$quarto_books_templates, user)
  cat("My repos - Shiny Applications:\n")
  data$my_repos$shiny_applications         <- refresh_list(data$my_repos$shiny_applications, user)
  cat("My repos - Analysis & Research Code:\n")
  data$my_repos$analysis_research_code     <- refresh_list(data$my_repos$analysis_research_code, user)
  cat("External contributions:\n")
  data$external_contributions              <- refresh_list(data$external_contributions, user)

  data$last_updated <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  writeLines(toJSON(data, pretty = TRUE, auto_unbox = TRUE), data_file)
  cat(sprintf("Saved %s\n", data_file))
}

# ---- Generate software.qmd ----

lines <- c(
  "---",
  'title: "Software"',
  "---",
  "",
  "<!-- This file is auto-generated by data/generate_software_page.R -->",
  sprintf("<!-- Last updated: %s -->", data$last_updated %||% "unknown"),
  "",
  "## R Packages on CRAN",
  "",
  make_cran_table(data$cran_packages, user),
  "",
  "---",
  "",
  "## My Repositories",
  "",
  paste0(
    "Repositories under my personal GitHub account, the ",
    "[UCD-SERG](https://github.com/UCD-SERG) organization, and other organizations ",
    "where I am an author or maintainer."
  ),
  "",
  "### R Packages",
  "",
  make_repo_table(data$my_repos$r_packages, "Package", user),
  "",
  "### Quarto Extensions",
  "",
  make_repo_table(data$my_repos$quarto_extensions, "Extension", user),
  "",
  "### Quarto Books & Templates",
  "",
  make_repo_table(data$my_repos$quarto_books_templates, "Project", user),
  "",
  "### Shiny Applications",
  "",
  make_repo_table(data$my_repos$shiny_applications, "App", user),
  "",
  "### Analysis & Research Code",
  "",
  make_repo_table(data$my_repos$analysis_research_code, "Repo", user),
  "",
  "---",
  "",
  "## External Contributions",
  "",
  paste0(
    "Pull requests I have authored or been assigned to in repositories outside ",
    "my own accounts, ordered by number of merged PRs."
  ),
  "",
  make_external_table(data$external_contributions, user)
)

writeLines(lines, output_file)
cat(sprintf("Generated %s\n", output_file))
