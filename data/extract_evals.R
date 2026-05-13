read_pdf_text <- function(path) {
  output <- system2(
    "pdftotext",
    c("-layout", normalizePath(path, mustWork = TRUE), "-"),
    stdout = TRUE
  )

  status <- attr(output, "status")
  if (!is.null(status) && status != 0) {
    stop(sprintf("pdftotext failed for %s", path))
  }

  paste(output, collapse = "\n")
}

extract_first_match <- function(text, pattern, label) {
  match <- regexec(pattern, text, perl = TRUE)
  captures <- regmatches(text, match)[[1]]

  if (!length(captures)) {
    stop(sprintf("Could not extract %s", label))
  }

  captures[-1]
}

extract_numeric_tokens <- function(text) {
  matches <- gregexpr("\\d+(?:\\.\\d+)?", text, perl = TRUE)
  as.numeric(regmatches(text, matches)[[1]])
}

extract_line_tail_stats <- function(text, pattern, label) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  matched_lines <- grep(pattern, lines, value = TRUE, perl = TRUE)

  if (!length(matched_lines)) {
    stop(sprintf("Could not find %s", label))
  }

  tokens <- extract_numeric_tokens(matched_lines[[1]])
  if (length(tokens) < 4) {
    stop(sprintf("Could not parse numeric values for %s", label))
  }

  list(
    mean = tokens[length(tokens) - 3],
    n = tokens[length(tokens)]
  )
}

extract_ucla_question_stats <- function(text, question_number, label) {
  pattern <- sprintf(
    "(?s)%s\\).*?n=([0-9]+).*?av\\.=?([0-9]+(?:\\.[0-9]+)?)",
    question_number
  )
  captures <- extract_first_match(text, pattern, label)

  list(
    n = as.integer(captures[[1]]),
    mean = as.numeric(captures[[2]])
  )
}

weighted_mean <- function(values, weights) {
  sum(values * weights) / sum(weights)
}

normalize_nine_point_to_five <- function(value) {
  1 + (value - 1) * 4 / 8
}

extract_uc_davis_eval <- function(path) {
  text <- read_pdf_text(path)
  term <- extract_first_match(text, "([A-Za-z]+) Quarter ([0-9]{4})", "term")

  overall <- extract_line_tail_stats(
    text,
    "Please indicate the overall educational value of the course\\.",
    "overall educational value"
  )
  instructor <- extract_line_tail_stats(
    text,
    "Please indicate the overall teaching effectiveness of the instructor\\.",
    "overall teaching effectiveness"
  )
  organization <- extract_line_tail_stats(
    text,
    "The course was (well-organized and coordinated|presented in a logical and organized manner)\\.?",
    "course organization"
  )

  data.frame(
    year = as.integer(term[[2]]),
    quarter = term[[1]],
    course = "Epi 204",
    overall_rating = overall$mean,
    instructor_effectiveness = instructor$mean,
    course_organization = organization$mean,
    response_rate = as.numeric(extract_first_match(text, "% responding\\s+([0-9]+(?:\\.[0-9]+)?)", "response rate")) / 100,
    n_responses = as.integer(overall$n),
    stringsAsFactors = FALSE
  )
}

extract_ucla_section <- function(path) {
  text <- read_pdf_text(path)
  year <- 2000 + as.integer(extract_first_match(text, "(\\d{2})W:", "year"))

  list(
    year = year,
    enrollment = as.integer(extract_first_match(text, "Enrollment =\\s*([0-9]+)", "enrollment")),
    n_responses = as.integer(extract_first_match(text, "No\\. of responses =\\s*([0-9]+)", "number of responses")),
    response_rate = as.numeric(extract_first_match(text, "Response Rate =\\s*([0-9]+(?:\\.[0-9]+)?)%", "response rate")) / 100,
    overall_rating = extract_ucla_question_stats(text, "2\\.7", "overall value"),
    instructor_effectiveness = extract_ucla_question_stats(text, "2\\.8", "overall teaching assistant rating"),
    course_organization = extract_ucla_question_stats(text, "2\\.3", "organization")
  )
}

extract_ucla_eval <- function(paths) {
  sections <- lapply(paths, extract_ucla_section)

  data.frame(
    year = sections[[1]]$year,
    quarter = "Winter",
    course = "Biostat 100B (TA)",
    overall_rating = round(
      normalize_nine_point_to_five(weighted_mean(
        vapply(sections, function(section) section$overall_rating$mean, numeric(1)),
        vapply(sections, function(section) section$overall_rating$n, numeric(1))
      )),
      2
    ),
    instructor_effectiveness = round(
      normalize_nine_point_to_five(weighted_mean(
        vapply(sections, function(section) section$instructor_effectiveness$mean, numeric(1)),
        vapply(sections, function(section) section$instructor_effectiveness$n, numeric(1))
      )),
      2
    ),
    course_organization = round(
      normalize_nine_point_to_five(weighted_mean(
        vapply(sections, function(section) section$course_organization$mean, numeric(1)),
        vapply(sections, function(section) section$course_organization$n, numeric(1))
      )),
      2
    ),
    response_rate = sum(vapply(sections, function(section) section$n_responses, numeric(1))) /
      sum(vapply(sections, function(section) section$enrollment, numeric(1))),
    n_responses = sum(vapply(sections, function(section) section$n_responses, numeric(1))),
    stringsAsFactors = FALSE
  )
}

extract_evals_data <- function(base_dir = file.path("static", "files", "evals")) {
  uc_davis_paths <- file.path(
    base_dir,
    c(
      "Epi 204 Evaluations Spring 2023.pdf",
      "Evaluations epi 204 2024 summary.pdf",
      "evals epi 204 2025 summary.pdf"
    )
  )
  ucla_paths <- file.path(
    base_dir,
    c(
      "Morrison teaching eval 1.pdf",
      "Morrison teaching eval 2.pdf"
    )
  )

  evals <- rbind(
    extract_ucla_eval(ucla_paths),
    do.call(rbind, lapply(uc_davis_paths, extract_uc_davis_eval))
  )

  evals[order(evals$year), ]
}
