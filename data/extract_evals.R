read_pdf_text <- function(path) {
  if (!file.exists(path)) {
    stop(sprintf("PDF file not found: %s (cwd: %s)", path, getwd()))
  }

  # stdout = TRUE forces system2 to go through /bin/sh on Linux, which
  # splits unquoted paths with spaces (every PDF filename here has spaces)
  # into separate args. shQuote() wraps the path so the shell sees it as
  # a single token.
  lines <- system2("pdftotext", c("-layout", shQuote(path), "-"),
                   stdout = TRUE)
  status <- attr(lines, "status")

  if (!is.null(status) && !identical(status, 0L)) {
    stop(sprintf("pdftotext failed for %s (exit status: %d)", path, status))
  }

  paste(lines, collapse = "\n")
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
  if (length(tokens) < 14) {
    stop(sprintf("Could not parse numeric values for %s", label))
  }

  # UC Davis line format: (count_r, pct_r) for r in 5..1, then M, SD, Mdn, N.
  counts <- tokens[c(1, 3, 5, 7, 9)]
  names(counts) <- as.character(5:1)

  list(
    mean   = tokens[length(tokens) - 3],
    sd     = tokens[length(tokens) - 2],
    n      = tokens[length(tokens)],
    counts = counts
  )
}

extract_ucla_question_stats <- function(text, question_number, label) {
  # Each question section runs from "N.M)" to the next "N.(M+1))" or the
  # next "3." subsection. Slice it out first, then pull pieces by name —
  # the layout puts n=, av.=, dev.=, and the count vector in inconsistent
  # orders across PDFs.
  section_pattern <- sprintf("(?s)%s\\)(.*?)(?:[0-9]+\\.[0-9]+\\)| 3\\.)", question_number)
  section <- extract_first_match(text, section_pattern, paste0(label, " section"))[[1]]

  counts_pattern <- paste0(
    "Very Low or\\s+",
    "([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+",
    "([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)\\s+([0-9]+)",
    "\\s+Very High or"
  )
  count_captures <- extract_first_match(section, counts_pattern, paste0(label, " counts"))
  counts <- as.integer(count_captures)
  names(counts) <- as.character(1:9)

  list(
    n      = as.integer(extract_first_match(section, "n=([0-9]+)", paste0(label, " n"))[[1]]),
    mean   = as.numeric(extract_first_match(section, "av\\.=?([0-9]+(?:\\.[0-9]+)?)", paste0(label, " mean"))[[1]]),
    sd     = as.numeric(extract_first_match(section, "dev\\.=?([0-9]+(?:\\.[0-9]+)?)", paste0(label, " sd"))[[1]]),
    counts = counts
  )
}

weighted_mean <- function(values, weights) {
  sum(values * weights) / sum(weights)
}

# Pool SDs across groups using sum-of-squares of deviations from each group mean,
# plus between-group variation from each group mean to the overall mean.
pooled_sd <- function(means, sds, ns) {
  overall_mean <- weighted_mean(means, ns)
  within_ss <- sum((ns - 1) * sds^2)
  between_ss <- sum(ns * (means - overall_mean)^2)
  sqrt((within_ss + between_ss) / (sum(ns) - 1))
}

normalize_nine_point_to_five <- function(value) {
  1 + (value - 1) * 4 / 8
}

# A linear rescale of x stretches/compresses spread by the same factor on
# either side of the midpoint, so SDs only get multiplied by that slope (4/8).
rescale_nine_point_sd_to_five <- function(value) {
  value * 4 / 8
}

counts_to_long <- function(counts, metric, year, role, course, scale_max) {
  data.frame(
    year      = year,
    role      = role,
    course    = course,
    metric    = metric,
    scale_max = scale_max,
    rating    = as.integer(names(counts)),
    count     = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

extract_written_comments <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  # Find the start of a comments section. Different PDF templates use different
  # headers:
  #   - UC Davis older: "Please write any additional comments"
  #   - UC Davis 2024+: bare "Course Comments." or "Comments."
  #   - UCLA Class Climate: "4. Comments:" followed by "4.1) Please identify ..."
  header_re <- "(?i)(additional (feedback|comments)|open.ended|write.*comment|student.*comment|comment.*section|^[[:space:]]*course comments?\\.|^[[:space:]]*[0-9]+\\.[[:space:]]+comments?:|comments report)"
  header_idx <- grep(header_re, lines, perl = TRUE)
  if (!length(header_idx)) return(character(0))

  start <- header_idx[[1]] + 1L
  # End at the next "page footer" or "Term  Eval Opened" header that marks
  # the next section in UC Davis 2024+ summaries.
  end_re <- "(?i)(Class Climate evaluation|^Term\\s+Eval Opened|^[[:space:]]*Eval Opened|^[[:space:]]*Survey Closed|^[[:space:]]*CRN[[:space:]]+Subject)"
  end_idx <- grep(end_re, lines, perl = TRUE)
  end_idx <- end_idx[end_idx > start]
  end <- if (length(end_idx)) end_idx[[1]] - 1L else length(lines)

  block <- lines[seq(start, end)]
  block <- block[!grepl("^[[:space:]]*NA[[:space:]]*$", block)]
  block <- block[!grepl("^[A-Z ]{10,}$|^[-=]{3,}$|^[0-9]+$|^Page|Class Climate evaluation", block)]
  # Strip lone question-number markers like "4.1)" left by the prompt.
  block <- sub("^\\s*[0-9]+\\.[0-9]+\\)\\s*$", "", block, perl = TRUE)

  # Join everything into a single string, collapse runs of blank lines into a
  # single newline, then split on a paragraph boundary defined as
  # "sentence-end-punctuation + newline + capital-letter". This works for both
  # UC Davis exports (which separate comments with blank lines) and UCLA
  # Class Climate exports (which run them together but always start a fresh
  # line per response).
  joined <- paste(trimws(block), collapse = "\n")
  # Strip multi-line prompts that wrap (e.g. UCLA's "Please identify ...
  # this teaching assistant\nand course."). Match across newlines via (?s).
  joined <- gsub(
    "(?is)Please (provide|write|share|identify)[^.]*?\\.\\s*",
    "", joined, perl = TRUE
  )
  joined <- gsub("\\n{2,}", "\n", joined, perl = TRUE)

  chunks <- strsplit(joined, "(?<=[.!?])\\s*\\n(?=[A-Z\"(])", perl = TRUE)[[1]]
  comments <- gsub("\\s*\\n\\s*", " ", chunks, perl = TRUE)
  comments <- trimws(comments)
  comments <- comments[nchar(comments) >= 20L]
  # Anything still starting lowercase is a wrap-fragment from the previous page.
  comments <- comments[grepl("^[A-Z\"\\(]", comments)]
  comments
}

# Crude sentiment filter for "highlight-worthy" comments: keep only those
# that contain a strong positive cue and no negation/criticism cue. We aren't
# trying to do real sentiment analysis — just to surface unambiguously
# positive feedback for the public page and leave qualifiers / critiques in
# the source PDFs.
positive_cues <- c(
  "awesome", "amazing", "excellent", "fantastic", "wonderful",
  "great", "loved", "love ", "perfect", "best", "outstanding", "incredible",
  "phenomenal", "spectacular", "appreciate", "appreciated", "grateful",
  "engaging", "patient", "knowledgeable", "knows his stuff", "knew his stuff",
  "helpful", "supportive", "approachable", "responsive", "thorough",
  "valuable", "thank you", "really enjoyed", "enjoyed the",
  "strong foundation", "well-structured", "well-organized", "well designed",
  "well planned", "went beyond", "above and beyond", "genuinely cares",
  "truly cares", "very good", "very clear", "very helpful",
  "kind of instructor", "strong instructor", "great instructor",
  "great professor"
)
negative_cues <- c(
  # Critique / suggestion / hedging markers — any one of these disqualifies
  # the comment from the public highlights, even if it also contains praise.
  " but ", " however", " though ", "although",
  " could have", " could be ", " should ", " would have", " would benefit",
  " would be helpful", " would be better", " would be great if",
  " wish ", " needed ", " need more", " need to ",
  " lacked", " lack of", " rushed", " rush through",
  "confusing", "frustrating", "impeded", "boring",
  " poor ", " bad ", " weak ", "disappointed",
  "not enough", "too much", "too little", "not clear",
  "wasn't", "didn't", "isn't", "doesn't", "wouldn't",
  "couldn't", "shouldn't", "n't ",
  "problem", " issue ", "more material", "less ",
  "missing", "instead of", "rather than",
  " slow", " slower", "took away", "spent more time", "if he",
  # Hedging language used by lukewarm reviewers ("I think he does try...").
  "i think", "i hope", "i'm sure", "i am sure", "all things considered",
  "for the most part", "to be fair", "key points were made",
  "more office hours", "more time", "would benefit from"
)

# Tokens that indicate a comment is about a TA rather than the Instructor of
# Record. Used only when filtering UC Davis (IOR) highlights — TA praise is
# kept verbatim under the UCLA (TA) role.
ta_mention_cues <- c(
  " ta ", " ta.", " ta!", " ta-", " ta,",
  " the ta", "a ta ", "great ta", "wonderful ta", "amazing ta",
  "katy"   # 2025 Epi 204 TA
)

is_positive_comment <- function(comment, role = NA_character_) {
  # Normalize Unicode curly quotes -> ASCII so "wasn't" etc. matches the
  # cue list (PDF extraction yields right single quotation marks U+2019).
  lc <- tolower(comment)
  lc <- gsub("[‘’]", "'", lc, perl = TRUE)
  lc <- gsub("[“”]", '"', lc, perl = TRUE)
  # Pad with spaces so word-boundary cues like " bad " match at the ends.
  lc_padded <- paste0(" ", lc, " ")

  has_positive <- any(vapply(positive_cues, grepl, logical(1),
                             x = lc_padded, fixed = TRUE))
  has_negative <- any(vapply(negative_cues, grepl, logical(1),
                             x = lc_padded, fixed = TRUE))
  if (!has_positive || has_negative) return(FALSE)

  # For IOR comments, drop ones that are clearly *primarily* about the TA —
  # they belong under the TA-role section if anywhere. A comment that
  # mentions both the TA and the instructor in roughly equal measure is
  # kept (e.g. "Both TA and instructor went beyond their way to help us").
  if (!is.na(role) && role == "UC Davis (Instructor of Record)") {
    mentions_ta <- any(vapply(ta_mention_cues, grepl, logical(1),
                              x = lc_padded, fixed = TRUE))
    mentions_ior <- any(vapply(
      c("professor", "instructor", "morrison", " dr. ", " he ", " his "),
      grepl, logical(1), x = lc_padded, fixed = TRUE))
    if (mentions_ta && !mentions_ior) return(FALSE)
  }
  TRUE
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

  year <- as.integer(term[[2]])
  role <- "UC Davis (Instructor of Record)"

  summary <- data.frame(
    year = year,
    quarter = term[[1]],
    course = "Epi 204",
    role = role,
    overall_rating = overall$mean,
    overall_rating_sd = overall$sd,
    instructor_effectiveness = instructor$mean,
    instructor_effectiveness_sd = instructor$sd,
    course_organization = organization$mean,
    course_organization_sd = organization$sd,
    response_rate = as.numeric(extract_first_match(text, "% responding\\s+([0-9]+(?:\\.[0-9]+)?)", "response rate")) / 100,
    n_responses = as.integer(overall$n),
    stringsAsFactors = FALSE
  )

  responses <- rbind(
    counts_to_long(overall$counts,      "overall_rating",           year, role, "Epi 204", 5L),
    counts_to_long(instructor$counts,   "instructor_effectiveness", year, role, "Epi 204", 5L),
    counts_to_long(organization$counts, "course_organization",      year, role, "Epi 204", 5L)
  )

  comments <- tryCatch(
    extract_written_comments(text),
    error = function(e) character(0)
  )

  list(summary = summary, responses = responses, comments = comments)
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

summarize_ucla_metric <- function(sections, metric) {
  means <- vapply(sections, function(section) section[[metric]]$mean, numeric(1))
  sds   <- vapply(sections, function(section) section[[metric]]$sd,   numeric(1))
  ns    <- vapply(sections, function(section) section[[metric]]$n,    numeric(1))

  list(
    mean = round(normalize_nine_point_to_five(weighted_mean(means, ns)), 2),
    sd   = round(rescale_nine_point_sd_to_five(pooled_sd(means, sds, ns)), 2)
  )
}

pool_ucla_counts <- function(sections, metric) {
  Reduce(`+`, lapply(sections, function(section) section[[metric]]$counts))
}

extract_ucla_eval <- function(paths) {
  sections <- lapply(paths, extract_ucla_section)

  overall      <- summarize_ucla_metric(sections, "overall_rating")
  instructor   <- summarize_ucla_metric(sections, "instructor_effectiveness")
  organization <- summarize_ucla_metric(sections, "course_organization")

  year <- sections[[1]]$year
  role <- "UCLA (TA)"
  course <- "Biostat 100B (TA)"

  summary <- data.frame(
    year = year,
    quarter = "Winter",
    course = course,
    role = role,
    overall_rating = overall$mean,
    overall_rating_sd = overall$sd,
    instructor_effectiveness = instructor$mean,
    instructor_effectiveness_sd = instructor$sd,
    course_organization = organization$mean,
    course_organization_sd = organization$sd,
    response_rate = sum(vapply(sections, function(section) section$n_responses, numeric(1))) /
      sum(vapply(sections, function(section) section$enrollment, numeric(1))),
    n_responses = sum(vapply(sections, function(section) section$n_responses, numeric(1))),
    stringsAsFactors = FALSE
  )

  responses <- rbind(
    counts_to_long(pool_ucla_counts(sections, "overall_rating"),           "overall_rating",           year, role, course, 9L),
    counts_to_long(pool_ucla_counts(sections, "instructor_effectiveness"), "instructor_effectiveness", year, role, course, 9L),
    counts_to_long(pool_ucla_counts(sections, "course_organization"),      "course_organization",      year, role, course, 9L)
  )

  # Collect written comments from all UCLA section PDFs.
  comments <- tryCatch(
    unique(unlist(lapply(paths, function(p) {
      extract_written_comments(read_pdf_text(p))
    }))),
    error = function(e) character(0)
  )

  list(summary = summary, responses = responses, comments = comments)
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

  parts <- c(
    list(extract_ucla_eval(ucla_paths)),
    lapply(uc_davis_paths, extract_uc_davis_eval)
  )

  summary   <- do.call(rbind, lapply(parts, `[[`, "summary"))
  responses <- do.call(rbind, lapply(parts, `[[`, "responses"))

  # Collect comments: data frame with year, role, course, and comment text.
  comments <- do.call(rbind, lapply(parts, function(p) {
    if (!length(p$comments)) return(NULL)
    data.frame(
      year    = p$summary$year[[1]],
      role    = p$summary$role[[1]],
      course  = p$summary$course[[1]],
      comment = p$comments,
      stringsAsFactors = FALSE
    )
  }))

  list(
    summary   = summary[order(summary$year), ],
    responses = responses[order(responses$year, responses$metric, responses$rating), ],
    comments  = if (!is.null(comments)) comments[order(comments$year), ] else data.frame()
  )
}
