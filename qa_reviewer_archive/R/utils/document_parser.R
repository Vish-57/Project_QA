# =============================================================================
# document_parser.R
# Extract clean text and (where possible) structural metadata (sections,
# pages, headings) from PDF / DOCX / TXT.
# =============================================================================

# Required packages: pdftools, officer, stringr, tools

#' Parse a document into a normalized representation.
#' @param path Absolute file path.
#' @return list(
#'   ok           = logical,
#'   error        = character or NULL,
#'   filename     = basename,
#'   ext          = "pdf"|"docx"|"txt",
#'   text         = full plain text,
#'   pages        = character vector, one per page (PDF) or one chunk (others),
#'   sections     = data.frame(heading, level, start_char, end_char),
#'   n_pages      = integer,
#'   n_chars      = integer,
#'   n_words      = integer
#' )
parse_document <- function(path) {
  if (!file.exists(path)) {
    return(list(ok = FALSE, error = paste("File not found:", path)))
  }
  ext <- tolower(tools::file_ext(path))
  res <- tryCatch({
    switch(ext,
      "pdf"  = parse_pdf(path),
      "docx" = parse_docx(path),
      "txt"  = parse_txt(path),
      stop("Unsupported file type: .", ext)
    )
  }, error = function(e) list(ok = FALSE, error = conditionMessage(e)))

  if (isFALSE(res$ok)) return(res)

  res$filename <- basename(path)
  res$ext      <- ext
  res$n_chars  <- nchar(res$text)
  res$n_words  <- length(strsplit(res$text, "\\s+")[[1]])
  res$sections <- detect_sections(res$text)
  res$ok       <- TRUE
  res
}

# ----- PDF ------------------------------------------------------------------
parse_pdf <- function(path) {
  pages <- pdftools::pdf_text(path)
  pages <- vapply(pages, normalize_text, character(1))
  list(
    ok      = TRUE,
    text    = paste(pages, collapse = "\n\n"),
    pages   = pages,
    n_pages = length(pages)
  )
}

# ----- DOCX -----------------------------------------------------------------
parse_docx <- function(path) {
  doc <- officer::read_docx(path)
  content <- officer::docx_summary(doc)
  # docx_summary returns a data.frame with content_type, text, style_name, level
  txt_rows <- content[content$content_type %in% c("paragraph", "table cell"), ]
  text_full <- paste(txt_rows$text, collapse = "\n")
  text_full <- normalize_text(text_full)
  # Approximate one "page" per ~3000 chars so downstream code has consistent units
  pages <- split_by_size(text_full, 3000L)
  list(
    ok      = TRUE,
    text    = text_full,
    pages   = pages,
    n_pages = length(pages)
  )
}

# ----- TXT ------------------------------------------------------------------
parse_txt <- function(path) {
  text_full <- normalize_text(paste(readLines(path, warn = FALSE,
                                              encoding = "UTF-8"),
                                    collapse = "\n"))
  pages <- split_by_size(text_full, 3000L)
  list(
    ok      = TRUE,
    text    = text_full,
    pages   = pages,
    n_pages = length(pages)
  )
}

# ----- helpers --------------------------------------------------------------

normalize_text <- function(x) {
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("[ ​-‍﻿]", " ", x, perl = TRUE)  # nbsp + zero-widths
  x <- gsub("[ \t]+", " ", x, perl = TRUE)
  x <- gsub("\n{3,}", "\n\n", x, perl = TRUE)
  trimws(x)
}

split_by_size <- function(text, chunk_size) {
  if (nchar(text) <= chunk_size) return(text)
  starts <- seq(1L, nchar(text), by = chunk_size)
  vapply(starts, function(s) substr(text, s, s + chunk_size - 1L), character(1))
}

#' Lightweight heading detector. Looks for common QA-document section labels
#' AND generic ALL CAPS / numbered headings. Returns a data.frame.
detect_sections <- function(text) {
  if (!nzchar(text)) {
    return(data.frame(heading = character(), level = integer(),
                      start_char = integer(), end_char = integer(),
                      stringsAsFactors = FALSE))
  }

  patterns <- c(
    # Numbered headings: "1. Introduction", "2.3 Methods", "10.4.1 Results"
    "(?m)^\\s*\\d+(?:\\.\\d+){0,3}\\.?\\s+[A-Z][^\n]{2,120}$",
    # All-caps headings (>= 3 chars, allowing spaces & ampersands)
    "(?m)^\\s*[A-Z][A-Z0-9 &/\\-]{2,80}$",
    # Common QA / clinical labels (case-insensitive)
    paste0("(?im)^\\s*(", paste(c(
      "Title Page", "Abstract", "Synopsis", "Table of Contents",
      "List of Abbreviations", "Introduction", "Background",
      "Objectives?", "Study Design", "Methodology", "Methods",
      "Statistical (Analysis|Methods)", "Results", "Discussion",
      "Conclusions?", "Safety", "Efficacy", "Adverse Events?",
      "References", "Appendi(x|ces)", "Signatures?",
      "Scope", "Purpose", "Responsibilities", "Procedure",
      "Definitions", "Quality Assurance", "Compliance"
    ), collapse = "|"), ")\\s*:?\\s*$")
  )

  hits <- list()
  for (p in patterns) {
    m <- gregexpr(p, text, perl = TRUE)[[1]]
    if (m[1] != -1) {
      lens <- attr(m, "match.length")
      for (i in seq_along(m)) {
        start <- m[i]; end <- start + lens[i] - 1L
        heading <- trimws(substr(text, start, end))
        # crude level guess: deeper headings have more dots
        level <- length(gregexpr("\\.", heading, perl = TRUE)[[1]])
        if (level < 0) level <- 0
        hits[[length(hits) + 1L]] <- data.frame(
          heading = heading, level = level,
          start_char = start, end_char = end,
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(hits))
    return(data.frame(heading = character(), level = integer(),
                      start_char = integer(), end_char = integer(),
                      stringsAsFactors = FALSE))

  df <- do.call(rbind, hits)
  df <- df[!duplicated(df$start_char), ]
  df <- df[order(df$start_char), ]
  rownames(df) <- NULL
  df
}
