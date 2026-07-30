.docx_xml_cache <- new.env(parent = emptyenv())

get_docx_xml <- function(path) {
  key <- normalizePath(path, winslash = "/", mustWork = FALSE)
  if (!is.null(.docx_xml_cache[[key]])) return(.docx_xml_cache[[key]])
  con <- tryCatch(unz(path, "word/document.xml", open = "rb"), error = function(e) NULL)
  if (is.null(con)) return(NULL)
  raw <- tryCatch(readBin(con, "raw", n = 8e7), error = function(e) NULL)
  try(close(con), silent = TRUE)
  if (is.null(raw) || length(raw) == 0) return(NULL)
  xml <- tryCatch({ x <- rawToChar(raw); Encoding(x) <- "UTF-8"; x }, error = function(e) NULL)
  if (is.null(xml)) return(NULL)
  .docx_xml_cache[[key]] <- xml
  xml
}

parse_document <- function(path) {
  if (!file.exists(path)) return(list(ok = FALSE, error = paste("File not found:", path)))
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
  res$n_words  <- max(0L, { m <- gregexpr("\\S+", res$text, perl = TRUE)[[1]]; if (m[1] == -1) 0L else length(m) })
  res$sections <- detect_sections(res$text)
  res$ok       <- TRUE
  res
}

parse_pdf <- function(path) {
  pages <- pdftools::pdf_text(path)
  pages <- vapply(pages, normalize_text, character(1))
  list(ok = TRUE, text = paste(pages, collapse = "\n\n"), pages = pages, n_pages = length(pages))
}

parse_docx <- function(path) {
  doc <- officer::read_docx(path)
  content <- officer::docx_summary(doc)
  txt_rows <- content[content$content_type %in% c("paragraph", "table cell"), ]
  text_full <- normalize_text(paste(txt_rows$text, collapse = "\n"))
  pages <- split_by_size(text_full, 3000L)
  list(ok = TRUE, text = text_full, pages = pages, n_pages = length(pages))
}

parse_txt <- function(path) {
  text_full <- normalize_text(paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n"))
  pages <- split_by_size(text_full, 3000L)
  list(ok = TRUE, text = text_full, pages = pages, n_pages = length(pages))
}

normalize_text <- function(x) {
  x <- gsub("\r\n?", "\n", x, perl = TRUE)
  x <- gsub("[ \t]+", " ", x, perl = TRUE)
  x <- gsub("\n{3,}", "\n\n", x, perl = TRUE)
  trimws(x)
}

split_by_size <- function(text, chunk_size) {
  if (nchar(text) <= chunk_size) return(text)
  starts <- seq(1L, nchar(text), by = chunk_size)
  vapply(starts, function(s) substr(text, s, s + chunk_size - 1L), character(1))
}

detect_sections <- function(text) {
  if (!nzchar(text)) {
    return(data.frame(heading = character(), level = integer(), start_char = integer(), end_char = integer(), stringsAsFactors = FALSE))
  }
  patterns <- c(
    "(?m)^\\s*\\d+(?:\\.\\d+){0,3}\\.?\\s+[A-Z][^\n]{2,120}$",
    "(?m)^\\s*[A-Z][A-Z0-9 &/\\-]{2,80}$",
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
        level <- length(gregexpr("\\.", heading, perl = TRUE)[[1]])
        if (level < 0) level <- 0
        hits[[length(hits) + 1L]] <- data.frame(heading = heading, level = level, start_char = start, end_char = end, stringsAsFactors = FALSE)
      }
    }
  }
  if (!length(hits))
    return(data.frame(heading = character(), level = integer(), start_char = integer(), end_char = integer(), stringsAsFactors = FALSE))
  df <- do.call(rbind, hits)
  df <- df[!duplicated(df$start_char), ]
  df <- df[order(df$start_char), ]
  rownames(df) <- NULL
  df
}

docx_paragraph_texts <- function(path) {
  xml <- get_docx_xml(path)
  if (is.null(xml) || !nzchar(xml)) return(character(0))
  xml <- gsub("(?s)<w:instrText[^>]*>.*?</w:instrText>", "", xml, perl = TRUE)
  xml <- gsub("(?s)<w:delText[^>]*>.*?</w:delText>", "", xml, perl = TRUE)
  xml <- gsub("<w:tab[ /][^>]*>", "\t", xml, perl = TRUE)
  xml <- gsub("</w:p>", "\n", xml, fixed = TRUE)
  xml <- gsub("<[^>]+>", "", xml, perl = TRUE)
  xml <- gsub("&lt;", "<", xml, fixed = TRUE)
  xml <- gsub("&gt;", ">", xml, fixed = TRUE)
  xml <- gsub("&quot;", "\"", xml, fixed = TRUE)
  xml <- gsub("&apos;", "'", xml, fixed = TRUE)
  xml <- gsub("&amp;", "&", xml, fixed = TRUE)
  strsplit(xml, "\n", fixed = TRUE)[[1]]
}

docx_structured_text <- function(path) {
  xml <- get_docx_xml(path)
  if (is.null(xml) || !nzchar(xml)) return("")
  xml <- gsub("(?s)<w:instrText[^>]*>.*?</w:instrText>", "", xml, perl = TRUE)
  xml <- gsub("(?s)<w:delText[^>]*>.*?</w:delText>", "", xml, perl = TRUE)
  unescape <- function(s) {
    s <- gsub("&lt;", "<", s, fixed = TRUE)
    s <- gsub("&gt;", ">", s, fixed = TRUE)
    s <- gsub("&quot;", "\"", s, fixed = TRUE)
    s <- gsub("&apos;", "'", s, fixed = TRUE)
    gsub("&amp;", "&", s, fixed = TRUE)
  }
  frag_text <- function(s) {
    s <- gsub("<w:tab[ /][^>]*>", " ", s, perl = TRUE)
    s <- gsub("<[^>]+>", "", s, perl = TRUE)
    trimws(gsub("[ \t]+", " ", unescape(s)))
  }
  m <- gregexpr("(?s)<w:tbl>.*?</w:tbl>|<w:p[ >].*?</w:p>", xml, perl = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) return("")
  blocks <- regmatches(xml, gregexpr("(?s)<w:tbl>.*?</w:tbl>|<w:p[ >].*?</w:p>", xml, perl = TRUE))[[1]]
  out <- character(0)
  tbl_no <- 0L
  for (b in blocks) {
    if (startsWith(b, "<w:tbl>")) {
      tbl_no <- tbl_no + 1L
      rows <- regmatches(b, gregexpr("(?s)<w:tr[ >].*?</w:tr>", b, perl = TRUE))[[1]]
      if (length(rows) == 0) next
      out <- c(out, sprintf("[TABLE %d]", tbl_no))
      for (r in rows) {
        cells <- regmatches(r, gregexpr("(?s)<w:tc[ >].*?</w:tc>", r, perl = TRUE))[[1]]
        if (length(cells) == 0) next
        vals <- vapply(cells, frag_text, character(1), USE.NAMES = FALSE)
        out <- c(out, paste0("| ", paste(vals, collapse = " | "), " |"))
      }
      out <- c(out, sprintf("[END TABLE %d]", tbl_no))
    } else {
      t <- frag_text(b)
      if (nzchar(t)) out <- c(out, t)
    }
  }
  paste(out, collapse = "\n")
}

read_doc <- function(path, ext) {
  ext <- tolower(ext)
  if (ext == "pdf") {
    paste(pdftools::pdf_text(path), collapse = "\n\n")
  } else if (ext == "docx") {
    txt <- tryCatch(docx_structured_text(path), error = function(e) "")
    if (nzchar(txt)) txt else {
      s <- officer::docx_summary(officer::read_docx(path))
      paste(s$text[nzchar(s$text)], collapse = "\n")
    }
  } else if (ext %in% c("xlsx", "xls")) {
    df <- readxl::read_excel(path, sheet = 1, .name_repair = "unique")
    paste(apply(df, 1, function(row) paste(row, collapse = " ")), collapse = "\n")
  } else if (ext == "txt") {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    stop("Unsupported file type: .", ext)
  }
}
