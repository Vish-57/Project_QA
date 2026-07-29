# =============================================================================
# chunker.R
# Section-aware chunking. Tries to break on detected headings; falls back to
# size-based chunking with overlap so cross-boundary issues are still caught.
# =============================================================================

#' Build chunks for LLM analysis.
#' @param parsed     Result of parse_document().
#' @param chunk_chars Target chunk size in characters (~4 chars per token).
#' @param overlap    Character overlap between adjacent chunks.
#' @return data.frame(id, heading, start_char, end_char, text)
build_chunks <- function(parsed, chunk_chars = 6000L, overlap = 400L) {
  text <- parsed$text
  if (!nzchar(text)) {
    return(data.frame(id = integer(), heading = character(),
                      start_char = integer(), end_char = integer(),
                      text = character(), stringsAsFactors = FALSE))
  }

  sections <- parsed$sections
  use_sections <- !is.null(sections) && nrow(sections) >= 2L

  if (use_sections) {
    # Build section spans
    starts <- sections$start_char
    ends   <- c(sections$start_char[-1L] - 1L, nchar(text))
    spans  <- data.frame(
      heading = sections$heading,
      start   = starts,
      end     = ends,
      stringsAsFactors = FALSE
    )
    # Split any oversized section
    chunks <- list()
    for (i in seq_len(nrow(spans))) {
      span_text <- substr(text, spans$start[i], spans$end[i])
      if (nchar(span_text) <= chunk_chars) {
        chunks[[length(chunks) + 1L]] <- list(
          heading = spans$heading[i],
          start = spans$start[i], end = spans$end[i],
          text = span_text
        )
      } else {
        sub <- size_chunks(span_text, chunk_chars, overlap)
        for (j in seq_along(sub)) {
          chunks[[length(chunks) + 1L]] <- list(
            heading = paste0(spans$heading[i], " (part ", j, ")"),
            start = spans$start[i] + attr(sub, "starts")[j] - 1L,
            end   = spans$start[i] + attr(sub, "ends")[j] - 1L,
            text  = sub[j]
          )
        }
      }
    }
  } else {
    sub <- size_chunks(text, chunk_chars, overlap)
    chunks <- lapply(seq_along(sub), function(j) list(
      heading = paste0("Chunk ", j),
      start = attr(sub, "starts")[j],
      end   = attr(sub, "ends")[j],
      text  = sub[j]
    ))
  }

  df <- data.frame(
    id         = seq_along(chunks),
    heading    = vapply(chunks, function(c) c$heading, character(1)),
    start_char = vapply(chunks, function(c) c$start,   integer(1)),
    end_char   = vapply(chunks, function(c) c$end,     integer(1)),
    text       = vapply(chunks, function(c) c$text,    character(1)),
    stringsAsFactors = FALSE
  )
  df
}

#' Size-based chunker with overlap.
size_chunks <- function(text, chunk_chars, overlap) {
  n <- nchar(text)
  if (n <= chunk_chars) {
    out <- text
    attr(out, "starts") <- 1L
    attr(out, "ends")   <- n
    return(out)
  }
  step <- chunk_chars - overlap
  starts <- seq(1L, n, by = step)
  ends   <- pmin(starts + chunk_chars - 1L, n)
  out <- mapply(function(s, e) substr(text, s, e), starts, ends,
                USE.NAMES = FALSE)
  attr(out, "starts") <- as.integer(starts)
  attr(out, "ends")   <- as.integer(ends)
  out
}
