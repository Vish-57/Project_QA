# =============================================================================
# Mascot Spincontrol QA Reviewer - Enterprise AI Quality Assurance Terminal
# Tailored specifically for Mascot Spincontrol Pvt. Ltd.
#
# KEY FEATURES:
#   1. Eagle-eyed Senior QA Expert Prompt for maximum microscopic error capture.
#   2. Persistent Global Guidelines saved to SQLite and dynamically verified.
#   3. Local SQLite-backed Study History Explorer with filtering and load/delete.
#   4. High-End Corporate Dark-Navy/Platinum Custom CSS Theme.
# =============================================================================

library(shiny)
library(httr2)
library(jsonlite)
library(pdftools)
library(officer)
library(DT)
library(DBI)
library(RSQLite)
library(readxl) # Added for Excel support

OLLAMA_URL <- "http://127.0.0.1:11434"

# DB Connection Setup
DB_DIR <- "data"
DB_PATH <- file.path(DB_DIR, "mascot_qa_history.db")

# Initialize SQLite database on startup
init_db <- function() {
  if (!dir.exists(DB_DIR)) {
    dir.create(DB_DIR, recursive = TRUE, showWarnings = FALSE)
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)

  # Reviews table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS reviews (
      id TEXT PRIMARY KEY,
      study_code TEXT NOT NULL,
      filename TEXT NOT NULL,
      doc_type TEXT NOT NULL,
      model TEXT NOT NULL,
      overall_score INTEGER,
      risk_level TEXT,
      executive_summary TEXT,
      missing_information TEXT,
      created_at TEXT NOT NULL
    )
  ")

  # Issues table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS issues (
      id INTEGER PRIMARY KEY AUTOINCREMENT,
      review_id TEXT NOT NULL,
      severity TEXT NOT NULL,
      category TEXT NOT NULL,
      location TEXT NOT NULL,
      what_is_wrong TEXT NOT NULL,
      suggested_fix TEXT NOT NULL,
      original_text TEXT DEFAULT '',
      corrected_text TEXT DEFAULT '',
      page TEXT DEFAULT '',
      section TEXT DEFAULT '',
      FOREIGN KEY (review_id) REFERENCES reviews(id) ON DELETE CASCADE
    )
  ")

  # Migration: add newer columns to pre-existing databases
  issue_cols <- tryCatch(DBI::dbGetQuery(con, "PRAGMA table_info(issues)")$name,
    error = function(e) character(0)
  )
  if (length(issue_cols) > 0) {
    if (!("original_text" %in% issue_cols)) {
      DBI::dbExecute(con, "ALTER TABLE issues ADD COLUMN original_text TEXT DEFAULT ''")
    }
    if (!("corrected_text" %in% issue_cols)) {
      DBI::dbExecute(con, "ALTER TABLE issues ADD COLUMN corrected_text TEXT DEFAULT ''")
    }
    if (!("page" %in% issue_cols)) {
      DBI::dbExecute(con, "ALTER TABLE issues ADD COLUMN page TEXT DEFAULT ''")
    }
    if (!("section" %in% issue_cols)) {
      DBI::dbExecute(con, "ALTER TABLE issues ADD COLUMN section TEXT DEFAULT ''")
    }
  }

  # Settings table for persistence
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS settings (
      key TEXT PRIMARY KEY,
      value TEXT NOT NULL
    )
  ")

  # Protocol reviews table (for cross-document consistency)
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS protocols (
      study_code TEXT PRIMARY KEY,
      executive_summary TEXT,
      missing_information TEXT
    )
  ")

  # Automated SHA-256 Execution Cache Table
  DBI::dbExecute(con, "
    CREATE TABLE IF NOT EXISTS execution_cache (
      hash TEXT PRIMARY KEY,
      response_json TEXT NOT NULL,
      elapsed_s REAL NOT NULL,
      created_at TEXT NOT NULL
    )
  ")

  DBI::dbDisconnect(con)
}

init_db()

# DB Helper Functions
generate_id <- function() {
  paste0("REV_", format(Sys.time(), "%Y%m%d_%H%M%S"), "_", paste(sample(c(0:9, letters), 6, replace = TRUE), collapse = ""))
}

get_setting <- function(key, default = "") {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))
  res <- tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT value FROM settings WHERE key = ?", params = list(key))
    },
    error = function(e) NULL
  )
  if (is.null(res) || nrow(res) == 0) {
    return(default)
  }
  return(res$value[1])
}

save_setting <- function(key, value) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?)", params = list(key, value))
}

save_review <- function(id, study_code, filename, doc_type, model, data, missing_info) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))

  missing_info_json <- jsonlite::toJSON(missing_info, auto_unbox = TRUE)

  # Insert Review header
  DBI::dbExecute(con,
    "INSERT OR REPLACE INTO reviews (id, study_code, filename, doc_type, model, overall_score, risk_level, executive_summary, missing_information, created_at) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
    params = list(
      id,
      study_code,
      filename,
      doc_type,
      model,
      as.integer(data$overall_score %||% 0),
      data$risk_level %||% "Unknown",
      data$executive_summary %||% "",
      as.character(missing_info_json),
      format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  )

  # Insert Issues
  if (!is.null(data$issues) && NROW(data$issues) > 0) {
    df <- as.data.frame(data$issues, stringsAsFactors = FALSE)
    for (col in c(
      "severity", "category", "location", "what_is_wrong", "suggested_fix",
      "original_text", "corrected_text", "page", "section"
    )) {
      if (is.null(df[[col]])) df[[col]] <- ""
    }

    for (i in seq_len(nrow(df))) {
      DBI::dbExecute(con,
        "INSERT INTO issues (review_id, severity, category, location, what_is_wrong, suggested_fix, original_text, corrected_text, page, section) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)",
        params = list(
          id,
          df$severity[i],
          df$category[i],
          df$location[i],
          df$what_is_wrong[i],
          df$suggested_fix[i],
          df$original_text[i] %||% "",
          df$corrected_text[i] %||% "",
          as.character(df$page[i] %||% ""),
          as.character(df$section[i] %||% "")
        )
      )
    }
  }
}

get_all_reviews <- function() {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))
  res <- tryCatch(
    {
      DBI::dbGetQuery(con, "
      SELECT r.id, r.study_code, r.filename, r.doc_type, r.model, r.overall_score, r.risk_level, r.created_at,
             (SELECT COUNT(*) FROM issues i WHERE i.review_id = r.id) as issue_count
      FROM reviews r
      ORDER BY r.created_at DESC
    ")
    },
    error = function(e) data.frame()
  )
  return(res)
}

get_review_details <- function(review_id) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))

  review <- tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT * FROM reviews WHERE id = ?", params = list(review_id))
    },
    error = function(e) NULL
  )

  if (is.null(review) || nrow(review) == 0) {
    return(NULL)
  }

  issues <- tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT severity, category, location, what_is_wrong, suggested_fix, original_text, corrected_text, page, section FROM issues WHERE review_id = ?", params = list(review_id))
    },
    error = function(e) data.frame()
  )

  missing_info <- tryCatch(
    {
      jsonlite::fromJSON(review$missing_information[1])
    },
    error = function(e) character(0)
  )

  list(
    ok = TRUE,
    id = review$id[1],
    study_code = review$study_code[1],
    filename = review$filename[1],
    doc_type = review$doc_type[1],
    model = review$model[1],
    created_at = review$created_at[1],
    data = list(
      overall_score = review$overall_score[1],
      risk_level = review$risk_level[1],
      executive_summary = review$executive_summary[1],
      issues = issues,
      missing_information = missing_info
    )
  )
}


# Delete a review and all of its associated issues.
# SQLite does not enforce ON DELETE CASCADE unless foreign keys are
# explicitly enabled per-connection, so we remove child rows manually
# inside a transaction to keep the two tables consistent.
delete_review <- function(review_id) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))

  DBI::dbExecute(con, "PRAGMA foreign_keys = ON")
  DBI::dbBegin(con)
  ok <- tryCatch(
    {
      DBI::dbExecute(con, "DELETE FROM issues  WHERE review_id = ?", params = list(review_id))
      DBI::dbExecute(con, "DELETE FROM reviews WHERE id = ?", params = list(review_id))
      DBI::dbCommit(con)
      TRUE
    },
    error = function(e) {
      DBI::dbRollback(con)
      message("delete_review failed: ", conditionMessage(e))
      FALSE
    }
  )
  ok
}

# Retrieve protocol review for a given study code
get_protocol_review <- function(study_code) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))

  res <- tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT executive_summary, missing_information FROM protocols WHERE study_code = ?", params = list(study_code))
    },
    error = function(e) NULL
  )
  if (is.null(res) || nrow(res) == 0) {
    return(NULL)
  }

  # Combine executive summary and missing info into a readable block
  exec_sum <- if (nzchar(res$executive_summary[1])) res$executive_summary[1] else ""
  missing_info <- tryCatch(
    {
      jsonlite::fromJSON(res$missing_information[1])
    },
    error = function(e) character(0)
  )
  missing_txt <- if (length(missing_info) > 0) paste("Missing items:", paste(missing_info, collapse = ", ")) else ""

  paste(exec_sum, missing_txt, sep = "\n\n")
}

# Store or update a protocol entry for a study code
upsert_protocol <- function(study_code, executive_summary, missing_information) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))

  missing_json <- jsonlite::toJSON(missing_information, auto_unbox = TRUE)
  DBI::dbExecute(con, "INSERT OR REPLACE INTO protocols (study_code, executive_summary, missing_information) VALUES (?, ?, ?)",
    params = list(study_code, executive_summary, missing_json)
  )
}


# Physical CPU cores detection
N_CORES <- local({
  n <- tryCatch(parallel::detectCores(logical = FALSE), error = function(e) NA_integer_)
  if (is.na(n) || n < 1L) 4L else max(4L, n)
})

# CPU Throughput estimates
MODEL_TPS <- list(
  "qwen2.5:0.5b" = 80,
  "qwen2.5:1.5b" = 45,
  "llama3.2:1b" = 50,
  "smollm2:1.7b" = 40,
  "gemma2:2b" = 35,
  "qwen2.5:3b" = 28,
  "llama3.2:3b" = 26,
  "phi3.5:3.8b" = 22,
  "deepseek-r1:1.5b" = 20,
  "llama3.1:8b" = 11,
  "qwen2.5:7b" = 11,
  "mistral:7b" = 11,
  "_default_" = 18
)

estimate_eta <- function(n_chars, model, fast = FALSE, n_verify = 0L) {
  tps <- MODEL_TPS[[model]] %||% MODEL_TPS[["_default_"]]
  is_cloud <- grepl("-cloud$", model)
  trim_max <- if (is_cloud) 600000 else if (isTRUE(fast)) 12000 else 24000
  out_max <- if (isTRUE(fast)) 600 else 1100
  input_tokens <- ceiling(min(n_chars, trim_max) / 4) + 400
  total_tokens <- input_tokens + out_max
  if (grepl("^deepseek-r1", model)) total_tokens <- total_tokens + 2000
  # Each numeric-verification chunk is a separate call (~2.8k in, ~600 out)
  verify_tokens <- as.integer(n_verify) * 3400L
  secs <- ceiling((total_tokens + verify_tokens) / tps)
  list(
    seconds = max(secs, 5L),
    note = sprintf(
      "~%s tokens at %d t/s%s%s",
      format(total_tokens + verify_tokens, big.mark = ","), tps,
      if (is_cloud) " (cloud)" else " on CPU",
      if (n_verify > 0) sprintf(" - includes %d table-verification passes", n_verify) else ""
    )
  )
}

# Extract paragraph texts straight from the .docx XML, preserving tabs and the
# field-result runs (Word stores TOC page numbers as text runs). officer's
# docx_summary drops these, so a TOC like "1.<tab>TITLE<tab>4" loses its "4".
# Reading word/document.xml directly recovers them, which is what the page
# locator needs. Returns a character vector of paragraph strings.
docx_paragraph_texts <- function(path) {
  # Read word/document.xml straight out of the .docx zip (base R, no xml2)
  con <- tryCatch(unz(path, "word/document.xml", open = "rb"), error = function(e) NULL)
  if (is.null(con)) {
    return(character(0))
  }
  raw <- tryCatch(readBin(con, "raw", n = 8e7), error = function(e) NULL)
  try(close(con), silent = TRUE)
  if (is.null(raw) || length(raw) == 0) {
    return(character(0))
  }
  xml <- tryCatch(
    {
      x <- rawToChar(raw)
      Encoding(x) <- "UTF-8"
      x
    },
    error = function(e) NULL
  )
  if (is.null(xml) || !nzchar(xml)) {
    return(character(0))
  }

  # Drop field codes / tracked deletions, turn tabs into \t and paragraph ends
  # into newlines, then strip ALL tags (this removes opening <w:p ...> tags with
  # their attributes too) and unescape XML entities. One line per paragraph.
  xml <- gsub("(?s)<w:instrText[^>]*>.*?</w:instrText>", "", xml, perl = TRUE)
  xml <- gsub("(?s)<w:delText[^>]*>.*?</w:delText>", "", xml, perl = TRUE)
  xml <- gsub("<w:tab[ /][^>]*>", "\t", xml, perl = TRUE)
  xml <- gsub("<w:tab/>", "\t", xml, fixed = TRUE)
  xml <- gsub("</w:p>", "\n", xml, fixed = TRUE)
  xml <- gsub("<[^>]+>", "", xml, perl = TRUE)
  xml <- gsub("&lt;", "<", xml, fixed = TRUE)
  xml <- gsub("&gt;", ">", xml, fixed = TRUE)
  xml <- gsub("&quot;", "\"", xml, fixed = TRUE)
  xml <- gsub("&apos;", "'", xml, fixed = TRUE)
  xml <- gsub("&amp;", "&", xml, fixed = TRUE)
  strsplit(xml, "\n", fixed = TRUE)[[1]]
}

# ---------------------------------------------------------------------------
# Structured .docx text: paragraphs AND tables, in document order, with tables
# rendered as pipe-delimited rows.
#
# Why this matters: officer's docx_summary returns table cells as a flat stream
# of strings, so "88%" arrives with no indication of which column ("8 Hours")
# or row ("helps control body odour") it belongs to. The model then cannot tell
# that the narrative claiming "88% at T+30 minutes" contradicts the table.
# Keeping the grid intact is what makes narrative-vs-table cross-checking work.
# ---------------------------------------------------------------------------
docx_structured_text <- function(path) {
  con <- tryCatch(unz(path, "word/document.xml", open = "rb"), error = function(e) NULL)
  if (is.null(con)) {
    return("")
  }
  raw <- tryCatch(readBin(con, "raw", n = 8e7), error = function(e) NULL)
  try(close(con), silent = TRUE)
  if (is.null(raw) || length(raw) == 0) {
    return("")
  }
  xml <- tryCatch(
    {
      x <- rawToChar(raw)
      Encoding(x) <- "UTF-8"
      x
    },
    error = function(e) NULL
  )
  if (is.null(xml) || !nzchar(xml)) {
    return("")
  }

  # Strip field codes / tracked deletions up front
  xml <- gsub("(?s)<w:instrText[^>]*>.*?</w:instrText>", "", xml, perl = TRUE)
  xml <- gsub("(?s)<w:delText[^>]*>.*?</w:delText>", "", xml, perl = TRUE)

  unescape <- function(s) {
    s <- gsub("&lt;", "<", s, fixed = TRUE)
    s <- gsub("&gt;", ">", s, fixed = TRUE)
    s <- gsub("&quot;", "\"", s, fixed = TRUE)
    s <- gsub("&apos;", "'", s, fixed = TRUE)
    gsub("&amp;", "&", s, fixed = TRUE)
  }
  # Plain text of one XML fragment (tabs -> space, tags removed)
  frag_text <- function(s) {
    s <- gsub("<w:tab[ /][^>]*>", " ", s, perl = TRUE)
    s <- gsub("<w:tab/>", " ", s, fixed = TRUE)
    s <- gsub("<[^>]+>", "", s, perl = TRUE)
    trimws(gsub("[ \t]+", " ", unescape(s)))
  }

  # Walk body blocks in order. Tables are listed before paragraphs in the
  # alternation so a <w:tbl> consumes its inner <w:p> elements as one block.
  m <- gregexpr("(?s)<w:tbl>.*?</w:tbl>|<w:p[ >].*?</w:p>", xml, perl = TRUE)[[1]]
  if (length(m) == 1 && m[1] == -1) {
    return("")
  }
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

# Document parsing
read_doc <- function(path, ext) {
  ext <- tolower(ext)
  if (ext == "pdf") {
    paste(pdftools::pdf_text(path), collapse = "\n\n")
  } else if (ext == "docx") {
    # Prefer the structured reader (keeps table grids); fall back to officer.
    txt <- tryCatch(docx_structured_text(path), error = function(e) "")
    if (nzchar(txt)) {
      txt
    } else {
      s <- officer::docx_summary(officer::read_docx(path))
      paste(s$text[nzchar(s$text)], collapse = "\n")
    }
  } else if (ext %in% c("xlsx", "xls")) {
    # Read Excel files; assume first sheet and convert to plain text
    df <- readxl::read_excel(path, sheet = 1, .name_repair = "unique")
    paste(apply(df, 1, function(row) paste(row, collapse = " ")), collapse = "\n")
  } else if (ext == "txt") {
    paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  } else {
    stop("Unsupported file type: .", ext)
  }
}

# ---------------------------------------------------------------------------
# Apply selected fixes to a COPY of the original Word document.
# Uses officer find/replace on the document body. SQLite/officer cannot
# guarantee a match when Word splits a phrase across runs, so every change is
# verified (occurrence count must drop) and a transparent "Change Log" is
# appended listing what was applied and what could not be auto-located.
# Returns the officer doc plus counts. Never modifies the source file.
# ---------------------------------------------------------------------------
build_corrected_docx <- function(src_path, fixes) {
  doc <- officer::read_docx(src_path)

  count_occ <- function(d, s) {
    if (!nzchar(s)) {
      return(0L)
    }
    txt <- paste(officer::docx_summary(d)$text, collapse = "\n")
    sum(lengths(regmatches(txt, gregexpr(s, txt, fixed = TRUE))))
  }

  applied <- list()
  failed <- list()
  for (f in fixes) {
    old <- f$original_text %||% ""
    new <- f$corrected_text %||% ""
    if (!nzchar(old)) {
      failed[[length(failed) + 1]] <- f
      next
    }

    before_n <- count_occ(doc, old)
    ok <- FALSE
    if (before_n > 0) {
      doc <- tryCatch(
        officer::body_replace_all_text(doc,
          old_value = old, new_value = new,
          only_at_cursor = FALSE, warn = FALSE,
          fixed = TRUE
        ),
        error = function(e) doc
      )
      ok <- count_occ(doc, old) < before_n
    }
    if (ok) applied[[length(applied) + 1]] <- f else failed[[length(failed) + 1]] <- f
  }

  # Add a paragraph, trying the requested style but falling back gracefully if
  # the uploaded document's template doesn't define that style name.
  add_par <- function(d, txt, style = NULL) {
    if (!is.null(style)) {
      out <- tryCatch(officer::body_add_par(d, txt, style = style), error = function(e) NULL)
      if (!is.null(out)) {
        return(out)
      }
    }
    tryCatch(officer::body_add_par(d, txt), error = function(e) d)
  }

  # Append change log on a fresh page
  doc <- tryCatch(officer::body_add_break(doc), error = function(e) doc)
  doc <- add_par(doc, "Mascot Spincontrol QA - Change Log", style = "heading 1")
  doc <- add_par(
    doc,
    sprintf(
      "Generated %s | %d change(s) applied, %d could not be auto-located.",
      format(Sys.time(), "%Y-%m-%d %H:%M:%S"), length(applied), length(failed)
    )
  )

  if (length(applied) > 0) {
    doc <- add_par(doc, "Applied changes", style = "heading 2")
    for (f in applied) {
      doc <- add_par(doc, sprintf("[%s] %s", toupper(f$severity %||% ""), f$location %||% ""))
      doc <- add_par(doc, sprintf(
        '   "%s"  ->  "%s"',
        f$original_text %||% "", f$corrected_text %||% ""
      ))
    }
  }

  if (length(failed) > 0) {
    doc <- add_par(doc, "Could not auto-locate (apply manually)", style = "heading 2")
    for (f in failed) {
      doc <- add_par(doc, sprintf(
        "[%s] %s: %s", toupper(f$severity %||% ""),
        f$location %||% "", f$what_is_wrong %||% ""
      ))
      if (nzchar(f$corrected_text %||% "")) {
        doc <- add_par(doc, sprintf('   Suggested: "%s"', f$corrected_text))
      }
    }
  }

  list(doc = doc, n_applied = length(applied), n_failed = length(failed))
}

# Ollama models API
ollama_models <- function() {
  r <- tryCatch(
    httr2::request(paste0(OLLAMA_URL, "/api/tags")) |>
      httr2::req_timeout(5) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(r)) {
    return(character(0))
  }
  b <- httr2::resp_body_json(r)
  vapply(b$models %||% list(), function(m) m$name, character(1))
}

# JSON Clean up
clean_json_text <- function(raw) {
  if (is.null(raw) || !nzchar(raw)) {
    return("")
  }
  # BOM / zero-width characters some models emit
  raw <- gsub("﻿|​|‌|‍", "", raw)
  # Reasoning blocks: closed OR unclosed (truncated) <think>/<thinking> tags
  raw <- gsub("(?s)<think(?:ing)?>.*?(?:</think(?:ing)?>|$)", "", raw, perl = TRUE)
  raw <- gsub("(?s)```(?:json|JSON)?\\s*", "", raw, perl = TRUE)
  raw <- gsub("```", "", raw, fixed = TRUE)
  first <- regexpr("\\{", raw)
  last <- max(gregexpr("\\}", raw)[[1]])
  if (first > 0 && last > first) {
    raw <- substr(raw, first, last)
  }
  trimws(raw)
}

# ---------------------------------------------------------------------------
# Tolerant JSON repair pipeline.
# Models other than the well-behaved ones produce: single-quoted strings,
# unquoted keys/values, smart quotes, Python literals (True/None), raw
# newlines inside strings, trailing commas, and output truncated mid-string
# by the token budget. Each helper below fixes one class of damage; the
# parse_model_json() driver tries progressively stronger repairs until
# jsonlite accepts the result.
# ---------------------------------------------------------------------------

# Escape raw control characters that appear INSIDE double-quoted strings
# (invalid JSON, common when models put real newlines in summaries).
escape_ctrl_in_strings <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  n <- length(chars)
  buf <- character(n + 64L)
  j <- 0L
  in_str <- FALSE
  esc <- FALSE
  for (ch in chars) {
    if (in_str) {
      if (esc) {
        j <- j + 1L
        buf[j] <- ch
        esc <- FALSE
        next
      }
      if (ch == "\\") {
        j <- j + 1L
        buf[j] <- ch
        esc <- TRUE
        next
      }
      if (ch == "\"") {
        in_str <- FALSE
        j <- j + 1L
        buf[j] <- ch
        next
      }
      if (ch == "\n") {
        j <- j + 1L
        buf[j] <- "\\n"
        next
      }
      if (ch == "\r") next
      if (ch == "\t") {
        j <- j + 1L
        buf[j] <- "\\t"
        next
      }
      j <- j + 1L
      buf[j] <- ch
    } else {
      if (ch == "\"") in_str <- TRUE
      j <- j + 1L
      buf[j] <- ch
    }
  }
  paste(buf[seq_len(j)], collapse = "")
}

# Light repairs: smart quotes, control chars in strings, trailing commas.
repair_json_light <- function(x) {
  x <- gsub("[“”„«»]", "\"", x)
  x <- gsub("[‘’‚]", "'", x)
  x <- escape_ctrl_in_strings(x)
  x <- gsub(",\\s*([}\\]])", "\\1", x, perl = TRUE)
  x
}

# Convert single-quoted strings to valid double-quoted JSON strings.
# A ' opens a string only when the previous significant character is a
# structural one (: , [ {); a ' closes it only when followed by a delimiter,
# so apostrophes inside text survive. Escaped \' becomes a plain apostrophe
# and embedded " gets escaped.
convert_single_quoted_strings <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  n <- length(chars)
  out <- character(n + 16L)
  j <- 0L
  i <- 1L
  in_dbl <- FALSE
  esc <- FALSE
  prev_sig <- ""
  is_ws <- function(ch) ch == " " || ch == "\t" || ch == "\n" || ch == "\r"
  while (i <= n) {
    ch <- chars[i]
    if (in_dbl) {
      j <- j + 1L
      out[j] <- ch
      if (esc) {
        esc <- FALSE
      } else if (ch == "\\") {
        esc <- TRUE
      } else if (ch == "\"") in_dbl <- FALSE
      i <- i + 1L
      next
    }
    if (ch == "\"") {
      in_dbl <- TRUE
      j <- j + 1L
      out[j] <- ch
      i <- i + 1L
      next
    }
    if (ch == "'" && prev_sig %in% c(":", ",", "[", "{")) {
      j <- j + 1L
      out[j] <- "\""
      k <- i + 1L
      while (k <= n) {
        c2 <- chars[k]
        if (c2 == "\\" && k < n && chars[k + 1L] == "'") {
          j <- j + 1L
          out[j] <- "'"
          k <- k + 2L
          next
        }
        if (c2 == "\\") {
          j <- j + 1L
          out[j] <- "\\"
          if (k < n) {
            j <- j + 1L
            out[j] <- chars[k + 1L]
            k <- k + 2L
          } else {
            k <- k + 1L
          }
          next
        }
        if (c2 == "\"") {
          j <- j + 1L
          out[j] <- "\\\""
          k <- k + 1L
          next
        }
        if (c2 == "'") {
          m <- k + 1L
          while (m <= n && is_ws(chars[m])) m <- m + 1L
          if (m > n || chars[m] %in% c(",", "}", "]", ":")) break
          j <- j + 1L
          out[j] <- "'"
          k <- k + 1L
          next
        }
        j <- j + 1L
        out[j] <- c2
        k <- k + 1L
      }
      j <- j + 1L
      out[j] <- "\""
      i <- k + 1L
      prev_sig <- "\""
      next
    }
    j <- j + 1L
    out[j] <- ch
    if (!is_ws(ch)) prev_sig <- ch
    i <- i + 1L
  }
  paste(out[seq_len(j)], collapse = "")
}

# Replace every double-quoted string with a placeholder token so structural
# regexes can run without damaging string contents; then restore.
mask_json_strings <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  n <- length(chars)
  buf <- character(n)
  j <- 0L
  strings <- character(0)
  i <- 1L
  while (i <= n) {
    ch <- chars[i]
    if (ch == "\"") {
      k <- i + 1L
      esc <- FALSE
      from <- k
      while (k <= n) {
        c2 <- chars[k]
        if (esc) {
          esc <- FALSE
        } else if (c2 == "\\") {
          esc <- TRUE
        } else if (c2 == "\"") break
        k <- k + 1L
      }
      strings <- c(strings, if (k > from) paste(chars[from:(k - 1L)], collapse = "") else "")
      j <- j + 1L
      buf[j] <- sprintf("%d", length(strings))
      i <- k + 1L
    } else {
      j <- j + 1L
      buf[j] <- ch
      i <- i + 1L
    }
  }
  list(skeleton = paste(buf[seq_len(j)], collapse = ""), strings = strings)
}

unmask_json_strings <- function(sk, strings) {
  for (i in seq_along(strings)) {
    sk <- sub(sprintf("%d", i),
      paste0("\"", strings[i], "\""),
      sk,
      fixed = TRUE
    )
  }
  sk
}

# Heavy repairs: single->double quotes, quote unquoted keys and bareword
# values, normalize Python/JS literals. String contents are masked first so
# the regexes cannot corrupt them.
repair_json_heavy <- function(x) {
  x <- convert_single_quoted_strings(x)
  masked <- mask_json_strings(x)
  sk <- masked$skeleton
  # unquoted keys:  { key :  /  , key :
  sk <- gsub("([,{\\[]\\s*)([A-Za-z_][A-Za-z0-9_]*)\\s*:", "\\1\"\\2\":", sk, perl = TRUE)
  sk <- gsub("^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*:", "\"\\1\":", sk, perl = TRUE)
  # Python / JS literals
  sk <- gsub("\\bTrue\\b", "true", sk)
  sk <- gsub("\\bFalse\\b", "false", sk)
  sk <- gsub("\\bNone\\b|\\bNaN\\b|\\bundefined\\b", "null", sk, perl = TRUE)
  # bareword values:  : critical ,   ->  : "critical",
  sk <- gsub(":\\s*(?!\\s*(?:true|false|null)\\b)(?!\\s*[-0-9\"\\{\\[])\\s*([A-Za-z][^,}\\]]*?)\\s*(?=[,}\\]]|$)",
    ": \"\\1\"", sk,
    perl = TRUE
  )
  sk <- gsub(",\\s*([}\\]])", "\\1", sk, perl = TRUE)
  unmask_json_strings(sk, masked$strings)
}

# Close truncated output: terminate an unfinished string, drop a dangling
# partial key/value fragment, then append the missing closers in order.
close_truncated_json <- function(x) {
  chars <- strsplit(x, "", fixed = TRUE)[[1]]
  in_str <- FALSE
  esc <- FALSE
  stack <- character(0)
  for (ch in chars) {
    if (in_str) {
      if (esc) {
        esc <- FALSE
      } else if (ch == "\\") {
        esc <- TRUE
      } else if (ch == "\"") in_str <- FALSE
    } else {
      if (ch == "\"") {
        in_str <- TRUE
      } else if (ch == "{") {
        stack <- c(stack, "}")
      } else if (ch == "[") {
        stack <- c(stack, "]")
      } else if ((ch == "}" || ch == "]") && length(stack) > 0) stack <- stack[-length(stack)]
    }
  }
  if (in_str) x <- paste0(x, "\"")
  x <- sub("[,:]\\s*$", "", x)
  x <- sub(",\\s*\"[^\"]*\"\\s*:?\\s*$", "", x)
  if (length(stack) > 0) x <- paste0(x, paste(rev(stack), collapse = ""))
  x
}

# Driver: try progressively stronger repairs until jsonlite parses.
parse_model_json <- function(raw) {
  base <- clean_json_text(raw)
  if (!nzchar(base)) {
    return(NULL)
  }
  try_json <- function(x) {
    if (is.null(x) || !nzchar(x)) {
      return(NULL)
    }
    out <- tryCatch(jsonlite::fromJSON(x, simplifyVector = TRUE), error = function(e) NULL)
    if (!is.null(out) && is.list(out)) out else NULL
  }
  light <- tryCatch(repair_json_light(base), error = function(e) base)
  heavy <- tryCatch(repair_json_heavy(light), error = function(e) light)
  candidates <- list(
    base,
    light,
    heavy,
    tryCatch(close_truncated_json(light), error = function(e) NULL),
    tryCatch(close_truncated_json(heavy), error = function(e) NULL)
  )
  for (cand in candidates) {
    out <- try_json(cand)
    if (!is.null(out)) {
      return(out)
    }
  }
  NULL
}

# Coerce whatever a model produced into the exact shape the app expects,
# so near-miss outputs (string scores, list-form issues, vector summaries)
# render instead of erroring downstream.
normalize_review <- function(out) {
  if (is.null(out) || !is.list(out)) {
    return(NULL)
  }

  sc <- suppressWarnings(as.integer(as.character(out$overall_score %||% 0)[1]))
  if (is.na(sc)) sc <- 0L
  out$overall_score <- max(0L, min(100L, sc))

  rl <- trimws(as.character(out$risk_level %||% "Unknown")[1])
  std <- c(low = "Low", medium = "Medium", high = "High", critical = "Critical")
  hit <- std[tolower(rl)]
  out$risk_level <- if (!is.na(hit)) unname(hit) else if (nzchar(rl)) rl else "Unknown"

  out$executive_summary <- paste(as.character(unlist(out$executive_summary %||% "")),
    collapse = " "
  )

  issue_cols <- c(
    "severity", "category", "location", "what_is_wrong",
    "suggested_fix", "original_text", "corrected_text"
  )
  iss <- out$issues
  if (!is.null(iss) && !is.data.frame(iss)) {
    if (is.list(iss) && length(iss) > 0) {
      rows <- lapply(iss, function(r) {
        if (!is.list(r)) {
          return(NULL)
        }
        vals <- lapply(issue_cols, function(cl) {
          paste(as.character(unlist(r[[cl]] %||% "")), collapse = "; ")
        })
        names(vals) <- issue_cols
        as.data.frame(vals, stringsAsFactors = FALSE)
      })
      rows <- rows[!vapply(rows, is.null, logical(1))]
      iss <- if (length(rows) > 0) do.call(rbind, rows) else NULL
    } else {
      iss <- NULL
    }
  }
  if (is.data.frame(iss)) {
    for (cl in issue_cols) if (is.null(iss[[cl]])) iss[[cl]] <- ""
    for (cl in names(iss)) {
      if (is.list(iss[[cl]])) {
        iss[[cl]] <- vapply(iss[[cl]], function(v) {
          paste(as.character(unlist(v)), collapse = "; ")
        }, character(1))
      }
      iss[[cl]][is.na(iss[[cl]])] <- ""
    }
    if (nrow(iss) == 0) iss <- NULL
  }
  out$issues <- iss

  mi <- out$missing_information
  out$missing_information <- if (is.null(mi)) {
    list()
  } else {
    as.list(as.character(unlist(mi)))
  }
  out
}

# Robust JSON Fallback Parser to handle loose structures (unquoted keys/values from models like minimax)
robust_json_parser <- function(raw) {
  cleaned <- clean_json_text(raw)

  res <- list(
    overall_score = 0,
    risk_level = "Unknown",
    executive_summary = "",
    issues = NULL,
    missing_information = list()
  )

  text <- cleaned
  text <- gsub("^\\{\\s*", "", text)
  text <- gsub("\\s*\\}$", "", text)

  # 1. overall_score
  score_match <- regexec("overall_score\\s*:\\s*([0-9]+)", text)
  score_matches <- regmatches(text, score_match)[[1]]
  if (length(score_matches) >= 2) {
    res$overall_score <- as.integer(score_matches[2])
  }

  # 2. risk_level
  risk_match <- regexec("risk_level\\s*:\\s*([a-zA-Z\"']+)", text)
  risk_matches <- regmatches(text, risk_match)[[1]]
  if (length(risk_matches) >= 2) {
    res$risk_level <- gsub("[\"']", "", risk_matches[2])
  }

  # 3. issues array
  issues_start <- regexpr("issues\\s*:\\s*\\[", text)
  if (issues_start > 0) {
    brackets_content <- substr(text, issues_start + attr(issues_start, "match.length"), nchar(text))
    depth <- 1
    bracket_pos <- 0
    chars <- strsplit(brackets_content, "")[[1]]
    for (i in seq_along(chars)) {
      if (chars[i] == "[") depth <- depth + 1
      if (chars[i] == "]") depth <- depth - 1
      if (depth == 0) {
        bracket_pos <- i
        break
      }
    }
    if (bracket_pos > 0) {
      issues_str <- substr(brackets_content, 1, bracket_pos - 1)
      matches_indices <- gregexpr("\\{[^\\}]+\\}", issues_str)
      matches <- regmatches(issues_str, matches_indices)[[1]]

      issues_df <- data.frame(
        severity = character(0),
        category = character(0),
        location = character(0),
        what_is_wrong = character(0),
        suggested_fix = character(0),
        original_text = character(0),
        corrected_text = character(0),
        stringsAsFactors = FALSE
      )

      for (m in matches) {
        get_field <- function(f_name, block_text) {
          m_dbl <- regexec(paste0(f_name, "\\s*:\\s*\"(.*?)\""), block_text)
          res_dbl <- regmatches(block_text, m_dbl)[[1]]
          if (length(res_dbl) >= 2) {
            return(res_dbl[2])
          }

          m_sgl <- regexec(paste0(f_name, "\\s*:\\s*'(.*?)'"), block_text)
          res_sgl <- regmatches(block_text, m_sgl)[[1]]
          if (length(res_sgl) >= 2) {
            return(res_sgl[2])
          }

          m_unq <- regexec(paste0(f_name, "\\s*:\\s*([^,\\}]+)"), block_text)
          res_unq <- regmatches(block_text, m_unq)[[1]]
          if (length(res_unq) >= 2) {
            return(trimws(res_unq[2]))
          }
          return("")
        }

        issue_obj <- list(
          severity = get_field("severity", m),
          category = get_field("category", m),
          location = get_field("location", m),
          what_is_wrong = get_field("what_is_wrong", m),
          suggested_fix = get_field("suggested_fix", m),
          original_text = get_field("original_text", m),
          corrected_text = get_field("corrected_text", m)
        )
        issues_df <- rbind(issues_df, as.data.frame(issue_obj, stringsAsFactors = FALSE))
      }
      res$issues <- issues_df
    }
  }

  # 4. missing_information
  missing_start <- regexpr("missing_information\\s*:\\s*\\[", text)
  if (missing_start > 0) {
    brackets_content <- substr(text, missing_start + attr(missing_start, "match.length"), nchar(text))
    depth <- 1
    bracket_pos <- 0
    chars <- strsplit(brackets_content, "")[[1]]
    for (i in seq_along(chars)) {
      if (chars[i] == "[") depth <- depth + 1
      if (chars[i] == "]") depth <- depth - 1
      if (depth == 0) {
        bracket_pos <- i
        break
      }
    }
    if (bracket_pos > 0) {
      missing_str <- substr(brackets_content, 1, bracket_pos - 1)
      matches_indices <- gregexpr("\"(.*?)\"|'(.*?)'", missing_str)
      matches <- regmatches(missing_str, matches_indices)[[1]]
      res$missing_information <- as.list(gsub("[\"']", "", matches))
    }
  }

  # 5. executive_summary
  exec_start <- regexpr("executive_summary\\s*:\\s*", text)
  if (exec_start > 0) {
    start_pos <- exec_start + attr(exec_start, "match.length")
    sub_text <- substr(text, start_pos, nchar(text))
    end_idx <- regexpr(",?\\s*(issues|missing_information)\\s*:", sub_text)
    if (end_idx > 0) {
      exec_val <- substr(sub_text, 1, end_idx - 1)
    } else {
      exec_val <- sub_text
    }
    exec_val <- trimws(exec_val)
    if (startsWith(exec_val, "\"") && endsWith(exec_val, "\"")) {
      exec_val <- substr(exec_val, 2, nchar(exec_val) - 1)
    } else if (startsWith(exec_val, "'") && endsWith(exec_val, "'")) {
      exec_val <- substr(exec_val, 2, nchar(exec_val) - 1)
    }
    res$executive_summary <- trimws(exec_val)
  }

  # Accept the result if ANY meaningful field was recovered
  if (nzchar(res$executive_summary) || NROW(res$issues) > 0 ||
    res$overall_score > 0 || !identical(res$risk_level, "Unknown")) {
    return(res)
  }
  return(NULL)
}

# Last-resort recovery: ask the model itself to re-emit its broken output as
# strictly valid JSON. One extra short call, made only when all local parsing
# has failed, so well-behaved models never pay this cost.
ollama_fix_json <- function(broken_text, model, timeout_sec = 180) {
  prompt <- paste0(
    "The following text was supposed to be a single valid JSON object but is malformed or truncated. ",
    "Rewrite it as ONE strictly valid JSON object with exactly these keys: ",
    "overall_score (integer), risk_level (string), executive_summary (string), ",
    "issues (array of objects with keys: severity, category, location, what_is_wrong, ",
    "suggested_fix, original_text, corrected_text), missing_information (array of strings). ",
    "Preserve the content faithfully. Do NOT invent new findings. ",
    "If the final item is incomplete, drop it. Output ONLY the JSON object.\n\n",
    "MALFORMED TEXT:\n\"\"\"\n", substr(broken_text, 1, 8000), "\n\"\"\"\n"
  )
  resp <- tryCatch(
    {
      httr2::request(paste0(OLLAMA_URL, "/api/generate")) |>
        httr2::req_method("POST") |>
        httr2::req_body_json(list(
          model = model, prompt = prompt, stream = FALSE,
          format = "json", keep_alive = "1h",
          options = list(
            temperature = 0, num_ctx = 8192L,
            num_predict = 2048L, num_thread = N_CORES
          )
        ), auto_unbox = TRUE) |>
        httr2::req_timeout(timeout_sec) |>
        httr2::req_perform()
    },
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(NULL)
  }
  b <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  if (is.null(b)) {
    return(NULL)
  }
  b$response %||% NULL
}

# Generate unique SHA-256 hash for run parameters
calculate_run_hash <- function(text, model, doc_type, guidelines, fast) {
  combined <- paste(text, model, doc_type, guidelines, as.character(fast), sep = "|||")
  digest::digest(combined, algo = "sha256", serialize = FALSE)
}

# Fetch from cache
get_cached_review <- function(hash_val) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))
  res <- tryCatch(
    {
      DBI::dbGetQuery(con, "SELECT response_json, elapsed_s FROM execution_cache WHERE hash = ?", params = list(hash_val))
    },
    error = function(e) NULL
  )
  if (is.null(res) || nrow(res) == 0) {
    return(NULL)
  }
  list(
    data = jsonlite::fromJSON(res$response_json[1]),
    elapsed_s = res$elapsed_s[1],
    cached = TRUE
  )
}

# Save to cache
save_cached_review <- function(hash_val, response_json, elapsed_s) {
  con <- DBI::dbConnect(RSQLite::SQLite(), DB_PATH)
  on.exit(DBI::dbDisconnect(con))
  DBI::dbExecute(con, "INSERT OR REPLACE INTO execution_cache (hash, response_json, elapsed_s, created_at) VALUES (?, ?, ?, ?)",
    params = list(hash_val, response_json, elapsed_s, format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  )
}

ollama_warmup <- function(model) {
  tryCatch(
    {
      body <- list(
        model = model, prompt = "Hi", stream = FALSE,
        keep_alive = "1h",
        options = list(num_predict = 1L, num_thread = N_CORES)
      )
      httr2::request(paste0(OLLAMA_URL, "/api/generate")) |>
        httr2::req_method("POST") |>
        httr2::req_body_json(body, auto_unbox = TRUE) |>
        httr2::req_timeout(120) |>
        httr2::req_perform()
    },
    error = function(e) invisible(NULL)
  )
  invisible(NULL)
}

# Highly pickier, expert-level prompt
ollama_review <- function(text, model, doc_type, custom_guidelines = "", fast = FALSE, timeout_sec = 600) {
  # Cloud-hosted models (name ends in "-cloud") have very large context windows,
  # so sending the whole document is both feasible and necessary for recall:
  # truncating to head+tail silently hides every issue in the middle of a long
  # report (methods sections, mid-document tables, etc.). Only fall back to the
  # conservative head+tail trim for small local models.
  is_cloud <- grepl("-cloud$", model)
  trim_max <- if (is_cloud) 600000L else if (isTRUE(fast)) 12000L else 24000L
  if (nchar(text) > trim_max) {
    half <- trim_max %/% 2
    text <- paste0(
      substr(text, 1, half),
      "\n\n[...middle truncated for length...]\n\n",
      substr(text, nchar(text) - half + 1, nchar(text))
    )
  }

  # 1. STATIC PREFIX (Always identical across runs - 100% CACHED BY OLLAMA)
  system_msg <- paste(
    "You are a Senior Quality Assurance Auditor with 25+ years of experience reviewing clinical study protocols,",
    "patch test reports, HRIPT reports, SPF studies, cosmetic claims substantiation reports and scientific",
    "documents at Mascot Spincontrol Pvt. Ltd. Your standard is absolute clarity, complete regulatory",
    "compliance (ICH-GCP, FDA, and ALCOA+ standards), and meticulous technical consistency.",
    "You base your audit strictly on the provided text without speculating."
  )

  # Place static rules, criteria and JSON schema at the start of the prompt
  prompt_prefix <- paste0(
    "YOUR ONLY JOB IS TO FIND ERRORS in the document provided at the end of this prompt.

Do NOT summarize (except inside executive_summary).
Do NOT explain your reasoning.
Do NOT praise the document.

Before generating output perform 5 independent review passes:

PASS 1:
Spelling Audit

PASS 2:
Grammar Audit

PASS 3:
Consistency Audit

PASS 4:
Regulatory Compliance Audit

PASS 5:
Statistical Audit

Collect findings from ALL passes.

CHECK FOR:

1. Spelling mistakes
2. Grammar mistakes
3. Missing words
4. Duplicate words
5. Wrong punctuation
6. Undefined abbreviations
7. Inconsistent abbreviations
8. Product name inconsistencies
9. Study code inconsistencies
10. Sample size inconsistencies
11. Timepoint inconsistencies
12. Table versus text inconsistencies
13. Missing sections
14. Missing methodology
15. Missing objective
16. Missing inclusion criteria
17. Missing exclusion criteria
18. Missing safety procedure
19. Missing adverse event reporting
20. Missing statistical methods
21. Incorrect statistical interpretation
22. Unsupported claims
23. Formatting inconsistencies
24. Date format inconsistencies
25. Unit inconsistencies

NUMERIC & CROSS-SECTION CONSISTENCY (HIGH PRIORITY - check this explicitly):
Track every key value wherever it appears - in the summary, methods, results tables, conclusion AND any appendix or protocol copy - and flag EVERY place two values disagree:
- Number of subjects / sample size stated in the text versus the N shown in the data tables (e.g. text says one number, tables show another).
- Age range of the panel stated in different sections (selection criteria vs recruited/exploited panel vs conclusion).
- Study duration and the number of timepoints (e.g. hours of application; whether all timepoints appear in every table).
- Result figures (percentages, means, p-values): the values quoted in the main body Discussion/Conclusion MUST match the values in the appendix / copy of the study protocol. Compare them number by number and flag any difference.
- A stated significance ('significant increase/decrease') that is contradicted by the p-values or the 'Significant at 5%' rows in the corresponding table.

HOW TABLES ARE PRESENTED:
Tables appear as pipe-delimited rows between [TABLE n] and [END TABLE n] markers. The first row(s) of a table are its column headers (e.g. timepoints such as 'T+30 mins', '8 Hours', 'T+14 days'). Read a value by pairing its position in the row with the matching header column.

NARRATIVE vs TABLE VERIFICATION (THIS IS THE HIGHEST PRIORITY - DO IT FIRST):
For EVERY sentence in the results/analysis/conclusion that quotes a number, you MUST find the corresponding table and verify it cell by cell. Report a critical or major issue for each mismatch:
1. VALUE MISMATCH - a percentage, score, mean, p-value or count in the text differs from the table cell it refers to. Example of the error to catch: the table row says '68%' at T+14 days, but the narrative says '71%'. Quote both numbers in what_is_wrong.
2. TIMEPOINT / COLUMN MISMATCH - the value is real but attributed to the WRONG column. Example: a row shows '88%' in the '8 Hours' column, but the narrative says '88% at T+30 minutes'. Check which column the number actually sits in before accepting the sentence.
3. A value quoted in the text that appears nowhere in the referenced table.
4. A table value that contradicts the stated significance ('significant increase/decrease' vs the p-value or 'Significant at 5%' cell).
5. Counts/N that disagree between the text and the table's N row.
Do this for every parameter section, one at a time. Never assume the narrative is right because it sounds plausible.

PARAMETER / LABEL CONSISTENCY (also high priority):
- The section heading, the narrative inside it, and the table's own caption/labels must all refer to the SAME parameter. Example of the error to catch: a section titled 'Skin Softness' whose table is headed 'Skin Smoothness' and whose rows say 'EVOLUTION OF THE SKIN SMOOTHNESS PARAMETERS'. Flag every such heading-vs-table-vs-text parameter mismatch - it is a critical data-integrity error, not a typo.
- A question or item wording in a results table must match the wording used for it in the narrative and in the questionnaire.
- Check that the direction stated ('increase' / 'decrease') matches the sign of the table values.

CLAIMS & PLACEHOLDERS:
- Proof-read claim statements and marketing/claims sections word by word; they are short and typos there are easy to miss (e.g. 'impovement' for 'improvement').
- Flag unresolved placeholders left in the text: 'X%', 'Rs. /-', 'TBD', 'XXX', empty amounts, blank dates or signature lines that should have been completed.

HEADINGS, TITLES & SPACING (check these explicitly - they are easy to miss):
- Scan every section heading and every Table of Contents entry, not just body paragraphs.
- Missing spaces / glued words: e.g. 'AMENDMENTSTO' should be 'AMENDMENTS TO', 'APPENDICE' should be 'APPENDICES', 'EVALUATION(ENGLISH)' should be 'EVALUATION (ENGLISH)'.
- Double spaces between words (e.g. 'study  will').
- Missing trailing letters or wrong plural in a heading (e.g. 'APPENDICE' vs 'APPENDICES').
- Inconsistent spacing/formatting of the SAME heading where it appears more than once.

DOCUMENT FURNITURE - do NOT report these as errors:
- Running headers/footers, the document title, study code or page numbers repeating on every page.
- Layout coordinate fragments or numeric position strings produced by text extraction.
Focus only on the authored content (sentences, tables, sections).

IMPORTANT:

- Every error must be reported separately.
- If the SAME error pattern recurs (e.g. a phrase like 'was be', a misspelling, a mixed spelling such as favour/favor), report EACH occurrence as its own issue with its own location. Never report a recurring error once and stop.
- If one sentence contains 5 errors then report 5 issues.
- Quote the exact original text: original_text must be copied character-for-character from the document, including punctuation and capitalization, so it can be located and replaced automatically.
- Provide the exact replacement in corrected_text.
- Leave original_text and corrected_text as empty strings when the issue is not a direct text substitution (e.g. a missing section).
- executive_summary: 2-4 sentences on the document's overall state and major risks. This is the ONLY place for summary.
- Never invent errors. Only flag issues clearly present in the document.
- Every issue must cite a specific section, heading, or paragraph in location (or 'unknown').
- If the document uses numbered sections, START location with that number exactly as printed, e.g. '3.1.1 PARTICIPANT SELECTION' or '4.9 ETHICS COMMITTEE'. Use the deepest number that applies. If a finding is outside any numbered section (title page, header, footer, an appendix), name that instead.
- Never assume missing information unless clearly absent.
- Be extremely strict.

SCORING:
overall_score starts at 100. Deduct 15 per critical issue, 7 per major issue, 2 per minor issue. Minimum 0.

Return ONLY valid JSON.

JSON SCHEMA:

{
  \"overall_score\": <integer 0-100 computed by the scoring rule>,
  \"risk_level\": \"Low|Medium|High|Critical\",
  \"executive_summary\": \"\",
  \"issues\": [
    {
      \"severity\": \"critical|major|minor\",
      \"category\": \"grammar|spelling|formatting|terminology|missing|inconsistency|statistics|compliance|clarity|other\",
      \"location\": \"\",
      \"what_is_wrong\": \"\",
      \"suggested_fix\": \"\",
      \"original_text\": \"\",
      \"corrected_text\": \"\"
    }
  ],
  \"missing_information\": []
}
"
  )

  # 2. DYNAMIC SUFFIX (Appended at the end - EVALUATED AT RUNTIME)
  prompt_suffix <- ""
  if (nzchar(trimws(custom_guidelines))) {
    prompt_suffix <- paste0(
      prompt_suffix,
      "MANDATORY MASCOT SPINCONTROL GUIDELINES TO ENFORCE:\n",
      "You MUST verify and enforce the following custom audit instructions. Flag any violation:\n",
      custom_guidelines, "\n\n"
    )
  }

  prompt_suffix <- paste0(
    prompt_suffix,
    "TARGET DOCUMENT TYPE TO AUDIT: ", doc_type, "\n\n",
    "DOCUMENT TO AUDIT:\n",
    "\"\"\"\n", text, "\n\"\"\"\n"
  )

  # Combine static prefix and dynamic suffix
  full_prompt <- paste0(prompt_prefix, prompt_suffix)

  out_budget <- if (isTRUE(fast)) 800L else 1800L
  # Long full-document audits produce many issues; give the model room to emit
  # them all (cloud models especially), otherwise the JSON is cut off mid-array.
  if (is_cloud) out_budget <- 8192L
  # Reasoning models burn output tokens on hidden "thinking" before emitting
  # the JSON; without extra room their answer gets truncated mid-object.
  if (grepl("^(deepseek-r1|qwq|openthinker|marco-o1|exaone-deep)", model)) {
    out_budget <- out_budget + 2048L
  }
  est_tokens <- ceiling(nchar(text) / 4) + out_budget + 128L
  ctx_raw <- 2^ceiling(log2(max(1024L, est_tokens)))
  # Cap context: small local models stay bounded for speed/RAM; cloud models
  # get a large window so the entire document fits.
  ctx_cap <- if (is_cloud) 131072L else if (isTRUE(fast)) 4096L else 8192L
  num_ctx_val <- min(ctx_raw, ctx_cap)

  body <- list(
    model = model,
    system = system_msg,
    prompt = full_prompt,
    stream = FALSE,
    format = "json",
    keep_alive = "1h",
    options = list(
      temperature = 0.1,
      num_ctx     = num_ctx_val,
      num_predict = out_budget,
      num_thread  = N_CORES,
      num_batch   = 512L,
      f16_kv      = TRUE
    )
  )

  resp <- httr2::request(paste0(OLLAMA_URL, "/api/generate")) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  raw <- parsed$response %||% ""

  cleaned <- clean_json_text(raw)

  # 1) Staged tolerant parsing: strict -> light repair -> heavy repair ->
  #    truncation repair (handles single quotes, unquoted keys, smart
  #    quotes, Python literals, raw newlines, cut-off output, think tags)
  out <- parse_model_json(raw)
  was_repaired <- FALSE

  # 2) Regex field-harvesting fallback
  if (is.null(out)) {
    out <- robust_json_parser(cleaned)
    if (!is.null(out)) was_repaired <- TRUE
  }

  # 3) Last resort: one extra call asking the model to fix its own JSON
  if (is.null(out)) {
    raw2 <- tryCatch(ollama_fix_json(raw, model), error = function(e) NULL)
    if (!is.null(raw2) && nzchar(raw2)) {
      out <- parse_model_json(raw2)
      if (is.null(out)) out <- robust_json_parser(clean_json_text(raw2))
      if (!is.null(out)) {
        was_repaired <- TRUE
        cleaned <- clean_json_text(raw2)
      }
    }
  }

  # Coerce whatever shape we got into what the UI/DB expect
  if (!is.null(out)) {
    out <- normalize_review(out)
    if (is.null(out)) was_repaired <- FALSE
  }

  # Capture detailed Ollama-reported execution timing
  total_duration <- (parsed$total_duration %||% 0) / 1e9
  load_duration <- (parsed$load_duration %||% 0) / 1e9
  prompt_eval_t <- (parsed$prompt_eval_duration %||% 0) / 1e9
  eval_duration <- (parsed$eval_duration %||% 0) / 1e9
  eval_count <- parsed$eval_count %||% 0

  # Calculate tokens per second (generation phase)
  tps <- if (eval_duration > 0) round(eval_count / eval_duration, 1) else 0

  list(
    ok = !is.null(out),
    data = out,
    raw_text = raw,
    cleaned_text = cleaned,
    elapsed_s = total_duration,
    repaired = was_repaired,
    stats = list(
      total_s = total_duration,
      load_s = load_duration,
      prompt_eval_s = prompt_eval_t,
      eval_s = eval_duration,
      eval_count = eval_count,
      tps = tps
    )
  )
}

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && is.na(a))) b else a
}

# ===========================================================================
# Deterministic pre-scan + section-aware deep scan
# ---------------------------------------------------------------------------
# A single LLM pass over a long document reliably surfaces the big issues but
# lets scattered mechanical errors (a stray duplicate word, a UK/US spelling
# mix) fall below its attention. Two countermeasures:
#   1. deterministic_findings(): rule-based, 100%-precision checks for the
#      boring-but-pervasive classes, merged into every result.
#   2. run_audit(... deep=TRUE): split the document into sections, audit each
#      so every line gets attention, plus one whole-doc consistency pass, then
#      merge + dedupe everything.
# ===========================================================================

ISSUE_COLS <- c(
  "severity", "category", "location", "what_is_wrong",
  "suggested_fix", "original_text", "corrected_text"
)

empty_issues <- function() {
  d <- data.frame(matrix(character(0), nrow = 0, ncol = length(ISSUE_COLS)),
    stringsAsFactors = FALSE
  )
  names(d) <- ISSUE_COLS
  d
}

mk_issue <- function(severity, category, location, what_is_wrong,
                     suggested_fix, original_text = "", corrected_text = "") {
  data.frame(
    severity = severity, category = category, location = location,
    what_is_wrong = what_is_wrong, suggested_fix = suggested_fix,
    original_text = original_text, corrected_text = corrected_text,
    stringsAsFactors = FALSE
  )
}

coerce_issues_df <- function(x) {
  if (is.null(x)) {
    return(empty_issues())
  }
  if (!is.data.frame(x)) {
    x <- tryCatch(as.data.frame(x, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(x)) {
      return(empty_issues())
    }
  }
  for (col in ISSUE_COLS) {
    if (is.null(x[[col]])) x[[col]] <- ""
    if (is.list(x[[col]])) {
      x[[col]] <- vapply(x[[col]], function(v) paste(as.character(unlist(v)), collapse = "; "), character(1))
    }
    x[[col]] <- as.character(x[[col]])
    x[[col]][is.na(x[[col]])] <- ""
  }
  x[, ISSUE_COLS, drop = FALSE]
}

# High-precision rule checks. Returns a standard issues data.frame.
deterministic_findings <- function(text) {
  out <- list()
  if (is.null(text) || !nzchar(text)) {
    return(empty_issues())
  }
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]

  # 1. Duplicate consecutive words (e.g. "at at", "the the")
  seen <- character(0)
  for (li in seq_along(lines)) {
    ln <- lines[li]
    mm <- regmatches(ln, gregexpr("\\b([A-Za-z]{2,})\\s+\\1\\b", ln, ignore.case = TRUE, perl = TRUE))[[1]]
    for (h in mm) {
      word <- sub("\\s+.*$", "", h)
      if (tolower(word) %in% c("had", "that")) next # legitimate doubles
      key <- tolower(gsub("\\s+", " ", h))
      if (key %in% seen) next
      seen <- c(seen, key)
      out[[length(out) + 1]] <- mk_issue(
        "minor", "grammar",
        sprintf("text (\"%s...\")", substr(trimws(ln), 1, 40)),
        sprintf("Duplicate consecutive word: \"%s\".", h),
        sprintf("Remove the repeated word (use \"%s\").", word),
        h, word
      )
      if (length(out) >= 60) break
    }
    if (length(out) >= 60) break
  }

  tl <- tolower(text)
  cnt <- function(w) {
    g <- gregexpr(paste0("\\b", w, "\\b"), tl, perl = TRUE)[[1]]
    if (length(g) == 1 && g[1] == -1) 0L else length(g)
  }

  # 2. Mixed UK/US spelling
  pairs <- list(
    c("favour", "favor"), c("colour", "color"), c("analyse", "analyze"),
    c("organisation", "organization"), c("standardise", "standardize"),
    c("randomisation", "randomization"), c("centre", "center"),
    c("litre", "liter"), c("odour", "odor"), c("behaviour", "behavior"),
    c("fibre", "fiber"), c("grey", "gray"), c("utilise", "utilize")
  )
  for (p in pairs) {
    a <- cnt(p[1])
    b <- cnt(p[2])
    if (a > 0 && b > 0) {
      out[[length(out) + 1]] <- mk_issue(
        "minor", "inconsistency", "throughout",
        sprintf("Mixed UK/US spelling: \"%s\" (%dx) and \"%s\" (%dx) both appear.", p[1], a, p[2], b),
        sprintf("Standardize to one spelling (\"%s\" or \"%s\") throughout.", p[1], p[2])
      )
    }
  }

  # 3. Inconsistent time-unit abbreviations
  for (p in list(c("mins", "minutes"), c("hrs", "hours"), c("sec", "seconds"))) {
    a <- cnt(p[1])
    b <- cnt(p[2])
    if (a > 0 && b > 0) {
      out[[length(out) + 1]] <- mk_issue(
        "minor", "terminology", "throughout",
        sprintf("Inconsistent unit: \"%s\" (%dx) and \"%s\" (%dx) both used.", p[1], a, p[2], b),
        sprintf("Use one form consistently (prefer \"%s\").", p[2])
      )
    }
  }

  # 4. Known misspellings. High-precision dictionary of forms that are always
  #    wrong in these reports, including ones models skim past in short claim
  #    lines (e.g. "impovement"). Matched case-insensitively, whole word.
  typos <- c(
    "impovement" = "improvement", "improvment" = "improvement",
    "imporvement" = "improvement", "impovment" = "improvement",
    "telephoic" = "telephonic", "comittee" = "committee",
    "commitee" = "committee", "committe" = "committee",
    "appendice" = "appendices", "amendmentsto" = "amendments to",
    "paitents" = "patients", "patinets" = "patients",
    "intitation" = "initiation", "photograpy" = "photography",
    "photograhy" = "photography", "balneatherapy" = "balneotherapy",
    "occurance" = "occurrence", "recieve" = "receive",
    "seperate" = "separate", "seperately" = "separately",
    "aged betwen" = "aged between",
    "witney" = "Whitney", "whitny" = "Whitney",
    "signficant" = "significant", "significanct" = "significant",
    "statisical" = "statistical", "statistcal" = "statistical",
    "evalaution" = "evaluation", "evalution" = "evaluation",
    "assesment" = "assessment", "asessment" = "assessment",
    "measurment" = "measurement", "mesurement" = "measurement",
    "tempreture" = "temperature", "temparature" = "temperature",
    "critera" = "criteria", "critieria" = "criteria",
    "protocal" = "protocol", "particpant" = "participant",
    "particpants" = "participants", "volunter" = "volunteer",
    "volunters" = "volunteers", "sponser" = "sponsor",
    "hygeine" = "hygiene", "erythama" = "erythema", "erythmea" = "erythema"
  )
  for (bad in names(typos)) {
    pat <- paste0("\\b", gsub(" ", "\\\\s+", bad), "\\b")
    for (li in seq_along(lines)) {
      ln <- lines[li]
      hit <- regmatches(ln, regexpr(pat, ln, ignore.case = TRUE, perl = TRUE))
      if (length(hit) == 0 || !nzchar(hit[1])) next
      # Never emit a no-op "X should be X" finding
      if (identical(tolower(trimws(hit[1])), tolower(trimws(typos[[bad]])))) next
      out[[length(out) + 1]] <- mk_issue(
        "major", "spelling",
        sprintf("text (\"%s...\")", substr(trimws(ln), 1, 40)),
        sprintf("Misspelling: \"%s\" should be \"%s\".", hit[1], typos[[bad]]),
        sprintf("Correct \"%s\" to \"%s\".", hit[1], typos[[bad]]),
        hit[1], typos[[bad]]
      )
      break # one report per misspelling form is enough
    }
  }

  # 5. Double spaces between words (LLMs routinely miss these)
  seen_ds <- character(0)
  for (li in seq_along(lines)) {
    ln <- lines[li]
    mm <- regmatches(ln, gregexpr("[A-Za-z]+  +[A-Za-z]+", ln, perl = TRUE))[[1]]
    for (h in mm) {
      if (h %in% seen_ds) next
      seen_ds <- c(seen_ds, h)
      out[[length(out) + 1]] <- mk_issue(
        "minor", "formatting",
        sprintf("text (\"%s...\")", substr(trimws(ln), 1, 40)),
        sprintf("Double space between words: \"%s\".", h),
        "Replace the double space with a single space.",
        h, gsub(" +", " ", h))
      if (length(out) >= 120) break
    }
    if (length(out) >= 120) break
  }

  if (length(out) == 0) {
    return(empty_issues())
  }
  do.call(rbind, out)
}

# Merge two issue sets and drop near-duplicates.
dedupe_issues <- function(df) {
  if (NROW(df) == 0) {
    return(df)
  }
  # Primary key: same category + same wording + same quoted text.
  key <- paste(tolower(trimws(df$category)),
    tolower(substr(trimws(df$what_is_wrong), 1, 60)),
    tolower(trimws(df$original_text)),
    sep = "||"
  )
  out <- df[!duplicated(key), , drop = FALSE]

  # Secondary pass: the model and the deterministic scan often report the SAME
  # spelling/formatting error in different words. Collapse those by category +
  # the offending text itself (or, when absent, the mentioned word), so the user
  # sees one row per real defect instead of two.
  mention <- vapply(seq_len(nrow(out)), function(i) {
    ot <- trimws(out$original_text[i] %||% "")
    if (nzchar(ot)) {
      return(tolower(ot))
    }
    w <- regmatches(
      out$what_is_wrong[i],
      regexpr("[\"'‘“]([^\"'’”]{2,40})[\"'’”]", out$what_is_wrong[i], perl = TRUE)
    )
    if (length(w) && nzchar(w[1])) tolower(gsub("[\"'‘“’”]", "", w[1])) else ""
  }, character(1))
  cat2 <- tolower(trimws(out$category))
  dedupe_scope <- cat2 %in% c("spelling", "grammar", "formatting", "terminology")
  key2 <- ifelse(dedupe_scope & nzchar(mention), paste(cat2, mention, sep = "||"),
    paste0("__keep__", seq_len(nrow(out)))
  )
  out <- out[!duplicated(key2), , drop = FALSE]

  rownames(out) <- NULL
  out
}

# ---------------------------------------------------------------------------
# Numeric verification pass.
# A single audit over a report with ~27 tables reliably finds the headline
# problems but cannot also check every quoted figure. This second pass does
# nothing else: it walks table-by-table with the surrounding narrative and
# compares each number, percentage and timepoint against the actual cell.
# ---------------------------------------------------------------------------

# Build focused chunks: each table plus the narrative immediately around it,
# batched so every chunk stays small enough for the model to read closely.
build_table_contexts <- function(text, before = 30L, after = 30L, max_chars = 11000L) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  starts <- grep("^\\[TABLE [0-9]+\\]$", lines)
  ends <- grep("^\\[END TABLE [0-9]+\\]$", lines)
  if (length(starts) == 0) {
    return(character(0))
  }
  n <- length(lines)
  spans <- list()
  for (k in seq_along(starts)) {
    s <- starts[k]
    e <- if (k <= length(ends)) ends[k] else min(n, s + 60L)
    spans[[k]] <- c(max(1L, s - before), min(n, e + after))
  }
  # Merge overlapping/adjacent spans
  merged <- list()
  cur <- spans[[1]]
  for (k in seq_along(spans)[-1]) {
    if (spans[[k]][1] <= cur[2] + 5L) {
      cur[2] <- max(cur[2], spans[[k]][2])
    } else {
      merged[[length(merged) + 1]] <- cur
      cur <- spans[[k]]
    }
  }
  merged[[length(merged) + 1]] <- cur

  # Emit chunks, splitting any span that is too large
  out <- character(0)
  for (sp in merged) {
    blk <- lines[sp[1]:sp[2]]
    cur_txt <- character(0)
    cur_len <- 0L
    for (l in blk) {
      if (cur_len > 0 && cur_len + nchar(l) > max_chars) {
        out <- c(out, paste(cur_txt, collapse = "\n"))
        cur_txt <- character(0)
        cur_len <- 0L
      }
      cur_txt <- c(cur_txt, l)
      cur_len <- cur_len + nchar(l) + 1L
    }
    if (length(cur_txt) > 0) out <- c(out, paste(cur_txt, collapse = "\n"))
  }
  out[nzchar(trimws(out))]
}

# One focused verification call. Returns an issues data.frame (possibly empty).
ollama_verify_numbers <- function(chunk, model, timeout_sec = 600) {
  system_msg <- paste(
    "You are a meticulous data-verification auditor at Mascot Spincontrol Pvt. Ltd.",
    "You compare narrative claims against table cells and report only real, provable mismatches."
  )
  prompt <- paste0(
    "Below is an extract of a clinical/cosmetic study report: narrative text plus tables.\n",
    "Tables are pipe-delimited rows between [TABLE n] and [END TABLE n]. The first row(s) hold the column headers (timepoints such as 'T+30 mins', '8 Hours', 'T+14 days', 'T+28 Days'). A value's column is determined by its position in the row, counting the pipes.\n\n",
    "YOUR ONLY TASK: verify every number in the narrative against the tables. For EACH sentence that quotes a figure:\n",
    "1. Locate the table and the exact row it refers to.\n",
    "2. Count pipe positions to identify which COLUMN (timepoint) the figure sits in.\n",
    "3. Compare the value AND the timepoint AND the row label with what the sentence says.\n\n",
    "Report an issue for every one of these:\n",
    "- VALUE MISMATCH: narrative figure differs from the table cell (e.g. table cell is 68% but the text says 71%).\n",
    "- TIMEPOINT MISMATCH: the figure exists but in a different column than stated (e.g. 88% sits in the '8 Hours' column but the text says 'at T+30 minutes').\n",
    "- ROW/ITEM MISMATCH: the figure belongs to a different question/parameter than the one named.\n",
    "- NOT IN TABLE: a quoted figure appears nowhere in the referenced table.\n",
    "- SIGNIFICANCE CONTRADICTION: 'significant' claimed but the p-value or 'Significant at 5%' cell says otherwise (or vice versa).\n",
    "- COUNT MISMATCH: N / number of subjects differs between text and table.\n",
    "- LABEL MISMATCH: the table's own caption/parameter name disagrees with the section heading or the narrative parameter.\n\n",
    "Rules: quote BOTH values in what_is_wrong (what the table shows vs what the text says). Only report a mismatch you can prove from the extract. If everything agrees, return an empty issues array. Do not report spelling, grammar or formatting. Return ONLY valid JSON.\n\n",
    "{\n",
    "  \"issues\": [\n",
    "    {\n",
    "      \"severity\": \"critical|major|minor\",\n",
    "      \"category\": \"statistics|inconsistency\",\n",
    "      \"location\": \"<section / table number and row>\",\n",
    "      \"what_is_wrong\": \"<table shows X at <column>, but the text states Y at <column>>\",\n",
    "      \"suggested_fix\": \"<the corrected sentence or value>\",\n",
    "      \"original_text\": \"<verbatim phrase from the narrative that is wrong, or empty>\",\n",
    "      \"corrected_text\": \"<the corrected phrase, or empty>\"\n",
    "    }\n",
    "  ]\n",
    "}\n\n",
    "EXTRACT TO VERIFY:\n\"\"\"\n", chunk, "\n\"\"\"\n"
  )
  body <- list(
    model = model, system = system_msg, prompt = prompt, stream = FALSE,
    format = "json", keep_alive = "1h",
    options = list(
      temperature = 0, num_ctx = 32768L, num_predict = 3072L,
      num_thread = N_CORES
    )
  )
  resp <- tryCatch(
    httr2::request(paste0(OLLAMA_URL, "/api/generate")) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(timeout_sec) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) {
    return(empty_issues())
  }
  parsed <- tryCatch(httr2::resp_body_json(resp), error = function(e) NULL)
  raw <- parsed$response %||% ""
  out <- parse_model_json(raw)
  if (is.null(out)) {
    return(empty_issues())
  }
  iss <- out$issues
  if (is.null(iss) || NROW(iss) == 0) {
    return(empty_issues())
  }
  coerce_issues_df(normalize_review(list(issues = iss))$issues)
}

# Run the verification pass over all table contexts and return merged issues.
numeric_verification_pass <- function(text, model,
                                      progress = function(msg) invisible(NULL)) {
  chunks <- build_table_contexts(text)
  if (length(chunks) == 0) {
    return(empty_issues())
  }
  acc <- list()
  for (i in seq_along(chunks)) {
    progress(sprintf("Verifying figures against tables (%d/%d) ...", i, length(chunks)))
    r <- tryCatch(ollama_verify_numbers(chunks[[i]], model), error = function(e) NULL)
    if (!is.null(r) && NROW(r) > 0) acc[[length(acc) + 1]] <- r
  }
  if (length(acc) == 0) {
    return(empty_issues())
  }
  dedupe_issues(do.call(rbind, acc))
}

merge_findings <- function(llm, det) {
  combined <- dedupe_issues(rbind(coerce_issues_df(llm), coerce_issues_df(det)))
  if (NROW(combined) == 0) {
    return(NULL)
  }
  combined
}

# Score and risk derived from the final, merged issue counts.
recompute_score <- function(df) {
  if (NROW(df) == 0) {
    return(list(score = 100L, risk = "Low"))
  }
  sev <- tolower(trimws(df$severity))
  cc <- sum(sev == "critical")
  mm <- sum(sev == "major")
  nn <- sum(!(sev %in% c("critical", "major")))
  s <- max(0L, min(100L, as.integer(100L - 15L * cc - 7L * mm - 2L * nn)))
  risk <- if (cc > 0 || s < 55) {
    "Critical"
  } else if (mm > 0 || s < 75) {
    "High"
  } else if (s < 90) "Medium" else "Low"
  list(score = s, risk = risk)
}

# Split a document into section-sized chunks at heading boundaries.
split_into_sections <- function(text, max_chars = 7000L) {
  paras <- strsplit(text, "\n", fixed = TRUE)[[1]]
  chunks <- list()
  cur <- character(0)
  cur_len <- 0L
  is_heading <- function(l) {
    grepl("^\\s*\\d+(\\.\\d+)*[\\.\\s]", l, perl = TRUE) ||
      grepl("^[A-Z][A-Z0-9 ,&/:-]{6,}$", l)
  }
  push <- function() if (length(cur) > 0) chunks[[length(chunks) + 1]] <<- paste(cur, collapse = "\n")
  for (p in paras) {
    np <- nchar(p)
    if (cur_len > 0 && (cur_len + np > max_chars || (is_heading(p) && cur_len > max_chars * 0.5))) {
      push()
      cur <- character(0)
      cur_len <- 0L
    }
    cur <- c(cur, p)
    cur_len <- cur_len + np + 1L
    if (cur_len > max_chars * 1.5) {
      push()
      cur <- character(0)
      cur_len <- 0L
    }
  }
  push()
  if (length(chunks) == 0) chunks <- list(text)
  chunks
}

# Parse the document's Table of Contents into (title, page). Word TOC entries
# survive extraction in many shapes - "1.2\tTITLE\t14", dot leaders
# ("TITLE......14"), glued numbers ("TITLE14"), mixed case, en-dashes - so we
# parse the Contents block tolerantly: strip a leading section number and any
# dot/space/tab leader, then take a trailing page number. Confining the scan to
# the Contents region keeps ordinary body sentences (which can also end in a
# number) out of the map. Longest title first so the most specific match wins.
parse_toc_pages <- function(text) {
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  start <- which(grepl("^\\s*(table of\\s+)?contents\\s*$", lines, ignore.case = TRUE))[1]
  if (is.na(start)) start <- 0L
  rng <- (start + 1L):min(length(lines), start + 250L)
  titles <- character(0)
  pages <- integer(0)
  sections <- character(0)
  misses <- 0L
  for (k in rng) {
    if (k < 1L || k > length(lines)) next
    l <- lines[k]
    if (!nzchar(trimws(l))) next
    m <- regmatches(l, regexec(
      "^\\s*(?:\\d+(?:\\.\\d+)*\\.?[\\t ]+)?(.+?)[\\t .]*?(\\d{1,4})\\s*$",
      l,
      perl = TRUE
    ))[[1]]
    ok <- FALSE
    if (length(m) >= 3) {
      ttl <- gsub("\\.{2,}.*$", "", m[2]) # drop dot leaders
      ttl <- toupper(trimws(gsub("[\\t ._]+$", "", ttl, perl = TRUE)))
      pg <- suppressWarnings(as.integer(m[3]))
      if (!is.na(pg) && pg >= 1 && pg <= 2000 &&
        nchar(ttl) >= 4 && grepl("[A-Z]{3}", ttl)) {
        # Section number that prefixes this TOC entry, e.g. "3.1" in
        # "3.1<tab>PARTICIPANT SELECTION<tab>8" ("" when unnumbered).
        sm <- regmatches(l, regexec("^\\s*(\\d+(?:\\.\\d+)*)\\.?[\\t ]+", l, perl = TRUE))[[1]]
        sec <- if (length(sm) >= 2) sub("\\.$", "", sm[2]) else ""
        titles <- c(titles, ttl)
        pages <- c(pages, pg)
        sections <- c(sections, sec)
        misses <- 0L
        ok <- TRUE
      }
    }
    if (!ok) misses <- misses + 1L
    if (length(titles) >= 3 && misses >= 10) break # drifted out of the TOC
  }
  if (length(titles) == 0) {
    return(NULL)
  }
  df <- data.frame(title = titles, page = pages, section = sections, stringsAsFactors = FALSE)
  df <- df[!duplicated(df$title), , drop = FALSE]
  df[order(-nchar(df$title)), , drop = FALSE]
}

# Add a `section` column: the document's own numbered section for each finding
# (e.g. "3.1.1"). Preference order:
#   1. an explicit number the model already put in `location` ("Section 3.1.1")
#   2. the TOC section number whose title matches `location`
# Falls back to "" so the UI can show a dash.
assign_sections <- function(issues, text, toc_map = NULL) {
  issues <- as.data.frame(issues, stringsAsFactors = FALSE)
  if (NROW(issues) == 0) {
    issues$section <- character(0)
    return(issues)
  }
  toc <- if (!is.null(toc_map) && NROW(toc_map) > 0) toc_map else parse_toc_pages(text)
  has_toc <- !is.null(toc) && NROW(toc) > 0 && !is.null(toc$section)
  toc_norm <- if (has_toc) vapply(toc$title, norm_heading, character(1)) else NULL

  n <- nrow(issues)
  sec <- rep("", n)
  for (i in seq_len(n)) {
    # Per-row tryCatch: one awkward location string must never wipe out the
    # section column for every other finding.
    sec[i] <- tryCatch(
      {
        loc <- issues$location[i] %||% ""
        val <- ""
        if (nzchar(trimws(loc))) {
          # 1) explicit dotted number already present in the location,
          #    e.g. "3.1.1 PARTICIPANT SELECTION" or "5.5RESULTS OF ..."
          em <- regmatches(loc, regexec("([0-9]+(?:\\.[0-9]+)+)", loc))[[1]]
          if (length(em) >= 2) {
            val <- em[2]
          } else if (has_toc) {
            # 2) match location (whole, then each part of a compound) to a TOC title
            parts <- unlist(strsplit(loc, "\\s*(/|:|->|>|,|vs\\.?| - )\\s*", perl = TRUE))
            cands <- unique(c(norm_heading(loc), vapply(parts, norm_heading, character(1))))
            cands <- cands[nchar(cands) >= 4]
            for (cand in cands) {
              hit <- ""
              for (j in seq_len(NROW(toc))) {
                t <- toc_norm[j]
                if (nchar(t) >= 4 && (grepl(t, cand, fixed = TRUE) || grepl(cand, t, fixed = TRUE))) {
                  if (nzchar(toc$section[j])) hit <- toc$section[j]
                  break
                }
              }
              if (nzchar(hit)) {
                val <- hit
                break
              }
            }
          }
        }
        val
      },
      error = function(e) ""
    )
  }
  issues$section <- sec
  issues
}

# Normalize a heading/location for fuzzy matching: uppercase, punctuation to
# spaces, collapse whitespace.
norm_heading <- function(s) trimws(gsub("\\s+", " ", gsub("[^A-Z0-9 ]", " ", toupper(s))))

# Add a `page` column to an issues data.frame.
#   page_texts (PDF): a character vector, one element per page -> exact page.
#   otherwise (docx): map each finding's location to the TOC page (approximate).
assign_pages <- function(issues, text, page_texts = NULL, toc_map = NULL) {
  issues <- as.data.frame(issues, stringsAsFactors = FALSE)
  if (NROW(issues) == 0) {
    issues$page <- character(0)
    return(issues)
  }
  n <- nrow(issues)
  page <- rep("", n)

  # Whitespace + case tolerant: extraction spacing and heading casing vary, so we
  # normalize both sides before matching. Models also rarely fill original_text,
  # so we locate a finding by several signals, not that one field alone.
  norm <- function(s) tolower(gsub("\\s+", " ", trimws(s %||% "")))

  pages_norm <- if (!is.null(page_texts) && length(page_texts) > 0) {
    vapply(page_texts, norm, character(1))
  } else {
    NULL
  }

  # Flag the front-matter / table-of-contents pages: a heading or quoted phrase
  # also appears there (as a TOC entry), so we skip them and take the first BODY
  # occurrence instead. A page is TOC-like if it names the contents section or
  # carries several dot leaders ("....." before a page number).
  toc_like <- if (!is.null(pages_norm)) {
    has_dots <- vapply(page_texts, function(t) {
      m <- gregexpr("[.]{4,}", t)[[1]]
      (m[1] != -1) && length(m) >= 3
    }, logical(1))
    grepl("table of contents", pages_norm, fixed = TRUE) | has_dots
  } else {
    logical(0)
  }

  # First BODY page (skipping TOC-like pages) whose text contains needle; falls
  # back to the first match overall if every hit is on a TOC page.
  locate <- function(needle, minlen = 4L) {
    k <- norm(needle)
    if (nchar(k) < minlen) {
      return(NA_integer_)
    }
    matches <- which(vapply(
      pages_norm,
      function(p) isTRUE(grepl(k, p, fixed = TRUE)), logical(1)
    ))
    if (!length(matches)) {
      return(NA_integer_)
    }
    body <- matches[!toc_like[matches]]
    if (length(body)) body[1] else matches[1]
  }
  # Substrings the model quoted from the document (straight or curly quotes).
  q_chars <- "['\"‘’“”]"
  quoted <- function(s) {
    if (!nzchar(s %||% "")) {
      return(character(0))
    }
    m <- regmatches(s, gregexpr(paste0(q_chars, "([^'\"‘’“”]{4,80})", q_chars),
      s,
      perl = TRUE
    ))[[1]]
    m <- gsub(paste0("^", q_chars, "|", q_chars, "$"), "", m, perl = TRUE)
    unique(trimws(m))
  }

  # TOC fallback: title -> page map (parsed lazily from the body if not supplied).
  toc <- if (!is.null(toc_map) && NROW(toc_map) > 0) toc_map else parse_toc_pages(text)
  toc_norm <- if (!is.null(toc) && NROW(toc) > 0) {
    vapply(toc$title, norm_heading, character(1))
  } else {
    NULL
  }

  for (i in seq_len(n)) {
    loc <- issues$location[i] %||% ""

    # 1) Exact page via per-page text (PDF, or docx with rendered page breaks).
    if (!is.null(pages_norm)) {
      pg <- NA_integer_
      # a) verbatim original_text, when the model supplied it
      ot <- issues$original_text[i] %||% ""
      if (nzchar(ot)) pg <- locate(ot)
      # b) any phrase the model quoted from the doc in what_is_wrong / suggested_fix
      if (is.na(pg)) {
        for (qq in c(quoted(issues$what_is_wrong[i]), quoted(issues$suggested_fix[i]))) {
          pg <- locate(qq)
          if (!is.na(pg)) break
        }
      }
      # c) the section heading named in `location` (first body occurrence)
      if (is.na(pg) && nzchar(trimws(loc))) {
        parts <- unlist(strsplit(loc,
          "(?i)\\s*(/|:|->|>|,|vs\\.?|–|—| - )\\s*",
          perl = TRUE
        ))
        for (p in c(loc, parts)) {
          pg <- locate(p, minlen = 5L)
          if (!is.na(pg)) break
        }
      }
      if (!is.na(pg)) {
        page[i] <- as.character(pg)
        next
      }
    }

    # 2) Fallback (no per-page text): map the location to a TOC heading's page.
    if (!is.null(toc_norm) && nzchar(trimws(loc))) {
      parts <- unlist(strsplit(loc, "\\s*(/|:|->|>|,|–|—| - )\\s*", perl = TRUE))
      cands <- unique(c(norm_heading(loc), vapply(parts, norm_heading, character(1))))
      cands <- cands[nchar(cands) >= 4]
      hit <- NA_integer_
      for (cand in cands) {
        for (j in seq_len(NROW(toc))) {
          t <- toc_norm[j]
          if (nchar(t) >= 4 && (grepl(t, cand, fixed = TRUE) || grepl(cand, t, fixed = TRUE))) {
            hit <- toc$page[j]
            break
          }
        }
        if (!is.na(hit)) break
      }
      if (!is.na(hit)) page[i] <- as.character(hit)
    }
  }
  issues$page <- page
  issues
}

# Orchestrator: single-pass (default) or deep section-by-section, always
# augmented with the deterministic pre-scan. Returns the same shape as
# ollama_review() so the caller is unchanged.
run_audit <- function(text, model, doc_type, custom_guidelines = "",
                      fast = FALSE, deep = FALSE, page_texts = NULL, toc_map = NULL,
                      verify_numbers = TRUE,
                      progress = function(msg) invisible(NULL)) {
  t0 <- Sys.time()
  repaired <- FALSE

  if (isTRUE(deep) && !isTRUE(fast)) {
    chunks <- split_into_sections(text)
    issue_sets <- list()
    missing_all <- character(0)
    exec_sum <- ""
    for (ci in seq_along(chunks)) {
      progress(sprintf("Deep scan: section %d of %d ...", ci, length(chunks)))
      r <- tryCatch(
        ollama_review(chunks[[ci]], model, doc_type,
          custom_guidelines = custom_guidelines, fast = TRUE
        ),
        error = function(e) NULL
      )
      if (!is.null(r) && isTRUE(r$ok)) {
        if (NROW(r$data$issues) > 0) issue_sets[[length(issue_sets) + 1]] <- r$data$issues
        if (isTRUE(r$repaired)) repaired <- TRUE
      }
    }
    progress("Deep scan: whole-document consistency pass ...")
    whole <- tryCatch(
      ollama_review(text, model, doc_type,
        custom_guidelines = custom_guidelines, fast = fast
      ),
      error = function(e) NULL
    )
    if (!is.null(whole) && isTRUE(whole$ok)) {
      if (NROW(whole$data$issues) > 0) issue_sets[[length(issue_sets) + 1]] <- whole$data$issues
      exec_sum <- whole$data$executive_summary %||% ""
      missing_all <- unique(c(missing_all, as.character(unlist(whole$data$missing_information %||% list()))))
      if (isTRUE(whole$repaired)) repaired <- TRUE
    }
    merged_llm <- if (length(issue_sets) > 0) {
      do.call(rbind, lapply(issue_sets, coerce_issues_df))
    } else {
      NULL
    }
    data <- list(
      overall_score = 0L, risk_level = "Unknown",
      executive_summary = exec_sum, issues = merged_llm,
      missing_information = as.list(missing_all)
    )
    if (is.null(merged_llm) && !nzchar(exec_sum)) {
      return(list(
        ok = FALSE, raw_text = "Deep scan produced no parseable output.",
        elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs")),
        repaired = repaired
      ))
    }
  } else {
    r <- ollama_review(text, model, doc_type, custom_guidelines = custom_guidelines, fast = fast)
    if (!isTRUE(r$ok)) {
      return(r)
    }
    data <- r$data
    repaired <- isTRUE(r$repaired)
  }

  # Dedicated numeric verification pass over every table + its narrative. This
  # is what catches figures attributed to the wrong timepoint and percentages
  # that disagree with the table cell - checks the main audit cannot fit in.
  if (isTRUE(verify_numbers) && !isTRUE(fast)) {
    num <- tryCatch(numeric_verification_pass(text, model, progress),
      error = function(e) NULL
    )
    if (!is.null(num) && NROW(num) > 0) {
      data$issues <- dedupe_issues(rbind(
        coerce_issues_df(data$issues), coerce_issues_df(num)
      ))
    }
  }

  # Always merge the deterministic pre-scan
  det <- tryCatch(deterministic_findings(text), error = function(e) NULL)
  n_before <- NROW(coerce_issues_df(data$issues))
  data$issues <- merge_findings(data$issues, det)
  n_added <- NROW(coerce_issues_df(data$issues)) - n_before

  if (isTRUE(deep)) {
    sc <- recompute_score(data$issues)
    data$overall_score <- sc$score
    data$risk_level <- sc$risk
  } else if (n_added > 0) {
    # Keep the model's holistic score but reflect the extra deterministic items
    base <- suppressWarnings(as.integer(data$overall_score %||% 0L))
    if (is.na(base)) base <- 0L
    data$overall_score <- max(0L, base - 2L * n_added)
  }

  # Locate each finding's page (exact for PDF, TOC-approximate for docx) and its
  # numbered section (e.g. "3.1.1") for the results table.
  if (!is.null(data$issues) && NROW(data$issues) > 0) {
    data$issues <- tryCatch(assign_pages(data$issues, text, page_texts, toc_map),
      error = function(e) data$issues
    )
    data$issues <- tryCatch(assign_sections(data$issues, text, toc_map),
      error = function(e) data$issues
    )
  }

  list(
    ok = TRUE, data = data,
    elapsed_s = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    repaired = repaired
  )
}

# ============================================================================
# UI Setup
# ============================================================================
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700;800&family=Plus+Jakarta+Sans:wght@300;400;500;600;700&display=swap');

      body {
        background-color: #f7f9fc;
        font-family: 'Plus Jakarta Sans', 'Segoe UI', system-ui, sans-serif;
        color: #2b2d42;
      }

      h1, h2, h3, h4, h5, h6 {
        font-family: 'Outfit', sans-serif;
        font-weight: 600;
        color: #1a202c;
      }

      .mascot-header {
        background: linear-gradient(135deg, #0d1b2a 0%, #1b263b 100%);
        color: white;
        padding: 20px 32px;
        border-radius: 0 0 16px 16px;
        margin-bottom: 24px;
        box-shadow: 0 4px 20px rgba(13, 27, 42, 0.15);
        display: flex;
        justify-content: space-between;
        align-items: center;
        border-bottom: 3px solid #e2e8f0;
      }

      .mascot-title-wrap {
        display: flex;
        align-items: center;
        gap: 16px;
      }

      .mascot-logo-icon {
        background: linear-gradient(135deg, #415a77 0%, #1b263b 100%);
        width: 48px;
        height: 48px;
        border-radius: 12px;
        display: flex;
        align-items: center;
        justify-content: center;
        box-shadow: 0 4px 12px rgba(27, 38, 59, 0.3);
        font-size: 20px;
        border: 1px solid rgba(255,255,255,0.1);
      }

      .mascot-title {
        margin: 0;
        font-size: 24px;
        font-weight: 800;
        letter-spacing: -0.5px;
        background: linear-gradient(to right, #ffffff, #e2e8f0);
        -webkit-background-clip: text;
        -webkit-text-fill-color: transparent;
      }

      .mascot-subtitle {
        margin: 4px 0 0 0;
        font-size: 11px;
        color: #a5a5a5;
        letter-spacing: 1px;
        text-transform: uppercase;
        font-weight: 700;
      }

      .sidebar-container {
        background: white;
        border-radius: 16px;
        padding: 20px;
        box-shadow: 0 4px 24px rgba(0, 0, 0, 0.03);
        border: 1px solid #e2e8f0;
      }

      .panel-card {
        background: white;
        border-radius: 16px;
        padding: 24px;
        box-shadow: 0 4px 24px rgba(0, 0, 0, 0.03);
        border: 1px solid #e2e8f0;
        margin-bottom: 24px;
        transition: transform 0.2s ease, box-shadow 0.2s ease;
      }

      .panel-card:hover {
        transform: translateY(-2px);
        box-shadow: 0 8px 30px rgba(0, 0, 0, 0.05);
      }

      .score-big {
        font-family: 'Outfit', sans-serif;
        font-size: 56px;
        font-weight: 800;
        line-height: 1;
        color: #1b263b;
      }

      .risk-pill {
        display: inline-block;
        padding: 6px 14px;
        border-radius: 999px;
        color: #fff;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
        font-size: 0.75em;
        box-shadow: 0 2px 6px rgba(0,0,0,0.08);
      }

      .risk-Low      { background: linear-gradient(135deg, #10b981 0%, #059669 100%); }
      .risk-Medium   { background: linear-gradient(135deg, #3b82f6 0%, #1d4ed8 100%); }
      .risk-High     { background: linear-gradient(135deg, #f59e0b 0%, #d97706 100%); }
      .risk-Critical { background: linear-gradient(135deg, #ef4444 0%, #dc2626 100%); }

      .progress-wrapper {
        background: #e2e8f0;
        border-radius: 8px;
        height: 16px;
        overflow: hidden;
        margin: 10px 0;
      }

      .progress-fill {
        height: 100%;
        width: 0%;
        background: linear-gradient(90deg, #1b263b, #415a77);
        animation-name: fillBar;
        animation-timing-function: ease-out;
        animation-fill-mode: forwards;
      }

      @keyframes fillBar { from { width: 0%; } to { width: 95%; } }
      .progress-fill.done { width: 100% !important; animation: none !important; }

      .eta-text {
        font-size: 0.82em;
        color: #64748b;
        margin-top: 4px;
      }

      .sev-critical  { background: #fee2e2; color: #ef4444; border: 1px solid #fecaca; padding: 3px 8px; border-radius: 4px; font-weight: 700; font-size: 0.72em; letter-spacing: 0.02em; text-transform: uppercase; }
      .sev-major     { background: #fef3c7; color: #d97706; border: 1px solid #fde68a; padding: 3px 8px; border-radius: 4px; font-weight: 700; font-size: 0.72em; letter-spacing: 0.02em; text-transform: uppercase; }
      .sev-minor     { background: #dbeafe; color: #2563eb; border: 1px solid #bfdbfe; padding: 3px 8px; border-radius: 4px; font-weight: 700; font-size: 0.72em; letter-spacing: 0.02em; text-transform: uppercase; }

      .nav-tabs {
        border-bottom: none;
        background: white;
        padding: 0 16px;
        border-radius: 12px;
        box-shadow: 0 2px 12px rgba(0,0,0,0.02);
        margin-bottom: 20px;
        display: flex;
        gap: 6px;
      }

      .nav-tabs > li > a {
        border: none !important;
        color: #64748b;
        font-weight: 600;
        padding: 14px 18px;
        transition: all 0.2s ease;
        font-family: 'Outfit', sans-serif;
        border-bottom: 3px solid transparent !important;
      }

      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus {
        color: #1b263b !important;
        background: transparent !important;
        border-bottom: 3px solid #1b263b !important;
      }

      .nav-tabs > li > a:hover {
        color: #1e293b;
        background: #f8fafc !important;
      }

      .table {
        width: 100% !important;
      }
    "))
  ),

  # Custom Mascot Header
  div(
    class = "mascot-header",
    div(
      class = "mascot-title-wrap",
      div(class = "mascot-logo-icon", icon("shield-halved", style = "color: #e2e8f0;")),
      div(
        h1(class = "mascot-title", "Mascot Spincontrol"),
        p(class = "mascot-subtitle", "AI-Powered Quality Assurance Terminal")
      )
    ),
    div(
      style = "text-align: right; color: #e2e8f0; font-size: 0.85em; font-family: 'Outfit', sans-serif;",
      tags$strong("Client: Mascot Spincontrol Pvt. Ltd."), br(),
      tags$span(style = "color: #8892b0;", format(Sys.Date(), "%d %b %Y"))
    )
  ),

  # Main Tabset
  tabsetPanel(
    id = "main_nav",
    tabPanel("Document Audit Terminal",
      value = "terminal",
      sidebarLayout(
        sidebarPanel(
          width = 3,
          # 1. Upload Document Card
          div(
            class = "panel-card", style = "padding: 16px;",
            textInput("study_code", "Study / Document Code", value = "", placeholder = "e.g. ST-2026-001"),
            fileInput("file", "Upload document",
              accept = c(".pdf", ".docx", ".xlsx", ".xls")
            ),
            selectInput("doc_type", "Document type",
              choices = c("Protocol", "Data Entry", "Report")
            ),
            uiOutput("model_ui"),
            actionButton("refresh_models", "Refresh model list",
              icon = icon("rotate"),
              class = "btn-sm btn-outline-secondary", style = "width: 100%; margin-bottom: 12px;"
            ),
            hr(style = "margin: 12px 0;"),
            # Per-run remarks: anything the auditor should consider this time only
            tags$label("Remarks for this audit", `for` = "remarks",
              style = "font-weight:600; font-size:0.9em; color:#1b263b;"
            ),
            tags$p("Optional. Read by the model before auditing (this run only - not saved).",
              style = "font-size: 0.78em; color: #64748b; margin: 2px 0 6px 0;"
            ),
            textAreaInput("remarks", NULL,
              value = "", rows = 3, width = "100%",
              placeholder = "e.g. This is a draft v0.9 - ignore missing signatures.\nFocus on the statistics section."
            ),
            uiOutput("eta_box"),
            actionButton("analyze", "Analyse document",
              icon = icon("play"), class = "btn-primary btn-block",
              width = "100%", style = "font-weight: 700; padding: 10px;"
            )
          ),

          # 2. Operations Status Info
          uiOutput("status")
        ),
        mainPanel(
          width = 9,
          uiOutput("progress_ui"),
          uiOutput("results_ui"),
          conditionalPanel(
            "output.has_analysis == true",
            div(
              class = "panel-card",
              downloadButton("dl_md", "Download report (.md)",
                class = "btn-outline-primary"
              ),
              downloadButton("dl_json", "Raw JSON",
                class = "btn-outline-secondary"
              )
            )
          )
        )
      )
    ),

    # Custom Audit Guidelines Tab - persistent rules, added/removed one by one
    tabPanel("Audit Guidelines",
      value = "guidelines",
      div(
        class = "panel-card",
        h3("Custom Audit Guidelines", style = "margin-top: 0; margin-bottom: 8px; color: #1b263b;"),
        p("Add your house rules one at a time. Every saved rule is enforced on every audit, in addition to the standard checks.",
          style = "color: #64748b; margin-bottom: 20px;"
        ),
        fluidRow(
          column(10,
            textAreaInput("new_guideline", NULL,
              value = "", rows = 2, width = "100%",
              placeholder = "e.g. In footer, Protocol sections must show PR, ICF must show AC, SSE must show MQ."
            )
          ),
          column(2,
            style = "margin-top: 2px;",
            actionButton("add_guideline", "Add rule",
              icon = icon("plus"), class = "btn-success",
              style = "width: 100%; font-weight: 600;"
            )
          )
        ),
        hr(),
        h4("Saved rules", style = "color:#1b263b; margin-bottom: 12px;"),
        uiOutput("guidelines_list")
      )
    ),

    # Study History Explorer Tab
    tabPanel("Study History Explorer",
      value = "history",
      div(
        class = "panel-card",
        h3("Mascot Study Audit Logs", style = "margin-top: 0; margin-bottom: 8px; color: #1b263b;"),
        p("Explore and retrieve past audits conducted on Mascot Spincontrol Pvt. Ltd. scientific documents. Select a study to view its detailed report.", style = "color: #64748b; margin-bottom: 24px;"),
        fluidRow(
          column(4, textInput("search_study", "Filter by Study Code", placeholder = "Type study code...")),
          column(4, textInput("search_file", "Filter by Filename", placeholder = "Type filename...")),
          column(4,
            style = "margin-top: 25px; text-align: right;",
            actionButton("clear_filters", "Clear Filters", icon = icon("times"), class = "btn-secondary btn-sm")
          )
        ),
        hr(),
        DT::dataTableOutput("history_table"),
        br(),
        div(
          style = "text-align: right;",
          actionButton("delete_history_review", "Delete Selected Record", icon = icon("trash"), class = "btn-danger"),
          actionButton("load_history_review", "View Selected Audit Dashboard", icon = icon("eye"), class = "btn-primary", style = "margin-left: 10px;")
        )
      )
    )
  )
)

# ============================================================================
# Server Setup
# ============================================================================
server <- function(input, output, session) {
  doc_text <- reactiveVal(NULL)
  analysis <- reactiveVal(NULL)
  models <- reactiveVal(character(0))
  status <- reactiveVal("")
  history_trigger <- reactiveVal(0)
  orig_docx_path <- reactiveVal(NULL) # path to uploaded .docx (this session only)
  orig_docx_name <- reactiveVal(NULL)
  applicable_fixes <- reactiveVal(list()) # issues with verbatim original_text we can apply
  pdf_pages <- reactiveVal(NULL) # per-page text of an uploaded PDF (exact page numbers)
  toc_map <- reactiveVal(NULL) # title->page map parsed from a docx TOC (XML-level)

  # Populate model list on startup and refresh
  refresh <- function() {
    m <- ollama_models()
    models(m)
    if (length(m) == 0) {
      status(paste(
        "Ollama not reachable at", OLLAMA_URL,
        "- start Ollama Desktop or run 'ollama serve'."
      ))
    } else {
      status(paste("Ollama OK -", length(m), "models installed."))
      preferred <- c("gpt-oss:120b-cloud", "gemma4:31b-cloud", "qwen2.5:1.5b", "qwen2.5:3b", "llama3.2:3b", "gemma2:2b")
      warmup_model <- intersect(preferred, m)[1]
      if (is.na(warmup_model)) warmup_model <- m[1]
      later::later(function() {
        status(paste0("Warming up ", warmup_model, " ..."))
        ollama_warmup(warmup_model)
        status(paste("Ollama OK -", length(m), "models installed. Model ready."))
      }, delay = 0.5)
    }
  }
  refresh()
  observeEvent(input$refresh_models, refresh())

  # Re-warm model on switch
  observeEvent(input$model,
    {
      m <- input$model
      if (!is.null(m) && nzchar(m) && m != "(no models installed)") {
        later::later(function() ollama_warmup(m), delay = 0.3)
      }
    },
    ignoreInit = TRUE
  )

  # ---- Persistent custom guidelines, managed one rule at a time ----
  # Stored as a JSON array under the "guidelines_list" setting. Any pre-existing
  # free-text "custom_guidelines" value is migrated once, one rule per line.
  # Rules are displayed and sent to the model with their own numbering, so drop
  # any "1." / "2)" the user typed at the start to avoid "1. 1. ..." duplication.
  clean_rule <- function(s) trimws(sub("^\\s*\\d+\\s*[.)]\\s*", "", s))

  load_guidelines <- function() {
    raw <- get_setting("guidelines_list", "")
    if (nzchar(raw)) {
      g <- tryCatch(as.character(unlist(jsonlite::fromJSON(raw))), error = function(e) character(0))
      return(g[nzchar(trimws(g))])
    }
    legacy <- get_setting("custom_guidelines", "")
    if (nzchar(trimws(legacy))) {
      g <- vapply(strsplit(legacy, "\n", fixed = TRUE)[[1]], clean_rule, character(1),
        USE.NAMES = FALSE
      )
      g <- g[nzchar(g)]
      if (length(g) > 0) save_setting("guidelines_list", as.character(jsonlite::toJSON(g)))
      return(g)
    }
    character(0)
  }
  guidelines <- reactiveVal(load_guidelines())

  persist_guidelines <- function(g) {
    save_setting("guidelines_list", as.character(jsonlite::toJSON(g)))
    guidelines(g)
  }

  observeEvent(input$add_guideline, {
    txt <- clean_rule(input$new_guideline %||% "")
    if (!nzchar(txt)) {
      showNotification("Type a rule first.", type = "warning", session = session)
      return(NULL)
    }
    if (length(guidelines()) >= 50L) {
      showNotification("Rule limit reached (50). Remove a rule first.",
        type = "warning", session = session
      )
      return(NULL)
    }
    persist_guidelines(c(guidelines(), txt))
    updateTextAreaInput(session, "new_guideline", value = "")
    showNotification("Rule saved.", type = "message", session = session)
  })

  # One delete button per rule (ids del_rule_1 ... del_rule_MAX). The observers
  # are registered once for a fixed set of slots, so re-rendering the list can
  # never stack duplicate handlers on the same button.
  MAX_RULES <- 50L
  lapply(seq_len(MAX_RULES), function(i) {
    observeEvent(input[[paste0("del_rule_", i)]],
      {
        cur <- guidelines()
        if (i <= length(cur)) {
          persist_guidelines(cur[-i])
          showNotification("Rule removed.", type = "warning", session = session)
        }
      },
      ignoreInit = TRUE
    )
  })

  output$guidelines_list <- renderUI({
    g <- guidelines()
    if (length(g) == 0) {
      return(tags$p(em("No custom rules yet. Add one above."), style = "color:#94a3b8;"))
    }
    tagList(lapply(seq_along(g), function(i) {
      div(
        style = "display:flex; align-items:flex-start; gap:12px; padding:10px 12px; border:1px solid #e2e8f0;
                 border-radius:10px; margin-bottom:8px; background:#f8fafc;",
        div(
          style = "flex:0 0 28px; height:28px; border-radius:8px; background:#1b263b; color:#fff;
                   display:flex; align-items:center; justify-content:center; font-weight:700; font-size:0.85em;",
          i
        ),
        div(style = "flex:1; line-height:1.45; color:#1e293b;", g[i]),
        actionButton(paste0("del_rule_", i), NULL,
          icon = icon("trash"), class = "btn-danger btn-sm",
          style = "flex:0 0 auto;"
        )
      )
    }))
  })

  output$model_ui <- renderUI({
    m <- models()
    preferred <- c("gpt-oss:120b-cloud", "gemma4:31b-cloud", "qwen2.5:1.5b", "qwen2.5:3b", "llama3.2:3b", "gemma2:2b", "phi3.5:3.8b")
    default <- intersect(preferred, m)[1]
    if (is.na(default)) default <- m[1]
    selectInput("model", "Ollama model",
      choices = if (length(m)) m else "(no models installed)",
      selected = default
    )
  })

  output$status <- renderUI({
    s <- status()
    if (!nzchar(s)) {
      return(NULL)
    }
    div(style = "margin-top: 10px; font-size: 0.82em; color: #475569; background: #f1f5f9; padding: 10px; border-radius: 8px; border: 1px solid #e2e8f0;", s)
  })

  # Read uploaded file
  observeEvent(input$file, {
    f <- input$file
    req(f)
    ext <- tolower(tools::file_ext(f$name))
    txt <- tryCatch(read_doc(f$datapath, ext),
      error = function(e) {
        showNotification(
          paste(
            "Could not read file:",
            conditionMessage(e)
          ),
          type = "error", duration = 10,
          session = session
        )
        NULL
      }
    )
    doc_text(txt)
    analysis(NULL)
    # Retain the original .docx so we can later build a corrected copy.
    # The "apply fixes" feature is only available for Word uploads.
    toc_note <- ""
    if (ext == "docx") {
      orig_docx_path(f$datapath)
      orig_docx_name(f$name)
      # Build the TOC page map from the raw docx XML (officer drops TOC page
      # runs). Word's printed page numbers live in the TOC; its rendered
      # page-break markers do NOT line up with them (the cover + contents share
      # one render segment), so the TOC is the authoritative source for a Word
      # document's page numbers.
      tm <- tryCatch(
        parse_toc_pages(paste(docx_paragraph_texts(f$datapath), collapse = "\n")),
        error = function(e) NULL
      )
      toc_map(tm)
      pdf_pages(NULL)
      n_toc <- if (is.null(tm)) 0L else nrow(tm)
      toc_note <- sprintf(" | TOC page anchors: %d", n_toc)
    } else {
      orig_docx_path(NULL)
      orig_docx_name(NULL)
      toc_map(NULL)
      # For PDFs, keep per-page text so findings get an exact page number.
      if (ext == "pdf") {
        pp <- tryCatch(pdftools::pdf_text(f$datapath), error = function(e) NULL)
        pdf_pages(pp)
        if (!is.null(pp)) toc_note <- sprintf(" | PDF pages: %d", length(pp))
      } else {
        pdf_pages(NULL)
      }
    }
    if (!is.null(txt)) {
      status(sprintf(
        "Loaded '%s' (%s chars, %s words).%s Ready to analyse.",
        f$name, format(nchar(txt), big.mark = ","),
        format(length(strsplit(txt, "\\s+")[[1]]), big.mark = ","),
        toc_note
      ))
    }
  })

  # Show ETA in sidebar
  eta <- reactive({
    txt <- doc_text()
    m <- input$model
    if (is.null(txt) || is.null(m) || !nzchar(m) ||
      m == "(no models installed)") {
      return(NULL)
    }
    nv <- length(tryCatch(build_table_contexts(txt), error = function(e) character(0)))
    estimate_eta(nchar(txt), m, fast = FALSE, n_verify = nv)
  })

  output$eta_box <- renderUI({
    e <- eta()
    if (is.null(e)) {
      return(NULL)
    }
    mins <- floor(e$seconds / 60)
    secs <- e$seconds %% 60
    pretty <- if (mins > 0) {
      sprintf("~%d min %d sec", mins, secs)
    } else {
      sprintf("~%d sec", secs)
    }
    div(
      style = "background:#ecfdf5; border-left:4px solid #10b981;
                 padding:8px 10px; border-radius:4px; margin-bottom:10px;",
      tags$div(tags$strong("Estimated analysis time: "), pretty),
      tags$div(class = "eta-text", e$note)
    )
  })

  # Running state + animated progress bar
  running <- reactiveVal(FALSE)

  output$progress_ui <- renderUI({
    if (!isTRUE(running())) {
      return(NULL)
    }
    e <- isolate(eta())
    secs <- if (!is.null(e)) e$seconds else 30
    div(
      class = "panel-card",
      h4(sprintf("Analysing document with %s ...", isolate(input$model)), style = "margin-top: 0; color: #1b263b;"),
      div(
        class = "progress-wrapper",
        div(
          class = "progress-fill",
          style = sprintf("animation-duration: %ds;", secs)
        )
      ),
      tags$p(
        class = "eta-text",
        sprintf("Estimated %d sec. The browser tab can stay open; you'll see the result here.", secs)
      )
    )
  })

  # Analyse document and persist to SQLite
  observeEvent(input$analyze, {
    req(doc_text())
    study_code_val <- trimws(input$study_code)
    if (!nzchar(study_code_val)) {
      showNotification("Please enter a Study / Document Code before running the analysis.",
        type = "warning", session = session
      )
      return(NULL)
    }

    if (is.null(input$model) || !nzchar(input$model) ||
      input$model == "(no models installed)") {
      showNotification("Pick an installed Ollama model first.",
        type = "warning", session = session
      )
      return(NULL)
    }
    running(TRUE)
    analysis(NULL)

    .text <- doc_text()
    .model <- input$model
    .doctype <- input$doc_type
    .pdf_pages <- pdf_pages()
    .toc_map <- toc_map()
    .session <- session
    .study_code <- study_code_val

    # Saved house rules (numbered) + optional one-off remarks for this run.
    g <- guidelines()
    rules_txt <- if (length(g) > 0) {
      paste(sprintf("%d. %s", seq_along(g), g), collapse = "\n")
    } else {
      ""
    }
    remarks_txt <- trimws(input$remarks %||% "")
    parts <- character(0)
    if (nzchar(rules_txt)) parts <- c(parts, rules_txt)
    if (nzchar(remarks_txt)) {
      parts <- c(parts, paste0(
        "REMARKS FOR THIS AUDIT (consider these before auditing):\n", remarks_txt
      ))
    }
    # ------------------------------------------------------------------
    # Check for a previously saved Protocol for this Study Code.
    # If found, embed its executive summary and missing items into the
    # custom guidelines that are sent to the LLM.
    proto_info <- get_protocol_review(.study_code)
    if (!is.null(proto_info)) parts <- c(parts, proto_info)
    .custom_guidelines <- paste(parts, collapse = "\n\n---\n\n")
    .filename <- if (!is.null(input$file)) input$file$name else "loaded_doc"

    later::later(function() {
      tryCatch(
        {
          res <- tryCatch(
            run_audit(.text, .model, .doctype,
              custom_guidelines = .custom_guidelines,
              fast = FALSE, deep = FALSE, page_texts = .pdf_pages, toc_map = .toc_map,
              progress = function(msg) status(msg)
            ),
            error = function(e) {
              showNotification(paste("Analysis failed:", conditionMessage(e)),
                type = "error", duration = 12,
                session = .session
              )
              NULL
            }
          )
          if (is.null(res)) {
            running(FALSE)
            return(NULL)
          }

          review_id_val <- generate_id()

          if (!isTRUE(res$ok)) {
            showNotification(
              paste(
                "Model did not return valid JSON.",
                "Showing raw text instead."
              ),
              type = "warning", duration = 10,
              session = .session
            )
            analysis(list(
              ok = FALSE, raw_text = res$raw_text,
              elapsed_s = res$elapsed_s
            ))
          } else {
            # Save to persistent database
            tryCatch(
              {
                save_review(
                  id = review_id_val,
                  study_code = .study_code,
                  filename = .filename,
                  doc_type = .doctype,
                  model = .model,
                  data = res$data,
                  missing_info = res$data$missing_information
                )
                # If this is a Protocol document, store its summary for future reuse
                if (identical(.doctype, "Protocol")) {
                  upsert_protocol(
                    study_code = .study_code,
                    executive_summary = res$data$executive_summary,
                    missing_information = res$data$missing_information
                  )
                }
                # Update history table
                history_trigger(history_trigger() + 1)
              },
              error = function(db_err) {
                message("DB Save failed: ", conditionMessage(db_err))
              }
            )

            analysis(list(
              ok = TRUE, data = res$data,
              elapsed_s = res$elapsed_s
            ))
          }
          status(sprintf(
            "Analysis complete in %.1fs.%s", res$elapsed_s,
            if (isTRUE(res$repaired)) " (model output was auto-repaired)" else ""
          ))
        },
        error = function(e) {
          message("later callback error: ", conditionMessage(e))
        }
      )
      running(FALSE)
    }, delay = 0.1)
  })

  output$has_analysis <- reactive(!is.null(analysis()))
  outputOptions(output, "has_analysis", suspendWhenHidden = FALSE)

  # Build the list of fixes that can be auto-applied to a Word copy:
  # any issue carrying a non-empty verbatim original_text.
  observe({
    a <- analysis()
    fixes <- list()
    if (!is.null(a) && isTRUE(a$ok) && !is.null(a$data$issues) && NROW(a$data$issues) > 0) {
      df <- as.data.frame(a$data$issues, stringsAsFactors = FALSE)
      ot <- if (!is.null(df$original_text)) df$original_text else rep("", nrow(df))
      ct <- if (!is.null(df$corrected_text)) df$corrected_text else rep("", nrow(df))
      for (i in seq_len(nrow(df))) {
        if (nzchar(ot[i] %||% "")) {
          fixes[[length(fixes) + 1]] <- list(
            severity       = df$severity[i] %||% "",
            location       = df$location[i] %||% "",
            what_is_wrong  = df$what_is_wrong[i] %||% "",
            original_text  = ot[i],
            corrected_text = ct[i] %||% ""
          )
        }
      }
    }
    applicable_fixes(fixes)
  })

  # Results layout
  output$results_ui <- renderUI({
    a <- analysis()
    if (is.null(a)) {
      return(div(
        class = "panel-card", style = "text-align: center; color: #64748b; padding: 40px;",
        tags$div(icon("file-lines", style = "font-size: 3em; margin-bottom: 12px; color: #cbd5e1;")),
        tags$p(
          style = "font-size: 1.1em; font-weight: 500;",
          "Upload a document and click Analyse."
        ),
        tags$p("Tip: define custom audit guidelines in the sidebar to enforce specific requirements.")
      ))
    }
    if (isFALSE(a$ok)) {
      return(div(
        class = "panel-card",
        h3("Raw model output (JSON parse failed)", style = "color: #b91c1c;"),
        tags$p(em(paste0(
          "The model's output couldn't be parsed as JSON, even after ",
          "automatic repair and a self-correction retry. ",
          "Raw output is shown below so nothing is lost. ",
          "Re-running the analysis usually succeeds; ",
          "qwen2.5:3b is the most reliable for JSON."
        ))),
        tags$pre(
          style = "white-space: pre-wrap; max-height: 600px; overflow: auto; background:#f8fafc; padding:12px; border-radius:6px; border: 1px solid #e2e8f0;",
          a$raw_text
        )
      ))
    }
    d <- a$data
    score <- d$overall_score %||% NA
    risk <- d$risk_level %||% "Unknown"

    issues_html <- if (is.null(d$issues) || NROW(d$issues) == 0) {
      tags$p(em("No issues identified by the model. Excellent work!"))
    } else {
      df <- as.data.frame(d$issues, stringsAsFactors = FALSE)
      for (col in c("severity", "category", "location", "what_is_wrong", "suggested_fix", "section")) {
        if (is.null(df[[col]])) df[[col]] <- ""
      }
      df$severity <- tolower(df$severity %||% "minor")

      tags$div(
        style = "overflow-x: auto;",
        tags$table(
          class = "table table-hover",
          style = "width:100%; border-collapse: collapse; margin-top: 10px;",
          tags$thead(
            tags$tr(
              style = "background:#f8fafc; text-align:left; border-bottom: 2px solid #e2e8f0;",
              tags$th(style = "padding:12px 8px; font-weight:700; color:#475569; font-size:0.85em; text-transform:uppercase;", "Severity"),
              tags$th(style = "padding:12px 8px; font-weight:700; color:#475569; font-size:0.85em; text-transform:uppercase;", "Category"),
              tags$th(style = "padding:12px 8px; font-weight:700; color:#475569; font-size:0.85em; text-transform:uppercase;", "Section"),
              tags$th(style = "padding:12px 8px; font-weight:700; color:#475569; font-size:0.85em; text-transform:uppercase;", "Location"),
              tags$th(style = "padding:12px 8px; font-weight:700; color:#475569; font-size:0.85em; text-transform:uppercase;", "What is wrong"),
              tags$th(style = "padding:12px 8px; font-weight:700; color:#475569; font-size:0.85em; text-transform:uppercase;", "Suggested fix")
            )
          ),
          tags$tbody(
            lapply(seq_len(nrow(df)), function(i) {
              row <- df[i, ]
              tags$tr(
                style = "border-top:1px solid #f1f5f9;",
                tags$td(
                  style = "padding:12px 8px; vertical-align:top;",
                  tags$span(
                    class = paste0("sev-", row$severity),
                    row$severity
                  )
                ),
                tags$td(style = "padding:12px 8px; vertical-align:top; font-weight:500; font-size:0.9em;", row$category),
                tags$td(
                  style = "padding:12px 8px; vertical-align:top; text-align:center; font-weight:700; color:#1b263b; font-size:0.9em; white-space:nowrap;",
                  if (nzchar(row$section %||% "")) row$section else "—"
                ),
                tags$td(style = "padding:12px 8px; vertical-align:top; color:#64748b; font-size:0.85em;", row$location),
                tags$td(style = "padding:12px 8px; vertical-align:top; font-size:0.9em; line-height:1.4;", row$what_is_wrong),
                tags$td(style = "padding:12px 8px; vertical-align:top; color:#1e293b; font-weight:500; font-size:0.9em; background:#faf5ff; border-radius: 4px; line-height:1.4;", row$suggested_fix)
              )
            })
          )
        )
      )
    }

    tagList(
      div(
        class = "panel-card",
        fluidRow(
          column(
            3,
            div(class = "score-big", paste0(score, "/100")),
            tags$div("Overall QA Score", style = "color:#64748b; font-weight:600; margin-top: 4px; font-family:'Outfit';")
          ),
          column(
            3,
            tags$div(class = paste0("risk-pill risk-", risk), risk),
            tags$div(style = "color:#64748b; margin-top:8px; font-weight:600; font-family:'Outfit';", "Risk Assessment")
          ),
          column(
            6,
            h4("Executive Summary", style = "margin-top: 0; color:#1b263b;"),
            tags$p(d$executive_summary %||% "(no summary)", style = "line-height: 1.5; color: #334155;")
          )
        )
      ),
      div(
        class = "panel-card",
        h4(sprintf(
          "Audit Findings List (%d)",
          if (is.null(d$issues)) 0L else NROW(d$issues)
        ), style = "margin-top:0; color:#1b263b; margin-bottom: 4px;"),
        tags$p(
          style = "font-size:0.78em; color:#94a3b8; margin-bottom:12px;",
          "Section is the document's own numbered section (e.g. 3.1.1), taken from the finding or matched to the table of contents ('—' = not numbered / could not be matched)."
        ),
        issues_html
      ),
      if (length(d$missing_information) > 0) {
        div(
          class = "panel-card",
          h4("Missing Information Checklist", style = "margin-top:0; color:#1b263b;"),
          tags$p("The auditor identified that a standard document of this type should contain the following items, which were missing:", style = "font-size:0.9em; color:#64748b; margin-bottom:12px;"),
          tags$ul(
            style = "padding-left: 20px; line-height: 1.6;",
            lapply(d$missing_information, tags$li)
          )
        )
      },

      # ---- Apply fixes to a Word copy ----
      local({
        fixes <- applicable_fixes()
        have_docx <- !is.null(orig_docx_path())

        if (!have_docx) {
          return(div(
            class = "panel-card", style = "border:1px dashed #cbd5e1; background:#f8fafc;",
            h4("Apply Fixes to a Word Copy", style = "margin-top:0; color:#1b263b;"),
            tags$p(
              style = "color:#64748b; margin:0;",
              "Upload the document as a Word file (.docx) to generate a corrected copy. ",
              "This option is unavailable for PDF/Excel uploads and for audits loaded from history."
            )
          ))
        }

        if (length(fixes) == 0) {
          return(div(
            class = "panel-card", style = "border:1px dashed #cbd5e1; background:#f8fafc;",
            h4("Apply Fixes to a Word Copy", style = "margin-top:0; color:#1b263b;"),
            tags$p(
              style = "color:#64748b; margin:0;",
              "No directly applicable text corrections were returned for this document ",
              "(the issues found are structural or do not map to a verbatim text replacement)."
            )
          ))
        }

        choice_names <- lapply(seq_along(fixes), function(i) {
          f <- fixes[[i]]
          sev <- tolower(f$severity %||% "minor")
          tags$span(
            tags$span(class = paste0("sev-", sev), sev),
            tags$span(style = "margin-left:8px; color:#64748b; font-size:0.85em;", f$location %||% ""),
            tags$div(
              style = "margin:4px 0 10px 0; font-size:0.9em; line-height:1.4;",
              tags$span(
                style = "background:#fee2e2; padding:1px 5px; border-radius:3px; text-decoration:line-through;",
                f$original_text
              ),
              tags$span(style = "margin:0 6px; color:#94a3b8;", HTML("&rarr;")),
              tags$span(
                style = "background:#dcfce7; padding:1px 5px; border-radius:3px; font-weight:600;",
                f$corrected_text
              )
            )
          )
        })

        div(
          class = "panel-card", style = "border:1px solid #bbf7d0; background:#f0fdf4;",
          h4("Apply Fixes to a Word Copy", style = "margin-top:0; color:#166534;"),
          tags$p(
            style = "font-size:0.9em; color:#475569; margin-bottom:14px;",
            sprintf("%d correction(s) can be applied automatically. ", length(fixes)),
            "Tick the ones you want, then download a corrected copy of ",
            tags$strong(orig_docx_name() %||% "your document"),
            ". Your original file is never changed; a Change Log is added to the end of the copy."
          ),
          checkboxGroupInput("apply_fixes", NULL,
            choiceNames  = choice_names,
            choiceValues = as.character(seq_along(fixes)),
            selected     = as.character(seq_along(fixes))
          ),
          div(
            style = "margin-top:6px;",
            actionLink("apply_select_all", "Select all", style = "margin-right:14px;"),
            actionLink("apply_select_none", "Clear all")
          ),
          hr(style = "margin:14px 0;"),
          downloadButton("dl_corrected", "Download corrected .docx",
            class = "btn-success", style = "font-weight:700;"
          )
        )
      })
    )
  })

  # Select-all / clear helpers for the apply-fixes checklist
  observeEvent(input$apply_select_all, {
    n <- length(applicable_fixes())
    if (n > 0) {
      updateCheckboxGroupInput(session, "apply_fixes",
        selected = as.character(seq_len(n))
      )
    }
  })
  observeEvent(input$apply_select_none, {
    updateCheckboxGroupInput(session, "apply_fixes", selected = character(0))
  })

  # History Table Rendering
  output$history_table <- DT::renderDataTable({
    # Reactive dependency on table alterations
    trigger <- history_trigger()

    df <- get_all_reviews()
    if (nrow(df) == 0) {
      return(DT::datatable(
        data.frame(
          Study = character(0),
          Filename = character(0),
          Type = character(0),
          Model = character(0),
          Score = integer(0),
          Risk = character(0),
          Date = character(0),
          Issues = integer(0)
        ),
        options = list(dom = "t")
      ))
    }

    # Reactively filter in R
    if (nzchar(input$search_study)) {
      df <- df[grepl(input$search_study, df$study_code, ignore.case = TRUE), ]
    }
    if (nzchar(input$search_file)) {
      df <- df[grepl(input$search_file, df$filename, ignore.case = TRUE), ]
    }

    if (nrow(df) == 0) {
      return(DT::datatable(df, options = list(dom = "t")))
    }

    # Display framing
    display_df <- data.frame(
      ID = df$id,
      `Study Code` = df$study_code,
      `File Name` = df$filename,
      `Doc Type` = df$doc_type,
      `Model` = df$model,
      `Score` = paste0(df$overall_score, "/100"),
      `Risk Level` = df$risk_level,
      `Audit Date` = df$created_at,
      `Errors Found` = df$issue_count,
      stringsAsFactors = FALSE,
      check.names = FALSE
    )

    DT::datatable(
      display_df,
      selection = "single",
      options = list(
        columnDefs = list(list(targets = 0, visible = FALSE)),
        pageLength = 10,
        dom = "rtip"
      ),
      rownames = FALSE
    )
  })

  observeEvent(input$clear_filters, {
    updateTextInput(session, "search_study", value = "")
    updateTextInput(session, "search_file", value = "")
  })

  # Delete selected audit review
  observeEvent(input$delete_history_review, {
    req(input$history_table_rows_selected)

    history_data <- get_all_reviews()
    if (nzchar(input$search_study)) {
      history_data <- history_data[grepl(input$search_study, history_data$study_code, ignore.case = TRUE), ]
    }
    if (nzchar(input$search_file)) {
      history_data <- history_data[grepl(input$search_file, history_data$filename, ignore.case = TRUE), ]
    }

    selected_row <- input$history_table_rows_selected
    review_id <- history_data$id[selected_row]
    study_code <- history_data$study_code[selected_row]

    ok <- delete_review(review_id)
    history_trigger(history_trigger() + 1)

    if (isTRUE(ok)) {
      showNotification(paste("Deleted review history for Study:", study_code), type = "warning")
    } else {
      showNotification(paste("Could not delete review for Study:", study_code),
        type = "error", session = session
      )
    }
  })

  # Load selected history review back into main terminal
  observeEvent(input$load_history_review, {
    req(input$history_table_rows_selected)

    history_data <- get_all_reviews()
    if (nzchar(input$search_study)) {
      history_data <- history_data[grepl(input$search_study, history_data$study_code, ignore.case = TRUE), ]
    }
    if (nzchar(input$search_file)) {
      history_data <- history_data[grepl(input$search_file, history_data$filename, ignore.case = TRUE), ]
    }

    selected_row <- input$history_table_rows_selected
    review_id <- history_data$id[selected_row]

    detail <- get_review_details(review_id)
    req(detail)

    analysis(list(
      ok = TRUE,
      data = detail$data,
      elapsed_s = 0
    ))
    doc_text("[Document loaded from Mascot history database]")
    # No original file is available for historical records, so disable apply-fixes.
    orig_docx_path(NULL)
    orig_docx_name(NULL)

    # Sync UI controls
    updateSelectInput(session, "doc_type", selected = detail$doc_type)
    updateTextInput(session, "study_code", value = detail$study_code)

    updateTabsetPanel(session, "main_nav", selected = "terminal")
    showNotification(paste("Audit dashboard loaded for Study:", detail$study_code), type = "message")
  })

  # Report downloads
  output$dl_md <- downloadHandler(
    filename = function() {
      paste0(
        "mascot_qa_report_",
        format(Sys.time(), "%Y%m%d_%H%M%S"), ".md"
      )
    },
    content = function(file) {
      a <- analysis()
      req(a, isTRUE(a$ok))
      d <- a$data
      lines <- c(
        "# Mascot Spincontrol QA Review Report",
        paste0("**Generated:** ", format(Sys.time())),
        paste0("**Document Type:** ", input$doc_type),
        paste0("**Study / Document Code:** ", input$study_code),
        paste0("**Source File:** ", if (!is.null(input$file)) {
          input$file$name
        } else {
          "(historical)"
        }),
        paste0("**Model:** ", input$model),
        "",
        "## Overall Score",
        paste0(
          "**", d$overall_score %||% "n/a", " / 100** - Risk Level: **",
          d$risk_level %||% "Unknown", "**"
        ),
        "",
        "## Executive Summary",
        d$executive_summary %||% "(none)",
        "",
        "## Issues & Discrepancies"
      )
      if (NROW(d$issues) > 0) {
        for (i in seq_len(NROW(d$issues))) {
          row <- d$issues[i, ]
          sc <- if (!is.null(row$section) && nzchar(as.character(row$section))) row$section else "n/a"
          lines <- c(
            lines,
            sprintf(
              "### [%s] %s (%s)",
              toupper(row$severity), row$category, row$location
            ),
            paste0("**Section:** ", sc),
            paste0("**What is wrong:** ", row$what_is_wrong),
            paste0("**Suggested correction:** ", row$suggested_fix),
            ""
          )
        }
      } else {
        lines <- c(lines, "_No issues identified._")
      }
      if (length(d$missing_information) > 0) {
        lines <- c(
          lines, "", "## Missing Checklist Items",
          paste0("- ", d$missing_information)
        )
      }
      writeLines(lines, file)
    }
  )

  output$dl_json <- downloadHandler(
    filename = function() {
      paste0(
        "mascot_qa_report_",
        format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"
      )
    },
    content = function(file) {
      a <- analysis()
      req(a, isTRUE(a$ok))
      writeLines(
        jsonlite::toJSON(a$data, auto_unbox = TRUE, pretty = TRUE),
        file
      )
    }
  )

  # Download a corrected copy of the Word document with the selected fixes applied
  output$dl_corrected <- downloadHandler(
    filename = function() {
      base <- orig_docx_name() %||% "document.docx"
      base <- sub("\\.docx$", "", base, ignore.case = TRUE)
      paste0(base, "_QA_corrected_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx")
    },
    content = function(file) {
      src <- orig_docx_path()
      validate(need(!is.null(src), "Original Word document is no longer available."))

      all_fixes <- applicable_fixes()
      sel <- input$apply_fixes
      idx <- suppressWarnings(as.integer(sel))
      idx <- idx[!is.na(idx) & idx >= 1 & idx <= length(all_fixes)]
      chosen <- all_fixes[idx]

      res <- build_corrected_docx(src, chosen)
      print(res$doc, target = file)

      showNotification(
        sprintf(
          "Corrected document ready: %d applied, %d listed for manual review.",
          res$n_applied, res$n_failed
        ),
        type = "message", session = session
      )
    }
  )
}

shinyApp(ui, server)
