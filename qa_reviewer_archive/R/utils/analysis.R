# =============================================================================
# analysis.R
# Orchestrates the full QA pipeline:
#   parse -> chunk -> per-chunk LLM pass -> structural audit ->
#   compliance audit -> executive aggregation.
# =============================================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

run_qa_analysis <- function(parsed, model, config,
                            doc_type = "Other",
                            progress = NULL) {

  t0 <- Sys.time()
  step <- function(frac, msg) if (is.function(progress)) progress(frac, msg)

  step(0.05, "Splitting document into chunks...")
  chunks <- build_chunks(parsed,
                         chunk_chars = config$analysis$chunk_chars %||% 6000L,
                         overlap     = config$analysis$chunk_overlap %||% 400L)
  if (nrow(chunks) == 0)
    return(list(ok = FALSE, error = "Document yielded no chunks."))

  max_chunks <- config$analysis$max_chunks %||% 40L
  if (nrow(chunks) > max_chunks) {
    chunks <- chunks[seq_len(max_chunks), , drop = FALSE]
    message(sprintf("Capping analysis at first %d chunks.", max_chunks))
  }

  step(0.10, "Analysing chunks with the local model...")
  per_chunk <- analyse_chunks_parallel(
    chunks, model, config, doc_type,
    progress = function(f, m) step(0.10 + 0.55 * f, m)
  )
  issues_df      <- per_chunk$issues
  per_chunk_meta <- per_chunk$meta

  step(0.70, "Auditing document structure...")
  structure_audit <- audit_structure(parsed, model, config, doc_type)

  step(0.82, "Running compliance scan...")
  compliance_audit <- audit_compliance(parsed, model, config, doc_type)

  step(0.92, "Aggregating executive summary...")
  summary_obj <- aggregate_summary(parsed, issues_df,
                                   structure_audit, compliance_audit,
                                   model, config, doc_type)

  step(1.00, "Done.")
  list(
    ok          = TRUE,
    started_at  = t0,
    finished_at = Sys.time(),
    duration_s  = as.numeric(difftime(Sys.time(), t0, units = "secs")),
    model       = model,
    doc_type    = doc_type,
    n_chunks    = nrow(chunks),
    chunks      = chunks,
    issues      = issues_df,
    structure   = structure_audit,
    compliance  = compliance_audit,
    summary     = summary_obj,
    per_chunk_meta = per_chunk_meta
  )
}

analyse_chunks_parallel <- function(chunks, model, config, doc_type,
                                    progress = NULL) {
  workers <- min(config$analysis$workers %||% 1L, nrow(chunks))
  if (workers < 1L) workers <- 1L

  N <- nrow(chunks)
  prompt_template <- load_prompt("chunk_analysis")
  options_llm     <- llm_options(config)
  cache_dir       <- config$cache$dir %||% "cache"
  app_root        <- normalizePath(getwd(), winslash = "/", mustWork = FALSE)

  one_chunk <- function(i) {
    chunk <- chunks[i, , drop = FALSE]
    prompt <- fill_prompt(prompt_template, list(
      DOC_TYPE = doc_type,
      HEADING  = chunk$heading,
      CHUNK_ID = chunk$id,
      TEXT     = chunk$text
    ))
    key <- cache_key(model, prompt, options_llm, SYSTEM_QA_REVIEWER)
    cached <- cache_get(key, cache_dir)
    if (!is.null(cached)) return(cached)
    res <- tryCatch(
      ollama_generate_json(prompt, model, config,
                           system  = SYSTEM_QA_REVIEWER,
                           options = options_llm,
                           timeout_sec = config$analysis$timeout_sec %||% 300),
      error = function(e) list(ok = FALSE, data = NULL, raw_text = "",
                                meta = list(error = conditionMessage(e)))
    )
    cache_put(key, res, cache_dir)
    res
  }

  if (workers <= 1L) {
    results <- lapply(seq_len(N), one_chunk)
  } else {
    ok <- tryCatch({
      future::plan(future::multisession, workers = workers); TRUE
    }, error = function(e) FALSE)
    on.exit(future::plan(future::sequential), add = TRUE)
    if (!ok) {
      results <- lapply(seq_len(N), one_chunk)
    } else {
      util_files <- list.files(file.path(app_root, "R", "utils"),
                               pattern = "\\.R$", full.names = TRUE)
      results <- future.apply::future_lapply(
        seq_len(N), function(i) {
          for (f in util_files) source(f, local = FALSE)
          one_chunk(i)
        },
        future.seed     = TRUE,
        future.packages = c("httr2", "jsonlite", "digest"),
        future.globals  = list(
          chunks = chunks, model = model, config = config,
          doc_type = doc_type, prompt_template = prompt_template,
          options_llm = options_llm, cache_dir = cache_dir,
          util_files = util_files, one_chunk = one_chunk,
          SYSTEM_QA_REVIEWER = SYSTEM_QA_REVIEWER
        )
      )
    }
  }

  issues <- list(); meta <- list()
  for (i in seq_along(results)) {
    r <- results[[i]]
    chunk <- chunks[i, , drop = FALSE]
    meta[[i]] <- list(chunk_id = chunk$id,
                      eval_ms  = r$meta$total_duration_ms,
                      ok       = isTRUE(r$ok))
    data <- r$data
    if (is.null(data) || is.null(data$issues)) next
    for (issue in data$issues) {
      issues[[length(issues) + 1L]] <- data.frame(
        chunk_id    = chunk$id,
        location    = chunk$heading,
        category    = issue$category %||% "other",
        severity    = tolower(issue$severity %||% "minor"),
        snippet     = issue$snippet %||% "",
        description = issue$description %||% "",
        suggestion  = issue$suggestion %||% "",
        stringsAsFactors = FALSE
      )
    }
    if (is.function(progress)) progress(i / N, sprintf("Chunk %d/%d", i, N))
  }
  issues_df <- if (length(issues)) do.call(rbind, issues) else
    data.frame(chunk_id = integer(), location = character(),
               category = character(), severity = character(),
               snippet = character(), description = character(),
               suggestion = character(), stringsAsFactors = FALSE)
  list(issues = issues_df, meta = meta)
}

audit_structure <- function(parsed, model, config, doc_type) {
  template <- load_prompt("structure_audit")
  prompt <- fill_prompt(template, list(
    DOC_TYPE = doc_type,
    HEADINGS = if (nrow(parsed$sections) == 0) "(none detected)" else
      paste(sprintf("- %s", parsed$sections$heading), collapse = "\n")
  ))
  key <- cache_key(model, prompt, llm_options(config), SYSTEM_QA_REVIEWER)
  cached <- cache_get(key, config$cache$dir %||% "cache")
  if (!is.null(cached)) return(cached$data)
  res <- tryCatch(
    ollama_generate_json(prompt, model, config,
                         system = SYSTEM_QA_REVIEWER,
                         options = llm_options(config),
                         timeout_sec = 180),
    error = function(e) list(ok = FALSE, data = list(error = conditionMessage(e))))
  cache_put(key, res, config$cache$dir %||% "cache")
  res$data
}

audit_compliance <- function(parsed, model, config, doc_type) {
  template <- load_prompt("compliance_audit")
  txt <- parsed$text
  excerpt <- if (nchar(txt) <= 12000L) txt else
    paste0(substr(txt, 1L, 6000L), "\n\n[...middle truncated...]\n\n",
           substr(txt, nchar(txt) - 6000L + 1L, nchar(txt)))
  prompt <- fill_prompt(template, list(
    DOC_TYPE = doc_type, EXCERPT = excerpt
  ))
  key <- cache_key(model, prompt, llm_options(config), SYSTEM_QA_REVIEWER)
  cached <- cache_get(key, config$cache$dir %||% "cache")
  if (!is.null(cached)) return(cached$data)
  res <- tryCatch(
    ollama_generate_json(prompt, model, config,
                         system = SYSTEM_QA_REVIEWER,
                         options = llm_options(config),
                         timeout_sec = 240),
    error = function(e) list(ok = FALSE, data = list(error = conditionMessage(e))))
  cache_put(key, res, config$cache$dir %||% "cache")
  res$data
}

aggregate_summary <- function(parsed, issues_df, structure_audit,
                              compliance_audit, model, config, doc_type) {
  sev_counts <- table(factor(issues_df$severity,
                             levels = c("critical", "major", "minor")))
  n_crit  <- as.integer(sev_counts["critical"]); if (is.na(n_crit))  n_crit  <- 0L
  n_major <- as.integer(sev_counts["major"]);    if (is.na(n_major)) n_major <- 0L
  n_minor <- as.integer(sev_counts["minor"]);    if (is.na(n_minor)) n_minor <- 0L

  score <- as.integer(max(0, 100 - n_crit*12 - n_major*4 - n_minor*1))
  risk_level <- if (score >= 85) "Low"
                else if (score >= 70) "Medium"
                else if (score >= 50) "High"
                else "Critical"

  template <- load_prompt("executive_summary")
  prompt <- fill_prompt(template, list(
    DOC_TYPE = doc_type, FILENAME = parsed$filename,
    N_PAGES = parsed$n_pages, N_WORDS = parsed$n_words,
    SCORE = score, RISK = risk_level,
    N_CRIT = n_crit, N_MAJOR = n_major, N_MINOR = n_minor,
    TOP_ISSUES = format_top_issues(issues_df, n = 10),
    STRUCTURE  = jsonlite::toJSON(structure_audit %||% list(), auto_unbox = TRUE),
    COMPLIANCE = jsonlite::toJSON(compliance_audit %||% list(), auto_unbox = TRUE)
  ))
  res <- tryCatch(
    ollama_generate_json(prompt, model, config,
                         system  = SYSTEM_QA_REVIEWER,
                         options = llm_options(config, low_temp = TRUE),
                         timeout_sec = 240),
    error = function(e) list(ok = FALSE, data = NULL))
  narrative <- if (isTRUE(res$ok) && !is.null(res$data)) res$data else list()

  list(
    overall_score       = score,
    risk_level          = risk_level,
    n_critical          = n_crit,
    n_major             = n_major,
    n_minor             = n_minor,
    executive_summary   = narrative$executive_summary   %||% "",
    key_recommendations = narrative$key_recommendations %||% list(),
    missing_information = narrative$missing_information %||% list()
  )
}

format_top_issues <- function(issues_df, n = 10) {
  if (nrow(issues_df) == 0) return("(no issues found)")
  ord <- order(factor(issues_df$severity,
                      levels = c("critical", "major", "minor")))
  top <- issues_df[ord, ][seq_len(min(n, nrow(issues_df))), ]
  paste(sprintf("- [%s] %s: %s",
                toupper(top$severity), top$category,
                substr(top$description, 1, 200)),
        collapse = "\n")
}

llm_options <- function(config, low_temp = FALSE) {
  list(
    temperature = if (low_temp) 0.1 else (config$analysis$temperature %||% 0.2),
    top_p       = config$analysis$top_p       %||% 0.9,
    num_ctx     = config$analysis$num_ctx     %||% 8192L,
    num_predict = config$analysis$num_predict %||% 1024L
  )
}
