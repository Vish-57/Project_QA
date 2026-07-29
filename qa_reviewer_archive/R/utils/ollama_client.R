# =============================================================================
# ollama_client.R
# Thin, robust client for the local Ollama HTTP API.
# Ollama default endpoint: http://localhost:11434
# Docs: https://github.com/ollama/ollama/blob/main/docs/api.md
# =============================================================================

# Required packages: httr2, jsonlite, digest, glue (loaded in global.R)

#' Build the Ollama base URL from app config (config$ollama$host, config$ollama$port).
ollama_base_url <- function(config) {
  host <- config$ollama$host %||% "127.0.0.1"
  port <- config$ollama$port %||% 11434L
  sprintf("http://%s:%s", host, port)
}

#' Check whether the Ollama server is reachable. Returns TRUE/FALSE.
ollama_is_up <- function(config, timeout_sec = 3) {
  url <- paste0(ollama_base_url(config), "/api/tags")
  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_timeout(timeout_sec) |>
      httr2::req_error(is_error = function(r) FALSE) |>
      httr2::req_perform()
    httr2::resp_status(resp) < 500
  }, error = function(e) FALSE)
}

#' List models installed locally in Ollama.
#' Returns a data.frame with columns: name, size_gb, modified.
ollama_list_models <- function(config) {
  url <- paste0(ollama_base_url(config), "/api/tags")
  resp <- tryCatch(
    httr2::request(url) |> httr2::req_timeout(10) |> httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(data.frame(name = character(), size_gb = numeric(),
                                       modified = character(), stringsAsFactors = FALSE))
  body <- httr2::resp_body_json(resp)
  models <- body$models %||% list()
  if (length(models) == 0)
    return(data.frame(name = character(), size_gb = numeric(),
                      modified = character(), stringsAsFactors = FALSE))

  data.frame(
    name     = vapply(models, function(m) m$name %||% "",          character(1)),
    size_gb  = vapply(models, function(m) round((m$size %||% 0) / 1024^3, 2), numeric(1)),
    modified = vapply(models, function(m) m$modified_at %||% "",   character(1)),
    stringsAsFactors = FALSE
  )
}

#' Call Ollama /api/generate (non-streaming).
#' @param prompt   The user prompt (already includes any system/few-shot context).
#' @param model    Ollama model tag, e.g. "llama3.2:3b".
#' @param system   Optional system prompt.
#' @param options  Named list of generation options (temperature, num_ctx, etc.).
#' @param fmt      "text" or "json" (forces JSON-mode in Ollama).
#' @param timeout_sec  Hard timeout for the HTTP call.
ollama_generate <- function(prompt,
                            model,
                            config,
                            system      = NULL,
                            options     = list(),
                            fmt         = c("text", "json"),
                            timeout_sec = 300) {
  fmt <- match.arg(fmt)
  url <- paste0(ollama_base_url(config), "/api/generate")

  body <- list(
    model   = model,
    prompt  = prompt,
    stream  = FALSE,
    options = options
  )
  if (!is.null(system)) body$system <- system
  if (fmt == "json")    body$format <- "json"

  resp <- httr2::request(url) |>
    httr2::req_method("POST") |>
    httr2::req_body_json(body, auto_unbox = TRUE) |>
    httr2::req_timeout(timeout_sec) |>
    httr2::req_retry(max_tries = 2, backoff = function(i) 2 ^ i) |>
    httr2::req_perform()

  parsed <- httr2::resp_body_json(resp)
  list(
    response = parsed$response %||% "",
    model    = parsed$model    %||% model,
    eval_count = parsed$eval_count %||% NA_integer_,
    eval_duration_ms = round((parsed$eval_duration %||% 0) / 1e6),
    total_duration_ms = round((parsed$total_duration %||% 0) / 1e6),
    raw = parsed
  )
}

#' Convenience: ask Ollama for a structured JSON response and parse it.
#' Returns list(ok = TRUE/FALSE, data = parsed_list_or_NULL, raw_text, meta).
ollama_generate_json <- function(prompt, model, config, system = NULL,
                                 options = list(), timeout_sec = 300) {
  res <- ollama_generate(prompt, model, config, system = system,
                         options = options, fmt = "json",
                         timeout_sec = timeout_sec)
  parsed <- tryCatch(jsonlite::fromJSON(res$response, simplifyVector = FALSE),
                     error = function(e) NULL)
  list(
    ok       = !is.null(parsed),
    data     = parsed,
    raw_text = res$response,
    meta     = list(eval_count = res$eval_count,
                    eval_duration_ms = res$eval_duration_ms,
                    total_duration_ms = res$total_duration_ms,
                    model = res$model)
  )
}

#' Compute embeddings (used for semantic dedup / chunk-similarity checks).
#' Returns numeric vector or NULL on failure.
ollama_embed <- function(text, model, config, timeout_sec = 60) {
  url <- paste0(ollama_base_url(config), "/api/embeddings")
  body <- list(model = model, prompt = text)
  resp <- tryCatch(
    httr2::request(url) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_timeout(timeout_sec) |>
      httr2::req_perform(),
    error = function(e) NULL
  )
  if (is.null(resp)) return(NULL)
  parsed <- httr2::resp_body_json(resp)
  unlist(parsed$embedding %||% list(), use.names = FALSE)
}

# ----- Small helpers --------------------------------------------------------

`%||%` <- function(a, b) if (is.null(a) || (length(a) == 1 && is.na(a))) b else a
