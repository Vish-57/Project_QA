# =============================================================================
# mod_settings.R
# Settings panel: shows current Ollama base URL + model list + cache stats.
# Lets admins clear cache and trigger a model refresh.
# =============================================================================

settingsUI <- function(id) {
  ns <- NS(id)
  tagList(
    bslib::card(
      bslib::card_header("Ollama"),
      bslib::card_body(
        verbatimTextOutput(ns("ollama_info")),
        actionButton(ns("refresh"), "Refresh model list",
                     icon = icon("rotate"))
      )
    ),
    bslib::card(
      bslib::card_header("Cache"),
      bslib::card_body(
        verbatimTextOutput(ns("cache_info")),
        actionButton(ns("clear_cache"), "Clear cache",
                     class = "btn-outline-danger",
                     icon = icon("trash"))
      )
    ),
    bslib::card(
      bslib::card_header("About"),
      bslib::card_body(
        tags$p("QA Reviewer v1.0 — local-only AI document review."),
        tags$p("Backend: R Shiny · Ollama (local) · SQLite (default).")
      )
    )
  )
}

settingsServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {

    tick <- reactiveVal(0)
    observeEvent(input$refresh,     tick(tick() + 1))
    observeEvent(input$clear_cache, {
      n <- cache_clear(config$cache$dir %||% "cache")
      showNotification(sprintf("Cleared %d cache entries.", n),
                       type = "message")
      tick(tick() + 1)
    })

    output$ollama_info <- renderText({
      tick()
      url <- ollama_base_url(config)
      up  <- ollama_is_up(config)
      models <- ollama_list_models(config)
      paste0(
        "Endpoint : ", url, "\n",
        "Reachable: ", if (up) "yes" else "no", "\n",
        "Models   : ", if (nrow(models)) paste(models$name, collapse = ", ")
                       else "(none installed)"
      )
    })

    output$cache_info <- renderText({
      tick()
      d <- config$cache$dir %||% "cache"
      files <- list.files(d, pattern = "\\.rds$", full.names = TRUE)
      sz <- if (length(files)) sum(file.info(files)$size, na.rm = TRUE) else 0L
      paste0(
        "Directory: ", normalizePath(d, mustWork = FALSE), "\n",
        "Entries  : ", length(files), "\n",
        "Size     : ", format(structure(sz, class = "object_size"),
                              units = "auto")
      )
    })
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
