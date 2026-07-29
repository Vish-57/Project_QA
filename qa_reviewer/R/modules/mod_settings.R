settingsUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Settings"),
    h5("Ollama Status"),
    uiOutput(ns("ollama_status")),
    hr(),
    h5("Cache"),
    actionButton(ns("clear_cache"), "Clear Cache", class = "btn-warning"),
    hr(),
    h5("About"),
    p("QA Reviewer v2.0 — Consolidated version with parallel processing, JSON repair pipeline, numeric verification, and deterministic pre-scan.")
  )
}

settingsServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    output$ollama_status <- renderUI({
      up <- ollama_is_up(config)
      div(style = paste("padding:8px; border-radius:4px;", if (up) "background:#dcfce7; color:#166534;" else "background:#fee2e2; color:#991b1b;"), if (up) "Ollama is running." else "Ollama is not reachable.")
    })
    observeEvent(input$clear_cache, {
      n <- cache_clear(config$cache$dir %||% "cache")
      showNotification(paste("Cleared", n, "cache entries."), type = "message")
    })
  })
}
