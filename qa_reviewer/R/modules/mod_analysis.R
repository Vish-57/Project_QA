analysisUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Analysis Configuration"),
    uiOutput(ns("model_ui")),
    numericInput(ns("workers"), "Parallel workers", value = 2, min = 1, max = 16),
    textAreaInput(ns("remarks"), "Remarks for this audit", rows = 2, placeholder = "e.g. This is a draft v0.9 - focus on statistics."),
    uiOutput(ns("eta_box")),
    actionButton(ns("run"), "Analyse document", class = "btn-primary", style = "width: 100%; font-weight: 700;"),
    br(),
    uiOutput(ns("status"))
  )
}

analysisServer <- function(id, parsed, doc_type, file_info, con, current_user, config) {
  moduleServer(id, function(input, output, session) {
    models <- reactiveVal(character(0))
    analysis <- reactiveVal(NULL)
    running <- reactiveVal(FALSE)
    applicable_fixes <- reactiveVal(list())
    ollama_url <- paste0("http://", config$ollama$host %||% "127.0.0.1", ":", config$ollama$port %||% 11434)
    refresh_models <- function() {
      m <- tryCatch({ r <- httr2::request(paste0(ollama_url, "/api/tags")) |> httr2::req_timeout(5) |> httr2::req_perform(); b <- httr2::resp_body_json(r); vapply(b$models %||% list(), function(m) m$name, character(1)) }, error = function(e) character(0))
      models(m)
    }
    refresh_models()
    output$model_ui <- renderUI({
      m <- models()
      preferred <- c("llama3.2:3b", "qwen2.5:3b", "phi3.5:3.8b", "gemma2:2b")
      default <- intersect(preferred, m)[1]
      if (is.na(default)) default <- m[1]
      selectInput(session$ns("model"), "Ollama model", choices = if (length(m)) m else "(no models installed)", selected = default)
    })
    output$eta_box <- renderUI({
      txt <- parsed(); m <- input$model
      if (is.null(txt) || is.null(m) || m == "(no models installed)") return(NULL)
      n_chars <- nchar(txt); est_sec <- max(30, ceiling(n_chars / 200))
      div(style = "background:#ecfdf5; border-left:4px solid #10b981; padding:8px 10px; border-radius:4px; margin-bottom:10px;", tags$div(tags$strong("Estimated time: "), sprintf("~%d min %d sec", est_sec %/% 60, est_sec %% 60)))
    })
    output$status <- renderUI({
      s <- if (length(models()) == 0) "Ollama not reachable. Start Ollama Desktop first." else paste("Ollama OK -", length(models()), "models.")
      div(style = "font-size:0.82em; color:#475569; background:#f1f5f9; padding:10px; border-radius:8px;", s)
    })
    observeEvent(input$run, {
      txt <- parsed(); req(txt)
      if (is.null(input$model) || input$model == "(no models installed)") { showNotification("Pick an installed Ollama model first.", type = "warning"); return(NULL) }
      running(TRUE); analysis(NULL)
      .text <- txt; .model <- input$model; .doctype <- doc_type(); .config <- config; .remarks <- input$remarks %||% ""
      later::later(function() {
        tryCatch({
          res <- tryCatch(run_audit(.text, .model, .config, .doctype, custom_guidelines = .remarks, progress = function(msg) message(msg)), error = function(e) { showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 12); NULL })
          if (!is.null(res)) analysis(res)
        }, error = function(e) message("analysis error: ", conditionMessage(e)))
        running(FALSE)
      }, delay = 0.1)
    })
    list(analysis = analysis, running = running, applicable_fixes = applicable_fixes)
  })
}
