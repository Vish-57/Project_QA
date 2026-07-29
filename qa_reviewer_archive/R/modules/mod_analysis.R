# =============================================================================
# mod_analysis.R
# Model picker + "Run analysis" trigger. Runs run_qa_analysis() asynchronously
# via promises/future so the UI stays responsive. Returns reactives:
#   analysis() : list or NULL
#   running()  : logical
# =============================================================================

analysisUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("2. Choose model & run analysis"),
    bslib::card_body(
      fluidRow(
        column(6, uiOutput(ns("model_select_ui"))),
        column(6,
          sliderInput(ns("workers"), "Parallel workers",
                      min = 1, max = 4, value = 2, step = 1)
        )
      ),
      div(
        class = "run-row",
        actionButton(ns("run"), "Run QA analysis",
                     class = "btn-primary",
                     icon = icon("play")),
        actionButton(ns("refresh_models"), "Refresh models",
                     icon = icon("rotate")),
        uiOutput(ns("ollama_status"))
      ),
      uiOutput(ns("progress_ui"))
    )
  )
}

analysisServer <- function(id, parsed, doc_type, file_info,
                           con, current_user, config) {
  moduleServer(id, function(input, output, session) {

    analysis_rv <- reactiveVal(NULL)
    running     <- reactiveVal(FALSE)
    progress_rv <- reactiveVal(list(frac = 0, msg = ""))

    models_rv <- reactiveVal(NULL)
    refresh_models <- function() {
      models_rv(ollama_list_models(config))
    }
    refresh_models()
    observeEvent(input$refresh_models, refresh_models())

    output$ollama_status <- renderUI({
      if (ollama_is_up(config)) {
        span(class = "ollama-ok",
             icon("check-circle"), " Ollama reachable")
      } else {
        span(class = "ollama-bad",
             icon("triangle-exclamation"),
             " Ollama not reachable at ", ollama_base_url(config))
      }
    })

    output$model_select_ui <- renderUI({
      models <- models_rv()
      choices <- if (!is.null(models) && nrow(models) > 0)
        models$name else config$ollama$fallback_models
      default <- if (!is.null(choices) &&
                     config$ollama$default_model %in% choices)
        config$ollama$default_model
      else choices[1]
      selectInput(session$ns("model"), "Ollama model",
                  choices = choices, selected = default)
    })

    observeEvent(input$run, {
      p <- parsed(); req(p)
      if (running()) {
        showNotification("Analysis already running.", type = "warning")
        return(NULL)
      }
      running(TRUE)
      progress_rv(list(frac = 0.02, msg = "Starting…"))

      # Snapshot reactive inputs (avoid touching reactives inside future).
      .parsed   <- p
      .model    <- input$model
      .doc_type <- doc_type()
      .config   <- config
      .config$analysis$workers <- input$workers
      .user_email <- current_user()$email

      # Run synchronously inside withProgress — simpler, robust on Windows.
      # For true async, swap to promises::future_promise().
      withProgress(message = "Analysing…", value = 0.05, {
        prog <- function(frac, msg) {
          progress_rv(list(frac = frac, msg = msg))
          setProgress(value = frac, message = msg)
        }

        rid <- db_insert_review(con, list(
          user_email = .user_email,
          filename   = .parsed$filename,
          doc_type   = .doc_type,
          model      = .model,
          status     = "running",
          started_at = Sys.time()
        ))

        res <- tryCatch(
          run_qa_analysis(.parsed, .model, .config,
                          doc_type = .doc_type, progress = prog),
          error = function(e) list(ok = FALSE, error = conditionMessage(e))
        )

        if (isFALSE(res$ok)) {
          db_update_review(con, rid,
            list(status = "error",
                 finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                 error = res$error %||% "unknown"))
          showNotification(paste("Analysis failed:", res$error),
                           type = "error", duration = 12)
          running(FALSE)
          return(NULL)
        }

        # Persist results
        db_insert_issues(con, rid, res$issues)
        db_update_review(con, rid, list(
          status        = "complete",
          finished_at   = format(res$finished_at, "%Y-%m-%d %H:%M:%S"),
          duration_s    = res$duration_s,
          overall_score = res$summary$overall_score,
          risk_level    = res$summary$risk_level,
          n_critical    = res$summary$n_critical,
          n_major       = res$summary$n_major,
          n_minor       = res$summary$n_minor
        ))
        db_audit(con, .user_email, "analysis.complete",
                 target = .parsed$filename,
                 meta = list(review_id = rid,
                             score = res$summary$overall_score))

        res$review_id <- rid
        analysis_rv(res)
        running(FALSE)
      })
    })

    output$progress_ui <- renderUI({
      st <- progress_rv()
      if (!running() && st$frac == 0) return(NULL)
      div(class = "progress-row",
        tags$div(class = "progress",
          tags$div(class = "progress-bar",
                   role = "progressbar",
                   style = sprintf("width: %.0f%%", st$frac * 100),
                   sprintf("%.0f%%", st$frac * 100))),
        tags$div(class = "progress-msg", st$msg)
      )
    })

    list(
      analysis = analysis_rv,
      running  = running
    )
  })
}

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
