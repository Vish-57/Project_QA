library(shiny)
source("global.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$style(HTML("
      body { background: #f7f9fc; font-family: 'Segoe UI', system-ui, sans-serif; }
      .panel-card { background: white; border-radius: 12px; padding: 20px; box-shadow: 0 2px 12px rgba(0,0,0,0.04); border: 1px solid #e2e8f0; margin-bottom: 16px; }
      .mascot-header { background: linear-gradient(135deg, #0d1b2a, #1b263b); color: white; padding: 16px 24px; border-radius: 0 0 12px 12px; margin-bottom: 20px; display: flex; justify-content: space-between; align-items: center; }
      .risk-Low { background: #10b981; color: white; padding: 4px 12px; border-radius: 999px; font-weight: 700; }
      .risk-Medium { background: #3b82f6; color: white; padding: 4px 12px; border-radius: 999px; font-weight: 700; }
      .risk-High { background: #f59e0b; color: white; padding: 4px 12px; border-radius: 999px; font-weight: 700; }
      .risk-Critical { background: #ef4444; color: white; padding: 4px 12px; border-radius: 999px; font-weight: 700; }
      .sev-critical { background: #fee2e2; color: #ef4444; padding: 2px 8px; border-radius: 4px; font-weight: 700; font-size: 0.8em; }
      .sev-major { background: #fef3c7; color: #d97706; padding: 2px 8px; border-radius: 4px; font-weight: 700; font-size: 0.8em; }
      .sev-minor { background: #dbeafe; color: #2563eb; padding: 2px 8px; border-radius: 4px; font-weight: 700; font-size: 0.8em; }
      @keyframes fillBar { from { width: 0%; } to { width: 95%; } }
    "))
  ),
  div(class = "mascot-header",
    div(h3("QA Reviewer", style = "margin:0; font-weight:700;"), p("AI-Powered Quality Assurance", style = "margin:2px 0 0 0; font-size:0.85em; color:#a5a5a5;")),
    uiOutput("user_chip")
  ),
  uiOutput("app_shell")
)

server <- function(input, output, session) {
  current_user <- reactiveVal(NULL)

  output$app_shell <- renderUI({
    if (is.null(current_user())) {
      return(div(style = "max-width:400px; margin:60px auto;",
        div(class = "panel-card",
          h4("Login", style = "margin-top:0;"),
          textInput("auth_email", "Email", value = "admin@local"),
          passwordInput("auth_password", "Password"),
          actionButton("auth_login", "Login", class = "btn-primary", style = "width:100%; font-weight:700;"),
          br(), br(),
          uiOutput("auth_error")
        )
      ))
    }
    tagList(
      tabsetPanel(id = "main_tab",
        tabPanel("Review", value = "review",
          sidebarLayout(
            sidebarPanel(width = 3,
              div(class = "panel-card", style = "padding:16px;",
                textInput("study_code", "Study / Document Code", placeholder = "e.g. ST-2026-001"),
                fileInput("file", "Upload document", accept = c(".pdf", ".docx", ".xlsx", ".xls", ".txt")),
                selectInput("doc_type", "Document type", choices = c("Protocol", "CSR", "SOP", "SAP", "Study Report", "Other")),
                uiOutput("model_ui"),
                numericInput("workers", "Parallel workers", value = 2, min = 1, max = 16),
                textAreaInput("remarks", "Remarks for this audit", rows = 2, placeholder = "e.g. Focus on statistics section."),
                uiOutput("eta_box"),
                actionButton("analyze", "Analyse document", class = "btn-primary", style = "width:100%; font-weight:700; padding:10px;"),
                br(), br(),
                uiOutput("status")
              )
            ),
            mainPanel(width = 9,
              uiOutput("progress_ui"),
              uiOutput("results_ui")
            )
          )
        ),
        tabPanel("History", value = "history",
          div(class = "panel-card",
            h4("Study History", style = "margin-top:0;"),
            DT::DTOutput("history_table"),
            br(),
            div(actionButton("load_history", "View Selected", class = "btn-primary"), actionButton("delete_history", "Delete Selected", class = "btn-danger"))
          )
        ),
        tabPanel("Settings", value = "settings",
          div(class = "panel-card",
            h4("Settings", style = "margin-top:0;"),
            h5("Ollama Status"), uiOutput("ollama_status"), hr(),
            h5("Cache"), actionButton("clear_cache", "Clear Cache", class = "btn-warning"), hr(),
            p("QA Reviewer v2.0 — Consolidated")
          )
        )
      ),
      if (!is.null(current_user())) {
        div(style = "text-align:right; padding:8px 16px;",
          actionLink("logout", "Logout", icon = icon("sign-out-alt"))
        )
      }
    )
  })

  # Auth
  observeEvent(input$auth_login, {
    u <- auth_login(DB, input$auth_email, input$auth_password)
    if (is.null(u)) {
      output$auth_error <- renderUI(div(style = "color:red;", "Invalid email or password."))
    } else {
      db_audit(DB, u$email, "login")
      current_user(list(email = u$email, display_name = u$display_name, role = u$role))
    }
  })

  observeEvent(input$logout, {
    u <- current_user()
    if (!is.null(u)) db_audit(DB, u$email, "logout")
    current_user(NULL)
  })

  output$user_chip <- renderUI({
    u <- current_user()
    if (is.null(u)) return(NULL)
    tags$span(style = "color:#e2e8f0; font-size:0.9em;", icon("user"), " ", u$display_name %||% u$email)
  })

  # Models
  models <- reactiveVal(character(0))
  refresh_models <- function() {
    m <- tryCatch({
      r <- httr2::request("http://127.0.0.1:11434/api/tags") |> httr2::req_timeout(5) |> httr2::req_perform()
      b <- httr2::resp_body_json(r)
      vapply(b$models %||% list(), function(m) m$name, character(1))
    }, error = function(e) character(0))
    models(m)
  }
  refresh_models()

  output$model_ui <- renderUI({
    m <- models()
    preferred <- c("llama3.2:3b", "qwen2.5:3b", "phi3.5:3.8b", "gemma2:2b")
    default <- intersect(preferred, m)[1]
    if (is.na(default)) default <- m[1]
    selectInput("model", "Ollama model", choices = if (length(m)) m else "(no models installed)", selected = default)
  })

  output$status <- renderUI({
    s <- if (length(models()) == 0) "Ollama not reachable. Start Ollama Desktop first." else paste("Ollama OK -", length(models()), "models.")
    div(style = "font-size:0.85em; color:#475569; background:#f1f5f9; padding:10px; border-radius:8px;", s)
  })

  output$ollama_status <- renderUI({
    up <- ollama_is_up(APP_CONFIG)
    div(style = paste("padding:8px; border-radius:4px;", if (up) "background:#dcfce7; color:#166534;" else "background:#fee2e2; color:#991b1b;"), if (up) "Ollama is running." else "Ollama is not reachable.")
  })

  observeEvent(input$clear_cache, {
    n <- cache_clear(APP_CONFIG$cache$dir %||% "cache")
    showNotification(paste("Cleared", n, "cache entries."), type = "message")
  })

  # Document state
  doc_text <- reactiveVal(NULL)
  analysis <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  pdf_pages <- reactiveVal(NULL)
  toc_map <- reactiveVal(NULL)
  orig_docx_path <- reactiveVal(NULL)
  orig_docx_name <- reactiveVal(NULL)

  observeEvent(input$file, {
    f <- input$file; req(f)
    ext <- tolower(tools::file_ext(f$name))
    txt <- tryCatch(read_doc(f$datapath, ext), error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error", duration = 10); NULL })
    if (!is.null(txt)) {
      doc_text(txt); analysis(NULL)
      if (ext == "docx") { orig_docx_path(f$datapath); orig_docx_name(f$name); tm <- tryCatch(parse_toc_pages(paste(docx_paragraph_texts(f$datapath), collapse = "\n")), error = function(e) NULL); toc_map(tm); pdf_pages(NULL) }
      else { orig_docx_path(NULL); orig_docx_name(NULL); toc_map(NULL); if (ext == "pdf") { pp <- tryCatch(pdftools::pdf_text(f$datapath), error = function(e) NULL); pdf_pages(pp) } else { pdf_pages(NULL) } }
      showNotification(sprintf("Loaded '%s' (%s chars)", f$name, format(nchar(txt), big.mark = ",")), type = "message")
    }
  })

  output$eta_box <- renderUI({
    txt <- doc_text(); m <- input$model
    if (is.null(txt) || is.null(m) || m == "(no models installed)") return(NULL)
    est_sec <- max(30, ceiling(nchar(txt) / 200))
    div(style = "background:#ecfdf5; border-left:4px solid #10b981; padding:8px 10px; border-radius:4px; margin-bottom:10px;", tags$div(tags$strong("Estimated time: "), sprintf("~%d min %d sec", est_sec %/% 60, est_sec %% 60)))
  })

  output$progress_ui <- renderUI({
    if (!isTRUE(running())) return(NULL)
    div(class = "panel-card", h4("Analysing document...", style = "margin-top:0;"), div(style = "background:#e2e8f0; border-radius:8px; height:16px; overflow:hidden;", div(style = "height:100%; width:95%; background:linear-gradient(90deg,#1b263b,#415a77); animation:fillBar 30s ease-out;")), tags$p("Processing chunks...", style = "color:#64748b; font-size:0.85em;"))
  })

  # Analyse
  observeEvent(input$analyze, {
    txt <- doc_text(); req(txt)
    if (is.null(input$model) || input$model == "(no models installed)") { showNotification("Pick an installed Ollama model first.", type = "warning"); return(NULL) }
    running(TRUE); analysis(NULL)
    .text <- txt; .model <- input$model; .doctype <- input$doc_type; .remarks <- input$remarks %||% ""
    .pdf_pages <- pdf_pages(); .toc_map <- toc_map()
    later::later(function() {
      tryCatch({
        res <- tryCatch(run_audit(.text, .model, APP_CONFIG, .doctype, custom_guidelines = .remarks, page_texts = .pdf_pages, toc_map = .toc_map, progress = function(msg) message(msg)), error = function(e) { showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 12); NULL })
        if (!is.null(res)) analysis(res)
      }, error = function(e) message("analysis error: ", conditionMessage(e)))
      running(FALSE)
    }, delay = 0.1)
  })

  # Results
  output$results_ui <- renderUI({
    a <- analysis()
    if (is.null(a)) return(div(class = "panel-card", style = "text-align:center; color:#64748b; padding:40px;", p("Upload a document and click Analyse.")))
    if (!isTRUE(a$ok)) return(div(class = "panel-card", h4("Analysis failed", style = "color:#b91c1c;"), pre(a$raw_text %||% "No output")))
    d <- a$data
    score <- d$overall_score %||% 0; risk <- d$risk_level %||% "Unknown"
    tagList(
      div(class = "panel-card",
        fluidRow(
          column(3, h2(paste0(score, "/100"), style = "margin:0; font-weight:800;"), p("Overall Score", style = "color:#64748b;")),
          column(3, div(class = paste0("risk-", risk), risk), p("Risk Level", style = "color:#64748b; margin-top:8px;")),
          column(6, h5("Executive Summary", style = "margin-top:0;"), p(d$executive_summary %||% "(no summary)"))
        )
      ),
      if (!is.null(d$issues) && NROW(d$issues) > 0) {
        df <- as.data.frame(d$issues, stringsAsFactors = FALSE)
        for (col in c("severity","category","location","what_is_wrong","suggested_fix","section","page")) if (is.null(df[[col]])) df[[col]] <- ""
        div(class = "panel-card",
          h5(sprintf("Findings (%d)", nrow(df)), style = "margin-top:0;"),
          DT::datatable(df[, c("severity","category","section","location","what_is_wrong","suggested_fix")], selection = "single", options = list(pageLength = 15, dom = "ftip"), rownames = FALSE) |> DT::formatStyle("severity", target = "row", backgroundColor = DT::styleEqual(c("critical","major"), c("#fee2e2","#fef3c7")))
        )
      },
      if (length(d$missing_information) > 0) div(class = "panel-card", h5("Missing Information", style = "margin-top:0;"), tags$ul(lapply(d$missing_information, tags$li))),
      div(class = "panel-card",
        downloadButton("dl_md", "Download Report (.md)", class = "btn-outline-primary"),
        downloadButton("dl_json", "Raw JSON", class = "btn-outline-secondary")
      )
    )
  })

  output$dl_md <- downloadHandler(
    filename = function() paste0("qa_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".md"),
    content = function(file) {
      a <- analysis(); req(a, isTRUE(a$ok)); d <- a$data
      lines <- c("# QA Review Report", paste("**Score:**", d$overall_score, "/ 100 — Risk:", d$risk_level), "", "## Executive Summary", d$executive_summary %||% "", "", "## Issues")
      if (NROW(d$issues) > 0) for (i in seq_len(NROW(d$issues))) { r <- d$issues[i,]; lines <- c(lines, sprintf("- [%s] %s: %s", toupper(r$severity), r$category, r$what_is_wrong)) }
      writeLines(lines, file)
    }
  )

  output$dl_json <- downloadHandler(
    filename = function() paste0("qa_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
    content = function(file) { a <- analysis(); req(a, isTRUE(a$ok)); writeLines(jsonlite::toJSON(a$data, auto_unbox = TRUE, pretty = TRUE), file) }
  )

  # History
  history_trigger <- reactiveVal(0)
  observeEvent(analysis(), { history_trigger(history_trigger() + 1) }, ignoreNULL = TRUE)

  output$history_table <- DT::renderDT({
    history_trigger()
    df <- db_list_reviews(DB, limit = 200)
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No reviews yet."), options = list(dom = "t")))
    show <- data.frame(ID = df$id, File = df$filename, Type = df$doc_type, Model = df$model, Score = df$overall_score, Risk = df$risk_level, Date = df$started_at, stringsAsFactors = FALSE)
    DT::datatable(show, selection = "single", options = list(pageLength = 10, dom = "ftip"), rownames = FALSE)
  })

  observeEvent(input$load_history, {
    sel <- input$history_table_rows_selected; req(sel)
    df <- db_list_reviews(DB, limit = 200); id <- df$id[sel]
    review <- db_get_review(DB, id); issues <- db_issues_for_review(DB, id)
    analysis(list(ok = TRUE, data = list(overall_score = review$overall_score, risk_level = review$risk_level, executive_summary = "", issues = issues, missing_information = list())))
    showNotification("Review loaded.", type = "message")
  })

  observeEvent(input$delete_history, {
    sel <- input$history_table_rows_selected; req(sel)
    df <- db_list_reviews(DB, limit = 200); id <- df$id[sel]
    DBI::dbExecute(DB, "DELETE FROM issues WHERE review_id = ?", params = list(id))
    DBI::dbExecute(DB, "DELETE FROM reviews WHERE id = ?", params = list(id))
    history_trigger(history_trigger() + 1)
    showNotification("Review deleted.", type = "warning")
  })
}

shiny::shinyApp(ui, server)
