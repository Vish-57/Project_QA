library(shiny)
source("global.R")

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", href = "custom.css"),
    tags$style(HTML("
      @import url('https://fonts.googleapis.com/css2?family=Inter:wght@300;400;500;600;700;800&display=swap');
      * { font-family: 'Inter', system-ui, sans-serif; }
      body { background: #f0f2f5; }
      .brand-header { background: linear-gradient(135deg, #0f172a 0%, #1e293b 100%); color: white; padding: 14px 28px; display: flex; justify-content: space-between; align-items: center; border-bottom: 3px solid #3b82f6; margin-bottom: 0; }
      .brand-title { font-size: 1.3rem; font-weight: 700; letter-spacing: -0.3px; }
      .brand-sub { font-size: 0.75rem; color: #94a3b8; letter-spacing: 0.5px; text-transform: uppercase; }
      .card { background: white; border-radius: 10px; padding: 20px; box-shadow: 0 1px 3px rgba(0,0,0,0.06); border: 1px solid #e2e8f0; margin-bottom: 14px; }
      .card-header { font-size: 0.85rem; font-weight: 600; color: #64748b; text-transform: uppercase; letter-spacing: 0.5px; margin-bottom: 12px; padding-bottom: 8px; border-bottom: 1px solid #f1f5f9; }
      .score-display { font-size: 2.5rem; font-weight: 800; line-height: 1; }
      .risk-badge { display: inline-block; padding: 4px 14px; border-radius: 999px; font-weight: 700; font-size: 0.8rem; text-transform: uppercase; letter-spacing: 0.5px; }
      .risk-Low { background: #dcfce7; color: #166534; }
      .risk-Medium { background: #dbeafe; color: #1e40af; }
      .risk-High { background: #fef3c7; color: #92400e; }
      .risk-Critical { background: #fee2e2; color: #991b1b; }
      .sev-critical { background: #fee2e2; color: #dc2626; padding: 2px 8px; border-radius: 4px; font-weight: 600; font-size: 0.75rem; }
      .sev-major { background: #fef3c7; color: #d97706; padding: 2px 8px; border-radius: 4px; font-weight: 600; font-size: 0.75rem; }
      .sev-minor { background: #dbeafe; color: #2563eb; padding: 2px 8px; border-radius: 4px; font-weight: 600; font-size: 0.75rem; }
      .nav-tabs { border-bottom: 2px solid #e2e8f0; margin-bottom: 16px; }
      .nav-tabs > li > a { border: none !important; color: #64748b; font-weight: 500; padding: 10px 18px; transition: all 0.15s; }
      .nav-tabs > li.active > a, .nav-tabs > li.active > a:hover, .nav-tabs > li.active > a:focus { color: #1e293b !important; background: transparent !important; border-bottom: 2px solid #3b82f6 !important; font-weight: 600; }
      .nav-tabs > li > a:hover { color: #1e293b; background: #f8fafc !important; }
      .btn-primary { background: #1e293b; border-color: #1e293b; font-weight: 600; }
      .btn-primary:hover { background: #0f172a; border-color: #0f172a; }
      .stat-box { text-align: center; padding: 12px; border-radius: 8px; background: #f8fafc; }
      .stat-value { font-size: 1.5rem; font-weight: 700; }
      .stat-label { font-size: 0.75rem; color: #64748b; text-transform: uppercase; letter-spacing: 0.3px; }
      .progress-anim { background: #e2e8f0; border-radius: 999px; height: 8px; overflow: hidden; }
      .progress-anim > div { height: 100%; background: linear-gradient(90deg, #3b82f6, #2563eb); animation: fillBar 30s ease-out; border-radius: 999px; }
      @keyframes fillBar { from { width: 0%; } to { width: 95%; } }
      .insight-card { background: #f0fdf4; border: 1px solid #bbf7d0; border-radius: 8px; padding: 12px; margin-bottom: 8px; }
      .insight-card h6 { color: #166534; margin: 0 0 4px 0; font-weight: 600; }
      .insight-card p { margin: 0; font-size: 0.85rem; color: #374151; }
    "))
  ),
  div(class = "brand-header",
    div(
      div(class = "brand-title", icon("check-square", style = "margin-right:8px;"), "QA Reviewer"),
      div(class = "brand-sub", "Mascot Spincontrol — AI-Powered Document Quality Assurance")
    ),
    uiOutput("user_chip")
  ),
  uiOutput("app_shell")
)

server <- function(input, output, session) {
  current_user <- reactiveVal(NULL)
  doc_text <- reactiveVal(NULL)
  analysis <- reactiveVal(NULL)
  running <- reactiveVal(FALSE)
  pdf_pages <- reactiveVal(NULL)
  toc_map <- reactiveVal(NULL)
  orig_docx_path <- reactiveVal(NULL)
  orig_docx_name <- reactiveVal(NULL)
  history_trigger <- reactiveVal(0)
  models <- reactiveVal(character(0))

  # --- Auth ---
  output$app_shell <- renderUI({
    if (is.null(current_user())) {
      return(div(style = "max-width:380px; margin:80px auto;",
        div(class = "card",
          div(class = "card-header", icon("lock"), " Login"),
          textInput("auth_email", "Email", value = "admin@local"),
          passwordInput("auth_password", "Password"),
          actionButton("auth_login", "Sign In", class = "btn-primary", style = "width:100%; font-weight:600; padding:8px;"),
          br(), br(),
          uiOutput("auth_error")
        )
      ))
    }
    tagList(
      tabsetPanel(id = "main_tab",
        tabPanel(title = tagList(icon("file-text"), " Review"), value = "review",
          sidebarLayout(
            sidebarPanel(width = 3,
              div(class = "card", style = "padding:16px;",
                div(class = "card-header", icon("upload"), " Upload Document"),
                textInput("study_code", "Study Code", placeholder = "e.g. ST-2026-001"),
                fileInput("file", "Choose File", accept = c(".pdf", ".docx", ".xlsx", ".xls", ".txt")),
                selectInput("doc_type", "Document Type", choices = c("Protocol", "CSR", "SOP", "SAP", "Study Report", "Other")),
                hr(),
                div(class = "card-header", icon("sliders"), " Analysis Settings"),
                uiOutput("model_ui"),
                textAreaInput("remarks", "Auditor Notes", rows = 2, placeholder = "e.g. Focus on statistics section."),
                uiOutput("eta_box"),
                uiOutput("analyze_btn"),
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
        tabPanel(title = tagList(icon("clock-rotate-left"), " History"), value = "history",
          div(class = "card",
            div(class = "card-header", icon("database"), " Past Reviews"),
            DT::DTOutput("history_table"),
            br(),
            div(style = "display:flex; gap:8px;",
              actionButton("load_history", "View Selected", class = "btn-primary"),
              actionButton("delete_history", "Delete", class = "btn-danger")
            )
          )
        ),
        tabPanel(title = tagList(icon("lightbulb"), " Insights"), value = "insights",
          div(class = "card",
            div(class = "card-header", icon("chart-line"), " Learning & Insights"),
            uiOutput("insights_ui")
          )
        ),
        tabPanel(title = tagList(icon("comments"), " Feedback & Learning"), value = "feedback",
          uiOutput("feedback_container")
        ),
        tabPanel(title = tagList(icon("clipboard-list"), " Audit Guidelines"), value = "guidelines",
          div(class = "card",
            div(class = "card-header", icon("clipboard-check"), " Custom Audit Guidelines"),
            p("Add your house rules one at a time. Every saved rule is enforced on every audit, in addition to the standard checks.", style = "color:#64748b; margin-bottom:16px;"),
            fluidRow(
              column(10, textAreaInput("new_guideline", NULL, value = "", rows = 2, width = "100%", placeholder = "e.g. In footer, Protocol sections must show PR, ICF must show AC, SSE must show MQ.")),
              column(2, actionButton("add_guideline", "Add rule", icon = icon("plus"), class = "btn-success", style = "width:100%; font-weight:600; margin-top:2px;"))
            ),
            hr(),
            h5("Saved rules", style = "font-weight:600; margin-bottom:12px;"),
            uiOutput("guidelines_list")
          )
        ),
        tabPanel(title = tagList(icon("gear"), " Settings"), value = "settings",
          div(class = "card",
            div(class = "card-header", icon("wrench"), " Configuration"),
            h5("Ollama Status"), uiOutput("ollama_status"), hr(),
            h5("Cache"), actionButton("clear_cache", "Clear Cache", class = "btn-warning btn-sm"), hr(),
            p("QA Reviewer v2.1 - Now with Self-Learning!", style = "color:#94a3b8; font-size:0.85rem;")
          )
        )
      ),
      div(style = "text-align:right; padding:6px 16px 16px 16px;",
        actionLink("logout", "Logout", icon = icon("sign-out-alt"), style = "color:#94a3b8; font-size:0.85rem;")
      )
    )
  })

  observeEvent(input$auth_login, {
    u <- auth_login(DB, input$auth_email, input$auth_password)
    if (is.null(u)) {
      output$auth_error <- renderUI(div(style = "color:#dc2626; font-size:0.85rem;", "Invalid email or password."))
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
    div(style = "display:flex; align-items:center; gap:8px;",
      div(style = "width:30px; height:30px; border-radius:50%; background:#3b82f6; display:flex; align-items:center; justify-content:center; font-weight:700; font-size:0.8rem;", substr(u$display_name %||% u$email, 1, 1)),
      div(div(u$display_name %||% u$email, style = "font-size:0.85rem; font-weight:500;"), div(u$role, style = "font-size:0.7rem; color:#94a3b8;"))
    )
  })

  # --- Models ---
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
    preferred <- c("gpt-oss:120b-cloud", "llama3.2:3b", "qwen2.5:3b", "phi3.5:3.8b", "gemma2:2b")
    default <- intersect(preferred, m)[1]
    if (is.na(default)) default <- m[1]
    selectInput("model", "Model", choices = if (length(m)) m else "(no models installed)", selected = default)
  })

  output$status <- renderUI({
    s <- if (length(models()) == 0) "Ollama not reachable. Start Ollama Desktop first." else paste("Ollama OK —", length(models()), "models available.")
    div(style = "font-size:0.8rem; color:#64748b; background:#f8fafc; padding:8px 10px; border-radius:6px;", s)
  })

  output$ollama_status <- renderUI({
    up <- ollama_is_up(APP_CONFIG)
    div(style = paste("padding:8px; border-radius:6px; font-size:0.85rem;", if (up) "background:#dcfce7; color:#166534;" else "background:#fee2e2; color:#991b1b;"), if (up) "Ollama is running." else "Ollama is not reachable.")
  })

  # Pre-load the selected local model into memory so the first analysis
  # doesn't pay the model-load cost. No-op for cloud models.
  observeEvent(input$model, {
    if (!is.null(input$model) && input$model != "(no models installed)") ollama_warmup_async(input$model, APP_CONFIG)
  })

  observeEvent(input$clear_cache, {
    n <- cache_clear(APP_CONFIG$cache$dir %||% "cache")
    showNotification(paste("Cleared", n, "cache entries."), type = "message")
  })

  # --- File Upload ---
  observeEvent(input$file, {
    f <- input$file; req(f)
    ext <- tolower(tools::file_ext(f$name))
    pp <- NULL
    txt <- tryCatch({
      if (ext == "pdf") { pp <- pdftools::pdf_text(f$datapath); paste(pp, collapse = "\n\n") }
      else read_doc(f$datapath, ext)
    }, error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error", duration = 10); NULL })
    if (!is.null(txt)) {
      doc_text(txt); analysis(NULL)
      if (ext == "docx") { orig_docx_path(f$datapath); orig_docx_name(f$name); tm <- tryCatch(parse_toc_pages(paste(docx_paragraph_texts(f$datapath), collapse = "\n")), error = function(e) NULL); toc_map(tm); pdf_pages(NULL) }
      else { orig_docx_path(NULL); orig_docx_name(NULL); toc_map(NULL); pdf_pages(pp) }
      showNotification(sprintf("Loaded '%s' (%s chars, %s words)", f$name, format(nchar(txt), big.mark = ","), format(max(0L, { m <- gregexpr("\\S+", txt, perl = TRUE)[[1]]; if (m[1] == -1) 0L else length(m) }), big.mark = ",")), type = "message")
    }
  })

  output$eta_box <- renderUI({
    txt <- doc_text(); m <- input$model
    if (is.null(txt) || is.null(m) || m == "(no models installed)") return(NULL)
    est_sec <- max(30, ceiling(nchar(txt) / 350))
    div(style = "background:#f0fdf4; border-left:3px solid #22c55e; padding:8px 10px; border-radius:6px; margin-bottom:10px; font-size:0.85rem;", tags$strong("Est. time: "), sprintf("~%d min %d sec", est_sec %/% 60, est_sec %% 60))
  })

  output$progress_ui <- renderUI({
    if (!isTRUE(running())) return(NULL)
    div(class = "card",
      div(style = "display:flex; justify-content:space-between; align-items:center; margin-bottom:8px;",
        h5(style = "margin:0; font-weight:600;", icon("spinner", class = "fa-pulse"), " Analysing..."),
        span(input$model, style = "font-size:0.8rem; color:#64748b;")
      ),
      div(class = "progress-anim", div(style = sprintf("animation-duration:%ds;", max(30L, ceiling(nchar(doc_text() %||% "") / 200))))),
      p("Processing document chunks with LLM...", style = "color:#64748b; font-size:0.8rem; margin-top:6px;")
    )
  })

  # --- Audit Guidelines (persistent house rules, enforced on every audit) ---
  # Stored as a JSON array under the "guidelines_list" setting. Rules are sent
  # to the model with their own numbering, so strip any "1." / "2)" the user
  # typed at the start to avoid "1. 1. ..." duplication.
  clean_rule <- function(s) trimws(sub("^\\s*\\d+\\s*[.)]\\s*", "", s))

  load_guidelines <- function() {
    raw <- db_get_setting(DB, "guidelines_list", "")
    if (!nzchar(raw)) return(character(0))
    g <- tryCatch(as.character(unlist(jsonlite::fromJSON(raw))), error = function(e) character(0))
    g[nzchar(trimws(g))]
  }
  guidelines <- reactiveVal(load_guidelines())

  persist_guidelines <- function(g) {
    db_save_setting(DB, "guidelines_list", as.character(jsonlite::toJSON(g)))
    guidelines(g)
  }

  observeEvent(input$add_guideline, {
    txt <- clean_rule(input$new_guideline %||% "")
    if (!nzchar(txt)) { showNotification("Type a rule first.", type = "warning"); return(NULL) }
    if (length(guidelines()) >= 50L) { showNotification("Rule limit reached (50). Remove a rule first.", type = "warning"); return(NULL) }
    persist_guidelines(c(guidelines(), txt))
    updateTextAreaInput(session, "new_guideline", value = "")
    showNotification("Rule saved.", type = "message")
  })

  # One delete button per rule (del_rule_1 ... del_rule_50). Observers are
  # registered once for a fixed set of slots, so re-rendering the list can
  # never stack duplicate handlers on the same button.
  lapply(seq_len(50L), function(i) {
    observeEvent(input[[paste0("del_rule_", i)]], {
      cur <- guidelines()
      if (i <= length(cur)) { persist_guidelines(cur[-i]); showNotification("Rule removed.", type = "warning") }
    }, ignoreInit = TRUE)
  })

  output$guidelines_list <- renderUI({
    g <- guidelines()
    if (length(g) == 0) return(p(em("No custom rules yet. Add one above."), style = "color:#94a3b8;"))
    tagList(lapply(seq_along(g), function(i) {
      div(style = "display:flex; align-items:flex-start; gap:12px; padding:10px 12px; border:1px solid #e2e8f0; border-radius:10px; margin-bottom:8px; background:#f8fafc;",
        div(style = "flex:0 0 28px; height:28px; border-radius:8px; background:#1e293b; color:#fff; display:flex; align-items:center; justify-content:center; font-weight:700; font-size:0.85em;", i),
        div(style = "flex:1; line-height:1.45; color:#1e293b;", g[i]),
        actionButton(paste0("del_rule_", i), NULL, icon = icon("trash"), class = "btn-danger btn-sm", style = "flex:0 0 auto;")
      )
    }))
  })

  # --- Analysis ---
  output$analyze_btn <- renderUI({
    if (isTRUE(running())) {
      tags$button(type = "button", class = "btn btn-primary", disabled = "disabled", style = "width:100%; font-weight:600; padding:10px; opacity:0.65;", icon("spinner", class = "fa-pulse"), " Analysing...")
    } else {
      actionButton("analyze", "Run Analysis", class = "btn-primary", style = "width:100%; font-weight:600; padding:10px;")
    }
  })

  observeEvent(input$analyze, {
    if (isTRUE(running())) return(NULL)
    txt <- doc_text(); req(txt)
    if (is.null(input$model) || input$model == "(no models installed)") { showNotification("Select an Ollama model first.", type = "warning"); return(NULL) }
    running(TRUE); analysis(NULL)
    # Saved house rules (numbered) + optional one-off remarks for this run.
    g <- guidelines()
    rules_txt <- if (length(g) > 0) paste(sprintf("%d. %s", seq_along(g), g), collapse = "\n") else ""
    remarks_txt <- trimws(input$remarks %||% "")
    parts <- character(0)
    if (nzchar(rules_txt)) parts <- c(parts, rules_txt)
    if (nzchar(remarks_txt)) parts <- c(parts, paste0("REMARKS FOR THIS AUDIT (consider these before auditing):\n", remarks_txt))
    .text <- txt; .model <- input$model; .doctype <- input$doc_type; .remarks <- paste(parts, collapse = "\n\n---\n\n")
    .pdf_pages <- pdf_pages(); .toc_map <- toc_map(); .study_code <- input$study_code %||% "UNKNOWN"
    .user_email <- if (!is.null(current_user())) current_user()$email else "anonymous"
    .filename <- if (!is.null(input$file)) input$file$name else "unknown"

    # Save review start to DB
    review_id <- tryCatch(db_insert_review(DB, list(
      user_email = .user_email, filename = .filename, doc_type = .doctype,
      model = .model, status = "running", started_at = Sys.time()
    )), error = function(e) NULL)

    later::later(function() {
      tryCatch({
        res <- tryCatch({
          r <- run_audit(.text, .model, APP_CONFIG, .doctype, custom_guidelines = .remarks, page_texts = .pdf_pages, toc_map = .toc_map, progress = function(msg) message(msg))
          if (!is.null(r) && isTRUE(r$ok)) r$data$model <- .model
          r
        }, error = function(e) { showNotification(paste("Analysis failed:", conditionMessage(e)), type = "error", duration = 12); NULL })
        if (!is.null(res) && isTRUE(res$ok)) {
          # Save results to DB
          if (!is.null(review_id)) {
            tryCatch({
              db_update_review(DB, review_id, list(
                status = "complete", finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
                duration_s = round(res$elapsed_s, 1),
                overall_score = res$data$overall_score %||% 0,
                risk_level = res$data$risk_level %||% "Unknown",
                executive_summary = res$data$executive_summary %||% "",
                n_critical = sum(tolower(res$data$issues$severity) == "critical", na.rm = TRUE),
                n_major = sum(tolower(res$data$issues$severity) == "major", na.rm = TRUE),
                n_minor = sum(tolower(res$data$issues$severity) == "minor", na.rm = TRUE)
              ))
              if (!is.null(res$data$issues) && NROW(res$data$issues) > 0) {
                issues_df <- res$data$issues
                if (!is.null(issues_df$what_is_wrong)) issues_df$description <- issues_df$what_is_wrong
                if (!is.null(issues_df$suggested_fix)) issues_df$suggestion <- issues_df$suggested_fix
                db_insert_issues(DB, review_id, issues_df)
              }
            }, error = function(e) message("DB save error: ", conditionMessage(e)))
          }
          analysis(res)
          history_trigger(history_trigger() + 1)
        } else {
          if (!is.null(review_id)) tryCatch(db_update_review(DB, review_id, list(status = "error", finished_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))), error = function(e) NULL)
          if (!is.null(res)) analysis(res)
        }
      }, error = function(e) message("analysis error: ", conditionMessage(e)))
      running(FALSE)
    }, delay = 0.1)
  })

  # --- Results ---
  output$results_ui <- renderUI({
    a <- analysis()
    if (is.null(a)) return(div(class = "card", style = "text-align:center; padding:60px; color:#94a3b8;", icon("file-lines", style = "font-size:3rem; margin-bottom:12px;"), p("Upload a document and click Run Analysis.")))
    if (!isTRUE(a$ok)) return(div(class = "card", h5("Analysis Failed", style = "color:#dc2626;"), pre(a$raw_text %||% "No output", style = "font-size:0.85rem; background:#f8fafc; padding:12px; border-radius:6px; max-height:400px; overflow:auto;")))
    d <- a$data
    score <- d$overall_score %||% 0; risk <- d$risk_level %||% "Unknown"
    n_crit <- sum(tolower(d$issues$severity) == "critical", na.rm = TRUE)
    n_major <- sum(tolower(d$issues$severity) == "major", na.rm = TRUE)
    n_minor <- sum(tolower(d$issues$severity) == "minor", na.rm = TRUE)

    model_label <- d$model %||% ""
    dur_label <- if (isTRUE(a$from_cache)) "instant (cached)" else if (length(a$elapsed_s) && is.numeric(a$elapsed_s) && a$elapsed_s > 0) sprintf("%.0f sec", a$elapsed_s) else ""
    tagList(
      div(class = "card",
        div(class = "card-header", icon("gauge-high"), " Score Overview"),
        fluidRow(
          column(3, div(class = "stat-box", div(class = "stat-value", paste0(score, "/100")), div(class = "stat-label", "QA Score"))),
          column(3, div(class = "stat-box", div(class = paste0("risk-badge risk-", risk), risk), div(class = "stat-label", style = "margin-top:8px;", "Risk Level"))),
          column(3, div(class = "stat-box", div(class = "stat-value", n_crit), div(class = "stat-label", "Critical"))),
          column(3, div(class = "stat-box", div(class = "stat-value", n_major), div(class = "stat-label", "Major")))
        ),
        if (nzchar(model_label) || nzchar(dur_label)) div(style = "margin-top:8px; font-size:0.8rem; color:#64748b; display:flex; gap:16px;",
          if (nzchar(model_label)) span(icon("cube"), sprintf(" Model: %s", model_label)),
          if (nzchar(dur_label)) span(icon("clock"), sprintf(" Duration: %s", dur_label))
        )
      ),
      if (nzchar(d$executive_summary %||% "")) div(class = "card",
        div(class = "card-header", icon("file-pen"), " Executive Summary"),
        p(d$executive_summary, style = "line-height:1.6; color:#374151;")
      ),
      if (!is.null(d$issues) && NROW(d$issues) > 0) {
        df <- as.data.frame(d$issues, stringsAsFactors = FALSE)
        for (col in c("severity","category","location","what_is_wrong","suggested_fix","section","page")) if (is.null(df[[col]])) df[[col]] <- ""
        div(class = "card",
          div(class = "card-header", icon("list-check"), sprintf(" Findings (%d)", nrow(df))),
          DT::datatable(
            df[, c("severity","category","section","location","what_is_wrong","suggested_fix")],
            selection = "single", options = list(pageLength = 15, dom = "ftip", columnDefs = list(list(width = '100px', targets = 0))), rownames = FALSE
          ) |> DT::formatStyle("severity", target = "row", backgroundColor = DT::styleEqual(c("critical","major"), c("#fee2e2","#fef3c7")))
        )
      },
      if (length(d$missing_information) > 0) div(class = "card",
        div(class = "card-header", icon("triangle-exclamation"), " Missing Information"),
        tags$ul(lapply(d$missing_information, function(x) tags$li(x, style = "color:#374151;")))
      ),
      div(class = "card",
        div(style = "display:flex; gap:8px;",
          downloadButton("dl_md", icon("download"), " Report (.md)", class = "btn-outline-primary btn-sm"),
          downloadButton("dl_json", icon("download"), " Raw JSON", class = "btn-outline-secondary btn-sm")
        )
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

  # --- History ---
  history_df <- reactiveVal(data.frame())

  output$history_table <- DT::renderDT({
    history_trigger()
    df <- db_list_reviews(DB, limit = 200)
    history_df(df)
    if (nrow(df) == 0) return(DT::datatable(data.frame(Message = "No reviews yet."), options = list(dom = "t")))
    nz <- function(v) ifelse(is.na(v), 0, v)
    dur <- ifelse(is.na(df$duration_s), "-", sprintf("%.0fs", nz(df$duration_s)))
    show <- data.frame(
      ID = df$id, File = df$filename, Type = df$doc_type, Model = df$model,
      Score = df$overall_score, Risk = df$risk_level,
      Issues = nz(df$n_critical) + nz(df$n_major) + nz(df$n_minor),
      Duration = dur, Date = df$started_at, stringsAsFactors = FALSE
    )
    DT::datatable(show, selection = "single", options = list(pageLength = 10, dom = "ftip"), rownames = FALSE)
  })

  observeEvent(input$load_history, {
    sel <- input$history_table_rows_selected; req(sel)
    df <- history_df(); req(nrow(df) >= sel)
    id <- df$id[sel]
    review <- db_get_review(DB, id); issues <- db_issues_for_review(DB, id)
    if (NROW(issues) > 0) {
      blank_if_na <- function(v) ifelse(is.na(v), "", as.character(v))
      issues$what_is_wrong <- blank_if_na(issues$description)
      issues$suggested_fix <- blank_if_na(issues$suggestion)
      issues$location <- blank_if_na(issues$location)
      issues$category <- blank_if_na(issues$category)
      issues$severity <- blank_if_na(issues$severity)
      issues$section <- if (is.null(issues$section)) "" else blank_if_na(issues$section)
      issues$page <- if (is.null(issues$page)) "" else blank_if_na(issues$page)
    }
    if (nrow(review) > 0) {
      analysis(list(ok = TRUE, data = list(
        overall_score = review$overall_score %||% 0, risk_level = review$risk_level %||% "Unknown",
        executive_summary = review$executive_summary %||% "",
        issues = issues, missing_information = list(),
        model = review$model %||% "", duration_s = review$duration_s %||% 0,
        started_at = review$started_at %||% "",
        n_critical = review$n_critical %||% 0, n_major = review$n_major %||% 0, n_minor = review$n_minor %||% 0
      ), elapsed_s = review$duration_s %||% 0))
      updateTabsetPanel(session, "main_tab", selected = "review")
      showNotification("Review loaded from history.", type = "message")
    }
  })

  observeEvent(input$delete_history, {
    sel <- input$history_table_rows_selected; req(sel)
    df <- history_df(); req(nrow(df) >= sel)
    id <- df$id[sel]
    DBI::dbExecute(DB, "DELETE FROM issues WHERE review_id = ?", params = list(id))
    DBI::dbExecute(DB, "DELETE FROM reviews WHERE id = ?", params = list(id))
    history_trigger(history_trigger() + 1)
    showNotification("Review deleted.", type = "warning")
  })

  # --- Insights (learning from past data) ---
  output$insights_ui <- renderUI({
    history_trigger()
    all_reviews <- tryCatch(db_list_reviews(DB, limit = 500), error = function(e) data.frame())
    if (nrow(all_reviews) == 0) {
      return(div(style = "text-align:center; padding:40px; color:#94a3b8;", icon("chart-simple", style = "font-size:2rem;"), p("No data yet. Run some analyses to see insights.")))
    }
    avg_score <- round(mean(all_reviews$overall_score, na.rm = TRUE), 0)
    total_reviews <- nrow(all_reviews)
    most_common_type <- names(sort(table(all_reviews$doc_type), decreasing = TRUE))[1]
    most_common_model <- names(sort(table(all_reviews$model), decreasing = TRUE))[1]
    high_risk <- sum(all_reviews$risk_level %in% c("High", "Critical"), na.rm = TRUE)

    # Get most common issue categories
    all_issues <- tryCatch(DBI::dbGetQuery(DB, "SELECT category, severity, COUNT(*) as cnt FROM issues GROUP BY category, severity ORDER BY cnt DESC LIMIT 10"), error = function(e) data.frame())

    tagList(
      fluidRow(
        column(3, div(class = "stat-box", div(class = "stat-value", total_reviews), div(class = "stat-label", "Total Reviews"))),
        column(3, div(class = "stat-box", div(class = "stat-value", paste0(avg_score, "/100")), div(class = "stat-label", "Avg Score"))),
        column(3, div(class = "stat-box", div(class = "stat-value", high_risk), div(class = "stat-label", "High Risk"))),
        column(3, div(class = "stat-box", div(class = "stat-value", most_common_type), div(class = "stat-label", "Most Reviewed")))
      ),
      if (nrow(all_issues) > 0) div(class = "card",
        div(class = "card-header", icon("chart-bar"), " Most Common Issues"),
        DT::datatable(all_issues, options = list(pageLength = 10, dom = "t"), rownames = FALSE)
      ),
      div(class = "card",
        div(class = "card-header", icon("robot"), " Recommendations"),
        div(class = "insight-card",
          h6(icon("check-circle"), " Preferred Model"),
          p(sprintf("Based on %d reviews, '%s' is the most used model. Consider keeping it as default.", total_reviews, most_common_model %||% "N/A"))
        ),
        div(class = "insight-card",
          h6(icon("triangle-exclamation"), " Risk Trends"),
          p(sprintf("%d out of %d reviews (%d%%) were High or Critical risk. Focus on improving document quality before submission.", high_risk, total_reviews, round(high_risk/total_reviews*100)))
        ),
        div(class = "insight-card",
          h6(icon("lightbulb"), " Document Type Focus"),
          p(sprintf("'%s' documents are reviewed most frequently. Consider creating a specialized checklist for this type.", most_common_type))
        )
      )
    )
  })

  # --- Feedback & Learning Module ---
  # Source the feedback module and initialize it
  source("R/modules/mod_feedback.R", local = TRUE)
  mod_feedback_server("feedback", DB)
  
  # Render the feedback UI in the dedicated container
  output$feedback_container <- renderUI({
    mod_feedback_ui("feedback")
  })
}

shiny::shinyApp(ui, server)
