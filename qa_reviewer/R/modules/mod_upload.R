uploadUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Upload Document"),
    textInput(ns("study_code"), "Study / Document Code", placeholder = "e.g. ST-2026-001"),
    fileInput(ns("file"), "Choose file", accept = c(".pdf", ".docx", ".xlsx", ".xls", ".txt")),
    selectInput(ns("doc_type"), "Document type", choices = c("Protocol", "CSR", "SOP", "SAP", "Study Report", "Other")),
    uiOutput(ns("file_info"))
  )
}

uploadServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {
    parsed <- reactiveVal(NULL)
    doc_type <- reactive(input$doc_type %||% "Other")
    file_info <- reactiveVal(NULL)
    pdf_pages <- reactiveVal(NULL)
    toc_map <- reactiveVal(NULL)
    orig_docx_path <- reactiveVal(NULL)
    orig_docx_name <- reactiveVal(NULL)
    observeEvent(input$file, {
      f <- input$file; req(f)
      ext <- tolower(tools::file_ext(f$name))
      txt <- tryCatch(read_doc(f$datapath, ext), error = function(e) { showNotification(paste("Could not read file:", conditionMessage(e)), type = "error", duration = 10); NULL })
      if (!is.null(txt)) {
        parsed(txt)
        if (ext == "docx") {
          orig_docx_path(f$datapath); orig_docx_name(f$name)
          tm <- tryCatch(parse_toc_pages(paste(docx_paragraph_texts(f$datapath), collapse = "\n")), error = function(e) NULL)
          toc_map(tm); pdf_pages(NULL)
        } else { orig_docx_path(NULL); orig_docx_name(NULL); toc_map(NULL); if (ext == "pdf") { pp <- tryCatch(pdftools::pdf_text(f$datapath), error = function(e) NULL); pdf_pages(pp) } else { pdf_pages(NULL) } }
        n_words <- max(0L, { m <- gregexpr("\\S+", txt, perl = TRUE)[[1]]; if (m[1] == -1) 0L else length(m) })
        file_info(list(name = f$name, ext = ext, n_chars = nchar(txt), n_words = n_words))
      }
    })
    output$file_info <- renderUI({
      fi <- file_info(); if (is.null(fi)) return(NULL)
      div(style = "font-size:0.85em; color:#475569;", sprintf("%s (%s chars, %s words)", fi$name, format(fi$n_chars, big.mark = ","), format(fi$n_words, big.mark = ",")))
    })
    list(parsed = parsed, doc_type = doc_type, file_info = file_info, pdf_pages = pdf_pages, toc_map = toc_map, orig_docx_path = orig_docx_path, orig_docx_name = orig_docx_name)
  })
}
