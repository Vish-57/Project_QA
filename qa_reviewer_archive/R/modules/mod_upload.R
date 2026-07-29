# =============================================================================
# mod_upload.R
# File upload + document-type picker. Returns reactives:
#   parsed()   : list (parse_document result) or NULL
#   doc_type() : selected document type
#   file_info(): filename, size, ext
# =============================================================================

uploadUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("1. Upload document"),
    bslib::card_body(
      fluidRow(
        column(7,
          fileInput(ns("file"),
            label = NULL,
            accept = c(".pdf", ".docx", ".txt",
                       "application/pdf",
                       "application/vnd.openxmlformats-officedocument.wordprocessingml.document",
                       "text/plain"),
            buttonLabel = "Choose file…",
            placeholder = "Drag & drop a PDF, DOCX, or TXT"),
        ),
        column(5,
          selectInput(ns("doc_type"), "Document type",
            choices  = c("Protocol", "Clinical Study Report (CSR)",
                         "SOP", "Statistical Analysis Plan (SAP)",
                         "Investigator Brochure", "Other"),
            selected = "Protocol")
        )
      ),
      uiOutput(ns("preview"))
    )
  )
}

uploadServer <- function(id, config) {
  moduleServer(id, function(input, output, session) {

    parsed <- reactiveVal(NULL)
    file_info <- reactiveVal(NULL)

    observeEvent(input$file, {
      f <- input$file
      req(f)
      withProgress(message = "Parsing document…", value = 0.3, {
        res <- parse_document(f$datapath)
        incProgress(0.6)
        if (isFALSE(res$ok)) {
          showNotification(paste("Parse failed:", res$error),
                           type = "error", duration = 10)
          parsed(NULL)
          file_info(NULL)
        } else {
          # Carry the original filename through (fileInput rewrites it)
          res$filename <- f$name
          parsed(res)
          file_info(list(name = f$name, size = f$size,
                         ext = tools::file_ext(f$name)))
        }
      })
    })

    output$preview <- renderUI({
      p <- parsed(); if (is.null(p)) return(NULL)
      fi <- file_info()
      bslib::value_box(
        title = fi$name,
        value = sprintf("%s words · %s pages",
                        format(p$n_words, big.mark = ","), p$n_pages),
        showcase = bsicons::bs_icon("file-earmark-text"),
        theme = "primary"
      )
    })

    list(
      parsed    = parsed,
      doc_type  = reactive(input$doc_type),
      file_info = file_info
    )
  })
}
