# =============================================================================
# mod_report.R
# Download report buttons (DOCX, PDF).
# =============================================================================

reportUI <- function(id) {
  ns <- NS(id)
  bslib::card(
    bslib::card_header("Download report"),
    bslib::card_body(
      p("Generate a polished review report you can attach to your QA record."),
      div(
        downloadButton(ns("dl_docx"), "Download .docx",
                       class = "btn-primary"),
        downloadButton(ns("dl_pdf"),  "Download .pdf",
                       class = "btn-outline-secondary"),
        downloadButton(ns("dl_json"), "Raw JSON",
                       class = "btn-outline-secondary")
      )
    )
  )
}

reportServer <- function(id, analysis, file_info) {
  moduleServer(id, function(input, output, session) {

    base_name <- reactive({
      fi <- file_info(); a <- analysis()
      stamp <- format(Sys.time(), "%Y%m%d-%H%M%S")
      base <- if (!is.null(fi$name)) tools::file_path_sans_ext(fi$name)
              else "qa_review"
      paste0("qa_review_", base, "_", stamp)
    })

    output$dl_docx <- downloadHandler(
      filename = function() paste0(base_name(), ".docx"),
      content = function(file) {
        a <- analysis(); req(a)
        generate_docx_report(a, file)
      }
    )

    output$dl_pdf <- downloadHandler(
      filename = function() paste0(base_name(), ".html"), # html if pandoc missing
      content = function(file) {
        a <- analysis(); req(a)
        out <- generate_pdf_report(a, file)
        if (is.null(out)) generate_docx_report(a, file)
      }
    )

    output$dl_json <- downloadHandler(
      filename = function() paste0(base_name(), ".json"),
      content = function(file) {
        a <- analysis(); req(a)
        writeLines(jsonlite::toJSON(
          list(summary = a$summary, structure = a$structure,
               compliance = a$compliance, issues = a$issues,
               metadata = list(model = a$model, doc_type = a$doc_type,
                               duration_s = a$duration_s)),
          auto_unbox = TRUE, pretty = TRUE), file)
      }
    )
  })
}
