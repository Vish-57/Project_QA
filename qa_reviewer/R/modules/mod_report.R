reportUI <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(ns("docx"), "Download DOCX Report", class = "btn-primary"),
    downloadButton(ns("json"), "Download Raw JSON", class = "btn-secondary")
  )
}

reportServer <- function(id, analysis, file_info) {
  moduleServer(id, function(input, output, session) {
    output$docx <- downloadHandler(
      filename = function() paste0("qa_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".docx"),
      content = function(file) {
        a <- analysis(); req(a, isTRUE(a$ok))
        generate_docx_report(a, file)
      }
    )
    output$json <- downloadHandler(
      filename = function() paste0("qa_report_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".json"),
      content = function(file) {
        a <- analysis(); req(a, isTRUE(a$ok))
        writeLines(jsonlite::toJSON(a$data, auto_unbox = TRUE, pretty = TRUE), file)
      }
    )
  })
}
